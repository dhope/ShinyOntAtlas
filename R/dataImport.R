library(sf)
library(dplyr)
library(tidyr)


ont.proj <- 3161
pc.sf <- readr::read_rds(here::here("data/rds/point_counts_preped.rds")) %>% 
  mutate(doy = lubridate::yday(ymd),
         m2s = round(min_tosunrise/60)*60)
aoi <- st_bbox(pc.sf) %>% st_as_sfc(.)
blocks <- st_read(here::here("data/spatial/OBBA/obba2_blocks.shp"), quiet = T) %>% 
  st_transform(ont.proj, partial=F, check=T) %>% 
  st_filter(aoi)
obba_sq <-
  st_read(quiet=T,here::here("data/spatial/OBBA/obba2_squares.shp")) %>%
  st_transform(ont.proj)  %>% st_filter(aoi)


pc.sf <-  pc.sf %>% 
  st_join(obba_sq)

fullPC <- readr::read_tsv(here::here("data/onatlas_pt_cnt_data.txt")) %>%
  filter(point_count_id %in% pc.sf$point_count_id) %>%
  replace_na(list(count_near=0, count_far=0))  %>%
  complete(point_count_id, species_code, fill=list(count_near=0, count_far=0))  %>%
  mutate(counts = count_near + count_far) %>%
  group_by(point_count_id) %>%
  mutate(all_spp_count = sum(counts, na.rm=T)) %>% ungroup %>%
  # filter(all_spp_count>0) %>%
  dplyr::select(-all_spp_count) %>% ungroup %>%
  left_join(st_drop_geometry(pc.sf), by = "point_count_id")

spp_id <- readr::read_csv(here::here("data/AVIAN_CORE_20210118.csv"), col_types = readr::cols())
full_spp <- spp_id[spp_id$Full_Species=="Yes",] %>% filter(Species_ID %in% fullPC$species_code)

# r <- raster::raster(crs = st_crs(aoi), resolution = 5000, ext = raster::extent(as(aoi, "Spatial")))

d_gr <- fullPC %>% group_by(species_code, SQUARE_ID) %>% 
  summarize(n=n(),
            sum_count = sum(counts),
            mean_count = mean(counts),
            sd_count = sd(counts),
            se_count = sd_count/n, .groups = 'drop')

# 
# summarize_group <- function(.data, ...){
#   .data %>% 
#     group_by(species_code, ...) %>% 
#     summarize(
#       n=n(),
#       n_obs = sum(counts>0,na.rm=T),
#       p_obs = n_obs / n,
#       median_count = median(counts, na.rm = T),
#       mean_count = mean(counts, na.rm = T),
#       max_count = max(counts, na.rm=T),
#       var_count = var(counts),
#       sd_count = sd(counts),
#       se.counts = sd_count/sqrt(n),
#       cv  = sd_count / mean_count,
#       .groups = 'drop'
#     )
# }
# admin_region_sum <- summarize_group(fullPC , admin_region)
# roadside_sum <- summarize_group(fullPC , roadside)
# month_sum <- summarize_group(fullPC , month)
# year_sum <- summarize_group(fullPC , year)
# 
# rm(fullPC)
# 
# 
# hab100 <- foreign::read.dbf("D:/CWS_ONT_local/OBBA2/habitat/point_habitat_100m.dbf") %>% 
#   as_tibble() %>% janitor::clean_names() %>% 
#   filter(point_coun %in% pc.sf$point_count_id) %>% 
#   rowwise() %>% 
#   mutate(rs = sum(c_across(matches("^value")))) %>% 
#   ungroup %>% 
#   mutate(across(matches("^value"),~.x/rs)) %>% 
#   pivot_longer(cols = matches("value_"), 
#                names_to = 'variable',
#                values_to = 'phab') %>% 
#   mutate(phab_round =round(phab*10)/10) %>% 
#   group_by(variable, phab_round, .groups = 'drop') %>% 
#   summarise(n=n())
# 
# runs_groups <- tibble(Name=c(
#   "Admin Region, Year"
# ), vars_ = list(alist(admin_region, year)))
# 
# purrr::map(runs_groups$vars_, summarize_group, .data = fullPC)
# 
# 
# summarize_group(fullPC , month) %>% 
#   filter(species_code == "ALFL") %>% 
#   ggplot(aes(month, cv)) + 
#   geom_text(aes(label = species_code))
#   geom_point()
