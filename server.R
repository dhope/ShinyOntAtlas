# Stolen from https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/server.R
library(shiny)
require(dplyr)
require(leaflet)
library(ggplot2)
library(patchwork)

source(here::here("R/dataImport.R"))


function(input, output, session) {
  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
          addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>% 
          addProviderTiles(providers$OpenStreetMap, group = "Open Street Map") %>%
          addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
          addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri.NatGeoWorldMap") %>%
      setView(lng = -87.42, lat = 49.8963, zoom = 6) %>% 
          addLayersControl(position = 'topleft',
              baseGroups = c("Esri.WorldImagery","Toner Lite","OSM", "Toner", "Esri.NatGeoWorldMap"),
              # overlayGroups = c("OBBA2 Revisits - Selected", 
              #                   "OBBA2 - all paid","CORT - selected",
              #                   "CORT - all","BMS Hexagons - Roaded",
              #                   "Atlas Squares", "Atlas Blocks"),
              options = layersControlOptions(collapsed = T) )
  })
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  PCInBounds <- reactive({
    if (is.null(input$map_bounds)) {
      return(pc.sf[FALSE, ])
    }
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(
      pc.sf,
      Y >= latRng[1] & Y <= latRng[2] &
        X >= lngRng[1] & X <= lngRng[2]
    )
  })

    

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- as.numeric(input$pointsize)#size


    if (colorBy == "m2s") {
      colorData <- pc.sf[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
      #     # Color and palette are treated specially in the "superzip" case, because
      #     # the values are categorical instead of continuous.
      #     colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
      #     pal <- colorFactor("viridis", colorData)
    } else {
      colorData <- factor(pc.sf[[colorBy]])
      pal <- colorFactor("viridis", colorData)
    }

    # if (sizeBy == "superzip") {
    # Radius is treated specially in the "superzip" case.
    # radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    # } else {
    radius <- sizeBy*100#pc.sf[[sizeBy]] / max(pc.sf[[sizeBy]]) * 200
    # }

    leafletProxy("map", data = pc.sf) %>%
      clearShapes() %>%
      addCircles(~X, ~Y,
        radius = radius,
        layerId = ~point_count_id,
        stroke = FALSE, fillOpacity = 0.4, fillColor = pal(colorData)
      ) %>%
      addLegend("bottomleft",
        pal = pal, values = colorData, title = colorBy,
        layerId = "colorLegend"
      )
  })





  output$time_day <- renderPlot({
    # If no point counts are in view, don't plot
    if (nrow(PCInBounds()) == 0) {
      return(NULL)
    }
      ptop <- ggplot(PCInBounds() %>% st_drop_geometry(),
                     aes(lubridate::mdy("01-01-2003") + doy))+
                         geom_density(fill = 'red', alpha = 0.7) +
          scale_x_date("",limits = lubridate::mdy("01-01-2003") +range(pc.sf$doy)) +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank()) +
          ylab("")
      pside <- ggplot(PCInBounds() %>% st_drop_geometry(),
                     aes(min_tosunrise))+
          geom_density(fill = 'grey', alpha = 0.7) +
          scale_x_continuous("",limits = range(pc.sf$min_tosunrise))  +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank()) +
          coord_flip() +
          ylab("")
      
      pmain <- ggplot(
        data = PCInBounds() %>% st_drop_geometry(),
        aes(lubridate::mdy("01-01-2003") + doy, min_tosunrise, z = 1)
      ) +
          scale_x_date(limits = lubridate::mdy("01-01-2003") +range(pc.sf$doy)) +
          scale_y_continuous(limits = range(pc.sf$min_tosunrise)) +
        stat_summary_2d( fun = "sum", geom = "tile", na.rm=T) +
        # geom_tile(stat = 'count') +
        scale_fill_viridis_c(option = "C") +
        # scale_fill_selection() +
        geom_hline(yintercept = 0, linetype = 2, colour = "red") +
        labs(x = "", y = "Minutes to sunrise", fill = "Number\nof\nPC") 
      
     
      
      
      (ptop + plot_spacer() + plot_layout(widths = c(1,0.1))) / 
          ( pmain + pside + plot_layout(widths = c(1,0.1))) +
          plot_layout(widths = c(1,0.1), heights = c(0.1,1), guides = 'collect')
      
      
      
  })
  
  output$year <- renderPlot({
      # If no point counts are in view, don't plot
      if (nrow(PCInBounds()) == 0) {
          return(NULL)
      }
      # plot(table(pc.sf$year))
      as.data.frame(table(PCInBounds()$year)) %>% 
      ggplot(aes(Var1, Freq)) +
          geom_bar(stat = 'identity') +
          labs(x = "Year", y = "Number of PC")
  })
  
  output$blocks <- renderPlot({
      sq_sum <- PCInBounds() %>% #PCInBounds() %>% 
          st_drop_geometry %>% 
          group_by(square_id) %>% 
          summarize(n=n_distinct(point_count_id))
      block_sum <- PCInBounds() %>% #PCInBounds() %>% 
          st_drop_geometry %>% 
          group_by(BLOCK_ID) %>% 
          summarize(n=n_distinct(point_count_id))
      region_sum <- PCInBounds() %>% #PCInBounds() %>% 
          st_drop_geometry %>% 
          group_by(REGION) %>% 
          summarize(n=n_distinct(point_count_id))
      p1 <- ggplot(block_sum, aes(n)) +
          geom_histogram(bins = 30) +
          labs(title = "Blocks", y = "Count", x = "Number of Point Counts")
      p2 <- ggplot(sq_sum, aes(n)) +
          geom_histogram(bins = 30) +
          labs(title = "Squares", y = "Count", x = "Number of Point Counts")
      p3 <- ggplot(region_sum, aes(n)) +
          geom_histogram(bins = 30) +
          labs(title = "Region", y = "Count", x = "Number of Point Counts")
      p1/p2/p3
      
  })
  
  # Show a popup at the given location
  showPCcodePopup <- function(point_count_id, lat, lng) {
      selectedPC <- pc.sf[pc.sf$point_count_id == point_count_id,]
      content <- as.character(tagList(
          tags$h4("Date:", selectedPC$ymd),
          tags$h4("Time:", selectedPC$timeid),
          tags$h4("Minutes to Sunrise:", round(selectedPC$min_tosunrise))#,
          # tags$strong(HTML(sprintf("%s, %s %s",
          #                          selectedPC$city.x, selectedZip$state.x, selectedZip$zipcode
          # ))), tags$br(),
          # sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
          # sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
          # sprintf("Adult population: %s", selectedZip$adultpop)
      ))
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = point_count_id)
  }
  
  # When map is clicked, show a popup with city info
  observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_shape_click
      if (is.null(event))
          return()
      
      isolate({
          showPCcodePopup(event$id, event$lat, event$lng)
      })
  })
  

  #     output$blocks <- renderPlot(
  #         {
  # dat_blocks <- pc.sf %>%
  #     st_drop_geometry %>%
  #
  #     left_join(
  #         st_drop_geometry(obba_sq) %>%
  #             dplyr::select(square_id =SQUARE_ID,
  #                           BLOCK_ID),
  #         by = "square_id"
  #             ) %>%
  #     group_by(square_id, BLOCK_ID) %>%
  #     summarise(n=n(), .groups='drop') %>%
  #     left_join(blocks, by = c("BLOCK_ID"="NAD83_100K")) %>%
  #     st_as_sf
  # ggplot(dat_blocks, aes(fill = n)) +
  #     geom_sf() + scale_fill_viridis_c("Number of PC")
  #         }
  #     )
  #
  # })
}
