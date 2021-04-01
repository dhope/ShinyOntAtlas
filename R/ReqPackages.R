req_pkgs <- c("here", "shiny", "dplyr", "ggplot2", "tidyr", "patchwork", "renv", "readr")
missing <- req_pkgs[!req_pkgs %in% installed.packages() ]
install.packages(missing)
