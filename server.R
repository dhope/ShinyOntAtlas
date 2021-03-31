# Stolen from https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/server.R
library(shiny)
require(dplyr)
require(leaflet)
library(ggplot2)

source(here::here("R/dataImport.R"))


function(input, output, session) {
  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -87.42, lat = 49.8963, zoom = 6)
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
    sizeBy <- input$size


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
    radius <- pc.sf[[sizeBy]] / max(pc.sf[[sizeBy]]) * 200
    # }

    leafletProxy("map", data = pc.sf) %>%
      clearShapes() %>%
      addCircles(~X, ~Y,
        radius = radius,
        layerId = ~pc.sf,
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
      ggplot(
        data = PCInBounds(),
        aes(lubridate::mdy("01-01-2003") + doy, min_tosunrise, z = 1)
      ) +
          scale_x_date(limits = lubridate::mdy("01-01-2003") +range(pc.sf$doy)) +
          scale_y_continuous(limits = range(pc.sf$min_tosunrise)) +
        stat_summary_2d( fun = "sum", geom = "tile") +
        # geom_tile(stat = 'count') +
        scale_fill_viridis_c(option = "C") +
        # scale_fill_selection() +
        geom_hline(yintercept = 0, linetype = 2, colour = "red") +
        labs(x = "", y = "Minutes to sunrise", fill = "Number\nof\nPC")
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
