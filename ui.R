library(leaflet)

# Choices for drop-downs
vars <- c(
  "Year" = "year",
  "Month" = "month",
  "Minutes to Sunrise" = "m2s",
  "Roadside" = "roadside"
)


navbarPage("Interactive map", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("PC explorer"),
                                      
                                      selectInput("color", "Color", vars),
                                      selectInput("size", "Size", vars, selected = "year"),
                                      # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                      #                  # Only prompt for threshold when coloring or sizing by superzip
                                      #                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      plotOutput("time_day", height = 250),
                                      plotOutput("year", height = 200)
                        ),
                        
                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('Ontario Breeding Bird Atlas'), ' by Birds Canada.'
                        )
                    )
           ))#,
           
           # tabPanel("Data explorer",
           #          fluidRow(
           #            column(3,
           #                   selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
           #            ),
           #            column(3,
           #                   conditionalPanel("input.states",
           #                                    selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
           #                   )
           #            ),
           #            column(3,
           #                   conditionalPanel("input.states",
           #                                    selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
           #                   )
           #            )
           #          ),
           #          fluidRow(
           #            column(1,
           #                   numericInput("minScore", "Min score", min=0, max=100, value=0)
           #            ),
           #            column(1,
           #                   numericInput("maxScore", "Max score", min=0, max=100, value=100)
           #            )
           #          ),
           #          hr(),
           #          DT::dataTableOutput("ziptable")
           # ),
           # 
           # conditionalPanel("false", icon("crosshair"))
# )