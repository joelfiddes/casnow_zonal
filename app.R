library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)

# Color scale definitions
# palettes <- list(
#   swe = colorNumeric(palette = c("#313695", "white", "#a50026"), domain = c(-150, 150), na.color = "transparent"),
#   snd = colorNumeric(palette = c("#2c7bb6", "white", "#d7191c"), domain = c(-0.5, 0.5), na.color = "transparent"),
#   rof = colorNumeric(palette = c("#8c510a", "white", "#01665e"), domain = c(-1, 1), na.color = "transparent"),
#   gst = colorNumeric(palette = "plasma", domain = c(0, 8), na.color = "transparent"),
#   alb = colorNumeric(palette = c("#762a83", "white", "#1b7837"), domain = c(-0.1, 0.1), na.color = "transparent")
# )


# Color scale definitions
palettes <- list(
  swe = colorNumeric(palette = c("#313695", "white", "#a50026"), domain = c(-150, 150), na.color = "transparent"),
  snd = colorNumeric(palette = c("#2c7bb6", "white", "#d7191c"), domain = c(-0.5, 0.5), na.color = "transparent"),
  rof = colorNumeric(palette = c("#8c510a", "white", "#01665e"), domain = c(-1, 1), na.color = "transparent"),
  gst = colorNumeric(palette = "plasma", domain = c(0, 8), na.color = "transparent"),
  alb = colorNumeric(palette = c("#762a83", "white", "#1b7837"), domain = c(-0.1, 0.1), na.color = "transparent")
)

units <- c(
  swe = "mm",
  snd = "m",
  rof = "mm",
  gst = "°C",
  alb = "unitless"
)

variables <- c("gst", "rof", "swe", "snd", "alb")
scenarios <- c("ssp126", "ssp245", "ssp585")

ui <- fluidPage(
      # Logos with links
  fluidRow(
    style = "position: fixed; bottom: 10px; left: 10px; z-index: 1000; display: flex; gap: 15px; align-items: center;",
    tags$a(href = "https://www.unesco.org", target = "_blank",
           tags$img(src = "unesco.png", height = "30px")),
    tags$a(href = "https://mountainfutures.ch/", target = "_blank",
           tags$img(src = "mf.png", height = "80px")),
    tags$a(href = "https://www.thegef.org/", target = "_blank",
           tags$img(src = "gef.png", height = "90px"))
  ),

  titlePanel("21st Century Snow Climate Scenarios for Central Asia: Catchments"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable:", choices = variables, selected = "swe"),
      selectInput("scenario", "Select Scenario:", choices = scenarios, selected = "ssp126"),
        wellPanel(
    tags$p(tags$b("Basin Average Anomaly :")),
tags$p("This app presents 21st-century snow climate scenarios for Central Asia using the TopoCLIM model chain (Fiddes et al., 2022). 
It integrates terrain clustering, high-resolution climate downscaling, and bias-corrected future climate projections (CMIP6 SSP2-4.5 & SSP5-8.5).
 The FSM snow model then simulates key snow variables—such as snow water equivalent, depth, and runoff—across different time periods (2000-2020, 2040-2060, 2080-2100). 
 This approach captures complex mountain influences to assess climate change impacts on snow conditions in the region.")

  )
    ),
    mainPanel(
      leafletOutput("mapPlot", height = "800px")
    )
  )
)

# Define palettes and domains as data, not color functions
palette_info <- list(
  swe = list(colors = c("#313695", "white", "#a50026"), domain = c(-150, 150)),
  snd = list(colors = c("#2c7bb6", "white", "#d7191c"), domain = c(-0.5, 0.5)),
  rof = list(colors = c("#8c510a", "white", "#01665e"), domain = c(-1, 1)),
  gst = list(colors = "plasma", domain = c(0, 8)),
  alb = list(colors = c("#762a83", "white", "#1b7837"), domain = c(-0.1, 0.1))
)

server <- function(input, output, session) {
  
  basins_data <- reactive({
    # ... your existing reactive code ...
  })
  
  output$mapPlot <- renderLeaflet({
    basins <- basins_data()
    req(basins)
    
    # Create palette fresh with fixed domain
    pal <- colorNumeric(
      palette = palette_info[[input$variable]]$colors,
      domain = palette_info[[input$variable]]$domain,
      na.color = "transparent"
    )
    


    leaflet(basins) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(mean),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~paste0("Mean anomaly: ", round(mean, 2), " ", units[[input$variable]]),
        highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend(
        pal = pal,
        values = palette_info[[input$variable]]$domain,
 
        title = paste(toupper(input$variable), "Anomaly (", units[[input$variable]], ")", sep = ""),
        position = "bottomright"
      )
  })
}


server <- function(input, output, session) {

basins_data <- reactive({
  file_path <- file.path("data", paste0(input$variable, "_", input$scenario, "_zonal_stats.geojson"))
  if (!file.exists(file_path)) {
    showNotification(paste("File not found:", file_path), type = "error")
    return(NULL)
  }
  
  data <- st_read(file_path, quiet = TRUE)
  

  
  # Check bounds and geometry
  print(st_bbox(data))
  print(summary(data$mean))
  print(any(!st_is_valid(data)))  # TRUE means some geometries are invalid
  
  # Fix invalid geometries if needed
  if (any(!st_is_valid(data))) {
    data <- st_make_valid(data)
  }
  
  data
})



  output$mapPlot <- renderLeaflet({
    basins <- basins_data()
    req(basins)

    pal <- palettes[[input$variable]]

    leaflet(basins) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(mean),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~paste0("Mean anomaly: ", round(mean, 2), " ", units[[input$variable]]),
        highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend(
        pal = pal,
                        values = palette_info[[input$variable]]$domain, 
        title = paste(toupper(input$variable), "Anomaly (", units[[input$variable]], ")", sep = ""),
        position = "bottomright"
      )
  })
}

shinyApp(ui, server)
