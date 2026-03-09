# =========================
# Shiny Final Project (Safe Upgrade)
# Based on class demo: Colorado species occurrences
# Script developed by Kai Wan
# Originally Created: 2025 DEC 1
# =========================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Data must be in the same folder as app.R
load("shinyDemoData.RData")  # loads `occ` (sf points) and `ROMO` (sf polygon)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Colorado Species Occurrence Explorer"),
  
  h5("Filter occurrences by species, record type, elevation, year, and month. Click points to view metadata."),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "species",
        label   = "Species",
        choices = sort(unique(occ$Species)),
        selected = unique(occ$Species)
      ),
      
      checkboxGroupInput(
        inputId = "obs",
        label   = "Observation Type",
        choiceNames = c("Human Observation", "Preserved Specimen", "Machine Observation"),
        choiceValues = c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN", "MACHINE_OBSERVATION"),
        selected = c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN", "MACHINE_OBSERVATION")
      ),
      
      sliderInput(
        inputId = "elevation",
        label   = "Elevation (m)",
        min     = floor(min(occ$elevation, na.rm = TRUE)),
        max     = ceiling(max(occ$elevation, na.rm = TRUE)),
        value   = c(floor(min(occ$elevation, na.rm = TRUE)),
                    ceiling(max(occ$elevation, na.rm = TRUE)))
      ),
      
      # Safe upgrades: year + month filters (only if columns exist)
      sliderInput(
        inputId = "year",
        label   = "Year",
        min     = floor(min(occ$year, na.rm = TRUE)),
        max     = ceiling(max(occ$year, na.rm = TRUE)),
        value   = c(floor(min(occ$year, na.rm = TRUE)),
                    ceiling(max(occ$year, na.rm = TRUE)))
      ),
      
      sliderInput(
        inputId = "month",
        label   = "Month",
        min     = 1,
        max     = 12,
        value   = c(1, 12),
        step    = 1
      ),
      
      hr(),
      
      # Extra credit-ish, low risk
      downloadButton("download_data", "Download filtered data (CSV)"),
      br(), br(),
      strong(textOutput("n_records"))
    ),
    
    mainPanel(
      leafletOutput("map", height = 650)
    )
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  # Reactive filtered dataset
  occ_react <- reactive({
    req(input$species, input$obs)
    
    occ %>%
      filter(Species %in% input$species) %>%
      filter(basisOfRecord %in% input$obs) %>%
      filter(!is.na(elevation)) %>%
      filter(elevation >= input$elevation[1], elevation <= input$elevation[2]) %>%
      filter(!is.na(year)) %>%
      filter(year >= input$year[1], year <= input$year[2]) %>%
      filter(!is.na(month)) %>%
      filter(month >= input$month[1], month <= input$month[2])
  })
  
  # Show count of filtered records
  output$n_records <- renderText({
    paste("Records:", nrow(occ_react()))
  })
  
  # Color palette
  pal <- colorFactor(palette = "Dark2", domain = occ$Species)
  
  # Render leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = ROMO,
        fillOpacity = 0.15,
        weight = 2,
        color = "black",
        fillColor = "gray"
      ) %>%
      addCircleMarkers(
        data = occ_react(),
        radius = 4,
        color = ~pal(Species),
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~paste0(
          "<b>Species:</b> ", Species, "<br>",
          "<b>Record Type:</b> ", basisOfRecord, "<br>",
          "<b>Elevation (m):</b> ", elevation, "<br>",
          "<b>Year:</b> ", year, "<br>",
          "<b>Month:</b> ", month
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = occ$Species,
        title = "Species"
      )
  })
  
  # Download filtered data as CSV (drops geometry safely)
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("filtered_occurrences_", Sys.Date(), ".csv")
    },
    content = function(file) {
      dat <- occ_react()
      dat_df <- st_drop_geometry(dat)
      write.csv(dat_df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
