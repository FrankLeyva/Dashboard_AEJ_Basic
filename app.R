# global.R
library(shiny)
library(plotly)
library(leaflet)
library(dplyr)
library(tidyr)
library(scales)  # For nice number formatting
library(sf)      # For spatial data
library(shinyjs) # For UI enhancements
library(DT)      # For any data tables we might need

# Load and process datasets
# INEGI Data
viviendas_data <- read.csv("data/processed/viviendas_completo.csv")

# SNIIV Data
sniiv_modalidad <- read.csv("data/processed/SNIIV_modalidad.csv")
sniiv_organismo <- read.csv("data/processed/SNIIV_organismo.csv")
sniiv_vivienda <- read.csv("data/processed/SNIIV_Vivienda.csv")

# Survey Data
survey_data <- read.csv("data/processed/AEJ_2019_2023_Vivienda.csv")

# Load district map data
districts_geo <- readRDS("data/processed/Map_Survey.rds")

# Helper functions for plots
create_time_series <- function(data, x, y, title) {
  plot_ly(data, x = ~get(x), y = ~get(y), type = 'scatter', mode = 'lines+markers') %>%
    layout(title = title,
           xaxis = list(title = x),
           yaxis = list(title = y))
}

# Color palettes and other common elements
pal <- colorFactor(
  palette = "Set3",
  domain = districts_geo$No_Distrit
)

server <-  function(input, output, session) {
      
    # Reactive values for shared state
    rv <- reactiveValues(
        selected_district = NULL
    )
    
    # INEGI Demographics Tab
    output$occupancy_plot <- renderPlotly({
        # Time series of total housing units
        plot_ly(
          y = viviendas_data$Año,
          x = viviendas_data$viviendas_habitadas,
          name = "Vivienda particular habitada",
          type = "bar",
          orientation='h'
        )
    })
    
    output$occupation_ratio_plot <- renderPlotly({

      plot_ly(
        x = factor(viviendas_data$Año[c(3,4,6)]),
        y = viviendas_data$viviendas_deshabitadas[c(3,4,6)],
        name = "Vivienda particular habitada",
        type = "bar"
      ) %>%
      layout(
        xaxis = list(
          categoryorder = "category"
        )
      )

    })
    
    output$occupants_plot <- renderPlotly({
      plot_ly(
        y = viviendas_data$Año,
        x = viviendas_data$Promedio_Ocupantes_Por_Vivienda ,
        name = "Vivienda particular habitada",
        type = "bar",
          orientation='h'
      )
    })
    
    # SNIIV Financing Tab
    output$housing_type_plot <- renderPlotly({
      plot_ly(
        y = sniiv_vivienda$valor_vivienda,
        x = sniiv_vivienda$acciones ,
        name = "Vivienda particular habitada",
        type = "bar",
        orientation='h'
      )
    })
    
    output$financing_type_plot <- renderPlotly({
      plot_ly(
        y = sniiv_modalidad$modalidad,
        x = sniiv_modalidad$acciones,
        name = "Vivienda particular habitada",
        type = "bar",
        orientation='h'
      )
    })
    
    output$org_plot <- renderPlotly({
      plot_ly(
        y = sniiv_organismo$organismo,
        x = sniiv_organismo$acciones ,
        name = "Vivienda particular habitada",
        type = "bar",
        orientation='h'
      )
    })
    selected_label <- reactive({ 

      choices <- c( "Satisfaction" = "avg_satisfaction", "Quality" = "avg_quality", "Size" = "avg_size", "Location" = "avg_location" ) 
      label <- names(choices)[choices == input$perception_metric] 
      return(label) })
    # District Perceptions Tab
    output$district_map <- renderLeaflet({
        # Filter data based on selected year and metric
        data <- districts_geo %>%
            filter(Año == input$survey_year)
        
        metric_values <- reactive({
          data[[input$perception_metric]]
        })
        # Create leaflet map
        # Implementation here
        leaflet(districts_geo) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(districts_geo$No_Distrit),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.5,
            label = ~paste0(
              "District: ", No_Distrit,' ',
              selected_label(), ": ", round(metric_values(), 1)
            ),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            ), 
            highlightOptions = highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE
            )
          
          ) 
    })
    
    output$perception_summary <- renderPlotly({
        # Bar chart summarizing perceptions
        # Implementation here
    })
    
    # Tables
    output$housing_type_table <- renderDT({
        # Detailed table view
        # Implementation here
    })
    
    # Observers for interactivity
    observeEvent(input$mainNav, {
        # Handle tab changes
    })
    
    observeEvent(input$survey_year, {
        # Update map and charts when year changes
    })
  }
  
# ui.R
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  
  # Custom CSS
  tags$head(
      tags$style(HTML("
          .nav-tabs > li > a {
              padding: 10px 15px;
              font-weight: 600;
          }
          .tab-content {
              padding: 20px 0;
          }
      "))
  ),
  
  # Title
  titlePanel("Vivienda"),
  
  # Main navigation
  navbarPage(
      title = NULL,  # Remove duplicate title
      id = "mainNav",
      
      # INEGI Demographics Tab
      tabPanel("Ocupación",
          fluidRow(
              column(12, 
                  h3("Housing Occupancy Trends"),
                  plotlyOutput("occupancy_plot")
              )
          ),
          fluidRow(
              column(6, 
                  h4("Occupied vs Unoccupied"),
                  plotlyOutput("occupation_ratio_plot")
              ),
              column(6,
                  h4("Occupants per House"),
                  plotlyOutput("occupants_plot")
              )
          )
      ),
      
      # SNIIV Financing Tab
      tabPanel("Financing",
          tabsetPanel(
              id = "financingTabs",
              tabPanel("Housing Type",
                  plotlyOutput("housing_type_plot"),
                  DTOutput("housing_type_table")
              ),
              tabPanel("Financing Type",
                  plotlyOutput("financing_type_plot"),
                  DTOutput("financing_type_table")
              ),
              tabPanel("Organizations",
                  plotlyOutput("org_plot"),
                  DTOutput("org_table")
              )
          )
      ),
      
      # District Perceptions Tab
      tabPanel("District Perceptions",
          sidebarLayout(
              sidebarPanel(
                  selectInput("survey_year", "Year:",
                            choices = 2019:2023,
                            selected = 2023),
                  selectInput("perception_metric", "Metric:",
                            choices = c(
                                "Satisfaction" = "avg_satisfaction",
                                "Quality" = "avg_quality",
                                "Size" = "avg_size",
                                "Location" = "avg_location"
                            )),
                  width = 3
              ),
              mainPanel(
                  leafletOutput("district_map", height = "600px"),
                  plotlyOutput("perception_summary"),
                  width = 9
              )
          )
      )
  )
)
shinyApp(ui = ui, server = server)