# global.R
library(shiny)
library(plotly)
library(leaflet)
library(dplyr)
library(tidyr)
library(scales)  
library(sf)      
library(shinyjs) 
library(DT)      

viviendas_data <- read.csv("data/processed/viviendas_completo.csv")
sniiv_modalidad <- read.csv("data/processed/SNIIV_modalidad.csv")
sniiv_organismo <- read.csv("data/processed/SNIIV_organismo.csv")
sniiv_vivienda <- read.csv("data/processed/SNIIV_Vivienda.csv")
survey_data <- read.csv("data/processed/AEJ_2019_2023_Vivienda.csv")
districts_geo <- readRDS("data/processed/Map_Survey.rds")
create_time_series <- function(data, x, y, title) {
  plot_ly(data, x = ~get(x), y = ~get(y), type = 'scatter', mode = 'lines+markers') %>%
    layout(title = title,
           xaxis = list(title = x),
           yaxis = list(title = y))
}

pal <- colorFactor(
  palette = "Set3",
  domain = districts_geo$No_Distrit
)
server <-  function(input, output, session) {
      
    rv <- reactiveValues(
        selected_district = NULL
    )
    
    output$occupancy_plot <- renderPlotly({
        plot_ly(
          y = viviendas_data$Año,
          x = viviendas_data$viviendas_habitadas,
          name = "Vivienda particular habitada",
          type = "bar",
          orientation='h',
          color = I('aquamarine4')
        )
    })
    
    output$occupation_ratio_plot <- renderPlotly({

      plot_ly(
        x = factor(viviendas_data$Año[c(3,4,6)]),
        y = viviendas_data$viviendas_deshabitadas[c(3,4,6)],
        name = "Vivienda particular habitada",
        type = "bar",
        color = I('darkorange')
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
          orientation='h',
          color = I('cornflowerblue')
      )
    })
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
    output$district_map <- renderLeaflet({
        data <- districts_geo %>%
            filter(Año == input$survey_year)
        
        metric_values <- reactive({
          data[[input$perception_metric]]
        })
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
                  h3("Vivienda Particular Habitada"),
                  plotlyOutput("occupancy_plot")
              )
          ),
          fluidRow(
              column(6, 
                  h4("Vivienda Particular Deshabitada"),
                  plotlyOutput("occupation_ratio_plot")
              ),
              column(6,
                  h4("Ocupantes por Viviendas"),
                  plotlyOutput("occupants_plot")
              )
          )
      ),
      
      # SNIIV Financing Tab
      tabPanel("Financiamiento 2023",
          tabsetPanel(
              id = "financingTabs",
              tabPanel("Por Tipo de Vivienda",
                  plotlyOutput("housing_type_plot"),
                  DTOutput("housing_type_table")
              ),
              tabPanel("Por Modalidad de la Vivienda",
                  plotlyOutput("financing_type_plot"),
                  DTOutput("financing_type_table")
              ),
              tabPanel("Por Organismo",
                  plotlyOutput("org_plot"),
                  DTOutput("org_table")
              )
          )
      ),
      
      # District Perceptions Tab
      tabPanel("Percepción Ciudadana",
          sidebarLayout(
              sidebarPanel(
                  selectInput("survey_year", "Año:",
                            choices = 2019:2023,
                            selected = 2023),
                  selectInput("perception_metric", "Rasgo:",
                            choices = c(
                                "Satisfacción" = "avg_satisfaction",
                                "Calidad" = "avg_quality",
                                "Tamaño" = "avg_size",
                                "Ubicación" = "avg_location"
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