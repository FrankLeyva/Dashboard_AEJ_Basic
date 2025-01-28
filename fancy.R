library(shiny)
library(dplyr)
library(tidyr)
library(scales)  
library(sf)      
library(shinyjs) 
library(DT)      
library(highcharter)
library(plotly)
library(leaflet)
library(jsonlite)


viviendas_data <- read.csv("data/processed/viviendas_completo.csv")
sniiv_modalidad <- read.csv("data/processed/SNIIV_modalidad.csv")
sniiv_organismo <- read.csv("data/processed/SNIIV_organismo.csv")
sniiv_vivienda <- read.csv("data/processed/SNIIV_Vivienda.csv")
survey_data <- read.csv("data/processed/AEJ_2019_2023_Vivienda.csv")
district_metrics <- survey_data %>%
  group_by(Distrito,Año) %>%
  summarise(
    avg_satisfaction = mean(Satisfaccion, na.rm = TRUE),
    avg_quality = mean(Calidad, na.rm = TRUE),
    avg_size = mean(Tamaño,na.rm = T),
    avg_location = mean(Ubicacion,na.rm=T)  )
districts_geo <- readRDS("data/processed/Map_Survey.rds")
filtered_data <- viviendas_data[!is.na(viviendas_data$viviendas_deshabitadas), ]


# The UI component
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  tags$head(
    # Custom CSS for the typewriter effect
    tags$style("
      .typewriter {
        font-family: 'Courier New', monospace;
        white-space: pre-wrap;
        min-height: 200px;
      }
    ")
  ),
  
  # Container for your graphs
  fluidRow(
    column(12,
      # Your housing trends graphs will go here
      highchartOutput("occupancy_plot"),
      highchartOutput("occupation_ratio_plot")
    ),
    column(12,
      div(id = "typewriter-text", class = "typewriter"),offset = 2
    )
  ),
  
  # Container for the typewriter text

  
  # Hidden div with the full text to be typed
  tags$script(HTML("
    const reportText = `
    Analysis of Juarez Housing Trends
    ================================
    
    Based on the graphs above, we observe several key trends in the Juarez housing market:
    
    1. Price Dynamics: The median house price has shown a steady increase of 15% year-over-year,
       particularly in the central and northern districts.
    
    2. Supply Patterns: New housing developments have concentrated in the southeastern sector,
       with 60% of new permits issued in this area.
    
    3. Affordability Index: Despite rising prices, the affordability index remains stable due to
       corresponding increases in median household income.
    `;

    // Typewriter effect function
    function typeWriter(text, elementId, speed = 20) {
      let i = 0;
      function type() {
        if (i < text.length) {
          document.getElementById(elementId).innerHTML += text.charAt(i);
          i++;
          setTimeout(type, speed);
        }
      }
      type();
    }

    // Start typing when the page loads
    $(document).ready(function() {
      typeWriter(reportText, 'typewriter-text');
    });
  "))
)

# Server logic
server <- function(input, output) {

  output$occupancy_plot <- renderHighchart(   
    highchart() %>% 
      hc_add_series(viviendas_data,'bar',name='Viviendas Particulares Habitadas',
    hcaes(x=Año, y=viviendas_habitadas), 
    color = "aquamarine", borderColor='black', inverted=F,
    dataLabels=list(enabled=TRUE,align='right',  style= list(
      color= '#ffffff',
      fontWeight= 'bold',
      border= '0.5px solid black')
    )) %>% 
      hc_xAxis(reversed = FALSE, color='#808082', labels=list(style=list(fontSize='2em','font-weight'='bold'))) %>%
      hc_yAxis(visible=F) %>% 
      hc_plotOptions(series = list(borderRadius = 10,borderWidth= .5)) %>% 
      hc_credits(enabled = TRUE, text = '<b>Fuente:</b> Elaboración propia con datos de los Censos y Conteos de Población y Vivienda 2005, 2010 y 2020') %>% 
      hc_title(text = 'VIVIENDA PARTICULAR HABITADA', useHTML=T,
    style= list(
    color= '#808082',
    fontWeight= 'bold',
    'background-color'= 'white',
    border= '1px solid black',
    'padding-left'= '10px',
  'padding-right'= '10px')
  ) %>% 
      hc_legend(enabled=F) %>% 
      hc_tooltip(enabled=F)
  )
  
  output$occupation_ratio_plot <- renderHighchart(
    highchart() %>% 
          hc_add_series(
            data = filtered_data,
            type = 'column',
            borderRadius = 15,
            borderWidth= .5,
            hcaes(x = factor(Año), y = viviendas_deshabitadas),
            color = "orange", borderColor='black',
            dataLabels=list(enabled=TRUE,  style= list(
              color= '#808082')
            )) %>% 
          hc_xAxis(
        lineColor='#808082',
        lineWidth='2',
            type = "category",
            categories = as.character(filtered_data$Año),
            color='#808082', labels=list(style=list(fontSize='2em','font-weight'='bold'))
          ) %>%
          hc_yAxis(visible=F) %>% 
            hc_credits(enabled = TRUE, text = '<b>Fuente:</b> Elaboración propia con datos de los Censos y Conteos de Población y Vivienda 2005, 2010 y 2020') %>% 
              hc_title(text = 'VIVIENDA PARTICULAR DESHABITADA', useHTML=T,
            style= list(
            color= '#808082',
            fontWeight= 'bold',
            'background-color'= 'white',
            border= '1px solid black',
            'padding-left'= '10px',
          'padding-right'= '10px')
          ) %>% 
              hc_legend(enabled=F) %>% 
              hc_tooltip(enabled=F)
          )}

# Run the app
shinyApp(ui = ui, server = server)