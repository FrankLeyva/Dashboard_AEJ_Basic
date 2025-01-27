# global.R
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

pal <- colorFactor(
  palette = "Set3",
  domain = districts_geo$No_Distrit
)
server <-  function(input, output, session) {
      
    rv <- reactiveValues(
        selected_district = NULL
    )
    
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
            )
    
    output$occupants_plot <- renderHighchart(
      highchart() %>% 
        hc_add_series(viviendas_data,'bar',hcaes(x=Año, y=Promedio_Ocupantes_Por_Vivienda),
          color = "#555bce", borderColor='black', inverted=F,
      dataLabels=list(enabled=TRUE, align='left',shape='square',
        color= '#808082', backgroundColor = 'white', 
        borderColor='#4c53cd',borderWidth= 1, padding = 3,x=10,format='{y:.1f}',style=list('font-weight'='bold'))
      ) %>% 
        hc_xAxis(reversed = FALSE, color='#808082', labels=list(style=list(fontSize='1.5em','font-weight'='bold'))) %>%
        hc_yAxis(visible=F) %>% 
        hc_plotOptions(series = list(borderRadius = 10,borderWidth= .5)) %>% 
        hc_credits(enabled = TRUE, text = '<b>Fuente:</b> Elaboración propia con datos de los Censos y Conteos de Población y Vivienda 2005, 2010 y 2020') %>% 
        hc_title(text = 'OCUPANTES POR VIVIENDAS', useHTML=T,
      style= list(
      color= '#808082',
      fontWeight= 'bold',
      'background-color'= 'white',
      border= '1px solid black',
      'padding-left'= '10px',
    'padding-right'= '10px')
    ) %>% 
        hc_legend(enabled=F) %>% 
        hc_tooltip(enabled=F))

        output$housing_type_plot <- renderHighchart(
          highchart() %>% 
          hc_add_series(sniiv_vivienda,'bar',hcaes(y=acciones, x=valor_vivienda), 
            color = "#E84066", borderColor='black', inverted=F,
        dataLabels=list(enabled=TRUE, align='left',shape='square',
          color= '#E84066',style=list('font-weight'='bold'))
        ) %>% 
          hc_xAxis(type='category',lineWidth='1',lineColor='black',categories=list(
           'Tradicional','Popular','Media','Residencial','Económica','Residencial plus','No disponible' 
          ),
            reversed = T, color='black', labels=list(style=list(fontSize='1.5em'))) %>%
          hc_yAxis(visible=F) %>% 
          hc_plotOptions(series = list(borderRadius = 10,borderWidth= .5)) %>% 
          hc_credits(enabled = TRUE, text = '<b>Fuente:</b> Elaboración propia con datos de los Censos y Conteos de Población y Vivienda 2005, 2010 y 2020') %>% 
          hc_title(text = 'FINANCIAMIENTO SOLICITADO POR TIPO DE VIVIENDA , 2023', useHTML=T,
        style= list(
        color= '#808082',
        fontWeight= 'bold',
        'background-color'= 'white',
        border= '1px solid black',
        'padding-left'= '10px',
      'padding-right'= '10px')
      ) %>% 
          hc_legend(enabled=F) %>% 
          hc_tooltip(enabled=F))
    
    output$financing_type_plot <- renderHighchart(
      highchart() %>% 
              hc_add_series(sniiv_modalidad,'bar',hcaes(y=acciones, x=modalidad),color = "#FC9E1C", borderColor='black', inverted=F,
            dataLabels=list(enabled=TRUE, align='left',shape='square',
              color= '#FC9E1C',style=list('font-weight'='bold'))
            ) %>% 
              hc_xAxis(type='category',lineWidth='1',lineColor='black',categories=list(
               'Viviendas existentes','Mejoramientos','Viviendas nuevas','Otros programas' 
              ),
                reversed = T, color='black', labels=list(style=list(fontSize='1.5em'))) %>%
              hc_yAxis(visible=F) %>% 
              hc_plotOptions(series = list(borderRadius = 10,borderWidth= .5)) %>% 
              hc_credits(enabled = TRUE, text = '<b>Fuente:</b> Elaboración propia con datos de los Censos y Conteos de Población y Vivienda 2005, 2010 y 2020') %>% 
              hc_title(text = 'FINANCIAMIENTO SOLICITADO POR MODALIDAD DE LA VIVIENDA, 2023', useHTML=T,
            style= list(
            color= '#808082',
            fontWeight= 'bold',
            'background-color'= 'white',
            border= '1px solid black',
            'padding-left'= '10px',
          'padding-right'= '10px')
          ) %>% 
              hc_legend(enabled=F) %>% 
              hc_tooltip(enabled=F))
    
    output$org_plot <- renderHighchart(
      highchart() %>% 
          hc_add_series(sniiv_organismo,'bar',hcaes(y=acciones, x=organismo),color = "#F5D963", borderColor='black', inverted=F,
        dataLabels=list(enabled=TRUE, align='left',shape='square',
          color= '#000000',style=list('font-weight'='bold'))
        ) %>% 
          hc_xAxis(type='category',lineWidth='1',lineColor='black',categories=list(
           'Infonavit','Banca (CNBV)','FOVISSTE','SHP¹','Banjercito','Otros' 
          ),
            reversed = T, color='black', labels=list(style=list(fontSize='1.5em'))) %>%
          hc_yAxis(visible=F) %>% 
          hc_plotOptions(series = list(borderRadius = 10,borderWidth= .5)) %>% 
          hc_credits(enabled = TRUE, text = '<b>Fuente:</b> Elaboración propia con datos de los Censos y Conteos de Población y Vivienda 2005, 2010 y 2020 <br/> 
          1: Sociedad Hipotecaria Federal', position=list(
            align= 'right',
            verticalAlign= 'bottom',
            x= -20,
            y= -20
          )) %>% 
          hc_title(text = 'FINANCIAMIENTO PARA VIVIENDA POR ORGANISMO, 2023', useHTML=T,
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
  
  # Map configuration
unique_districts <- unique(geo_data$No_Distrit)
color_palette <- c("#A1A3E8", "#F7D4C7", "#E5EBF0", "#ADDEE8", "#D6EDDE",
                   "#FAC7B2", "#FFE0C2", "#EDDBED", "#BAC2E3")
geo_data <-districts_geo[,c("Año","No_Distrit",'avg_satisfaction','avg_quality','avg_size','avg_location')]

jrzmap <- jsonlite::fromJSON("data/Cartografía/Jrz_Map.geojson",simplifyVector = F)

selected_label <- reactive({ 
  choices <- c( "Satisfacción" = "avg_satisfaction", "Calidad" = "avg_quality", "Tamaño" = "avg_size", "Ubicación" = "avg_location" ) 
      label <- names(choices)[choices == input$perception_metric] 
      return(label) })
  output$district_map <- renderHighchart({

      selected_label <- names(choices)[choices == input$perception_metric]
      geo_data <- district_metrics %>%
                    filter(Año == input$survey_year) %>% 
                    mutate(color = color_palette[match(Distrito, unique_districts)]) %>% 
                      mutate(avg_satisfaction_rounded = round(avg_satisfaction, 1))
                    highchart() %>%
                      hc_add_series_map(
                        animation = TRUE,
                        map = jrzmap,
                        geo_data,
                        value = input$perception_metric,
                        joinBy = c('No_Distrit','Distrito'),
                        name = selected_label,
                        dataLabels = list(enabled = FALSE),
                        tooltip = list(headerFormat ='',
                          pointFormat = 'Distrito:{point.No_Distrit} <br/>Promedio: {point.value}'
                        )
                      ) %>%
                      hc_plotOptions(
                        series = list(
                          animation = TRUE,
                          colorByPoint = TRUE
                        )
                      ) %>%
                      hc_legend(enabled = FALSE) %>%
                      hc_title(text='Percepción Ciudadana con respecto a la vivienda')
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
                  highchartOutput("occupancy_plot")
              )
          ),
          fluidRow(
              column(6, 
                 highchartOutput("occupation_ratio_plot")
              ),
              column(6,
                highchartOutput("occupants_plot")
              )
          )
      ),
      
      # SNIIV Financing Tab
      tabPanel("Financiamiento 2023",
          tabsetPanel(
              id = "financingTabs",
              tabPanel("Por Tipo de Vivienda",
              highchartOutput("housing_type_plot")
              ),
              tabPanel("Por Modalidad de la Vivienda",
              highchartOutput("financing_type_plot")
              ),
              tabPanel("Por Organismo",
              highchartOutput("org_plot")
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
                                "Satisfacción General" = "avg_satisfaction",
                                "Calidad de los materiales" = "avg_quality",
                                "Tamaño de la vivienda" = "avg_size",
                                "Ubicación de la vivienda" = "avg_location"
                            )),
                  width = 3
              ),
              mainPanel(
                  highchartOutput("district_map", height = "600px"),
                  width = 9
              )
          )
      )
  )
)
shinyApp(ui = ui, server = server)