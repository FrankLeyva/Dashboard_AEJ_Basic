library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(thematic)
library(lubridate)
library(dplyr)
library(bsicons)
library(DT)
library(wordcloud)
thematic::thematic_shiny(font = "auto")
Q1.1 <- textOutput('Q1.1Perc')
Q1.2 <- textOutput('Q1.2Perc')
Q1.3 <- textOutput('Q1.3Perc')
# Define UI for application that draws a histogram
page_navbar( 

  nav_panel('Economía',
            page_sidebar(
              accordion(
                accordion_panel(
                  title = 'Cambio de economía en el ultimo año',
                  icon = bs_icon("graph-up-arrow"),
              layout_columns(value_box(title= 'Respondieron que su situación económica EMPEORÓ',Q1.1,
              showcase =bs_icon('graph-down-arrow'),theme='danger',
              full_screen = FALSE, fill = FALSE,height = 200L),
            value_box(title= 'Respondieron que su situación económica permaneció IGUAL', Q1.2,
             showcase =bs_icon('dash-square'),
            theme='warning', full_screen = FALSE, 
            fill = FALSE,height = 200L),
            value_box(title= 'Respondieron que su situación económica MEJORÓ',Q1.3,
             showcase =bs_icon('graph-up-arrow'),
            theme='success', full_screen = FALSE, 
            fill = FALSE,height = 200L),value_box(title= 'Porcentaje sin respuesta',textOutput('Q1.NA.Perc'), showcase=bs_icon('chat'),
                                                  theme='light', full_screen = FALSE, 
                                                  fill = FALSE, height = 200L),),
            layout_columns(card(plotlyOutput('Q1.plot'), full_screen = TRUE), card(downloadButton('D1.table','Descargar Tabla'),
                                                                                   dataTableOutput('Q1.table'), full_screen=T)
  ),
              ),
  accordion_panel(
    title = 'Alcanza con el ingreso de la vivienda?',
    icon = bs_icon("piggy-bank"),
    layout_columns(value_box(title= 'Respondieron que NO les alcanza',textOutput('Q2.1'),
                             showcase =bs_icon('bank'),theme='danger',
                             full_screen = FALSE, fill = FALSE,height = 200L),
                   value_box(title= 'Respondieron que Si les alcanza', textOutput('Q2.2'),
                             showcase =bs_icon('wallet2'),
                             theme='warning', full_screen = FALSE, 
                             fill = FALSE,height = 200L),
                   value_box(title= 'Porcentaje sin respuesta',
                             textOutput('Q2.NA'), showcase=bs_icon('chat'),
                             theme='light', full_screen = FALSE, 
                             fill = FALSE, height = 200L),
                   ),
    layout_columns(card(plotOutput("Q2.plot"), full_screen = TRUE), card(downloadButton('D2.table','Descargar Tabla'),
                                                                           dataTableOutput('Q2.table'), full_screen=T)
    
  ),
  ),
  accordion_panel(
    title = 'Ha pensado en irse de El Paso?',
    icon = bs_icon("suitcase-lg"),
    layout_columns(value_box(title= 'Nunca ha pensado irse',textOutput('Q3.1'),
                             showcase =bs_icon('house-fill'),theme='primary',
                             full_screen = FALSE, fill = FALSE,height = 200L),
                   value_box(title= 'Ha pensado en irse', textOutput('Q3.2'),
                             showcase =bs_icon('airplane'),
                             theme='warning', full_screen = FALSE, 
                             fill = FALSE,height = 200L),
                   value_box(title= 'Porcentaje sin respuesta',
                             textOutput('Q3.NA'), showcase=bs_icon('chat'),
                             theme='light', full_screen = FALSE, 
                             fill = FALSE, height = 200L),
    ),
    layout_columns(card(plotlyOutput("Q3.plot"), full_screen = TRUE), card(downloadButton('D3.table','Descargar Tabla'),
                                                                         dataTableOutput('Q3.table'), full_screen=T)
                   
    ),
  ),
    
              ),
  
  sidebar =   sidebar(checkboxGroupInput( 
    "District_filter", "Distrito", 
    c( 
      "Distrito 1" = "Distrito 1", 
      "Distrito 2" = "Distrito 2", 
      "Distrito 3" = "Distrito 3",
      "Distrito 4" = "Distrito 4",
      "Distrito 5" = "Distrito 5",
      "Distrito 6" = "Distrito 6",
      "Distrito 7" = "Distrito 7",
      "Distrito 8" = "Distrito 8"
    ))
    ),
  ),
  ),
  title = "This is El Paso",
  theme = bs_theme(bootswatch = "superhero")
  
  
)