library(shiny)
library(plotly)
library(leaflet)
library(dplyr)
library(sf)     # For spatial data handling

# Load data
districts_with_metrics <- readRDS('data/processed/Map_Survey.rds')

# Function to get color based on value
get_color_palette <- colorNumeric(
  palette = "YlOrRd",
  domain = NULL  # Will set based on selected metric
)

# Define UI
ui <- fluidPage(
  titlePanel("District Housing Metrics"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("metric", "Choose metric:", 
                  choices = c("Satisfaction" = "avg_satisfaction",
                            "Quality" = "avg_quality",
                            "Size" = "avg_size"
                            )),
      
      # Add a legend
      tags$div(id = "legend")
    ),
    
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive expression for the selected metric's values
  metric_values <- reactive({
    districts_with_metrics[[input$metric]]
  })
  
  output$map <- renderLeaflet({
    # Update color palette based on current metric
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = metric_values()
    )
    
    leaflet(districts_with_metrics) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(metric_values()),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        
        # Add labels
        label = ~paste0(
          "District: ", No_Distrit, "<br>",
          input$metric, ": ", round(metric_values(), 2)
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        
        # Add highlighting
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      # Add legend
      addLegend(
        pal = pal,
        values = metric_values(),
        opacity = 0.7,
        title = input$metric,
        position = "bottomright"
      )
  })
}

# Create and return the Shiny app object
shinyApp(ui = ui, server = server)