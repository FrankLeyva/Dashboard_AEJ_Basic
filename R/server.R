server <- function(input, output, session) {
  # Reactive filtered dataset
  filtered_data <- reactive({
    survey_data %>%
      filter(
        age_group == input$age_group,
        gender == input$gender
      )
  })
  
  # Demographics outputs
  output$total_respondents <- renderText({
    nrow(filtered_data())
  })
  
  output$avg_age <- renderText({
    mean(filtered_data()$age, na.rm = TRUE) %>%
      round(1)
  })
  
  output$pct_women <- renderText({
    filtered_data() %>%
      summarise(pct = mean(gender == "F", na.rm = TRUE) * 100) %>%
      pull(pct) %>%
      round(1) %>%
      paste0("%")
  })
  
  # Plots
  output$age_dist <- renderPlotly({
    p <- filtered_data() %>%
      ggplot(aes(x = age_group, fill = gender)) +
      geom_bar(position = "dodge") +
      theme_minimal() +
      labs(x = "Grupo de Edad", y = "Cantidad",
           title = "Distribución por Edad y Género")
    
    ggplotly(p)
  })
  
  output$gender_dist <- renderPlotly({
    p <- filtered_data() %>%
      ggplot(aes(x = gender, fill = gender)) +
      geom_bar() +
      theme_minimal() +
      labs(x = "Género", y = "Cantidad",
           title = "Distribución por Género")
    
    ggplotly(p)
  })
  
  # Tables
  output$demographics_table <- DT::renderDataTable({
    filtered_data() %>%
      select(age_group, gender, income_group) %>%
      DT::datatable(
        options = list(pageLength = 10),
        rownames = FALSE
      )
  })
  
  # Downloads
  output$download_demo <- downloadHandler(
    filename = function() {
      paste0("demographics_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}