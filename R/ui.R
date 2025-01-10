ui <- page_navbar(
  title = "AEJ Dashboard 2024",
  theme = bs_theme(
    version = 5,
    bootswatch = "morph",
    primary = "#0d6efd",
    # Add custom fonts if needed
    base_font = font_google("Inter"),
    heading_font = font_google("Montserrat")
  ),
  
  # Demographics tab
  nav_panel(
    "Demografía",
    page_sidebar(
      sidebar = sidebar(
        # Filters
        selectInput("age_group", "Grupo de Edad",
                   choices = unique(survey_data$age_group)),
        selectInput("gender", "Género",
                   choices = unique(survey_data$gender)),
        hr(),
        downloadButton("download_demo", "Descargar Datos")
      ),
      
      # Main content
      layout_columns(
        value_box(
          title = "Total Encuestados",
          value = textOutput("total_respondents"),
          showcase = bs_icon("people-fill")
        ),
        value_box(
          title = "Edad Promedio",
          value = textOutput("avg_age"),
          showcase = bs_icon("person-badge")
        ),
        value_box(
          title = "% Mujeres",
          value = textOutput("pct_women"),
          showcase = bs_icon("gender-female")
        )
      ),
      
      layout_columns(
        card(
          card_header("Distribución por Edad"),
          plotlyOutput("age_dist")
        ),
        card(
          card_header("Distribución por Género"),
          plotlyOutput("gender_dist")
        )
      ),
      
      card(
        card_header("Datos Detallados"),
        DT::dataTableOutput("demographics_table")
      )
    )
  ),
  
  # Economic Indicators tab
  nav_panel(
    "Economía",
    page_sidebar(
      sidebar = sidebar(
        # Economic filters
        selectInput("income_group", "Grupo de Ingreso",
                   choices = unique(survey_data$income_group)),
        downloadButton("download_econ", "Descargar Datos")
      ),
      
      # Main economic content
      layout_columns(
        value_box(
          title = "Ingreso Promedio",
          value = textOutput("avg_income"),
          showcase = bs_icon("cash")
        ),
        value_box(
          title = "% Satisfacción Económica",
          value = textOutput("economic_satisfaction"),
          showcase = bs_icon("graph-up")
        )
      ),
      
      card(
        card_header("Distribución de Ingresos"),
        plotlyOutput("income_dist")
      )
    )
  ),
  
  # Add more nav panels for other sections
  nav_spacer(),
  
  # About section
  nav_panel(
    "Acerca de",
    card(
      card_header("Acerca del Dashboard"),
      card_body(
        "Este dashboard presenta los resultados de la Encuesta de Percepción Ciudadana de Ciudad Juárez 2024."
      )
    )
  )
)
