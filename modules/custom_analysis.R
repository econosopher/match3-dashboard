# modules/custom_analysis.R

custom_analysis_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "custom_analysis",
    fluidRow(
      box(
        width = 12, status = "primary",
        fluidRow(
          column(6, uiOutput(ns("x_axis_selector_ui"))),
          column(6, uiOutput(ns("y_axis_selector_ui")))
        )
      )
    ),
    fluidRow(
      box(
        width = 12, status = "primary",
        plotlyOutput(ns("custom_scatter_plot"), height = "600px")
      )
    )
  )
}

custom_analysis_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    metric_choices <- reactive({
      df <- data()
      req(df)
      numerics <- df[,sapply(df, is.numeric)]
      # Include level_number and exclude some non-metric variables
      choices <- setdiff(names(numerics), c("players", "wins", "attempts", "retained_players_to_next_level"))
      snakecase::to_title_case(choices)
    })

    output$x_axis_selector_ui <- renderUI({
      selectInput(ns("x_axis"), "Select X-Axis Metric:", 
                  choices = metric_choices(),
                  selected = "Level Number")
    })
    
    output$y_axis_selector_ui <- renderUI({
      selectInput(ns("y_axis"), "Select Y-Axis Metric:", 
                  choices = metric_choices(),
                  selected = "Win Rate")
    })

    output$custom_scatter_plot <- renderPlotly({
      req(input$x_axis, input$y_axis)
      df <- data()
      
      x_var <- snakecase::to_snake_case(input$x_axis)
      y_var <- snakecase::to_snake_case(input$y_axis)
      
      req(x_var %in% names(df), y_var %in% names(df))

      p <- ggplot(df, aes_string(x = x_var, y = y_var)) +
        geom_point(aes(text = paste("Level:", level_number, 
                                    "<br>Difficulty:", labeled_difficulty)), alpha = 0.6) +
        geom_smooth(method = "loess", se = FALSE) +
        labs(
          title = paste(input$y_axis, "vs.", input$x_axis),
          subtitle = NULL,
          x = input$x_axis,
          y = input$y_axis
        ) +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter"))

      ggplotly(p, tooltip = "text")
    })
  })
} 