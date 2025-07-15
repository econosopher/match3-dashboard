# modules/sequence_analysis.R

sequence_analysis_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "sequence_analysis",
    # Remove the top fluidRow with Sequence Configuration and metric picker
    # Remove the main sequence_plot chart
    hr(),
    fluidRow(
      box(width = 6, status = "primary", plotlyOutput(ns("win_rate_by_sequence_chart"), height = "480px")),
      box(width = 6, status = "primary", plotlyOutput(ns("churn_rate_by_sequence_chart"), height = "480px"))
    )
  )
}

sequence_analysis_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sequence_data <- reactive({
      req(data(), input$sequence_length)
      df <- data()
      df %>%
        mutate(level_group = floor((level_number - 1) / input$sequence_length)) %>%
        group_by(level_group) %>%
        summarise(
          start_level = min(level_number),
          end_level = max(level_number),
          level_range = paste(min(level_number), "-", max(level_number)),
          total_players = sum(players, na.rm = TRUE),
          total_wins = sum(wins, na.rm = TRUE),
          total_attempts = sum(attempts, na.rm = TRUE),
          retained_at_end = last(retained_players_to_next_level, order_by = level_number),
          win_rate = ifelse(total_attempts > 0, total_wins / total_attempts, 0),
          churn_rate = ifelse(total_players > 0, 1 - (retained_at_end / total_players), 0),
          .groups = 'drop'
        )
    })

    output$sequence_metric_selector <- renderUI({
      choices <- c("win_rate", "churn_rate")
      selectInput(ns("sequence_metric"), "Metric:", choices = choices, selected = "churn_rate")
    })

    observeEvent(sequence_data(), {
      if(is.null(input$sequence_metric))
        updateSelectInput(session, "sequence_metric", selected = "win_rate")
    })

    output$sequence_plot <- renderPlotly({
      req(data(), input$sequence_length, input$sequence_metric)
      df <- data()
      group_size <- input$sequence_length
      df <- df %>% mutate(
        group = floor((level_number - 1) / group_size) + 1,
        window_index = (level_number - 1) %% group_size + 1
      )
      metric <- input$sequence_metric
      req(metric %in% names(df))
      df <- df %>% filter(!is.na(.data[[metric]]))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      p <- ggplot(df, aes(x = window_index, y = .data[[metric]], color = as.factor(group))) +
        geom_point(size = 2, alpha = 0.8) +
        geom_line(aes(group = group), alpha = 0.7) +
        labs(title = paste("Sequence Analysis by Group"), x = "Window Index", y = snakecase::to_title_case(metric), color = "Group") +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter"))
      ggplotly(p, tooltip = "text")
    })

    sequence_grouped_data <- reactive({
      req(data(), input$sequence_length)
      df <- data()
      if (is.null(df) || !"level_number" %in% names(df) || is.null(input$sequence_length)) return(data.frame())
      df %>%
        mutate(level_group = floor((level_number - 1) / input$sequence_length)) %>%
        group_by(level_group) %>%
        summarise(
          start_level = min(level_number),
          end_level = max(level_number),
          level_range = paste(min(level_number), "-", max(level_number)),
          total_players = sum(players, na.rm = TRUE),
          total_wins = sum(wins, na.rm = TRUE),
          total_attempts = sum(attempts, na.rm = TRUE),
          retained_at_end = last(retained_players_to_next_level, order_by = level_number),
          win_rate = ifelse(total_attempts > 0, total_wins / total_attempts, 0),
          churn_rate = ifelse(total_players > 0, 1 - (retained_at_end / total_players), 0),
          .groups = 'drop'
        )
    })

    output$win_rate_by_sequence_chart <- renderPlotly({
      df <- sequence_grouped_data()
      if(is.null(df) || nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      p <- ggplot(df, aes(x = level_range, y = win_rate, group=1)) +
        geom_boxplot(aes(fill=level_range), alpha = 0.7) +
        labs(title = "Win Rate by Level Sequence", x = "Level Sequence", y = "Win Rate") +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter")) +
        guides(fill = "none") +
        scale_y_continuous(labels = scales::percent) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    })

    output$churn_rate_by_sequence_chart <- renderPlotly({
      df <- sequence_grouped_data()
      if(is.null(df) || nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      p <- ggplot(df, aes(x = level_range, y = churn_rate, group=1)) +
        geom_boxplot(aes(fill=level_range), alpha = 0.7) +
        labs(title = "Churn Rate by Level Sequence", x = "Level Sequence", y = "Churn Rate") +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter")) +
        guides(fill = "none") +
        scale_y_continuous(labels = scales::percent) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    })
  })
} 