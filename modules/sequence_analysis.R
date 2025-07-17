# modules/sequence_analysis.R

# UI for Sequence Analysis Tab
sequence_analysis_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "sequence_analysis",
    fluidRow(
      box(width = 12, status = "info",
        title = "Sequence Analysis Controls",
        column(4, sliderInput(ns("sequence_length"), "Sequence Length:", 
                             value = 10, min = 1, max = 50, step = 1)),
        column(4, actionButton(ns("update_sequence"), "Update Analysis", class = "btn-primary")),
        column(4, selectizeInput(ns("metrics"), "Y-Axis Metrics:",
                                choices = c("Win Rate" = "win_rate", "Churn Rate" = "churn_rate", "Attempts" = "total_attempts", "Players" = "total_players", "Retained" = "retained_at_end"),
                                selected = c("win_rate", "churn_rate"), multiple = TRUE))
      )
    ),
    fluidRow(
      box(width = 12, status = "primary", plotlyOutput(ns("sequence_trends"), height = "400px"))
    ),
    fluidRow(
      box(width = 6, status = "primary", plotlyOutput(ns("win_rate_sequence"), height = "400px")),
      box(width = 6, status = "primary", plotlyOutput(ns("churn_rate_sequence"), height = "400px"))
    )
  )
}

# Server logic for Sequence Analysis Tab
sequence_analysis_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive expression for sequence data
    sequence_data <- reactive({
      req(data(), input$sequence_length)
      sequence_grouped_data(data(), input$sequence_length)
    })
    
    # Sequence trends chart with selectable y-axis metrics
    output$sequence_trends <- renderPlotly({
      df <- sequence_data()
      req(df, nrow(df) > 0, input$metrics)
      
      df_with_index <- df %>% mutate(sequence_index = row_number())
      metric_names <- setNames(
        c("win_rate", "churn_rate", "total_attempts", "total_players", "retained_at_end"),
        c("Win Rate", "Churn Rate", "Attempts", "Players", "Retained")
      )
      selected_metrics <- input$metrics
      colors <- c("win_rate" = "#0072B2", "churn_rate" = "#D55E00", "total_attempts" = "#009E73", "total_players" = "#E69F00", "retained_at_end" = "#CC79A7")
      
      df_long <- df_with_index %>%
        tidyr::pivot_longer(
          cols = all_of(selected_metrics),
          names_to = "metric",
          values_to = "value"
        )
      
      p <- ggplot(df_long, aes(x = sequence_index, y = value, color = metric, group = metric)) +
        geom_line(size = 1.5) +
        geom_point(size = 3) +
        labs(
          title = "Sequence Trends by Sequence Number",
          subtitle = paste0("Sequence length: ", input$sequence_length, " levels"),
          x = "Sequence Number", y = "Value", color = "Metric"
        ) +
        scale_color_manual(
          values = colors[selected_metrics],
          labels = names(metric_names)[match(selected_metrics, metric_names)]
        ) +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter"))
      
      # Use percent scale for win_rate and churn_rate, else default
      if (all(selected_metrics %in% c("win_rate", "churn_rate"))) {
        p <- p + scale_y_continuous(labels = scales::percent)
      }
      ggplotly(p)
    })
    
    # Win rate by sequence chart
    output$win_rate_sequence <- renderPlotly({
      df <- sequence_data()
      req(df, nrow(df) > 0)
      
      p <- ggplot(df, aes(x = level_range, y = win_rate, group = 1)) +
        geom_line(color = "#0072B2", size = 1.5) +
        geom_point(color = "#0072B2", size = 3) +
        labs(
          title = "Win Rate by Level Sequence",
          subtitle = "wins / attempts",
          x = "Level Range", y = "Win Rate"
        ) +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter")) +
        scale_y_continuous(labels = scales::percent) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    })
    
    # Churn rate by sequence chart
    output$churn_rate_sequence <- renderPlotly({
      df <- sequence_data()
      req(df, nrow(df) > 0)
      
      p <- ggplot(df, aes(x = level_range, y = churn_rate, group = 1)) +
        geom_line(color = "#D55E00", size = 1.5) +
        geom_point(color = "#D55E00", size = 3) +
        labs(
          title = "Churn Rate by Level Sequence",
          subtitle = "1 - (retained_players_to_next_level / players)",
          x = "Level Range", y = "Churn Rate"
        ) +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter")) +
        scale_y_continuous(labels = scales::percent) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    })
  })
} 