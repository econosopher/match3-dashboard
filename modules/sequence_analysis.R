# modules/sequence_analysis.R

# UI for Sequence Analysis Tab
sequence_analysis_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "sequence_analysis",
    fluidRow(
      box(width = 12, status = "info",
        title = "Sequence Analysis Controls",
        column(6, sliderInput(ns("sequence_length"), "Sequence Length:", 
                             value = 10, min = 1, max = 50, step = 1)),
        column(6, actionButton(ns("update_sequence"), "Update Analysis", class = "btn-primary"))
      )
    ),
    fluidRow(
      box(width = 12, status = "primary",
        div(style = "margin-bottom: 10px;",
          selectizeInput(ns("metrics"), "Y-Axis Metric:",
            choices = c(
              "Attempts per Success" = "attempts_per_success", 
              "Win Rate" = "win_rate", 
              "Churn Rate" = "churn_rate", 
              "Attempts" = "total_attempts", 
              "Players" = "total_players", 
              "Retained" = "retained_at_end",
              "Win Streak Tier 0 Attempt Rate" = "winstreak_tier_0_attempt_rate",
              "Win Streak Tier 1 Attempt Rate" = "winstreak_tier_1_attempt_rate", 
              "Win Streak Tier 2 Attempt Rate" = "winstreak_tier_2_attempt_rate",
              "Win Streak Tier 3 Attempt Rate" = "winstreak_tier_3_attempt_rate"
            ),
            selected = c("attempts_per_success"), multiple = FALSE)
        ),
        plotlyOutput(ns("sequence_trends"), height = "400px"),
        DT::dataTableOutput(ns("sequence_table"))
      )
    ),
    fluidRow(
      box(width = 6, status = "primary", plotlyOutput(ns("win_rate_sequence"), height = "400px")),
      box(width = 6, status = "primary", plotlyOutput(ns("winstreak_tier3_sequence"), height = "400px"))
    ),
    fluidRow(
      box(width = 12, status = "primary", plotlyOutput(ns("winstreak_attempts_per_success"), height = "400px"))
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
      sequence_grouped_data(data(), input$sequence_length) %>% mutate(sequence_index = row_number())
    })
    
    # Add this reactive to the server
    individual_sequence_data <- reactive({
      req(data(), input$sequence_length)
      data() %>%
        mutate(
          level_group = floor((level_number - 1) / input$sequence_length),
          sequence_index = (level_number - 1) %% input$sequence_length + 1,
          sequence_index = as.integer(sequence_index),
          level_range = paste(
            level_group * input$sequence_length + 1,
            "-",
            (level_group + 1) * input$sequence_length
          )
        )
    })
    
    # Sequence trends chart grouped and colored by level_range
    output$sequence_trends <- renderPlotly({
      df <- individual_sequence_data()
      req(df, nrow(df) > 0, input$metrics)
      metric <- input$metrics
      
      p <- ggplot(df, aes(x = sequence_index, y = .data[[metric]], group = level_range, color = level_range)) +
        geom_line(size = 1.5) +
        geom_point(size = 3) +
        labs(
          title = "Sequence Trends by Sequence Number",
          subtitle = paste0("Sequence length: ", input$sequence_length, " levels"),
          x = "Level Number",
          y = names(which(c(
            attempts_per_success = "Attempts per Success",
            win_rate = "Win Rate",
            churn_rate = "Churn Rate",
            total_attempts = "Attempts",
            total_players = "Players",
            retained_at_end = "Retained",
            winstreak_tier_0_attempt_rate = "Win Streak Tier 0 Attempt Rate",
            winstreak_tier_1_attempt_rate = "Win Streak Tier 1 Attempt Rate",
            winstreak_tier_2_attempt_rate = "Win Streak Tier 2 Attempt Rate",
            winstreak_tier_3_attempt_rate = "Win Streak Tier 3 Attempt Rate"
          ) == metric)), color = "Level Range"
        ) +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter"))
      
      ggplotly(p)
    })

    
    # Win rate by sequence chart
    output$win_rate_sequence <- renderPlotly({
      df <- sequence_data()
      req(df, nrow(df) > 0)
      
      p <- ggplot(df, aes(x = level_range, y = win_rate, group = 1)) +
        geom_line(color = "#009E73", size = 1.5) +
        geom_point(color = "#009E73", size = 3) +
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
    
    # Win Streak Tier 3 Attempt Rate by sequence chart
    output$winstreak_tier3_sequence <- renderPlotly({
      df <- sequence_data()
      req(df, nrow(df) > 0, "winstreak_tier_3_attempt_rate" %in% names(df))
      
      p <- ggplot(df, aes(x = level_range, y = winstreak_tier_3_attempt_rate, group = 1)) +
        geom_line(color = "#D55E00", size = 1.5) +
        geom_point(color = "#D55E00", size = 3) +
        labs(
          title = "Win Streak Tier 3 Attempt Rate by Level Sequence",
          subtitle = "first_attempt_win_streak_tier_3 / players",
          x = "Level Range", y = "Tier 3 Attempt Rate"
        ) +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter")) +
        scale_y_continuous(labels = scales::percent) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    })
    
    # Win Streak Attempts per Success by tier chart
    output$winstreak_attempts_per_success <- renderPlotly({
      df <- sequence_data()
      req(df, nrow(df) > 0)
      
      # Calculate attempts per success for each tier
      tier_data <- df %>%
        mutate(
          tier_1_attempts_per_success = ifelse(first_attempt_winstreak_tier_1_wins > 0, first_attempt_win_streak_tier_1 / first_attempt_winstreak_tier_1_wins, NA),
          tier_2_attempts_per_success = ifelse(first_attempt_winstreak_tier_2_wins > 0, first_attempt_win_streak_tier_2 / first_attempt_winstreak_tier_2_wins, NA),
          tier_3_attempts_per_success = ifelse(first_attempt_winstreak_tier_3_wins > 0, first_attempt_win_streak_tier_3 / first_attempt_winstreak_tier_3_wins, NA)
        ) %>%
        select(level_range, tier_1_attempts_per_success, tier_2_attempts_per_success, tier_3_attempts_per_success) %>%
        pivot_longer(
          cols = starts_with("tier_"),
          names_to = "tier",
          values_to = "attempts_per_success"
        ) %>%
        mutate(
          tier = case_when(
            tier == "tier_1_attempts_per_success" ~ "Tier 1", 
            tier == "tier_2_attempts_per_success" ~ "Tier 2",
            tier == "tier_3_attempts_per_success" ~ "Tier 3"
          )
        )
      
      p <- ggplot(tier_data, aes(x = level_range, y = attempts_per_success, group = tier, color = tier)) +
        geom_line(size = 1.5) +
        geom_point(size = 3) +
        labs(
          title = "Win Streak Attempts per Success by Tier",
          subtitle = "first_attempt_win_streak_tier_n / first_attempt_winstreak_tier_n_wins",
          x = "Level Range", 
          y = "Attempts per Success",
          color = "Win Streak Tier"
        ) +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(values = c("Tier 1" = "#56B4E9", "Tier 2" = "#009E73", "Tier 3" = "#D55E00"))
      
      ggplotly(p)
    })
  })
} 