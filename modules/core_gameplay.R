# modules/core_gameplay.R

core_gameplay_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "core_gameplay",
    fluidRow(
      box(width = 6, status = "info", plotlyOutput(ns("attempts_per_success_chart"), height = "480px")),
      box(width = 6, status = "info", plotlyOutput(ns("win_streak_chart"), height = "480px"))
    ),
    fluidRow(
      box(width = 6, status = "info", plotlyOutput(ns("daily_win_rate_chart"), height = "480px")),
      box(width = 6, status = "info", plotlyOutput(ns("avg_active_day_chart"), height = "480px"))
    ),
    fluidRow(
      box(width = 12, status = "info", plotlyOutput(ns("churn_chart"), height = "480px"))
    ),
    fluidRow(
      box(width = 12, status = "info", plotlyOutput(ns("winstreak_attempts_per_success_chart"), height = "480px"))
    )
  )
}

core_gameplay_server <- function(id, data, difficulty_colors) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$attempts_per_success_chart <- renderPlotly({
      df <- data()
      req(df, "attempts_per_success" %in% names(df))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      generate_level_scatter(df, "attempts_per_success", "Attempts per Success", "Attempts", 
                             subtitle = "attempts / wins",
                             difficulty_colors = difficulty_colors())
    })

    output$win_streak_chart <- renderPlotly({
      df <- data()
      req(df, startsWith(names(df), "winstreak_tier_"))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      df_long <- df %>%
        select(level_number, starts_with("winstreak_tier_")) %>%
        tidyr::pivot_longer(cols = -level_number, names_to = "tier", values_to = "rate", names_pattern = "winstreak_tier_(.)_rate") %>%
        mutate(tier = paste("Tier", tier))
      
      tier_colors <- setNames(RColorBrewer::brewer.pal(length(unique(df_long$tier)), "Set2"), unique(df_long$tier))
      
      p <- ggplot(df_long, aes(x = level_number, y = rate, color = tier, group = tier)) +
        geom_point(aes(text = paste("Level:", level_number)), alpha = 0.5) +
        geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
        labs(
          title = "First Attempt Win Streak Rates", 
          x = "Level Number", y = "Rate per Player", color = "Win Streak Tier") +
        scale_color_manual(values = tier_colors) +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter"))
      
      ggplotly(p, tooltip = "text") 
    })

    output$daily_win_rate_chart <- renderPlotly({
      df <- data()
      req(df, "daily_win_rate" %in% names(df))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      generate_level_scatter(df, "daily_win_rate", NULL, NULL,
                             subtitle = "avg_daily_wins_before_first_attempt / avg_daily_attempts_before_first_attempt",
                             difficulty_colors = difficulty_colors(), y_percent = TRUE)
    })

    output$avg_active_day_chart <- renderPlotly({
      df <- data()
      req(df, "avg_active_day_on_first_attempt" %in% names(df))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      generate_level_scatter(df, "avg_active_day_on_first_attempt", "Average Active Day on First Attempt", "Active Days", 
                             subtitle = "avg_active_day_on_first_attempt",
                             difficulty_colors = difficulty_colors())
    })

    output$churn_chart <- renderPlotly({
      df <- data()
      req(df, "churn_rate" %in% names(df))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      generate_level_scatter(df, "churn_rate", "Churn Rate", "Churn Rate", 
                             subtitle = "1 - (retained_players_to_next_level / players)",
                             difficulty_colors = difficulty_colors())
    })
    
    output$winstreak_attempts_per_success_chart <- renderPlotly({
      df <- data()
      req(df, nrow(df) > 0)
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      
      # Calculate attempts per success for each tier
      tier_data <- df %>%
        mutate(
          tier_1_attempts_per_success = ifelse(first_attempt_winstreak_tier_1_wins > 0, first_attempt_win_streak_tier_1 / first_attempt_winstreak_tier_1_wins, NA),
          tier_2_attempts_per_success = ifelse(first_attempt_winstreak_tier_2_wins > 0, first_attempt_win_streak_tier_2 / first_attempt_winstreak_tier_2_wins, NA),
          tier_3_attempts_per_success = ifelse(first_attempt_winstreak_tier_3_wins > 0, first_attempt_win_streak_tier_3 / first_attempt_winstreak_tier_3_wins, NA)
        ) %>%
        select(level_number, labeled_difficulty, tier_1_attempts_per_success, tier_2_attempts_per_success, tier_3_attempts_per_success) %>%
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
      
      p <- ggplot(tier_data, aes(x = level_number, y = attempts_per_success, color = tier, group = tier)) +
        geom_point(aes(text = paste("Level:", level_number, "\nTier:", tier, "\nAttempts per Success:", round(attempts_per_success, 2))), alpha = 0.7) +
        geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
        labs(
          title = "Win Streak Attempts per Success by Tier",
          subtitle = "first_attempt_win_streak_tier_n / first_attempt_winstreak_tier_n_wins",
          x = "Level Number", 
          y = "Attempts per Success",
          color = "Win Streak Tier"
        ) +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter")) +
        scale_color_manual(values = c("Tier 1" = "#56B4E9", "Tier 2" = "#009E73", "Tier 3" = "#D55E00"))
      
      ggplotly(p, tooltip = "text")
    })
  })
} 