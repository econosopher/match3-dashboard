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
      generate_level_scatter(df, "attempts_per_success", "Attempts per Success", "Attempts", difficulty_colors = difficulty_colors)
    })

    output$win_streak_chart <- renderPlotly({
      df <- data()
      req(df, startsWith(names(df), "winstreak_tier_"))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      df_long <- df %>%
        select(level_number, starts_with("winstreak_tier_")) %>%
        tidyr::pivot_longer(cols = -level_number, names_to = "tier", values_to = "rate", names_pattern = "winstreak_tier_(.)_rate") %>%
        mutate(tier = paste("Tier", tier))
      p <- ggplot(df_long, aes(x = level_number, y = rate, color = tier)) +
        geom_point(aes(text = paste("Level:", level_number)), alpha = 0.5) +
        geom_smooth(aes(color = tier), method = "loess", se = FALSE, linewidth = 0.8) +
        labs(title = "First Attempt Win Streak Rates", x = "Level Number", y = "Rate per Player", color = "Win Streak Tier") +
        scale_color_manual(values = difficulty_colors) +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter"))
      ggplotly(p, tooltip = "text")
    })

    output$daily_win_rate_chart <- renderPlotly({
      df <- data()
      req(df, "daily_win_rate" %in% names(df))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      generate_level_scatter(df, "daily_win_rate", "Daily Win Rate", "Win Rate", difficulty_colors = difficulty_colors)
    })

    output$avg_active_day_chart <- renderPlotly({
      df <- data()
      req(df, "avg_active_day_on_first_attempt" %in% names(df))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      generate_level_scatter(df, "avg_active_day_on_first_attempt", "Average Active Day on First Attempt", "Active Days", difficulty_colors = difficulty_colors)
    })

    output$churn_chart <- renderPlotly({
      df <- data()
      req(df, "churn_rate" %in% names(df))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      generate_level_scatter(df, "churn_rate", "Churn Rate", "Churn Rate", difficulty_colors = difficulty_colors)
    })
  })
} 