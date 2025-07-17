# modules/difficulty_analysis.R

# UI for Difficulty Analysis Tab

difficulty_analysis_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "difficulty_analysis",
    fluidRow(
      box(width = 4, status = "primary", plotlyOutput(ns("difficulty_dist_chart"), height = "480px")),
      box(width = 4, status = "primary", plotlyOutput(ns("winstreak_by_difficulty_chart"), height = "480px")),
      box(width = 4, status = "primary", plotlyOutput(ns("churn_by_difficulty_chart"), height = "480px"))
    ),
    fluidRow(
      box(width = 6, status = "primary", plotlyOutput(ns("attempts_by_difficulty_chart"), height = "480px"))
    )
  )
}

# Server logic for Difficulty Analysis Tab

difficulty_analysis_server <- function(id, data, difficulty_colors) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$difficulty_dist_chart <- renderPlotly({
      df <- data()
      req(df, "labeled_difficulty" %in% names(df))
      
      # Use the reactive difficulty_colors value
      colors <- difficulty_colors()
      
      lvls <- names(colors)
      df$labeled_difficulty <- factor(df$labeled_difficulty, levels = lvls, ordered = TRUE)
      
      df_counts <- df %>% 
        count(labeled_difficulty) %>% 
        mutate(percentage = n / sum(n))
      
      if(nrow(df_counts) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      
      p <- ggplot(df_counts, aes(x = labeled_difficulty, y = percentage, fill = labeled_difficulty)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = scales::percent) +
        labs(title = "Distribution of Levels by Difficulty", x = "Difficulty Label", y = "Percentage of Total Levels") +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter"), 
              axis.title = element_text(family = "Inter"), 
              axis.text = element_text(family = "Inter")) +
        scale_fill_manual(values = colors) +
        guides(fill = "none") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
    })
    
    output$winstreak_by_difficulty_chart <- renderPlotly({
      df <- data()
      req(df, "winstreak_tier_3_rate" %in% names(df), "labeled_difficulty" %in% names(df))
      
      # Use the reactive difficulty_colors value
      colors <- difficulty_colors()
      
      lvls <- names(colors)
      df$labeled_difficulty <- factor(df$labeled_difficulty, levels = lvls, ordered = TRUE)
      
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      
      p <- ggplot(df, aes(x = labeled_difficulty, y = winstreak_tier_3_rate, fill = labeled_difficulty)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = "Tier 3 Win Streak Rate by Difficulty", x = "Difficulty Label", y = "Win Streak Tier 3 Rate") +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter")) +
        scale_fill_manual(values = colors) +
        guides(fill = "none") +
        scale_y_continuous(labels = scales::percent)
      ggplotly(p)
    })
    
    output$churn_by_difficulty_chart <- renderPlotly({
      df <- data()
      req(df, "churn_rate" %in% names(df), "labeled_difficulty" %in% names(df))
      
      # Use the reactive difficulty_colors value
      colors <- difficulty_colors()
      
      lvls <- names(colors)
      df$labeled_difficulty <- factor(df$labeled_difficulty, levels = lvls, ordered = TRUE)
      
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      
      p <- ggplot(df, aes(x = labeled_difficulty, y = churn_rate, fill = labeled_difficulty)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = "Churn Rate by Difficulty", x = "Difficulty Label", y = "Churn Rate") +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter")) +
        scale_fill_manual(values = colors) +
        guides(fill = "none") +
        scale_y_continuous(labels = scales::percent)
      ggplotly(p)
    })

    output$attempts_by_difficulty_chart <- renderPlotly({
      df <- data()
      req(df, "attempts_per_success" %in% names(df), "labeled_difficulty" %in% names(df))
      
      # Use the reactive difficulty_colors value
      colors <- difficulty_colors()
      
      lvls <- names(colors)
      df$labeled_difficulty <- factor(df$labeled_difficulty, levels = lvls, ordered = TRUE)
      
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      
      p <- ggplot(df, aes(x = labeled_difficulty, y = attempts_per_success, fill = labeled_difficulty)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = "Attempts per Success by Difficulty", x = "Difficulty Label", y = "Attempts per Success") +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter")) +
        scale_fill_manual(values = colors) +
        guides(fill = "none")
      ggplotly(p)
    })
  })
} 