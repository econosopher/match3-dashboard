# modules/economy_analysis.R

economy_analysis_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "economy_analysis",
    fluidRow(
      box(width = 6, status = "warning", plotlyOutput(ns("extra_moves_chart"), height = "480px")),
      box(width = 6, status = "warning", plotlyOutput(ns("near_win_loss_chart"), height = "480px"))
    ),
    fluidRow(
      box(width = 6, status = "warning", plotlyOutput(ns("extra_moves_by_level_chart"), height = "480px")),
      box(width = 6, status = "warning", plotlyOutput(ns("gold_sink_composition_chart"), height = "480px"))
    )
  )
}

economy_analysis_server <- function(id, data, difficulty_colors) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$extra_moves_chart <- renderPlotly({
      df <- data()
      req(df, "extra_moves_conversion" %in% names(df))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      generate_level_scatter(df, "extra_moves_conversion", "Extra Moves Conversion Rate", "Conversion Rate", difficulty_colors = difficulty_colors)
    })

    output$near_win_loss_chart <- renderPlotly({
      df <- data()
      req(df, "near_win_rate" %in% names(df), "near_loss_rate" %in% names(df))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      generate_level_scatter(df, "near_win_rate", "Near Win Rate", "Near Win Rate", color_var = "near_loss_rate", color_lab = "Near Loss Rate", difficulty_colors = difficulty_colors)
    })

    output$extra_moves_by_level_chart <- renderPlotly({
      df <- data()
      req(df, "extra_moves_offered" %in% names(df), "extra_moves_purchased" %in% names(df))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      generate_level_scatter(df, "extra_moves_conversion", "Extra Moves Conversion Rate by Level", "Conversion Rate", color_var = "level_number", color_lab = "Level", difficulty_colors = difficulty_colors)
    })

    output$gold_sink_composition_chart <- renderPlotly({
      df <- data()
      req(df, "level_number" %in% names(df), "median_gold_inventory_amount_first_attempt" %in% names(df), "median_customer_gold_inventory_amount_first_attempt" %in% names(df), "median_90th_percentile_customer_gold_inventory_amount_first_attempt" %in% names(df), "median_95th_percentile_customer_gold_inventory_amount_first_attempt" %in% names(df), "median_99th_percentile_customer_gold_inventory_amount_first_attempt" %in% names(df))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      df_long <- df %>%
        select(level_number, median_gold_inventory_amount_first_attempt, median_customer_gold_inventory_amount_first_attempt, median_90th_percentile_customer_gold_inventory_amount_first_attempt, median_95th_percentile_customer_gold_inventory_amount_first_attempt, median_99th_percentile_customer_gold_inventory_amount_first_attempt) %>%
        tidyr::pivot_longer(-level_number, names_to = "inventory_type", values_to = "amount")
      p <- ggplot(df_long, aes(x = level_number, y = amount, color = inventory_type)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 1.5) +
        labs(title = "Gold Inventory by Level", x = "Level Number", y = "Gold Inventory", color = "Inventory Type") +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter"))
      ggplotly(p)
    })
  })
} 