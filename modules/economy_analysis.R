# modules/economy_analysis.R

economy_analysis_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "economy_analysis",
    fluidRow(
      box(width = 6, status = "warning", plotlyOutput(ns("extra_moves_chart"), height = "480px")),
      box(width = 6, status = "warning", plotlyOutput(ns("extra_moves_offered_chart"), height = "480px"))
    ),
    fluidRow(
      box(width = 12, status = "warning", plotlyOutput(ns("gold_sink_composition_chart"), height = "480px"))
    )
  )
}

economy_analysis_server <- function(id, data, difficulty_colors, okabe_ito) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$extra_moves_chart <- renderPlotly({
      df <- data()
      req(df, "extra_moves_conversion" %in% names(df))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      generate_level_scatter(df, "extra_moves_conversion", "Extra Moves Conversion Rate", "Conversion Rate",
                             subtitle = "extra_moves_purchased / extra_moves_offered",
                             y_percent = TRUE, color_var = "attempts_per_success", color_lab = "Attempts per Success")
    })

    output$extra_moves_offered_chart <- renderPlotly({
      df <- data()
      req(df, "extra_moves_offered" %in% names(df), "attempts" %in% names(df))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      
      df <- df %>% mutate(extra_moves_offered_per_attempt = extra_moves_offered / attempts)
      
      generate_level_scatter(df, "extra_moves_offered_per_attempt", "Extra Moves Offered per Attempt", "Offered per Attempt",
                             subtitle = "extra_moves_offered / attempts",
                             y_percent = FALSE, color_var = "attempts_per_success", color_lab = "Attempts per Success")
    })

    output$gold_sink_composition_chart <- renderPlotly({
      df <- data()
      req(df, "level_number" %in% names(df), "median_gold_inventory_amount_first_attempt" %in% names(df), "median_customer_gold_inventory_amount_first_attempt" %in% names(df), "median_90th_percentile_customer_gold_inventory_amount_first_attempt" %in% names(df), "median_95th_percentile_customer_gold_inventory_amount_first_attempt" %in% names(df), "median_99th_percentile_customer_gold_inventory_amount_first_attempt" %in% names(df))
      if(nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = "No data available"))
      
      df_long <- df %>%
        select(level_number, 
               "Median" = median_gold_inventory_amount_first_attempt, 
               "P50" = median_customer_gold_inventory_amount_first_attempt, 
               "P90" = median_90th_percentile_customer_gold_inventory_amount_first_attempt, 
               "P95" = median_95th_percentile_customer_gold_inventory_amount_first_attempt, 
               "P99" = median_99th_percentile_customer_gold_inventory_amount_first_attempt) %>%
        tidyr::pivot_longer(-level_number, names_to = "inventory_type", values_to = "amount")
      
      p <- ggplot(df_long, aes(x = level_number, y = amount, color = inventory_type)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 1.5) +
        scale_color_manual(values = okabe_ito) +
        labs(
          title = "Gold Inventory by Level", 
          x = "Level Number", y = "Gold Inventory", color = "Inventory Type") +
        theme_fivethirtyeight() +
        theme(text = element_text(family = "Inter"))
      
      ggplotly(p) %>%
        layout(title = list(text = paste0("Gold Inventory by Level",
                                          "<br>",
                                          "<sup>",
                                          "median_gold_inventory_amount_first_attempt",
                                          "</sup>"), x = 0))
    })
  })
} 