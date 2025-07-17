# modules/level_metrics_table.R

level_metrics_table_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "level_metrics",
    box(width = 12, status = "primary", DT::dataTableOutput(ns("level_dt_table")))
  )
}

level_metrics_table_server <- function(id, data, difficulty_colors) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$level_dt_table <- DT::renderDataTable({
      df <- data()
      req(df)
      display_cols <- c(
        "level_number", "labeled_difficulty", "win_rate", "churn_rate", "attempts_per_success", 
        "daily_win_rate", "near_win_rate", "near_loss_rate", "winstreak_tier_3_rate", 
        "extra_moves_conversion", "median_moves_remaining_win", "median_objective_pct_remaining_loss"
      )
      short_headers <- c(
        level_number = "Level",
        labeled_difficulty = "Diff",
        win_rate = "Win %",
        churn_rate = "Churn %",
        attempts_per_success = "Attempts",
        daily_win_rate = "DailyWin %",
        near_win_rate = "NearWin %",
        near_loss_rate = "NearLoss %",
        winstreak_tier_3_rate = "WStreak %",
        extra_moves_conversion = "EMoves %",
        median_moves_remaining_win = "Moves Left",
        median_objective_pct_remaining_loss = "ObjLeft %"
      )
      percent_cols <- c("win_rate", "churn_rate", "daily_win_rate", "near_win_rate", "near_loss_rate", "winstreak_tier_3_rate", "extra_moves_conversion", "median_objective_pct_remaining_loss")
      int_cols <- c("level_number", "median_moves_remaining_win", "attempts_per_success")
      df_fmt <- df[, display_cols]
      for (col in intersect(percent_cols, names(df_fmt))) {
        pct <- pmax(0, pmin(1, df_fmt[[col]]))
        bar_html <- sprintf(
          '<div style="background:#0072B2;width:%d%%;height:16px;border-radius:3px;text-align:right;padding-right:4px;font-size:11px;line-height:16px;color:white;">%d%%</div>',
          as.integer(100*pct), as.integer(round(100*pct))
        )
        df_fmt[[col]] <- bar_html
      }
      for (col in intersect(int_cols, names(df_fmt))) {
        df_fmt[[col]] <- as.integer(round(df_fmt[[col]]))
      }
      
      # Use the reactive difficulty_colors value
      colors <- difficulty_colors()
      
      df_fmt$labeled_difficulty <- ifelse(
        is.na(df_fmt$labeled_difficulty),
        "",
        paste0('<span style="color:white; background:', colors[df_fmt$labeled_difficulty], '; border-radius:4px; padding:2px 8px;">', df_fmt$labeled_difficulty, '</span>')
      )
      DT::datatable(
        df_fmt,
        escape = FALSE,
        filter = "top",
        rownames = FALSE,
        colnames = unname(short_headers[display_cols]),
        options = list(
          pageLength = 200,
          autoWidth = TRUE,
          scrollX = TRUE,
          dom = 'tip',
          headerCallback = JS(
            "function(thead, data, start, end, display){",
            "  $(thead).find('th').css({'font-size':'12px','padding':'4px 2px','text-align':'center'});",
            "}"
          )
        )
      )
    })
  })
} 