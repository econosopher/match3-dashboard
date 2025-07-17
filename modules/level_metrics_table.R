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
        "level_number", "labeled_difficulty", "attempts_per_success", "churn_rate", 
        "daily_win_rate", "near_win_rate", "near_loss_rate", "winstreak_tier_0_rate", "winstreak_tier_3_rate", 
        "extra_moves_conversion", "median_moves_remaining_win", "median_objective_pct_remaining_loss"
      )
      
      short_headers <- c(
        level_number = "Level",
        labeled_difficulty = "Diff",
        attempts_per_success = "Attempts",
        churn_rate = "Churn %",
        daily_win_rate = "DailyWin %",
        near_win_rate = "NearWin %",
        near_loss_rate = "NearLoss %",
        winstreak_tier_0_rate = "WStreak0 %",
        winstreak_tier_3_rate = "WStreak3 %",
        extra_moves_conversion = "EMoves %",
        median_moves_remaining_win = "Moves Left",
        median_objective_pct_remaining_loss = "ObjLeft %"
      )
      
      df_fmt <- df[, display_cols]
      
      # Apply bar charts to all numeric columns except level_number
      for (col in setdiff(names(df_fmt), "level_number")) {
        if (is.numeric(df_fmt[[col]])) {
          df_fmt[[col]] <- style_bar_chart(df_fmt[[col]])
        }
      }
      
      colors <- difficulty_colors()
      
      df_fmt$labeled_difficulty <- ifelse(
        is.na(df_fmt$labeled_difficulty),
        "",
        paste0('<span style="color:white; background:', colors[df_fmt$labeled_difficulty], '; border-radius:4px; padding:2px 8px;">', df_fmt$labeled_difficulty, '</span>')
      )
      
      datatable(
        df_fmt,
        escape = FALSE,
        filter = "top",
        rownames = FALSE,
        colnames = unname(short_headers[display_cols]),
        options = list(
          pageLength = 200,
          autoWidth = FALSE,
          scrollX = TRUE,
          dom = 'tip',
          headerCallback = JS(
            "function(thead, data, start, end, display){",
            "  $(thead).find('th').css({'font-size':'10px','padding':'2px','text-align':'center'});",
            "}"
          ),
          createdRow = JS(
            "function(row, data, dataIndex){",
            "  $(row).find('td').css({'font-size':'10px','padding':'2px'});",
            "}"
          ),
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(width = '50px', targets = 0:11)
          )
        )
      )
    })
    
    style_bar_chart <- function(values) {
      sapply(values, function(x) {
        if (is.na(x)) return("")
        # Normalize the value to a 0-100 scale for the bar width
        normalized_val <- (x - min(values, na.rm = TRUE)) / (max(values, na.rm = TRUE) - min(values, na.rm = TRUE)) * 100
        color <- "#0072B2" # Standard blue color
        paste0(
          '<div style="position: relative; height: 12px; background-color: #f1f1f1; border-radius: 3px;">',
          '<div style="position: absolute; left: 0; top: 0; height: 100%; width:', normalized_val,
          '%; background-color:', color, ';"></div>',
          '<div style="position: relative; z-index: 1; text-align: right; padding-right: 4px; font-size: 10px; color: black;">',
          round(x, 2),
          '</div>',
          '</div>'
        )
      })
    }
  })
} 