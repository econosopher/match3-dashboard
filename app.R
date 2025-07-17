if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, DT, ggplot2, dplyr, gt, gtExtras, plotly, ggthemes, ggridges, snakecase, tidyr)

# At the top of app.R, source the config and helpers
source("utils/config.R")
source("utils/plotting_helpers.R")
source("modules/difficulty_analysis.R")
source("modules/core_gameplay.R")
source("modules/economy_analysis.R")
source("modules/sequence_analysis.R")
source("modules/level_metrics_table.R")
source("modules/static_level_data.R")

# 1. Set global font for all ggplot charts
suppressPackageStartupMessages(library(ggplot2))
theme_set(theme_fivethirtyeight(base_family = "Inter"))

# --- Expected Headers ---
expected_headers <- c(
    "blocker_type", "feature_introduced", "move_limit", "objective_type", "objective_amount",
    "avg_daily_wins_before_first_attempt", "avg_daily_attempts_before_first_attempt",
    "avg_active_day_on_first_attempt", "near_loss_wins_2_or_less_moves", "near_win_losses_5_percent_or_less_objective",
    "level_number", "labeled_difficulty", "median_attempts_per_winner", "players", "mid_match_quits", "attempts",
    "wins", "first_attempt_win_streak_tier_0", "first_attempt_win_streak_tier_1", "first_attempt_win_streak_tier_2",
    "first_attempt_win_streak_tier_3", "median_moves_remaining_win", "median_objective_pct_remaining_loss",
    "median_base_coins_rewarded_win", "median_move_coins_rewarded", "first_attempt_winstreak_tier_1_wins",
    "first_attempt_winstreak_tier_2_wins", "first_attempt_winstreak_tier_3_wins", "extra_moves_offered",
    "extra_moves_purchased", "extra_moves_wins", "gold_extra_move_purchased", "median_pgb_inventory_amount_first_attempt",
    "median_igb_inventory_amount_first_attempt", "median_gold_inventory_amount_first_attempt",
    "median_customer_gold_inventory_amount_first_attempt", "median_90th_percentile_customer_gold_inventory_amount_first_attempt",
    "median_95th_percentile_customer_gold_inventory_amount_first_attempt", "median_99th_percentile_customer_gold_inventory_amount_first_attempt",
    "pgb_purchased", "gold_pgb_sink", "pgb_amount_used", "igb_purchased", "igb_gold_pbg_sink", "igb_amount_used",
    "average_moves_coins_rewarded", "balance_patch_version", "retained_players_to_next_level"
)

# --- UI Definition ---
ui <- dashboardPage(
    dashboardHeader(title = "Match-3 Balancing"),
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("Level Metrics Table", tabName = "level_metrics", icon = icon("table")),
            menuItem("Core Analysis", icon = icon("chart-line"), startExpanded = TRUE,
                menuSubItem("Difficulty Analysis", tabName = "difficulty_analysis", icon = icon("ruler-combined")),
                menuSubItem("Core Gameplay Charts", tabName = "core_gameplay", icon = icon("gamepad")),
                menuSubItem("Economy Analysis", tabName = "economy_analysis", icon = icon("coins")),
                menuSubItem("Sequence Analysis", tabName = "sequence_analysis", icon = icon("stream"))
            ),
            menuItem("Static Level Data", tabName = "static_level_data", icon = icon("filter")),
            uiOutput("balance_patch_selector_ui"),
            fileInput("file1", "Upload Level Data CSV", accept = ".csv"),
            uiOutput("level_range_slider")
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap"),
            tags$style(HTML("
                body, .content-wrapper, .right-side, .main-sidebar, .main-header .navbar, .main-header .logo, .box, h1, h2, h3, h4, h5, h6, .shiny-input-container, .selectize-input, .selectize-dropdown {
                    font-family: 'Inter', 'Lato', 'Helvetica Neue', Helvetica, Arial, sans-serif !important;
                }
                body, .content-wrapper, .right-side, .main-sidebar, .main-header .navbar, .main-header .logo {
                    background-color: #F7F9FB !important;
                    color: #222;
                }
                .box, .shiny-input-container, .selectize-input, .selectize-dropdown {
                     background-color: #FFFFFF !important;
                }
                .main-header .logo { font-weight: 700; color: #222 !important; }
                .main-header .navbar .sidebar-toggle, .main-header .navbar .nav > li > a { color: #222 !important; }
                .skin-blue .main-sidebar .sidebar .sidebar-menu > li > a,
                .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu > li > a {
                    color: #222 !important;
                    font-weight: 400;
                }
                .skin-blue .main-sidebar .sidebar .sidebar-menu .active > a,
                .skin-blue .main-sidebar .sidebar .sidebar-menu > li:hover > a,
                .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu > .active > a,
                .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu > li:hover > a {
                    background-color: #E3E8EF !important;
                    color: #0072B2 !important;
                    font-weight: 700;
                }
                .box {
                    border-top: 3px solid #0072B2;
                    background: #FFFFFF !important;
                }

                /* Force the background of the level metrics tab content area to be white */
                #shiny-tab-level_metrics {
                    background-color: #FFFFFF !important;
                }

                .box-body { padding: 0 6px 6px 6px !important; }
                label[for='file1'], .shiny-input-container label, .irs-single, .irs-bar, .irs-bar-edge, .irs-line { color: #222 !important; font-weight: 600; }
                .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview > a { color: #fff !important; }
                .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview > a > i { color: #fff !important; }
                .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu > li > a { color: #fff !important; }
                .skin-blue .main-sidebar .sidebar .sidebar-menu > li > a > i.fa-chart-line, .skin-blue .main-sidebar .sidebar .sidebar-menu > li > a:has(.fa-chart-line) { color: #222 !important; }
                .skin-blue .main-sidebar .sidebar .sidebar-menu > li > a:has(.fa-chart-line) { color: #222 !important; font-weight: 700; }
            "))
        ),
        # --- Only show missing columns warning in body ---
        uiOutput("csv_check_results"),
        tabItems(
            level_metrics_table_ui("metrics"),
            difficulty_analysis_ui("difficulty"),
            core_gameplay_ui("coregame"),
            economy_analysis_ui("economy"),
            sequence_analysis_ui("sequence"),
            static_level_data_ui("staticdata")
        )
    ),
    skin = "blue"
)

# --- Server Logic ---
server <- function(input, output, session) {

    # --- Aesthetics and Configuration ---
    # Okabe-Ito palette for categorical, Viridis for continuous
    okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
    get_difficulty_levels <- function(df) {
        lvls <- unique(na.omit(as.character(df$labeled_difficulty)))
        lvls[order(lvls)]
    }
    get_difficulty_colors <- function(levels) {
        setNames(okabe_ito[seq_along(levels)], levels)
    }

    # --- Reactive Values and Data Processing ---
    
    rv <- reactiveValues(missing_headers = NULL, data = NULL)
    
    difficulty_colors <- reactive({
      req(rv$data)
      get_difficulty_colors(get_difficulty_levels(rv$data))
    })

    observe({
        if (is.null(input$file1)) {
            df <- read.csv("examples/Level Data for Audit.csv")
        } else {
            df <- read.csv(input$file1$datapath)
        }
        
        current_headers <- names(df)
        missing <- setdiff(expected_headers, current_headers)
        rv$missing_headers <- if(length(missing) > 0) missing else NULL
        
        for (col in missing) { df[[col]] <- NA }
        
        rv$data <- df
    })

    calculated_data <- reactive({
        req(rv$data)
        df <- rv$data
        difficulty_levels <- get_difficulty_levels(df)
        df %>%
        mutate(
            labeled_difficulty = factor(labeled_difficulty, levels = difficulty_levels, ordered = TRUE),
            win_rate = ifelse(attempts > 0, wins / attempts, 0),
            daily_win_rate = ifelse(avg_daily_attempts_before_first_attempt > 0, avg_daily_wins_before_first_attempt / avg_daily_attempts_before_first_attempt, 0),
            churn_rate = ifelse(players > 0, 1 - (retained_players_to_next_level / players), 0),
            attempts_per_success = ifelse(wins > 0, attempts / wins, NA),
            near_win_rate = ifelse(attempts > 0 & (attempts - wins) > 0, near_win_losses_5_percent_or_less_objective / (attempts - wins), 0),
            near_loss_rate = ifelse(wins > 0, near_loss_wins_2_or_less_moves / wins, 0),
            extra_moves_conversion = ifelse(extra_moves_offered > 0, extra_moves_purchased / extra_moves_offered, 0),
            winstreak_tier_0_rate = ifelse(players > 0, first_attempt_win_streak_tier_0 / players, 0),
            winstreak_tier_1_rate = ifelse(players > 0, first_attempt_win_streak_tier_1 / players, 0),
            winstreak_tier_2_rate = ifelse(players > 0, first_attempt_win_streak_tier_2 / players, 0),
            winstreak_tier_3_rate = ifelse(players > 0, first_attempt_win_streak_tier_3 / players, 0)
        )
    })

    observe({
        df <- rv$data
        updateSelectizeInput(session, "sidebar_balance_patch", choices = sort(unique(na.omit(df$balance_patch_version))), server = TRUE)
    })

    filtered_data <- reactive({
        req(calculated_data(), input$level_range)
        df <- calculated_data()
        
        # Filter by sidebar balance patch
        if (!is.null(input$sidebar_balance_patch) && input$sidebar_balance_patch != "") {
            df <- df[df$balance_patch_version == input$sidebar_balance_patch, ]
        }
        
        df <- df[df$level_number >= input$level_range[1] & df$level_number <= input$level_range[2], ]
        df
    })

    # --- Server Modules ---
    level_metrics_table_server("metrics", filtered_data, difficulty_colors)
    difficulty_analysis_server("difficulty", filtered_data, difficulty_colors)
    core_gameplay_server("coregame", filtered_data, difficulty_colors)
    economy_analysis_server("economy", filtered_data, difficulty_colors)
    sequence_analysis_server("sequence", filtered_data)
    static_level_data_server("staticdata", reactive(rv$data))

    output$balance_patch_selector_ui <- renderUI({
      req(rv$data)
      patches <- sort(unique(na.omit(rv$data$balance_patch_version)), decreasing = TRUE)
      selectizeInput("sidebar_balance_patch", "Balance Patch Version", 
                     choices = patches, selected = patches[1], multiple = FALSE)
    })

    # --- Global controls only ---
    output$level_range_slider <- renderUI({
        req(rv$data)
        max_level <- max(rv$data$level_number, na.rm = TRUE)
        sliderInput("level_range", "Filter Level Range:", min = 1, max = max_level, value = c(1, max_level), step = 1)
    })
    output$csv_check_results <- renderUI({
        if (!is.null(rv$missing_headers)) {
            tags$div(class = "alert alert-warning", role = "alert", style = "margin-top: 15px;",
                tags$strong("Missing Columns:"), tags$ul(lapply(rv$missing_headers, tags$li))
            )
        }
    })
}

# --- Sequence Grouped Data Helper ---
sequence_grouped_data <- function(df, sequence_length) {
    if (is.null(df) || !"level_number" %in% names(df) || is.null(sequence_length)) return(data.frame())
    df %>%
        mutate(level_group = floor((level_number - 1) / sequence_length)) %>%
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
}

shinyApp(ui = ui, server = server) 