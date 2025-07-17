# server.R

server <- function(input, output, session) {

    # --- Password Authentication ---
    USER <- reactiveValues(logged = FALSE)

    observeEvent(input$login_button, {
        if (input$password == "big$") {
            USER$logged <- TRUE
            shinyjs::hide("login")
            shinyjs::show("main_content")
        } else {
            showNotification("Incorrect password.", type = "error")
        }
    })

    # --- Aesthetics and Configuration ---
    # Okabe-Ito palette for categorical, Viridis for continuous
    okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
    get_difficulty_levels <- function(df) {
        # Desired order for difficulty levels
        desired_order <- c("Easy", "Normal", "Hard", "Very Hard", "Insane", "Bonus")
        
        present_levels <- unique(na.omit(as.character(df$labeled_difficulty)))
        
        # Filter desired_order to only include levels present in the data, maintaining the order
        ordered_levels <- desired_order[desired_order %in% present_levels]
        
        # Identify any levels from the data that are not in our desired_order list
        other_levels <- setdiff(present_levels, ordered_levels)
        
        # Combine them, so known levels are first and ordered, and unknown levels are appended
        c(ordered_levels, other_levels)
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

    # Render UI for balance patch filter
    output$balance_patch_filter_ui <- renderUI({
      req(rv$data)
      patch_choices <- sort(unique(na.omit(rv$data$balance_patch_version)), decreasing = TRUE)
      
      selectInput("balance_patch_filter", 
                  "Filter by Balance Patch:",
                  choices = patch_choices,
                  selected = patch_choices[1])
    })

    # Render UI for difficulty filter
    output$difficulty_filter_ui <- renderUI({
      req(rv$data)
      difficulty_levels <- get_difficulty_levels(rv$data)
      
      selectizeInput("difficulty_filter", "Filter by Difficulty:",
                     choices = difficulty_levels,
                     selected = setdiff(difficulty_levels, "Bonus"),
                     multiple = TRUE)
    })
    
    # Render UI for level range slider
    output$level_range_slider <- renderUI({
        req(rv$data)
        max_level <- max(rv$data$level_number, na.rm = TRUE)
        sliderInput("level_range", "Filter Level Range:", min = 1, max = max_level, value = c(1, 60), step = 1)
    })

    # Reactive expression for filtered data based on balance patch
    balance_patch_filtered_data <- reactive({
      req(calculated_data(), input$balance_patch_filter)
      data <- calculated_data()
      if (!is.null(input$balance_patch_filter) && input$balance_patch_filter != "") {
        data <- data %>% filter(balance_patch_version == input$balance_patch_filter)
      }
      data
    })
    
    # Reactive expression for data filtered by both balance patch and difficulty
    filtered_data <- reactive({
      data <- balance_patch_filtered_data()
      
      if (!is.null(input$difficulty_filter)) {
        data <- data %>% filter(labeled_difficulty %in% input$difficulty_filter)
      }
      
      if (!is.null(input$level_range)) {
        data <- data %>% filter(level_number >= input$level_range[1] & level_number <= input$level_range[2])
      }
      
      data
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
            extra_moves_conversion = ifelse(extra_moves_offered > 0, (extra_moves_purchased / 5) / extra_moves_offered, 0),
            winstreak_tier_0_rate = ifelse(players > 0, first_attempt_win_streak_tier_0 / players, 0),
            winstreak_tier_1_rate = ifelse(players > 0, first_attempt_win_streak_tier_1 / players, 0),
            winstreak_tier_2_rate = ifelse(players > 0, first_attempt_win_streak_tier_2 / players, 0),
            winstreak_tier_3_rate = ifelse(players > 0, first_attempt_win_streak_tier_3 / players, 0)
        )
    })

    # --- Server Modules ---
    observe({
        req(USER$logged)
        level_metrics_table_server("level_metrics", filtered_data, difficulty_colors)
        difficulty_analysis_server("difficulty", filtered_data, difficulty_colors)
        core_gameplay_server("core_gameplay", filtered_data, difficulty_colors)
        economy_analysis_server("economy", filtered_data, difficulty_colors, okabe_ito)
        sequence_analysis_server("sequence", filtered_data)
        static_level_data_server("static_level_data", reactive(rv$data))
        custom_analysis_server("custom_analysis", filtered_data)
    })
}