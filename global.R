# global.R

# --- Libraries ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny, shinydashboard, shinyjs, DT, ggplot2, dplyr, gt, 
  gtExtras, plotly, ggthemes, ggridges, snakecase, tidyr
)

# --- Sourcing ---
# Source all modules explicitly
source("modules/static_level_data.R")
source("modules/difficulty_analysis.R")
source("modules/core_gameplay.R")
source("modules/economy_analysis.R")
source("modules/sequence_analysis.R")
source("modules/level_metrics_table.R")
source("modules/custom_analysis.R")

source("utils/config.R")
source("utils/plotting_helpers.R")

# --- Global Settings ---
suppressPackageStartupMessages(library(ggplot2))
theme_set(theme_fivethirtyeight(base_family = "Inter"))

# --- Constants ---
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

# --- Helper Functions ---
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