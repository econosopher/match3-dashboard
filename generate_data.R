if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr)

# --- Configuration ---
num_levels <- 150
output_path <- "examples/sample_level_data.csv"

# --- Data Generation ---
set.seed(42)
level_data <- tibble(
    level_number = 1:num_levels,
    
    # --- Difficulty & Features ---
    labeled_difficulty = sample(c("Easy", "Normal", "Hard", "Very Hard"), num_levels, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
    blocker_type = sample(c("Goo", "Chains", "Ice", "Waffles", "None"), num_levels, replace = TRUE),
    feature_introduced = ifelse(runif(num_levels) > 0.9, "New Booster", "None"),
    move_limit = sample(20:40, num_levels, replace = TRUE),
    objective_type = "Clear Jellies",
    objective_amount = sample(40:80, num_levels, replace = TRUE),

    # --- Player Stats ---
    players = round(10000 * (1 - level_number / (num_levels * 2))),
    attempts = players * round(runif(num_levels, 1.1, 4)),
    wins = round(attempts * runif(num_levels, 0.25, 0.7)),
    mid_match_quits = round(attempts * runif(num_levels, 0.01, 0.1)),
    retained_players_to_next_level = round(players * runif(num_levels, 0.85, 0.99)),

    # --- Near Win/Loss ---
    near_loss_wins_2_or_less_moves = round(wins * runif(num_levels, 0.1, 0.3)),
    near_win_losses_5_percent_or_less_objective = round((attempts - wins) * runif(num_levels, 0.15, 0.4)),

    # --- Win Streaks ---
    first_attempt_win_streak_tier_0 = round(players * runif(num_levels, 0.3, 0.7)),
    first_attempt_win_streak_tier_1 = round(players * runif(num_levels, 0.1, 0.3)),
    first_attempt_win_streak_tier_2 = round(players * runif(num_levels, 0.05, 0.15)),
    first_attempt_win_streak_tier_3 = round(players * runif(num_levels, 0.01, 0.1)),
    first_attempt_winstreak_tier_1_wins = round(first_attempt_win_streak_tier_1 * runif(num_levels, 0.5, 0.9)),
    first_attempt_winstreak_tier_2_wins = round(first_attempt_win_streak_tier_2 * runif(num_levels, 0.4, 0.8)),
    first_attempt_winstreak_tier_3_wins = round(first_attempt_win_streak_tier_3 * runif(num_levels, 0.3, 0.7)),

    # --- Progression & Economy ---
    avg_active_day_on_first_attempt = pmin(90, round(1 + level_number * runif(num_levels, 0.5, 1.2))),
    avg_daily_wins_before_first_attempt = round(avg_active_day_on_first_attempt * runif(num_levels, 0.5, 2)),
    avg_daily_attempts_before_first_attempt = round(avg_daily_wins_before_first_attempt / runif(num_levels, 0.3, 0.8)),
    
    # --- Medians ---
    median_attempts_per_winner = pmax(1, round(runif(num_levels, 1, 5) + ifelse(labeled_difficulty == "Hard", 2, 0))),
    median_moves_remaining_win = sample(1:10, num_levels, replace = TRUE),
    median_objective_pct_remaining_loss = runif(num_levels, 0.01, 0.3),
    median_base_coins_rewarded_win = 50,
    median_move_coins_rewarded = 5,
    average_moves_coins_rewarded = 5.2,

    # --- Extra Moves ---
    extra_moves_offered = round((attempts - wins) * runif(num_levels, 0.4, 0.8)),
    extra_moves_purchased = round(extra_moves_offered * runif(num_levels, 0.1, 0.5)),
    extra_moves_wins = round(extra_moves_purchased * runif(num_levels, 0.7, 0.95)),
    gold_extra_move_purchased = extra_moves_purchased * 100,

    # --- Boosters ---
    pgb_purchased = round(players * runif(num_levels, 0.1, 0.3)),
    gold_pgb_sink = pgb_purchased * 50,
    pgb_amount_used = round(attempts * runif(num_levels, 0.05, 0.2)),
    igb_purchased = round(players * runif(num_levels, 0.2, 0.5)),
    igb_gold_pbg_sink = igb_purchased * 25,
    igb_amount_used = round(attempts * runif(num_levels, 0.1, 0.4)),

    # --- Gold Inventories (Median) ---
    median_pgb_inventory_amount_first_attempt = sample(0:5, num_levels, replace = TRUE),
    median_igb_inventory_amount_first_attempt = sample(0:5, num_levels, replace = TRUE),
    median_gold_inventory_amount_first_attempt = sample(50:500, num_levels, replace = TRUE),
    median_customer_gold_inventory_amount_first_attempt = sample(100:1000, num_levels, replace = TRUE),
    median_90th_percentile_customer_gold_inventory_amount_first_attempt = sample(1000:5000, num_levels, replace=T),
    median_95th_percentile_customer_gold_inventory_amount_first_attempt = sample(2000:8000, num_levels, replace=T),
    median_99th_percentile_customer_gold_inventory_amount_first_attempt = sample(5000:20000, num_levels, replace=T),

    # --- Versioning ---
    balance_patch_version = "1.0.0"
)

# --- Write to CSV ---
dir.create("examples", showWarnings = FALSE)
write_csv(level_data, output_path)

message("Successfully generated new sample data at ", output_path) 