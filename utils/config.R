# utils/config.R

# Okabe-Ito palette for categorical, Viridis for continuous
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
difficulty_levels <- c("Easy", "Medium", "Hard", "Very Hard", "Insane")
difficulty_colors <- setNames(okabe_ito[1:length(difficulty_levels)], difficulty_levels)

# Expected headers for data validation
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