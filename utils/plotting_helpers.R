# utils/plotting_helpers.R

#' Generate a standardized scatter plot against level number.
#' @param df The dataframe to use.
#' @param y_var The name of the column for the y-axis.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param y_lab The y-axis label.
#' @param color_var The column to use for coloring points and lines.
#' @param color_lab The label for the color legend.
#' @param y_percent TRUE to format the y-axis as percentages.
#' @return A plotly object.
generate_level_scatter <- function(df, y_var, title, y_lab, subtitle = NULL, color_var = "labeled_difficulty", color_lab = "Difficulty", y_percent = FALSE, difficulty_colors = NULL) {
  req(y_var %in% names(df), color_var %in% names(df))
  
  # Ensure the color variable exists in the dataframe
  if (!color_var %in% names(df)) {
    stop(paste("Color variable '", color_var, "' not found in dataframe."))
  }

  p <- ggplot(df, aes(x = level_number, y = .data[[y_var]], color = .data[[color_var]], group = .data[[color_var]])) +
    geom_point(aes(text = paste("Level:", level_number)), alpha = 0.6)
  
  # Add geom_smooth only if there are enough points to do so
  if (nrow(df) > 2) {
    p <- p + geom_smooth(method = "loess", se = FALSE)
  }
  
  p <- p +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Level Number",
      y = y_lab,
      color = color_lab
    ) +
    theme_fivethirtyeight() +
    theme(text = element_text(family = "Inter"))
  
  if (y_percent) {
    p <- p + scale_y_continuous(labels = scales::percent)
  }
  
  # Add difficulty color scale if applicable and provided
  if (!is.null(difficulty_colors) && color_var == "labeled_difficulty") {
    p <- p + scale_color_manual(values = difficulty_colors)
  }
  
  plot <- ggplotly(p, tooltip = "text")
  
  if (!is.null(subtitle)) {
    plot 
  } else {
    plot 
  }
  
  plot
}

#' Generate a standardized boxplot by difficulty.
#' @param df The dataframe to use.
#' @param y_var The column for the y-axis.
#' @param title The plot title.
#' @param y_lab The y-axis label.
#' @param y_percent TRUE to format the y-axis as percentages.
#' @return A plotly object.
generate_difficulty_boxplot <- function(df, y_var, title, y_lab, y_percent = FALSE, difficulty_colors = NULL) {
  req(y_var %in% names(df))
  p <- ggplot(df, aes(x = labeled_difficulty, y = .data[[y_var]], fill = labeled_difficulty)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = title, x = "Difficulty Label", y = y_lab) +
    theme_fivethirtyeight() +
    theme(text = element_text(family = "Inter"))
  if (!is.null(difficulty_colors)) {
    p <- p + scale_fill_manual(values = difficulty_colors)
  }
  p <- p + guides(fill = "none")
  if (y_percent) {
    p <- p + scale_y_continuous(labels = scales::percent)
  }
  ggplotly(p)
} 