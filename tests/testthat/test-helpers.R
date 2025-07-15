library(testthat)
library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)
library(scales)
source("../../utils/plotting_helpers.R")

test_that("generate_level_scatter returns a plotly object", {
  df <- data.frame(level_number = 1:10, win_rate = runif(10), labeled_difficulty = rep("Easy", 10))
  plt <- generate_level_scatter(df, "win_rate", "Test", "Win Rate")
  expect_true("plotly" %in% class(plt))
}) 