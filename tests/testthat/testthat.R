# tests/testthat.R
library(testthat)
library(shinytest2)
library(tidyverse)

# Test data processing functions
test_that("Data processing works correctly", {
  # Setup test data
  test_data <- data.frame(
    Q1 = c(1, 2, 3, -99),
    Q2 = c("A", "B", "C", "D"),
    Q3 = c(10, 20, -88, 40)
  )
  
  # Test missing value handling
  processed <- process_variables(test_data)
  expect_true(is.na(processed$Q1[4]))
  expect_true(is.na(processed$Q3[3]))
  
  # Test value ranges
  expect_true(all(processed$Q1[!is.na(processed$Q1)] %in% 1:3))
})

# Test data validation functions
test_that("Data validation catches discrepancies", {
  spss_data <- data.frame(Q1 = 1:4, Q2 = letters[1:4])
  excel_data <- data.frame(Q1 = 1:4, Q2 = letters[1:4])
  
  # Test matching data
  result <- validate_against_excel(spss_data, excel_data)
  expect_true(result$check_row_counts)
  expect_true(result$check_column_names)
  
  # Test mismatched data
  excel_data$Q1[1] <- 999
  result <- validate_against_excel(spss_data, excel_data)
  expect_false(check_value_ranges(spss_data, excel_data)[[1]]$matches)
})

# Test module functions
test_that("Demographics module calculations are correct", {
  test_data <- data.frame(
    age = c(20, 30, 40, 50),
    gender = c("M", "F", "M", "F")
  )
  
  # Test age group calculation
  result <- create_age_groups(test_data$age)
  expect_equal(length(unique(result)), 4)
  
  # Test gender distribution
  dist <- calculate_gender_distribution(test_data$gender)
  expect_equal(dist$M, 2)
  expect_equal(dist$F, 2)
})

# tests/testthat/test-shiny.R
library(shinytest2)

test_that("Shiny app works", {
  # Record test events
  app <- AppDriver$new(name = "demographics", height = 980, width = 1440)
  
  # Test navigation
  app$click("nav_demographics")
  app$expect_values()
  
  # Test filters
  app$set_inputs(age_filter = "18-25")
  app$expect_values()
  
  # Test downloads
  app$click("download_data")
  app$expect_download()
})

# Run all tests
test_dir("tests/testthat/")