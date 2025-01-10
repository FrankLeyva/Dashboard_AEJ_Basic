library(testthat)
library(tidyverse)

#' @param processed_data Processed SPSS data
#' @param excel_data Excel validation data
#' @return List of validation results
check_value_ranges <- function(processed_data, excel_data) {
  # Get numeric columns
  numeric_cols <- names(processed_data)[sapply(processed_data, is.numeric)]
  
  # Check each numeric column
  map(numeric_cols, function(col) {
    if(col %in% names(excel_data)) {
      processed_range <- range(processed_data[[col]], na.rm = TRUE)
      excel_range <- range(excel_data[[col]], na.rm = TRUE)
      
      list(
        column = col,
        matches = all(processed_range == excel_range),
        processed_range = processed_range,
        excel_range = excel_range
      )
    }
  })
}

#' Check missing value patterns
#' @param processed_data Processed SPSS data
#' @param excel_data Excel validation data
#' @return List of validation results
check_missing_patterns <- function(processed_data, excel_data) {
  common_cols <- intersect(names(processed_data), names(excel_data))
  
  map(common_cols, function(col) {
    processed_nas <- sum(is.na(processed_data[[col]]))
    excel_nas <- sum(is.na(excel_data[[col]]))
    
    list(
      column = col,
      matches = processed_nas == excel_nas,
      processed_nas = processed_nas,
      excel_nas = excel_nas
    )
  })
}

#' Run comprehensive data quality checks
#' @param data Processed data frame
#' @return List of quality check results
run_quality_checks <- function(data) {
  list(
    # Check for unexpected missing values
    missing_check = map(names(data), function(col) {
      list(
        column = col,
        missing_count = sum(is.na(data[[col]])),
        missing_percent = mean(is.na(data[[col]])) * 100
      )
    }),
    
    # Check value distributions for categorical variables
    categorical_check = map(names(data)[sapply(data, is.factor)], function(col) {
      list(
        column = col,
        levels = levels(data[[col]]),
        counts = table(data[[col]], useNA = "always")
      )
    }),
    
    # Check numeric variable distributions
    numeric_check = map(names(data)[sapply(data, is.numeric)], function(col) {
      list(
        column = col,
        summary = summary(data[[col]]),
        outliers = boxplot.stats(data[[col]])$out
      )
    }),
    
    # Check for logical inconsistencies
    logic_check = check_logical_consistency(data)
  )
}

#' Check logical consistency between related variables
#' @param data Processed data frame
#' @return List of consistency check results
check_logical_consistency <- function(data) {
  # Add specific consistency checks based on survey logic
  list(
    # Example: Check if age and education level are consistent
    age_education = if(all(c("age", "education") %in% names(data))) {
      data %>%
        filter(age < 25 & education == "Doctorado") %>%
        nrow()
    },
    
    # Add more specific checks based on survey structure
    # Example: income validation
    income_check = if("income" %in% names(data)) {
      data %>%
        filter(income < 0 | income > 1000000) %>%
        nrow()
    }
  )
}

#' Generate validation report
#' @param validation_results List of validation results
#' @return Character string containing report
generate_validation_report <- function(validation_results) {
  report <- c("# Data Validation Report\n\n")
  
  # Add sections for each type of validation
  report <- c(report, "## Value Range Checks\n")
  # Add value range results
  
  report <- c(report, "\n## Missing Value Patterns\n")
  # Add missing value results
  
  report <- c(report, "\n## Logical Consistency Checks\n")
  # Add consistency check results
  
  # Write report to file
  writeLines(report, "docs/validation_report.md")
  
  return(report)
}

# Example test suite
if(FALSE) {
  test_that("value ranges are consistent", {
    expect_true(all(map_lgl(check_value_ranges(processed_data, excel_data), "matches")))
  })
  
  test_that("missing patterns match", {
    expect_true(all(map_lgl(check_missing_patterns(processed_data, excel_data), "matches")))
  })
  
  test_that("logical consistency checks pass", {
    consistency_results <- check_logical_consistency(processed_data)
    expect_equal(consistency_results$age_education, 0)
    expect_equal(consistency_results$income_check, 0)
  })
}