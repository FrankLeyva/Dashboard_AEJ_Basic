library(haven)      # For reading SPSS files
library(readxl)     # For reading Excel files
library(tidyverse)  # For data manipulation
library(janitor)    # For clean column names

#' Process survey data from SPSS and validate against Excel
#' @param spss_path Path to SPSS file
#' @param excel_path Path to Excel validation file
#' @return List containing processed data, metadata, and validation results
process_survey_data <- function(spss_path, excel_path) {
  # Read data sources
  spss_data <- read_spss(spss_path)
  excel_data <- read_excel(excel_path)
  
  # Extract metadata from SPSS file
  metadata <- extract_spss_metadata(spss_data)
  
  # Process SPSS data
  processed_data <- spss_data %>%
    process_variables() %>%
    validate_against_excel(excel_data) %>%
    create_analysis_variables()
    
  # Save outputs
  save_processed_data(processed_data)
  save_metadata(metadata)
  
  return(list(
    data = processed_data,
    metadata = metadata
  ))
}

#' Extract metadata from SPSS object
#' @param spss_data SPSS data read with haven
#' @return Data frame with variable metadata
extract_spss_metadata <- function(spss_data) {
  # Get variable information
  vars_info <- tibble(
    variable = names(spss_data),
    label = map_chr(spss_data, ~attr(., "label", exact = TRUE) %||% NA_character_),
    type = map_chr(spss_data, ~class(.)[1]),
    value_labels = map(spss_data, ~attr(., "labels", exact = TRUE))
  ) %>%
    mutate(
      has_value_labels = !map_lgl(value_labels, is.null),
      value_labels = map_chr(value_labels, ~if(is.null(.)) NA_character_ 
                            else paste(names(.), ., sep = "=", collapse = "; "))
    )
  
  return(vars_info)
}

#' Process variables from SPSS
#' @param data Raw survey data
#' @return Processed data frame
process_variables <- function(data) {
  data %>%
    # Clean column names
    clean_names() %>%
    # Handle missing values
    mutate(across(everything(), ~if_else(. %in% c(-99, -88, -77), NA, .))) %>%
    # Convert labelled variables to factors
    mutate(across(where(is.labelled), as_factor))
}

#' Create additional analysis variables
#' @param data Processed data
#' @return Data frame with derived variables
create_analysis_variables <- function(data) {
  data %>%
    # Add derived variables needed for analysis
    # This should be customized based on your specific needs
    mutate(
      # Example derivations - update as needed
      across(where(is.factor), ~as.character(.)),
      across(where(is.labelled), ~as.character(.))
    )
}

#' Validate processed data against Excel file
#' @param processed_data Processed SPSS data
#' @param excel_data Excel validation data
#' @return Validation results
validate_against_excel <- function(processed_data, excel_data) {
  # Print initial dimensions
  cat("\nValidation Summary:\n")
  cat("SPSS data dimensions:", dim(processed_data), "\n")
  cat("Excel data dimensions:", dim(excel_data), "\n")
  
  # Check row counts
  row_match <- nrow(processed_data) == nrow(excel_data)
  cat("\nRow count match:", row_match)
  if(!row_match) {
    cat("\nSPSS rows:", nrow(processed_data))
    cat("\nExcel rows:", nrow(excel_data))
  }
  
  # Check column names
  spss_cols <- sort(names(processed_data))
  excel_cols <- sort(names(excel_data))
  col_match <- all(spss_cols %in% excel_cols)
  
  cat("\nColumn names match:", col_match)
  if(!col_match) {
    cat("\nColumns only in SPSS:", 
        setdiff(spss_cols, excel_cols))
    cat("\nColumns only in Excel:", 
        setdiff(excel_cols, spss_cols))
  }
  
  # Return validation results
  results<-list(
    row_count_match = row_match,
    column_names_match = col_match,
    spss_cols = spss_cols,
    excel_cols = excel_cols,
    validation_summary = list(
      row_diff = nrow(processed_data) - nrow(excel_data),
      cols_only_in_spss = setdiff(spss_cols, excel_cols),
      cols_only_in_excel = setdiff(excel_cols, spss_cols)
    )
  )
  log_validation_results(results)
return(spss_data)
}

#' Check value ranges for numeric variables
#' @param processed_data Processed SPSS data
#' @param excel_data Excel validation data
#' @return List of validation results
check_value_ranges <- function(processed_data, excel_data) {
  # Get numeric columns
  numeric_cols <- names(processed_data)[sapply(processed_data, is.numeric)]
  
  # Compare ranges for each numeric column
  map(numeric_cols, function(col) {
    if(col %in% names(excel_data)) {
      list(
        column = col,
        processed_range = range(processed_data[[col]], na.rm = TRUE),
        excel_range = range(excel_data[[col]], na.rm = TRUE)
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
    list(
      column = col,
      processed_nas = sum(is.na(processed_data[[col]])),
      excel_nas = sum(is.na(excel_data[[col]]))
    )
  })
}

#' Save processed data
#' @param data Processed data frame
save_processed_data <- function(data) {
  # Create directories if they don't exist
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  
  # Save as RDS for R
  saveRDS(data, "data/processed/survey_2023.rds")
  
  # Save as CSV for external use
  write_csv(data, "data/processed/survey_2023.csv")
}

#' Save metadata
#' @param metadata Metadata data frame
save_metadata <- function(metadata) {
  # Create directories if they don't exist
  dir.create("data/metadata", recursive = TRUE, showWarnings = FALSE)
  
  # Save as CSV
  write_csv(metadata, "data/metadata/variable_metadata.csv")
  
  # Generate coding manual
  generate_coding_manual(metadata)
}

#' Generate coding manual from metadata
#' @param metadata Metadata data frame
generate_coding_manual <- function(metadata) {
  # Create manual in markdown format
  manual <- c("# Coding Manual\n\n")
  
  cat("Generating coding manual...\n")
  cat("Number of variables to document:", nrow(metadata), "\n")
  
  # Add variable documentation
  for(i in 1:nrow(metadata)) {
    var_info <- metadata[i,]
    
    # Basic variable information
    var_doc <- sprintf(
      "## %s\n\n**Label:** %s\n\n**Type:** %s\n\n",
      var_info$variable, 
      ifelse(is.na(var_info$label), "No label", var_info$label),
      var_info$type
    )
    
    # Add value labels if present
    if(var_info$has_value_labels) {
      var_doc <- c(var_doc, "**Value Labels:**\n\n")
      labels <- var_info$value_labels
      if(!is.na(labels)) {
        labels_split <- strsplit(labels, "; ")[[1]]
        var_doc <- c(var_doc, paste("- ", labels_split, "\n"))
      }
    }
    
    var_doc <- c(var_doc, "\n---\n\n")
    manual <- c(manual, var_doc)
  }
  
  # Write manual to file
  dir.create("docs", recursive = TRUE, showWarnings = FALSE)
  writeLines(manual, "docs/coding_manual.md")
  
  cat("Coding manual generated successfully\n")
}

#' Log validation results
#' @param results Validation results list
log_validation_results <- function(results, file_path = "logs/validation_report.md") {
  # Create directory if it doesn't exist
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  
  # Initialize report
  report <- c("# Validation Report\n\n")
  
  # Add timestamp
  report <- c(report, sprintf("**Generated:** %s\n\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  
  # Basic Match Summary
  report <- c(report, "## Basic Validation Checks\n\n")
  report <- c(report, sprintf("- Row count match: **%s**\n", results$row_count_match))
  report <- c(report, sprintf("- Column names match: **%s**\n\n", results$column_names_match))
  
  # Detailed Results
  if(!results$row_count_match || !results$column_names_match) {
    report <- c(report, "## Detailed Analysis\n\n")
    
    # Row count analysis
    if(!is.null(results$validation_summary$row_diff)) {
      report <- c(report, "### Row Count Analysis\n\n")
      report <- c(report, sprintf("- Row difference: %d\n\n", 
                                results$validation_summary$row_diff))
    }
    
    # Column analysis
    report <- c(report, "### Column Analysis\n\n")
    
    # Columns only in SPSS
    if(length(results$validation_summary$cols_only_in_spss) > 0) {
      report <- c(report, "#### Columns only in SPSS data:\n\n")
      for(col in results$validation_summary$cols_only_in_spss) {
        report <- c(report, sprintf("- `%s`\n", col))
      }
      report <- c(report, "\n")
    }
    
    # Columns only in Excel
    if(length(results$validation_summary$cols_only_in_excel) > 0) {
      report <- c(report, "#### Columns only in Excel data:\n\n")
      for(col in results$validation_summary$cols_only_in_excel) {
        report <- c(report, sprintf("- `%s`\n", col))
      }
      report <- c(report, "\n")
    }
  }
  
  # Add recommendations if there are issues
  if(!results$row_count_match || !results$column_names_match) {
    report <- c(report, "## Recommendations\n\n")
    
    if(!results$row_count_match) {
      report <- c(report, "### Row Count Mismatch\n")
      report <- c(report, "Possible actions:\n")
      report <- c(report, "1. Check for duplicate rows in either file\n")
      report <- c(report, "2. Verify data export process from both sources\n")
      report <- c(report, "3. Check for filtered or hidden rows in Excel\n\n")
    }
    
    if(!results$column_names_match) {
      report <- c(report, "### Column Name Mismatch\n")
      report <- c(report, "Possible actions:\n")
      report <- c(report, "1. Check for encoding issues in column names\n")
      report <- c(report, "2. Verify naming conventions between files\n")
      report <- c(report, "3. Look for hidden or renamed columns\n\n")
    }
  }
  
  # Write report to file
  writeLines(report, file_path)
  
  # Also print to console
  cat("\nValidation Report Summary:\n")
  cat("------------------------\n")
  cat(sprintf("Row count match: %s\n", results$row_count_match))
  cat(sprintf("Column names match: %s\n", results$column_names_match))
  if(!results$row_count_match || !results$column_names_match) {
    cat("\nDetailed report saved to:", file_path, "\n")
  }
  
  # Return invisible copy of report
  invisible(report)
}

# Example usage:
if(FALSE) {
  results <- process_survey_data(
    spss_path = "data/raw/survey_2023.sav",
    excel_path = "data/raw/survey_2023.xlsx"
  )
}

inspect_data_structure <- function(spss_data, excel_data) {
  cat("\nData Structure Comparison:\n")
  
  # SPSS data
  cat("\nSPSS Data Structure:\n")
  cat("Dimensions:", dim(spss_data), "\n")
  cat("Column names (first 5):", head(names(spss_data), 5), "...\n")
  cat("Column types (sample):\n")
  print(sapply(spss_data[1:5], class))
  
  # Excel data
  cat("\nExcel Data Structure:\n")
  cat("Dimensions:", dim(excel_data), "\n")
  cat("Column names (first 5):", head(names(excel_data), 5), "...\n")
  cat("Column types (sample):\n")
  print(sapply(excel_data[1:5], class))
  
  # Compare column names
  common_cols <- intersect(names(spss_data), names(excel_data))
  cat("\nNumber of common columns:", length(common_cols), "\n")
  
  # Return detailed information
  results<-list(
    spss_structure = str(spss_data),
    excel_structure = str(excel_data),
    common_columns = common_cols,
    comparison = list(
      spss_cols = names(spss_data),
      excel_cols = names(excel_data),
      spss_types = sapply(spss_data, class),
      excel_types = sapply(excel_data, class)
    )
  )
}
