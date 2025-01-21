library(haven)      
library(readxl)     
library(tidyverse)  
library(janitor)    

process_survey_data <- function(spss_path, excel_path) {
  spss_data <- read_spss(spss_path)
  excel_data <- read_excel(excel_path)
  metadata <- extract_spss_metadata(spss_data)
  processed_data <- spss_data %>%
    process_variables() %>%
    validate_against_excel(excel_data) %>%
    create_analysis_variables()
  save_processed_data(processed_data)
  save_metadata(metadata)
  return(list(
    data = processed_data,
    metadata = metadata
  ))
}
split_value_labels <- function(df, column_name='value_labels', num_columns = 35) {
  # Create a matrix to hold the new columns
  new_columns_matrix <- matrix(NA, nrow = nrow(df), ncol = num_columns)
  
  # Loop over each row and fill the matrix with the corresponding labels
  for (i in 1:nrow(df)) {
    # Check if the value labels column is not empty
    if (!is.na(df[[column_name]][i]) && df[[column_name]][i] != "") {
      # Split the value labels into a vector
      value_labels_vector <- unlist(strsplit(df[[column_name]][i], ";"))
      # Extract the number and label
      value_labels_vector <- sapply(value_labels_vector, function(x) {
        number_label <- unlist(strsplit(x, " = "))
        return(number_label)
      })
      
      # Loop over the value labels vector and fill the matrix
      for (j in 1:ncol(value_labels_vector)) {
        number <- as.numeric(value_labels_vector[1, j])
        label <- value_labels_vector[2, j]
        if (!is.na(number) && number <= num_columns) {
          new_columns_matrix[i, number + 1] <- label # Using number + 1 for 0-index adjustment
        }
      }
    }
  }
  
  # Convert the matrix to a dataframe and set column names
  new_columns_df <- as.data.frame(new_columns_matrix)
  colnames(new_columns_df) <- 0:(num_columns - 1)
  
  # Combine the new columns with the original dataframe
  df <- cbind(df, new_columns_df)
  
  return(df)
}
extract_spss_metadata <- function(spss_data, output_csv_path='data/processed/metadata.csv') {

  # Initialize lists to store metadata
  variables <- list()
  value_labels <- list()
  
  # Extract metadata for each variable
  for (var_name in names(spss_data)) {
    var_info <- attributes(spss_data[[var_name]])
    
    # Get variable label
    var_label <- var_info$label
    if (is.null(var_label)) var_label <- NA
    
    # Get variable type
    var_type <- class(spss_data[[var_name]])[1]
    
    # Get value labels if they exist
    val_labs <- var_info$labels
    if (!is.null(val_labs)) {
      value_labels[[var_name]] <- tibble(
        variable = var_name,
        value = as.character(val_labs),
        label = names(val_labs)
      )
    }
    
    # Store variable information
    variables[[var_name]] <- tibble(
      variable = var_name,
      label = var_label,
      type = var_type,
      has_value_labels = !is.null(val_labs)
    )
  }
  
  # Combine all variable information
  var_info_df <- bind_rows(variables)
  
  # Combine all value labels
  if (length(value_labels) > 0) {
    val_labels_df <- bind_rows(value_labels)
    
    # Create wide format value labels string
    val_labels_str <- val_labels_df %>%
      group_by(variable) %>%
      summarize(value_labels = paste(paste(value, "=", label), collapse = "; "))
    
    # Join with variable information
    var_info_df <- var_info_df %>%
      left_join(val_labels_str, by = "variable")
  } else {
    var_info_df$value_labels <- NA_character_
  }
  var_info_df <- split_value_labels(var_info_df)
  # Export to CSV
  write_csv(var_info_df, output_csv_path)
  
  # Return the data frame
  return(var_info_df)
}



process_variables <- function(data) {
  data %>%
    clean_names() %>%
    mutate(across(everything(), ~if_else(. %in% c(-99, -88, -77), NA, .))) %>%
    mutate(across(where(is.labelled), as_factor))
}

create_analysis_variables <- function(data) {
  data %>%
    mutate(
      across(where(is.factor), ~as.character(.)),
      across(where(is.labelled), ~as.character(.))
    )
}


validate_against_excel <- function(processed_data, excel_data) {
  cat("\nValidation Summary:\n")
  cat("SPSS data dimensions:", dim(processed_data), "\n")
  cat("Excel data dimensions:", dim(excel_data), "\n")
  
  row_match <- nrow(processed_data) == nrow(excel_data)
  cat("\nRow count match:", row_match)
  if(!row_match) {
    cat("\nSPSS rows:", nrow(processed_data))
    cat("\nExcel rows:", nrow(excel_data))
  }
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


check_value_ranges <- function(processed_data, excel_data) {
  numeric_cols <- names(processed_data)[sapply(processed_data, is.numeric)]
  
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


save_processed_data <- function(data) {
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  write_csv(data, "data/processed/survey_2023.csv")
}

save_metadata <- function(metadata) {
  dir.create("data/metadata", recursive = TRUE, showWarnings = FALSE)
  write_csv(metadata, "data/metadata/variable_metadata.csv")
  generate_coding_manual(metadata)
}
generate_coding_manual <- function(metadata) {
  manual <- c("# Coding Manual\n\n")
  cat("Generating coding manual...\n")
  cat("Number of variables to document:", nrow(metadata), "\n")
  for(i in 1:nrow(metadata)) {
    var_info <- metadata[i,]
    var_doc <- sprintf(
      "## %s\n\n**Label:** %s\n\n**Type:** %s\n\n",
      var_info$variable, 
      ifelse(is.na(var_info$label), "No label", var_info$label),
      var_info$type
    )
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
  dir.create("docs", recursive = TRUE, showWarnings = FALSE)
  writeLines(manual, "docs/coding_manual.md")
  cat("Coding manual generated successfully\n")
}
log_validation_results <- function(results, file_path = "logs/validation_report.md") {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  
  report <- c("# Validation Report\n\n")

  report <- c(report, sprintf("**Generated:** %s\n\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

  report <- c(report, "## Basic Validation Checks\n\n")
  report <- c(report, sprintf("- Row count match: **%s**\n", results$row_count_match))
  report <- c(report, sprintf("- Column names match: **%s**\n\n", results$column_names_match))
  if(!results$row_count_match || !results$column_names_match) {
    report <- c(report, "## Detailed Analysis\n\n")
    if(!is.null(results$validation_summary$row_diff)) {
      report <- c(report, "### Row Count Analysis\n\n")
      report <- c(report, sprintf("- Row difference: %d\n\n", 
                                results$validation_summary$row_diff))
    }
    report <- c(report, "### Column Analysis\n\n")

    if(length(results$validation_summary$cols_only_in_spss) > 0) {
      report <- c(report, "#### Columns only in SPSS data:\n\n")
      for(col in results$validation_summary$cols_only_in_spss) {
        report <- c(report, sprintf("- `%s`\n", col))
      }
      report <- c(report, "\n")
    }
    if(length(results$validation_summary$cols_only_in_excel) > 0) {
      report <- c(report, "#### Columns only in Excel data:\n\n")
      for(col in results$validation_summary$cols_only_in_excel) {
        report <- c(report, sprintf("- `%s`\n", col))
      }
      report <- c(report, "\n")
    }
  }
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
  writeLines(report, file_path)
  invisible(report)
}
fix_latin_encoding <- function(df, column = 'Pregunta') {
  encoding_map <- list(
    "Ã¡" = "á", "Ã©" = "é", "Ã­" = "í", "Ã³" = "ó", "Ãº" = "ú",
    "Ã�" = "Á", "Ã‰" = "É", "Ã�" = "Í", "Ã“" = "Ó","�"="ó","Ãš" = "Ú",
    "Ã'" = "Ñ", "Ã±" = "ñ", "Ãœ" = "Ü", "Ã¼" = "ü",
    "Â¡" = "¡", "Â¿" = "¿")  
for (wrong_char in names(encoding_map)) {
    df[[column]] <- gsub(wrong_char, encoding_map[[wrong_char]], df[[column]])
  }
  
  return(df)
}
process_coding_manual <- function(manual) {
  response_types_map <- tribble(
    ~type_id, ~type_name,
    1,        "TEXT",
    2,        "NUMERIC",
    3,        "DATE"
  )
  
  # Process each row
  processed_data <- manual %>%
    mutate(
      numeric_id = row_number(),
      question_internal_id = create_formatted_id(numeric_id)
    ) %>% 
    rowwise() %>%
    mutate(
      response_type_id = detect_basic_type(cur_data()),
    ) %>%
    left_join(response_types_map, by = c("response_type_id" = "type_id")) %>%
      ungroup()
  
  # Extract response options for each question
  response_options <- processed_data %>%
    rowwise() %>%
    mutate(
      options = list(extract_response_options(cur_data()))
    ) %>%
    ungroup()
  
  # Separate the results into main questions data, response type data and response options
  questions_data <- processed_data %>%
    select(
      question_internal_id,
      question_text = Pregunta
    )
    response_data <- processed_data %>%
      select(
        question_internal_id,
        response_type_id,
        response_type_name = `type_name`
      )
  options_data <- response_options %>%
    select(question_internal_id, options) %>%
    unnest(options)
  
  return(list(
    questions = questions_data,
    response_type = response_data,
    response_options = options_data
  ))
}

convert_excel_date <- function(value, datetime = TRUE) {
  if (is.na(value) || value == "") return(NA)
  
  tryCatch({
    excel_date <- as.numeric(value)
    date <- as.POSIXct(
      (excel_date - 25569) * 86400, 
      origin = "1970-01-01",
      tz = "UTC"
    )
    
    # Format for MySQL
    if(datetime) {
      # DATETIME format: YYYY-MM-DD HH:MM:SS
      format(date, "%Y-%m-%d %H:%M:%S")
    } else {
      # DATE format: YYYY-MM-DD
      format(date, "%Y-%m-%d")
    }
  }, error = function(e) NA_character_)
}

convert_response_value <- function(value, response_type_id, datetime = TRUE) {
  # Handle NA/empty values consistently
  if (is.na(value) || value == "" || value == "NA") {
    return(NA)
  }
  
  # Convert based on response type
  converted_value <- switch(
    as.character(response_type_id),
    # Text (1)
    "1" = {
      str_trim(as.character(value))
    },
    # numeric (2)
    "2" = {
      as.character(suppressWarnings(as.numeric(value)))

    },
    # Date (3)
    "3" = {
      convert_excel_date(value, datetime)

    },
    # Default case
    as.character(value)
  )
  
  return(converted_value)
}

process_survey_responses <- function(responses_df, response_type_mapping, datetime = TRUE) {
  processed_responses <- responses_df
  
  for (col in names(responses_df)) {
    response_type <- response_type_mapping %>%
      filter(question_internal_id == col) %>%
      pull(response_type_id)
    
    if (length(response_type) == 1) {
      processed_responses[[col]] <- sapply(
        responses_df[[col]], 
        convert_response_value, 
        response_type_id = response_type,
        datetime = datetime
      )
    }
  }
  
  return(processed_responses)
}