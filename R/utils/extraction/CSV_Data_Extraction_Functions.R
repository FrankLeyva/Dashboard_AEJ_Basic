#install.packages('readxl')
#install.packages('tidyverse')
#install.packages('janitor')
library(readxl)
library(tidyverse)
library(janitor)

read_survey_sheet  <- function(file_path, sheet_name) {
  sheet_data  <- read_excel(
    file_path, 
    sheet = sheet_name,
    col_types = "text",   # Prevents automatic type inference
  ) 
  return(sheet_data )
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


detect_basic_type <- function(row) {
  # Simplified response types map
response_types_map <- tribble(
  ~type_id, ~type_name,
  1,        "TEXT",
  2,        "NUMERIC",
  3,        "DATE"
)
  # Extract columns that are potential response options (numbered 0-20)
  response_values <- row[grep("^\\d+$", names(row))]
  
  # First check for dates and text
  if (!is.null(row$`Tipo de pregunta`)==TRUE){
    if (str_detect(row$`Tipo de pregunta`, "Fecha")) {
    return(3) # DATE
    }
    if (str_detect(row$`Tipo de pregunta`, "Cadena|Texto")) {
    return(1) # TEXT
    }
}
  
if (!is.null(row$`label`)==TRUE){
  if (str_detect(row$`label`, "POSIXct")) {
    return(3) # DATE
  }
  if (str_detect(row$`label`, "character")) {
    return(1) # TEXT
  }
}
return(2) # NUMERIC
}
extract_response_options <- function(row) {
  # Get columns that contain response options
  response_cols <- grep("^\\d+$", names(row))
  values <- row[response_cols]
  # Create a data frame of codes and labels
  options <- tibble(
    original_code = as.integer(names(values)),
    original_label = as.character(unlist(values))
  ) %>%
    filter(!is.na(original_label) & original_label != "")
  
  # Detect special codes
options <- options %>%
    mutate(
      is_special_code = str_detect(original_label, "(?i)\\b(NS|NC|NS/NC|NC/NS|NC/No sé|NC/No se|NS/No contestó|NS/No contesta|No S.|No S||NA|No Aplica|No contest.|No contest)\\b"),
      special_code_type = case_when(
        str_detect(original_label, "(?i)\\b(NS|NC|NS/NC|NC/NS|NC/No sé|NC/No se|NS/No contestó|NS/No contesta|No Sé)\\b") ~ "NC",
        str_detect(original_label, "(?i)\\b(NA|No Aplica)\\b") ~ "NA",
        TRUE ~ NA_character_
      )
    )

  return(options)
}
create_formatted_id <- function(number) {
  sprintf("IC%03d", number)  # Creates IDs like IC001, IC002, etc.
}
# Main function to process coding manual
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
      response_type_id = detect_basic_type(cur_data())
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
  
  # Determine which column to use for question_text
  question_text_column <- if ("Pregunta" %in% colnames(manual)) "Pregunta" else "label"
  
  # Separate the results into main questions data, response type data, and response options
  questions_data <- processed_data %>%
    mutate(question_text = dplyr::pull(., question_text_column)) %>%
    select(
      question_internal_id,
      question_text
    )
  response_data <- processed_data %>%
    select(
      question_internal_id,
      response_type_id,
      response_type_name = type_name
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
  