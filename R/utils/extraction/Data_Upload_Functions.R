library(RMySQL)
library(DBI)
library(dplyr)

# Step 1: Response Types
insert_response_types <- function(con, types_df) {
  dbBegin(con)
  tryCatch({
    # Clear existing response types if needed
    dbExecute(con, "DELETE FROM response_types")
    
    # Get unique response types
    unique_types <- types_df %>%
      distinct(response_type_id, response_type_name)
    
    # Insert response types
    for(i in 1:nrow(unique_types)) {
      sql <- "INSERT INTO response_types (response_type_id, response_type_name) VALUES (?, ?)"
      query <- sqlInterpolate(
        con,
        sql,
        unique_types$response_type_id[i],
        unique_types$response_type_name[i]
      )
      dbExecute(con, query)
    }
    dbCommit(con)
    return(TRUE)
  }, error = function(e) {
    dbRollback(con)
    message("Error inserting response types: ", e$message)
    return(FALSE)
  })
}
insert_questions <- function(con, questions_df) {
  id_mapping <- data.frame(
    internal_id = character(),
    db_id = integer(),
    stringsAsFactors = FALSE
  )
  
  dbBegin(con)
  tryCatch({
    for(i in 1:nrow(questions_df)) {
      sql <- "INSERT INTO questions (question_text) VALUES (?)"
      query <- sqlInterpolate(
        con,
        sql,
        questions_df$question_text[i]
      )
      dbExecute(con, query)
      
      # Get the inserted ID
      db_id <- dbGetQuery(con, "SELECT LAST_INSERT_ID() as id")$id
      
      # Add to mapping
      id_mapping <- rbind(id_mapping, data.frame(
        internal_id = questions_df$question_internal_id[i],
        db_id = db_id,
        stringsAsFactors = FALSE
      ))
    }
    dbCommit(con)
    return(id_mapping)
  }, error = function(e) {
    dbRollback(con)
    message("Error inserting questions: ", e$message)
    return(NULL)
  })
}

#  Step 3 QSRT 

insert_qsrt <- function(con, response_type_df, id_mapping, survey_id) {
  qsrt_mapping <- data.frame(
    internal_id = character(),
    qsrt_id = integer(),
    stringsAsFactors = FALSE
  )
  
  dbBegin(con)
  tryCatch({
    for(i in 1:nrow(response_type_df)) {
      internal_id <- response_type_df$question_internal_id[i]
      question_id <- id_mapping$db_id[id_mapping$internal_id == internal_id]
      
      sql <- "INSERT INTO question_survey_response_type (question_id, survey_id, response_type_id) VALUES (?, ?, ?)"
      query <- sqlInterpolate(
        con,
        sql,
        question_id,
        survey_id,
        response_type_df$response_type_id[i]
      )
      dbExecute(con, query)
      
      # Get the inserted QSRT ID
      qsrt_id <- dbGetQuery(con, "SELECT LAST_INSERT_ID() as id")$id
      
      # Add to mapping
      qsrt_mapping <- rbind(qsrt_mapping, data.frame(
        internal_id = internal_id,
        qsrt_id = qsrt_id,
        stringsAsFactors = FALSE
      ))
    }
    dbCommit(con)
    return(qsrt_mapping)
  }, error = function(e) {
    dbRollback(con)
    message("Error inserting QSRT: ", e$message)
    return(NULL)
  })
}

# Response Options Table
insert_response_options <- function(con, options_df, qsrt_mapping,dependency_df = NULL) {
  dbBegin(con)
  tryCatch({
    for(i in 1:nrow(options_df)) {
      internal_id <- options_df$question_internal_id[i]
      qsrt_id <- qsrt_mapping$qsrt_id[qsrt_mapping$internal_id == internal_id]
      
      # Handle special_code_type - convert NA to empty string
      special_code <- if(is.na(options_df$special_code_type[i])) "" else options_df$special_code_type[i]
      
      sql <- "INSERT INTO response_options (qsrt_id, original_code, original_label, is_special_code, special_code_type) VALUES (?, ?, ?, ?, ?)"
      query <- sqlInterpolate(
        con,
        sql,
        qsrt_id,
        options_df$original_code[i],
        options_df$original_label[i],
        ifelse(options_df$is_special_code[i], 1, 0),
        special_code
      )
      dbExecute(con, query)
    }
    dbCommit(con)
    return(TRUE)
  }, error = function(e) {
    dbRollback(con)
    message("Error inserting response options: ", e$message)
    return(FALSE)
  })
}


# Step 5: Insert Responses (Fixed version)
insert_responses <- function(con, responses_df, qsrt_mapping) {
  dbBegin(con)
  tryCatch({
    
    # Add row numbers as participant IDs before pivoting
    responses_df$participant_id <- 1:nrow(responses_df)
    
    # Convert responses from wide to long format
    responses_long <- responses_df %>%
      pivot_longer(
        cols = starts_with("IC"),
        names_to = "internal_id",
        values_to = "value"
      ) %>%
      # Keep only necessary columns
      select(participant_id, internal_id, value)
    
    # Insert responses
    for(i in 1:nrow(responses_long)) {
      qsrt_id <- qsrt_mapping$qsrt_id[qsrt_mapping$internal_id == responses_long$internal_id[i]]
      
      if(!is.na(qsrt_id)) {
        sql <- "INSERT INTO responses (participant_id, qsrt_id, value) VALUES (?, ?, ?)"
        query <- sqlInterpolate(
          con,
          sql,
          responses_long$participant_id[i],
          qsrt_id,
          responses_long$value[i]
        )
        dbExecute(con, query)
      }
    }
    
    dbCommit(con)
    return(TRUE)
  }, error = function(e) {
    dbRollback(con)
    message("Error inserting responses: ", e$message)
    return(FALSE)
  })
}


# Extra, question dependencies
update_question_dependency <- function(con, dependency_df, qsrt_mapping) {
  dbBegin(con)
  tryCatch({
    for(i in 1:nrow(dependency_df)) {
      if(!is.na(dependency_df$internal_id[i])) {  # Skip rows with NA internal_id
        # Get the database question_id using our mapping
        question_id <- qsrt_mapping$qsrt_id[qsrt_mapping$internal_id == dependency_df$internal_id[i]]

        if(!is.na(question_id)) {
          # Prepare the has_parent value (0 or 1)
          dependency_type <- ifelse(!is.na(dependency_df$dependency_type[i]), dependency_df$dependency_type[i], 'None')
          # Prepare the parent_question value (NULL if NA)
          parent_question <- if(is.na(dependency_df$parent_question[i])) NA else dependency_df$parent_question[i]

          sql <- "UPDATE question_survey_response_type SET dependency_type = ?, parent_question = ? WHERE qsrt_id = ?"
          query <- sqlInterpolate(
            con,
            sql,
            dependency_type,
            parent_question,
            question_id
          )
          dbExecute(con, query)
        }
      }
    }
    dbCommit(con)
    return(TRUE)
  }, error = function(e) {
    dbRollback(con)
    message("Error updating question dependencies: ", e$message)
    return(FALSE)
  })
}