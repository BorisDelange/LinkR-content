##########################################
# Translations                           #
##########################################

new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
  "", "", ""
)

##########################################
# Reload data                            #
##########################################

r$reload_%group_id%_%study_id% <- Sys.time()
observeEvent(r$data_patient, r$reload_%group_id%_%study_id% <- Sys.time())

observeEvent(r$reload_%group_id%_%study_id%, {
  
  tryCatch({
    
    req(!is.na(r$chosen_patient))
    
    sql <- glue::glue_sql("SELECT f1.id, f1.link_id, f1.value AS feature_value FROM modules_elements_options f1
            WHERE f1.deleted IS FALSE AND f1.study_id = {r$chosen_study} AND f1.patient_id = {r$chosen_patient} AND f1.category = 'patient_lvl' AND f1.name = 'feature_value'", .con = r$db)
    features_values <- DBI::dbGetQuery(r$db, sql)
    
    sql <- glue::glue_sql("SELECT f1.id, f1.link_id, f1.value AS feature_datetime FROM modules_elements_options f1
            WHERE f1.deleted IS FALSE AND f1.study_id = {r$chosen_study} AND f1.patient_id = {r$chosen_patient} AND f1.category = 'patient_lvl' AND f1.name = 'feature_datetime'", .con = r$db)
    features_datetimes <- DBI::dbGetQuery(r$db, sql)
    
    sapply(r$features_%group_id%_%study_id%, function(input_name){
      
      input_id <- substr(input_name, nchar("feature_") + 1, 100) %>% stringr::str_replace_all("_%group_id%_%study_id%", "") %>% as.integer()
      
      sql <- glue::glue_sql("SELECT value FROM modules_elements_options WHERE link_id = {input_id} AND category = 'aggregated' AND name = 'feature_type'", .con = r$db)
      feature_type <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(value)
      
      # By default, feature_date is the admission date
      value <- NULL
      if (feature_type == "toggle") value <- FALSE
      feature_date <- r$data_patient$stays %>% dplyr::pull(admission_datetime) %>% min() %>% as.Date() %>% as.character()
      feature_time <- "00:00:00"
      
      if (nrow(features_values) > 0){
        value <- features_values %>% dplyr::filter(link_id == input_id) %>% dplyr::pull(feature_value)
        feature_datetime <- features_datetimes %>% dplyr::filter(link_id == input_id) %>% dplyr::pull(feature_datetime) %>% lubridate::ymd_hms()
        feature_date <- as.character(as.Date(feature_datetime))
        feature_time <- as.character(format(feature_datetime, format = "%H:%M:%S"))
      }
      
      if (feature_type == "toggle"){
        print(value)
        print(as.logical(as.integer(value)))
      } 
      
      shiny.fluent::updateDatePicker.shinyInput(session, paste0("date_", input_name), value = feature_date)
      shiny.fluent::updateDatePicker.shinyInput(session, paste0("time_", input_name), value = feature_time)
      if (feature_type == "dropdown") shiny.fluent::updateDropdown.shinyInput(session, input_name, value = as.integer(value))
      if (feature_type == "textfield") shiny.fluent::updateTextField.shinyInput(session, input_name, value = value)
      if (feature_type == "toggle") shiny.fluent::updateToggle.shinyInput(session, input_name, value = as.logical(as.integer(value)))
      
    })
  })
})

##########################################
# Save updates                           #
##########################################

observeEvent(input$save_%group_id%_%study_id%, {
  
  tryCatch({
    
    # For each input
    
    sapply(r$features_%group_id%_%study_id%, function(input_name){
      
      input_id <- substr(input_name, nchar("feature_") + 1, 100) %>% stringr::str_replace_all("_%group_id%_%study_id%", "") %>% as.integer()
      
      sql <- glue::glue_sql("SELECT value FROM modules_elements_options WHERE link_id = {input_id} AND category = 'aggregated' AND name = 'feature_type'", .con = r$db)
      feature_type <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(value)
      
      value <- input[[input_name]]
      feature_date <- as.character(as.Date(substr(input[[paste0("date_", input_name)]], 1, 10)) + lubridate::days(1))
      
      if (length(input[[paste0("time_", input_name)]]) > 0){
        feature_time <- stringr::str_extract(input[[paste0("time_", input_name)]], "^[0-9]{2}:[0-9]{2}:[0-9]{2}$")
        if (is.na(feature_time)) feature_time <- "00:00:00"
      }
      else feature_time <- "00:00:00"
      
      feature_datetime <- paste0(feature_date, " ", feature_time)
      
      if (feature_type == "toggle" & length(input[[input_name]]) == 0) value <- "false"
      
      sql <- glue::glue_sql("SELECT * FROM modules_elements_options WHERE study_id = {r$chosen_study} AND patient_id = {r$chosen_patient}
                AND category = 'patient_lvl' AND name = 'feature_value' AND link_id = {input_id}", .con = r$db)
      row_exists <- DBI::dbGetQuery(r$db, sql)
      
      if (nrow(row_exists) == 0){
        
        last_row <- get_last_row(r$db, "modules_elements_options")
        
        sql <- glue::glue_sql("INSERT INTO modules_elements_options(id, study_id, patient_id, link_id, category, name, value, creator_id, datetime, deleted)
                    SELECT {last_row + 1}, {r$chosen_study}, {r$chosen_patient}, {input_id}, 'patient_lvl', 'feature_value', {value}, {r$user_id}, {as.character(Sys.time())}, FALSE
                    UNION SELECT {last_row + 2}, {r$chosen_study}, {r$chosen_patient}, {input_id}, 'patient_lvl', 'feature_datetime', {feature_datetime}, {r$user_id}, {as.character(Sys.time())}, FALSE", 
          .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
      }
      
      # Update row if exists
      
      if (nrow(row_exists) > 0){
        sql <- glue::glue_sql("UPDATE modules_elements_options SET value = {value}
                    WHERE link_id = {input_id} AND study_id = {r$chosen_study} AND patient_id = {r$chosen_patient} AND category = 'patient_lvl' AND name = 'feature_value'", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
        
        sql <- glue::glue_sql("UPDATE modules_elements_options SET value = {feature_datetime}
                    WHERE link_id = {input_id} AND study_id = {r$chosen_study} AND patient_id = {r$chosen_patient} AND category = 'patient_lvl' AND name = 'feature_datetime'", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
      }
    })
    
    show_message_bar(output, 1, "modif_saved", "success", language)
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})