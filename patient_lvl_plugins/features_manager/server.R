##########################################
# Translations                           #
##########################################

new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
  "", "", ""
)

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
      if (feature_type == "toggle" & length(input[[input_name]]) == 0) value <- "false"
      
      sql <- glue::glue_sql("SELECT * FROM modules_elements_options WHERE study_id = %study_id% AND patient_id = %study_id%
                AND category = 'patient_lvl' AND name = 'feature_value' AND link_id = {input_id}", .con = r$db)
      row_exists <- DBI::dbGetQuery(r$db, sql)
      
      if (nrow(row_exists) == 0){
        
        last_row <- get_last_row(r$db, "modules_elements_options")
        
        sql <- glue::glue_sql("INSERT INTO modules_elements_options(id, study_id, patient_id, link_id, category, name, value, creator_id, datetime, deleted)
                    SELECT {last_row + 1}, %study_id%, %study_id%, {input_id}, 'patient_lvl', 'feature_value', {value},
                    {r$user_id}, {as.character(Sys.time())}, FALSE", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
      }
      
      # Update row if exists
      
      if (nrow(row_exists) > 0){
        sql <- glue::glue_sql("UPDATE modules_elements_options SET value = {value}
                    WHERE link_id = {input_id} AND study_id = %study_id% AND patient_id = %study_id% AND category = 'patient_lvl' AND name = 'feature_value'", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
      }
    })
    
    show_message_bar(output, 1, "modif_saved", "success", language)
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})