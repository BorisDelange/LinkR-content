##########################################
# Translations                           #
##########################################

new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
  "EN", "new_words_set_added", "New words set added",
  "FR", "new_words_set_added", "Nouveau set de mots ajouté",
  "EN", "delete_words_set", "Delete a words set",
  "FR", "delete_words_set", "Supprimer un set de mots",
  "EN", "delete_words_set_subtext", "Are you sure you want to delete this words set ?",
  "FR", "delete_words_set_subtext", "Etes-vous sûr de vouloir supprimer ce set de mots ?",
  "EN", "words_set_deleted", "Words set deleted",
  "FR", "words_set_deleted", "Set de mots supprimé",
  "EN", "no_data_patient_text", "No patient's text data available",
  "FR", "no_data_patient_text", "Pas de données texte disponibles pour ce patient",
  "EN", "no_data_stay_text", "No stay's text data available",
  "FR", "no_data_stay_text", "Pas de données texte disponibles pour ce séjour"
)

##########################################
# Show / hide divs                       #
##########################################

divs_%group_id%_%study_id% <- c("div_figure_%group_id%_%study_id%", "div_settings_%group_id%_%study_id%",
  "div_manage_words_set_%group_id%_%study_id%", "div_new_words_set_%group_id%_%study_id%", "div_delete_words_set_%group_id%_%study_id%")

observeEvent(input$div_%group_id%_%study_id%, {
  
  sapply(divs_%group_id%_%study_id% %>% setdiff(., input$div_%group_id%_%study_id%), shinyjs::hide)
  shinyjs::show(input$div_%group_id%_%study_id%)
})

##########################################
# Add a new words set                    #
##########################################

observeEvent(input$new_words_set_add_%group_id%_%study_id%, {
  
  tryCatch({
    
    new_name <- input$new_words_set_name_%group_id%_%study_id%
      
      if (length(new_name) == 0) shiny.fluent::updateTextField.shinyInput(session, "new_words_set_name_%group_id%_%study_id%", errorMessage = translate(language, paste0("provide_valid_name"), r$words))
    else shiny.fluent::updateTextField.shinyInput(session, "new_words_set_name_%group_id%_%study_id%", errorMessage = NULL)
    
    req(length(new_name) > 0)
    
    sql <- "SELECT DISTINCT(value) FROM modules_elements_options WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id% AND name = 'words_set_name'" 
    distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull() %>% tolower()
    
    if (tolower(new_name) %in% distinct_values) show_message_bar(output, 2, "name_already_used", "severeWarning", language)
    req(tolower(new_name) %not_in% distinct_values)
    
    last_row <- get_last_row(r$db, "modules_elements_options")
    
    # Insert new words_set in database
    
    sql <- glue::glue_sql("INSERT INTO modules_elements_options(id, group_id, study_id, category, name, value, creator_id, datetime, deleted)
            SELECT {last_row + 1}, %group_id%, %study_id%, 'patient_lvl', 'words_set_name', {new_name}, {r$user_id}, {as.character(Sys.time())}, FALSE", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    # Reset textfield
    shiny.fluent::updateTextField.shinyInput(session, "new_words_set_name_%group_id%_%study_id%", value = NULL)
    
    # Update dropdowns
    r$reload_dropdowns_%group_id%_%study_id% <- Sys.time()
    
    show_message_bar(output, 1, translate(language, "new_words_set_added", new_words), "success", language)
    
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

##########################################
# Delete a words set                     #
##########################################

# Delete button is pressed
observeEvent(input$delete_words_set_validate_%group_id%_%study_id%, r$delete_dialog_%group_id%_%study_id% <- TRUE)

# Rendering react output
observeEvent(r$delete_dialog_%group_id%_%study_id% , {
  output$delete_confirm_%group_id%_%study_id% <- shiny.fluent::renderReact({
    dialogContentProps <- list(
      type = 0,
      title = translate(language, "delete_words_set", new_words),
      closeButtonAriaLabel = "Close",
      subText = translate(language, "delete_words_set_subtext", new_words)
    )
    shiny.fluent::Dialog(
      hidden = !r$delete_dialog_%group_id%_%study_id%,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('hide_dialog_%group_id%_%study_id%', Math.random()); }")),
      dialogContentProps = dialogContentProps,
      modalProps = list(),
      shiny.fluent::DialogFooter(
        shiny.fluent::PrimaryButton.shinyInput(ns("delete_confirmed_%group_id%_%study_id%"), text = translate(language, "delete", r$words)),
        shiny.fluent::DefaultButton.shinyInput(ns("delete_canceled_%group_id%_%study_id%"), text = translate(language, "dont_delete", r$words))
      )
    )
  })
})

# Whether to close or not delete dialog box
observeEvent(input$hide_dialog_%group_id%_%study_id%, r$delete_dialog_%group_id%_%study_id% <- FALSE)
observeEvent(input$delete_canceled_%group_id%_%study_id%, r$delete_dialog_%group_id%_%study_id% <- FALSE)

# When the delete is confirmed
observeEvent(input$delete_confirmed_%group_id%_%study_id%, {
  
  tryCatch({
    
    # Get value of deleted row
    row_deleted <- as.integer(input$delete_words_set_%group_id%_%study_id%)
    
    # Delete rows in database
    sql <- glue::glue_sql("UPDATE modules_elements_options SET deleted = TRUE WHERE id = {row_deleted} OR link_id = {row_deleted}", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    # Close dialog box
    r$delete_dialog_%group_id%_%study_id% <- FALSE
    
    # Notification to user
    show_message_bar(output = output, id = 3, translate(language, "words_set_deleted", new_words), type = "severeWarning", language = language)
    
    r$reload_dropdowns_%group_id%_%study_id% <- Sys.time()
    
    # If this words set is the currently selected words set, reload select words set dropdown
    
    if (length(input$select_words_set_%group_id%_%study_id%) > 0){
      if (input$select_words_set_%group_id%_%study_id% == input$delete_words_set_%group_id%_%study_id%){
        shiny.fluent::updateDropdown.shinyInput(session, "select_words_%group_id%_%study_id%", options = list(), value = NULL)
      }
    }
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

##########################################
# Load words of a words set              #
##########################################

observeEvent(input$manage_words_set_%group_id%_%study_id%, {
  
  tryCatch({
    
    sql <- glue::glue_sql("SELECT * FROM modules_elements_options WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id%
            AND name = 'words_set_word' AND link_id = {input$manage_words_set_%group_id%_%study_id%}", .con = r$db)
    
    r$words_%group_id%_%study_id% <- DBI::dbGetQuery(r$db, sql) %>% dplyr::select(value) %>% dplyr::arrange(value)
    
    shiny.fluent::updateDropdown.shinyInput(session, "manage_words_%group_id%_%study_id%", 
      options = convert_tibble_to_list(data = r$words_%group_id%_%study_id%, key_col = "value", text_col = "value", null_value = FALSE),
      value = r$words_%group_id%_%study_id% %>% dplyr::pull(value), multiSelect = TRUE, multiSelectDelimiter = " || ")
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

##########################################
# Update words in a words set            #
##########################################

observeEvent(input$manage_words_%group_id%_%study_id%, {
  
  tryCatch({
    
    r$words_%group_id%_%study_id% <- r$words_%group_id%_%study_id% %>% dplyr::filter(value %in% input$manage_words_%group_id%_%study_id%) 
    
    if (nrow(r$words_%group_id%_%study_id%) > 0){
      
      r$words_%group_id%_%study_id% <- r$words_%group_id%_%study_id% %>% dplyr::arrange(value)
      
      options <- convert_tibble_to_list(r$words_%group_id%_%study_id% %>% dplyr::arrange(value), key_col = "value", text_col = "value", null_value = FALSE)
      value <- r$words_%group_id%_%study_id% %>% dplyr::pull(value)
    }
    else {
      options <- list()
      value <- NULL
    }
    
    shiny.fluent::updateDropdown.shinyInput(session, "manage_words_%group_id%_%study_id%", options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language)) 
})

##########################################
# Add a new word                         #
##########################################

observeEvent(input$manage_words_add_%group_id%_%study_id%, {
  
  tryCatch({
    
    req(!is.na(input$manage_words_add_name_%group_id%_%study_id%) & input$manage_words_add_name_%group_id%_%study_id% != "")
    req(length(input$manage_words_set_%group_id%_%study_id%) > 0)
    
    if (length(input$manage_words_%group_id%_%study_id%) == 0) r$words_%group_id%_%study_id% <- tibble::tribble(~value, input$manage_words_add_name_%group_id%_%study_id%)
    else r$words_%group_id%_%study_id% <- r$words_%group_id%_%study_id% %>% dplyr::bind_rows(tibble::tribble(~value, input$manage_words_add_name_%group_id%_%study_id%)) %>% dplyr::arrange(value)
    
    shiny.fluent::updateDropdown.shinyInput(session, "manage_words_%group_id%_%study_id%", 
      options = convert_tibble_to_list(data = r$words_%group_id%_%study_id%, key_col = "value", text_col = "value", null_value = FALSE),
      value = r$words_%group_id%_%study_id% %>% dplyr::pull(value))
    
    shiny.fluent::updateTextField.shinyInput(session, "manage_words_add_name_%group_id%_%study_id%", value = "")
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))  
})

##########################################
# Save words of a words set              #
##########################################

observeEvent(input$manage_words_save_%group_id%_%study_id%, {
  
  tryCatch({
    
    req(length(input$manage_words_set_%group_id%_%study_id%) > 0)
    
    # Delete old values
    
    sql <- glue::glue_sql("DELETE FROM modules_elements_options WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id%
            AND name = 'words_set_word' AND link_id = {input$manage_words_set_%group_id%_%study_id%}", .con = r$db)
    DBI::dbSendStatement(r$db, sql) -> query
    DBI::dbClearResult(query)
    
    # Add new ones
    
    if (length(input$manage_words_%group_id%_%study_id%) == 0) r$words_%group_id%_%study_id% <- tibble::tribble(~value)
    
    if (nrow(r$words_%group_id%_%study_id%) > 0){
      
      last_row <- get_last_row(r$db, "modules_elements_options")
      new_rows <- tibble::tibble(id = 1:nrow(r$words_%group_id%_%study_id%) + last_row, group_id = %group_id%, study_id = %study_id%, patient_id = NA_integer_,
        link_id = input$manage_words_set_%group_id%_%study_id%, category = "patient_lvl", name = "words_set_word", value = r$words_%group_id%_%study_id% %>% dplyr::pull(value), value_num = NA_real_, 
        creator_id = r$user_id, datetime = as.character(Sys.time()), deleted = FALSE)
      
      DBI::dbAppendTable(r$db, "modules_elements_options", new_rows)
    }
    
    show_message_bar(output, 1, "modif_saved", "success", language, r$words)
    
    # If this words set is the currently selected words set, reload select words set dropdown
    
    if (length(input$select_words_set_%group_id%_%study_id%) > 0){
      if (input$select_words_set_%group_id%_%study_id% == input$manage_words_set_%group_id%_%study_id%){
        options <- convert_tibble_to_list(DBI::dbGetQuery(r$db, "SELECT * FROM modules_elements_options WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id% AND name = 'words_set_name'"), key_col = "id", text_col = "value")
        shiny.fluent::updateDropdown.shinyInput(session, "select_words_set_%group_id%_%study_id%", options = options, value = NULL)
        shiny.fluent::updateDropdown.shinyInput(session, "select_words_%group_id%_%study_id%", options = list(), value = NULL)
      }
    }
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

##########################################
# Reload data                            #
##########################################

observeEvent(r$reload_dropdowns_%group_id%_%study_id%, {
  
  tryCatch({
    
    # Update dropdowns
    
    options <- convert_tibble_to_list(DBI::dbGetQuery(r$db, "SELECT * FROM modules_elements_options WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id% AND name = 'words_set_name'"), key_col = "id", text_col = "value")
    shiny.fluent::updateDropdown.shinyInput(session, "select_words_set_%group_id%_%study_id%", options = options)
    shiny.fluent::updateDropdown.shinyInput(session, "manage_words_set_%group_id%_%study_id%", options = options)
    shiny.fluent::updateDropdown.shinyInput(session, "delete_words_set_%group_id%_%study_id%", options = options)
    
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

##########################################
# Get results for search & sets          #
##########################################

# Words set chosen

observeEvent(input$select_words_set_%group_id%_%study_id%, {
  
  tryCatch({
    
    sql <- glue::glue_sql("SELECT * FROM modules_elements_options WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id%
            AND name = 'words_set_word' AND link_id = {input$select_words_set_%group_id%_%study_id%}", .con = r$db)
    
    words <- DBI::dbGetQuery(r$db, sql) 
    if (nrow(words) > 0) words <- words %>% dplyr::select(value) %>% dplyr::arrange(value)
    options <- convert_tibble_to_list(words, key_col = "value", text_col = "value", null_value = FALSE)
    
    shiny.fluent::updateDropdown.shinyInput(session, "select_words_%group_id%_%study_id%", 
      options = options, value = words %>% dplyr::pull(value), multiSelect = TRUE, multiSelectDelimiter = " || ")
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

# Search words button clicked

observeEvent(input$search_words_%group_id%_%study_id%, r$reload_%group_id%_%study_id% <- Sys.time())

# Show all data

observeEvent(input$show_all_data_%group_id%_%study_id%, {
  
  r$reload_%group_id%_%study_id% <- paste0(Sys.time(), "_show_all_data")
  
  options <- convert_tibble_to_list(DBI::dbGetQuery(r$db, "SELECT * FROM modules_elements_options WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id% AND name = 'words_set_name'"), key_col = "id", text_col = "value")
  shiny.fluent::updateDropdown.shinyInput(session, "select_words_set_%group_id%_%study_id%", options = options, value = NULL)
  shiny.fluent::updateDropdown.shinyInput(session, "select_words_%group_id%_%study_id%", options = list(), value = NULL)
})

##########################################
# Settings                               #
##########################################

# Text : ascending order or not

observeEvent(input$more_recent_up_%group_id%_%study_id%, r$reload_%group_id%_%study_id% <- Sys.time())

# Show only last version

# ...

##########################################
# Load & render text                     #
##########################################

# Initiate marker
marker_%group_id%_%study_id% <- marker::marker$new("#text_result_%group_id%_%study_id%")

observeEvent(input$data_choice_%group_id%_%study_id%, r$reload_%group_id%_%study_id% <- Sys.time())

observeEvent(r$data_patient, r$reload_%group_id%_%study_id% <- Sys.time())
observeEvent(r$data_stay, r$reload_%group_id%_%study_id% <- Sys.time())

output$text_%group_id%_%study_id% <- renderUI({
  
  r$reload_%group_id%_%study_id%
    
    tryCatch({
      
      result <- tagList()
      
      data_temp <- tibble::tibble()
      
      if (isolate(input$data_choice_%group_id%_%study_id%) == "all_stays"){
        
        if (nrow(r$data_patient$text) > 0){
          
          data_temp <-
            r$data_patient$text %>%
            dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id"))
        }
      } 
      if (isolate(input$data_choice_%group_id%_%study_id%) == "current_stay"){
        
        if (nrow(r$data_stay$text) > 0){
          
          data_temp <- 
            r$data_stay$text %>%
            dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id"))
        }
      }
      
      # If there are words to filter
      if (length(isolate(input$select_words_%group_id%_%study_id%)) > 0){
        if (!grepl("show_all_data", r$reload_%group_id%_%study_id%) & nrow(data_temp) > 0) data_temp <- data_temp %>% 
            dplyr::filter(grepl(paste(tolower(isolate(input$select_words_%group_id%_%study_id%)), collapse = "|"), tolower(value)))
      }
      
      if (nrow(data_temp) > 0){
        
        if (input$more_recent_up_%group_id%_%study_id%) data_temp <- data_temp %>% dplyr::arrange(desc(datetime_start))
        else data_temp <- data_temp %>% dplyr::arrange(datetime_start)
        
        sapply(1:nrow(data_temp), function(i){
          row <- data_temp[i, ]
          result <<- tagList(result, br(),
            div(id = "text_result_%group_id%_%study_id%",
              strong(paste0(row$datetime_start, " - ", row$display_name)), br(), br(),
              HTML(row$value %>% stringr::str_replace_all("\n", "<br />")),
              style = "border: dashed 1px; padding: 10px"
            ),
          )
        })
      }
      
      if (nrow(data_temp) == 0) result <- div(br(), shiny.fluent::MessageBar(translate(language, "DT_empty", r$words), messageBarType = 0), style = "margin-top:10px;")
      
      r$load_markers_%group_id%_%study_id% <- Sys.time()
      
      result
      
    },
      error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

observeEvent(r$load_markers_%group_id%_%study_id%, {
  
  tryCatch({
    
    clear_markers <- TRUE
    
    if (length(isolate(input$select_words_%group_id%_%study_id%)) > 0){
      if (!grepl("show_all_data", r$reload_%group_id%_%study_id%)) clear_markers <- FALSE
    }
    
    if (clear_markers) sapply(1:10, function(i) marker_%group_id%_%study_id%$unmark(className = paste0("colour", i)))
    else {
      
      i <- 1
      
      sapply(input$select_words_%group_id%_%study_id%, function(word){
        
        i <<- i + 1
        if (i == 11) i <<- 1
        
        marker_%group_id%_%study_id%$unmark(className = paste0("colour", i))$mark(word, className = paste0("colour", i))
      })
    }
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})