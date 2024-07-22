# Reload words sets

observeEvent(input$reload_words_sets_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$reload_words_sets_%widget_id%"))
    
    tryCatch({
    
        if (length(input$saved_settings_%widget_id%) > 0) link_id <- input$saved_settings_%widget_id% else link_id <- 0
        sql <- glue::glue_sql("SELECT id, value FROM widgets_options WHERE widget_id = %widget_id% AND link_id = {link_id} AND category = 'words_set'", .con = m$db)
        
        words_sets <- DBI::dbGetQuery(m$db, sql)
        m$words_sets_%widget_id% <- words_sets %>% dplyr::select(id, text = value)
        words_sets_dropdown <- words_sets %>% convert_tibble_to_list(key_col = "id", text_col = "value")
        
        if (length(input$update_words_set_value_%widget_id%) > 0) value <- input$update_words_set_value_%widget_id%
        else {
            sapply(c("words_set_details_div_%widget_id%", "delete_words_set_div_%widget_id%"), shinyjs::hide)
            value <- NULL
        }
        
        shiny.fluent::updateDropdown.shinyInput(session, "words_set_%widget_id%", options = words_sets_dropdown, value = value)
        shiny.fluent::updateDropdown.shinyInput(session, "filters_words_set_%widget_id%", options = words_sets_dropdown, value = NULL)
        
        # Hide filters / words set add button
         shinyjs::hide("filters_add_words_set_div_%widget_id%")
        
    }, error = function(e) cat(paste0("\\n", now(), " - ", toString(e))))
})

shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_words_sets_%widget_id%', Math.random())"))

# A words set is selected

observeEvent(input$words_set_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$words_set_%widget_id%"))
    
    sapply(c("delete_words_set_div_%widget_id%", "words_set_details_div_%widget_id%"), shinyjs::show)
    
    tryCatch({
        
        # Load words list
        words_set_id <- input$words_set_%widget_id%
        sql <- glue::glue_sql("SELECT id, link_id, value FROM widgets_options WHERE widget_id = %widget_id% AND category = 'word' AND link_id = {words_set_id}", .con = m$db)
        m$words_%widget_id% <- 
            m$words_%widget_id% %>%
            dplyr::filter(link_id != words_set_id) %>%
            dplyr::bind_rows(DBI::dbGetQuery(m$db, sql))
        
        # Reload words list
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_words_list_%widget_id%', Math.random())"))
        
    }, error = function(e) cat(paste0("\\n", now(), " - ", toString(e))))
})


# Add a new words set

## Show add new words set div
observeEvent(input$new_words_set_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$new_words_set_%widget_id%"))
    shinyjs::show("add_words_set_modal_%widget_id%")
})

## Cancel new words set
observeEvent(input$close_add_words_set_modal_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$cancel_new_words_set_%widget_id%"))
    shinyjs::hide("add_words_set_modal_%widget_id%")
})

## Add button clicked
observeEvent(input$add_words_set_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$add_words_set_%widget_id%"))
    
    tryCatch({
    
        # A settings file must be selected
        if (length(input$saved_settings_%widget_id%) == 0){
            shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_saved_settings_tab_%widget_id%', Math.random());"))
            shinyjs::hide("add_words_set_modal_%widget_id%")
            stop()
        }
        
        if (input$new_words_set_name_%widget_id% == ""){
            shiny.fluent::updateTextField.shinyInput(session, "new_words_set_name_%widget_id%", errorMessage = i18np$t("provide_valid_name"))
            stop()
        }
        new_name <- input$new_words_set_name_%widget_id%
        
        # Check if the name is already used
        
        words_set_id <- input$words_set_%widget_id%
        saved_settings_file_id <- input$saved_settings_%widget_id%
        
        sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id% AND link_id = {saved_settings_file_id} AND category = 'words_set' AND value = {new_name}", .con = m$db)
        result <- DBI::dbGetQuery(m$db, sql)
        if (nrow(result) > 0){
            shiny.fluent::updateTextField.shinyInput(session, "new_words_set_name_%widget_id%", errorMessage = i18np$t("name_already_used"))
            stop()
        }
        
        shiny.fluent::updateTextField.shinyInput(session, "new_words_set_name_%widget_id%", errorMessage = NULL)
            
        # Add new words set in database
        new_options_row <- get_last_row(m$db, "widgets_options") + 1
        
        new_options <- tibble::tibble(
            id = new_options_row, widget_id = %widget_id%, person_id = NA_integer_, link_id = saved_settings_file_id,
            category = "words_set", name = NA_character_, value = new_name, value_num = NA_integer_,
            creator_id = NA_integer_, datetime = now(), deleted = FALSE)
            
        DBI::dbAppendTable(m$db, "widgets_options", new_options)
        
        # Update dropdown of words sets
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_words_set_value_%widget_id%', ", new_options_row, ")"))
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_words_sets_%widget_id%', Math.random())"))
        
        # Reset textfield
        shiny.fluent::updateTextField.shinyInput(session, "new_words_set_name_%widget_id%", value = "")
        
        # Notify user
        show_message_bar(output, "new_words_set_added", "success", i18n = i18np, ns = ns)
        
        # Return to all words sets div
        shinyjs::hide("add_words_set_modal_%widget_id%")
        
    }, error = function(e) cat(paste0("\\n", now(), " - ", toString(e))))
})

# Delete a words set

## Open modal

observeEvent(input$delete_words_set_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$delete_words_set_%widget_id%"))
    shinyjs::show("delete_words_set_modal_%widget_id%")
})

## Close modal

observeEvent(input$close_delete_words_set_modal_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$close_delete_words_set_modal_%widget_id%"))
    shinyjs::hide("delete_words_set_modal_%widget_id%")
})

## Deletion confirmed

observeEvent(input$confirm_words_set_deletion_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$confirm_words_set_deletion_%widget_id%"))
    
    # Delete rows in db
    widgets_options_id <- input$words_set_%widget_id%
    
    sql <- glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND category = 'words_set' AND id = {widgets_options_id}", .con = m$db)
    sql_send_statement(m$db, sql)
    
    sql <- glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND category = 'word' AND link_id = {widgets_options_id}", .con = m$db)
    sql_send_statement(m$db, sql)
    
    # Reload words list
    m$words_%widget_id% <- tibble::tibble(id = integer(), link_id = integer(), text = character())
    
    # Update dropdown
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_words_set_value_%widget_id%', null)"))
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_words_sets_%widget_id%', Math.random())"))
    
    # Notify user
    show_message_bar(output, "words_set_deleted", "warning", i18n = i18np, ns = ns)
    
    # Close modal
    shinyjs::hide("delete_words_set_modal_%widget_id%")
})
