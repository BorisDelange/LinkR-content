# Settings / editor side-by-side with figure
observeEvent(input$figure_and_settings_side_by_side_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$figure_and_settings_side_by_side"))
    
    tryCatch({
        if (input$figure_and_settings_side_by_side_%widget_id%){
            shinyjs::runjs(paste0(
                "$('#", id, "-figure_div_%widget_id%').css('width', '50%');",
                "$('#", id, "-figure_settings_div_%widget_id%').css('width', '50%');",
                "$('#", id, "-code_div_%widget_id%').css('width', '50%');"
            ))
            shinyjs::hide("figure_button_div_%widget_id%")
        }
        else {
            shinyjs::runjs(paste0(
                "$('#", id, "-figure_div_%widget_id%').css('width', '100%');",
                "$('#", id, "-figure_settings_div_%widget_id%').css('width', '100%');",
                "$('#", id, "-code_div_%widget_id%').css('width', '100%');"
            ))
            shinyjs::show("figure_button_div_%widget_id%")
        }
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# Save general settings in db

observeEvent(input$save_general_settings_button_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$save_general_settings_button"))
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_general_settings_%widget_id%', Math.random());"))
    
    # Notify user
    show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
})
observeEvent(input$save_general_settings_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$save_general_settings"))
    
    tryCatch({
    
        # Delete old rows
        sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND category = 'general_settings'", .con = m$db))
        
        file_id <- input$saved_settings_%widget_id%
        new_data <- tibble::tibble(name = "selected_file_id", value = NA_character_, value_num = file_id)
        
        for (name in c("show_saved_file", "figure_and_settings_side_by_side", "run_code_at_patient_update")){
            toggle_value <- 0L
            input_name <- paste0(name, "_%widget_id%")
            if (length(input[[input_name]]) > 0) if (input[[input_name]]) toggle_value <- 1L
            new_data <- new_data %>% dplyr::bind_rows(tibble::tibble(name = name, value = NA_character_, value_num = toggle_value))
        }
        
        new_data <-
            new_data %>%
            dplyr::transmute(
                id = get_last_row(m$db, "widgets_options") + 1:4, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
                category = "general_settings", name, value, value_num, creator_id = m$user_id, datetime = now(), deleted = FALSE
            )
        
        DBI::dbAppendTable(m$db, "widgets_options", new_data)
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

#Load settings file
# shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-load_settings_file_%widget_id%', Math.random());"))
# 
# observeEvent(input$load_settings_file_%widget_id%, {
#     %req%
#     if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$load_general_settings"))
#     
#     tryCatch({
#         
#         # Get data from db
#         sql <- glue::glue_sql("SELECT name, value, value_num FROM widgets_options WHERE widget_id = %widget_id% AND category = 'general_settings' AND name = 'selected_file_id'", .con = m$db)
#         loaded_file <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull(value_num)
#         if (length(loaded_file) == 0) loaded_file <- NULL
#         
#         # Reload dropdown
#         sql <- glue::glue_sql("SELECT id, value AS name FROM widgets_options WHERE widget_id = %widget_id% AND category = 'saved_settings' AND name = 'file_name'", .con = m$db)
#         m$settings_filenames_%widget_id% <- DBI::dbGetQuery(m$db, sql)
#         
#         dropdown_options <- convert_tibble_to_list(m$settings_filenames_%widget_id%, key_col = "id", text_col = "name")
#         shiny.fluent::updateDropdown.shinyInput(session, "saved_settings_%widget_id%", options = dropdown_options, value = loaded_file)
#     
#     }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
# })
