saved_settings_ui_style <- paste0(
    "display: inline-block; color: white; max-width: 250px; border-radius: 8px; padding: 1px 5px; align-items: center;",
    "height: 18px; font-weight: 600; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; cursor: pointer; margin: 8px 0 0 10px;"
)

output$saved_settings_ui_%widget_id% <- renderUI({
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer output$saved_settings"))
    
    div(i18np$t("no_settings_file_selected"), style = paste0(saved_settings_ui_style, "background-color: #606060ab;"))
})

# Show / hide saved settings file
observeEvent(input$show_saved_file_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$show_saved_file"))
    
    if (input$show_saved_file_%widget_id%) shinyjs::show("saved_settings_ui_%widget_id%")
    else shinyjs::hide("saved_settings_ui_%widget_id%")
})

# Show / hide saved settings div
observeEvent(input$show_saved_settings_tab_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$show_saved_settings_tab"))
    
    sapply(c(paste0(tabs, "_div_%widget_id%"), "figure_settings_code_div_%widget_id%"), shinyjs::hide)
    shinyjs::show("saved_settings_div_%widget_id%")
})

# Show add settings file modal
observeEvent(input$create_settings_file_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$create_settings_file"))
    
    shinyjs::show("add_settings_file_modal_%widget_id%")
})

# Close add settings file modal
observeEvent(input$close_add_settings_file_modal_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$close_add_settings_file_modal"))
    
    shinyjs::hide("add_settings_file_modal_%widget_id%")
})

# Confirm creation of settings file
observeEvent(input$add_settings_file_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$add_settings_file"))
    
    tryCatch({
        file_name <- input$settings_file_name_%widget_id%
        
        # Check if name if empty
        empty_name <- TRUE
        if (length(file_name) > 0) if (!is.na(file_name) & file_name != "") empty_name <- FALSE
        if (empty_name) shiny.fluent::updateTextField.shinyInput(session, "settings_file_name_%widget_id%", errorMessage = i18np$t("provide_valid_name"))
        else {
        
            shiny.fluent::updateTextField.shinyInput(session, "settings_file_name_%widget_id%", errorMessage = NULL)
            
            # Check if name is already used
            sql <- glue::glue_sql("SELECT name FROM widgets_options WHERE widget_id = %widget_id% AND category = 'saved_settings' AND name = 'file_name' AND LOWER(value) = {tolower(file_name)}", .con = m$db)
            name_already_used <- nrow(DBI::dbGetQuery(m$db, sql) > 0)
            
            if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "settings_file_name_%widget_id%", errorMessage = i18np$t("name_already_used"))
            else {
                
                # Add settings file in database
                new_data <- tibble::tibble(
                    id = get_last_row(m$db, "widgets_options") + 1, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
                    category = "saved_settings", name = "file_name", value = file_name, value_num = NA_real_, creator_id = m$user_id, datetime = now(), deleted = FALSE
                )
                DBI::dbAppendTable(m$db, "widgets_options", new_data)
                
                # Reset fields
                shiny.fluent::updateTextField.shinyInput(session, "settings_file_name_%widget_id%", value = "")
                
                # Update dropdown
                shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_dropdown_%widget_id%', Math.random());"))
                
                # Close modal
                shinyjs::hide("add_settings_file_modal_%widget_id%")
            }
        }
    }, error = function(e) cat(paste0("\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# Update dropdown
observeEvent(input$reload_dropdown_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$settings_file"))
    
    tryCatch({
        sql <- glue::glue_sql("SELECT id, value AS name FROM widgets_options WHERE widget_id = %widget_id% AND category = 'saved_settings' AND name = 'file_name'", .con = m$db)
        m$settings_filenames_%widget_id% <- DBI::dbGetQuery(m$db, sql)
        
        dropdown_options <- convert_tibble_to_list(m$settings_filenames_%widget_id%, key_col = "id", text_col = "name")
        shiny.fluent::updateDropdown.shinyInput(session, "saved_settings_%widget_id%", options = dropdown_options)
    }, error = function(e) cat(paste0("\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# A settings file is selected
observeEvent(input$saved_settings_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$settings_file"))
    
    tryCatch({
    
        # Show delete button
        shinyjs::show("delete_saved_settings_file_div_%widget_id%")
    
        # Get file name
        filename <- m$settings_filenames_%widget_id% %>% dplyr::filter(id == input$saved_settings_%widget_id%) %>% dplyr::pull(name)
        
        output$saved_settings_ui_%widget_id% <- renderUI(div(filename, style = paste0(saved_settings_ui_style, "background-color: #1d94ce;")))
        
    }, error = function(e) cat(paste0("\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# Open delete a settings file modal
observeEvent(input$delete_saved_settings_file_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$delete_saved_settings_file"))
    
    shinyjs::show("delete_settings_file_modal_%widget_id%")
})

# Close delete a settings file modal
observeEvent(input$close_file_deletion_modal_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$close_file_deletion_modal"))
    
    shinyjs::hide("delete_settings_file_modal_%widget_id%")
})

# Confirm settings file deletion
observeEvent(input$confirm_file_deletion_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$confirm_file_deletion"))
    
    tryCatch({
        file_id <- input$saved_settings_%widget_id%
        
        # Delete row in db
        sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE id = {file_id}", .con = m$db))
        
        # Update dropdown
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_dropdown_%widget_id%', Math.random());"))
        
        # Close modal
        shinyjs::hide("delete_settings_file_modal_%widget_id%")
        
        # Update selected settings file UI
        output$saved_settings_ui_%widget_id% <- renderUI(div(i18np$t("no_settings_file_selected"), style = paste0(saved_settings_ui_style, "background-color: #606060ab;")))
        
        # Hide delete button
        shinyjs::hide("delete_saved_settings_file_div_%widget_id%")
        
    }, error = function(e) cat(paste0("\n", now(), " - widget %widget_id% - error = ", toString(e))))
})
