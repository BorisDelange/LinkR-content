output$saved_settings_%widget_id% <- renderUI({
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer output$saved_settings"))
    
    i18np$t("parameters_not_saved")
})

# Show / hide saved settings file
observeEvent(input$show_saved_file_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$show_saved_file"))
    
    if (input$show_saved_file_%widget_id%) shinyjs::show("saved_settings_%widget_id%")
    else shinyjs::hide("saved_settings_%widget_id%")
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
            
            # Check if name is already used
        }
    }, error = function(e) cat(paste0("\n", now(), " - widget %widget_id% - error = ", toString(e))))
})
