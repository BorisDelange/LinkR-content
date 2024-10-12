# Server - Figure settings

# Load saved settings

observeEvent(input$load_figure_settings_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$load_figure_settings"))
    
    # Update figure settings UI
    
    link_id <- input$settings_file_%widget_id%
    sql <- glue::glue_sql("SELECT name, value FROM widgets_options WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", .con = m$db)
    figure_settings <- DBI::dbGetQuery(m$db, sql)
    
    if (nrow(figure_settings) > 0){
        sapply(figure_settings$name, function(name){
        
            value <- figure_settings %>% dplyr::filter(name == !!name) %>% dplyr::pull(value)
            
            if (name == "prog_language") shiny.fluent::updateDropdown.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            else if (name == "output"){
                prog_language <- figure_settings %>% dplyr::filter(name == "prog_language") %>% dplyr::pull(value)
                shinyjs::delay(100, shiny.fluent::updateDropdown.shinyInput(session, paste0(name, "_%widget_id%"), options = output_dropdown_options[[prog_language]], value = value))
            }
            else if (name == "code") shinyAce::updateAceEditor(session, "code_%widget_id%", value = value)
        })
    }
    
    # Run code if toggle is activated
    if (length(input$run_code_at_settings_file_load_%widget_id%) > 0) if (input$run_code_at_settings_file_load_%widget_id%) shinyjs::delay(500, 
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-display_figure_%widget_id%', Math.random());")))
})

# Save current settings

observeEvent(input$save_params_and_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$save_params_and_code"))
    
    tryCatch({
    
        # If no saved settings file is selected, go to settings files management page
        if (length(input$saved_settings_%widget_id%) == 0) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_saved_settings_tab_%widget_id%', Math.random());"))
        
        if (length(input$saved_settings_%widget_id%) > 0){
            
            link_id <- input$saved_settings_%widget_id%
        
            # Delete old settings
            sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND link_id = {link_id}", .con = m$db))
            
            # Add new settings in db
            new_data <- tibble::tribble(
                ~name, ~value, ~value_num,
                "features", input$input$features_%widget_id% %>% toString(), NA_real_
            ) %>%
            dplyr::transmute(
                id = get_last_row(m$db, "widgets_options") + 1:1, widget_id = %widget_id%, person_id = NA_integer_, link_id = link_id,
                category = "saved_settings", name, value, value_num, creator_id = m$user_id, datetime = now(), deleted = FALSE
            )
            
            DBI::dbAppendTable(m$db, "widgets_options", new_data)
            
            # Notify user
            show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
        }
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})
