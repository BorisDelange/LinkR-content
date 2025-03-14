# Settings / editor side-by-side with figure
observeEvent(input$figure_and_settings_side_by_side_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$figure_and_settings_side_by_side"))
    
    tryCatch({
        if (input$figure_and_settings_side_by_side_%widget_id%){
            shinyjs::runjs(paste0(
                "$('#", id, "-figure_div_%widget_id%').css('width', '50%');",
                "$('#", id, "-figure_settings_div_%widget_id%').css('width', '50%');",
                "$('#", id, "-code_div_%widget_id%').css('width', '50%');",
                "$('#", id, "-figure_div_%widget_id%').resizable({",
                "    handles: 'e',",
                "    resize: function(event, ui) {",
                "        var containerWidth = $('#", id, "-figure_settings_code_div_%widget_id%').width();",
                "        var leftWidth = (ui.size.width + 25) / containerWidth * 100;",
                "        $('#", id, "-figure_div_%widget_id%').css('flex-basis', leftWidth + '%');",
                "        $('#", id, "-figure_settings_div_%widget_id%').css('flex-basis', (100 - leftWidth) + '%');",
                "        $('#", id, "-code_div_%widget_id%').css('flex-basis', (100 - leftWidth) + '%');",
                "    },",
                "    stop: function(event, ui) {",
                "        var containerWidth = $('#", id, "-figure_settings_code_div_%widget_id%').width();",
                "        var leftWidth = (ui.size.width + 25) / containerWidth * 100;",
                "        $('#", id, "-figure_div_%widget_id%').css('flex-basis', leftWidth + '%');",
                "        $('#", id, "-code_div_%widget_id%').css('flex-basis', (100 - leftWidth) + '%');",
                "    }",
                "});"
            ))
            shinyjs::hide("figure_button_div_%widget_id%")
        }
        else {
            shinyjs::runjs(paste0(
                "$('#", id, "-figure_div_%widget_id%').resizable('destroy');",
                "$('#", id, "-figure_div_%widget_id%').css('width', '100%');",
                "$('#", id, "-figure_div_%widget_id%').css('flex-basis', '100%');",
                "$('#", id, "-figure_settings_div_%widget_id%').css('width', '100%');",
                "$('#", id, "-figure_settings_div_%widget_id%').css('flex-basis', '100%');",
                "$('#", id, "-code_div_%widget_id%').css('width', '100%');",
                "$('#", id, "-code_div_%widget_id%').css('flex-basis', '100%');"
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
        
        for (name in c("show_saved_file", "figure_and_settings_side_by_side", "run_code_at_patient_update", "run_code_at_settings_file_load")){
            toggle_value <- 0L
            input_name <- paste0(name, "_%widget_id%")
            if (length(input[[input_name]]) > 0) if (input[[input_name]]) toggle_value <- 1L
            new_data <- new_data %>% dplyr::bind_rows(tibble::tibble(name = name, value = NA_character_, value_num = toggle_value))
        }
        
        new_data <-
            new_data %>%
            dplyr::transmute(
                id = get_last_row(m$db, "widgets_options") + 1:5, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
                category = "general_settings", name, value, value_num, creator_id = m$user_id, datetime = now(), deleted = FALSE
            )
        
        DBI::dbAppendTable(m$db, "widgets_options", new_data)
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})
