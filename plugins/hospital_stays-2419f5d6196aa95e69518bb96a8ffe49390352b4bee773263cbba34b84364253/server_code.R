# Server - Code

# Init code var
m$code_%widget_id% <- ""

# Prevent a bug with scroll into ace editor
shinyjs::delay(300, shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);"))

# Comment code
observeEvent(input$code_%widget_id%_comment, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$code_comment"))

    tryCatch({
        lines <- strsplit(input$code_%widget_id%, "\\n")[[1]]
        req(length(lines) > 0)
        
        start_row <- input$code_%widget_id%_comment$range$start$row + 1
        end_row <- input$code_%widget_id%_comment$range$end$row + 1
        
        for (i in start_row:end_row) if (startsWith(lines[i], "# ")) lines[i] <- substr(lines[i], 3, nchar(lines[i])) else lines[i] <- paste0("# ", lines[i])
        
        shinyAce::updateAceEditor(session, "code_%widget_id%", value = paste0(lines, collapse = "\\n"))
        
        shinyjs::runjs(sprintf("
            var editor = ace.edit('%s-rode');
            editor.moveCursorTo(%d, %d);
            editor.focus();
        ", id, input$code_%widget_id%_comment$range$end$row, input$code_%widget_id%_comment$range$end$column))
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# Run all code with shortcut
observeEvent(input$code_%widget_id%_run_all, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$code_run_all"))

    m$code_%widget_id% <- input$code_%widget_id%
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Run code when button is clicked
observeEvent(input$display_figure_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$display_figure"))
    
    m$code_%widget_id% <- input$code_%widget_id%
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Run code
observeEvent(input$run_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_code"))
    
    tryCatch({
        
        isolate_code <- TRUE
        if (length(input$run_code_on_data_update_%widget_id%) > 0) if (input$run_code_on_data_update_%widget_id%) isolate_code <- FALSE
        
        if (language == "fr") date_labels <- "%Y-%m-%d"
        else date_labels <- "%d-%m-%Y"
        
        output$stays_plot_%widget_id% <- renderPlot({
            
            if (isolate_code) data <- isolate(d$data_person$visit_detail)
            else data <- d$data_person$visit_detail
            
            if (data %>% dplyr::count() %>% dplyr::pull() > 0){
                data <-
                    d$data_person$visit_detail %>%
                    dplyr::select(visit_detail_start_datetime, visit_detail_end_datetime, care_site_id) %>%
                    dplyr::left_join(
                        d$care_site %>% dplyr::select(care_site_id, care_site_name),
                        by = "care_site_id"
                    ) %>%
                    dplyr::collect() %>%
                    dplyr::arrange(visit_detail_start_datetime) %>%
                    dplyr::select(care_site_name, start = visit_detail_start_datetime, end = visit_detail_end_datetime) %>%
                    dplyr::filter(!is.na(care_site_name))  # Supprime les lignes où le care_site_name est NA
                
                ggplot2::ggplot(data) +
                    ggplot2::geom_segment(
                        ggplot2::aes(
                            x = start,
                            xend = end,
                            y = forcats::fct_rev(forcats::fct_inorder(care_site_name)),
                            yend = forcats::fct_rev(forcats::fct_inorder(care_site_name))
                        ),
                        size = 5,
                        color = "steelblue"
                    ) +
                    ggplot2::scale_x_datetime(date_labels = "%Y-%m-%d") +
                    ggplot2::labs(
                        title = "",
                        x = "Date",
                        y = "Service Hospitalier"
                    ) +
                    ggplot2::theme_minimal()
            }
        })

        # Go to figure tab
        if (!input$figure_and_settings_side_by_side_%widget_id%) shinyjs::click("figure_button_%widget_id%")
        
    }, error = function(e){
        show_message_bar(output, "error_displaying_figure", "severeWarning", i18n = i18np, ns = ns)
        cat(paste0("\\n", now(), " - widget %widget_id% - input$display_figure - error = ", toString(e)))
    })
})

# Save code with shortcut
observeEvent(input$code_%widget_id%_save, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$code_save"))
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_params_and_code_%widget_id%', Math.random());"))
})
