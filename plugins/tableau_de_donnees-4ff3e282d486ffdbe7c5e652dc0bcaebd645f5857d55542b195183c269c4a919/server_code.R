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
    
    if (is.na(m$selected_person)){
        shinyjs::show("figure_message_%widget_id%")
        shinyjs::hide("dygraph_%widget_id%")
        output$figure_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("select_patient"), messageBarType = 5), style = "display: inline-block;"))
    }
    
    req(!is.na(m$selected_person))
    
    tryCatch({
        
        if (length(input$features_%widget_id%) > 0){
            
            num_cols <- 8

            concept_ids <- input$features_%widget_id%
            
            data <-
                d$data_person$measurement %>%
                dplyr::collect() %>%
                dplyr::left_join(
                    d$concept %>% 
                    dplyr::select(measurement_concept_id = concept_id, measurement_concept_name = concept_name),
                    by = "measurement_concept_id"
                ) %>%
                dplyr::filter(measurement_concept_id %in% concept_ids) %>%
                dplyr::select(measurement_concept_name, measurement_datetime, value_as_number) %>%
                dplyr::arrange(measurement_concept_name, measurement_datetime)
            
            time_range <- range(data$measurement_datetime)
            interval_duration <- as.numeric(difftime(time_range[2], time_range[1], units = "secs")) / num_cols
            
            intervals <- tibble::tibble(
                interval = 0:(num_cols - 1),
                interval_start = time_range[1] + interval * interval_duration,
                interval_end = interval_start + interval_duration
            ) %>%
                dplyr::mutate(
                    interval_label = paste(
                        format(interval_start, "%Y-%m-%d %H:%M"),
                        "\\nau\\n",
                        format(interval_end, "%Y-%m-%d %H:%M")
                    )
                )
            
            data <-
                data %>%
                dplyr::mutate(
                    interval = floor(as.numeric(difftime(measurement_datetime, time_range[1], units = "secs")) / interval_duration)
                ) %>%
                dplyr::group_by(measurement_concept_name, interval) %>%
                dplyr::summarize(
                    avg_value = round(mean(value_as_number, na.rm = TRUE), 1),
                    n = sum(!is.na(value_as_number)),
                    .groups = "drop"
                ) %>%
                dplyr::mutate(avg_value = paste0(avg_value, " (n = ", n, ")")) %>%
                dplyr::right_join(intervals, by = "interval") %>%
                dplyr::select(measurement_concept_name, interval_label, avg_value) %>%
                tidyr::pivot_wider(
                    names_from = interval_label,
                    values_from = avg_value
                ) %>%
                dplyr::arrange(measurement_concept_name) %>%
                dplyr::rename(!!i18np$t("concept") := measurement_concept_name)
            
            output$datatable_%widget_id% <- DT::renderDT(
                DT::datatable(
                    data,
                    rownames = FALSE,
                    options = list(
                        dom = "<'top't><'bottom'p>",
                        compact = TRUE, 
                        hover = TRUE,
                        columnDefs = list(list(width = '200px', targets = 0))
                    ),
                    
                    # CSS for datatable
                    callback = htmlwidgets::JS(
                      "table.on('draw.dt', function() {",
                      "  $('.dataTable tbody tr td').css({",
                      "    'height': '12px',",
                      "    'padding': '2px 5px'",
                      "  });",
                      "  $('.dataTable thead tr td div .form-control').css('font-size', '12px');",
                      "  $('.dataTable thead tr td').css('padding', '5px');",
                      "});"
                    )
                )
            )
        }
        
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
