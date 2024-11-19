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

# Run code at patient update
observeEvent(m$selected_person, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$selected_person"))
    
    if (isTRUE(input$run_code_on_data_update_%widget_id%) && length(input$data_source_%widget_id%) > 0 && input$data_source_%widget_id% == "person") shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Run code at visit_detail update
observeEvent(m$selected_visit_detail, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$selected_visit_detail"))
    
    if (isTRUE(input$run_code_on_data_update_%widget_id%) && length(input$data_source_%widget_id%) > 0 && input$data_source_%widget_id% == "visit_detail") shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Run code
observeEvent(input$run_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_code"))
    
    tryCatch({
        
        data_source <- "visit_detail"
        if (length(input$data_source_%widget_id%) > 0) data_source <- input$data_source_%widget_id%
        
        if ((data_source == "person" & is.na(m$selected_person)) | (data_source == "visit_detail" & is.na(m$selected_visit_detail))){
            
            if (data_source == "person" & is.na(m$selected_person)) output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("select_patient"), messageBarType = 5), style = "display: inline-block;"))
            else if (data_source == "visit_detail" & is.na(m$selected_visit_detail)) output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("select_stay"), messageBarType = 5), style = "display: inline-block;"))
            
            shinyjs::hide("datatable_%widget_id%")
            shinyjs::show("error_message_div_%widget_id%")
            
        } else {
        
            data <- d[[paste0("data_", data_source)]]$measurement
            
            if (length(input$concepts_choice_%widget_id%) > 0){
                if (input$concepts_choice_%widget_id% == "selected_concept_classes"){
                    if (data %>% dplyr::count() %>% dplyr::pull() > 0) data <- 
                        data %>%
                        dplyr::collect() %>%
                        dplyr::inner_join(d$concept %>% dplyr::select(measurement_concept_id = concept_id, concept_class_id), by = "measurement_concept_id") %>%
                        dplyr::filter(concept_class_id %in% input$concept_classes_%widget_id%) %>%
                        dplyr::select(-concept_class_id)
                }
                else if (input$concepts_choice_%widget_id% == "selected_concepts"){
                    if (data %>% dplyr::count() %>% dplyr::pull() > 0) data <- data %>% dplyr::filter(measurement_concept_id %in% input$concepts_%widget_id%)
                }
            }
            
            if (data %>% dplyr::count() %>% dplyr::pull() == 0){
                
                output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("no_data_to_display"), messageBarType = 5), style = "display: inline-block;"))
                
                shinyjs::hide("datatable_%widget_id%")
                shinyjs::show("error_message_div_%widget_id%")
            } else {
                
                shinyjs::hide("error_message_div_%widget_id%")
                shinyjs::show("datatable_%widget_id%")
                
                if (length(input$num_cols_%widget_id%) > 0) num_cols <- input$num_cols_%widget_id%
                else num_cols <- 8
                
                if (length(input$aggregate_fct_%widget_id%) > 0) aggregate_fct <- input$aggregate_fct_%widget_id%
                else aggregate_fct <- 8
                
                data <-
                    data %>%
                    dplyr::collect() %>%
                    dplyr::left_join(
                        d$concept %>% 
                        dplyr::select(measurement_concept_id = concept_id, measurement_concept_name = concept_name),
                        by = "measurement_concept_id"
                    ) %>%
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
                            format(interval_start, "%Y-%m-%d"),
                            " <span style='color:#0084D8'>", format(interval_start, "%H:%M"), "</span>",
                            " ", tolower(i18np$t("to")), " ",
                            format(interval_end, "%Y-%m-%d"),
                            " <span style='color:#0084D8'>", format(interval_end, "%H:%M"), "</span>"
                        )
                    )
                
                data <-
                    data %>%
                    dplyr::mutate(
                        interval = floor(as.numeric(difftime(measurement_datetime, time_range[1], units = "secs")) / interval_duration)
                    ) %>%
                    dplyr::group_by(measurement_concept_name, interval) %>%
                    dplyr::summarize(
                        agg_value = round(do.call(aggregate_fct, list(value_as_number, na.rm = TRUE)), 1),
                        n = sum(!is.na(value_as_number)),
                        .groups = "drop"
                    ) %>%
                    dplyr::mutate(agg_value = paste0(agg_value, " (n = ", n, ")")) %>%
                    dplyr::right_join(intervals, by = "interval") %>%
                    dplyr::select(measurement_concept_name, interval_label, agg_value) %>%
                    tidyr::pivot_wider(
                        names_from = interval_label,
                        values_from = agg_value
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
                            hover = TRUE
                        ),
                        escape = FALSE,
                        
                        # CSS for datatable
                        callback = htmlwidgets::JS(
                          "table.on('draw.dt', function() {",
                          "  $('.dataTable tbody tr td').css({",
                          "    'height': '12px',",
                          "    'padding': '2px 5px'",
                          "  });",
                          "  $('.dataTable thead tr td div .form-control').css('font-size', '12px');",
                          "  $('.dataTable thead tr td').css('padding', '5px');",
                          "  $('.dataTable tbody tr td:first-child').css({",
                          "    'min-width': '100px',",
                          "    'max-width': '100px',",
                          "    'width': '100px',",
                          "    'white-space': 'nowrap'",
                          "  });",
                          "  $('.dataTable thead tr th:first-child').css({",
                          "    'min-width': '100px',",
                          "    'max-width': '100px',",
                          "    'width': '100px',",
                          "    'white-space': 'nowrap'",
                          "  });",
                          "});"
                        )
                    )
                )
            }
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
