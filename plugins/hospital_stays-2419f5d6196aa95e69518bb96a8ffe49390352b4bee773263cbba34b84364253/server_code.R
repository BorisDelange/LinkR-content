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
    
    if (isTRUE(input$run_code_on_data_update_%widget_id%)) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Run code
observeEvent(input$run_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_code"))
    
    tryCatch({
        
        if (is.na(m$selected_person)){
            output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("select_patient"), messageBarType = 5), style = "display: inline-block;"))
            shinyjs::hide("stays_plot_%widget_id%")
            shinyjs::show("error_message_div_%widget_id%")
        }
        else {

            data <- d$data_person$visit_detail
            
            if (data %>% dplyr::count() %>% dplyr::pull() == 0){
                    
                output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("no_data_to_display"), messageBarType = 5), style = "display: inline-block;"))
                
                shinyjs::hide("stays_plot_%widget_id%")
                shinyjs::show("error_message_div_%widget_id%")
            } else {
                
                shinyjs::hide("error_message_div_%widget_id%")
                shinyjs::show("stays_plot_%widget_id%")
                
                data <-
                    d$data_person$visit_detail %>%
                    dplyr::select(visit_detail_start_datetime, visit_detail_end_datetime, care_site_id) %>%
                    dplyr::left_join(
                        d$care_site %>% dplyr::select(care_site_id, care_site_name),
                        by = "care_site_id"
                    ) %>%
                    dplyr::collect() %>%
                    dplyr::arrange(visit_detail_start_datetime) %>%
                    dplyr::filter(!is.na(care_site_name)) %>%
                    dplyr::mutate(
                        service_order = as.numeric(forcats::fct_rev(forcats::fct_inorder(care_site_name)))
                    )
                
                unique_levels <- data$service_order
                unique_labels <- data$care_site_name
                
                if (language == "fr") datetime_format <- "%d-%m-%Y %H:%M"
                else datetime_format <- "%Y-%m-%d %H:%M"
                
                plotly_visit_detail <- plotly::plot_ly(data = data, source = "visit_detail_plot_%widget_id%") %>%
                    plotly::add_segments(
                        x = ~visit_detail_start_datetime,
                        xend = ~visit_detail_end_datetime,
                        y = ~service_order,
                        yend = ~service_order,
                        line = list(color = "steelblue", width = 10),
                        text = ~paste0(
                            i18np$t("hospital_unit"), " : ", care_site_name, "<br>",
                            i18np$t("datetime_start"), " : ", format(visit_detail_start_datetime, datetime_format), "<br>",
                            i18np$t("datetime_end"), " : ", format(visit_detail_end_datetime, datetime_format)
                        ),
                        hoverinfo = "text"
                    ) %>%
                    plotly::layout(
                        xaxis = list(
                            type = "date",
                            tickmode = "auto",
                            title = "",
                            nticks = 10,
                            tickfont = list(size = 10),
                            tickformat = datetime_format
                        ),
                        yaxis = list(
                            tickvals = unique_levels,
                            ticktext = unique_labels,
                            title = "",
                            automargin = TRUE
                        ),
                        hoverlabel = list(align = "left")
                    ) %>%
                    plotly::config(displayModeBar = FALSE)
            
                output$stays_plot_%widget_id% <- plotly::renderPlotly(plotly_visit_detail)
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
