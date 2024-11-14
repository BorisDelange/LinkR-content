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
        
        if (is.na(m$selected_person) | is.na(m$selected_person)){
            
            if (is.na(m$selected_person)) output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("select_patient"), messageBarType = 5), style = "display: inline-block;"))
            else if (is.na(m$selected_visit_detail)) output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("select_stay"), messageBarType = 5), style = "display: inline-block;"))
            
            shinyjs::hide("drug_exposure_plot_%widget_id%")
            shinyjs::show("error_message_div_%widget_id%")
        } else {
            data_source <- "visit_detail"
            if (length(input$data_source_%widget_id%) > 0) data_source <- input$data_source_%widget_id%
            
            data <- d[[paste0("data_", data_source)]]$drug_exposure
            
            if (data %>% dplyr::count() %>% dplyr::pull() == 0){
                
                output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("no_data_to_display"), messageBarType = 5), style = "display: inline-block;"))
                
                shinyjs::hide("drug_exposure_plot_%widget_id%")
                shinyjs::show("error_message_div_%widget_id%")
            } else {
                
                shinyjs::hide("error_message_div_%widget_id%")
                shinyjs::show("drug_exposure_plot_%widget_id%")
                
                data <-
                    data %>%
                    join_concepts(d$concept, c("drug", "drug_type", "route")) %>%
                    dplyr::left_join(
                        d$drug_strength %>%
                            join_concepts(d$concept, c("ingredient", "amount_unit", "numerator_unit", "denominator_unit")) %>%
                            dplyr::select(
                                drug_concept_id, ingredient_concept_id, ingredient_concept_name,
                                amount_value, amount_unit_concept_id, amount_unit_concept_name,
                                numerator_value, numerator_unit_concept_id, numerator_unit_concept_name,
                                denominator_value, denominator_unit_concept_id, denominator_unit_concept_name
                            ),
                        by = "drug_concept_id",
                        copy = TRUE
                    ) %>%
                    dplyr::collect() %>%
                    dplyr::arrange(person_id, drug_exposure_start_datetime) %>%
                    dplyr::mutate(
                        amount = dplyr::case_when(
                            !is.na(amount_value) ~ round(quantity * amount_value, 1),
                            !is.na(numerator_value) ~ round(quantity * numerator_value, 1)
                        ),
                        amount_unit = dplyr::case_when(
                            !is.na(amount_value) ~ amount_unit_concept_name,
                            !is.na(numerator_value) ~ numerator_unit_concept_name
                        ),
                        duration_hours = as.numeric(difftime(drug_exposure_end_datetime, drug_exposure_start_datetime, units = "hours")),
                        rate = dplyr::case_when(
                            !is.na(numerator_value) & !is.na(duration_hours) & duration_hours > 0 ~ round(amount / duration_hours, 1)
                        ),
                        rate_unit = dplyr::case_when(
                            !is.na(rate) & !is.na(amount_unit) ~ paste0(amount_unit, " per hour")
                        ),
                        daily_dose = dplyr::case_when(
                            is.na(rate) & !is.na(amount) ~ amount / duration_hours * 24
                        ),
                        daily_dose_unit = dplyr::case_when(
                            is.na(rate) & !is.na(amount_unit) ~ paste0(amount_unit, " per day")
                        )
                    ) %>%
                    dplyr::select(
                        person_id, drug_concept_name,
                        drug_exposure_start_datetime, drug_exposure_end_datetime, duration_hours,
                        amount, amount_unit, rate, rate_unit, daily_dose, daily_dose_unit
                    )
                    
                data <-
                    data %>%
                    dplyr::mutate(
                        drug_concept_name = factor(drug_concept_name, levels = unique(drug_concept_name)),
                        drug_concept_name_short = ifelse(
                            nchar(as.character(drug_concept_name)) > 30,
                            paste0(substr(as.character(drug_concept_name), 1, 27), "..."),
                            as.character(drug_concept_name)
                        )
                    )
                
                unique_levels <- levels(data$drug_concept_name)
                unique_labels <- ifelse(nchar(unique_levels) > 30, paste0(substr(unique_levels, 1, 27), "..."), unique_levels)
                
                if (language == "fr") datetime_format <- "%d-%m-%Y %H:%M"
                else datetime_format <- "%Y-%m-%d %H:%M"
                
                plotly_drug_exposure <- plotly::plot_ly(data = data) %>%
                    plotly::add_segments(
                        x = ~drug_exposure_start_datetime,
                        xend = ~drug_exposure_end_datetime,
                        y = ~as.numeric(drug_concept_name),
                        yend = ~as.numeric(drug_concept_name),
                        line = list(color = "coral", width = 5),
                        text = ~paste0(
                            i18np$t("drug"), " : ", drug_concept_name, "<br>",
                            i18np$t("start"), " : ", format(drug_exposure_start_datetime, datetime_format), "<br>",
                            i18np$t("end"), " : ", format(drug_exposure_end_datetime, datetime_format), "<br>",
                            i18np$t("amount"), " : ", ifelse(is.na(amount), "/", amount), " ", ifelse(is.na(amount_unit), "", amount_unit), "<br>",
                            i18np$t("rate"), " : ", ifelse(is.na(rate), "/", rate), " ", ifelse(is.na(rate_unit), "", rate_unit)
                        ),
                        hoverinfo = "text"
                    ) %>%
                    plotly::layout(
                        xaxis = list(
                            type = "date",
                            tickmode = "auto",
                            title = "",
                            nticks = 10,
                            tickformat = datetime_format
                        ),
                        yaxis = list(
                            tickvals = seq_along(unique_levels),
                            ticktext = unique_labels,
                            title = ""
                        ),
                        hoverlabel = list(align = "left")
                    ) %>%
                    plotly::config(displayModeBar = FALSE)
                
                output$drug_exposure_plot_%widget_id% <- plotly::renderPlotly(plotly_drug_exposure)
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
