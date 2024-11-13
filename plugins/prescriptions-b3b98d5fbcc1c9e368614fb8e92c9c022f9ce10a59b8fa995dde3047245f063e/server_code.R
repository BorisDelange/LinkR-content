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
        
        # Put here the code to execute when the "Run code" button is clicked
        
        if (d$data_person$drug_exposure %>% dplyr::count() %>% dplyr::pull() > 0){
                
                drug_exposure <-
                    d$data_person$drug_exposure %>%
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
                            !is.na(amount_value) ~ quantity * amount_value,
                            !is.na(numerator_value) ~ quantity * numerator_value
                        ),
                        amount_unit = dplyr::case_when(
                            !is.na(amount_value) ~ amount_unit_concept_name,
                            !is.na(numerator_value) ~ numerator_unit_concept_name
                        ),
                        duration_hours = as.numeric(difftime(drug_exposure_end_datetime, drug_exposure_start_datetime, units = "hours")),
                        rate = dplyr::case_when(
                            !is.na(numerator_value) & !is.na(duration_hours) & duration_hours > 0 ~ amount / duration_hours
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
                    
            drug_exposure <-
                drug_exposure %>%
                dplyr::mutate(
                    drug_concept_name = factor(drug_concept_name, levels = unique(drug_concept_name)),
                    drug_concept_name_short = ifelse(
                        nchar(as.character(drug_concept_name)) > 30,
                        paste0(substr(as.character(drug_concept_name), 1, 27), "..."),
                        as.character(drug_concept_name)
                    )
                )
            
            unique_levels <- levels(drug_exposure$drug_concept_name)
            unique_labels <- ifelse(nchar(unique_levels) > 30, paste0(substr(unique_levels, 1, 27), "..."), unique_levels)
            
            p_drug_exposure <- ggplot2::ggplot(drug_exposure) +
                ggplot2::geom_rect(
                    ggplot2::aes(
                        xmin = drug_exposure_start_datetime,
                        xmax = drug_exposure_end_datetime,
                        ymin = as.numeric(drug_concept_name) - 0.2,
                        ymax = as.numeric(drug_concept_name) + 0.2,
                        text = paste(
                            "Médicament :", drug_concept_name,
                            "<br>Début :", format(drug_exposure_start_datetime, "%Y-%m-%d %H:%M"),
                            "<br>Fin :", format(drug_exposure_end_datetime, "%Y-%m-%d %H:%M"),
                            "<br>Rate :", rate,
                            "<br>Rate Unit :", rate_unit
                        )
                    ),
                    fill = "coral"
                ) +
                ggplot2::scale_x_datetime(breaks = scales::breaks_pretty(n = 6)) +
                ggplot2::scale_y_continuous(
                    breaks = seq_along(unique_levels),
                    labels = unique_labels,
                    name = ""
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    axis.text.y = ggplot2::element_text(size = 8),
                    axis.text.x = ggplot2::element_text(size = 8)
                )
            
            plotly_drug_exposure <- plotly::ggplotly(p_drug_exposure, tooltip = "text") %>%
                plotly::config(displayModeBar = FALSE)
            
            output$drug_exposure_plot_%widget_id% <- plotly::renderPlotly(plotly_drug_exposure)
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
