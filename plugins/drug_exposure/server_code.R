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
        toggle_comments(
            id = id, input_id = "code_%widget_id%", code = input$code_%widget_id%,
            selection = input$code_%widget_id%_comment$range, session = session
        )
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# Run all code with shortcut
observeEvent(input$code_%widget_id%_run_all, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$code_run_all"))

    if ("projects_widgets_console" %in% user_accesses){
        m$code_%widget_id% <- input$code_%widget_id%
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

# Run code when button is clicked
observeEvent(input$display_figure_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$display_figure"))
    
    tryCatch({
    
        # If current selected tab is figure settings when run code button is clicked, generate code from these settings
        if (length(input$current_tab_%widget_id%) == 0) current_tab <- "figure_settings"
        else current_tab <- input$current_tab_%widget_id%
        
        if (current_tab == "figure_settings"){
            
            # Code to generate code from figure settings
            
            # ...
            code <- ""
            
            # Update ace editor with generated code
            shinyAce::updateAceEditor(session, "code_%widget_id%", value = code)
            
            m$code_%widget_id% <- code
            
            shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
        }
        
        # Check if user has access
        else if ("projects_widgets_console" %in% user_accesses){
            m$code_%widget_id% <- input$code_%widget_id%
            shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
        }
        
    }, error = function(e){
        show_message_bar(id, output, "error_displaying_figure", "severeWarning", i18n = i18np, ns = ns)
        cat(paste0("\\n", now(), " - widget %widget_id% - input$display_figure - error = ", toString(e)))
    })
})

# Run code at patient update
observeEvent(m$selected_person, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$selected_person"))
    
    if (isTRUE(input$run_code_on_data_update_%widget_id%) && length(input$data_source_%widget_id%) > 0 && input$data_source_%widget_id% == "person"){
        
        tryCatch({
        
            # Reset synchronized datetimes
            m$debounced_datetimes_timeline_%tab_id% <- reactiveVal()
            m$datetimes_timeline_%tab_id% <- reactiveVal()
            m$debounced_datetimes_timeline_%tab_id% <- reactiveVal()
            m$debounced_datetimes_timeline_%tab_id% <- reactive(m$datetimes_timeline_%tab_id%()) %>% debounce(500)
            
            # Run code
            shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
            
         }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
    }
})

# Run code at visit_detail update
observeEvent(m$selected_visit_detail, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$selected_visit_detail"))
    
    if (isTRUE(input$run_code_on_data_update_%widget_id%) && length(input$data_source_%widget_id%) > 0 && input$data_source_%widget_id% == "visit_detail"){
        
        tryCatch({
        
            # Reset synchronized datetimes
            m$debounced_datetimes_timeline_%tab_id% <- reactiveVal()
            m$datetimes_timeline_%tab_id% <- reactiveVal()
            m$debounced_datetimes_timeline_%tab_id% <- reactiveVal()
            m$debounced_datetimes_timeline_%tab_id% <- reactive(m$datetimes_timeline_%tab_id%()) %>% debounce(500)
            
            # Run code
            shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
            
         }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
    }
})

# Run code
observeEvent(input$run_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_code"))
    
    tryCatch({
        
        data_source <- "visit_detail"
        if (length(input$data_source_%widget_id%) > 0) data_source <- input$data_source_%widget_id%
        
        if ((data_source == "person" & is.na(m$selected_person)) | (data_source == "visit_detail" & is.na(m$selected_visit_detail))){
            
            if (data_source == "person" & is.na(m$selected_person)) output$error_message_%widget_id% <- renderUI(
                div(shiny.fluent::MessageBar(i18np$t("select_patient"), messageBarType = 5), style = "display: inline-block;")
            )
            else if (data_source == "visit_detail" & is.na(m$selected_visit_detail)) output$error_message_%widget_id% <- renderUI(
                div(shiny.fluent::MessageBar(i18np$t("select_stay"), messageBarType = 5), style = "display: inline-block;")
            )
            
            shinyjs::hide("drug_exposure_plot_%widget_id%")
            shinyjs::show("error_message_div_%widget_id%")
            
        } else {
            
            # data <- d[[paste0("data_", data_source)]]$drug_exposure %>% dplyr::collect()
            if (data_source == "person") sql <- glue::glue_sql("SELECT * FROM drug_exposure WHERE person_id = {m$selected_person}", .con = d$con)
            else if (data_source == "visit_detail") sql <- glue::glue_sql("SELECT * FROM drug_exposure WHERE person_id = {m$selected_visit_detail}", .con = d$con)
            data <- DBI::dbGetQuery(d$con, sql)
            
            if (length(input$concepts_choice_%widget_id%) > 0){
                if (input$concepts_choice_%widget_id% == "selected_concept_classes"){
                    if (nrow(data) > 0) data <- 
                        data %>%
                        dplyr::inner_join(d$dataset_concept %>% dplyr::select(drug_concept_id = concept_id, concept_class_id), by = "drug_concept_id") %>%
                        dplyr::filter(concept_class_id %in% input$concept_classes_%widget_id%) %>%
                        dplyr::select(-concept_class_id)
                }
                else if (input$concepts_choice_%widget_id% == "selected_concepts"){
                    if (nrow(data) > 0) data <- data %>% dplyr::filter(drug_concept_id %in% input$concepts_%widget_id%)
                }
            }
            
            if (nrow(data) == 0){
                
                output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("no_data_to_display"), messageBarType = 5), style = "display: inline-block;"))
                
                shinyjs::hide("drug_exposure_plot_%widget_id%")
                shinyjs::show("error_message_div_%widget_id%")
            } else {
                
                shinyjs::hide("error_message_div_%widget_id%")
                shinyjs::show("drug_exposure_plot_%widget_id%")
                
                data <-
                    data %>%
                    join_concepts(d$dataset_concept, c("drug", "drug_type", "route")) %>%
                    # Replace NA drug_concept_name by their concept_id
                    dplyr::mutate(drug_concept_name = dplyr::if_else(is.na(drug_concept_name), as.character(drug_concept_id), drug_concept_name)) %>%
                    dplyr::left_join(
                        d$dataset_drug_strength %>%
                            join_concepts(d$dataset_concept, c("ingredient", "amount_unit", "numerator_unit", "denominator_unit")) %>%
                            dplyr::select(
                                drug_concept_id, ingredient_concept_id, ingredient_concept_name,
                                amount_value, amount_unit_concept_id, amount_unit_concept_name,
                                numerator_value, numerator_unit_concept_id, numerator_unit_concept_name,
                                denominator_value, denominator_unit_concept_id, denominator_unit_concept_name
                            ),
                        by = "drug_concept_id",
                        copy = TRUE
                    ) %>%
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
                    
                data <- data %>% dplyr::mutate(drug_concept_name = factor(drug_concept_name, levels = unique(drug_concept_name)))
                
                unique_levels <- levels(data$drug_concept_name)
                unique_labels <- ifelse(
                    nchar(unique_levels) > 22,
                    paste0(substr(unique_levels, 1, 17), "..."),
                    unique_levels
                )
                
                if (language == "fr") datetime_format <- "%d-%m-%Y %H:%M"
                else datetime_format <- "%Y-%m-%d %H:%M"
                
                if (data_source == "person") {
                    sql <- glue::glue_sql("
                        SELECT MIN(visit_start_datetime) AS min_visit_start_datetime, MAX(visit_end_datetime) AS max_visit_end_datetime
                        FROM visit_occurrence
                        WHERE person_id = {m$selected_person}
                    ", .con = d$con)
                    data_datetimes_range <- DBI::dbGetQuery(d$con, sql)
                }
                else if (data_source == "visit_detail") {
                    sql <- glue::glue_sql("
                        SELECT MIN(visit_start_datetime) AS min_visit_start_datetime, MAX(visit_end_datetime) AS max_visit_end_datetime
                        FROM visit_detail
                        WHERE visit_detail_id = {m$selected_visit_detail}
                    ", .con = d$con)
                    data_datetimes_range <- DBI::dbGetQuery(d$con, sql)
                }
                
                data_datetimes_range <- c(data_datetimes_range$min_visit_start_datetime, data_datetimes_range$max_visit_end_datetime)
                m$data_datetimes_range_%widget_id% <- data_datetimes_range
                
                if (isTRUE(input$synchronize_timelines_%widget_id%) && length(m$debounced_datetimes_timeline_%tab_id%()) > 0) datetimes <- m$debounced_datetimes_timeline_%tab_id%()
                else datetimes <- data_datetimes_range
                
                m$datetimes_%widget_id% <- datetimes
                
                plotly_drug_exposure <-
                    plotly::plot_ly(data = data, source = "drug_exposure_plot_%widget_id%") %>%
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
                            tickfont = list(size = 10),
                            tickformat = datetime_format,
                            range = c(
                                format(datetimes[[1]], "%Y-%m-%d %H:%M:%S"),
                                format(datetimes[[2]], "%Y-%m-%d %H:%M:%S")
                            )
                        ),
                        yaxis = list(
                            tickvals = seq_along(unique_levels),
                            ticktext = unique_labels,
                            title = "",
                            tickfont = list(family = "Courier New", size = 11),
                            automargin = FALSE
                        ),
                        hoverlabel = list(align = "left"),
                        margin = list(l = 145, r = 0, t = 0, b = 0)
                    ) %>%
                    plotly::config(displayModeBar = FALSE) %>%
                    plotly::event_register("plotly_relayout")
                
                output$drug_exposure_plot_%widget_id% <- plotly::renderPlotly(plotly_drug_exposure)
            }
        }
        
        # Go to figure tab
        if (!input$figure_and_settings_side_by_side_%widget_id%) shinyjs::click("figure_button_%widget_id%")
        
    }, error = function(e){
        show_message_bar(id, output, "error_displaying_figure", "severeWarning", i18n = i18np, ns = ns)
        cat(paste0("\\n", now(), " - widget %widget_id% - input$display_figure - error = ", toString(e)))
    })
})

# Save code with shortcut
observeEvent(input$code_%widget_id%_save, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$code_save"))
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_params_and_code_%widget_id%', Math.random());"))
})
