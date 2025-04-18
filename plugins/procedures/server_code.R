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
            
            data_source <- "visit_detail"
            if (length(input$data_source_%widget_id%) > 0) data_source <- input$data_source_%widget_id%
            
            code <- paste0(
                "concepts <- tibble::tribble(\\n",
                "    ~concept_id, ~concept_name, ~domain_id, ~vocabulary_id"
            )
            
            concepts <- selected_concepts %>% dplyr::filter(concept_id %in% input$concepts_%widget_id%)
            
            if (nrow(concepts) > 0){
                for (i in 1:nrow(concepts)){
                        row <- concepts[i, ]
                        code <- paste0(
                            code,
                            ",\\n",
                            "    ", row$concept_id, ", '", row$concept_name, "', '", row$domain_id, "', '", row$vocabulary_id, "'"
                        )
                    }
            }
            
            code <- paste0(code, "\\n", ")")
            
            # ...
            
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
        
        fig <- character()
        
        # To test code without plugin aceEditor, comment Part 1 and uncomment Part 2
        
        ## Part 1 - run code from aceEditor
        
        # eval(parse(text = m$code_%widget_id%))
        
        ## Part 2 - run code directly without aceEditor (dev mode)
        
        data_source <- "visit_detail"
        if (length(input$data_source_%widget_id%) > 0) data_source <- input$data_source_%widget_id%
        
        concepts <- selected_concepts %>% dplyr::filter(concept_id %in% input$concepts_%widget_id%)
        
        if (m$omop_version == "5.3") sql_cols <- DBI::SQL("
            procedure_occurrence_id,
            person_id,
            visit_detail_id,
            procedure_concept_id,
            procedure_datetime,
            procedure_type_concept_id,
            quantity
        ") else sql_cols <- DBI::SQL("
            procedure_occurrence_id,
            person_id,
            visit_detail_id,
            procedure_concept_id,
            procedure_datetime,
            procedure_end_datetime,
            procedure_type_concept_id,
            quantity
        ")
        
        if (data_source == "person") sql <- glue::glue_sql("
            SELECT {`sql_cols`}
            FROM procedure_occurrence
            WHERE person_id = {m$selected_person}
        ", .con = d$con)
        else if (data_source == "visit_detail") sql <- glue::glue_sql("
            SELECT {`sql_cols`}
            FROM procedure_occurrence
            WHERE visit_detail_id = {m$selected_visit_detail}
        ", .con = d$con)
        
        data <- DBI::dbGetQuery(d$con, sql)
        
        if (length(input$concepts_choice_%widget_id%) > 0){
            if (input$concepts_choice_%widget_id% == "selected_concept_classes"){
                if (nrow(data) > 0) data <- 
                    data %>%
                    dplyr::inner_join(d$dataset_concept %>% dplyr::select(procedure_concept_id = concept_id, concept_class_id), by = "procedure_concept_id") %>%
                    dplyr::filter(concept_class_id %in% input$concept_classes_%widget_id%) %>%
                    dplyr::select(-concept_class_id)
            }
            else if (input$concepts_choice_%widget_id% == "selected_concepts"){
                if (nrow(data) > 0) data <- data %>% dplyr::filter(procedure_concept_id %in% input$concepts_%widget_id%)
            }
        }
        
        data <-
            data %>%
            join_concepts(d$dataset_concept, c("procedure", "procedure_type")) %>%
            # Replace NA procedure_concept_name by their concept_id
            dplyr::mutate(procedure_concept_name = dplyr::if_else(is.na(procedure_concept_name), as.character(procedure_concept_id), procedure_concept_name)) %>%
            dplyr::arrange(person_id, procedure_datetime)
            
        ordered_levels <- rev(sort(unique(data$procedure_concept_name)))
        data <- data %>% dplyr::mutate(procedure_concept_name = factor(procedure_concept_name, levels = ordered_levels))
            
        unique_levels <- levels(data$procedure_concept_name)
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
                SELECT MIN(visit_detail_start_datetime) AS min_visit_start_datetime, MAX(visit_detail_end_datetime) AS max_visit_end_datetime
                FROM visit_detail
                WHERE visit_detail_id = {m$selected_visit_detail}
            ", .con = d$con)
            data_datetimes_range <- DBI::dbGetQuery(d$con, sql)
        }
        
        data_datetimes_range <- c(data_datetimes_range$min_visit_start_datetime, data_datetimes_range$max_visit_end_datetime)
        m$data_datetimes_range_%widget_id% <- data_datetimes_range
        
        datetimes <- data_datetimes_range
        
        if (isTRUE(input$synchronize_timelines_%widget_id%)){
            if(!is.null(m$debounced_datetimes_timeline_%tab_id%)) if (length(m$debounced_datetimes_timeline_%tab_id%()) > 0) datetimes <- m$debounced_datetimes_timeline_%tab_id%()
        }
        
        if (length(datetimes) > 0) m$datetimes_%widget_id% <- datetimes
        
        if (nrow(data) > 0){
            fig <-
                plotly::plot_ly(data = data, source = "procedure_occurrence_plot_%widget_id%")
                
            if (m$omop_version == "5.3"){
                fig <-
                    fig %>%
                    plotly::add_segments(
                        x = ~procedure_datetime,
                        xend = ~procedure_datetime,
                        y = ~as.numeric(procedure_concept_name),
                        yend = ~as.numeric(procedure_concept_name),
                        marker = list(color = "coral", size = 10),
                        text = ~paste0(
                            i18np$t("procedure"), " : ", procedure_concept_name, "<br>",
                            i18np$t("start"), " : ", format(procedure_datetime, datetime_format), "<br>",
                            i18np$t("quantity"), " : ", quantity
                        ),
                        hoverinfo = "text"
                    )
            } else {
                fig <-
                    fig %>%
                    plotly::add_segments(
                        x = ~procedure_datetime,
                        xend = ~procedure_end_datetime,
                        y = ~as.numeric(procedure_concept_name),
                        yend = ~as.numeric(procedure_concept_name),
                        line = list(color = "coral", width = 5),
                        text = ~paste0(
                            i18np$t("procedure"), " : ", procedure_concept_name, "<br>",
                            i18np$t("start"), " : ", format(procedure_datetime, datetime_format), "<br>",
                            i18np$t("end"), " : ", format(procedure_end_datetime, datetime_format), "<br>",
                            i18np$t("quantity"), " : ", quantity
                        ),
                        hoverinfo = "text"
                    )
            }
            
            fig <-
                fig %>%
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
        }
        
        ## End of part 2
        
        if (length(fig) == 0){
            output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("no_data_to_display"), messageBarType = 5), style = "display: inline-block;"))
        
            shinyjs::show("error_message_div_%widget_id%")
            shinyjs::hide("procedure_occurrence_plot_%widget_id%")
        }
        
        if (length(fig) > 0){
            output$procedure_occurrence_plot_%widget_id% <- plotly::renderPlotly(fig)
            
            shinyjs::hide("error_message_div_%widget_id%")
            shinyjs::show("procedure_occurrence_plot_%widget_id%")
        }
        
        # Go to figure tab
        if (length(input$figure_and_settings_side_by_side_%widget_id%) > 0) if (!input$figure_and_settings_side_by_side_%widget_id%) shinyjs::click("figure_button_%widget_id%")
        
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
