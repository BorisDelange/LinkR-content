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

    if ("projects_console_access" %in% user_accesses){
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
        else if ("projects_console_access" %in% user_accesses){
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
            
            shinyjs::hide("dygraph_div_%widget_id%")
            shinyjs::show("error_message_div_%widget_id%")
            
        } else {
            
            if (data_source == "person") sql <- glue::glue_sql("SELECT * FROM measurement WHERE person_id = {m$selected_person}", .con = d$con)
            else if (data_source == "visit_detail") sql <- glue::glue_sql("SELECT * FROM measurement WHERE visit_detail_id = {m$selected_visit_detail}", .con = d$con)
            raw_data <- DBI::dbGetQuery(d$con, sql)
            
            concept_ids <- input$concepts_%widget_id%
            
            if (nrow(raw_data) > 0) raw_data <- raw_data %>% dplyr::filter(measurement_concept_id %in% concept_ids)
                
            features <- list()
            features_names <- c()
            
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
            
            for (concept_id in concept_ids){
            
                concept <- selected_concepts %>% dplyr::filter(concept_id == !!concept_id)
            
                if (concept$domain_id == "Measurement"){
                
                    data <- raw_data
                
                    if (nrow(data) > 0){
                        data <-
                            data %>%
                            dplyr::filter(
                                !!rlang::sym(paste0(tolower(concept$domain_id), "_concept_id")) == concept_id
                            ) %>%
                            dplyr::rename(datetime = !!rlang::sym(paste0(tolower(concept$domain_id), "_datetime"))) %>%
                            dplyr::select(datetime, value_as_number)
                        
                        if (nrow(data) > 0){
                            fake_data <- tibble::tibble(
                                datetime = c(data_datetimes_range[[1]] - lubridate::seconds(1), data_datetimes_range[[2]] + lubridate::seconds(1)),
                                value_as_number = c(NA, NA)
                            )
                            
                            data <- dplyr::bind_rows(fake_data, data)
                            data <- data %>% dplyr::arrange(datetime)
                        
                            features[[paste0("concept_", concept_id)]] <- xts::xts(data$value_as_number, data$datetime)
                            features_names <- c(features_names, concept$concept_name)
                        }
                    }
                }
            }
            
            if (length(features) == 0){
                shinyjs::show("error_message_div_%widget_id%")
                shinyjs::hide("dygraph_div_%widget_id%")
                output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("no_data_to_display"), messageBarType = 5), style = "display: inline-block;"))
            }
            
            if (length(features) > 0){
                combined_features <- do.call(merge, features)
                colnames(combined_features) <- features_names
                
                output$dygraph_%widget_id% <- dygraphs::renderDygraph({
                
                    if (isTRUE(input$synchronize_timelines_%widget_id%)) fig <- dygraphs::dygraph(combined_features, group = "tab_%tab_id%")
                    else fig <- dygraphs::dygraph(combined_features)
                
                    fig <-
                        fig %>%
                        dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2, useDataTimezone = TRUE) %>%
                        dygraphs::dyRangeSelector(dateWindow = c(
                            format(datetimes[[1]], "%Y-%m-%d %H:%M:%S"),
                            format(datetimes[[2]], "%Y-%m-%d %H:%M:%S")
                        )) %>%
                        dygraphs::dyAxis("y", valueRange = c(0, NA))
                })
                
                shinyjs::hide("error_message_div_%widget_id%")
                shinyjs::show("dygraph_div_%widget_id%")
            }
        }
        
        # Go to figure tab
        if (length(input$figure_and_settings_side_by_side_%widget_id%) > 0) if (!input$figure_and_settings_side_by_side_%widget_id%) shinyjs::click("figure_button_%widget_id%")
        
    }, error = function(e){
        show_message_bar(id, output, "error_displaying_figure", "severeWarning", i18n = i18np, ns = ns)
        cat(paste0("\\n", now(), " - widget %widget_id% - input$run_code - error = ", toString(e)))
    })
})

# Save code with shortcut
observeEvent(input$code_%widget_id%_save, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$code_save"))
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_params_and_code_%widget_id%', Math.random());"))
})
