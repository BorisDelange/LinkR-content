# Server - Code

# Init code var
m$code_%widget_id% <- ""

# Prevent a bug with scroll into ace editor
shinyjs::delay(300, shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);"))

# Run code at patient update
observeEvent(m$selected_person, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$selected_person"))
    
    req(length(m$selected_person) > 0)
    req(!is.na(m$selected_person))
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

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
            
            shinyjs::hide("dygraph_%widget_id%")
            shinyjs::show("error_message_div_%widget_id%")
            
        } else {
        
            data <- d[[paste0("data_", data_source)]]$measurement
            
            concept_ids <- input$concepts_%widget_id%
            
            if (data %>% dplyr::count() %>% dplyr::pull() > 0) data <- data %>% dplyr::filter(measurement_concept_id %in% concept_ids)
                
            features <- list()
            features_names <- c()
            
            for (concept_id in concept_ids){
            
                concept <- selected_concepts %>% dplyr::filter(concept_id == !!concept_id)
            
                if (concept$domain_id == "Measurement"){
                    
                    data <- d[[tolower(concept$domain_id)]]
                    
                    if (data %>% dplyr::count() %>% dplyr::pull() > 0){
                        data <-
                            data %>%
                            dplyr::filter(
                                person_id == m$selected_person,
                                !!rlang::sym(paste0(tolower(concept$domain_id), "_concept_id")) == concept_id
                            ) %>%
                            dplyr::rename(datetime = !!rlang::sym(paste0(tolower(concept$domain_id), "_datetime"))) %>%
                            dplyr::select(datetime, value_as_number) %>%
                            dplyr::collect()
                        
                        if (nrow(data) > 0){
                            features[[paste0("concept_", concept_id)]] <- xts::xts(data$value_as_number, data$datetime)
                            features_names <- c(features_names, concept$concept_name)
                        }
                    }
                }
            }
            
            if (length(features) == 0){
                shinyjs::show("error_message_div_%widget_id%")
                shinyjs::hide("dygraph_%widget_id%")
                output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("no_data_to_display"), messageBarType = 5), style = "display: inline-block;"))
            }
            
            if (length(features) > 0){
                combined_features <- do.call(merge, features)
                colnames(combined_features) <- features_names
                
                output$dygraph_%widget_id% <- dygraphs::renderDygraph({
                    dygraphs::dygraph(combined_features) %>%
                    dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2) %>%
                    dygraphs::dyRangeSelector()
                })
                
                shinyjs::hide("error_message_div_%widget_id%")
                shinyjs::show("dygraph_%widget_id%")
            }
        }
        
        # Go to figure tab
        if (length(input$figure_and_settings_side_by_side_%widget_id%) > 0) if (!input$figure_and_settings_side_by_side_%widget_id%) shinyjs::click("figure_button_%widget_id%")
        
    }, error = function(e){
        show_message_bar(output, "error_displaying_figure", "severeWarning", i18n = i18np, ns = ns)
        cat(paste0("\\n", now(), " - widget %widget_id% - input$run_code - error = ", toString(e)))
    })
})

# Save code with shortcut
observeEvent(input$code_%widget_id%_save, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$code_save"))
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_params_and_code_%widget_id%', Math.random());"))
})
