# Server - Figure settings

# Load figure settings

observeEvent(input$load_figure_settings_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$load_figure_settings"))
    
    # Update figure settings UI
    
    link_id <- input$settings_file_%widget_id%
    sql <- glue::glue_sql("SELECT name, value, value_num FROM widgets_options WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", .con = m$db)
    figure_settings <- DBI::dbGetQuery(m$db, sql)
    
    if (nrow(figure_settings) > 0){
        sapply(figure_settings$name, function(name){
        
            value <- figure_settings %>% dplyr::filter(name == !!name) %>% dplyr::pull(value)
            value_num <- figure_settings %>% dplyr::filter(name == !!name) %>% dplyr::pull(value_num)
            
            if (name == "data_source") shiny.fluent::updateDropdown.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            else if (name == "concepts"){
                value <- as.numeric(unlist(strsplit(value, ", ")))
                shiny.fluent::updateDropdown.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            }
            else if (name == "synchronize_timelines"){
                value <- as.logical(value_num)
                shiny.fluent::updateToggle.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            }
        })
    }
    
    # Run code if toggle is activated
    if (length(input$run_code_at_settings_file_load_%widget_id%) > 0) if (input$run_code_at_settings_file_load_%widget_id%) shinyjs::delay(500, 
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-display_figure_%widget_id%', Math.random());")))
})

# Save current settings

observeEvent(input$save_params_and_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$save_params_and_code"))
    
    tryCatch({
    
        # If no settings file is selected, go to settings files management page
        if (length(input$settings_file_%widget_id%) == 0) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_settings_files_tab_%widget_id%', Math.random());"))
        
        if (length(input$settings_file_%widget_id%) > 0){
            
            link_id <- input$settings_file_%widget_id%
        
            # Delete old settings
            sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", .con = m$db))
            
            # Add new settings in db
            new_data <- tibble::tribble(
                ~name, ~value, ~value_num,
                "data_source", input$data_source_%widget_id%, NA_real_,
                "concepts", input$concepts_%widget_id% %>% toString(), NA_real_,
                "synchronize_timelines", NA_character_, as.integer(input$synchronize_timelines_%widget_id%)
            )
            
            new_data <-
                new_data %>%
                dplyr::transmute(
                    id = get_last_row(m$db, "widgets_options") + 1:nrow(new_data), widget_id = %widget_id%, person_id = NA_integer_, link_id = link_id,
                    category = "figure_settings", name, value, value_num, creator_id = m$user_id, datetime = now(), deleted = FALSE
                )
            
            DBI::dbAppendTable(m$db, "widgets_options", new_data)
            
            # Notify user
            show_message_bar(id, output, "modif_saved", "success", i18n = i18n, ns = ns)
        }
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# Synchronize timelines

# Create a reactiveVal with timeline datetimes
if (length(m$datetimes_timeline_%tab_id%) == 0){
    m$datetimes_timeline_%tab_id% <- reactiveVal()
    m$debounced_datetimes_timeline_%tab_id% <- reactive(m$datetimes_timeline_%tab_id%()) %>% debounce(500)
}

observeEvent(input$synchronize_timelines_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$synchronize_timelines"))
    
    tryCatch({
        # Add a padding to align widget's timeline with other widgets
        if (input$synchronize_timelines_%widget_id%) {
            shinyjs::runjs(sprintf(
                "document.getElementById('%s').style.paddingLeft = '80px'; var event = new Event('resize'); window.dispatchEvent(event);",
                ns("dygraph_div_%widget_id%")
            ))
        } else {
            shinyjs::runjs(sprintf(
                "document.getElementById('%s').style.paddingLeft = '0px'; var event = new Event('resize'); window.dispatchEvent(event);",
                ns("dygraph_div_%widget_id%")
            ))
        }
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

observeEvent(input$dygraph_%widget_id%_date_window, {
    %req%
    req(input$synchronize_timelines_%widget_id%)
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$dygraph_%widget_id%_date_window"))
        
    tryCatch({
        datetime_values <- as.POSIXct(input$dygraph_%widget_id%_date_window, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
        m$datetimes_timeline_%tab_id%(datetime_values)
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

observeEvent(m$debounced_datetimes_timeline_%tab_id%(), {
    %req%
    req(input$synchronize_timelines_%widget_id%)
    req(length(m$debounced_datetimes_timeline_%tab_id%()) > 0)
    req(length(m$datetimes_%widget_id%) > 0)
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$debounced_datetimes_timeline"))
    
    tryCatch({
    
        if (
            abs(as.numeric(m$debounced_datetimes_timeline_%tab_id%()[[1]]) - as.numeric(m$datetimes_%widget_id%[[1]])) > 5 |
            abs(as.numeric(m$debounced_datetimes_timeline_%tab_id%()[[2]]) - as.numeric (m$datetimes_%widget_id%[[2]])) > 5){
            shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
        }
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
}, ignoreInit = TRUE)
