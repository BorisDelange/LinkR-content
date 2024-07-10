# -------
# Tabs --
# -------

## All tabs
tabs <- c("figure", "figure_settings", "code", "general_settings")

## Create an observer by tab, show selected tab, hide all others
sapply(tabs, function(tab){

    observeEvent(input[[paste0(tab, "_button_%widget_id%")]], {
        %req%
        if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$", tab))
        
        tryCatch({
            if (tab == "general_settings") shinyjs::hide("figure_settings_code_div_%widget_id%")
            else shinyjs::show("figure_settings_code_div_%widget_id%")
            
            sapply(paste0(setdiff(tabs, tab), "_div_%widget_id%"), shinyjs::hide)
            shinyjs::hide("saved_settings_div_%widget_id%")
            shinyjs::show(paste0(tab, "_div_%widget_id%"))
            
            if (tab %in% c("figure_settings", "code")){
                if (length(input$figure_and_settings_side_by_side_%widget_id%) > 0) if (input$figure_and_settings_side_by_side_%widget_id%) shinyjs::show("figure_div_%widget_id%")
                shinyjs::show("figure_settings_code_sidenav_%widget_id%")
            }
            else shinyjs::hide("figure_settings_code_sidenav_%widget_id%")
        }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
    })
})

# ------------------
# Figure settings --
# ------------------

%import_script('server_figure_settings.R')%

# -------
# Code --
# -------

%import_script('server_code.R')%

# -----------
# Settings --
# -----------

# Settings / editor side-by-side with figure
observeEvent(input$figure_and_settings_side_by_side_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$figure_and_settings_side_by_side"))
    
    tryCatch({
        if (input$figure_and_settings_side_by_side_%widget_id%){
            shinyjs::runjs(paste0(
                "$('#", id, "-figure_div_%widget_id%').css('width', '50%');",
                "$('#", id, "-figure_settings_div_%widget_id%').css('width', '50%');",
                "$('#", id, "-code_div_%widget_id%').css('width', '50%');"
            ))
            shinyjs::hide("figure_button_div_%widget_id%")
        }
        else {
            shinyjs::runjs(paste0(
                "$('#", id, "-figure_div_%widget_id%').css('width', '100%');",
                "$('#", id, "-figure_settings_div_%widget_id%').css('width', '100%');",
                "$('#", id, "-code_div_%widget_id%').css('width', '100%');"
            ))
            shinyjs::show("figure_button_div_%widget_id%")
        }
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# Save general settings in db
observeEvent(input$save_general_settings_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$save_general_settings"))
    
    tryCatch({
    
        # Delete old rows
        sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND category = 'general_settings'", .con = m$db))
        
        file_id <- input$saved_settings_%widget_id%
        new_data <- tibble::tibble(name = "selected_file_id", value = NA_character_, value_num = file_id)
        
        for (name in c("show_saved_file", "figure_and_settings_side_by_side", "run_code_at_patient_update")){
            toggle_value <- 0L
            input_name <- paste0(name, "_%widget_id%")
            if (length(input[[input_name]]) > 0) if (input[[input_name]]) toggle_value <- 1L
            new_data <- new_data %>% dplyr::bind_rows(tibble::tibble(name = name, value = NA_character_, value_num = toggle_value))
        }
        
        new_data <-
            new_data %>%
            dplyr::transmute(
                id = get_last_row(m$db, "widgets_options") + 1:4, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
                category = "general_settings", name, value, value_num, creator_id = m$user_id, datetime = now(), deleted = FALSE
            )
        
        DBI::dbAppendTable(m$db, "widgets_options", new_data)
        
        # Notify user
        show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# Apply settings when the plugin is loaded
shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-load_general_settings_%widget_id%', Math.random());"))

observeEvent(input$load_general_settings_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$load_general_settings"))
    
    tryCatch({
        
        # Get data from db
        sql <- glue::glue_sql("SELECT name, value, value_num FROM widgets_options WHERE widget_id = %widget_id% AND category = 'general_settings'", .con = m$db)
        general_settings <- DBI::dbGetQuery(m$db, sql)
        
        if (nrow(general_settings) > 0){
            # Loaded file
            loaded_file <- general_settings %>% dplyr::filter(name == "selected_file_id") %>% dplyr::pull(value_num)
            if (is.na(loaded_file)) loaded_file <- "null"
            
            shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_dropdown_%widget_id%', Math.random());"))
            shinyjs::delay(100, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-saved_settings_%widget_id%', ", loaded_file, ");")))
        }
        
        for (name in c("show_saved_file", "figure_and_settings_side_by_side", "run_code_at_patient_update")){
        
            toggle_value <- general_settings %>% dplyr::filter(name == !!name) %>% dplyr::pull(value_num)
            if (is.na(toggle_value)) toggle_value <- FALSE
            
            shiny.fluent::updateToggle.shinyInput(session, paste0(name, "_%widget_id%"), value = toggle_value)
        }
    
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# -----------------
# Saved settings --
# -----------------

%import_script('server_saved_settings.R')%
