# -----------------------------
# --- Get saved parameters ----
# -----------------------------

# Get widget options
sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id%", .con = m$db)
widget_options <- DBI::dbGetQuery(m$db, sql)

m$widget_options_%widget_id% <- widget_options
m$scripts_%widget_id% <- widget_options %>% dplyr::filter(name == "script") %>% dplyr::select(id = value_num, name = value)
m$scripts_temp_%widget_id% <- m$scripts_%widget_id% %>% dplyr::mutate(modified = FALSE)
m$reload_dt_%widget_id% <- now()

# List of inputs (to save & get saved params)

# List of inputs (to save & get saved params)
# Add each input name (without the suffix "_widget_id%") in the corresponding list
# Example : dropdowns <- c("variable") for the dropdown with id "variable_%widget_id%"

dropdowns <- c("variable")
textfields <- c("")
spin_buttons <- c("")
toggle_inputs <- c("run_plot_at_script_load", "run_code_at_script_load")
colour_inputs <- c("")
ace_inputs <- c("code")
inputs <- c(dropdowns, textfields, spin_buttons, toggle_inputs, colour_inputs, ace_inputs)

# List of default values
# Add a default value for each input, with the corresponding name without the suffix "_%widget_id%"
# Example : default_values$num_bins <- 50L

default_values <- list()
default_values$variable <- 0L
###
###
### Default values here
###
###

# Get concepts from selected concepts for this widget
concepts <- 
    tibble::tibble(concept_id = 0L, concept_name = i18np$t("none")) %>% dplyr::bind_rows(selected_concepts %>% dplyr::select(concept_id, concept_name)) %>%
    convert_tibble_to_list(key_col = "concept_id", text_col = "concept_name")

shinyjs::delay(100, shiny.fluent::updateDropdown.shinyInput(session, "concept_%widget_id%", options = concepts, value = 0L))

# -------------------------
# --- Show / hide divs ----
# -------------------------

observeEvent(input$current_tab_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$current_tab_%widget_id%"))
    
    tryCatch({
        sapply(c("plot_and_code_tab_header_%widget_id%", "plot_tab_%widget_id%", "code_tab_%widget_id%", "scripts_management_tab_%widget_id%"), shinyjs::hide)
        shinyjs::show(input$current_tab_%widget_id%)
        if (input$current_tab_%widget_id% %in% c("plot_tab_%widget_id%", "code_tab_%widget_id%")){
            shinyjs::show("plot_and_code_tab_header_%widget_id%")
            sapply(c("plot_tab_header_%widget_id%", "code_tab_header_%widget_id%"), shinyjs::hide)
            if (input$current_tab_%widget_id% == "plot_tab_%widget_id%") shinyjs::show("plot_tab_header_%widget_id%")
            else if (input$current_tab_%widget_id% == "code_tab_%widget_id%") shinyjs::show("code_tab_header_%widget_id%")
        }
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$plot_current_tab_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$plot_current_tab_%widget_id%"))
    
    tryCatch({
        sapply(c("plot_parameters_tab_%widget_id%", "variables_tab_%widget_id%"), shinyjs::hide)
        shinyjs::show(input$plot_current_tab_%widget_id%)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

shinyjs::delay(100, m$trigger_hide_tab_%widget_id% <- now())
observeEvent(m$trigger_hide_tab_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$trigger_hide_tab_%widget_id%"))
    
    tryCatch({
        shinyjs::hide("variables_tab_%widget_id%")
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Hide parameters div
observeEvent(input$hide_params_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$hide_params_%widget_id%"))

    tryCatch({
        if (input$hide_params_%widget_id%){
            shinyjs::hide("split_layout_right_%widget_id%")
            shinyjs::runjs(glue::glue("$('#{id}-split_layout_left_%widget_id%').css('width', '100%');"))
        }
        else {
            shinyjs::runjs(glue::glue("$('#{id}-split_layout_left_%widget_id%').css('width', '50%');"))
            shinyjs::show("split_layout_right_%widget_id%")
        }
        
        # Reload plot
        m$create_plot_type_%widget_id% <- "show_plot"
        m$create_plot_trigger_%widget_id% <- now()
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

divs <- c("plot_and_code_tab_header_%widget_id%", "split_layout_right_%widget_id%", "plot_size_div_%widget_id%", "pivot_%widget_id%")

# Additional buttons for this plugin
output$additional_buttons_%widget_id% <- renderUI({

    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer output$additional_buttons_%widget_id%"))

    tryCatch({
        actionButton(ns("maximize_fig_%widget_id%"), "", icon = icon("maximize"))
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Maximize fig
observeEvent(input$maximize_fig_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$maximize_fig_%widget_id%"))
    
    tryCatch({
        shinyjs::runjs(glue::glue("$('#{id}-split_layout_left_%widget_id%').css('width', '100%');"))
        shinyjs::delay(100, {
            output$additional_buttons_%widget_id% <- renderUI(actionButton(ns("minimize_fig_%widget_id%"), "", icon = icon("minimize")))
            sapply(divs, shinyjs::hide)
            m$create_plot_type_%widget_id% <- "show_plot"
            m$create_plot_trigger_%widget_id% <- now()
        })
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Minimize fig
observeEvent(input$minimize_fig_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$minimize_fig_%widget_id%"))
    
    tryCatch({
        shinyjs::runjs(glue::glue("$('#{id}-split_layout_left_%widget_id%').css('width', '50%');"))
        shinyjs::delay(100, {
            output$additional_buttons_%widget_id% <- renderUI(actionButton(ns("maximize_fig_%widget_id%"), "", icon = icon("maximize")))
            sapply(divs, shinyjs::show)
            m$create_plot_type_%widget_id% <- "show_plot"
            m$create_plot_trigger_%widget_id% <- now()
        })
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# -------------
# --- Plot ----
# -------------

# Load plot or code at patient changeover
observeEvent(m$selected_person, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$selected_person"))
    
    tryCatch({
        if (length(input$run_plot_at_script_load_%widget_id%) > 0){
            m$create_plot_type_%widget_id% <- "show_plot"
            m$create_plot_trigger_%widget_id% <- now()
        }
        
        if (length(input$run_code_at_script_load_%widget_id%) > 0){
            
        }
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Render plot from "plot" tab
observeEvent(input$show_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$show_%widget_id%"))
    
    tryCatch({
        # Then load plot
        m$create_plot_type_%widget_id% <- "show_plot"
        m$create_plot_trigger_%widget_id% <- now()
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Render plot
observeEvent(m$create_plot_trigger_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$create_plot_trigger_%widget_id%"))
    
    tryCatch({
        
        create_plot_type <- m$create_plot_type_%widget_id%
        
        ##
        ##
        ## Code for the output here
        ##
        ##
        
        code <- ""
        
        # Update shinyAce code editor
        if (create_plot_type == "generate_code"){
            shinyAce::updateAceEditor(session, "code_%widget_id%", value = code)
            
            # Go to "Code" tab
            shinyjs::runjs(glue::glue("$('#{id}-pivot_%widget_id% button[name=\"{i18np$t('code')}\"]').click();"))
        }
        
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Save widget parameters
observeEvent(input$save_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$save_%widget_id%"))
    
    tryCatch({
        req(length(input$script_choice_%widget_id%) > 0)
        
        # Delete old options
        sql <- glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = {%widget_id%} AND link_id = {input$script_choice_%widget_id%}", .con = m$db)
        DBI::dbSendStatement(m$db, sql) -> query
        DBI::dbClearResult(query)
        
        last_row <- get_last_row(m$db, "widgets_options")
        
        # Add new options
        new_options <- tibble::tibble(
            id = seq(last_row + 1, last_row + length(inputs)),
            widget_id = %widget_id%, person_id = NA_integer_, link_id = input$script_choice_%widget_id%,
            category = NA_character_, name = NA_character_, value = NA_character_, value_num = NA_real_,
            creator_id = NA_integer_, datetime = as.character(now()), deleted = FALSE)
        
        new_options_values <- tibble::tibble(name = character(), value = character(), value_num = numeric())
        
        for (input_name in inputs){
            if (input_name %in% spin_buttons){
                value_num <- NA_real_
                if (length(input[[paste0(input_name, "_%widget_id%")]]) > 0) value_num <- input[[paste0(input_name, "_%widget_id%")]]
                new_options_values <- new_options_values %>% dplyr::bind_rows(tibble::tibble(name = input_name, value = NA_character_, value_num = value_num))
            } 
            else {
                value <- NA_character_
                if (length(input[[paste0(input_name, "_%widget_id%")]]) > 0) value <- as.character(input[[paste0(input_name, "_%widget_id%")]])
                new_options_values <- new_options_values %>% dplyr::bind_rows(tibble::tibble(name = input_name, value = value, value_num = NA_real_))
            } 
        }
        
        for (col in c("name", "value", "value_num")) new_options[[col]] <- new_options_values[[col]]
        
        DBI::dbAppendTable(m$db, "widgets_options", new_options)
        
        m$widget_options_%widget_id% <- m$widget_options_%widget_id% %>% dplyr::filter(link_id != input$script_choice_%widget_id%) %>% dplyr::bind_rows(new_options)
        
        # Notify user
        show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Run plot / code at script launch
observeEvent(input$run_plot_at_script_load_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_plot_at_script_load_%widget_id%"))
    
    tryCatch({
        if (input$run_plot_at_script_load_%widget_id%) shiny.fluent::updateToggle.shinyInput(session, "run_code_at_script_load_%widget_id%", value = FALSE)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})
observeEvent(input$run_code_at_script_load_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_code_at_script_load_%widget_id%"))
    
    tryCatch({
        if (input$run_code_at_script_load_%widget_id%) shiny.fluent::updateToggle.shinyInput(session, "run_plot_at_script_load_%widget_id%", value = FALSE)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Plot width & height

sapply(c("width", "height"), function(name){
    m[[paste0("debounce_plot_", name, "_%widget_id%")]] <- now()
    observeEvent(input[[paste0("plot_", name, "_%widget_id%")]], {
        %req%
        if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$plot_", name, "_%widget_id%"))
        
        tryCatch({
            # Percentage for width
            # Pixesl for height, starting from 100 % = 400 px
            if (name == "width") value <- paste0(input$plot_width_%widget_id%, "%")
            else value <- paste0(input$plot_height_%widget_id% * 0.01 * 400, "px")
            
            # Manual debounce (it seems debounce doesn't work everywhere)
            
            if (difftime(now(), m[[paste0("debounce_plot_", name, "_%widget_id%")]], units = "secs") > 1){
                m[[paste0("debounce_plot_", name, "_%widget_id%")]] <- now()
                
                # Code to update CSS code for the output
                shinyjs::runjs(glue::glue("$('#{id}-plot_%widget_id%').css('{name}', '{value}');"))
                m$create_plot_type_%widget_id% <- "show_plot"
                m$create_plot_trigger_%widget_id% <- now()
            }
        }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
    })
})


# ------------
# --- Code ---
# ------------

# Generate code
observeEvent(input$generate_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$generate_code_%widget_id%"))
    
    tryCatch({
        m$create_plot_type_%widget_id% <- "generate_code"
        m$create_plot_trigger_%widget_id% <- now()
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Render plot from "code" tab

observeEvent(input$run_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_code_%widget_id%"))
    
    tryCatch({
        m$run_code_%widget_id% <- input$code_%widget_id%
        m$run_code_trigger_%widget_id% <- now()
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(m$run_code_trigger_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$run_code_trigger_%widget_id%"))
    
    tryCatch({
        # Go to plot tab
        shinyjs::runjs(glue::glue("$('#{id}-pivot_%widget_id% button[name=\"{i18np$t('plot')}\"]').click();"))
        
        ##
        ##
        ## Code for the output here
        ##
        ##
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# ---------------
# --- Scripts ---
# ---------------

# Add a new script
observeEvent(input$add_script_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$add_script_%widget_id%"))
    
    tryCatch({
        # Check if name is not empty
        empty_name <- TRUE
        if (length(input$script_name_%widget_id%) > 0) if (input$script_name_%widget_id% != "") empty_name <- FALSE
        if (empty_name) shiny.fluent::updateTextField.shinyInput(session, "script_name_%widget_id%", errorMessage = i18n$t("provide_valid_name"))
        req(!empty_name)
        shiny.fluent::updateTextField.shinyInput(session, "script_name_%widget_id%", errorMessage = NULL)
        
        # Check if name is not already used
        sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id% AND name = 'script' AND value = {input$script_name_%widget_id%}", .con = m$db)
        already_used_name <- DBI::dbGetQuery(m$db, sql) %>% nrow() >= 1
        if (already_used_name) shiny.fluent::updateTextField.shinyInput(session, "script_name_%widget_id%", errorMessage = i18n$t("name_already_used"))
        req(!already_used_name)
        shiny.fluent::updateTextField.shinyInput(session, "script_name_%widget_id%", errorMessage = NULL)
        
        # Add script to database
        last_row <- get_last_row(m$db, "widgets_options")
        sql <- glue::glue_sql("SELECT COALESCE(MAX(value_num), 0) FROM widgets_options WHERE widget_id = %widget_id% AND name = 'script'", .con = m$db)
        last_id <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull()
        
        new_options <- tibble::tribble(
            ~id, ~widget_id, ~person_id, ~link_id, ~category, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
            last_row + 1, %widget_id%, NA_integer_, NA_integer_, NA_character_, "script", input$script_name_%widget_id%, last_id + 1, NA_integer_, as.character(now()), FALSE)
        
        code_row <- tibble::tribble(
            ~id, ~widget_id, ~person_id, ~link_id, ~category, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
            last_row + 2, %widget_id%, NA_integer_, last_id + 1, NA_character_, "code", "", NA_integer_, NA_integer_, as.character(now()), FALSE)
        
        new_options <- new_options %>% dplyr::bind_rows(code_row)
            
        DBI::dbAppendTable(m$db, "widgets_options", new_options)
        
        sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id%", .con = m$db)
        widget_options <- DBI::dbGetQuery(m$db, sql)
        
        m$widget_options_%widget_id% <- m$widget_options_%widget_id% %>% dplyr::bind_rows(code_row)
        
        # Reset TextField
        shiny.fluent::updateTextField.shinyInput(session, "script_name_%widget_id%", value = "")
        
        # Notify user
        show_message_bar(output, "script_added", "success", i18n = i18np, ns = ns)
        
        # Add new script to scripts vector
        m$scripts_%widget_id% <- m$scripts_%widget_id% %>% 
            dplyr::bind_rows(tibble::tibble(id = last_id + 1, name = input$script_name_%widget_id%))
        m$scripts_temp_%widget_id% <- m$scripts_%widget_id% %>% dplyr::mutate(modified = TRUE)
        
        # Update DT & dropdown
        m$reload_dt_%widget_id% <- now()
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Load a script
observeEvent(input$script_choice_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$script_choice_%widget_id%"))
    
    tryCatch({
        widget_options <- m$widget_options_%widget_id% %>% dplyr::filter(link_id == input$script_choice_%widget_id%)
        
        for (input_name in inputs){
            widget_option <- widget_options %>% dplyr::filter(name == input_name)
            
            # Update inputs with saved values
            if (nrow(widget_option) > 0){
            
                if (input_name %in% dropdowns){
                    if (grepl("variable_[0-9]", input_name)) value <- widget_option$value_num else value <- widget_option$value
                    shiny.fluent::updateDropdown.shinyInput(session, paste0(input_name, "_%widget_id%"), value = value)
                }
                if (input_name %in% textfields) shiny.fluent::updateTextField.shinyInput(session, paste0(input_name, "_%widget_id%"), value = widget_option$value)
                if (input_name %in% spin_buttons) shiny.fluent::updateSpinButton.shinyInput(session, paste0(input_name, "_%widget_id%"), value = widget_option$value_num)
                if (input_name %in% toggle_inputs) shiny.fluent::updateToggle.shinyInput(session, paste0(input_name, "_%widget_id%"), value = as.logical(widget_option$value))
                if (input_name %in% colour_inputs) shiny.fluent::updateSwatchColorPicker.shinyInput(session, paste0(input_name, "_%widget_id%"), value = widget_option$value)
                if (input_name %in% ace_inputs) shinyAce::updateAceEditor(session, paste0(input_name, "_%widget_id%"), value = widget_option$value)
                
                if (input_name == "run_code_at_script_load") run_code_at_script_load <- as.logical(widget_option$value)
                if (input_name == "run_plot_at_script_load") run_plot_at_script_load <- as.logical(widget_option$value)
            }
            if (nrow(widget_option) == 0){
                if (input_name %in% dropdowns) shiny.fluent::updateDropdown.shinyInput(session, paste0(input_name, "_%widget_id%"), value = default_values[[input_name]])
                if (input_name %in% textfields) shiny.fluent::updateTextField.shinyInput(session, paste0(input_name, "_%widget_id%"), value = default_values[[input_name]])
                if (input_name %in% spin_buttons) shiny.fluent::updateSpinButton.shinyInput(session, paste0(input_name, "_%widget_id%"), value = default_values[[input_name]])
                if (input_name %in% toggle_inputs) shiny.fluent::updateToggle.shinyInput(session, paste0(input_name, "_%widget_id%"), value = default_values[[input_name]])
                if (input_name %in% colour_inputs) shiny.fluent::updateSwatchColorPicker.shinyInput(session, paste0(input_name, "_%widget_id%"), value = default_values[[input_name]])
                if (input_name %in% ace_inputs) shinyAce::updateAceEditor(session, paste0(input_name, "_%widget_id%"), value = default_values[[input_name]])
            }
        }
        
        # Save that this script is selected
        sql <- glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND name = 'selected_script'", .con = m$db)
        query <- DBI::dbSendStatement(m$db, sql)
        DBI::dbClearResult(query)
        
        last_row <- get_last_row(m$db, "widgets_options")
        
        new_options <- tibble::tibble(
            id = last_row + 1, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
            category = NA_character_, name = "selected_script", value = NA_character_, value_num = input$script_choice_%widget_id%,
            creator_id = NA_integer_, datetime = as.character(now()), deleted = FALSE)
            
        DBI::dbAppendTable(m$db, "widgets_options", new_options)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Var for delete confirm react
m$delete_open_dialog_%widget_id% <- FALSE

# Update scripts DT & dropdown
observeEvent(m$reload_dt_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$reload_dt_%widget_id%"))
    
    tryCatch({
        # Reload DT
        
        # Prepare data for the datatable
        m$scripts_datatable_temp_%widget_id% <- 
            m$scripts_%widget_id% %>%
            dplyr::rename(id_temp = id) %>%
            dplyr::mutate(action = as.character(actionButton("delete_%id%", "", icon = icon("trash-alt"), 
                onclick = paste0("Shiny.setInputValue('", id, "-deleted_pressed_%widget_id%', this.id, {priority: 'event'})")))) %>%
            dplyr::mutate(action = stringr::str_replace_all(action, "%id%", as.character(id_temp))) %>%
            dplyr::rename(id = id_temp)
        
        # If there is not already a proxy, create datatable
        if (length(m$datatable_proxy_%widget_id%) == 0){
            render_datatable(output = output, ns = ns, i18n = i18n, data = m$scripts_datatable_temp_%widget_id%,
                output_name = "scripts_management_datatable_%widget_id%", col_names = c(i18n$t("id"), i18n$t("name"), i18n$t("action")),
                editable_cols = "name", sortable_cols = c("id", "name"), centered_cols = c("id", "action"), column_widths = c("id" = "80px", "action" = "80px"),
                searchable_cols = "name", filter = TRUE, selection = "multiple")
            
            # Create a proxy for this datatable
            m$datatable_proxy_%widget_id% <- DT::dataTableProxy("scripts_management_datatable_%widget_id%", deferUntilFlush = FALSE)
        }
        if (length(m$datatable_proxy_%widget_id%) > 0) DT::replaceData(m$datatable_proxy_%widget_id%, m$scripts_datatable_temp_%widget_id%, resetPaging = FALSE, rownames = FALSE)
        
        # Update dropdown
        value <- NULL
        if (length(input$script_choice_%widget_id%) > 0) value <- input$script_choice_%widget_id%
        if (length(input$script_choice_%widget_id%) == 0 & nrow(m$scripts_%widget_id%) > 0){
            # Load last selected script
            selected_script <- m$widget_options_%widget_id%  %>% dplyr::filter(name == "selected_script")
            if (nrow(selected_script) > 0) value <- selected_script %>% dplyr::pull(value_num)
        }
        
        shiny.fluent::updateDropdown.shinyInput(session, "script_choice_%widget_id%", 
            options = convert_tibble_to_list(m$scripts_%widget_id%, key_col = "id", text_col = "name"), value = value)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Updates on scripts DT
observeEvent(input$scripts_management_datatable_%widget_id%_cell_edit, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_management_datatable_%widget_id%_cell_edit"))
    
    tryCatch({
        edit_info <- input$scripts_management_datatable_%widget_id%_cell_edit
        m$scripts_temp_%widget_id% <- DT::editData(m$scripts_temp_%widget_id%, edit_info, rownames = FALSE)
          
        # Store that this row has been modified
        m$scripts_temp_%widget_id%[[edit_info$row, "modified"]] <- TRUE
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Save updates on scripts
observeEvent(input$save_scripts_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$save_scripts_%widget_id%"))
    
    tryCatch({
        # Check if there are no duplicates in names
        duplicates <- m$scripts_temp_%widget_id% %>% dplyr::mutate_at("name", tolower) %>% dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow() >= 1
        
        if (duplicates) show_message_bar(output, "scripts_names_duplicates", "severeWarning", i18n = i18np, ns = ns)
        
        req(!duplicates)
        
        req(nrow(m$scripts_temp_%widget_id%) > 0)
        
        # Delete old options
        sql <- glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND name = 'script'", .con = m$db)
        query <- DBI::dbSendStatement(m$db, sql)
        DBI::dbClearResult(query)
        
        # Add new options
        last_row <- get_last_row(m$db, "widgets_options")
        
        new_options <- tibble::tibble(
            id = seq(last_row + 1, last_row + nrow(m$scripts_temp_%widget_id%)),
            widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
            category = NA_character_, name = "script", value = m$scripts_temp_%widget_id%$name, value_num = m$scripts_temp_%widget_id%$id,
            creator_id = NA_integer_, datetime = as.character(now()), deleted = FALSE)
            
        DBI::dbAppendTable(m$db, "widgets_options", new_options)
        m$scripts_%widget_id% <- new_options %>% dplyr::select(id = value_num, name = value)
        
        # Update scripts dropdown
        value <- NULL
        if (length(input$script_choice_%widget_id%) > 0) value <- input$script_choice_%widget_id%
        shiny.fluent::updateDropdown.shinyInput(session, "script_choice_%widget_id%", 
            options = convert_tibble_to_list(m$scripts_temp_%widget_id%, key_col = "id", text_col = "name"), value = value)
        
        # Notify user
        show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Delete scripts

### Delete with trash icon
observeEvent(input$deleted_pressed_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$deleted_pressed_%widget_id%"))
    
    tryCatch({
        # Reload datatable (to unselect rows)
        DT::replaceData(m$datatable_proxy_%widget_id%, m$scripts_datatable_temp_%widget_id%, resetPaging = FALSE, rownames = FALSE)
        
        m$delete_scripts_%widget_id% <- as.integer(substr(input$deleted_pressed_%widget_id%, nchar("delete_") + 1, 100))
        m$delete_open_dialog_%widget_id% <- TRUE
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

### Delete with "delete selection" button
observeEvent(input$delete_scripts_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$delete_scripts_%widget_id%"))
    
    tryCatch({
        req(length(input$scripts_management_datatable_%widget_id%_rows_selected) > 0)
        m$delete_scripts_%widget_id% <- m$scripts_%widget_id%[input$scripts_management_datatable_%widget_id%_rows_selected, ] %>% dplyr::pull(id)
        m$delete_open_dialog_%widget_id% <- TRUE
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

### reactOutput for deletion confirmation
output$delete_confirm_%widget_id% <- shiny.fluent::renderReact({
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - output$delete_confirm_%widget_id%"))
    
    tryCatch({
        shiny.fluent::Dialog(
            hidden = !m$delete_open_dialog_%widget_id%,
            onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('scripts_hide_dialog_%widget_id%', Math.random()); }")),
            dialogContentProps = list(
                type = 0,
                title = i18np$t("confirm_deletion_title"),
                closeButtonAriaLabel = "Close",
                subText = tagList(i18np$t("confirm_deletion_subtext"), br(), br()
            )
        ),
        modalProps = list(),
        shiny.fluent::DialogFooter(
            shiny.fluent::PrimaryButton.shinyInput(ns("scripts_delete_confirmed_%widget_id%"), text = i18n$t("delete")),
            shiny.fluent::DefaultButton.shinyInput(ns("scripts_delete_canceled_%widget_id%"), text = i18n$t("dont_delete"))
            )
        )
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

### Close reactOutput
observeEvent(input$scripts_hide_dialog_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_hide_dialog_%widget_id%"))
    
    tryCatch({
        m$delete_open_dialog_%widget_id% <- FALSE
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})
observeEvent(input$scripts_delete_canceled_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_delete_canceled_%widget_id%"))
    
    tryCatch({
        m$delete_open_dialog_%widget_id% <- FALSE
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

### Deletion confirmed
observeEvent(input$scripts_delete_confirmed_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_delete_confirmed_%widget_id%"))
    
    tryCatch({
        m$delete_open_dialog_%widget_id% <- FALSE
        
        # Get scripts ids
        ids_to_del <- m$delete_scripts_%widget_id%
        
        # Delete scripts in DB
        sql <- glue::glue_sql(paste0("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND (",
            "(name = 'script' AND value_num IN ({ids_to_del*})) OR ",
            "(link_id IN ({ids_to_del*})))"), .con = m$db)
        query <- DBI::dbSendStatement(m$db, sql)
        DBI::dbClearResult(query)
        
        # Update m var
        m$scripts_%widget_id% <- m$scripts_%widget_id% %>% dplyr::filter(id %not_in% ids_to_del)
        m$scripts_temp_%widget_id% <- m$scripts_%widget_id% %>% dplyr::mutate(modified = TRUE)
        
        # Reload DT
        m$reload_dt_%widget_id% <- now()
        
        # Notify user
        show_message_bar(output, "script_deleted", "severeWarning", i18n = i18np, ns = ns)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})
