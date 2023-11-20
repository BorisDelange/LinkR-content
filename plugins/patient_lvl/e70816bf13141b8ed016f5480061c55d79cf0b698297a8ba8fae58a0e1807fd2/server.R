# Get saved params for this widget
# sql <- glue::glue_sql("SELECT * FROM aggregated_widgets_options WHERE widget_id = %widget_id%", .con = r$db)
# widget_options <- DBI::dbGetQuery(m$db, sql)
# 
# m$widget_options_%widget_id% <- widget_options
# m$scripts_%widget_id% <- widget_options %>% dplyr::filter(name == "script") %>% dplyr::select(id = value_num, name = value)
# m$scripts_temp_%widget_id% <- m$scripts_%widget_id% %>% dplyr::mutate(modified = FALSE)
# m$reload_dt_%widget_id% <- Sys.time()
# 
# List of inputs (to save & get saved params)

# dropdowns <- c("plot_function", "plot_theme", "bins_type", "x_variable", "y_variable", "colour_pal", "group_by", "group_by_type", "summarize_fct")
# textfields <- c("x_label", "y_label")
# spin_buttons <- c("num_of_bins", "bin_width", "group_by_num")
# toggle_inputs <- c("group_data", "run_code_at_script_launch", "run_plot_at_script_launch")
# colour_inputs <- "colour"
# ace_inputs <- "code"
# inputs <- c(dropdowns, textfields, spin_buttons, toggle_inputs, colour_inputs, ace_inputs)
# 
# default_values <- list()
# default_values$plot_function <- "geom_histogram"
# default_values$plot_theme <- "theme_minimal"
# default_values$bins_type <- "num_of_bins"
# default_values$x_variable <- 0L
# default_values$y_variable <- 0L
# default_values$colour_pal <- "Set1"
# default_values$group_by <- "datetime"
# default_values$group_by_type <- "hours"
# default_values$summarize_fct <- "mean"
# default_values$x_label <- ""
# default_values$y_label <- ""
# default_values$num_of_bins <- 50L
# default_values$bin_width <- 10L
# default_values$group_by_num <- 4L
# default_values$group_data <- FALSE
# default_values$colour <- "#E41A1C"
# default_values$run_code_at_script_launch <- FALSE
# default_values$run_plot_at_script_launch <- FALSE
# default_values$code <- ""

# -------------
# --- Plot ----
# -------------

# Update x & y variables dropdowns
# concepts <- tibble::tibble(concept_id = 0L, concept_name = i18np$t("none")) %>% dplyr::bind_rows(selected_concepts %>% dplyr::select(concept_id, concept_name))
# x_variables <- convert_tibble_to_list(concepts, key_col = "concept_id", text_col = "concept_name")
# shiny.fluent::updateDropdown.shinyInput(session, "x_variable_%widget_id%", options = x_variables)
# y_variables <- convert_tibble_to_list(concepts, key_col = "concept_id", text_col = "concept_name")
# shiny.fluent::updateDropdown.shinyInput(session, "y_variable_%widget_id%", options = y_variables)



# ------------
# --- Code ---
# ------------

# Generate code
observeEvent(input$generate_code_%widget_id%, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$generate_code_%widget_id%"))
    m$create_plot_type_%widget_id% <- "generate_code"
    m$create_plot_trigger_%widget_id% <- Sys.time()
})

# Render plot from "code" tab

observeEvent(input$run_code_%widget_id%, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_code_%widget_id%"))
    m$run_code_%widget_id% <- input$code_%widget_id%
    m$run_code_trigger_%widget_id% <- Sys.time()
})

observeEvent(m$run_code_trigger_%widget_id%, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer m$run_code_trigger_%widget_id%"))
    
    # Go to plot tab
    shinyjs::runjs(glue::glue("$('#{id}-pivot_%widget_id% button[name=\"{i18np$t('plot')}\"]').click();"))
    
    # Render plot
    output$plot_output_%widget_id% <- renderPlot({
        %req%
        if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - output$plot_output_%widget_id%"))
        eval(parse(text = m$run_code_%widget_id%))
    })
})

# ---------------
# --- Scripts ---
# ---------------

# Add a new script
observeEvent(input$add_script_%widget_id%, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$add_script_%widget_id%"))
    
    # Check if name is not empty
    empty_name <- TRUE
    if (length(input$script_name_%widget_id%) > 0) if (input$script_name_%widget_id% != "") empty_name <- FALSE
    if (empty_name) shiny.fluent::updateTextField.shinyInput(session, "script_name_%widget_id%", errorMessage = i18n$t("provide_valid_name"))
    req(!empty_name)
    shiny.fluent::updateTextField.shinyInput(session, "script_name_%widget_id%", errorMessage = NULL)
    
    # Check if name is not already used
    sql <- glue::glue_sql("SELECT * FROM aggregated_widgets_options WHERE widget_id = %widget_id% AND name = 'script' AND value = {input$script_name_%widget_id%}", .con = m$db)
    already_used_name <- DBI::dbGetQuery(m$db, sql) %>% nrow() >= 1
    if (already_used_name) shiny.fluent::updateTextField.shinyInput(session, "script_name_%widget_id%", errorMessage = i18n$t("name_already_used"))
    req(!already_used_name)
    shiny.fluent::updateTextField.shinyInput(session, "script_name_%widget_id%", errorMessage = NULL)
    
    # Add script to database
    
    last_row <- get_last_row(m$db, "aggregated_widgets_options")
    sql <- glue::glue_sql("SELECT COALESCE(MAX(value_num), 0) FROM aggregated_widgets_options WHERE widget_id = %widget_id% AND name = 'script'", .con = m$db)
    last_id <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull()
    
    new_options <- tibble::tibble(
        id = last_row + 1, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
        category = NA_character_, name = "script", value = input$script_name_%widget_id%, value_num = last_id + 1,
        creator_id = NA_integer_, datetime = as.character(Sys.time()), deleted = FALSE)
        
    DBI::dbAppendTable(m$db, "aggregated_widgets_options", new_options)
    
    # Reset TextField
    shiny.fluent::updateTextField.shinyInput(session, "script_name_%widget_id%", value = "")
    
    # Notify user
    show_message_bar(output, "script_added", "success", i18n = i18np, ns = ns)
    
    # Add new script to scripts vector
    m$scripts_%widget_id% <- m$scripts_%widget_id% %>% 
        dplyr::bind_rows(tibble::tibble(id = last_id + 1, name = input$script_name_%widget_id%))
    m$scripts_temp_%widget_id% <- m$scripts_%widget_id% %>% dplyr::mutate(modified = TRUE)
    
    # Update DT & dropdown
    m$reload_dt_%widget_id% <- Sys.time()
})

# Load a script
observeEvent(input$script_choice_%widget_id%, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$script_choice_%widget_id%"))
    
    widget_options <- m$widget_options_%widget_id% %>% dplyr::filter(link_id == input$script_choice_%widget_id%)
    
    run_code_at_script_launch <- FALSE
    run_plot_at_script_launch <- FALSE
    code <- ""
    
    for (input_name in inputs){
        widget_option <- widget_options %>% dplyr::filter(name == input_name)
        
        # Update inputs with saved values
        if (nrow(widget_option) > 0){
            if (input_name %in% dropdowns){
                if (input_name %in% c("x_variable", "y_variable")) value <- widget_option$value_num else value <- widget_option$value
                shiny.fluent::updateDropdown.shinyInput(session, paste0(input_name, "_%widget_id%"), value = value)
            }
            if (input_name %in% textfields) shiny.fluent::updateTextField.shinyInput(session, paste0(input_name, "_%widget_id%"), value = widget_option$value)
            if (input_name %in% spin_buttons) shiny.fluent::updateSpinButton.shinyInput(session, paste0(input_name, "_%widget_id%"), value = widget_option$value_num)
            if (input_name %in% toggle_inputs) shiny.fluent::updateToggle.shinyInput(session, paste0(input_name, "_%widget_id%"), value = as.logical(widget_option$value))
            if (input_name %in% colour_inputs) shiny.fluent::updateSwatchColorPicker.shinyInput(session, paste0(input_name, "_%widget_id%"), value = widget_option$value)
            if (input_name %in% ace_inputs) shinyAce::updateAceEditor(session, paste0(input_name, "_%widget_id%"), value = widget_option$value)
            
            if (input_name == "run_code_at_script_launch") run_code_at_script_launch <- as.logical(widget_option$value)
            if (input_name == "run_plot_at_script_launch") run_plot_at_script_launch <- as.logical(widget_option$value)
            if (input_name == "code") code <- widget_option$value
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
    
    # Render this plot
    if (run_code_at_script_launch){
        m$run_code_%widget_id% <- code
        m$run_code_trigger_%widget_id% <- Sys.time()
    }
    else if (run_plot_at_script_launch) shinyjs::delay(500, shinyjs::click("show_%widget_id%"))
    
    # Save that this script is selected
    sql <- glue::glue_sql("DELETE FROM aggregated_widgets_options WHERE widget_id = %widget_id% AND name = 'selected_script'", .con = m$db)
    query <- DBI::dbSendStatement(m$db, sql)
    DBI::dbClearResult(query)
    
    last_row <- get_last_row(m$db, "aggregated_widgets_options")
    
    new_options <- tibble::tibble(
        id = last_row + 1, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
        category = NA_character_, name = "selected_script", value = NA_character_, value_num = input$script_choice_%widget_id%,
        creator_id = NA_integer_, datetime = as.character(Sys.time()), deleted = FALSE)
        
    DBI::dbAppendTable(m$db, "aggregated_widgets_options", new_options)
})

# Var for delete confirm react
m$delete_open_dialog_%widget_id% <- FALSE

# Update scripts DT & dropdown
observeEvent(m$reload_dt_%widget_id%, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer m$reload_dt_%widget_id%"))
    
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
})

# Updates on scripts DT
observeEvent(input$scripts_management_datatable_%widget_id%_cell_edit, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_management_datatable_%widget_id%_cell_edit"))
    
    edit_info <- input$scripts_management_datatable_%widget_id%_cell_edit
    m$scripts_temp_%widget_id% <- DT::editData(m$scripts_temp_%widget_id%, edit_info, rownames = FALSE)
      
    # Store that this row has been modified
    m$scripts_temp_%widget_id%[[edit_info$row, "modified"]] <- TRUE
})

# Save updates on scripts
observeEvent(input$save_scripts_%widget_id%, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$save_scripts_%widget_id%"))
    
    # Check if there are no duplicates in names
    duplicates <- m$scripts_temp_%widget_id% %>% dplyr::mutate_at("name", tolower) %>% dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow() >= 1
    
    if (duplicates) show_message_bar(output, "scripts_names_duplicates", "severeWarning", i18n = i18np, ns = ns)
    
    req(!duplicates)
    
    req(nrow(m$scripts_temp_%widget_id%) > 0)
    
    # Delete old options
    sql <- glue::glue_sql("DELETE FROM aggregated_widgets_options WHERE widget_id = %widget_id% AND name = 'script'", .con = m$db)
    query <- DBI::dbSendStatement(m$db, sql)
    DBI::dbClearResult(query)
    
    # Add new options
    last_row <- get_last_row(m$db, "aggregated_widgets_options")
    
    new_options <- tibble::tibble(
        id = seq(last_row + 1, last_row + nrow(m$scripts_temp_%widget_id%)),
        widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
        category = NA_character_, name = "script", value = m$scripts_temp_%widget_id%$name, value_num = m$scripts_temp_%widget_id%$id,
        creator_id = NA_integer_, datetime = as.character(Sys.time()), deleted = FALSE)
        
    DBI::dbAppendTable(m$db, "aggregated_widgets_options", new_options)
    
    # Update scripts dropdown
    value <- NULL
    if (length(input$script_choice_%widget_id%) > 0) value <- input$script_choice_%widget_id%
    shiny.fluent::updateDropdown.shinyInput(session, "script_choice_%widget_id%", 
        options = convert_tibble_to_list(m$scripts_temp_%widget_id%, key_col = "id", text_col = "name"), value = value)
    
    # Notify user
    show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
})

# Delete scripts

### Delete with trash icon
observeEvent(input$deleted_pressed_%widget_id%, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$deleted_pressed_%widget_id%"))
    
    # Reload datatable (to unselect rows)
    DT::replaceData(m$datatable_proxy_%widget_id%, m$scripts_datatable_temp_%widget_id%, resetPaging = FALSE, rownames = FALSE)
    
    m$delete_scripts_%widget_id% <- as.integer(substr(input$deleted_pressed_%widget_id%, nchar("delete_") + 1, 100))
    m$delete_open_dialog_%widget_id% <- TRUE
})

### Delete with "delete selection" button
observeEvent(input$delete_scripts_%widget_id%, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$delete_scripts_%widget_id%"))
    
    req(length(input$scripts_management_datatable_%widget_id%_rows_selected) > 0)
    m$delete_scripts_%widget_id% <- m$scripts_%widget_id%[input$scripts_management_datatable_%widget_id%_rows_selected, ] %>% dplyr::pull(id)
    m$delete_open_dialog_%widget_id% <- TRUE
})

### reactOutput for deletion confirmation
output$delete_confirm_%widget_id% <- shiny.fluent::renderReact({
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - output$delete_confirm_%widget_id%"))
    
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
})

### Close reactOutput
observeEvent(input$scripts_hide_dialog_%widget_id%, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_hide_dialog_%widget_id%"))
    m$delete_open_dialog_%widget_id% <- FALSE
})
observeEvent(input$scripts_delete_canceled_%widget_id%, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_delete_canceled_%widget_id%"))
    m$delete_open_dialog_%widget_id% <- FALSE
})

### Deletion confirmed
observeEvent(input$scripts_delete_confirmed_%widget_id%, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_delete_confirmed_%widget_id%"))
    
    m$delete_open_dialog_%widget_id% <- FALSE
    
    # Get scripts ids
    ids_to_del <- m$delete_scripts_%widget_id%
    
    # Delete scripts in DB
    sql <- glue::glue_sql(paste0("DELETE FROM aggregated_widgets_options WHERE widget_id = %widget_id% AND (",
        "(name = 'script' AND value_num IN ({ids_to_del*})) OR ",
        "(link_id IN ({ids_to_del*})))"), .con = m$db)
    query <- DBI::dbSendStatement(m$db, sql)
    DBI::dbClearResult(query)
    
    # Update m var
    m$scripts_%widget_id% <- m$scripts_%widget_id% %>% dplyr::filter(id %not_in% ids_to_del)
    m$scripts_temp_%widget_id% <- m$scripts_%widget_id% %>% dplyr::mutate(modified = TRUE)
    
    # Reload DT
    m$reload_dt_%widget_id% <- Sys.time()
    
    # Notify user
    show_message_bar(output, "script_deleted", "severeWarning", i18n = i18np, ns = ns)
})
