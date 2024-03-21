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

dropdowns <- c("")
textfields <- c("")
spin_buttons <- c("")
toggle_inputs <- c("")
colour_inputs <- c("")
ace_inputs <- c("code")
inputs <- c(dropdowns, textfields, spin_buttons, toggle_inputs, colour_inputs, ace_inputs)

# List of default values
# Add a default value for each input, with the corresponding name without the suffix "_%widget_id%"
# Example : default_values$num_bins <- 50L

default_values <- list()
###
###
### Default values here
###
###

# Get concepts from selected concepts for this widget
concepts <- selected_concepts %>% dplyr::select(concept_id, concept_name) %>% convert_tibble_to_list(key_col = "concept_id", text_col = "concept_name")

shinyjs::delay(100, shiny.fluent::updateDropdown.shinyInput(session, "concept_%widget_id%", options = concepts, value = 0L))

# -------------------------
# --- Show / hide divs ----
# -------------------------

observeEvent(input$no_code_right_pivot_current_tab_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$no_code_right_pivot_current_tab_%widget_id%"))
    
    tryCatch({
        sapply(c("parameters_tab_%widget_id%", "variables_tab_%widget_id%"), shinyjs::hide)
        shinyjs::show(input$no_code_right_pivot_current_tab_%widget_id%)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Additional buttons for this plugin

style <- "width:25px; padding-left:0; padding-right:0;"
buttons_with_maximize <- tagList(
    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 2),
        actionButton(ns("show_no_code_%widget_id%"), "", icon = icon("eye"), style = style),
        actionButton(ns("show_code_%widget_id%"), "", icon = icon("code"), style = style),
        actionButton(ns("show_scripts_%widget_id%"), "", icon = icon("file-code"), style = style),
        actionButton(ns("maximize_fig_%widget_id%"), "", icon = icon("maximize"), style = style)
    )
)
buttons_with_minimize <- tagList(
    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 2),
        actionButton(ns("show_no_code_%widget_id%"), "", icon = icon("eye"), style = style),
        actionButton(ns("show_code_%widget_id%"), "", icon = icon("code"), style = style),
        actionButton(ns("show_scripts_%widget_id%"), "", icon = icon("file-code"), style = style),
        actionButton(ns("minimize_fig_%widget_id%"), "", icon = icon("minimize"), style = style)
    )
)

output$additional_buttons_%widget_id% <- renderUI({
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer output$additional_buttons_%widget_id%"))

    tryCatch({
        buttons_with_maximize
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$show_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$show_code_%widget_id%"))
    
    tryCatch({
        sapply(c("no_code_tab_%widget_id%", "scripts_management_tab_%widget_id%"), shinyjs::hide)
        sapply(c("no_code_and_code_tab_header_%widget_id%", "code_tab_%widget_id%"), shinyjs::show)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$show_no_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$show_no_code_%widget_id%"))
    
    tryCatch({
        sapply(c("code_tab_%widget_id%", "scripts_management_tab_%widget_id%"), shinyjs::hide)
        sapply(c("no_code_and_code_tab_header_%widget_id%", "no_code_tab_%widget_id%"), shinyjs::show)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$show_scripts_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$show_scripts_%widget_id%"))
    
    tryCatch({
        sapply(c("no_code_tab_%widget_id%", "code_tab_%widget_id%", "no_code_and_code_tab_header_%widget_id%"), shinyjs::hide)
        shinyjs::show("scripts_management_tab_%widget_id%")
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Maximize fig
observeEvent(input$maximize_fig_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$maximize_fig_%widget_id%"))
    
    tryCatch({
        output$additional_buttons_%widget_id% <- renderUI(buttons_with_minimize)
        shinyjs::hide("split_layout_right_%widget_id%")
        shinyjs::runjs(glue::glue("$('#{id}-split_layout_left_%widget_id%').css('width', '100%');"))
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Minimize fig
observeEvent(input$minimize_fig_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$minimize_fig_%widget_id%"))
    
    tryCatch({
        output$additional_buttons_%widget_id% <- renderUI(buttons_with_maximize)
        shinyjs::show("split_layout_right_%widget_id%")
        shinyjs::runjs(glue::glue("$('#{id}-split_layout_left_%widget_id%').css('width', '50%');"))
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# ----------------
# --- No-code ----
# ----------------

# Update start & end datetime dropdowns with min and max for selected patient
observeEvent(m$selected_person, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$selected_person"))
    
    tryCatch({
        min_visit_detail_start_datetime <- d$data_person$measurement %>% dplyr::summarize(minimum = min(measurement_datetime, na.rm = TRUE)) %>% dplyr::pull(minimum)
        max_visit_detail_end_datetime <- d$data_person$measurement %>% dplyr::summarize(maximum = max(measurement_datetime, na.rm = TRUE)) %>% dplyr::pull(maximum)
        
        shiny.fluent::updateDatePicker.shinyInput(session, "start_datetime_%widget_id%", value = min_visit_detail_start_datetime)
        shiny.fluent::updateDatePicker.shinyInput(session, "end_datetime_%widget_id%", value = max_visit_detail_end_datetime)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Execute with no-code parameters
observeEvent(input$execute_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$generate_code_%widget_id%"))
    
    tryCatch({
        req(nrow(d$data_person$measurement) > 0, length(input$variables_%widget_id%) > 0)
        
        start_datetime <- as.POSIXct(input$start_datetime_%widget_id%)
        end_datetime <- as.POSIXct(input$end_datetime_%widget_id%)
        num_intervals <- input$num_of_intervals_%widget_id%
        agg_func <- input$aggregate_function_%widget_id%
        
        time_difference <- end_datetime - start_datetime
        interval <- time_difference / num_intervals
        
        equal_times <- seq(from = start_datetime, by = interval, length.out = num_intervals + 1)
        
        filtered_df <-
            d$data_person$measurement %>%
            dplyr::filter(
                measurement_concept_id %in% input$variables_%widget_id%,
                measurement_datetime >= start_datetime,
                measurement_datetime <= end_datetime
            ) %>%
            dplyr::mutate(time_interval = findInterval(measurement_datetime, equal_times) - 1)
            
        all_intervals_df <- tibble::tibble(time_interval = 0:(num_intervals - 1))

        merged_df <- dplyr::left_join(all_intervals_df, filtered_df, by = "time_interval")
        
#         pivot_table <- 
#             merged_df %>%
#             dplyr::group_by(measurement_concept_id, time_interval) %>%
#             dplyr::summarise(value = round(match.fun(agg_func)(value_as_number, na.rm = TRUE), 2), .groups = "drop") %>%
#             tidyr::pivot_wider(names_from = time_interval, values_from = value, values_fill = list(value = NA))
            
        pivot_table <- 
            merged_df %>%
            dplyr::group_by(measurement_concept_id, time_interval) %>%
            dplyr::summarise(mean_value = round(match.fun(agg_func)(value_as_number, na.rm = TRUE), 2), n = dplyr::n(), .groups = "drop") %>%
            dplyr::mutate(combined = dplyr::case_when(is.na(mean_value) ~ NA_character_, TRUE ~ paste0(mean_value, " (", n, ")"))) %>%
            dplyr::select(measurement_concept_id, time_interval, combined) %>%
            tidyr::pivot_wider(names_from = time_interval, values_from = combined, values_fill = list(combined = NA))
            
        interval_labels <- purrr::map2_chr(equal_times[-length(equal_times)], equal_times[-1], function(x, y) {
            date_x <- format(x, "%Y-%m-%d")
            time_x <- format(x, "%H:%M")
            date_y <- format(y, "%Y-%m-%d")
            time_y <- format(y, "%H:%M")
            paste0("<b>", date_x, "</b> <span style='color:#0078D4;'>", time_x, "</span> <span style='font-weight:normal;'>", i18np$t("to"), "</span> <b>", date_y, "</b> <span style='color:#0078D4;'>", time_y, "</span>")
        })

        names(pivot_table)[-1] <- interval_labels
        
        data <- 
            pivot_table %>%
            dplyr::filter(!is.na(measurement_concept_id)) %>%
            dplyr::left_join(d$dataset_all_concepts %>% dplyr::select(concept_name = concept_name_1, measurement_concept_id = concept_id_1), by = "measurement_concept_id") %>%
            dplyr::relocate(concept_name, .before = "measurement_concept_id") %>%
            dplyr::select(-measurement_concept_id) %>%
            dplyr::rename(!!i18np$t("concept") := concept_name)
        
        data_output <- DT::datatable(data, options = list(dom = "<'datatable_length'l><'top't><'bottom'p>"), rownames = FALSE, escape = FALSE)
        output$datatable_%widget_id% <- DT::renderDT(data_output, server = TRUE)
        
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# ------------
# --- Code ---
# ------------



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
