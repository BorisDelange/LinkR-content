# Get saved params for this widget
sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id%", .con = m$db)
widget_options <- DBI::dbGetQuery(m$db, sql)

m$widget_options_%widget_id% <- widget_options
m$scripts_%widget_id% <- widget_options %>% dplyr::filter(name == "script") %>% dplyr::select(id = value_num, name = value)
m$scripts_temp_%widget_id% <- m$scripts_%widget_id% %>% dplyr::mutate(modified = FALSE)
m$reload_dt_%widget_id% <- Sys.time()

# List of inputs (to save & get saved params)

dropdowns <- paste(unlist(sapply(c("variable", "colour_pal"), function(x) paste0(x, "_", 1:10))))
textfields <- paste(unlist(sapply("variable_name", function(x) paste0(x, "_", 1:10))))
spin_buttons <- c("y_min", "y_max")
toggle_inputs <- c("show_stays", "stay_data_only", "show_range_selector", "synchronize_timelines", "smooth_curves", "draw_points", "change_y_values")
colour_inputs <- paste(unlist(sapply("colour", function(x) paste0(x, "_", 1:10))))
ace_inputs <- "code"
inputs <- c(dropdowns, textfields, spin_buttons, toggle_inputs, colour_inputs, ace_inputs)

default_values <- list()
default_values$run_code_at_script_launch <- FALSE
default_values$run_plot_at_script_launch <- FALSE
default_values$y_min <- NULL
default_values$y_max <- NULL
default_values$show_stays <- FALSE
default_values$stay_data_only <- FALSE
default_values$show_range_selector <- TRUE
default_values$synchronize_timelines <- FALSE
default_values$smooth_curves <- FALSE
default_values$draw_points <- TRUE
default_values$change_y_values <- FALSE
default_values$code <- ""

for (i in 1:10){
    default_values[[paste0("variable_", i)]] <- 0L
    default_values[[paste0("variable_name_", i)]] <- ""
    default_values[[paste0("colour_", i)]] <- "#E41A1C"
    default_values[[paste0("colour_pal_", i)]] <- "Set1"
}

# -------------------------
# --- Show / hide divs ----
# -------------------------

observeEvent(input$current_tab_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$current_tab_%widget_id%"))
    
    sapply(c("plot_and_code_tab_header_%widget_id%", "plot_tab_%widget_id%", "code_tab_%widget_id%", "scripts_management_tab_%widget_id%"), shinyjs::hide)
    shinyjs::show(input$current_tab_%widget_id%)
    if (input$current_tab_%widget_id% %in% c("plot_tab_%widget_id%", "code_tab_%widget_id%")){
        shinyjs::show("plot_and_code_tab_header_%widget_id%")
        sapply(c("plot_tab_header_%widget_id%", "code_tab_header_%widget_id%"), shinyjs::hide)
        if (input$current_tab_%widget_id% == "plot_tab_%widget_id%") shinyjs::show("plot_tab_header_%widget_id%")
        else if (input$current_tab_%widget_id% == "code_tab_%widget_id%") shinyjs::show("code_tab_header_%widget_id%")
    }
})

observeEvent(input$plot_current_tab_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$plot_current_tab_%widget_id%"))
    
    sapply(c("plot_parameters_tab_%widget_id%", "variables_tab_%widget_id%"), shinyjs::hide)
    shinyjs::show(input$plot_current_tab_%widget_id%)
})

observeEvent(input$variable_num_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$variable_num_%widget_id%"))
    
    sapply(1:10, function(i) shinyjs::hide(paste0("variable_div_", i)))
    shinyjs::show(paste0("variable_div_", input$variable_num_%widget_id%))
})

# -------------
# --- Plot ----
# -------------

# Update x & y variables dropdowns
concepts <- tibble::tibble(concept_id = 0L, concept_name = i18np$t("none")) %>% dplyr::bind_rows(selected_concepts %>% dplyr::select(concept_id, concept_name))
variables <- convert_tibble_to_list(concepts, key_col = "concept_id", text_col = "concept_name")
for (i in 1:10) shiny.fluent::updateDropdown.shinyInput(session, paste0("variable_", i, "_%widget_id%"), options = variables)   

sapply(1:10, function(i){

    # Update SwatchColorPicker
    observeEvent(input[[paste0("colour_pal_", i, "_%widget_id%")]], {
        %req%
        if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$colour_pal_", i, "_%widget_id%"))
    
        pal <- RColorBrewer::brewer.pal(n = 8, name = input[[paste0("colour_pal_", i, "_%widget_id%")]])
        pal_tibble <- tibble::tibble(name = pal)
        colorCells <- list()
        for (j in 1:nrow(pal_tibble)) colorCells <- rlist::list.append(colorCells, list(id = pal_tibble[[j, "name"]], color = pal_tibble[[j, "name"]]))
        
        # Get saved colour
        value <- pal[1]
        if (length(input$script_choice_%widget_id%) > 0){
            sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id% AND link_id = {input$script_choice_%widget_id%} AND name = {paste0('colour_', i)}", .con = m$db)
            colour <- DBI::dbGetQuery(m$db, sql)
            if (nrow(colour) > 0) if (colour %>% dplyr::pull(value) %in% pal) value <- colour %>% dplyr::pull(value)   
        }
        
        output[[paste0("colour_ui_", i, "_%widget_id%")]] <- renderUI({
            %req%
            if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - output$colour_ui_", i, "_%widget_id%"))
            shiny.fluent::SwatchColorPicker.shinyInput(ns(paste0("colour_", i, "_%widget_id%")), colorCells = colorCells, columnCount = length(colorCells), value = value)
        })
    })
    
    # Update variable name
    observeEvent(input[[paste0("variable_", i, "_%widget_id%")]], {
        %req%
        if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$variable_", i, "_%widget_id%"))
        
        concept_id <- input[[paste0("variable_", i, "_%widget_id%")]]
        req(concept_id != 0L, length(concept_id) > 0)
        table_name <- d$dataset_all_concepts %>% dplyr::filter(concept_id_1 == concept_id, is.na(relationship_id)) %>% dplyr::pull(domain_id) %>% tolower()
        
        unit_concept_name <- ""
        
        if ("tbl_lazy" %in% class(d[[table_name]])) unit_concept_id <- d[[table_name]] %>% dplyr::filter(rlang::sym(paste0(table_name, "_concept_id")) == !!concept_id) %>% 
            dplyr::distinct(unit_concept_id) %>% dplyr::collect() %>% dplyr::pull(unit_concept_id)
        
        else unit_concept_id <- d[[table_name]] %>% dplyr::filter(get(paste0(table_name, "_concept_id")) == !!concept_id) %>%
            dplyr::distinct(unit_concept_id) %>% dplyr::pull(unit_concept_id)
            
        if (length(unit_concept_id) > 0) unit_concept_name <- d$dataset_all_concepts %>% dplyr::filter(concept_id_1 %in% unit_concept_id, is.na(relationship_id)) %>% dplyr::slice(1) %>% dplyr::pull(concept_name_1)
        
        concept_name <- d$dataset_all_concepts %>% dplyr::filter(concept_id_1 == concept_id, is.na(relationship_id)) %>% dplyr::pull(concept_name_1)
        if (unit_concept_name != "") concept_name <- paste0(concept_name, " (", unit_concept_name, ")")
        
        shiny.fluent::updateTextField.shinyInput(session, paste0("variable_name_", i, "_%widget_id%"), value = concept_name)
    })
})

# Render plot from "plot" tab
observeEvent(input$show_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$show_%widget_id%"))
    
    # Then load plot
    m$create_plot_type_%widget_id% <- "show_plot"
    m$create_plot_trigger_%widget_id% <- Sys.time()
})

# Generate code and update aceEditor or render plot

observeEvent(m$create_plot_trigger_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer m$create_plot_trigger_%widget_id%"))
    
    req(requireNamespace("dygraphs", quietly = TRUE), requireNamespace("xts", quietly = TRUE), length(input$variable_1_%widget_id%) > 0)
    
    create_plot_type <- isolate(m$create_plot_type_%widget_id%)
    
    # At each step of the code, we put the code in the code variable, for the shinyAce code editor (tab "Code")
    
    req(length(isolate(input$colour_1_%widget_id%)) > 0)
    
    data <- tibble::tibble(concept_id = integer(), datetime = lubridate::ymd_hms(), value_as_number = numeric())
#     code <- "# A tibble containing the data for the plot\ndata <- tibble::tibble()\n"
    code <- ""
    
    # Prevents a bug with filter on a list element (m$selected_person) which doesn't work on lazy tibbles
    selected_person <- m$selected_person
    
    # Keep track of used concept_ids for dygraph plot
    concept_ids <- integer()
    
    for (i in 1:10){
        if (input[[paste0("variable_", i, "_%widget_id%")]] != 0L){
            concept_id <- input[[paste0("variable_", i, "_%widget_id%")]]
            
            if (concept_id %not_in% concept_ids){
        
                variable <- d$dataset_all_concepts %>% dplyr::filter(concept_id_1 == concept_id, is.na(relationship_id))
                
                table_name <- tolower(variable$domain_id)
                
                if (table_name %in% c("observation", "measurement")){
                    concept_name <- variable$concept_name_1
                    
                    data_filtered <- d[[table_name]] %>% dplyr::filter(person_id == selected_person, !!rlang::sym(paste0(table_name, "_concept_id")) == !!concept_id)
                    
                    print(input$stay_data_only_%widget_id%)
                    if (length(input$stay_data_only_%widget_id%) > 0) if (input$stay_data_only_%widget_id% & !is.na(m$selected_visit_detail)){
                    
                        selected_visit_detail_id <- m$selected_visit_detail
                        selected_visit_detail <- d$visit_detail %>% dplyr::filter(visit_detail_id == selected_visit_detail_id)
                    
                        data_filtered <- data_filtered %>% 
                            dplyr::filter(
                                !!rlang::sym(paste0(table_name, "_datetime")) >= selected_visit_detail$visit_detail_start_datetime,
                                !!rlang::sym(paste0(table_name, "_datetime")) <= selected_visit_detail$visit_detail_end_datetime
                            )
                    }
                    
                    data_filtered <- data_filtered %>% dplyr::collect()
                    
                    if (nrow(data_filtered) > 0) data <- data %>% 
                        dplyr::bind_rows(
                            data_filtered %>%
                                dplyr::select(concept_id = paste0(table_name, "_concept_id"), datetime = paste0(table_name, "_datetime"), value_as_number) %>% 
                                dplyr::mutate(concept_name = concept_name)
                        )
                }
                
                concept_ids <- c(concept_ids, concept_id)
            }
        }
    }
    
    if (nrow(data) == 0){
        shinyjs::hide("dygraph_%widget_id%")
        shinyjs::show("empty_dygraph_%widget_id%")
    }
        
    req(nrow(data) > 0)
    
    # Pivot data
    pivoted_data <- data %>% tidyr::pivot_wider(names_from = "concept_name", values_from = "value_as_number", id_cols = "datetime", values_fn = mean)
    pivoted_data <- xts::xts(pivoted_data[,-1], order.by = as.POSIXct(pivoted_data$datetime))
    
    # Create dygraph
    result <- pivoted_data %>% dygraphs::dygraph() %>% dygraphs::dyOptions(useDataTimezone = TRUE)
    
    concept_ids <- integer()
    
    for (i in 1:10){
        concept_id <- input[[paste0("variable_", i, "_%widget_id%")]]
        
        if (concept_id %not_in% concept_ids){
            if (concept_id != 0){
                concept_name <- d$dataset_all_concepts %>% dplyr::filter(concept_id_1 == concept_id, is.na(relationship_id)) %>% dplyr::pull(concept_name_1)
                
                draw_points <- FALSE
                if (length(input$draw_points_%widget_id%) > 0) if(input$draw_points_%widget_id%) draw_points <- TRUE
                
                result <- result %>% 
                    dygraphs::dySeries(concept_name, label = input[[paste0("variable_name_", i, "_%widget_id%")]], color = input[[paste0("colour_", i, "_%widget_id%")]], drawPoints = draw_points)
            }
            
            concept_ids <- c(concept_ids, concept_id)
        }
    }
    
    # Show patient stays
    if (length(input$show_stays_%widget_id%) > 0) if(input$show_stays_%widget_id%){
        
        visit_details <-
            d$visit_detail %>% 
            dplyr::filter(person_id == selected_person) %>%
            dplyr::collect() %>%
            dplyr::left_join(d$dataset_all_concepts %>% dplyr::select(visit_detail_concept_id = concept_id_1, visit_detail_concept_name = concept_name_1), by = "visit_detail_concept_id")
            
        visit_details <-
            visit_details %>%
            dplyr::distinct(visit_detail_start_datetime) %>%
            dplyr::rename(datetime = visit_detail_start_datetime) %>%
            dplyr::left_join(visit_details %>% dplyr::select(unit_in = visit_detail_concept_name, datetime = visit_detail_start_datetime), by = "datetime") %>%
            dplyr::left_join(visit_details %>% dplyr::select(unit_out = visit_detail_concept_name, datetime = visit_detail_end_datetime), by = "datetime") %>%
            dplyr::mutate(text = dplyr::case_when(
                !is.na(unit_out) ~ paste0(unit_out, " - ", unit_in),
                TRUE ~ unit_in
            )) %>%
            dplyr::mutate(text = ifelse(is.na(text), "", text))
            
        for (i in 1:nrow(visit_details)){
            row <- visit_details[i, ]
            result <- result %>% dygraphs::dyEvent(row$datetime, row$text)
        }
    }
    
    # Add range selector
    if (length(input$show_range_selector_%widget_id% > 0)) if(input$show_range_selector_%widget_id% > 0) result <- result %>% dygraphs::dyRangeSelector()
    
    # Smooth curves
    if (length(input$smooth_curves_%widget_id%) > 0) if (input$smooth_curves_%widget_id%) result <- result %>% dygraphs::dyRoller(rollPeriod = 5)
    
    # Y range values
    if (length(input$change_y_values_%widget_id%) > 0 & length(input$y_min_%widget_id%) > 0 & length(input$y_max_%widget_id%)){
        if (input$change_y_values_%widget_id%) result <- result %>% dygraphs::dyAxis("y", valueRange = c(input$y_min_%widget_id%, input$y_max_%widget_id%))
    }
    
    # Update shinyAce code editor
    if (create_plot_type == "generate_code"){
        shinyAce::updateAceEditor(session, "code_%widget_id%", value = code)
        
        # Go to "Code" tab
        shinyjs::runjs(glue::glue("$('#{id}-pivot_%widget_id% button[name=\"{i18np$t('code')}\"]').click();"))
    }
    
    # Final object of dygraph figure
    
    shinyjs::show("dygraph_%widget_id%")
    shinyjs::hide("empty_dygraph_%widget_id%")
    
    if (create_plot_type == "show_plot") output$dygraph_%widget_id% <- dygraphs::renderDygraph({
        %req%
        if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - output$dygraph_%widget_id%"))
        
        result
#         eval(parse(text = code))
    })
})

# Save widget parameters
observeEvent(input$save_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$save_%widget_id%"))
    
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
        creator_id = NA_integer_, datetime = as.character(Sys.time()), deleted = FALSE)
    
    new_options_values <- tibble::tibble(name = character(), value = character(), value_num = numeric())
    
    for (input_name in inputs){
        if (input_name %in% spin_buttons || grepl("variable_[0-9]", input_name)){
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
})

# Hide parameters div
# observeEvent(input$hide_params_%widget_id%, {
#     %req%
#     if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$hide_params_%widget_id%"))
# 
#     if (input$hide_params_%widget_id%){
#         shinyjs::hide("split_layout_right_%widget_id%")
#         shinyjs::delay(100, shinyjs::runjs(glue::glue("$('#{id}-split_layout_left_%widget_id%').css('width', '100%');")))
#     }
#     else {
#         shinyjs::runjs(glue::glue("$('#{id}-split_layout_left_%widget_id%').css('width', '50%');"))
#         shinyjs::delay(100, shinyjs::show("split_layout_right_%widget_id%"))
#     }
# })

# Plot width
# observeEvent(input$plot_width_%widget_id%, {
#     %req%
#     if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$plot_width_%widget_id%"))
#     shinyjs::runjs(glue::glue("$('#{id}-plot_div_%widget_id%').css('width', '{isolate(input$plot_width_%widget_id%)}%');")) %>% throttle(1000)
# })

# Run plot / code at script launch
observeEvent(input$run_plot_at_script_launch_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_plot_at_script_launch_%widget_id%"))
    if (input$run_plot_at_script_launch_%widget_id%) shiny.fluent::updateToggle.shinyInput(session, "run_code_at_script_launch_%widget_id%", value = FALSE)
})
observeEvent(input$run_code_at_script_launch_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_code_at_script_launch_%widget_id%"))
    if (input$run_code_at_script_launch_%widget_id%) shiny.fluent::updateToggle.shinyInput(session, "run_plot_at_script_launch_%widget_id%", value = FALSE)
})

# ------------
# --- Code ---
# ------------

# Generate code
observeEvent(input$generate_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$generate_code_%widget_id%"))
    m$create_plot_type_%widget_id% <- "generate_code"
    m$create_plot_trigger_%widget_id% <- Sys.time()
})

# Render plot from "code" tab

observeEvent(input$run_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_code_%widget_id%"))
    m$run_code_%widget_id% <- input$code_%widget_id%
    m$run_code_trigger_%widget_id% <- Sys.time()
})

observeEvent(m$run_code_trigger_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer m$run_code_trigger_%widget_id%"))
    
    # Go to plot tab
    shinyjs::runjs(glue::glue("$('#{id}-pivot_%widget_id% button[name=\"{i18np$t('plot')}\"]').click();"))
    
    # Render plot
    output$plot_output_%widget_id% <- renderPlot({
        %req%
        if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - output$plot_output_%widget_id%"))
        eval(parse(text = m$run_code_%widget_id%))
    })
})

# ---------------
# --- Scripts ---
# ---------------

# Add a new script
observeEvent(input$add_script_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$add_script_%widget_id%"))
    
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
    
    new_options <- tibble::tibble(
        id = last_row + 1, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
        category = NA_character_, name = "script", value = input$script_name_%widget_id%, value_num = last_id + 1,
        creator_id = NA_integer_, datetime = as.character(Sys.time()), deleted = FALSE)
        
    DBI::dbAppendTable(m$db, "widgets_options", new_options)
    
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
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$script_choice_%widget_id%"))
    
    widget_options <- m$widget_options_%widget_id% %>% dplyr::filter(link_id == input$script_choice_%widget_id%)
    
    run_code_at_script_launch <- FALSE
    run_plot_at_script_launch <- FALSE
    code <- ""
    
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
#     
    # Render this plot
    if (run_code_at_script_launch){
        m$run_code_%widget_id% <- code
        m$run_code_trigger_%widget_id% <- Sys.time()
    }
    else if (run_plot_at_script_launch) shinyjs::delay(500, shinyjs::click("show_%widget_id%"))
    
    # Save that this script is selected
    sql <- glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND name = 'selected_script'", .con = m$db)
    query <- DBI::dbSendStatement(m$db, sql)
    DBI::dbClearResult(query)
    
    last_row <- get_last_row(m$db, "widgets_options")
    
    new_options <- tibble::tibble(
        id = last_row + 1, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
        category = NA_character_, name = "selected_script", value = NA_character_, value_num = input$script_choice_%widget_id%,
        creator_id = NA_integer_, datetime = as.character(Sys.time()), deleted = FALSE)
        
    DBI::dbAppendTable(m$db, "widgets_options", new_options)
})

# Var for delete confirm react
m$delete_open_dialog_%widget_id% <- FALSE

# Update scripts DT & dropdown
observeEvent(m$reload_dt_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer m$reload_dt_%widget_id%"))
    
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
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_management_datatable_%widget_id%_cell_edit"))
    
    edit_info <- input$scripts_management_datatable_%widget_id%_cell_edit
    m$scripts_temp_%widget_id% <- DT::editData(m$scripts_temp_%widget_id%, edit_info, rownames = FALSE)
      
    # Store that this row has been modified
    m$scripts_temp_%widget_id%[[edit_info$row, "modified"]] <- TRUE
})

# Save updates on scripts
observeEvent(input$save_scripts_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$save_scripts_%widget_id%"))
    
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
        creator_id = NA_integer_, datetime = as.character(Sys.time()), deleted = FALSE)
        
    DBI::dbAppendTable(m$db, "widgets_options", new_options)
    
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
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$deleted_pressed_%widget_id%"))
    
    # Reload datatable (to unselect rows)
    DT::replaceData(m$datatable_proxy_%widget_id%, m$scripts_datatable_temp_%widget_id%, resetPaging = FALSE, rownames = FALSE)
    
    m$delete_scripts_%widget_id% <- as.integer(substr(input$deleted_pressed_%widget_id%, nchar("delete_") + 1, 100))
    m$delete_open_dialog_%widget_id% <- TRUE
})

### Delete with "delete selection" button
observeEvent(input$delete_scripts_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$delete_scripts_%widget_id%"))
    
    req(length(input$scripts_management_datatable_%widget_id%_rows_selected) > 0)
    m$delete_scripts_%widget_id% <- m$scripts_%widget_id%[input$scripts_management_datatable_%widget_id%_rows_selected, ] %>% dplyr::pull(id)
    m$delete_open_dialog_%widget_id% <- TRUE
})

### reactOutput for deletion confirmation
output$delete_confirm_%widget_id% <- shiny.fluent::renderReact({
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - output$delete_confirm_%widget_id%"))
    
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
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_hide_dialog_%widget_id%"))
    m$delete_open_dialog_%widget_id% <- FALSE
})
observeEvent(input$scripts_delete_canceled_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_delete_canceled_%widget_id%"))
    m$delete_open_dialog_%widget_id% <- FALSE
})

### Deletion confirmed
observeEvent(input$scripts_delete_confirmed_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_delete_confirmed_%widget_id%"))
    
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
    m$reload_dt_%widget_id% <- Sys.time()
    
    # Notify user
    show_message_bar(output, "script_deleted", "severeWarning", i18n = i18np, ns = ns)
})
