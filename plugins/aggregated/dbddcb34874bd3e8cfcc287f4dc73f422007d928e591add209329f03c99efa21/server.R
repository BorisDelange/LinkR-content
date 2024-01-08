# Get saved params for this widget
sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id%", .con = r$db)
widget_options <- DBI::dbGetQuery(m$db, sql)

m$widget_options_%widget_id% <- widget_options
m$scripts_%widget_id% <- widget_options %>% dplyr::filter(name == "script") %>% dplyr::select(id = value_num, name = value)
m$scripts_temp_%widget_id% <- m$scripts_%widget_id% %>% dplyr::mutate(modified = FALSE)
m$reload_dt_%widget_id% <- Sys.time()

# List of inputs (to save & get saved params)

dropdowns <- c("plot_function", "plot_theme", "bins_type", "x_variable", "y_variable", "colour_pal", "group_by", "group_by_type", "summarize_fct")
textfields <- c("x_label", "y_label")
spin_buttons <- c("num_of_bins", "bin_width", "group_by_num")
toggle_inputs <- c("group_data", "run_code_at_script_launch", "run_plot_at_script_launch")
colour_inputs <- "colour"
ace_inputs <- "code"
inputs <- c(dropdowns, textfields, spin_buttons, toggle_inputs, colour_inputs, ace_inputs)

default_values <- list()
default_values$plot_function <- "geom_histogram"
default_values$plot_theme <- "theme_minimal"
default_values$bins_type <- "num_of_bins"
default_values$x_variable <- 0L
default_values$y_variable <- 0L
default_values$colour_pal <- "Set1"
default_values$group_by <- "datetime"
default_values$group_by_type <- "hours"
default_values$summarize_fct <- "mean"
default_values$x_label <- ""
default_values$y_label <- ""
default_values$num_of_bins <- 50L
default_values$bin_width <- 10L
default_values$group_by_num <- 4L
default_values$group_data <- FALSE
default_values$colour <- "#E41A1C"
default_values$run_code_at_script_launch <- FALSE
default_values$run_plot_at_script_launch <- FALSE
default_values$code <- ""

# In case of widget settings, update dropdowns of variables
concepts <- tibble::tibble(concept_id = 0L, concept_name = i18np$t("none")) %>% dplyr::bind_rows(selected_concepts %>% dplyr::select(concept_id, concept_name))
variables <- convert_tibble_to_list(concepts, key_col = "concept_id", text_col = "concept_name")

shinyjs::delay(100, {
    for (i in c("x", "y")){
        if (length(input[[paste0(i, "_variable_%widget_id%")]]) > 0){
            value <- 0L
            if (input[[paste0(i, "_variable_%widget_id%")]] %in% selected_concepts$concept_id) value <- input[[paste0(i, "_variable_%widget_id%")]]
            shiny.fluent::updateDropdown.shinyInput(session, paste0(i, "_variable_%widget_id%"), options = variables, value = value)
        }
    }
})

# -------------------------
# --- Show / hide divs ----
# -------------------------

observeEvent(input$current_tab_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$current_tab_%widget_id%"))
    
    sapply(c("plot_and_code_tab_header_%widget_id%", "plot_tab_%widget_id%", "code_tab_%widget_id%", "scripts_management_tab_%widget_id%"), shinyjs::hide)
    shinyjs::show(input$current_tab_%widget_id%)
    if (input$current_tab_%widget_id% %in% c("plot_tab_%widget_id%", "code_tab_%widget_id%")){
        sapply(c("toggle_run_plot_div_%widget_id%", "toggle_run_code_div_%widget_id%"), shinyjs::hide)
        sapply(c(paste0("toggle_run_", input$current_tab_%widget_id%), "plot_and_code_tab_header_%widget_id%"), shinyjs::show)
    }
})

observeEvent(input$plot_current_tab_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$plot_current_tab_%widget_id%"))
    
    sapply(c("plot_parameters_div_%widget_id%", "variables_div_%widget_id%"), shinyjs::hide)
    shinyjs::show(input$plot_current_tab_%widget_id%)
})

observeEvent(input$group_data_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$group_data_%widget_id%"))
    
    if (input$group_data_%widget_id%) shinyjs::show("group_data_div_%widget_id%")
    else shinyjs::hide("group_data_div_%widget_id%")
})

observeEvent(input$group_by_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$group_by_%widget_id%"))
    
    if (input$group_by_%widget_id% == "datetime") shinyjs::show("group_by_datetime_div_%widget_id%")
    else shinyjs::hide("group_by_datetime_div_%widget_id%")
})

observeEvent(input$plot_function_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$plot_function_%widget_id%"))
    
    sapply(c("y_variable_div_%widget_id%", "plot_function_geom_histogram_div_%widget_id%"), shinyjs::hide)
    
    if (input$plot_function_%widget_id% == "geom_point") shinyjs::show("y_variable_div_%widget_id%")
    if (input$plot_function_%widget_id% == "geom_histogram") shinyjs::show("plot_function_geom_histogram_div_%widget_id%")
})

observeEvent(input$plot_function_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$plot_function_%widget_id% (only once)"))
    shinyjs::delay(500, shinyjs::hide("variables_div_%widget_id%"))
}, once = TRUE)

observeEvent(input$bins_type_%widget_id%, {
     %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$bins_type_%widget_id%"))
    
    sapply(c("num_of_bins_div_%widget_id%", "bin_width_div_%widget_id%"), shinyjs::hide)
    shinyjs::show(paste0(input$bins_type_%widget_id%, "_div_%widget_id%"))
})

# -------------
# --- Plot ----
# -------------

# Update x & y variables dropdowns
concepts <- tibble::tibble(concept_id = 0L, concept_name = i18np$t("none")) %>% dplyr::bind_rows(selected_concepts %>% dplyr::select(concept_id, concept_name))
x_variables <- convert_tibble_to_list(concepts, key_col = "concept_id", text_col = "concept_name")
shiny.fluent::updateDropdown.shinyInput(session, "x_variable_%widget_id%", options = x_variables)
y_variables <- convert_tibble_to_list(concepts, key_col = "concept_id", text_col = "concept_name")
shiny.fluent::updateDropdown.shinyInput(session, "y_variable_%widget_id%", options = y_variables)

# Render plot from "plot" tab
observeEvent(input$show_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$show_%widget_id%"))
    
    # Blank plot
    output$plot_output_%widget_id% <- renderPlot({
        %req%
        if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - output$plot_output_%widget_id%"))
        ggplot2::ggplot()
    })
    
    # Then load plot
    m$create_plot_type_%widget_id% <- "show_plot"
    m$create_plot_trigger_%widget_id% <- Sys.time()
})

# Generate code and update aceEditor or render plot
observeEvent(m$create_plot_trigger_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer m$create_plot_trigger_%widget_id%"))
    
    create_plot_type <- isolate(m$create_plot_type_%widget_id%)
    
    # At each step of the code, we put the code in the code variable, for the shinyAce code editor (tab "Code")
    
    req(length(isolate(input$colour_%widget_id%)) > 0)
    
    data <- list()
    code <- "# A list containing the data for the plot\ndata <- list()\n"
    variable <- list()
    
    data$x <- tibble::tibble()
    variable$x <- tibble::tibble()
    data$y <- tibble::tibble()
    variable$y <- tibble::tibble()
    
    # Get x & y variables in data
    # For now, we do not include merged concepts
    if (length(input$x_variable_%widget_id%) > 0) variable$x <- d$dataset_all_concepts %>% dplyr::filter(concept_id_1 == input$x_variable_%widget_id%, is.na(relationship_id))
    if (length(input$y_variable_%widget_id%) > 0) variable$y <- d$dataset_all_concepts %>% dplyr::filter(concept_id_1 == input$y_variable_%widget_id%, is.na(relationship_id))
 
    code <- paste0(code, "\n# Filter data")
    sapply(c("x", "y"), function(var_name){
        if (nrow(variable[[var_name]]) > 0){
            variable[[var_name]]$domain_id <<- tolower(variable[[var_name]]$domain_id)
            if (variable[[var_name]]$domain_id %in% c("observation", "measurement")){
            
                table_name <- tolower(variable[[var_name]]$domain_id)
                concept_id <- variable[[var_name]]$concept_id_1
                
                if ("tbl_lazy" %in% class(d[[table_name]])) data[[var_name]] <<- d[[table_name]] %>% dplyr::filter(rlang::sym(paste0(table_name, "_concept_id")) == !!concept_id) %>% dplyr::collect()
                else data[[var_name]] <<- d[[table_name]] %>% dplyr::filter(get(paste0(table_name, "_concept_id")) == !!concept_id)
                
                code <<- glue::glue(
                    "{code}\n",
                    "data${var_name} <- d${tolower(variable[[var_name]]$domain_id)} %>% ",
                    "dplyr::filter({paste0(tolower(variable[[var_name]]$domain_id), '_concept_id')} == {variable[[var_name]]$concept_id_1}) %>% ",
                    "dplyr::collect()"
                )
            }
        }
    })
        
    # If we choose to group data
    if (isolate(input$group_data_%widget_id%)){
        for(var in c("x", "y")){
            if (nrow(data[[var]]) > 0){
            
                code <- glue::glue(code, "\n\n# Group {var} data")
            
                # Group by person_id
                if (isolate(input$group_by_%widget_id% == "person_id")){
                    data[[var]] <- data[[var]] %>% dplyr::group_by(person_id)
                    
                    code <- glue::glue("{code}\ndata${var} <- data${var} %>% dplyr::group_by(person_id)")
                }
                
                # Group by datetime
                if (isolate(input$group_by_%widget_id%) == "datetime"){
                    data[[var]] <- data[[var]] %>%
                        dplyr::filter(!is.na(value_as_number)) %>%
                        dplyr::mutate(datetime_rounded = lubridate::floor_date(
                            !!rlang::sym(paste0(tolower(variable$x$domain_id), "_datetime")), 
                            unit = paste0(isolate(input$group_by_num_%widget_id%), " ", isolate(input$group_by_type_%widget_id%)))) %>%
                        dplyr::group_by(datetime_rounded)
                        
                    code <- glue::glue(
                        "{code}\n",
                        "data${var} <-\n",
                        "    data${var} %>%\n",
                        "    dplyr::filter(!is.na(value_as_number)) %>%\n",
                        "    dplyr::mutate(datetime_rounded = lubridate::floor_date(",
                        "{paste0(tolower(variable$x$domain_id), '_datetime')}, ",
                        "unit = \"{paste0(isolate(input$group_by_num_%widget_id%), ' ', isolate(input$group_by_type_%widget_id%))}\")) %>%\n",
                        "    dplyr::group_by(datetime_rounded)"
                    )

                }
                
                # Summarize with selected summarize function
                data[[var]] <-
                    data[[var]] %>% 
                    dplyr::summarize(value_as_number = match.fun(isolate(input$summarize_fct_%widget_id%))(value_as_number, na.rm = TRUE)) %>%
                    dplyr::ungroup()
                    
                code <- paste0(
                    code, " %>%\n",
                    "    ", glue::glue("dplyr::summarize(value_as_number = match.fun(\"{isolate(input$summarize_fct_%widget_id%)}\")(value_as_number, na.rm = TRUE)) %>%\n"),
                    "    ", glue::glue("dplyr::ungroup()")
                )
                
                # Rename datetime col with original name
                if (isolate(input$group_by_%widget_id%) == "datetime"){
                    data[[var]] <- 
                        data[[var]] %>% 
                        dplyr::rename(!!paste0(tolower(variable$x$domain_id), "_datetime") := datetime_rounded)
                    
                    code <- paste0(
                        code, " %>%\n",
                        "    ", glue::glue("dplyr::rename({paste0(tolower(variable$x$domain_id), '_datetime')} := datetime_rounded)")
                    )
                }
            }
        }
    }

    # HISTOGRAM
    
    if (isolate(input$plot_function_%widget_id%) == "geom_histogram"){
        req(nrow(data$x) > 0)
        
        # Create ggplot2 figure
        code <- glue::glue("{code}\n\n# Create ggplot2 plot\n", "data$x %>%\n    ggplot2::ggplot(ggplot2::aes(x = value_as_number))")
        
        # Add params
        if (isolate(input$bins_type_%widget_id%) == "num_of_bins") code <- paste0(
            code, " +\n    ",
            glue::glue("ggplot2::geom_histogram(bins = {isolate(input$num_of_bins_%widget_id%)}, fill = \"{isolate(input$colour_%widget_id%)}\", color = \"#FFFFFF\")")
        )
            
        else if (isolate(input$bins_type_%widget_id%) == "bin_width") code <- paste0(
            code, " +\n    ",
            glue::glue("ggplot2::geom_histogram(binwidth = {isolate(input$bin_width_%widget_id%)}, fill = \"{isolate(input$colour_%widget_id%)}\", color = \"#FFFFFF\")")
        )
    }
    
    # SCATTER PLOT
    
    if (isolate(input$plot_function_%widget_id%) == "geom_point"){
        req(nrow(data$x) > 0, nrow(data$y) > 0)
        
        code <- paste0(code, "\n\n# Prepare data for ggplot2 plot")
        
        # If data is grouped by person_id
        if (isolate(input$group_data_%widget_id%) & isolate(input$group_by_%widget_id% == "person_id")) code <- paste0(
            code, "\n",
            "fig_data <-\n",
            "    data$x %>%\n",
            "    dplyr::transmute(person_id, variable = \"x\", value_as_number) %>%\n",
            "    dplyr::bind_rows(\n",
            "        data$y %>%\n",
            "        dplyr::transmute(person_id, variable = \"y\", value_as_number)\n",
            "    ) %>%\n",
            "    dplyr::mutate(n = 1:dplyr::n()) %>%\n",
            "    tidyr::pivot_wider(names_from = \"variable\", values_from = \"value_as_number\") %>%\n",
            "    dplyr::select(-n) %>%\n",
            "    dplyr::group_by(person_id) %>%\n",
            "    dplyr::summarize(x = dplyr::first(na.omit(x)), y = dplyr::first(na.omit(y))) %>%\n",
            "    dplyr::ungroup()"
        )
        
        # If data is not grouped or grouped by datetime
        else code <- glue::glue(
            "{code}\n",
            "fig_data <-\n",
            "    data$x %>%\n",
            "    dplyr::rename(datetime = {paste0(tolower(variable$x$domain_id), '_datetime')}) %>%\n",
            "    dplyr::transmute(datetime, variable = \"x\", value_as_number) %>%\n",
            "    dplyr::bind_rows(\n",
            "        data$y %>%\n",
            "        dplyr::rename(datetime = {paste0(tolower(variable$y$domain_id), '_datetime')}) %>%\n",
            "        dplyr::transmute(datetime, variable = \"y\", value_as_number)\n",
            "    ) %>%\n",
            "    dplyr::mutate(n = 1:dplyr::n()) %>%\n",
            "    tidyr::pivot_wider(names_from = \"variable\", values_from = \"value_as_number\") %>%\n",
            "    dplyr::select(-n) %>%\n",
            "    dplyr::group_by(datetime) %>%\n",
            "    dplyr::summarize(x = dplyr::first(na.omit(x)), y = dplyr::first(na.omit(y))) %>%\n",
            "    dplyr::ungroup()"
        )
        
        # Create ggplot2 figure
        code <- paste0(
            code, "\n\n# Create ggplot2 plot\n",
            "fig_data %>%\n",
            "    ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +\n",
            glue::glue("    ggplot2::geom_point(colour = \"{isolate(input$colour_%widget_id%)}\")")
        )
    }
    
    # Add theme & labs
    code <- paste0(
        code, " +\n    ",
        glue::glue("ggplot2::{isolate(input$plot_theme_%widget_id%)}() +"), "\n    ",
        glue::glue("ggplot2::labs(x = \"{isolate(input$x_label_%widget_id%)}\", y = \"{isolate(input$y_label_%widget_id%)}\")")
    )
    
    # Update shinyAce code editor
    if (create_plot_type == "generate_code"){
        shinyAce::updateAceEditor(session, "code_%widget_id%", value = code)
        
        # Go to "Code" tab
        shinyjs::runjs(glue::glue("$('#{id}-pivot_%widget_id% button[name=\"{i18np$t('code')}\"]').click();"))
    }
    
    # Final object of ggplot2 figure
    if (create_plot_type == "show_plot") output$plot_output_%widget_id% <- renderPlot({
        %req%
        if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - output$plot_output_%widget_id%"))
        eval(parse(text = code))
    })
})

# Change SwatchColorPicker
observeEvent(input$colour_pal_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$colour_pal_%widget_id%"))

    pal <- RColorBrewer::brewer.pal(n = 8, name = input$colour_pal_%widget_id%)
    pal_tibble <- tibble::tibble(name = pal)
    colorCells <- list()
    for (i in 1:nrow(pal_tibble)) colorCells <- rlist::list.append(colorCells, list(id = pal_tibble[[i, "name"]], color = pal_tibble[[i, "name"]]))
    
    # Get saved colour
    value <- pal[1]
    if (length(input$script_choice_%widget_id%) > 0){
        sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id% AND link_id = {input$script_choice_%widget_id%} AND name = 'colour'", .con = r$db)
        colour <- DBI::dbGetQuery(m$db, sql)
        if (nrow(colour) > 0) if (colour %>% dplyr::pull(value) %in% pal) value <- colour %>% dplyr::pull(value)   
    }
    
    output$colour_ui_%widget_id% <- renderUI({
        %req%
        if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - output$colour_ui_%widget_id%"))
        shiny.fluent::SwatchColorPicker.shinyInput(ns("colour_%widget_id%"), colorCells = colorCells, columnCount = length(colorCells), value = value)
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
        if (input_name %in% spin_buttons || input_name %in% c("x_variable", "y_variable")){
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
observeEvent(input$hide_params_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$hide_params_%widget_id%"))

    if (input$hide_params_%widget_id%){
        shinyjs::hide("split_layout_right_%widget_id%")
        shinyjs::delay(100, shinyjs::runjs(glue::glue("$('#{id}-split_layout_left_%widget_id%').css('width', '100%');")))
    }
    else {
        shinyjs::runjs(glue::glue("$('#{id}-split_layout_left_%widget_id%').css('width', '50%');"))
        shinyjs::delay(100, shinyjs::show("split_layout_right_%widget_id%"))
    }
})

# Plot width
observeEvent(input$plot_width_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$plot_width_%widget_id%"))
    
    shinyjs::delay(100, shinyjs::runjs(glue::glue("$('#{id}-plot_output_%widget_id%').css('width', '{isolate(input$plot_width_%widget_id%)}%');")))
    
    m$create_plot_trigger_%widget_id% <- Sys.time()
}) %>% throttle(1000)

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
        m$create_plot_type_%widget_id% <- "generate_code"
    }
    else if (run_plot_at_script_launch){
        m$create_plot_type_%widget_id% <- "show_plot"
        shinyjs::delay(500, shinyjs::click("show_%widget_id%"))
    }
    
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
    show_message_bar(output, "script_deleted", "warning", i18n = i18np, ns = ns)
})
