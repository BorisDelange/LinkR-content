# Update x & y variables dropdowns
concepts <- tibble::tibble(concept_id = 0L, concept_name = i18np$t("none")) %>% dplyr::bind_rows(selected_concepts %>% dplyr::select(concept_id, concept_name))
x_variables <- convert_tibble_to_list(concepts, key_col = "concept_id", text_col = "concept_name")
shiny.fluent::updateDropdown.shinyInput(session, "x_variable_%widget_id%", options = x_variables)
y_variables <- convert_tibble_to_list(concepts, key_col = "concept_id", text_col = "concept_name")
shiny.fluent::updateDropdown.shinyInput(session, "y_variable_%widget_id%", options = y_variables)

# List of inputs (to save & get saved params)

dropdowns <- c("plot_function", "plot_theme", "stat", "bins_type", "x_variable", "y_variable", "colour_pal", "group_by", "summarize_fct")
textfields <- c("x_label", "y_label")
spin_buttons <- c("num_of_bins", "bin_width", "group_by_num")
toggle_inputs <- "group_data"
colour_inputs <- "colour"
inputs <- c(dropdowns, textfields, spin_buttons, toggle_inputs, colour_inputs)

# Render plot
observeEvent(input$show_%widget_id%, {
    %req%
    
    data <- list()
    variable <- list()
    
    data$x <- tibble::tibble()
    variable$x <- tibble::tibble()
    data$y <- tibble::tibble()
    variable$y <- tibble::tibble()
    
    # Get x & y variables in data
    if (length(input$x_variable_%widget_id%) > 0) variable$x <- d$dataset_all_concepts %>% dplyr::filter(concept_id_1 == input$x_variable_%widget_id%)
    if (length(input$y_variable_%widget_id%) > 0) variable$y <- d$dataset_all_concepts %>% dplyr::filter(concept_id_1 == input$y_variable_%widget_id%)
    
    if (nrow(variable$x) > 0){
        variable$x$domain_id <- tolower(variable$x$domain_id)
        if (variable$x$domain_id %in% c("observation", "measurement")) data$x <- d[[tolower(variable$x$domain_id)]] %>% 
            dplyr::filter(get(paste0(tolower(variable$x$domain_id), "_concept_id")) == variable$x$concept_id_1)
    }
    
    if (nrow(variable$y) > 0){
        variable$y$domain_id <- tolower(variable$y$domain_id)
        if (variable$y$domain_id %in% c("observation", "measurement")) data$y <- d[[tolower(variable$y$domain_id)]] %>% 
            dplyr::filter(get(paste0(tolower(variable$y$domain_id), "_concept_id")) == variable$y$concept_id_1)
    }
    
    # Render plot
    output$plot_output_%widget_id% <- renderPlot({
        %req%
    
        figure <- ggplot2::ggplot()
        
        # If we choose to group data
        if (isolate(input$group_data_%widget_id%)){
            for(var in c("x", "y")){
                if (nrow(data[[var]]) > 0){
                
                    # Group by person_id
                    if (isolate(input$group_by_%widget_id% == "person_id")) data[[var]] <- data[[var]] %>% dplyr::group_by(person_id)
                    
                    # Group by datetime
                    if (isolate(input$group_by_%widget_id%) == "datetime"){
                        data[[var]] <- data[[var]]  %>%
                            dplyr::filter(!is.na(value_as_number)) %>%
                            dplyr::mutate(datetime_rounded = lubridate::floor_date(
                                !!rlang::sym(paste0(tolower(variable$x$domain_id), "_datetime")), 
                                unit = paste0(isolate(input$group_by_num_%widget_id%), " ", isolate(input$group_by_type_%widget_id%)))) %>%
                            dplyr::group_by(datetime_rounded)
                    }
                    
                    # Summarize with selected summarize function
                    data[[var]] <- 
                        data[[var]] %>% 
                        dplyr::summarize(value_as_number = match.fun(isolate(input$summarize_fct_%widget_id%))(value_as_number, na.rm = TRUE)) %>%
                        dplyr::ungroup() 
                    
                    # Rename datetime col with original name
                    if (isolate(input$group_by_%widget_id%) == "datetime") data[[var]] <- data[[var]] %>% 
                        dplyr::rename(!!paste0(tolower(variable$x$domain_id), "_datetime") := datetime_rounded)
                }
            }
        }
    
        # HISTOGRAM
        
        if (isolate(input$plot_function_%widget_id%) == "geom_histogram"){
            req(nrow(data$x) > 0)
        
            # Create ggplot2 figure
            figure <- data$x %>% ggplot2::ggplot(ggplot2::aes(x = value_as_number))
            
            # Add params
            if (isolate(input$bins_type_%widget_id%) == "num_of_bins") figure <- figure + 
                ggplot2::geom_histogram(bins = isolate(input$num_of_bins_%widget_id%), fill = isolate(input$colour_%widget_id%), color = "#FFFFFF")
            else if (isolate(input$bins_type_%widget_id%) == "bin_width") figure <- figure + 
                ggplot2::geom_histogram(binwidth = isolate(input$bin_width_%widget_id%), fill = isolate(input$colour_%widget_id%), color = "#FFFFFF")
        }
        
        # SCATTER PLOT
        
        if (isolate(input$plot_function_%widget_id%) == "geom_point"){
            req(nrow(data$x) > 0, nrow(data$y) > 0)
            
            # If data is grouped by person_id
            if (isolate(input$group_data_%widget_id%) & isolate(input$group_by_%widget_id% == "person_id")){
                fig_data <- data$x %>%
                    dplyr::transmute(person_id, variable = "x", value_as_number) %>%
                    dplyr::bind_rows(
                        data$y %>%
                        dplyr::transmute(person_id, variable = "y", value_as_number)
                    ) %>%
                    dplyr::mutate(n = 1:dplyr::n()) %>%
                    tidyr::pivot_wider(names_from = "variable", values_from = "value_as_number") %>%
                    dplyr::select(-n) %>%
                    dplyr::group_by(person_id) %>%
                    dplyr::summarize(x = dplyr::first(na.omit(x)), y = dplyr::first(na.omit(y))) %>%
                    dplyr::ungroup()
            }
            
            # If data is not grouped or grouped by datetime
            else fig_data <- data$x %>%
                dplyr::rename(datetime = paste0(tolower(variable$x$domain_id), "_datetime")) %>%
                dplyr::transmute(datetime, variable = "x", value_as_number) %>%
                dplyr::bind_rows(
                    data$y %>% 
                    dplyr::rename(datetime = paste0(tolower(variable$y$domain_id), "_datetime")) %>%
                    dplyr::transmute(datetime, variable = "y", value_as_number)
                ) %>%
                dplyr::mutate(n = 1:dplyr::n()) %>%
                tidyr::pivot_wider(names_from = "variable", values_from = "value_as_number") %>%
                dplyr::select(-n) %>%
                dplyr::group_by(datetime) %>%
                dplyr::summarize(x = dplyr::first(na.omit(x)), y = dplyr::first(na.omit(y))) %>%
                dplyr::ungroup()
            
            # Create ggplot2 figure
            figure <- fig_data %>% 
                ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
                ggplot2::geom_point(colour = isolate(input$colour_%widget_id%))
        }
        
        # Add theme & labs
        figure <- figure +
            do.call(getFromNamespace(isolate(input$plot_theme_%widget_id%), "ggplot2"), list()) +
            ggplot2::labs(x = isolate(input$x_label_%widget_id%), y = isolate(input$y_label_%widget_id%))
        
        # Final object of ggplot2 figure
        figure
    })
})

# Change SwatchColorPicker
observeEvent(input$colour_pal_%widget_id%, {
    %req%

    pal <- RColorBrewer::brewer.pal(n = 8, name = input$colour_pal_%widget_id%)
    pal_tibble <- tibble::tibble(name = pal)
    colorCells <- list()
    for (i in 1:nrow(pal_tibble)) colorCells <- rlist::list.append(colorCells, list(id = pal_tibble[[i, "name"]], color = pal_tibble[[i, "name"]]))
    
    # Get saved colour
    value <- pal[1]
    sql <- glue::glue_sql("SELECT * FROM aggregated_widgets_options WHERE widget_id = %widget_id% AND name = 'colour'", .con = r$db)
    colour <- DBI::dbGetQuery(m$db, sql)
    if (nrow(colour) > 0) if (colour %>% dplyr::pull(value) %in% pal) value <- colour %>% dplyr::pull(value)
    
    output$colour_ui_%widget_id% <- renderUI({
        %req%
        shiny.fluent::SwatchColorPicker.shinyInput(ns("colour_%widget_id%"), colorCells = colorCells, columnCount = length(colorCells), value = value)
    })
})

# Save widget parameters
observeEvent(input$save_%widget_id%, {
    %req%

    # Delete old options
    sql <- glue::glue_sql("DELETE FROM aggregated_widgets_options WHERE widget_id = {%widget_id%}", .con = m$db)
    DBI::dbSendStatement(m$db, sql) -> query
    DBI::dbClearResult(query)
    
    last_row <- get_last_row(m$db, "aggregated_widgets_options")
    
    # Add new options
    new_options <- tibble::tibble(
        id = seq(last_row + 1, last_row + length(inputs)),
        widget_id = %widget_id%, patient_id = NA_integer_, link_id = NA_integer_,
        category = NA_character_, name = NA_character_, value = NA_character_, value_num = NA_real_,
        creator_id = NA_integer_, datetime = as.character(Sys.time()), deleted = FALSE)
    
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
    
    DBI::dbAppendTable(m$db, "aggregated_widgets_options", new_options)
    
    # Notify user
    show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
})
