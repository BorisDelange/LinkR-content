# ==========================================
# server_code_plotly.R - Plotly Specific Logic
# ==========================================
# 
# Handles plotly specific functionality including:
# - Code generation for all OMOP data types (Measurement, Observation, Drug, Procedure, Condition)
# - Timeline visualization with segments and markers
# - Plotly chart configuration and interactivity
# - Timeline synchronization for plotly
#
# ==========================================

# ======================================
# PLOTLY CODE GENERATION
# ======================================

# Generate plotly specific figure code
generate_plotly_figure_code <- function(data_source, concepts, synchronize_timelines) {
    
    # Initialize code blocks list
    code <- list()
    
    # Build concepts table
    code$block_1 <- generate_concepts_block(concepts)
    
    # Initialize data structures for plotly
    code$block_2 <- paste0(
        "data <- tibble::tibble()\n",
        "data_datetimes_range <- c()"
    )
    
    # Build SQL query for all OMOP tables
    code$block_3 <- generate_plotly_sql_block(data_source)
    
    # Date range processing
    code$block_4 <- generate_date_range_block(data_source, synchronize_timelines)
    
    # Data processing for plotly timeline
    code$block_5 <- generate_plotly_data_processing_block()
    
    # Chart generation
    code$block_6 <- generate_plotly_chart_block(synchronize_timelines)
    
    # Combine all blocks into final code
    final_code <- paste(code, collapse = "\n\n")
    
    return(final_code)
}

# Generate SQL query for plotly (all OMOP data types)
generate_plotly_sql_block <- function(data_source) {
    
    # Define OMOP version handling for procedure columns
    procedure_sql_cols <- paste0(
        "if (m$omop_version == \"5.3\") procedure_sql_cols <- DBI::SQL(\"\n",
        "    'procedure' AS source_table,\n",
        "    procedure_occurrence_id AS source_id,\n",
        "    person_id,\n",
        "    visit_detail_id,\n",
        "    CASE WHEN procedure_concept_id > 0 THEN procedure_concept_id ELSE procedure_source_concept_id END AS data_concept_id,\n",
        "    procedure_datetime AS datetime,\n",
        "    procedure_datetime AS end_datetime,\n",
        "    procedure_type_concept_id AS type_concept_id,\n",
        "    quantity,\n",
        "    NULL AS value_as_number,\n",
        "    NULL AS value_as_string,\n",
        "    NULL AS value_as_concept_id\n",
        "\") else procedure_sql_cols <- DBI::SQL(\"\n",
        "    'procedure' AS source_table,\n",
        "    procedure_occurrence_id AS source_id,\n",
        "    person_id,\n",
        "    visit_detail_id,\n",
        "    CASE WHEN procedure_concept_id > 0 THEN procedure_concept_id ELSE procedure_source_concept_id END AS data_concept_id,\n",
        "    procedure_datetime AS datetime,\n",
        "    procedure_end_datetime AS end_datetime,\n",
        "    procedure_type_concept_id AS type_concept_id,\n",
        "    quantity,\n",
        "    NULL AS value_as_number,\n",
        "    NULL AS value_as_string,\n",
        "    NULL AS value_as_concept_id\n",
        "\")"
    )
    
    # Define column mappings for different table types
    condition_sql_cols <- paste0(
        "condition_sql_cols <- DBI::SQL(\"\n",
        "    'condition' AS source_table,\n",
        "    condition_occurrence_id AS source_id,\n",
        "    person_id,\n",
        "    visit_detail_id,\n",
        "    CASE WHEN condition_concept_id > 0 THEN condition_concept_id ELSE condition_source_concept_id END AS data_concept_id,\n",
        "    condition_start_datetime AS datetime,\n",
        "    condition_end_datetime AS end_datetime,\n",
        "    condition_type_concept_id AS type_concept_id,\n",
        "    NULL AS quantity,\n",
        "    NULL AS value_as_number,\n",
        "    NULL AS value_as_string,\n",
        "    NULL AS value_as_concept_id\n",
        "\")"
    )
    
    observation_sql_cols <- paste0(
        "observation_sql_cols <- DBI::SQL(\"\n",
        "    'observation' AS source_table,\n",
        "    observation_id AS source_id,\n",
        "    person_id,\n",
        "    visit_detail_id,\n",
        "    CASE WHEN observation_concept_id > 0 THEN observation_concept_id ELSE observation_source_concept_id END AS data_concept_id,\n",
        "    observation_datetime AS datetime,\n",
        "    observation_datetime AS end_datetime,\n",
        "    observation_type_concept_id AS type_concept_id,\n",
        "    NULL AS quantity,\n",
        "    value_as_number,\n",
        "    value_as_string,\n",
        "    value_as_concept_id\n",
        "\")"
    )
    
    measurement_sql_cols <- paste0(
        "measurement_sql_cols <- DBI::SQL(\"\n",
        "    'measurement' AS source_table,\n",
        "    measurement_id AS source_id,\n",
        "    person_id,\n",
        "    visit_detail_id,\n",
        "    CASE WHEN measurement_concept_id > 0 THEN measurement_concept_id ELSE measurement_source_concept_id END AS data_concept_id,\n",
        "    measurement_datetime AS datetime,\n",
        "    measurement_datetime AS end_datetime,\n",
        "    measurement_type_concept_id AS type_concept_id,\n",
        "    NULL AS quantity,\n",
        "    value_as_number,\n",
        "    NULL AS value_as_string,\n",
        "    value_as_concept_id\n",
        "\")"
    )
    
    drug_sql_cols <- paste0(
        "drug_sql_cols <- DBI::SQL(\"\n",
        "    'drug_exposure' AS source_table,\n",
        "    drug_exposure_id AS source_id,\n",
        "    person_id,\n",
        "    visit_detail_id,\n",
        "    CASE WHEN drug_concept_id > 0 THEN drug_concept_id ELSE drug_source_concept_id END AS data_concept_id,\n",
        "    drug_exposure_start_datetime AS datetime,\n",
        "    drug_exposure_end_datetime AS end_datetime,\n",
        "    drug_type_concept_id AS type_concept_id,\n",
        "    quantity,\n",
        "    NULL AS value_as_number,\n",
        "    NULL AS value_as_string,\n",
        "    NULL AS value_as_concept_id\n",
        "\")"
    )
    
    # Build the main SQL query
    if (data_source == "person") {
        main_sql <- paste0(
            "sql <- glue::glue_sql(\"\n",
            "    SELECT {`procedure_sql_cols`}\n",
            "    FROM procedure_occurrence\n",
            "    WHERE person_id = {m$selected_person}\n",
            "    \n",
            "    UNION ALL\n",
            "    \n",
            "    SELECT {`condition_sql_cols`}\n",
            "    FROM condition_occurrence\n",
            "    WHERE person_id = {m$selected_person}\n",
            "    \n",
            "    UNION ALL\n",
            "    \n",
            "    SELECT {`observation_sql_cols`}\n",
            "    FROM observation\n",
            "    WHERE person_id = {m$selected_person}\n",
            "    \n",
            "    UNION ALL\n",
            "    \n",
            "    SELECT {`measurement_sql_cols`}\n",
            "    FROM measurement\n",
            "    WHERE person_id = {m$selected_person}\n",
            "    \n",
            "    UNION ALL\n",
            "    \n",
            "    SELECT {`drug_sql_cols`}\n",
            "    FROM drug_exposure\n",
            "    WHERE person_id = {m$selected_person}\n",
            "\", .con = d$con)"
        )
    } else if (data_source == "visit_detail") {
        main_sql <- paste0(
            "sql <- glue::glue_sql(\"\n",
            "    SELECT {`procedure_sql_cols`}\n",
            "    FROM procedure_occurrence\n",
            "    WHERE visit_detail_id = {m$selected_visit_detail}\n",
            "    \n",
            "    UNION ALL\n",
            "    \n",
            "    SELECT {`condition_sql_cols`}\n",
            "    FROM condition_occurrence\n",
            "    WHERE visit_detail_id = {m$selected_visit_detail}\n",
            "    \n",
            "    UNION ALL\n",
            "    \n",
            "    SELECT {`observation_sql_cols`}\n",
            "    FROM observation\n",
            "    WHERE visit_detail_id = {m$selected_visit_detail}\n",
            "    \n",
            "    UNION ALL\n",
            "    \n",
            "    SELECT {`measurement_sql_cols`}\n",
            "    FROM measurement\n",
            "    WHERE visit_detail_id = {m$selected_visit_detail}\n",
            "    \n",
            "    UNION ALL\n",
            "    \n",
            "    SELECT {`drug_sql_cols`}\n",
            "    FROM drug_exposure\n",
            "    WHERE visit_detail_id = {m$selected_visit_detail}\n",
            "\", .con = d$con)"
        )
    }
    
    # Combine all SQL parts
    combined_sql <- paste(
        procedure_sql_cols,
        condition_sql_cols,
        observation_sql_cols,
        measurement_sql_cols,
        drug_sql_cols,
        "",
        main_sql,
        "",
        "data <- DBI::dbGetQuery(d$con, sql)",
        sep = "\n\n"
    )
    
    return(combined_sql)
}

# Generate data processing block for plotly timeline
generate_plotly_data_processing_block <- function() {
    
    processing_block <- paste0(
        "# Filter data based on concepts selection\n",
        "if (length(input$concepts_choice_%widget_id%) > 0) {\n",
        "    if (input$concepts_choice_%widget_id% == \"selected_concept_classes\") {\n",
        "        if (nrow(data) > 0) data <- \n",
        "            data %>%\n",
        "            dplyr::inner_join(d$dataset_concept %>% dplyr::select(data_concept_id = concept_id, concept_class_id), by = \"data_concept_id\") %>%\n",
        "            dplyr::filter(concept_class_id %in% input$concept_classes_%widget_id%) %>%\n",
        "            dplyr::select(-concept_class_id)\n",
        "    }\n",
        "    else if (input$concepts_choice_%widget_id% == \"selected_concepts\") {\n",
        "        if (nrow(data) > 0) data <- data %>% dplyr::filter(data_concept_id %in% input$concepts_%widget_id%)\n",
        "    }\n",
        "}\n\n",
        "# Join with concept information and process data\n",
        "data <-\n",
        "    data %>%\n",
        "    join_concepts(d$dataset_concept, c(\"data\", \"type\")) %>%\n",
        "    # Replace NA concept_name by their concept_id\n",
        "    dplyr::mutate(concept_name = dplyr::if_else(is.na(data_concept_name), as.character(data_concept_id), data_concept_name))\n\n",
        "# Add drug strength information for drug_exposure data\n",
        "if (any(data$source_table == \"drug_exposure\", na.rm = TRUE)) {\n",
        "    data <- data %>%\n",
        "        dplyr::left_join(\n",
        "            d$dataset_drug_strength %>%\n",
        "                join_concepts(d$dataset_concept, c(\"ingredient\", \"amount_unit\", \"numerator_unit\", \"denominator_unit\")) %>%\n",
        "                dplyr::select(\n",
        "                    drug_concept_id = drug_concept_id, ingredient_concept_id, ingredient_concept_name,\n",
        "                    amount_value, amount_unit_concept_id, amount_unit_concept_name,\n",
        "                    numerator_value, numerator_unit_concept_id, numerator_unit_concept_name,\n",
        "                    denominator_value, denominator_unit_concept_id, denominator_unit_concept_name\n",
        "                ),\n",
        "            by = c(\"data_concept_id\" = \"drug_concept_id\"),\n",
        "            copy = TRUE\n",
        "        ) %>%\n",
        "        dplyr::mutate(\n",
        "            amount = dplyr::case_when(\n",
        "                source_table == \"drug_exposure\" & !is.na(amount_value) ~ round(quantity * amount_value, 1),\n",
        "                source_table == \"drug_exposure\" & !is.na(numerator_value) ~ round(quantity * numerator_value, 1)\n",
        "            ),\n",
        "            amount_unit = dplyr::case_when(\n",
        "                source_table == \"drug_exposure\" & !is.na(amount_value) ~ amount_unit_concept_name,\n",
        "                source_table == \"drug_exposure\" & !is.na(numerator_value) ~ numerator_unit_concept_name\n",
        "            ),\n",
        "            duration_hours = dplyr::case_when(\n",
        "                source_table == \"drug_exposure\" ~ as.numeric(difftime(end_datetime, datetime, units = \"hours\"))\n",
        "            ),\n",
        "            rate = dplyr::case_when(\n",
        "                source_table == \"drug_exposure\" & !is.na(numerator_value) & !is.na(duration_hours) & duration_hours > 0 ~ round(amount / duration_hours, 1)\n",
        "            ),\n",
        "            rate_unit = dplyr::case_when(\n",
        "                source_table == \"drug_exposure\" & !is.na(rate) & !is.na(amount_unit) ~ paste0(amount_unit, \" per hour\")\n",
        "            ),\n",
        "            daily_dose = dplyr::case_when(\n",
        "                source_table == \"drug_exposure\" & is.na(rate) & !is.na(amount) & !is.na(duration_hours) & duration_hours > 0 ~ amount / duration_hours * 24\n",
        "            ),\n",
        "            daily_dose_unit = dplyr::case_when(\n",
        "                source_table == \"drug_exposure\" & !is.na(daily_dose) & !is.na(amount_unit) ~ paste0(amount_unit, \" per day\")\n",
        "            )\n",
        "        )\n",
        "}\n\n",
        "# Final data arrangement\n",
        "data <- data %>% dplyr::arrange(person_id, datetime)\n\n",
        "# Process concept names for display\n",
        "ordered_levels <- rev(sort(unique(data$concept_name)))\n",
        "data <- data %>% dplyr::mutate(concept_name = factor(concept_name, levels = ordered_levels))\n\n",
        "unique_levels <- levels(data$concept_name)\n",
        "unique_labels <- ifelse(\n",
        "    nchar(unique_levels) > 22,\n",
        "    paste0(substr(unique_levels, 1, 17), \"...\"),\n",
        "    unique_levels\n",
        ")\n\n",
        "# Set datetime format based on language\n",
        "if (language == \"fr\") datetime_format <- \"%d-%m-%Y %H:%M\" else datetime_format <- \"%Y-%m-%d %H:%M\""
    )
    
    return(processing_block)
}

# Generate plotly chart visualization code block
generate_plotly_chart_block <- function(synchronize_timelines) {
    
    chart_block <- paste0(
        "if (nrow(data) > 0) {\n",
        "    fig <- plotly::plot_ly(data = data, source = \"plot_%widget_id%\")\n",
        "    \n",
        "    # Add segments for events with duration (end_datetime > datetime)\n",
        "    fig <- fig %>%\n",
        "        plotly::add_segments(\n",
        "            data = subset(data, end_datetime > datetime),\n",
        "            x = ~datetime,\n",
        "            xend = ~end_datetime,\n",
        "            y = ~as.numeric(concept_name),\n",
        "            yend = ~as.numeric(concept_name),\n",
        "            line = list(color = \"coral\", width = 5),\n",
        "            text = ~paste0(\n",
        "                \"<b>\", concept_name, \"</b><br>\",\n",
        "                i18np$t(\"start\"), \" : \", format(datetime, datetime_format), \"<br>\",\n",
        "                i18np$t(\"end\"), \" : \", format(end_datetime, datetime_format), \"<br>\",\n",
        "                ifelse(source_table == \"drug_exposure\", paste0(\"<br><b>\", i18np$t(\"dosage\"), \"</b><br>\"), \n",
        "                       ifelse(source_table == \"measurement\", paste0(\"<br><b>\", i18np$t(\"measurement\"), \"</b><br>\"), \n",
        "                              ifelse(source_table == \"observation\", paste0(\"<br><b>\", i18np$t(\"observation\"), \"</b><br>\"), \"<br>\"))),\n",
        "                # Add drug-specific information when available\n",
        "                ifelse(source_table == \"drug_exposure\" & !is.na(amount), \n",
        "                       paste0(i18np$t(\"dose\"), \" : \", amount, \" \", ifelse(is.na(amount_unit), \"\", amount_unit), \"<br>\"), \"\"),\n",
        "                ifelse(source_table == \"drug_exposure\" & !is.na(rate), \n",
        "                       paste0(i18np$t(\"rate\"), \" : \", rate, \" \", ifelse(is.na(rate_unit), \"\", rate_unit), \"<br>\"), \"\"),\n",
        "                ifelse(source_table == \"drug_exposure\" & !is.na(daily_dose), \n",
        "                       paste0(i18np$t(\"daily_dose\"), \" : \", daily_dose, \" \", ifelse(is.na(daily_dose_unit), \"\", daily_dose_unit), \"<br>\"), \"\"),\n",
        "                ifelse(source_table == \"drug_exposure\" & !is.na(duration_hours), \n",
        "                       paste0(i18np$t(\"duration\"), \" : \", round(duration_hours, 1), \" \", i18np$t(\"hours\"), \"<br>\"), \"\"),\n",
        "                # Add measurement/observation values\n",
        "                ifelse(!is.na(value_as_number), paste0(i18np$t(\"value\"), \" : \", value_as_number, \"<br>\"), \"\"),\n",
        "                ifelse(!is.na(value_as_string) & value_as_string != \"\", paste0(i18np$t(\"result\"), \" : \", value_as_string, \"<br>\"), \"\"),\n",
        "                # Add quantity for procedures\n",
        "                ifelse(!is.na(quantity) & source_table != \"drug_exposure\", paste0(i18np$t(\"quantity\"), \" : \", quantity, \"<br>\"), \"\"),\n",
        "                \"<br><i>\", i18np$t(paste0(\"source_\", source_table)), \"</i>\"\n",
        "            ),\n",
        "            hoverinfo = \"text\"\n",
        "        ) %>%\n",
        "        # Add markers for point events (end_datetime == datetime or NA)\n",
        "        plotly::add_markers(\n",
        "            data = subset(data, end_datetime == datetime | is.na(end_datetime)),\n",
        "            x = ~datetime,\n",
        "            y = ~as.numeric(concept_name),\n",
        "            marker = list(color = \"coral\", size = 10),\n",
        "            text = ~paste0(\n",
        "                \"<b>\", concept_name, \"</b><br>\",\n",
        "                i18np$t(\"datetime\"), \" : \", format(datetime, datetime_format), \"<br>\",\n",
        "                ifelse(source_table == \"drug_exposure\", paste0(\"<br><b>\", i18np$t(\"administration\"), \"</b><br>\"), \n",
        "                       ifelse(source_table == \"measurement\", paste0(\"<br><b>\", i18np$t(\"measurement\"), \"</b><br>\"), \n",
        "                              ifelse(source_table == \"observation\", paste0(\"<br><b>\", i18np$t(\"observation\"), \"</b><br>\"), \"<br>\"))),\n",
        "                # Add drug-specific information when available\n",
        "                ifelse(source_table == \"drug_exposure\" & !is.na(amount), \n",
        "                       paste0(i18np$t(\"dose\"), \" : \", amount, \" \", ifelse(is.na(amount_unit), \"\", amount_unit), \"<br>\"), \"\"),\n",
        "                ifelse(source_table == \"drug_exposure\" & !is.na(rate), \n",
        "                       paste0(i18np$t(\"rate\"), \" : \", rate, \" \", ifelse(is.na(rate_unit), \"\", rate_unit), \"<br>\"), \"\"),\n",
        "                # Add measurement/observation values\n",
        "                ifelse(!is.na(value_as_number), paste0(i18np$t(\"value\"), \" : \", value_as_number, \"<br>\"), \"\"),\n",
        "                ifelse(!is.na(value_as_string) & value_as_string != \"\", paste0(i18np$t(\"result\"), \" : \", value_as_string, \"<br>\"), \"\"),\n",
        "                # Add quantity for procedures\n",
        "                ifelse(!is.na(quantity) & source_table != \"drug_exposure\", paste0(i18np$t(\"quantity\"), \" : \", quantity, \"<br>\"), \"\"),\n",
        "                \"<br><i>\", i18np$t(paste0(\"source_\", source_table)), \"</i>\"\n",
        "            ),\n",
        "            hoverinfo = \"text\"\n",
        "        )\n",
        "    \n",
        "    # Configure plot layout\n",
        "    fig <- fig %>%\n",
        "        plotly::layout(\n",
        "            xaxis = list(\n",
        "                type = \"date\",\n",
        "                tickmode = \"auto\",\n",
        "                title = \"\",\n",
        "                nticks = 10,\n",
        "                tickfont = list(size = 10),\n",
        "                tickformat = datetime_format,\n",
        "                range = c(\n",
        "                    format(datetimes[[1]], \"%Y-%m-%d %H:%M:%S\"),\n",
        "                    format(datetimes[[2]], \"%Y-%m-%d %H:%M:%S\")\n",
        "                )\n",
        "            ),\n",
        "            yaxis = list(\n",
        "                tickvals = seq_along(unique_levels),\n",
        "                ticktext = unique_labels,\n",
        "                title = \"\",\n",
        "                tickfont = list(family = \"Courier New\", size = 11),\n",
        "                automargin = FALSE\n",
        "            ),\n",
        "            hoverlabel = list(align = \"left\"),\n",
        "            margin = list(l = 145, r = 0, t = 0, b = 0),\n",
        "            showlegend = FALSE\n",
        "        ) %>%\n",
        "        plotly::config(displayModeBar = FALSE) %>%\n",
        "        plotly::event_register(\"plotly_relayout\")\n",
        "}"
    )
    
    return(chart_block)
}

# ======================================
# PLOTLY TIMELINE SYNCHRONIZATION
# ======================================

# Adjust plotly padding when timeline synchronization is toggled
observe_event(input$synchronize_timelines_%widget_id%, {
    
    # Only apply padding adjustments for plotly
    chart_type <- if (length(input$chart_type_%widget_id%) > 0) {
        input$chart_type_%widget_id%
    } else {
        "dygraphs"
    }
    
    if (chart_type == "plotly") {
        if (input$synchronize_timelines_%widget_id%) {
            # Add left padding to align with synchronized timeline
            shinyjs::runjs(sprintf(
                "document.getElementById('%s').style.paddingLeft = '80px';",
                ns("plot_%widget_id%")
            ))
        } else {
            # Remove padding when synchronization is disabled
            shinyjs::runjs(sprintf(
                "document.getElementById('%s').style.paddingLeft = '0px';",
                ns("plot_%widget_id%")
            ))
        }
    }
})

# Monitor plotly relayout events for timeline synchronization
observe_event(plotly::event_data("plotly_relayout", source = "plot_%widget_id%"), {
    
    # Only process if timeline synchronization is enabled
    if (!input$synchronize_timelines_%widget_id%) return()
    
    relayout_data <- plotly::event_data("plotly_relayout", source = "plot_%widget_id%")
    
    if (!is.null(relayout_data) && is.list(relayout_data)) {
        
        if ("xaxis.autorange" %in% names(relayout_data) && relayout_data[["xaxis.autorange"]]) {
            # Auto-range was selected, use full data range
            if (length(m$data_datetimes_range_%widget_id%) > 0) {
                m$datetimes_timeline_%tab_id%(m$data_datetimes_range_%widget_id%)
            }
        } else if ("xaxis.range[0]" %in% names(relayout_data) && "xaxis.range[1]" %in% names(relayout_data)) {
            # Manual range selection
            selected_min <- lubridate::force_tz(
                as.POSIXct(relayout_data[["xaxis.range[0]"]], origin = "1970-01-01"), 
                tzone = "UTC"
            )
            selected_max <- lubridate::force_tz(
                as.POSIXct(relayout_data[["xaxis.range[1]"]], origin = "1970-01-01"), 
                tzone = "UTC"
            )
            m$datetimes_timeline_%tab_id%(c(selected_min, selected_max))
        }
    }
})

# Listen for timeline changes from other synchronized widgets (plotly version)
observe_event(m$debounced_datetimes_timeline_%tab_id%(), {
    
    # Only process if synchronization is enabled and data is available
    if (!input$synchronize_timelines_%widget_id% || 
        length(m$debounced_datetimes_timeline_%tab_id%()) == 0 || 
        length(m$datetimes_%widget_id%) == 0) {
        return()
    }
    
    # Check for significant timeline changes
    # Calculate time difference between current and synchronized timelines
    time_diff_start <- abs(
        as.numeric(m$debounced_datetimes_timeline_%tab_id%()[[1]]) - 
        as.numeric(m$datetimes_%widget_id%[[1]])
    )
    time_diff_end <- abs(
        as.numeric(m$debounced_datetimes_timeline_%tab_id%()[[2]]) - 
        as.numeric(m$datetimes_%widget_id%[[2]])
    )
    
    # Trigger code re-execution if timeline has changed significantly (>5 seconds)
    if (time_diff_start > 5 || time_diff_end > 5) {
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
    
}, ignoreInit = TRUE)
