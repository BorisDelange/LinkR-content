# ==========================================
# server_code_dygraphs.R - Dygraphs Specific Logic
# ==========================================
# 
# Handles dygraphs specific functionality including:
# - Code generation for Measurement and Observation data
# - XTS data processing for time series
# - Dygraphs chart configuration
# - Timeline synchronization for dygraphs
#
# ==========================================

# ======================================
# DYGRAPHS CODE GENERATION
# ======================================

# Generate dygraphs specific figure code
generate_dygraphs_figure_code <- function(data_source, concepts, synchronize_timelines) {
    
    # Initialize code blocks list
    code <- list()
    
    # Build concepts table
    code$block_1 <- generate_concepts_block(concepts)
    
    # Initialize data structures for dygraphs
    code$block_2 <- paste0(
        "features <- list()\n",
        "features_names <- c()\n",
        "raw_data <- tibble::tibble()\n",
        "data_datetimes_range <- c()\n",
        "combined_features <- c()"
    )
    
    # Build SQL query for Measurement and Observation
    code$block_3 <- generate_dygraphs_sql_block(data_source)
    
    # Date range processing
    code$block_4 <- generate_date_range_block(data_source, synchronize_timelines)
    
    # Data processing loop for XTS creation
    code$block_5 <- generate_dygraphs_data_processing_block()
    
    # Chart generation
    code$block_6 <- generate_dygraphs_chart_block(synchronize_timelines)
    
    # Combine all blocks into final code
    final_code <- paste(code, collapse = "\n\n")
    
    return(final_code)
}

# Generate SQL query for dygraphs (Measurement and Observation only)
generate_dygraphs_sql_block <- function(data_source) {
    
    if (data_source == "person") {
        sql_block <- paste0(
            "sql <- glue::glue_sql('\n",
            "    SELECT \n",
            "        measurement_concept_id AS concept_id,\n",
            "        measurement_source_concept_id AS source_concept_id,\n",
            "        measurement_datetime AS datetime,\n",
            "        value_as_number\n",
            "    FROM measurement \n",
            "    WHERE person_id = {m$selected_person} \n",
            "    AND (measurement_concept_id IN ({concepts$concept_id*}) OR measurement_source_concept_id IN ({concepts$concept_id*}))\n",
            "    UNION\n",
            "    SELECT \n",
            "        observation_concept_id AS concept_id,\n",
            "        observation_source_concept_id AS source_concept_id,\n",
            "        observation_datetime AS datetime, \n",
            "        value_as_number\n",
            "    FROM observation \n",
            "    WHERE person_id = {m$selected_person} \n",
            "    AND (observation_concept_id IN ({concepts$concept_id*}) OR observation_source_concept_id IN ({concepts$concept_id*}))\n",
            "', .con = d$con)\n\n",
            "raw_data <- DBI::dbGetQuery(d$con, sql) %>% tibble::as_tibble()"
        )
    } else if (data_source == "visit_detail") {
        sql_block <- paste0(
            "sql <- glue::glue_sql('\n",
            "    SELECT \n",
            "        measurement_concept_id AS concept_id,\n",
            "        measurement_source_concept_id AS source_concept_id,\n",
            "        measurement_datetime AS datetime,\n",
            "        value_as_number\n",
            "    FROM measurement \n",
            "    WHERE visit_detail_id = {m$selected_visit_detail} \n",
            "    AND (measurement_concept_id IN ({concepts$concept_id*}) OR measurement_source_concept_id IN ({concepts$concept_id*}))\n",
            "    UNION\n",
            "    SELECT \n",
            "        observation_concept_id AS concept_id,\n",
            "        observation_source_concept_id AS source_concept_id,\n",
            "        observation_datetime AS datetime, \n",
            "        value_as_number\n",
            "    FROM observation \n",
            "    WHERE visit_detail_id = {m$selected_visit_detail} \n",
            "    AND (observation_concept_id IN ({concepts$concept_id*}) OR observation_source_concept_id IN ({concepts$concept_id*}))\n",
            "', .con = d$con)\n\n",
            "raw_data <- DBI::dbGetQuery(d$con, sql) %>% tibble::as_tibble()"
        )
    }
    
    return(sql_block)
}

# Generate data processing block for dygraphs XTS creation
generate_dygraphs_data_processing_block <- function() {
    
    processing_block <- paste0(
        "for (concept_id in concepts$concept_id) {\n",
        "    concept <- concepts %>% dplyr::filter(concept_id == !!concept_id)\n",
        "    \n",
        "    if (nrow(concept) > 0) {\n",
        "        if (concept$domain_id %in% c('Measurement', 'Observation')) {\n",
        "            data <- raw_data\n",
        "            \n",
        "            if (nrow(data) > 0) {\n",
        "                data <- data %>%\n",
        "                    dplyr::filter(concept_id == !!concept_id | source_concept_id == !!concept_id) %>%\n",
        "                    dplyr::select(datetime, value_as_number)\n",
        "                \n",
        "                if (nrow(data) > 0) {\n",
        "                    fake_data <- tibble::tibble(\n",
        "                        datetime = c(data_datetimes_range[[1]] - lubridate::seconds(1), \n",
        "                                   data_datetimes_range[[2]] + lubridate::seconds(1)),\n",
        "                        value_as_number = c(NA, NA)\n",
        "                    )\n",
        "                    \n",
        "                    data <- dplyr::bind_rows(fake_data, data)\n",
        "                    data <- data %>% dplyr::arrange(datetime)\n",
        "                    \n",
        "                    features[[paste0('concept_', concept_id)]] <- xts::xts(data$value_as_number, data$datetime)\n",
        "                    features_names <- c(features_names, concept$concept_name)\n",
        "                }\n",
        "            }\n",
        "        }\n",
        "    }\n",
        "}\n\n",
        "if (length(features) > 0) combined_features <- do.call(merge, features)\n",
        "if (length(features_names) > 0) colnames(combined_features) <- features_names"
    )
    
    return(processing_block)
}

# Generate dygraph chart visualization code block
generate_dygraphs_chart_block <- function(synchronize_timelines) {
    
    if (synchronize_timelines) {
        chart_block <- paste0(
            "if (length(combined_features) > 0) {\n",
            "    fig <- dygraphs::dygraph(combined_features, group = 'tab_%tab_id%') %>%\n",
            "        dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2, useDataTimezone = TRUE) %>%\n",
            "        dygraphs::dyRangeSelector(dateWindow = c(\n",
            "            format(datetimes[[1]], '%Y-%m-%d %H:%M:%S'),\n",
            "            format(datetimes[[2]], '%Y-%m-%d %H:%M:%S')\n",
            "        )) %>%\n",
            "        dygraphs::dyAxis('y', valueRange = c(0, NA))\n",
            "}\n\n",
            "fig"
        )
    } else {
        chart_block <- paste0(
            "if (length(combined_features) > 0) {\n",
            "    fig <- dygraphs::dygraph(combined_features) %>%\n",
            "        dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2, useDataTimezone = TRUE) %>%\n",
            "        dygraphs::dyRangeSelector(dateWindow = c(\n",
            "            format(datetimes[[1]], '%Y-%m-%d %H:%M:%S'),\n",
            "            format(datetimes[[2]], '%Y-%m-%d %H:%M:%S')\n",
            "        )) %>%\n",
            "        dygraphs::dyAxis('y', valueRange = c(0, NA))\n",
            "}\n\n",
            "fig"
        )
    }
    
    return(chart_block)
}

# ======================================
# DYGRAPHS TIMELINE SYNCHRONIZATION
# ======================================

# Initialize timeline variables if they don't exist
if (length(m$datetimes_timeline_%tab_id%) == 0) {
    # Main timeline reactive value
    m$datetimes_timeline_%tab_id% <- reactiveVal()
    
    # Debounced version to prevent excessive updates
    m$debounced_datetimes_timeline_%tab_id% <- reactive(m$datetimes_timeline_%tab_id%()) %>% debounce(500)
}

# Adjust dygraph padding when timeline synchronization is toggled
observe_event(input$synchronize_timelines_%widget_id%, {
    
    # Only apply padding adjustments for dygraphs
    chart_type <- if (length(input$chart_type_%widget_id%) > 0) {
        input$chart_type_%widget_id%
    } else {
        "dygraphs"
    }
    
    if (chart_type == "dygraphs") {
        if (input$synchronize_timelines_%widget_id%) {
            # Add left padding to align with synchronized timeline
            shinyjs::runjs(sprintf(
                "document.getElementById('%s').style.paddingLeft = '80px'; var event = new Event('resize'); window.dispatchEvent(event);",
                ns("dygraph_div_%widget_id%")
            ))
        } else {
            # Remove padding when synchronization is disabled
            shinyjs::runjs(sprintf(
                "document.getElementById('%s').style.paddingLeft = '0px'; var event = new Event('resize'); window.dispatchEvent(event);",
                ns("dygraph_div_%widget_id%")
            ))
        }
    }
})

# Monitor dygraph date window changes and broadcast to other widgets
observe_event(input$dygraph_%widget_id%_date_window, {
    
    # Only process if timeline synchronization is enabled
    if (!input$synchronize_timelines_%widget_id%) return()
    
    # Convert date window from JavaScript format to R POSIXct
    datetime_values <- as.POSIXct(
        input$dygraph_%widget_id%_date_window, 
        format = "%Y-%m-%dT%H:%M:%OSZ", 
        tz = "UTC"
    )
    
    # Update the shared timeline reactive value
    m$datetimes_timeline_%tab_id%(datetime_values)
})

# Listen for timeline changes from other synchronized widgets
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
