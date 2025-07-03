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
        "# Initialize data structures for dygraphs visualization\n",
        "features <- list()              # List to store individual time series\n",
        "features_names <- c()           # Vector to store concept names for chart labels\n",
        "raw_data <- tibble::tibble()    # Initialize empty tibble for raw data\n",
        "data_datetimes_range <- c()     # Date range for the dataset\n",
        "combined_features <- c()        # Combined XTS object for all features"
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
            "# Build SQL query to fetch Measurement and Observation data for selected person\n",
            "# This query combines data from both measurement and observation tables\n",
            "sql <- glue::glue_sql('\n",
            "    -- Query measurements table for numeric values\n",
            "    SELECT \n",
            "        measurement_concept_id AS concept_id,\n",
            "        measurement_source_concept_id AS source_concept_id,\n",
            "        measurement_datetime AS datetime,\n",
            "        value_as_number\n",
            "    FROM measurement \n",
            "    WHERE person_id = {m$selected_person} \n",
            "    AND (measurement_concept_id IN ({concepts$concept_id*}) OR measurement_source_concept_id IN ({concepts$concept_id*}))\n",
            "    UNION\n",
            "    -- Query observations table for numeric values\n",
            "    SELECT \n",
            "        observation_concept_id AS concept_id,\n",
            "        observation_source_concept_id AS source_concept_id,\n",
            "        observation_datetime AS datetime, \n",
            "        value_as_number\n",
            "    FROM observation \n",
            "    WHERE person_id = {m$selected_person} \n",
            "    AND (observation_concept_id IN ({concepts$concept_id*}) OR observation_source_concept_id IN ({concepts$concept_id*}))\n",
            "', .con = d$con)\n\n",
            "# Execute query and convert to tibble\n",
            "raw_data <- DBI::dbGetQuery(d$con, sql) %>% tibble::as_tibble()"
        )
    } else if (data_source == "visit_detail") {
        sql_block <- paste0(
            "# Build SQL query to fetch Measurement and Observation data for selected visit detail\n",
            "# This query combines data from both measurement and observation tables\n",
            "sql <- glue::glue_sql('\n",
            "    -- Query measurements table for numeric values\n",
            "    SELECT \n",
            "        measurement_concept_id AS concept_id,\n",
            "        measurement_source_concept_id AS source_concept_id,\n",
            "        measurement_datetime AS datetime,\n",
            "        value_as_number\n",
            "    FROM measurement \n",
            "    WHERE visit_detail_id = {m$selected_visit_detail} \n",
            "    AND (measurement_concept_id IN ({concepts$concept_id*}) OR measurement_source_concept_id IN ({concepts$concept_id*}))\n",
            "    UNION\n",
            "    -- Query observations table for numeric values\n",
            "    SELECT \n",
            "        observation_concept_id AS concept_id,\n",
            "        observation_source_concept_id AS source_concept_id,\n",
            "        observation_datetime AS datetime, \n",
            "        value_as_number\n",
            "    FROM observation \n",
            "    WHERE visit_detail_id = {m$selected_visit_detail} \n",
            "    AND (observation_concept_id IN ({concepts$concept_id*}) OR observation_source_concept_id IN ({concepts$concept_id*}))\n",
            "', .con = d$con)\n\n",
            "# Execute query and convert to tibble\n",
            "raw_data <- DBI::dbGetQuery(d$con, sql) %>% tibble::as_tibble()"
        )
    }
    
    return(sql_block)
}

# Generate data processing block for dygraphs XTS creation
generate_dygraphs_data_processing_block <- function() {
    
    processing_block <- paste0(
        "# Process each concept to create XTS time series objects\n",
        "for (concept_id in concepts$concept_id) {\n",
        "    # Get concept details from the concepts table\n",
        "    concept <- concepts %>% dplyr::filter(concept_id == !!concept_id)\n",
        "    \n",
        "    if (nrow(concept) > 0) {\n",
        "        # Only process Measurement and Observation domains for dygraphs\n",
        "        if (concept$domain_id %in% c('Measurement', 'Observation')) {\n",
        "            data <- raw_data\n",
        "            \n",
        "            if (nrow(data) > 0) {\n",
        "                # Filter data for current concept (by concept_id or source_concept_id)\n",
        "                data <- data %>%\n",
        "                    dplyr::filter(concept_id == !!concept_id | source_concept_id == !!concept_id) %>%\n",
        "                    dplyr::select(datetime, value_as_number)\n",
        "                \n",
        "                if (nrow(data) > 0) {\n",
        "                    # Add fake data points at timeline boundaries to ensure proper chart alignment\n",
        "                    # This helps with timeline synchronization across multiple charts\n",
        "                    fake_data <- tibble::tibble(\n",
        "                        datetime = c(data_datetimes_range[[1]] - lubridate::seconds(1), \n",
        "                                   data_datetimes_range[[2]] + lubridate::seconds(1)),\n",
        "                        value_as_number = c(NA, NA)  # NA values won't be plotted\n",
        "                    )\n",
        "                    \n",
        "                    # Combine fake boundary points with actual data\n",
        "                    data <- dplyr::bind_rows(fake_data, data)\n",
        "                    data <- data %>% dplyr::arrange(datetime)\n",
        "                    \n",
        "                    # Create XTS object (extensible time series) for dygraphs\n",
        "                    features[[paste0('concept_', concept_id)]] <- xts::xts(data$value_as_number, data$datetime)\n",
        "                    features_names <- c(features_names, concept$concept_name)\n",
        "                }\n",
        "            }\n",
        "        }\n",
        "    }\n",
        "}\n\n",
        "# Merge all individual time series into a single XTS object\n",
        "if (length(features) > 0) combined_features <- do.call(merge, features)\n",
        "# Set column names to concept names for chart legend\n",
        "if (length(features_names) > 0) colnames(combined_features) <- features_names"
    )
    
    return(processing_block)
}

# Generate dygraph chart visualization code block
generate_dygraphs_chart_block <- function(synchronize_timelines) {
    
    if (synchronize_timelines) {
        chart_block <- paste0(
            "# Create dygraph with timeline synchronization enabled\n",
            "if (length(combined_features) > 0) {\n",
            "    fig <- dygraphs::dygraph(combined_features, group = 'tab_%tab_id%') %>%\n",
            "        # Add interactive points and configure timezone handling\n",
            "        dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2, useDataTimezone = TRUE) %>%\n",
            "        # Add range selector with predefined date window\n",
            "        dygraphs::dyRangeSelector(dateWindow = c(\n",
            "            format(datetimes[[1]], '%Y-%m-%d %H:%M:%S'),\n",
            "            format(datetimes[[2]], '%Y-%m-%d %H:%M:%S')\n",
            "        )) %>%\n",
            "        # Configure Y-axis to start at 0\n",
            "        dygraphs::dyAxis('y', valueRange = c(0, NA))\n",
            "}\n\n",
            "# Return the chart object\n",
            "fig"
        )
    } else {
        chart_block <- paste0(
            "# Create standalone dygraph (no timeline synchronization)\n",
            "if (length(combined_features) > 0) {\n",
            "    fig <- dygraphs::dygraph(combined_features) %>%\n",
            "        # Add interactive points and configure timezone handling\n",
            "        dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2, useDataTimezone = TRUE) %>%\n",
            "        # Add range selector with predefined date window\n",
            "        dygraphs::dyRangeSelector(dateWindow = c(\n",
            "            format(datetimes[[1]], '%Y-%m-%d %H:%M:%S'),\n",
            "            format(datetimes[[2]], '%Y-%m-%d %H:%M:%S')\n",
            "        )) %>%\n",
            "        # Configure Y-axis to start at 0\n",
            "        dygraphs::dyAxis('y', valueRange = c(0, NA))\n",
            "}\n\n",
            "# Return the chart object\n",
            "fig"
        )
    }
    
    return(chart_block)
}
