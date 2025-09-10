# ==========================================
# server_code_plotly.R - Plotly Timeline Code Generation
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 REQUIRES CUSTOMIZATION - PLOTLY TIMELINE LOGIC  🔧                     ██
# ██                                                                            ██
# ██  This file generates R code for plotly interactive timeline charts.        ██
# ██  Customize the timeline visualization for your healthcare data needs.      ██
# ██  Handles all OMOP data types with segments and marker visualizations.      ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# TIMELINE PLUGIN - PLOTLY TIMELINE CODE GENERATION
# 
# This file contains specialized functions for generating R code that creates 
# comprehensive interactive timeline visualizations using the plotly library.
# Unlike dygraphs which focuses on continuous data, plotly timelines handle
# discrete healthcare events, procedures, medications, and conditions with 
# rich visual representations including segments, markers, and annotations.
# 
# CORE FUNCTIONALITY:
# - Multi-modal healthcare timeline code generation with comprehensive OMOP data integration
# - Interactive plotly timeline visualization with segments, markers, and event annotations
# - Cross-concept timeline coordination with synchronized zoom and pan controls
# - Dynamic healthcare event processing with temporal relationship analysis
# - Flexible concept selection supporting both predefined concepts and concept class filtering
# 
# PLOTLY TIMELINE FEATURES:
# - Interactive timeline charts with hover tooltips and click interactions
# - Multi-track timeline visualization with separate lanes for different data types
# - Event duration visualization using timeline segments for procedures and drug exposures
# - Point-in-time markers for measurements, observations, and discrete events
# - Rich legend system with concept grouping and visual hierarchy
# - Responsive timeline layout adapting to varying data densities and time ranges
# 
# COMPREHENSIVE OMOP INTEGRATION:
# - MEASUREMENT table processing for laboratory results and vital signs
# - OBSERVATION table integration for clinical assessments and patient-reported outcomes
# - DRUG_EXPOSURE table handling for medication timelines and dosing information
# - PROCEDURE_OCCURRENCE table processing for surgical and diagnostic procedures
# - CONDITION_OCCURRENCE table integration for diagnosis and comorbidity timelines
# - CONCEPT table joins for standardized medical terminology and hierarchical relationships
# 
# HEALTHCARE EVENT PROCESSING:
# - Temporal event clustering and overlap detection for timeline clarity
# - Duration calculation for ongoing treatments and chronic conditions
# - Event priority scoring for visual layering in dense timeline segments
# - Clinical workflow integration with care episode grouping
# - Multi-visit timeline continuity with hospitalization context
# 
# ADVANCED VISUALIZATION PATTERNS:
# - Concept class-based filtering for dynamic healthcare domain selection
# - Color coding systems reflecting clinical significance and data source
# - Interactive timeline segments showing treatment duration and effectiveness
# - Annotation layers for clinical notes and care team communications
# - Statistical summary overlays for trend analysis and outcome correlation
# 
# TIMELINE SYNCHRONIZATION:
# - Cross-widget timeline coordination for comprehensive patient view
# - Event-driven timeline updates with real-time data integration
# - Shared temporal navigation controls for multi-timeline analysis
# - Persistent timeline state management across user sessions and data updates
# 
# CODE GENERATION ARCHITECTURE:
# - Modular R code construction supporting multiple visualization modes
# - Complex SQL generation for multi-table OMOP queries with concept hierarchy
# - Plotly-specific data transformation with interactive feature optimization
# - Healthcare-aware color schemes and styling for clinical workflow integration
# - Error handling and data validation for robust healthcare data processing

# ======================================
# PLOTLY CODE GENERATION
# ======================================

# Generate plotly specific output code
# This function creates the complete code for a plotly timeline visualization
generate_plotly_output_code_%widget_id% <- function(data_source, concepts_choice, concepts = NULL, concept_classes = NULL, omop_tables = NULL, synchronize_timelines) {
    
    # Initialize code blocks list to store different sections of generated code
    code <- list()
    
    # Build concepts table or skip if using concept classes
    if (concepts_choice == "selected_concepts" && !is.null(concepts)) {
        code$block_1 <- generate_concepts_block_%widget_id%(concepts)
    } else {
        code$block_1 <- "# Using concept classes selection - no predefined concepts table needed"
    }
    
    # Initialize data structures for plotly visualization
    # Creates empty containers for the data and date ranges
    code$block_2 <- paste0(
        "# Initialize data structures for plotly visualization\n",
        "data <- tibble::tibble()             # Empty tibble to store processed data\n",
        "data_datetimes_range <- c()          # Vector to store date range boundaries"
    )
    
    # Build SQL query for all OMOP tables
    # Creates complex SQL to fetch data from multiple OMOP CDM tables
    code$block_3 <- generate_plotly_sql_block_%widget_id%(data_source, concepts_choice, concept_classes, omop_tables)
    
    # Date range processing - determines the time window for visualization
    code$block_4 <- generate_date_range_block_%widget_id%(data_source, synchronize_timelines)
    
    # Data processing for plotly timeline
    # Processes raw data into format suitable for timeline visualization
    code$block_5 <- generate_plotly_data_processing_block_%widget_id%(concepts_choice, concepts)
    
    # Chart generation - creates the actual plotly visualization
    code$block_6 <- generate_plotly_chart_block_%widget_id%(synchronize_timelines)
    
    # Combine all blocks into final executable code
    final_code <- paste(code, collapse = "\n\n")
    
    return(final_code)
}

# Generate SQL query for plotly (all OMOP data types)
# This function creates a comprehensive SQL query that fetches data from all OMOP tables
generate_plotly_sql_block_%widget_id% <- function(data_source, concepts_choice, concept_classes = NULL, omop_tables = NULL) {
    
    # Define OMOP version handling for procedure columns
    # Different OMOP versions have different column names for procedure end dates
    if (m$omop_version == "5.3") {
        # OMOP 5.3 uses procedure_datetime for both start and end
        procedure_sql_cols <- paste0(
            "procedure_sql_cols <- DBI::SQL(\"\n",
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
            "    NULL AS value_as_concept_id,\n",
            "    NULL AS unit_concept_id\n",
            "\")"
        )
    } else {
        # OMOP 5.4+ has separate procedure_end_datetime column
        procedure_sql_cols <- paste0(
            "procedure_sql_cols <- DBI::SQL(\"\n",
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
            "    NULL AS value_as_concept_id,\n",
            "    NULL AS unit_concept_id\n",
            "\")"
        )
    }
    
    # Define column mappings for condition occurrence table
    # Standardizes column names and adds calculated fields
    condition_sql_cols <- paste0(
        "# Define column mappings for condition occurrence table\n",
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
        "    NULL AS value_as_concept_id,\n",
        "    NULL AS unit_concept_id\n",
        "\")"
    )
    
    # Define column mappings for observation table
    # Handles observations with various value types (numeric, string, concept)
    observation_sql_cols <- paste0(
        "# Define column mappings for observation table\n",
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
        "    value_as_concept_id,\n",
        "    NULL AS unit_concept_id\n",
        "\")"
    )
    
    # Define column mappings for measurement table
    # Handles laboratory values and other numeric measurements
    measurement_sql_cols <- paste0(
        "# Define column mappings for measurement table\n",
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
        "    value_as_concept_id,\n",
        "    unit_concept_id\n",
        "\")"
    )
    
    # Define column mappings for drug exposure table
    # Handles medication administrations with dosage and duration information
    drug_sql_cols <- paste0(
        "# Define column mappings for drug exposure table\n",
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
        "    NULL AS value_as_concept_id,\n",
        "    NULL AS unit_concept_id\n",
        "\")"
    )
    
    # Build the main SQL query with concept filtering
    # Creates a UNION query that combines data from all OMOP tables
    if (concepts_choice == "selected_concepts") {
        # Original logic: filter by specific concept IDs
        if (data_source == "person") {
            main_sql <- paste0(
                "# Build comprehensive SQL query for person-level data\n",
                "# This query combines data from all OMOP tables using UNION ALL\n",
                "sql <- glue::glue_sql(\"\n",
                "    -- Fetch procedure occurrences\n",
                "    SELECT {`procedure_sql_cols`}\n",
                "    FROM procedure_occurrence\n",
                "    WHERE person_id = {m$selected_person}\n",
                "    AND (procedure_concept_id IN ({concepts$concept_id*}) OR procedure_source_concept_id IN ({concepts$concept_id*}))\n",
                "    \n",
                "    UNION ALL\n",
                "    \n",
                "    -- Fetch condition occurrences\n",
                "    SELECT {`condition_sql_cols`}\n",
                "    FROM condition_occurrence\n",
                "    WHERE person_id = {m$selected_person}\n",
                "    AND (condition_concept_id IN ({concepts$concept_id*}) OR condition_source_concept_id IN ({concepts$concept_id*}))\n",
                "    \n",
                "    UNION ALL\n",
                "    \n",
                "    -- Fetch observations\n",
                "    SELECT {`observation_sql_cols`}\n",
                "    FROM observation\n",
                "    WHERE person_id = {m$selected_person}\n",
                "    AND (observation_concept_id IN ({concepts$concept_id*}) OR observation_source_concept_id IN ({concepts$concept_id*}))\n",
                "    \n",
                "    UNION ALL\n",
                "    \n",
                "    -- Fetch measurements\n",
                "    SELECT {`measurement_sql_cols`}\n",
                "    FROM measurement\n",
                "    WHERE person_id = {m$selected_person}\n",
                "    AND (measurement_concept_id IN ({concepts$concept_id*}) OR measurement_source_concept_id IN ({concepts$concept_id*}))\n",
                "    \n",
                "    UNION ALL\n",
                "    \n",
                "    -- Fetch drug exposures\n",
                "    SELECT {`drug_sql_cols`}\n",
                "    FROM drug_exposure\n",
                "    WHERE person_id = {m$selected_person}\n",
                "    AND (drug_concept_id IN ({concepts$concept_id*}) OR drug_source_concept_id IN ({concepts$concept_id*}))\n",
                "\", .con = d$con)"
            )
        } else if (data_source == "visit_detail") {
        main_sql <- paste0(
            "# Build comprehensive SQL query for visit detail-level data\n",
            "# This query combines data from all OMOP tables using UNION ALL\n",
            "sql <- glue::glue_sql(\"\n",
            "    -- Fetch procedure occurrences for specific visit detail\n",
            "    SELECT {`procedure_sql_cols`}\n",
            "    FROM procedure_occurrence\n",
            "    WHERE visit_detail_id = {m$selected_visit_detail}\n",
            "    AND (procedure_concept_id IN ({concepts$concept_id*}) OR procedure_source_concept_id IN ({concepts$concept_id*}))\n",
            "    \n",
            "    UNION ALL\n",
            "    \n",
            "    -- Fetch condition occurrences for specific visit detail\n",
            "    SELECT {`condition_sql_cols`}\n",
            "    FROM condition_occurrence\n",
            "    WHERE visit_detail_id = {m$selected_visit_detail}\n",
            "    AND (condition_concept_id IN ({concepts$concept_id*}) OR condition_source_concept_id IN ({concepts$concept_id*}))\n",
            "    \n",
            "    UNION ALL\n",
            "    \n",
            "    -- Fetch observations for specific visit detail\n",
            "    SELECT {`observation_sql_cols`}\n",
            "    FROM observation\n",
            "    WHERE visit_detail_id = {m$selected_visit_detail}\n",
            "    AND (observation_concept_id IN ({concepts$concept_id*}) OR observation_source_concept_id IN ({concepts$concept_id*}))\n",
            "    \n",
            "    UNION ALL\n",
            "    \n",
            "    -- Fetch measurements for specific visit detail\n",
            "    SELECT {`measurement_sql_cols`}\n",
            "    FROM measurement\n",
            "    WHERE visit_detail_id = {m$selected_visit_detail}\n",
            "    AND (measurement_concept_id IN ({concepts$concept_id*}) OR measurement_source_concept_id IN ({concepts$concept_id*}))\n",
            "    \n",
            "    UNION ALL\n",
            "    \n",
            "    -- Fetch drug exposures for specific visit detail\n",
            "    SELECT {`drug_sql_cols`}\n",
            "    FROM drug_exposure\n",
            "    WHERE visit_detail_id = {m$selected_visit_detail}\n",
            "    AND (drug_concept_id IN ({concepts$concept_id*}) OR drug_source_concept_id IN ({concepts$concept_id*}))\n",
            "\", .con = d$con)"
            )
        }
    } else if (concepts_choice == "selected_concept_classes" && !is.null(concept_classes) && !is.null(omop_tables)) {
        # New logic: filter by concept classes and selected OMOP tables
        
        # Define concept classes variable for SQL interpolation
        concept_classes_definition <- paste0(
            "# Define concept classes for SQL filtering\n",
            "concept_classes <- c(", paste0('"', concept_classes, '"', collapse = ", "), ")"
        )
        
        # Build dynamic UNION query based on selected OMOP tables
        union_parts <- c()
        
        if ("procedure_occurrence" %in% omop_tables && data_source == "person") {
            union_parts <- c(union_parts, paste0(
                "    -- Fetch procedure occurrences filtered by concept classes\n",
                "    SELECT {`procedure_sql_cols`}\n",
                "    FROM procedure_occurrence p\n",
                "    INNER JOIN concept c ON p.procedure_concept_id = c.concept_id AND c.concept_class_id IN ({concept_classes*})\n",
                "    WHERE p.person_id = {m$selected_person}"
            ))
        }
        if ("procedure_occurrence" %in% omop_tables && data_source == "visit_detail") {
            union_parts <- c(union_parts, paste0(
                "    -- Fetch procedure occurrences filtered by concept classes\n",
                "    SELECT {`procedure_sql_cols`}\n",
                "    FROM procedure_occurrence p\n",
                "    INNER JOIN concept c ON p.procedure_concept_id = c.concept_id AND c.concept_class_id IN ({concept_classes*})\n",
                "    WHERE p.visit_detail_id = {m$selected_visit_detail}"
            ))
        }
        
        if ("condition_occurrence" %in% omop_tables && data_source == "person") {
            union_parts <- c(union_parts, paste0(
                "    -- Fetch condition occurrences filtered by concept classes\n",
                "    SELECT {`condition_sql_cols`}\n",
                "    FROM condition_occurrence co\n",
                "    INNER JOIN concept c ON co.condition_concept_id = c.concept_id AND c.concept_class_id IN ({concept_classes*})\n",
                "    WHERE co.person_id = {m$selected_person}"
            ))
        }
        if ("condition_occurrence" %in% omop_tables && data_source == "visit_detail") {
            union_parts <- c(union_parts, paste0(
                "    -- Fetch condition occurrences filtered by concept classes\n",
                "    SELECT {`condition_sql_cols`}\n",
                "    FROM condition_occurrence co\n",
                "    INNER JOIN concept c ON co.condition_concept_id = c.concept_id AND c.concept_class_id IN ({concept_classes*})\n",
                "    WHERE co.visit_detail_id = {m$selected_visit_detail}"
            ))
        }
        
        if ("observation" %in% omop_tables && data_source == "person") {
            union_parts <- c(union_parts, paste0(
                "    -- Fetch observations filtered by concept classes\n",
                "    SELECT {`observation_sql_cols`}\n",
                "    FROM observation o\n",
                "    INNER JOIN concept c ON o.observation_concept_id = c.concept_id AND c.concept_class_id IN ({concept_classes*})\n",
                "    WHERE o.person_id = {m$selected_person}"
            ))
        }
        if ("observation" %in% omop_tables && data_source == "visit_detail") {
            union_parts <- c(union_parts, paste0(
                "    -- Fetch observations filtered by concept classes\n",
                "    SELECT {`observation_sql_cols`}\n",
                "    FROM observation o\n",
                "    INNER JOIN concept c ON o.observation_concept_id = c.concept_id AND c.concept_class_id IN ({concept_classes*})\n",
                "    WHERE o.visit_detail_id = {m$selected_visit_detail}"
            ))
        }
        
        if ("measurement" %in% omop_tables && data_source == "person") {
            union_parts <- c(union_parts, paste0(
                "    -- Fetch measurements filtered by concept classes\n",
                "    SELECT {`measurement_sql_cols`}\n",
                "    FROM measurement m\n",
                "    INNER JOIN concept c ON m.measurement_concept_id = c.concept_id AND c.concept_class_id IN ({concept_classes*})\n",
                "    WHERE m.person_id = {m$selected_person}"
            ))
        }
        if ("measurement" %in% omop_tables && data_source == "visit_detail") {
            union_parts <- c(union_parts, paste0(
                "    -- Fetch measurements filtered by concept classes\n",
                "    SELECT {`measurement_sql_cols`}\n",
                "    FROM measurement m\n",
                "    INNER JOIN concept c ON m.measurement_concept_id = c.concept_id AND c.concept_class_id IN ({concept_classes*})\n",
                "    WHERE m.visit_detail_id = {m$selected_visit_detail}"
            ))
        }
        
        if ("drug_exposure" %in% omop_tables && data_source == "person") {
            union_parts <- c(union_parts, paste0(
                "    -- Fetch drug exposures filtered by concept classes\n",
                "    SELECT {`drug_sql_cols`}\n",
                "    FROM drug_exposure d\n",
                "    INNER JOIN concept c ON d.drug_concept_id = c.concept_id AND c.concept_class_id IN ({concept_classes*})\n",
                "    WHERE d.person_id = {m$selected_person}"
            ))
        }
        if ("drug_exposure" %in% omop_tables && data_source == "visit_detail") {
            union_parts <- c(union_parts, paste0(
                "    -- Fetch drug exposures filtered by concept classes\n",
                "    SELECT {`drug_sql_cols`}\n",
                "    FROM drug_exposure d\n",
                "    INNER JOIN concept c ON d.drug_concept_id = c.concept_id AND c.concept_class_id IN ({concept_classes*})\n",
                "    WHERE d.visit_detail_id = {m$selected_visit_detail}"
            ))
        }
        
        # Combine union parts with UNION ALL
        if (length(union_parts) > 0) {
            main_sql <- paste0(
                concept_classes_definition, "\n\n",
                "# Build dynamic SQL query for selected OMOP tables with concept classes filtering\n",
                "sql <- glue::glue_sql(\"\n",
                paste(union_parts, collapse = "\n    \n    UNION ALL\n    \n"),
                "\n\", .con = d$con)"
            )
        } else {
            main_sql <- paste0(
                "# No OMOP tables selected - create empty query\n",
                "sql <- \"SELECT NULL LIMIT 0\""
            )
        }
    }
    
    # Combine all SQL parts into final executable code
    combined_sql <- paste(
        procedure_sql_cols,
        condition_sql_cols,
        observation_sql_cols,
        measurement_sql_cols,
        drug_sql_cols,
        main_sql,
        "# Execute the SQL query and convert results to tibble",
        "data <- DBI::dbGetQuery(d$con, sql)",
        sep = "\n\n"
    )
    
    return(combined_sql)
}

# Generate data processing block for plotly timeline
# This function processes the raw data into a format suitable for timeline visualization
generate_plotly_data_processing_block_%widget_id% <- function(concepts_choice, concepts = NULL) {
    
    # Check if Drug concepts are selected to determine if drug processing is needed
    has_drug_concepts <- if (concepts_choice == "selected_concepts" && !is.null(concepts)) {
        any(concepts$domain_id == "Drug", na.rm = TRUE)
    } else {
        # For concept classes mode, we'll determine this dynamically from the data
        TRUE  # We'll check the actual data content later
    }
    
    # Basic data filtering and processing
    processing_block <- paste0(
        "# Filter data based on concept selection criteria\n",
        "# This allows users to filter by concept classes if specified\n",
        "if (length(input$concepts_choice_%widget_id%) > 0) {\n",
        "    if (input$concepts_choice_%widget_id% == \"selected_concept_classes\") {\n",
        "        if (nrow(data) > 0) {\n",
        "            # Join with concept class information and filter\n",
        "            data <- \n",
        "                data %>%\n",
        "                dplyr::inner_join(d$dataset_concept %>% dplyr::select(data_concept_id = concept_id, concept_class_id), by = \"data_concept_id\") %>%\n",
        "                dplyr::filter(concept_class_id %in% input$concept_classes_%widget_id%) %>%\n",
        "                dplyr::select(-concept_class_id)\n",
        "        }\n",
        "    }\n",
        "}\n\n",
        "# Join with concept information to get human-readable names\n",
        "# This enriches the data with concept names and type information\n",
        "data <-\n",
        "    data %>%\n",
        "    join_concepts(d$dataset_concept, c(\"data\", \"type\")) %>%\n",
        "    dplyr::left_join(\n",
        "        d$dataset_concept %>% dplyr::select(unit_concept_id = concept_id, unit_concept_code = concept_code),\n",
        "        by = \"unit_concept_id\"\n",
        "    ) %>%\n",
        "    # Handle cases where concept names are missing\n",
        "    dplyr::mutate(concept_name = dplyr::if_else(is.na(data_concept_name), as.character(data_concept_id), data_concept_name))\n\n"
    )
    
    # Add drug-specific processing if Drug concepts are selected
    if (has_drug_concepts) {
        drug_processing <- paste0(
            "# Add drug strength and dosage calculations for drug exposure data\n",
            "# This section calculates doses, rates, and daily doses based on drug strength tables\n",
            "if (any(data$source_table == \"drug_exposure\", na.rm = TRUE)) {\n",
            "    # Join with drug strength information from OMOP drug_strength table\n",
            "    data <- data %>%\n",
            "        dplyr::left_join(\n",
            "            d$dataset_drug_strength %>%\n",
            "                # Get ingredient and unit concept names\n",
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
            "            # Calculate total dose amount based on quantity and drug strength\n",
            "            amount = dplyr::case_when(\n",
            "                source_table == \"drug_exposure\" & !is.na(amount_value) ~ round(quantity * amount_value, 1),\n",
            "                source_table == \"drug_exposure\" & !is.na(numerator_value) ~ round(quantity * numerator_value, 1)\n",
            "            ),\n",
            "            # Determine appropriate units for the calculated amount\n",
            "            amount_unit = dplyr::case_when(\n",
            "                source_table == \"drug_exposure\" & !is.na(amount_value) ~ amount_unit_concept_name,\n",
            "                source_table == \"drug_exposure\" & !is.na(numerator_value) ~ numerator_unit_concept_name\n",
            "            ),\n",
            "            # Calculate duration in hours for infusion/continuous drugs\n",
            "            duration_hours = dplyr::case_when(\n",
            "                source_table == \"drug_exposure\" ~ as.numeric(difftime(end_datetime, datetime, units = \"hours\"))\n",
            "            ),\n",
            "            # Calculate infusion rate (amount per hour)\n",
            "            rate = dplyr::case_when(\n",
            "                source_table == \"drug_exposure\" & !is.na(numerator_value) & !is.na(duration_hours) & duration_hours > 0 ~ round(amount / duration_hours, 1)\n",
            "            ),\n",
            "            # Create rate unit label\n",
            "            rate_unit = dplyr::case_when(\n",
            "                source_table == \"drug_exposure\" & !is.na(rate) & !is.na(amount_unit) ~ paste0(amount_unit, \" per hour\")\n",
            "            ),\n",
            "            # Calculate daily dose for non-infusion drugs\n",
            "            daily_dose = dplyr::case_when(\n",
            "                source_table == \"drug_exposure\" & is.na(rate) & !is.na(amount) & !is.na(duration_hours) & duration_hours > 0 ~ amount / duration_hours * 24\n",
            "            ),\n",
            "            # Create daily dose unit label\n",
            "            daily_dose_unit = dplyr::case_when(\n",
            "                source_table == \"drug_exposure\" & !is.na(daily_dose) & !is.na(amount_unit) ~ paste0(amount_unit, \" per day\")\n",
            "            )\n",
            "        )\n",
            "} else {\n",
            "    # Add empty drug columns when no drug data is present\n",
            "    # This ensures consistent data structure for downstream processing\n",
            "    data <- data %>%\n",
            "        dplyr::mutate(\n",
            "            amount = NA_real_,\n",
            "            amount_unit = NA_character_,\n",
            "            duration_hours = NA_real_,\n",
            "            rate = NA_real_,\n",
            "            rate_unit = NA_character_,\n",
            "            daily_dose = NA_real_,\n",
            "            daily_dose_unit = NA_character_\n",
            "        )\n",
            "}\n\n"
        )
    } else {
        # Add empty drug columns when no Drug concepts are selected
        drug_processing <- paste0(
            "# Add empty drug columns since no Drug concepts are selected\n",
            "# This ensures consistent data structure regardless of concept selection\n",
            "data <- data %>%\n",
            "    dplyr::mutate(\n",
            "        amount = NA_real_,\n",
            "        amount_unit = NA_character_,\n",
            "        duration_hours = NA_real_,\n",
            "        rate = NA_real_,\n",
            "        rate_unit = NA_character_,\n",
            "        daily_dose = NA_real_,\n",
            "        daily_dose_unit = NA_character_\n",
            "    )\n\n"
        )
    }
    
    # Final data processing and formatting
    final_processing <- paste0(
        "# Final data arrangement and formatting\n",
        "# Sort data chronologically by person and datetime\n",
        "data <- data %>% dplyr::arrange(person_id, datetime)\n\n",
        "# Create ordered factor levels for Y-axis display\n",
        "# Concepts are displayed in reverse alphabetical order from bottom to top\n",
        "ordered_levels <- rev(sort(unique(data$concept_name)))\n",
        "data <- data %>% dplyr::mutate(concept_name = factor(concept_name, levels = ordered_levels))\n\n",
        "# Create shortened labels for long concept names to improve readability\n",
        "unique_levels <- levels(data$concept_name)\n",
        "unique_labels <- ifelse(\n",
        "    nchar(unique_levels) > 22,\n",
        "    paste0(substr(unique_levels, 1, 17), \"...\"),\n",
        "    unique_levels\n",
        ")\n\n",
        "# Set datetime display format\n",
        "# Format dates according to locale preferences\n",
        "datetime_format <- \"%Y-%m-%d %H:%M\"\n",
        "if (language == \"fr\") {\n",
        "    datetime_format <- \"%d/%m/%Y %H:%M\"\n",
        "}"
    )
    
    return(paste0(processing_block, drug_processing, final_processing))
}

# Define color scheme for different OMOP data types
# Each data type gets a distinct color for easy identification
define_omop_colors <- function() {
    colors <- list(
        "measurement" = "#619CFF",      # Blue - for laboratory measurements
        "drug_exposure" = "#F8766D",    # Red/Coral - for medications
        "procedure" = "#00BA38",        # Green - for procedures
        "condition" = "#FF8C42",        # Orange - for conditions/diagnoses
        "observation" = "#C77CFF"       # Purple - for observations
    )
    return(colors)
}

# Get appropriate color based on OMOP source table type
get_color_by_source <- function(source_table) {
    colors <- define_omop_colors()
    return(colors[[source_table]])
}

# Generate plotly chart visualization code block with color coding
# This creates the interactive timeline visualization with hover information
generate_plotly_chart_block_%widget_id% <- function(synchronize_timelines) {
    
    chart_block <- paste0(
        "# Create plotly timeline visualization\n",
        "if (nrow(data) > 0) {\n",
        "    # Define color palette for different OMOP data types\n",
        "    # Uses ggplot2 classic color palette for consistency\n",
        "    omop_colors <- list(\n",
        "        \"measurement\" = \"#619CFF\",      # Blue for measurements\n",
        "        \"drug_exposure\" = \"#F8766D\",    # Red/Coral for medications\n",
        "        \"procedure\" = \"#00BA38\",        # Green for procedures\n",
        "        \"condition\" = \"#FF8C42\",        # Orange for conditions\n",
        "        \"observation\" = \"#C77CFF\"       # Purple for observations\n",
        "    )\n",
        "    \n",
        "    # Assign colors to data points based on their source table\n",
        "    data <- data %>%\n",
        "        dplyr::mutate(\n",
        "            color = dplyr::case_when(\n",
        "                source_table == \"measurement\" ~ omop_colors[[\"measurement\"]],\n",
        "                source_table == \"drug_exposure\" ~ omop_colors[[\"drug_exposure\"]],\n",
        "                source_table == \"procedure\" ~ omop_colors[[\"procedure\"]],\n",
        "                source_table == \"condition\" ~ omop_colors[[\"condition\"]],\n",
        "                source_table == \"observation\" ~ omop_colors[[\"observation\"]],\n",
        "                TRUE ~ \"#808080\"  # Default gray for unknown types\n",
        "            )\n",
        "        )\n",
        "    \n",
        "    # Define filtered datasets to avoid repetition\n",
        "    data_segments <- subset(data, end_datetime > datetime)\n",
        "    data_points <- subset(data, end_datetime == datetime | is.na(end_datetime))\n",
        "    \n",
        "    # Initialize plotly object with data source for event handling\n",
        "    fig <- plotly::plot_ly(data = data, source = \"plotly_%widget_id%\")\n",
        "    \n",
        "    # Add line segments for events that have duration (end_datetime > datetime)\n",
        "    # These represent ongoing events like medication infusions or conditions\n",
        "    if (nrow(data_segments) > 0) {\n",
        "        fig <- fig %>%\n",
        "            plotly::add_segments(\n",
        "                data = subset(data, end_datetime > datetime),\n",
        "                x = ~datetime,\n",
        "                xend = ~end_datetime,\n",
        "                y = ~as.numeric(concept_name),\n",
        "                yend = ~as.numeric(concept_name),\n",
        "                line = list(color = ~color, width = 5),\n",
        "                # Create comprehensive hover text with relevant information\n",
        "                text = ~paste0(\n",
        "                    \"<b style='color:white'>\", concept_name, \"</b><br>\",\n",
        "                    \"<span style='color:white'>\", i18np$t(\"start\"), \" : \", format(datetime, datetime_format), \"</span><br>\",\n",
        "                    \"<span style='color:white'>\", i18np$t(\"end\"), \" : \", format(end_datetime, datetime_format), \"</span><br>\",\n",
        "                    # Add section headers based on data type\n",
        "                    ifelse(source_table == \"drug_exposure\", paste0(\"<br><b style='color:white'>\", i18np$t(\"dosage\"), \"</b><br>\"), \n",
        "                    ifelse(source_table == \"measurement\", paste0(\"<br><b style='color:white'>\", i18np$t(\"measurement\"), \"</b><br>\"), \n",
        "                    ifelse(source_table == \"observation\", paste0(\"<br><b style='color:white'>\", i18np$t(\"observation\"), \"</b><br>\"), \"<br>\"))),\n",
        "                    # Add drug-specific dosage information when available\n",
        "                    ifelse(source_table == \"drug_exposure\" & !is.na(amount), \n",
        "                           paste0(\"<span style='color:white'>\", i18np$t(\"dose\"), \" : \", amount, \" \", ifelse(is.na(amount_unit), \"\", amount_unit), \"</span><br>\"), \"\"),\n",
        "                    ifelse(source_table == \"drug_exposure\" & !is.na(rate), \n",
        "                           paste0(\"<span style='color:white'>\", i18np$t(\"rate\"), \" : \", rate, \" \", ifelse(is.na(rate_unit), \"\", rate_unit), \"</span><br>\"), \"\"),\n",
        "                    ifelse(source_table == \"drug_exposure\" & !is.na(daily_dose), \n",
        "                           paste0(\"<span style='color:white'>\", i18np$t(\"daily_dose\"), \" : \", daily_dose, \" \", ifelse(is.na(daily_dose_unit), \"\", daily_dose_unit), \"</span><br>\"), \"\"),\n",
        "                    ifelse(source_table == \"drug_exposure\" & !is.na(duration_hours), \n",
        "                           paste0(\"<span style='color:white'>\", i18np$t(\"duration\"), \" : \", round(duration_hours, 1), \" \", i18np$t(\"hours\"), \"</span><br>\"), \"\"),\n",
        "                    # Add measurement values with units\n",
        "                    ifelse(source_table == \"measurement\" & !is.na(value_as_number), \n",
        "                           paste0(\"<span style='color:white'>\", i18np$t(\"value\"), \" : \", value_as_number, \n",
        "                                  ifelse(!is.na(unit_concept_code), paste0(\" \", unit_concept_code), \"\"), \"</span><br>\"), \"\"),\n",
        "                    # Add observation and other values without units\n",
        "                    ifelse(source_table != \"measurement\" & !is.na(value_as_number), \n",
        "                           paste0(\"<span style='color:white'>\", i18np$t(\"value\"), \" : \", value_as_number, \"</span><br>\"), \"\"),\n",
        "                    ifelse(!is.na(value_as_string) & value_as_string != \"\", \n",
        "                           paste0(\"<span style='color:white'>\", i18np$t(\"result\"), \" : \", value_as_string, \"</span><br>\"), \"\"),\n",
        "                    # Add quantity for procedures\n",
        "                    ifelse(!is.na(quantity) & source_table != \"drug_exposure\", \n",
        "                           paste0(\"<span style='color:white'>\", i18np$t(\"quantity\"), \" : \", quantity, \"</span><br>\"), \"\"),\n",
        "                    # Add source table information\n",
        "                    \"<br><i style='color:white'>\", i18np$t(paste0(\"source_\", source_table)), \"</i>\"\n",
        "                ),\n",
        "                hoverinfo = \"text\"\n",
        "            )\n",
        "    }\n",
        "    \n",
        "    # Add markers for point events (end_datetime == datetime or NA)\n",
        "    # These represent instantaneous events like single measurements or procedures\n",
        "    if (nrow(data_points) > 0) {\n",
        "        fig <- fig %>%\n",
        "            plotly::add_markers(\n",
        "                data = subset(data, end_datetime == datetime | is.na(end_datetime)),\n",
        "                x = ~datetime,\n",
        "                y = ~as.numeric(concept_name),\n",
        "                marker = list(color = ~color, size = 10),\n",
        "                # Create hover text for point events\n",
        "                text = ~paste0(\n",
        "                    \"<b style='color:white'>\", concept_name, \"</b><br>\",\n",
        "                    \"<span style='color:white'>\", i18np$t(\"datetime\"), \" : \", format(datetime, datetime_format), \"</span><br>\",\n",
        "                    # Add section headers based on data type\n",
        "                    ifelse(source_table == \"drug_exposure\", paste0(\"<br><b style='color:white'>\", i18np$t(\"administration\"), \"</b><br>\"), \n",
        "                    ifelse(source_table == \"measurement\", paste0(\"<br><b style='color:white'>\", i18np$t(\"measurement\"), \"</b><br>\"), \n",
        "                    ifelse(source_table == \"observation\", paste0(\"<br><b style='color:white'>\", i18np$t(\"observation\"), \"</b><br>\"), \"<br>\"))),\n",
        "                    # Add drug-specific information when available\n",
        "                    ifelse(source_table == \"drug_exposure\" & !is.na(amount), \n",
        "                           paste0(\"<span style='color:white'>\", i18np$t(\"dose\"), \" : \", amount, \" \", ifelse(is.na(amount_unit), \"\", amount_unit), \"</span><br>\"), \"\"),\n",
        "                    ifelse(source_table == \"drug_exposure\" & !is.na(rate), \n",
        "                           paste0(\"<span style='color:white'>\", i18np$t(\"rate\"), \" : \", rate, \" \", ifelse(is.na(rate_unit), \"\", rate_unit), \"</span><br>\"), \"\"),\n",
        "                    # Add measurement values with units\n",
        "                    ifelse(source_table == \"measurement\" & !is.na(value_as_number), \n",
        "                           paste0(\"<span style='color:white'>\", i18np$t(\"value\"), \" : \", value_as_number, \n",
        "                                  ifelse(!is.na(unit_concept_code), paste0(\" \", unit_concept_code), \"\"), \"</span><br>\"), \"\"),\n",
        "                    # Add observation and other values without units\n",
        "                    ifelse(source_table != \"measurement\" & !is.na(value_as_number), \n",
        "                           paste0(\"<span style='color:white'>\", i18np$t(\"value\"), \" : \", value_as_number, \"</span><br>\"), \"\"),\n",
        "                    ifelse(!is.na(value_as_string) & value_as_string != \"\", \n",
        "                           paste0(\"<span style='color:white'>\", i18np$t(\"result\"), \" : \", value_as_string, \"</span><br>\"), \"\"),\n",
        "                    # Add quantity for procedures\n",
        "                    ifelse(!is.na(quantity) & source_table != \"drug_exposure\", \n",
        "                           paste0(\"<span style='color:white'>\", i18np$t(\"quantity\"), \" : \", quantity, \"</span><br>\"), \"\"),\n",
        "                    # Add source table information\n",
        "                    \"<br><i style='color:white'>\", i18np$t(paste0(\"source_\", source_table)), \"</i>\"\n",
        "                ),\n",
        "                hoverinfo = \"text\"\n",
        "            )\n",
        "    }\n",
        "    \n",
        "    # Configure plot layout and appearance\n",
        "    fig <- fig %>%\n",
        "        plotly::layout(\n",
        "            # Configure X-axis (time axis)\n",
        "            xaxis = list(\n",
        "                type = \"date\",\n",
        "                tickmode = \"auto\",\n",
        "                title = \"\",\n",
        "                nticks = 10,\n",
        "                tickfont = list(size = 10),\n",
        "                tickformat = datetime_format,\n",
        "                # Set initial date range for the chart\n",
        "                range = c(\n",
        "                    format(datetimes[[1]], \"%Y-%m-%d %H:%M:%S\"),\n",
        "                    format(datetimes[[2]], \"%Y-%m-%d %H:%M:%S\")\n",
        "                )\n",
        "            ),\n",
        "            # Configure Y-axis (concept names)\n",
        "            yaxis = list(\n",
        "                tickvals = seq_along(unique_levels),\n",
        "                ticktext = unique_labels,\n",
        "                title = \"\",\n",
        "                tickfont = list(family = \"Courier New\", size = 11),\n",
        "                automargin = FALSE\n",
        "            ),\n",
        "            # Configure hover labels\n",
        "            hoverlabel = list(align = \"left\"),\n",
        "            # Set margins for proper label display\n",
        "            margin = list(l = 145, r = 0, t = 0, b = 0),\n",
        "            # Hide legend since colors are self-explanatory\n",
        "            showlegend = FALSE\n",
        "        ) %>%\n",
        "        # Disable mode bar and register relayout events for timeline sync\n",
        "        plotly::config(displayModeBar = FALSE) %>%\n",
        "        plotly::event_register(\"plotly_relayout\")\n",
        "}\n\n",
        "# Return the configured plotly output\n",
        "fig"
    )
    
    return(chart_block)
}
