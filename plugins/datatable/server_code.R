# ==========================================
# server_code.R - Code Editor Server Logic
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 REQUIRES CUSTOMIZATION - PLUGIN IMPLEMENTATION  🔧                     ██
# ██                                                                            ██
# ██  This file MUST be customized for your specific plugin.                    ██
# ██  Follow the template structure and implement your logic.                   ██
# ██  See comments and examples for guidance.                                   ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# PLUGIN TEMPLATE - CODE EDITOR SERVER FILE
# 
# This file handles the server-side logic for the code editor and output generation.
# It provides the foundation for automatic code generation from UI settings and
# manual code execution, creating a seamless no-code to code experience.
# 
# WHEN CREATING A NEW PLUGIN WITH THIS TEMPLATE:
# - Implement the generate_output_code_%widget_id%() function for your specific analysis
# - Customize the code execution logic in the run_code observer
# - Add patient selection validation following the commented example (lines 307-318)
# - Configure UI message handling for user-friendly error display (lines 350-398)
# - Add translations for plugin-specific messages in translations.csv
# - Add any plugin-specific helper functions and variables
# - Modify auto-execution triggers based on your data dependencies
# 
# CORE FUNCTIONALITY:
# - Automatic R code generation from UI configuration settings
# - Code editor with syntax highlighting and keyboard shortcuts
# - Manual code execution with error handling and output display
# - Auto-execution when data context changes (optional)
# - Integration with the widget's database storage system
# 
# COMMON PLUGIN PATTERNS:
# 
# HEALTHCARE DATA ANALYSIS PLUGINS:
#   - Patient selection validation with user-friendly error messages
#   - OMOP medical data integration and filtering
#   - Healthcare indicator calculations (mortality, LOS, readmission rates)
#   - Generate statistical analysis code (t-tests, regression, ANOVA)
#   - Create summary statistics and descriptive analysis
# 
# VISUALIZATION PLUGINS:
#   - Timeline visualizations with dygraphs and plotly
#   - Healthcare dashboards with indicator cards
#   - Generate ggplot2 code for various chart types
#   - Create interactive plotly visualizations with medical data
#   - Build custom plotting functions with user parameters
# 
# MEDICAL DATA PROCESSING PLUGINS:
#   - OMOP CDM data filtering and transformation
#   - Patient cohort and visit detail processing  
#   - Medical concept mapping and vocabulary integration
#   - Care site and hospital unit filtering
#   - Generate data cleaning and preparation workflows
# 
# REPORTING PLUGINS:
#   - Healthcare indicator reports and dashboards
#   - Patient summary reports with medical timelines
#   - Generate markdown or HTML report code
#   - Create formatted table output code
#   - Build export and download functionality

# ======================================
# INITIALIZATION
# ======================================

# Initialize code storage variable
m$code_%widget_id% <- ""

# Fix ACE editor rendering issues on startup
# Delay ensures DOM is fully loaded before triggering resize
shinyjs::delay(300, shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);"))

# ======================================
# CODE EDITOR KEYBOARD SHORTCUTS
# ======================================

# Handle comment/uncomment keyboard shortcut (Ctrl+Shift+C)
observe_event(input$code_%widget_id%_comment, {
    editor_toggle_comments(
        input_id = "code_%widget_id%", 
        code = input$code_%widget_id%,
        selection = input$code_%widget_id%_comment$range, 
        session = session
    )
})

# Handle run all code keyboard shortcut (Ctrl+Shift+Enter)
observe_event(input$code_%widget_id%_run_all, {
    # Only allow code execution if user has console access
    if ("projects_widgets_console" %in% user_accesses) {
        m$code_%widget_id% <- input$code_%widget_id%
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

# Handle save keyboard shortcut (Ctrl+S)
observe_event(input$code_%widget_id%_save, {
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_output_settings_and_code_%widget_id%', Math.random());"))
})

# ======================================
# OUTPUT DISPLAY CONTROLLER
# ======================================

# Main code execution handler - triggered by display button or shortcuts
observe_event(input$display_output_%widget_id%, {
    
    # Determine current tab (default to output_settings if not set)
    current_tab <- if (length(input$current_tab_%widget_id%) == 0) {
        "output_settings"
    } else {
        input$current_tab_%widget_id%
    }
    
    # ====================
    # AUTO-GENERATE CODE FROM OUTPUT SETTINGS
    # ====================
    if (current_tab == "output_settings") {
        
        # Extract user settings from the UI inputs for Data Table plugin
        data_source <- if (length(input$data_source_%widget_id%) > 0) {
            input$data_source_%widget_id%
        } else {
            "person"
        }
        
        concepts_choice <- if (length(input$concepts_choice_%widget_id%) > 0) {
            input$concepts_choice_%widget_id%
        } else {
            "selected_concepts"
        }
        
        omop_table <- if (length(input$omop_table_%widget_id%) > 0) {
            input$omop_table_%widget_id%
        } else {
            "measurement"
        }
        
        concept_classes <- if (length(input$concept_classes_%widget_id%) > 0) {
            input$concept_classes_%widget_id%
        } else {
            c()
        }
        
        concepts <- if (length(input$concepts_%widget_id%) > 0) {
            # Handle "all_available" special case
            if (identical(input$concepts_%widget_id%, "all_available")) {
                # Get all available concept IDs for Measurement and Observation domains
                selected_concepts %>%
                    dplyr::filter(domain_id %in% c("Measurement", "Observation")) %>%
                    dplyr::pull(concept_id)
            } else {
                input$concepts_%widget_id%
            }
        } else {
            c()
        }
        
        column_organization <- if (length(input$column_organization_%widget_id%) > 0) {
            input$column_organization_%widget_id%
        } else {
            "regular_intervals"
        }
        
        num_cols <- if (length(input$num_cols_%widget_id%) > 0) {
            input$num_cols_%widget_id%
        } else {
            8
        }
        
        aggregate_fct <- if (length(input$aggregate_fct_%widget_id%) > 0) {
            input$aggregate_fct_%widget_id%
        } else {
            "mean"
        }
        
        # Generate R code based on current configuration
        generated_code <- generate_output_code_%widget_id%(
            data_source = data_source,
            concepts_choice = concepts_choice,
            omop_table = omop_table,
            concept_classes = concept_classes,
            concepts = concepts,
            column_organization = column_organization,
            num_cols = num_cols,
            aggregate_fct = aggregate_fct
        )
        
        # Update ACE editor with generated code
        shinyAce::updateAceEditor(session, "code_%widget_id%", value = generated_code)
        
        # Store code and trigger execution
        m$code_%widget_id% <- generated_code
        shinyjs::delay(500, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());")))
    }
    # ====================
    # MANUAL CODE EXECUTION
    # ====================
    else if ("projects_widgets_console" %in% user_accesses) {
        # If on code tab, run whatever is currently in the editor
        m$code_%widget_id% <- input$code_%widget_id%
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

# ======================================
# CODE GENERATION FUNCTION
# ======================================

# Data Table code generation function
# Generates R code for time-based measurement data aggregation and display
generate_output_code_%widget_id% <- function(data_source = "person", concepts_choice = "selected_concepts", 
                                           omop_table = "measurement", concept_classes = c(), 
                                           concepts = c(), column_organization = "regular_intervals",
                                           num_cols = 8, aggregate_fct = "mean") {
    
    code_lines <- c()
    
    # Add data source and patient/visit validation
    code_lines <- c(code_lines,
        "# Data source and validation",
        paste0("data_source <- \"", data_source, "\"")
    )
    
    # Add patient/visit selection validation
    if (data_source == "person") {
        code_lines <- c(code_lines,
            "",
            "# Check if patient is selected",
            "patient_selected <- TRUE",
            "if (is.null(m$selected_person) || is.na(m$selected_person)) {",
            "    patient_selected <- FALSE",
            "    error_message <- i18np$t(\"select_patient\")",
            "}"
        )
    } else {
        code_lines <- c(code_lines,
            "",
            "# Check if visit detail is selected", 
            "visit_selected <- TRUE",
            "if (is.null(m$selected_visit_detail) || is.na(m$selected_visit_detail)) {",
            "    visit_selected <- FALSE",
            "    error_message <- i18np$t(\"select_stay\")",
            "}"
        )
    }
    
    # Add data query generation
    code_lines <- c(code_lines,
        "",
        "# Execute only if patient/visit is selected",
        "if ((exists('patient_selected') && patient_selected || exists('visit_selected') && visit_selected) && is.null(error_message)) {"
    )
    
    if (data_source == "person") {
        code_lines <- c(code_lines,
            "    # Query measurement data for selected patient",
            "    sql <- glue::glue_sql(\"",
            "        SELECT",
            "            person_id,",
            "            visit_detail_id,",
            "            CASE WHEN measurement_concept_id > 0 THEN measurement_concept_id ELSE measurement_source_concept_id END AS measurement_concept_id,",
            "            measurement_datetime,", 
            "            value_as_number",
            "        FROM measurement",
            "        WHERE person_id = {m$selected_person}\", .con = d$con)"
        )
    } else {
        code_lines <- c(code_lines,
            "    # Query measurement data for selected visit detail",
            "    sql <- glue::glue_sql(\"",
            "        SELECT",
            "            person_id,",
            "            visit_detail_id,", 
            "            CASE WHEN measurement_concept_id > 0 THEN measurement_concept_id ELSE measurement_source_concept_id END AS measurement_concept_id,",
            "            measurement_datetime,",
            "            value_as_number", 
            "        FROM measurement",
            "        WHERE visit_detail_id = {m$selected_visit_detail}\", .con = d$con)"
        )
    }
    
    code_lines <- c(code_lines,
        "    data <- DBI::dbGetQuery(d$con, sql)"
    )
    
    # Add concept filtering logic
    if (concepts_choice == "selected_concepts") {
        if (length(concepts) > 0) {
            concepts_string <- paste0("c(", paste(concepts, collapse = ", "), ")")
            code_lines <- c(code_lines,
                "",
                "    # Filter by selected concepts",
                paste0("    if (nrow(data) > 0) data <- data %>% dplyr::filter(measurement_concept_id %in% ", concepts_string, ")")
            )
        } else {
            # No concepts selected - should show no data
            code_lines <- c(code_lines,
                "",
                "    # No concepts selected - filter to show no data",
                "    data <- data %>% dplyr::filter(FALSE)"
            )
        }
    } else if (concepts_choice == "selected_concept_classes") {
        if (length(concept_classes) > 0) {
            classes_string <- paste0("c(\"", paste(concept_classes, collapse = "\", \""), "\")")
            code_lines <- c(code_lines,
                "",
                "    # Filter by selected concept classes",
                "    if (nrow(data) > 0) {",
                "        data <- data %>%",
                "            dplyr::collect() %>%",
                "            dplyr::inner_join(d$dataset_concept %>% dplyr::select(measurement_concept_id = concept_id, concept_class_id), by = \"measurement_concept_id\") %>%",
                paste0("            dplyr::filter(concept_class_id %in% ", classes_string, ") %>%"),
                "            dplyr::select(-concept_class_id)",
                "    }"
            )
        } else {
            # No concept classes selected - should show no data
            code_lines <- c(code_lines,
                "",
                "    # No concept classes selected - filter to show no data",
                "    data <- data %>% dplyr::filter(FALSE)"
            )
        }
    }
    
    # Add temporal processing logic based on column organization
    code_lines <- c(code_lines,
        "",
        "    # Check if data is available",
        "    if (nrow(data) == 0) {",
        "        error_message <- i18np$t(\"no_data_to_display\")",
        "    } else {",
        "        ",
        "        # Show datatable and datetime slider",
        "        shinyjs::hide(\"dynamic_output_div_%widget_id%\")",
        "        sapply(c(\"datatable_div_%widget_id%\", \"datetime_slider_div_%widget_id%\"), shinyjs::show)",
        "        ",
        "        # Get date range for temporal filtering"
    )
    
    # Add temporal range logic before processing modes
    code_lines <- c(code_lines, "")
    
    # Add visit occurrence date range query for person data source
    if (data_source == "person") {
        code_lines <- c(code_lines,
            "        range_sql <- glue::glue_sql(\"",
            "            SELECT",
            "                MIN(visit_start_datetime) AS min_datetime,", 
            "                MAX(visit_end_datetime) AS max_datetime",
            "            FROM visit_occurrence",
            "            WHERE person_id = {m$selected_person}\", .con = d$con)"
        )
    } else {
        code_lines <- c(code_lines,
            "        range_sql <- glue::glue_sql(\"",
            "            SELECT",
            "                MIN(visit_detail_start_datetime) AS min_datetime,", 
            "                MAX(visit_detail_end_datetime) AS max_datetime",
            "            FROM visit_detail",
            "            WHERE visit_detail_id = {m$selected_visit_detail}\", .con = d$con)"
        )
    }
    
    code_lines <- c(code_lines,
        "        datetime_range <- DBI::dbGetQuery(d$con, range_sql)",
        "        data_datetimes_range <- c(datetime_range$min_datetime, datetime_range$max_datetime)",
        "        m$data_datetimes_range_%widget_id% <- data_datetimes_range",
        "        ",
        "        # Determine filtering datetimes (with synchronization support)",
        "        datetimes <- data_datetimes_range",
        "        ",
        "        # Check for timeline synchronization",
        "        if (isTRUE(input$synchronize_timelines_%widget_id%)) {",
        "            if (!is.null(m$debounced_datetimes_timeline_%tab_id%)) {",
        "                if (length(m$debounced_datetimes_timeline_%tab_id%()) > 0) {",
        "                    datetimes <- m$debounced_datetimes_timeline_%tab_id%()",
        "                }",
        "            }",
        "        } else {",
        "            if (!is.null(m$debounced_datetime_slider_%widget_id%)) {",
        "                if (length(m$debounced_datetime_slider_%widget_id%()) > 0) {",
        "                    if (m$debounced_datetime_slider_%widget_id%()[[1]] >= data_datetimes_range[[1]] &",
        "                        m$debounced_datetime_slider_%widget_id%()[[2]] <= data_datetimes_range[[2]]) {",
        "                        datetimes <- m$debounced_datetime_slider_%widget_id%()",
        "                    }",
        "                }",
        "            }",
        "        }",
        "        ",
        "        if (length(datetimes) > 0) m$datetimes_%widget_id% <- datetimes",
        "        ",
        "        # Update datetime slider",
        "        updateSliderInput(",
        "            session, \"datetime_slider_%widget_id%\",",
        "            min = data_datetimes_range[[1]], max = data_datetimes_range[[2]],",
        "            value = datetimes,",
        "            timeFormat = ifelse(language == \"fr\", \"%d-%m-%Y %H:%M\", \"%Y-%m-%d %H:%M\"),",
        "            step = 3600000",
        "        )",
        ""
    )

    if (column_organization == "regular_intervals") {
        # Regular intervals mode - existing logic
        code_lines <- c(code_lines,
            "",
            "        # Mode: Regular intervals with aggregation",
            "        if (language == \"fr\") date_format <- \"%d-%m-%Y\"",
            "        else date_format <- \"%Y-%m-%d\""
        )
    
    if (data_source == "person") {
        code_lines <- c(code_lines,
            "        range_sql <- glue::glue_sql(\"",
            "            SELECT ",
            "                MIN(visit_start_datetime) AS min_datetime,",
            "                MAX(visit_end_datetime) AS max_datetime",
            "            FROM visit_occurrence",
            "            WHERE person_id = {m$selected_person}\", .con = d$con)"
        )
    } else {
        code_lines <- c(code_lines,
            "        range_sql <- glue::glue_sql(\"",
            "            SELECT",
            "                MIN(visit_detail_start_datetime) AS min_datetime,", 
            "                MAX(visit_detail_end_datetime) AS max_datetime",
            "            FROM visit_detail",
            "            WHERE visit_detail_id = {m$selected_visit_detail}\", .con = d$con)"
        )
    }
    
    code_lines <- c(code_lines,
        "        datetime_range <- DBI::dbGetQuery(d$con, range_sql)",
        "        datetimes <- c(datetime_range$min_datetime, datetime_range$max_datetime)",
        "",
        paste0("        # Create ", num_cols, " time intervals"),
        paste0("        interval_duration <- as.numeric(difftime(datetimes[[2]], datetimes[[1]], units = \"secs\")) / ", num_cols),
        "",
        paste0("        intervals <- tibble::tibble("),
        paste0("            interval = 0:(", num_cols, " - 1),"),
        "            interval_start = datetimes[[1]] + interval * interval_duration,",
        "            interval_end = interval_start + interval_duration",
        "        ) %>%",
        "        dplyr::mutate(",
        "            interval_label = paste(",
        "                format(interval_start, date_format),",
        "                \" <span style='color:#0084D8'>\", format(interval_start, \"%H:%M\"), \"</span>\",",
        "                \" \", tolower(i18np$t(\"to\")), \" \",",
        "                format(interval_end, date_format),",
        "                \" <span style='color:#0084D8'>\", format(interval_end, \"%H:%M\"), \"</span>\"",
        "            )",
        "        )",
        "",
        "        # Process and aggregate data",
        "        data <- data %>%",
        "            dplyr::collect() %>%",
        "            dplyr::left_join(",
        "                d$dataset_concept %>% ",
        "                dplyr::select(measurement_concept_id = concept_id, measurement_concept_name = concept_name),",
        "                by = \"measurement_concept_id\"",
        "            ) %>%",
        "            dplyr::mutate(measurement_concept_name = dplyr::if_else(is.na(measurement_concept_name), as.character(measurement_concept_id), measurement_concept_name)) %>%",
        "            dplyr::filter(measurement_datetime >= datetimes[[1]] & measurement_datetime <= datetimes[[2]]) %>%",
        "            dplyr::select(measurement_concept_name, measurement_datetime, value_as_number) %>%",
        "            dplyr::arrange(measurement_concept_name, measurement_datetime) %>%",
        "            dplyr::mutate(",
        "                interval = floor(as.numeric(difftime(measurement_datetime, datetimes[[1]], units = \"secs\")) / interval_duration)",
        "            ) %>%",
        "            dplyr::group_by(measurement_concept_name, interval) %>%",
        paste0("            dplyr::summarize(agg_value = round(", aggregate_fct, "(value_as_number, na.rm = TRUE), 1), n = sum(!is.na(value_as_number)), .groups = \"drop\") %>%"),
        "            dplyr::mutate(agg_value = paste0(agg_value, \" (n = \", n, \")\")) %>%",
        "            dplyr::right_join(intervals, by = \"interval\") %>%",
        "            dplyr::select(measurement_concept_name, interval_label, agg_value) %>%",
        "            tidyr::pivot_wider(names_from = interval_label, values_from = agg_value) %>%",
        "            dplyr::arrange(measurement_concept_name) %>%",
        "            dplyr::rename(!!i18np$t(\"concept\") := measurement_concept_name) %>%",
        "            dplyr::filter(!is.na(!!rlang::sym(i18np$t(\"concept\"))) & !!rlang::sym(i18np$t(\"concept\")) != \"\")",
        "",
        "        # Reorder date columns (works also with french date format)",
        "        date_cols <- names(data)[-1]",
        "        sorted_date_cols <- date_cols[order(as.Date(",
        "            gsub(\" <.*$\", \"\", date_cols),",
        "            format = date_format",
        "        ))]",
        "",
        "        data <- data %>%",
        "            dplyr::relocate(all_of(sorted_date_cols), .after = 1)",
        "",
        "        # Create DT datatable result",
        "        result <- DT::datatable(",
        "            data,",
        "            rownames = FALSE,",
        "            options = list(",
        "                dom = if (nrow(data) > 10) \"<'datatable_length'l><'top't><'bottom'p>\" else \"<'top't>\",",
        "                pageLength = if (nrow(data) > 10) 25 else 10,",
        "                paging = nrow(data) > 10,",
        "                compact = TRUE,",
        "                hover = TRUE",
        "            ),",
        "            escape = FALSE",
        "        )",
        "        ",
        "        # Define CSS styling JavaScript",
        "        css_js <- \"",
        "          $('.dataTable tbody tr td').css({",
        "            'height': '12px',",
        "            'padding': '2px 5px'",
        "          });",
        "          $('.dataTable thead tr td div .form-control').css('font-size', '12px');",
        "          $('.dataTable thead tr td').css('padding', '5px');",
        "          $('.dataTable tbody tr td:first-child').css({",
        "            'min-width': '100px',",
        "            'max-width': '100px',",
        "            'width': '100px',",
        "            'white-space': 'nowrap'",
        "          });",
        "          $('.dataTable thead tr th:first-child').css({",
        "            'min-width': '100px',",
        "            'max-width': '100px',",
        "            'width': '100px',",
        "            'white-space': 'nowrap'",
        "          });",
        "          $('.dataTable tbody td').each(function() {",
        "            var cellText = $(this).text();",
        "            $(this).attr('title', cellText);",
        "          });\"",
        "        ",
        "        # Apply callback with CSS styling",
        "        result <- result %>% htmlwidgets::onRender(paste0(",
        "          \"function(el, x) {\",",
        "          \"  var table = $(el).find('table').DataTable();\",",
        "          \"  table.on('draw.dt', function() {\", css_js, \"});\",",
        "          \"  setTimeout(function() {\", css_js, \"}, 200);\",",
        "          \"}\"",
        "        ))"
    )
    
    } else if (column_organization == "by_timestamp") {
        # By timestamp mode - one column per unique datetime
        code_lines <- c(code_lines,
            "",
            "        # Mode: By timestamp (no aggregation)",
            "        if (language == \"fr\") date_format <- \"%d-%m-%Y\"",
            "        else date_format <- \"%Y-%m-%d\"",
            "",
            "        # Process data without aggregation - one column per timestamp",
            "        data <- data %>%",
            "            dplyr::collect() %>%",
            "            dplyr::left_join(",
            "                d$dataset_concept %>% ",
            "                dplyr::select(measurement_concept_id = concept_id, measurement_concept_name = concept_name),",
            "                by = \"measurement_concept_id\"",
            "            ) %>%",
            "            dplyr::mutate(",
            "                measurement_concept_name = dplyr::if_else(is.na(measurement_concept_name), as.character(measurement_concept_id), measurement_concept_name),",
            "                formatted_datetime = paste0(",
            "                    format(measurement_datetime, date_format),",
            "                    \" <span style='color:#0084D8'>\", format(measurement_datetime, \"%H:%M\"), \"</span>\"",
            "                )",
            "            ) %>%",
            "            dplyr::filter(measurement_datetime >= datetimes[[1]] & measurement_datetime <= datetimes[[2]]) %>%",
            "            dplyr::select(measurement_concept_name, formatted_datetime, value_as_number, measurement_datetime) %>%",
            "            dplyr::arrange(measurement_concept_name, measurement_datetime) %>%",
            "            dplyr::select(-measurement_datetime) %>%",
            "            tidyr::pivot_wider(",
            "                names_from = formatted_datetime,",
            "                values_from = value_as_number,", 
            "                values_fn = function(x) paste(x, collapse = \" | \"),",
            "                names_sort = TRUE",
            "            ) %>%",
            "            dplyr::arrange(measurement_concept_name) %>%",
            "            dplyr::rename(!!i18np$t(\"concept\") := measurement_concept_name) %>%",
            "            dplyr::filter(!is.na(!!rlang::sym(i18np$t(\"concept\"))) & !!rlang::sym(i18np$t(\"concept\")) != \"\")",
            "",
            "        # Create DT datatable result with sorting only on first column",
            "        result <- DT::datatable(",
            "            data,",
            "            rownames = FALSE,",
            "            options = list(",
            "                dom = if (nrow(data) > 10) \"<'datatable_length'l><'top't><'bottom'p>\" else \"<'top't>\",",
            "                pageLength = if (nrow(data) > 10) 25 else 10,",
            "                paging = nrow(data) > 10,",
            "                compact = TRUE,",
            "                hover = TRUE,",
            "                # Only allow sorting on first column (Concept)",
            "                columnDefs = list(",
            "                    list(orderable = TRUE, targets = 0),",
            "                    list(orderable = FALSE, targets = '_all')",
            "                )",
            "            ),",
            "            escape = FALSE",
            "        )",
            "        ",
            "        # Define CSS styling JavaScript",
            "        css_js <- \"",
            "          $('.dataTable tbody tr td').css({",
            "            'height': '12px',",
            "            'padding': '2px 5px'",
            "          });",
            "          $('.dataTable thead tr td div .form-control').css('font-size', '12px');",
            "          $('.dataTable thead tr td').css('padding', '5px');",
            "          $('.dataTable tbody tr td:first-child').css({",
            "            'min-width': '100px',",
            "            'max-width': '100px',",
            "            'width': '100px',",
            "            'white-space': 'nowrap'",
            "          });",
            "          $('.dataTable thead tr th:first-child').css({",
            "            'min-width': '100px',",
            "            'max-width': '100px',",
            "            'width': '100px',",
            "            'white-space': 'nowrap'",
            "          });",
            "          $('.dataTable tbody td').each(function() {",
            "            var cellText = $(this).text();",
            "            $(this).attr('title', cellText);",
            "          });\"",
            "        ",
            "        # Apply callback with CSS styling",
            "        result <- result %>% htmlwidgets::onRender(paste0(",
            "          \"function(el, x) {\",",
            "          \"  var table = $(el).find('table').DataTable();\",",
            "          \"  table.on('draw.dt', function() {\", css_js, \"});\",",
            "          \"  setTimeout(function() {\", css_js, \"}, 200);\",",
            "          \"}\"",
            "        ))"
        )
    }
    
    # Close the data processing blocks  
    code_lines <- c(code_lines, "    }", "}")
    
    # Combine all code lines
    generated_code <- paste(code_lines, collapse = "\n")
    
    return(generated_code)
}

# ======================================
# AUTO-EXECUTION TRIGGERS
# ======================================

# Example: Auto-run code when data context changes
# Uncomment and customize based on your plugin's data dependencies

# Auto-run code when patient selection changes
observe_event(m$selected_person, {
    # Check if auto-run is enabled and data source matches
    if (!isTRUE(input$automatically_update_output_%widget_id%) || 
        length(input$data_source_%widget_id%) == 0 || 
        input$data_source_%widget_id% != "person") {
        return()
    }
    
    # Execute code when patient selection changes
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Auto-run code when visit detail selection changes
observe_event(m$selected_visit_detail, {
    # Check if auto-run is enabled and data source matches
    if (!isTRUE(input$automatically_update_output_%widget_id%) || 
        length(input$data_source_%widget_id%) == 0 || 
        input$data_source_%widget_id% != "visit_detail") {
        return()
    }
    
    # Execute code when visit detail selection changes
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# ======================================
# CODE EXECUTION ENGINE
# ======================================

# Main code execution handler
observe_event(input$run_code_%widget_id%, {
    
    # Initialize result variable
    result <- NULL
    error_message <- NULL
    
    # ====================
    # PATIENT SELECTION VALIDATION (EXAMPLE)
    # ====================
    # For plugins that require patient selection, check if a patient is selected
    # before executing code. This prevents errors and provides user-friendly messages.
    # 
    # EXAMPLE IMPLEMENTATION (commented out for template):
    # patient_selected <- TRUE
    # if (is.null(m$selected_person) || is.na(m$selected_person)) {
    #     patient_selected <- FALSE
    #     error_message <- i18np$t("select_patient")
    # }
    # 
    # USAGE PATTERN:
    # - Check patient selection before code execution
    # - Set error_message instead of using stop() in generated code
    # - Provide user-friendly messages via translations
    # - Display messages elegantly in UI (see UI message handling below)
    
    # ====================
    # HIDE ALL OUTPUTS INITIALLY
    # ====================
    # Hide all output containers before execution
    sapply(c("error_message_div_%widget_id%", "plot_div_%widget_id%", "table_div_%widget_id%", "datatable_div_%widget_id%", "dynamic_output_div_%widget_id%", "datetime_slider_div_%widget_id%"), shinyjs::hide)
    
    # ====================
    # EXECUTE USER CODE
    # ====================
    tryCatch({
        # Execute the R code in the current environment
        eval(parse(text = m$code_%widget_id%))
    }, error = function(e) {
        # Capture any execution errors
        error_message <<- paste(i18np$t("error_executing_code"), e$message, sep = ": ")
    })
    
    # ====================
    # HANDLE EXECUTION RESULTS
    # ====================
    
    # Show error message if execution failed or no output was generated
    if (!is.null(error_message) || is.null(result)) {
        
        display_message <- if (!is.null(error_message)) {
            error_message
        } else {
            i18np$t("no_output_generated")
        }
        
        # Display all messages with the same simple centered style
        output$dynamic_output_%widget_id% <- renderUI({
            div(
                style = "display: flex; justify-content: center; align-items: center; height: 100%; text-align: center; padding: 10px;",
                div(
                    style = "font-size: 14px; color: #6c757d;",
                    display_message
                )
            )
        })
        shinyjs::show("dynamic_output_div_%widget_id%")
        shinyjs::hide("error_message_div_%widget_id%")
        shinyjs::hide("plot_div_%widget_id%")
        shinyjs::hide("table_div_%widget_id%")
        shinyjs::hide("datatable_div_%widget_id%")
    }
    
    # Display output if execution was successful
    if (is.null(error_message) && !is.null(result)) {
        
        # Route output to appropriate renderer based on type
        # Customize this section based on your plugin's output types
        
        if ("ggplot" %in% class(result)) {
            # Handle ggplot objects
            output$plot_%widget_id% <- renderPlot(result)
            shinyjs::hide("error_message_div_%widget_id%")
            shinyjs::show("plot_div_%widget_id%")
            
        } else if ("plotly" %in% class(result) || "htmlwidget" %in% class(result)) {
            # Handle plotly/htmlwidget objects
            output$dynamic_output_%widget_id% <- renderUI(result)
            shinyjs::hide("error_message_div_%widget_id%")
            shinyjs::show("dynamic_output_div_%widget_id%")
            
        } else if ("datatables" %in% class(result)) {
            # Handle DT datatable objects
            output$datatable_%widget_id% <- DT::renderDT(result)
            shinyjs::hide("error_message_div_%widget_id%")
            shinyjs::show("datatable_div_%widget_id%")
            
        } else {
            # Handle other output types (text, HTML, etc.)
            output$dynamic_output_%widget_id% <- renderUI({
                if (is.character(result)) {
                    verbatimTextOutput(ns("text_display"))
                } else {
                    pre(capture.output(print(result)))
                }
            })
            shinyjs::hide("error_message_div_%widget_id%")
            shinyjs::show("dynamic_output_div_%widget_id%")
        }
    }
    
    # ====================
    # AUTO-NAVIGATION
    # ====================
    # Optional: automatically switch to output tab after execution
    if (isFALSE(input$output_and_settings_side_by_side_%widget_id%)) shinyjs::click("output_button_%widget_id%")
    
    # ====================
    # SAVE AFTER DISPLAY IF REQUESTED
    # ====================
    # Check if save was requested after display (from Display + Save button)
    if (exists("save_after_display_%widget_id%") && save_after_display_%widget_id%()) {
        # Reset the flag
        save_after_display_%widget_id%(FALSE)
        
        # Trigger the save process
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_configuration_trigger_%widget_id%', Math.random());"))
        
        # Notify user
        show_message_bar("modif_saved", "success")
    }
})

# ======================================
# DATETIME SLIDER MANAGEMENT
# ======================================

# Initialize reactive values for datetime slider
if (!exists("m$debounced_datetime_slider_%widget_id%")) {
    m$debounced_datetime_slider_%widget_id% <- reactiveVal()
    m$datetime_slider_%widget_id% <- reactiveVal()
    m$debounced_datetime_slider_%widget_id% <- reactive(m$datetime_slider_%widget_id%()) %>% debounce(500)
}

# Observe datetime slider changes
observe_event(input$datetime_slider_%widget_id%, {
    m$datetime_slider_%widget_id%(input$datetime_slider_%widget_id%)
})

# Auto-execute when datetime slider changes (debounced)
observe_event(m$debounced_datetime_slider_%widget_id%(), {
    # Check if auto-execution is enabled
    if (length(input$automatically_update_output_%widget_id%) > 0 && 
        isTRUE(input$automatically_update_output_%widget_id%)) {
        
        # Execute the code when slider changes
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})
