# ==========================================
# server_code.R - Code Editor Server Logic with Multiple Tabs
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 REQUIRES CUSTOMIZATION - PLUGIN IMPLEMENTATION  🔧                     ██
# ██                                                                            ██
# ██  This file has been customized for multiple tabbed ACE editors.           ██
# ██  Each tab has its own editor with synchronized navigation.                 ██
# ██  Tab synchronization between output_settings and code editor implemented. ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# ======================================
# INITIALIZATION
# ======================================

# Initialize code storage variables for each tab
m$code_import_data_%widget_id% <- ""
m$code_visualization_%widget_id% <- ""
m$code_statistics_%widget_id% <- ""
m$code_report_%widget_id% <- ""

# Current active code editor tab
current_code_tab_%widget_id% <- reactiveVal("import_data")

# Sub tabs definition
code_sub_tabs <- c("import_data", "visualization", "statistics", "report")

# Initialize ACE editors visibility - show only import_data by default
sapply(code_sub_tabs, function(sub_tab) {
    if (sub_tab == "import_data") {
        shinyjs::show(paste0("code_", sub_tab, "_div_%widget_id%"))
    } else {
        shinyjs::hide(paste0("code_", sub_tab, "_div_%widget_id%"))
    }
})

# Fix ACE editor rendering issues on startup
# Delay ensures DOM is fully loaded before triggering resize
shinyjs::delay(300, shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);"))

# ======================================
# TAB SYNCHRONIZATION - OUTPUT SETTINGS CONTROLS CODE EDITORS
# ======================================

# Show/hide code editor divs when output_settings tabs change
observe_event(input$current_figure_settings_tab_trigger_%widget_id%, {
    
    current_sub_tab <- 
        input$current_figure_settings_tab_%widget_id% %>%
        gsub(paste0(id, "-"), "", .) %>%
        gsub("_%widget_id%", "", .)
    
    # Update code editor visibility
    sapply(code_sub_tabs, function(sub_tab) {
        if (current_sub_tab == sub_tab) {
            shinyjs::delay(50, shinyjs::show(paste0("code_", sub_tab, "_div_%widget_id%")))
        } else {
            shinyjs::hide(paste0("code_", sub_tab, "_div_%widget_id%"))
        }
    })
    
    # Update current tab
    current_code_tab_%widget_id%(current_sub_tab)
})

# Initialize default tab visibility when UI loads
observe({
    # Only run if UI element is ready
    if (is.null(session$clientData$output_code_import_data_div_%widget_id%_width)) {
        return()
    }
    
    # Ensure import_data tab is visible by default
    if (is.null(input$current_figure_settings_tab_%widget_id%)) {
        shinyjs::delay(100, {
            shinyjs::show("code_import_data_div_%widget_id%")
            sapply(c("visualization", "statistics", "report"), function(tab) {
                shinyjs::hide(paste0("code_", tab, "_div_%widget_id%"))
            })
        })
    }
})

# ======================================
# CODE EDITOR KEYBOARD SHORTCUTS FOR EACH TAB
# ======================================

# Import Data Editor
observe_event(input$code_import_data_%widget_id%_comment, {
    editor_toggle_comments(
        input_id = "code_import_data_%widget_id%", 
        code = input$code_import_data_%widget_id%,
        selection = input$code_import_data_%widget_id%_comment$range, 
        session = session
    )
})

observe_event(input$code_import_data_%widget_id%_run_all, {
    if ("projects_widgets_console" %in% user_accesses) {
        m$code_import_data_%widget_id% <- input$code_import_data_%widget_id%
        current_code_tab_%widget_id%("import_data")
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

observe_event(input$code_import_data_%widget_id%_save, {
    m$code_import_data_%widget_id% <- input$code_import_data_%widget_id%
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_output_settings_and_code_%widget_id%', Math.random());"))
})

# Visualization Editor
observe_event(input$code_visualization_%widget_id%_comment, {
    editor_toggle_comments(
        input_id = "code_visualization_%widget_id%", 
        code = input$code_visualization_%widget_id%,
        selection = input$code_visualization_%widget_id%_comment$range, 
        session = session
    )
})

observe_event(input$code_visualization_%widget_id%_run_all, {
    if ("projects_widgets_console" %in% user_accesses) {
        m$code_visualization_%widget_id% <- input$code_visualization_%widget_id%
        current_code_tab_%widget_id%("visualization")
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

observe_event(input$code_visualization_%widget_id%_save, {
    m$code_visualization_%widget_id% <- input$code_visualization_%widget_id%
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_output_settings_and_code_%widget_id%', Math.random());"))
})

# Statistics Editor
observe_event(input$code_statistics_%widget_id%_comment, {
    editor_toggle_comments(
        input_id = "code_statistics_%widget_id%", 
        code = input$code_statistics_%widget_id%,
        selection = input$code_statistics_%widget_id%_comment$range, 
        session = session
    )
})

observe_event(input$code_statistics_%widget_id%_run_all, {
    if ("projects_widgets_console" %in% user_accesses) {
        m$code_statistics_%widget_id% <- input$code_statistics_%widget_id%
        current_code_tab_%widget_id%("statistics")
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

observe_event(input$code_statistics_%widget_id%_save, {
    m$code_statistics_%widget_id% <- input$code_statistics_%widget_id%
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_output_settings_and_code_%widget_id%', Math.random());"))
})

# Report Editor
observe_event(input$code_report_%widget_id%_comment, {
    editor_toggle_comments(
        input_id = "code_report_%widget_id%", 
        code = input$code_report_%widget_id%,
        selection = input$code_report_%widget_id%_comment$range, 
        session = session
    )
})

observe_event(input$code_report_%widget_id%_run_all, {
    if ("projects_widgets_console" %in% user_accesses) {
        m$code_report_%widget_id% <- input$code_report_%widget_id%
        current_code_tab_%widget_id%("report")
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

observe_event(input$code_report_%widget_id%_save, {
    m$code_report_%widget_id% <- input$code_report_%widget_id%
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
        
        # Get current tab from figure settings
        current_sub_tab <- if (length(input$current_figure_settings_tab_%widget_id%) > 0) {
            input$current_figure_settings_tab_%widget_id% %>%
                gsub(paste0(id, "-"), "", .) %>%
                gsub("_%widget_id%", "", .)
        } else {
            "import_data"  # Default tab
        }
        
        # Get dataset selection
        selected_dataset <- if (length(input$selected_dataset_%widget_id%) > 0) {
            input$selected_dataset_%widget_id%
        } else {
            NULL
        }
        
        # Get visualization parameters
        x_var <- if (length(input$x_axis_%widget_id%) > 0) {
            input$x_axis_%widget_id%
        } else {
            NULL
        }
        
        y_var <- if (length(input$y_axis_%widget_id%) > 0) {
            input$y_axis_%widget_id%
        } else {
            NULL
        }
        
        plot_type <- if (length(input$plot_type_%widget_id%) > 0) {
            input$plot_type_%widget_id%
        } else {
            "histogram"
        }
        
        plot_title <- if (length(input$plot_title_%widget_id%) > 0) {
            input$plot_title_%widget_id%
        } else {
            "Data Analysis Results"
        }
        
        x_legend <- if (length(input$x_legend_%widget_id%) > 0) {
            input$x_legend_%widget_id%
        } else {
            ""
        }
        
        y_legend <- if (length(input$y_legend_%widget_id%) > 0) {
            input$y_legend_%widget_id%
        } else {
            ""
        }
        
        # Get statistics parameters
        statistics_type <- if (length(input$statistics_type_%widget_id%) > 0) {
            input$statistics_type_%widget_id%
        } else {
            "table_one"
        }
        
        table_variables <- if (length(input$table_variables_%widget_id%) > 0) {
            input$table_variables_%widget_id%
        } else {
            NULL
        }
        
        table_title <- if (length(input$table_title_%widget_id%) > 0) {
            input$table_title_%widget_id%
        } else {
            "Table 1. Statistiques descriptives"
        }
        
        grouping_var <- if (length(input$grouping_variable_%widget_id%) > 0) {
            input$grouping_variable_%widget_id%
        } else {
            NULL
        }
        
        var1 <- if (length(input$variable_1_%widget_id%) > 0) {
            input$variable_1_%widget_id%
        } else {
            NULL
        }
        
        var2 <- if (length(input$variable_2_%widget_id%) > 0) {
            input$variable_2_%widget_id%
        } else {
            NULL
        }
        
        # Generate R code based on current configuration and tab
        generated_code <- generate_output_code_%widget_id%(
            current_tab = current_sub_tab,
            selected_dataset = selected_dataset,
            x_var = x_var,
            y_var = y_var,
            plot_type = plot_type,
            plot_title = plot_title,
            x_legend = x_legend,
            y_legend = y_legend,
            statistics_type = statistics_type,
            table_variables = table_variables,
            table_title = table_title,
            grouping_var = grouping_var,
            var1 = var1,
            var2 = var2
        )
        
        # Update the appropriate ACE editor with generated code
        editor_id <- paste0("code_", current_sub_tab, "_%widget_id%")
        shinyAce::updateAceEditor(session, editor_id, value = generated_code)
        
        # Store code in the appropriate variable
        if (current_sub_tab == "import_data") {
            m$code_import_data_%widget_id% <- generated_code
        } else if (current_sub_tab == "visualization") {
            m$code_visualization_%widget_id% <- generated_code
        } else if (current_sub_tab == "statistics") {
            m$code_statistics_%widget_id% <- generated_code
        } else if (current_sub_tab == "report") {
            m$code_report_%widget_id% <- generated_code
        }
        
        # Also update the main code variable for execution
        m$code_%widget_id% <- generated_code
        
        # Only trigger execution if code is not empty
        if (generated_code != "") {
            # Trigger execution
            shinyjs::delay(500, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());")))
        } else {
            # If code is empty (plot not feasible), just stay on helper UI
            if (current_sub_tab == "visualization") {
                shinyjs::hide("plot_div_%widget_id%")
                shinyjs::show("visualization_helper_div_%widget_id%")
            }
        }
    }
    # ====================
    # MANUAL CODE EXECUTION
    # ====================
    else if ("projects_widgets_console" %in% user_accesses) {
        # If on code tab, run whatever is currently in the active editor
        current_tab_name <- current_code_tab_%widget_id%()
        
        if (current_tab_name == "import_data") {
            m$code_%widget_id% <- input$code_import_data_%widget_id%
            m$code_import_data_%widget_id% <- input$code_import_data_%widget_id%
        } else if (current_tab_name == "visualization") {
            m$code_%widget_id% <- input$code_visualization_%widget_id%
            m$code_visualization_%widget_id% <- input$code_visualization_%widget_id%
        } else if (current_tab_name == "statistics") {
            m$code_%widget_id% <- input$code_statistics_%widget_id%
            m$code_statistics_%widget_id% <- input$code_statistics_%widget_id%
        } else if (current_tab_name == "report") {
            m$code_%widget_id% <- input$code_report_%widget_id%
            m$code_report_%widget_id% <- input$code_report_%widget_id%
        }
        
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

# ======================================
# CODE GENERATION FUNCTION
# ======================================

# IMPLEMENT THIS FUNCTION FOR YOUR SPECIFIC PLUGIN
# This function should generate R code based on user settings from the UI
generate_output_code_%widget_id% <- function(current_tab = "import_data", selected_dataset = NULL, x_var = NULL, y_var = NULL, plot_type = "histogram", plot_title = "Data Analysis Results", x_legend = "", y_legend = "", statistics_type = "table_one", table_variables = NULL, table_title = "Table 1. Statistiques descriptives", grouping_var = NULL, var1 = NULL, var2 = NULL) {
    
    code_lines <- c()
    
    # Generate code based on current tab
    if (current_tab == "import_data") {
        # IMPORT DATA TAB - Generate datatable code
        if (is.null(selected_dataset)) {
            code_lines <- c(code_lines, "# No dataset selected")
            code_lines <- c(code_lines, "result <- NULL")
        } else {
            code_lines <- c(code_lines,
                "# Read selected dataset", 
                "data_folder <- if (!is.null(m$selected_project_path)) file.path(m$selected_project_path, 'data') else file.path(m$app_folder, 'temp_files', 'projects_data')",
                paste0("file_path <- file.path(data_folder, '", selected_dataset, "')"),
                "data <- read.csv(file_path, stringsAsFactors = FALSE)",
                "",
                "# Create datatable",
                "result <- DT::datatable(",
                "    data,",
                "    options = list(",
                "        pageLength = 25,",
                "        scrollX = TRUE,",
                "        scrollY = '400px',",
                "        dom = 'Brtip',",
                "        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),",
                "        searching = FALSE",
                "    ),",
                "    class = 'display nowrap compact',",
                "    filter = 'none',",
                "    rownames = FALSE",
                ")"
            )
        }
        
    } else if (current_tab == "visualization") {
        # VISUALIZATION TAB - Generate plot code
        if (is.null(selected_dataset)) {
            code_lines <- c(code_lines, "# No dataset selected")
            code_lines <- c(code_lines, "result <- NULL")
        } else if (is.null(x_var) || x_var == "") {
            code_lines <- c(code_lines, "# No X variable selected")
            code_lines <- c(code_lines, "result <- NULL")
        } else {
            # Check if plot is feasible with current variables
            data_folder <- if (!is.null(m$selected_project_path)) file.path(m$selected_project_path, "data") else file.path(m$app_folder, "temp_files", "projects_data")
            file_path <- file.path(data_folder, selected_dataset)
            if (file.exists(file_path)) {
                data <- read.csv(file_path, stringsAsFactors = FALSE)
                x_type <- if (is.numeric(data[[x_var]])) "numeric" else if (is.factor(data[[x_var]])) "factor" else if (is.character(data[[x_var]])) "character" else "other"
                y_type <- if (!is.null(y_var) && y_var != "") {
                    if (is.numeric(data[[y_var]])) "numeric" else if (is.factor(data[[y_var]])) "factor" else if (is.character(data[[y_var]])) "character" else "other"
                } else {
                    NULL
                }
                
                # Check feasibility
                is_feasible <- TRUE
                if (is.null(y_type)) {
                    # Single variable plots
                    if (plot_type == "histogram") {
                        is_feasible <- (x_type == "numeric")
                    } else if (plot_type == "scatter") {
                        is_feasible <- FALSE  # Scatter needs both variables
                    }
                } else {
                    # Two variable plots
                    if (plot_type == "histogram") {
                        is_feasible <- FALSE  # Histogram doesn't work with two variables
                    } else if (plot_type == "scatter") {
                        is_feasible <- (x_type == "numeric" && y_type == "numeric")
                    }
                }
                
                if (!is_feasible) {
                    # Don't generate code if plot is not feasible - return empty code
                    return("")
                }
            }
            code_lines <- c(code_lines,
                "# Read selected dataset",
                "data_folder <- if (!is.null(m$selected_project_path)) file.path(m$selected_project_path, 'data') else file.path(m$app_folder, 'temp_files', 'projects_data')",
                paste0("file_path <- file.path(data_folder, '", selected_dataset, "')"),
                "data <- read.csv(file_path, stringsAsFactors = FALSE)",
                "",
                "# Generate plot"
            )
            
            if (plot_type == "histogram") {
                code_lines <- c(code_lines,
                    "# Check if variable is numeric for histogram",
                    paste0("if (!is.numeric(data[['", x_var, "']])) {"),
                    paste0("    stop('Variable ", x_var, " is not numeric. Histograms require numeric variables. Try using a bar plot instead.')"),
                    "}",
                    "",
                    paste0("result <- ggplot2::ggplot(data, ggplot2::aes(x = `", x_var, "`)) +"),
                    "    ggplot2::geom_histogram(bins = 30, fill = 'steelblue', alpha = 0.7) +",
                    paste0("    ggplot2::labs(title = '", plot_title, "', x = '", if(x_legend != "") x_legend else x_var, "', y = '", if(y_legend != "") y_legend else "Count", "') +"),
                    "    ggplot2::theme_minimal() +",
                    "    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))"
                )
            } else if (plot_type == "scatter") {
                if (is.null(y_var) || y_var == "") {
                    code_lines <- c(code_lines, "stop('Scatter plot requires both X and Y variables')")
                } else {
                    code_lines <- c(code_lines,
                        paste0("result <- ggplot2::ggplot(data, ggplot2::aes(x = `", x_var, "`, y = `", y_var, "`)) +"),
                        "    ggplot2::geom_point(color = 'steelblue', alpha = 0.7) +",
                        paste0("    ggplot2::labs(title = '", plot_title, "', x = '", if(x_legend != "") x_legend else x_var, "', y = '", if(y_legend != "") y_legend else y_var, "') +"),
                        "    ggplot2::theme_minimal() +",
                        "    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))"
                    )
                }
            } else if (plot_type == "boxplot") {
                if (is.null(y_var) || y_var == "") {
                    code_lines <- c(code_lines,
                        paste0("result <- ggplot2::ggplot(data, ggplot2::aes(y = `", x_var, "`)) +"),
                        "    ggplot2::geom_boxplot(fill = 'steelblue', alpha = 0.7) +",
                        paste0("    ggplot2::labs(title = '", plot_title, "', y = '", if(x_legend != "") x_legend else x_var, "') +"),
                        "    ggplot2::theme_minimal() +",
                        "    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))"
                    )
                } else {
                    code_lines <- c(code_lines,
                        paste0("result <- ggplot2::ggplot(data, ggplot2::aes(x = `", x_var, "`, y = `", y_var, "`)) +"),
                        "    ggplot2::geom_boxplot(fill = 'steelblue', alpha = 0.7) +",
                        paste0("    ggplot2::labs(title = '", plot_title, "', x = '", if(x_legend != "") x_legend else x_var, "', y = '", if(y_legend != "") y_legend else y_var, "') +"),
                        "    ggplot2::theme_minimal() +",
                        "    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))"
                    )
                }
            } else if (plot_type == "barplot") {
                if (is.null(y_var) || y_var == "") {
                    code_lines <- c(code_lines,
                        paste0("result <- ggplot2::ggplot(data, ggplot2::aes(x = `", x_var, "`)) +"),
                        "    ggplot2::geom_bar(fill = 'steelblue', alpha = 0.7) +",
                        paste0("    ggplot2::labs(title = '", plot_title, "', x = '", if(x_legend != "") x_legend else x_var, "', y = '", if(y_legend != "") y_legend else "Count", "') +"),
                        "    ggplot2::theme_minimal() +",
                        "    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), plot.title = ggplot2::element_text(hjust = 0.5))"
                    )
                } else {
                    code_lines <- c(code_lines,
                        paste0("result <- ggplot2::ggplot(data, ggplot2::aes(x = `", x_var, "`, y = `", y_var, "`)) +"),
                        "    ggplot2::geom_bar(stat = 'identity', fill = 'steelblue', alpha = 0.7) +",
                        paste0("    ggplot2::labs(title = '", plot_title, "', x = '", if(x_legend != "") x_legend else x_var, "', y = '", if(y_legend != "") y_legend else y_var, "') +"),
                        "    ggplot2::theme_minimal() +",
                        "    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), plot.title = ggplot2::element_text(hjust = 0.5))"
                    )
                }
            }
        }
        
    } else if (current_tab == "statistics") {
        # STATISTICS TAB - Generate statistical analysis code
        if (is.null(selected_dataset)) {
            code_lines <- c(code_lines, "# No dataset selected")
            code_lines <- c(code_lines, "result <- NULL")
        } else {
            code_lines <- c(code_lines,
                "# Read selected dataset",
                "data_folder <- if (!is.null(m$selected_project_path)) file.path(m$selected_project_path, 'data') else file.path(m$app_folder, 'temp_files', 'projects_data')",
                paste0("file_path <- file.path(data_folder, '", selected_dataset, "')"),
                "data <- read.csv(file_path, stringsAsFactors = FALSE)",
                ""
            )
            
            if (statistics_type == "table_one") {
                code_lines <- c(code_lines,
                    "# Check if gtsummary package is available",
                    "if (!requireNamespace('gtsummary', quietly = TRUE)) {",
                    "    stop('Le package gtsummary est requis pour générer Table 1. Installez-le avec: install.packages(\"gtsummary\")')",
                    "}",
                    ""
                )
                
                # Handle variable selection
                if (!is.null(table_variables) && length(table_variables) > 0) {
                    vars_code <- paste0("c(", paste0("\"", table_variables, "\"", collapse = ", "), ")")
                    data_selection <- paste0("data %>% dplyr::select(all_of(", vars_code, ")")
                    if (!is.null(grouping_var) && grouping_var != "") {
                        data_selection <- paste0(data_selection, ", `", grouping_var, "`)")
                    } else {
                        data_selection <- paste0(data_selection, ")")
                    }
                    code_lines <- c(code_lines,
                        paste0("# Select specific variables for Table 1"),
                        paste0("selected_data <- ", data_selection),
                        ""
                    )
                    data_var <- "selected_data"
                } else {
                    data_var <- "data"
                }
                
                if (is.null(grouping_var) || grouping_var == "") {
                    code_lines <- c(code_lines,
                        "# Generate Table 1 (descriptive statistics)",
                        paste0("result <- ", data_var, " %>%"),
                        "    gtsummary::tbl_summary(",
                        "        statistic = list(",
                        "            gtsummary::all_continuous() ~ \"{mean} ({sd})\",",
                        "            gtsummary::all_categorical() ~ \"{n} ({p}%)\"",
                        "        ),",
                        "        missing_text = \"Manquant\"",
                        "    ) %>%",
                        "    gtsummary::modify_header(label ~ \"**Variable**\") %>%",
                        paste0("    gtsummary::modify_caption(\"**", table_title, "**\")")
                    )
                } else {
                    code_lines <- c(code_lines,
                        "# Generate Table 1 stratified by grouping variable",
                        paste0("result <- ", data_var, " %>%"),
                        paste0("    gtsummary::tbl_summary("),
                        paste0("        by = `", grouping_var, "`,"),
                        "        statistic = list(",
                        "            gtsummary::all_continuous() ~ \"{mean} ({sd})\",",
                        "            gtsummary::all_categorical() ~ \"{n} ({p}%)\"",
                        "        ),",
                        "        missing_text = \"Manquant\"",
                        "    ) %>%",
                        "    gtsummary::add_p(test = list(",
                        "        gtsummary::all_continuous() ~ \"t.test\",",
                        "        gtsummary::all_categorical() ~ \"chisq.test\"",
                        "    )) %>%",
                        "    gtsummary::add_overall() %>%",
                        "    gtsummary::modify_header(label ~ \"**Variable**\") %>%",
                        paste0("    gtsummary::modify_caption(\"**", table_title, "**\")")
                    )
                }
            } else if (statistics_type == "variable_comparison") {
                # Variable comparison uses helper UI instead of generating code
                # Return empty code - the helper UI will be shown instead
                return("")
            }
        }
        
    } else if (current_tab == "report") {
        # REPORT TAB - Generate report code
        code_lines <- c(code_lines,
            "# Report generation placeholder",
            "result <- 'Report functionality will be implemented here'"
        )
    }
    
    # Combine all code lines
    generated_code <- paste(code_lines, collapse = "\n")
    
    return(generated_code)
}

# ======================================
# AUTO-EXECUTION TRIGGERS
# ======================================

# No auto-execution for aggregated data plugins
# Auto-update only occurs when loading configuration files

# ======================================
# CODE EXECUTION ENGINE
# ======================================

# Main code execution handler
observe_event(input$run_code_%widget_id%, {
    
    # Initialize result variable
    result <- NULL
    error_message <- NULL
    
    # ====================
    # HIDE ALL OUTPUTS INITIALLY
    # ====================
    # Hide all output containers before execution
    sapply(c("error_message_div_%widget_id%", "plot_div_%widget_id%", "table_div_%widget_id%", "datatable_div_%widget_id%", "dynamic_output_div_%widget_id%", "visualization_helper_div_%widget_id%"), shinyjs::hide)
    
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
        shinyjs::hide("visualization_helper_div_%widget_id%")
    }
    
    # Display output if execution was successful
    if (is.null(error_message) && !is.null(result)) {
        
        # Get current tab to route output appropriately
        current_tab_name <- current_code_tab_%widget_id%()
        
        # Route output to appropriate renderer based on tab and result type
        if (current_tab_name == "import_data") {
            # Import tab - always show datatable
            if ("datatables" %in% class(result)) {
                output$datatable_%widget_id% <- DT::renderDT(result)
            } else {
                # Convert other results to datatable if possible
                output$dynamic_output_%widget_id% <- renderUI({
                    div("Import data result:", br(), pre(capture.output(print(result))))
                })
            }
            # Show only datatable for import tab
            shinyjs::hide("error_message_div_%widget_id%")
            shinyjs::show("datatable_div_%widget_id%")
            shinyjs::hide("plot_div_%widget_id%")
            shinyjs::hide("table_div_%widget_id%")
            
        } else if (current_tab_name == "visualization") {
            # Visualization tab - show plots
            if ("ggplot" %in% class(result)) {
                output$plot_%widget_id% <- renderPlot(result)
            } else if ("plotly" %in% class(result) || "htmlwidget" %in% class(result)) {
                output$dynamic_output_%widget_id% <- renderUI(result)
                # For interactive plots, show in dynamic output instead
                shinyjs::hide("error_message_div_%widget_id%")
                shinyjs::hide("datatable_div_%widget_id%")
                shinyjs::hide("plot_div_%widget_id%")
                shinyjs::hide("table_div_%widget_id%")
                shinyjs::hide("visualization_helper_div_%widget_id%")
                shinyjs::show("dynamic_output_div_%widget_id%")
                
                return()
            }
            # Show only plot for visualization tab (hide helper)
            shinyjs::hide("error_message_div_%widget_id%")
            shinyjs::hide("datatable_div_%widget_id%")
            shinyjs::hide("visualization_helper_div_%widget_id%")
            shinyjs::show("plot_div_%widget_id%")
            shinyjs::hide("table_div_%widget_id%")
            
            
        } else if (current_tab_name == "statistics") {
            # Statistics tab - show tables/text output
            if ("gt_tbl" %in% class(result) || "gtsummary" %in% class(result)) {
                # Handle gtsummary tables
                output$table_%widget_id% <- renderUI({
                    result %>% as_gt() %>% gt::as_raw_html()
                })
                shinyjs::hide("error_message_div_%widget_id%")
                shinyjs::hide("datatable_div_%widget_id%")
                shinyjs::hide("plot_div_%widget_id%")
                shinyjs::show("table_div_%widget_id%")
            } else if ("datatables" %in% class(result)) {
                # DT tables for statistics
                output$datatable_%widget_id% <- DT::renderDT(result)
                shinyjs::hide("error_message_div_%widget_id%")
                shinyjs::show("datatable_div_%widget_id%")
                shinyjs::hide("plot_div_%widget_id%")
                shinyjs::hide("table_div_%widget_id%")
            } else {
                # Other statistics output
                output$dynamic_output_%widget_id% <- renderUI({
                    if (is.character(result)) {
                        verbatimTextOutput(ns("text_display"))
                    } else {
                        pre(capture.output(print(result)))
                    }
                })
                shinyjs::hide("error_message_div_%widget_id%")
                shinyjs::hide("datatable_div_%widget_id%")
                shinyjs::hide("plot_div_%widget_id%")
                shinyjs::hide("table_div_%widget_id%")
                shinyjs::show("dynamic_output_div_%widget_id%")
            }
            
        } else if (current_tab_name == "report") {
            # Report tab - show in dynamic output
            output$dynamic_output_%widget_id% <- renderUI({
                if (is.character(result)) {
                    HTML(result)
                } else {
                    pre(capture.output(print(result)))
                }
            })
            shinyjs::hide("error_message_div_%widget_id%")
            shinyjs::hide("datatable_div_%widget_id%")
            shinyjs::hide("plot_div_%widget_id%")
            shinyjs::hide("table_div_%widget_id%")
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