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
    req(session$clientData$output_code_import_data_div_%widget_id%_width)
    
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
        
        # Get statistics parameters
        statistics_type <- if (length(input$statistics_type_%widget_id%) > 0) {
            input$statistics_type_%widget_id%
        } else {
            "table_one"
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
            statistics_type = statistics_type,
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
        
        # Trigger execution
        shinyjs::delay(500, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());")))
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
generate_output_code_%widget_id% <- function(current_tab = "import_data", selected_dataset = NULL, x_var = NULL, y_var = NULL, plot_type = "histogram", plot_title = "Data Analysis Results", statistics_type = "table_one", grouping_var = NULL, var1 = NULL, var2 = NULL) {
    
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
                "data_folder <- file.path(m$app_folder, 'temp_files', 'projects_data')",
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
            code_lines <- c(code_lines,
                "# Read selected dataset",
                "data_folder <- file.path(m$app_folder, 'temp_files', 'projects_data')",
                paste0("file_path <- file.path(data_folder, '", selected_dataset, "')"),
                "data <- read.csv(file_path, stringsAsFactors = FALSE)",
                "",
                "# Check if variable is numeric for histogram",
                paste0("if (!is.numeric(data[['", x_var, "']])) {"),
                paste0("    stop('Variable ", x_var, " is not numeric. Histograms require numeric variables. Try using a bar plot instead.')"),
                "}",
                "",
                "# Generate plot"
            )
            
            if (plot_type == "histogram") {
                code_lines <- c(code_lines,
                    paste0("result <- ggplot2::ggplot(data, ggplot2::aes(x = `", x_var, "`)) +"),
                    "    ggplot2::geom_histogram(bins = 30, fill = 'steelblue', alpha = 0.7) +",
                    paste0("    ggplot2::labs(title = '", plot_title, "', x = '", x_var, "', y = 'Count') +"),
                    "    ggplot2::theme_minimal()"
                )
            } else if (plot_type == "scatter") {
                if (is.null(y_var) || y_var == "") {
                    code_lines <- c(code_lines, "stop('Scatter plot requires both X and Y variables')")
                } else {
                    code_lines <- c(code_lines,
                        paste0("result <- ggplot2::ggplot(data, ggplot2::aes(x = `", x_var, "`, y = `", y_var, "`)) +"),
                        "    ggplot2::geom_point(color = 'steelblue', alpha = 0.7) +",
                        paste0("    ggplot2::labs(title = '", plot_title, "', x = '", x_var, "', y = '", y_var, "') +"),
                        "    ggplot2::theme_minimal()"
                    )
                }
            } else if (plot_type == "boxplot") {
                if (is.null(y_var) || y_var == "") {
                    code_lines <- c(code_lines,
                        paste0("result <- ggplot2::ggplot(data, ggplot2::aes(y = `", x_var, "`)) +"),
                        "    ggplot2::geom_boxplot(fill = 'steelblue', alpha = 0.7) +",
                        paste0("    ggplot2::labs(title = '", plot_title, "', y = '", x_var, "') +"),
                        "    ggplot2::theme_minimal()"
                    )
                } else {
                    code_lines <- c(code_lines,
                        paste0("result <- ggplot2::ggplot(data, ggplot2::aes(x = `", x_var, "`, y = `", y_var, "`)) +"),
                        "    ggplot2::geom_boxplot(fill = 'steelblue', alpha = 0.7) +",
                        paste0("    ggplot2::labs(title = '", plot_title, "', x = '", x_var, "', y = '", y_var, "') +"),
                        "    ggplot2::theme_minimal()"
                    )
                }
            } else if (plot_type == "barplot") {
                if (is.null(y_var) || y_var == "") {
                    code_lines <- c(code_lines,
                        paste0("result <- ggplot2::ggplot(data, ggplot2::aes(x = `", x_var, "`)) +"),
                        "    ggplot2::geom_bar(fill = 'steelblue', alpha = 0.7) +",
                        paste0("    ggplot2::labs(title = '", plot_title, "', x = '", x_var, "', y = 'Count') +"),
                        "    ggplot2::theme_minimal() +",
                        "    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))"
                    )
                } else {
                    code_lines <- c(code_lines,
                        paste0("result <- ggplot2::ggplot(data, ggplot2::aes(x = `", x_var, "`, y = `", y_var, "`)) +"),
                        "    ggplot2::geom_bar(stat = 'identity', fill = 'steelblue', alpha = 0.7) +",
                        paste0("    ggplot2::labs(title = '", plot_title, "', x = '", x_var, "', y = '", y_var, "') +"),
                        "    ggplot2::theme_minimal() +",
                        "    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))"
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
                "data_folder <- file.path(m$app_folder, 'temp_files', 'projects_data')",
                paste0("file_path <- file.path(data_folder, '", selected_dataset, "')"),
                "data <- read.csv(file_path, stringsAsFactors = FALSE)",
                ""
            )
            
            if (statistics_type == "table_one") {
                if (is.null(grouping_var) || grouping_var == "") {
                    code_lines <- c(code_lines,
                        "# Generate Table 1 without grouping",
                        "tbl <- data %>%",
                        "    gtsummary::tbl_summary()",
                        "",
                        "result <- tbl"
                    )
                } else {
                    code_lines <- c(code_lines,
                        "# Generate Table 1 with grouping",
                        paste0("tbl <- data %>%"),
                        paste0("    gtsummary::tbl_summary(by = `", grouping_var, "`) %>%"),
                        "    gtsummary::add_p()",
                        "",
                        "result <- tbl"
                    )
                }
            } else if (statistics_type == "variable_comparison") {
                if (is.null(var1) || var1 == "" || is.null(var2) || var2 == "") {
                    code_lines <- c(code_lines, "# Please select two variables for comparison")
                    code_lines <- c(code_lines, "result <- NULL")
                } else {
                    code_lines <- c(code_lines,
                        "# Variable comparison analysis",
                        paste0("comparison_data <- data %>% dplyr::select(`", var1, "`, `", var2, "`)"),
                        "",
                        "# Basic correlation if both numeric",
                        "if (is.numeric(data[['", var1, "']]) && is.numeric(data[['", var2, "']])) {",
                        "    correlation <- cor(data[['", var1, "']], data[['", var2, "']], use = 'complete.obs')",
                        "    cat('Correlation between ", var1, " and ", var2, ":', round(correlation, 3), '\\n')",
                        "}",
                        "",
                        "# Summary table",
                        "result <- DT::datatable(",
                        "    comparison_data,",
                        "    options = list(pageLength = 10, scrollX = TRUE),",
                        "    rownames = FALSE",
                        ")"
                    )
                }
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

# Example: Auto-run code when data context changes
observe_event(m$selected_person, {
    # Check if auto-run is enabled
    if (!isTRUE(input$auto_update_%widget_id%)) {
        return()
    }
    
    # Execute code when data context changes
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
    # HIDE ALL OUTPUTS INITIALLY
    # ====================
    # Hide all output containers before execution
    sapply(c("error_message_div_%widget_id%", "plot_div_%widget_id%", "table_div_%widget_id%", "datatable_div_%widget_id%", "dynamic_output_div_%widget_id%"), shinyjs::hide)
    
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