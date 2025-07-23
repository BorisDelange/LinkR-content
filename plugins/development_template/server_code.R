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
# DATA ANALYSIS PLUGINS:
#   - Generate statistical analysis code (t-tests, regression, ANOVA)
#   - Create summary statistics and descriptive analysis
#   - Build model fitting and validation code
# 
# VISUALIZATION PLUGINS:
#   - Generate ggplot2 code for various chart types
#   - Create interactive plotly visualizations
#   - Build custom plotting functions with user parameters
# 
# DATA PROCESSING PLUGINS:
#   - Generate data filtering and transformation code
#   - Create data cleaning and preparation workflows
#   - Build aggregation and summary code
# 
# REPORTING PLUGINS:
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
    toggle_comments(
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
        
        # CUSTOMIZE THIS SECTION FOR YOUR PLUGIN
        # Extract user settings from the UI inputs
        # Example: Get output type selection
        output_type <- if (length(input$output_type_%widget_id%) > 0) {
            input$output_type_%widget_id%
        } else {
            "plot"  # Default output type
        }
        
        # Example: Get selected variables  
        selected_variables <- if (length(input$variables_%widget_id%) > 0) {
            input$variables_%widget_id%
        } else {
            c()  # Empty selection
        }
        
        # Get plot title from input
        plot_title <- if (length(input$plot_title_%widget_id%) > 0) {
            input$plot_title_%widget_id%
        } else {
            "Data Analysis Results"  # Default title
        }
        
        # Generate R code based on current configuration
        # IMPLEMENT THIS FUNCTION FOR YOUR SPECIFIC PLUGIN
        generated_code <- generate_output_code_%widget_id%(
            output_type = output_type,
            variables = selected_variables,
            plot_title = plot_title
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

# IMPLEMENT THIS FUNCTION FOR YOUR SPECIFIC PLUGIN
# This function should generate R code based on user settings from the UI
generate_output_code_%widget_id% <- function(output_type = "plot", variables = c(), plot_title = "Data Analysis Results") {
    
    # Example code generation using the iris dataset
    code_lines <- c()
    
    # Add data loading/preparation code
    code_lines <- c(code_lines, 
        "# Load and prepare data",
        "data <- iris",
        ""
    )
    
    # Add variable selection code
    if (length(variables) > 0) {
        vars_string <- paste0("c(\"", paste(variables, collapse = "\", \""), "\")")
        code_lines <- c(code_lines,
            paste0("# Selected variables: ", vars_string),
            paste0("selected_vars <- ", vars_string),
            "selected_data <- data %>% dplyr::select(dplyr::all_of(selected_vars))",
            ""
        )
    } else {
        code_lines <- c(code_lines,
            "# No variables selected, using all numeric variables",
            "selected_data <- data %>% dplyr::select(dplyr::where(is.numeric))",
            ""
        )
    }
    
    # Add output generation code based on type
    if (output_type == "histogram") {
        code_lines <- c(code_lines,
            "# Generate histogram plot"
        )
        
        if (length(variables) == 1) {
            # Single variable histogram
            code_lines <- c(code_lines,
                paste0("result <- ggplot2::ggplot(data, ggplot2::aes(x = `", variables[1], "`)) +"),
                "    ggplot2::geom_histogram(bins = 30, fill = 'steelblue', alpha = 0.7) +",
                paste0("    ggplot2::labs(title = '", plot_title, "', x = '", variables[1], "', y = 'Frequency') +"),
                "    ggplot2::theme_minimal()"
            )
        } else if (length(variables) > 1) {
            # Multiple variables - create faceted histogram
            code_lines <- c(code_lines,
                "# Reshape data for multiple variables",
                "plot_data <- selected_data %>%",
                "    tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'variable', values_to = 'value')",
                "",
                "result <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value)) +",
                "    ggplot2::geom_histogram(bins = 30, fill = 'steelblue', alpha = 0.7) +",
                "    ggplot2::facet_wrap(~variable, scales = 'free') +",
                paste0("    ggplot2::labs(title = '", plot_title, "', x = 'Value', y = 'Frequency') +"),
                "    ggplot2::theme_minimal()"
            )
        } else {
            # No variables selected - show all numeric variables
            code_lines <- c(code_lines,
                "# Reshape data for all numeric variables",
                "plot_data <- selected_data %>%",
                "    tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'variable', values_to = 'value')",
                "",
                "result <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value)) +",
                "    ggplot2::geom_histogram(bins = 30, fill = 'steelblue', alpha = 0.7) +",
                "    ggplot2::facet_wrap(~variable, scales = 'free') +",
                paste0("    ggplot2::labs(title = '", plot_title, "', x = 'Value', y = 'Frequency') +"),
                "    ggplot2::theme_minimal()"
            )
        }
        
    } else if (output_type == "table") {
        code_lines <- c(code_lines,
            "# Generate data table",
            "result <- DT::datatable(selected_data,",
            "    options = list(pageLength = 10, scrollX = TRUE),",
            "    caption = 'Selected Variables from Iris Dataset'",
            ")"
        )
        
    } else if (output_type == "summary") {
        code_lines <- c(code_lines,
            "# Generate summary statistics",
            "result <- selected_data %>%",
            "    dplyr::summarise(dplyr::across(dplyr::everything(), list(",
            "        Mean = ~mean(., na.rm = TRUE),",
            "        SD = ~sd(., na.rm = TRUE),",
            "        Min = ~min(., na.rm = TRUE),",
            "        Max = ~max(., na.rm = TRUE)",
            "    ))) %>%",
            "    tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'statistic', values_to = 'value') %>%",
            "    tidyr::separate(statistic, into = c('variable', 'stat'), sep = '_') %>%",
            "    tidyr::pivot_wider(names_from = stat, values_from = value)",
            "",
            "# Convert to DT table for display",
            "result <- DT::datatable(result, options = list(pageLength = 15))"
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
# Uncomment and customize based on your plugin's data dependencies

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
        error_message <<- paste("Error executing code:", e$message)
    })
    
    # ====================
    # HANDLE EXECUTION RESULTS
    # ====================
    
    # Show error message if execution failed or no output was generated
    if (!is.null(error_message) || is.null(result)) {
        
        display_message <- if (!is.null(error_message)) {
            error_message
        } else {
            "No output generated. Please check your code and settings."
        }
        
        output$error_message_%widget_id% <- renderUI(
            div(
                shiny.fluent::MessageBar(
                    display_message, 
                    messageBarType = 5  # Error type
                ), 
                style = "display: inline-block;"
            )
        )
        
        shinyjs::show("error_message_div_%widget_id%")
        # Hide all output containers - customize based on your UI structure
        # shinyjs::hide("plot_div_%widget_id%")
        # shinyjs::hide("table_div_%widget_id%")
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
})
