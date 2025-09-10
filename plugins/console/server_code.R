# ==========================================
# server_code.R - Code Editor Server Logic
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 CONSOLE PLUGIN - CUSTOMIZED IMPLEMENTATION  🔧                         ██
# ██                                                                            ██
# ██  This file implements the Console plugin functionality with multi-language ██
# ██  support and multiple output types. Based on the plugin template structure.██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# CONSOLE PLUGIN - CODE EDITOR SERVER FILE
# 
# This file handles the server-side logic for the Console plugin code editor.
# It provides multi-language code execution (R and Python) with various output types.
# 
# CONSOLE PLUGIN FEATURES:
# - Multi-language support: R and Python
# - Multiple output types: console, ui, figure, table, datatable, dygraphs, plotly, rmarkdown
# - Default code templates for each output type
# - Advanced code execution with error handling
# - Auto-execution triggers and language switching
# 
# CORE FUNCTIONALITY:
# - Code editor with syntax highlighting for R/Python
# - Default code templates that change based on output type selection
# - Multi-language code execution with appropriate output routing
# - Error handling and display for both R and Python
# - Integration with the widget's configuration system

# ======================================
# INITIALIZATION
# ======================================

# Initialize code storage variable
m$code_%widget_id% <- ""

# Define available output types for each programming language
outputs <- list()
outputs$r <- c("console", "ui", "figure", "table", "datatable", "dygraphs", "plotly", "rmarkdown")
outputs$python <- c("console", "matplotlib")

# Fix ACE editor rendering issues on startup
# Delay ensures DOM is fully loaded before triggering resize
shinyjs::delay(300, shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);"))

# ======================================
# DEFAULT CODE TEMPLATES
# ======================================

# Generate default code examples for different output types
get_default_code <- function(output_type) {
  switch(output_type,
    "console" = "# Data exploration with dplyr
# Basic operations on iris dataset
iris %>% 
  dplyr::filter(Species == 'setosa') %>%
  dplyr::select(Sepal.Length, Sepal.Width, Petal.Length) %>%
  dplyr::summarise(
    mean_sepal_length = mean(Sepal.Length),
    mean_sepal_width = mean(Sepal.Width),
    mean_petal_length = mean(Petal.Length),
    count = dplyr::n()
  ) %>% tibble::as_tibble()",
    
    "figure" = "# Stylized histogram of iris sepal length
ggplot2::ggplot(iris, ggplot2::aes(x = Sepal.Length, fill = Species)) +
  ggplot2::geom_histogram(bins = 20, alpha = 0.7, color = 'white') +
  ggplot2::scale_fill_manual(values = c('#1f77b4', '#2ca02c', '#ff7f0e')) +
  ggplot2::labs(
    title = 'Distribution of Sepal Length by Species',
    x = 'Sepal Length (cm)',
    y = 'Frequency',
    fill = 'Species'
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 14, face = 'bold', hjust = 0.5),
    axis.title = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 11)
  )",
    
    "table" = "# Display iris summary statistics
iris %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(
    Count = dplyr::n(),
    Mean_Sepal_Length = round(mean(Sepal.Length), 2),
    Mean_Sepal_Width = round(mean(Sepal.Width), 2),
    Mean_Petal_Length = round(mean(Petal.Length), 2),
    Mean_Petal_Width = round(mean(Petal.Width), 2),
    .groups = 'drop'
  )",
    
    "datatable" = "# Interactive data table
iris %>%
  dplyr::mutate(
    Sepal.Ratio = round(Sepal.Length / Sepal.Width, 2),
    Petal.Ratio = round(Petal.Length / Petal.Width, 2)
  ) %>%
  dplyr::select(
    Species, Sepal.Length, Sepal.Width, Sepal.Ratio, 
    Petal.Length, Petal.Width, Petal.Ratio
  )",
    
    "dygraphs" = "# Time series visualization with dygraphs
# Create sample time series data
dates <- seq(as.Date('2023-01-01'), as.Date('2023-12-31'), by = 'day')
values <- cumsum(rnorm(length(dates), 0, 1)) + 100
ts_data <- xts::xts(values, order.by = dates)

dygraphs::dygraph(ts_data, main = 'Sample Time Series') %>%
  dygraphs::dyOptions(colors = '#1f77b4', strokeWidth = 2) %>%
  dygraphs::dyRangeSelector() %>%
  dygraphs::dyHighlight(highlightCircleSize = 5)",
    
    "plotly" = "# Interactive scatter plot with plotly
plotly::plot_ly(
  data = iris,
  x = ~Sepal.Length,
  y = ~Sepal.Width,
  color = ~Species,
  colors = c('#1f77b4', '#2ca02c', '#ff7f0e'),
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 8, opacity = 0.8)
) %>%
  plotly::layout(
    title = 'Iris Dataset: Sepal Length vs Width',
    xaxis = list(title = 'Sepal Length (cm)'),
    yaxis = list(title = 'Sepal Width (cm)'),
    hovermode = 'closest'
  )",
    
    "rmarkdown" = "# Iris Dataset Analysis

## Overview
The iris dataset contains measurements of iris flowers from three species.

```{r}
summary(iris)
```

## Species Distribution
```{r}
table(iris$Species)
```

## Visualization
```{r, fig.width=6, fig.height=4, dpi=300, out.width=\"500px\", fig.cap=\"\"}
ggplot2::ggplot(iris, ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  ggplot2::geom_point(size = 2) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = 'Sepal Dimensions by Species') +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
```",
    
    "ui" = "# Simple UI metric display
# Calculate mean sepal length
mean_sepal_length <- round(mean(iris$Sepal.Length), 1)

div(
  style = \"
    text-align: center; 
    height: 100%; 
    display: flex;
    align-items: center;
    flex-direction: column;
    justify-content: center;
  \",
  div(
    style = \"color: #2C699A; margin-bottom: 10px;\",
    shiny::tags$i(class = \"fas fa-seedling fa-2x\")
  ),
  div(
    style = \"color: #2C699A; font-size: 36px; font-weight: bold; margin: 10px 0;\",
    paste0(mean_sepal_length, \" cm\")
  ),
  div(
    style = \"color: #666; font-size: 14px; text-transform: uppercase;\",
    \"Average Sepal Length\"
  )
)",
    
    # Default fallback
    "# Select an output type to see example code"
  )
}

# ======================================
# OUTPUT TYPE CHANGE HANDLER
# ======================================

# Provide default code when output type changes
observe_event(input$output_%widget_id%, {
    # Get current code from editor
    current_code <- trimws(if(is.null(input$code_%widget_id%)) "" else input$code_%widget_id%)
    
    if (!is.null(input$output_%widget_id%)) {
        # Check if current code is empty OR is a default template from any output type
        is_empty <- current_code == ""
        is_default_template <- FALSE
        
        # Check if current code matches any default template
        all_output_types <- c("console", "ui", "figure", "table", "datatable", "dygraphs", "plotly", "rmarkdown")
        for (output_type in all_output_types) {
            template_code <- trimws(get_default_code(output_type))
            if (current_code == template_code) {
                is_default_template <- TRUE
                break
            }
        }
        
        # Update code if it's empty or a default template
        if (is_empty || is_default_template) {
            default_code <- get_default_code(input$output_%widget_id%)
            
            # Update the ACE editor with new default code
            shinyAce::updateAceEditor(
                session = session,
                editorId = paste0("code_%widget_id%"),
                value = default_code
            )
        }
    }
})

# Change ACE editor language mode based on output type
observe_event(input$output_%widget_id%, {
  if (!is.null(input$output_%widget_id%)) {
    # Determine editor mode based on output type
    editor_mode <- switch(input$output_%widget_id%,
      "rmarkdown" = "markdown",
      input$prog_language_%widget_id%  # Default for all other output types
    )
    
    # Update ACE editor mode
    shinyAce::updateAceEditor(
      session = session,
      editorId = paste0("code_%widget_id%"),
      mode = editor_mode
    )
  }
})

# Change ACE editor language mode based on programming language
observe_event(input$prog_language_%widget_id%, {
  if (!is.null(input$prog_language_%widget_id%)) {
    # Update ACE editor mode based on selected language
    shinyAce::updateAceEditor(
      session = session,
      editorId = paste0("code_%widget_id%"),
      mode = input$prog_language_%widget_id%
    )
  }
})

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
    
    # ====================
    # CONSOLE PLUGIN CODE PREPARATION
    # ====================
    
    if ("projects_widgets_console" %in% user_accesses) {
        # Get current code from editor
        current_code <- trimws(if(is.null(input$code_%widget_id%)) "" else input$code_%widget_id%)
        
        # If code is empty or default template, use the template for current output type
        if (current_code == "" || current_code == "# Select an output type to see example code") {
            if (!is.null(input$output_%widget_id%)) {
                default_code <- get_default_code(input$output_%widget_id%)
                
                # Update the ACE editor with default code
                shinyAce::updateAceEditor(
                    session = session,
                    editorId = paste0("code_%widget_id%"),
                    value = default_code
                )
                
                # Store the code for execution
                m$code_%widget_id% <- default_code
            }
        } else {
            # Use the code from the editor
            m$code_%widget_id% <- current_code
        }
        
        # Trigger code execution
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

# ======================================
# CODE EXECUTION ENGINE
# ======================================

# Main code execution handler
observe_event(input$run_code_%widget_id%, {
    
    # Check if programming language is selected
    if (length(input$prog_language_%widget_id%) == 0) return()
    
    # Get user selections
    language <- input$prog_language_%widget_id%
    code_output <- input$output_%widget_id%
    code <- m$code_%widget_id%
    
    # Determine if code should be isolated based on auto-update setting
    isolate_code <- TRUE
    if (isTRUE(input$auto_update_%widget_id%)) isolate_code <- FALSE
    
    # Apply isolation wrapper for R code (except rmarkdown)
    if (isolate_code & language == "r" & code_output != "rmarkdown") {
        code <- paste0("isolate({\n", code, "\n})")
    }
    
    # ====================
    # HIDE ALL OUTPUTS INITIALLY
    # ====================
    
    # Hide all output containers for the current language
    sapply(paste0(outputs[[language]], "_output_div_%widget_id%"), shinyjs::hide)
    
    # ====================
    # EXECUTE CODE BY LANGUAGE
    # ====================
    
    # R Language Execution
    if (language == "r") {
        
        # Console Output - capture all output including errors
        if (code_output == "console") {
            captured_output <- capture.output(
                tryCatch(
                    eval(parse(text = code)), 
                    error = function(e) print(e), 
                    warning = function(w) print(w)
                )
            ) %>% paste(collapse = "\n")
            output$console_output_%widget_id% <- renderText(captured_output)
            shinyjs::show("console_output_div_%widget_id%")
        }
        
        # Special handling for rmarkdown - don't evaluate as R code
        else if (code_output == "rmarkdown") {
            tryCatch({
                # Set required variables for create_rmarkdown_file
                user_id <- m$user_id
                app_folder <- m$app_folder
                
                output_file <- create_rmarkdown_file(code, interpret_code = TRUE)
                output$rmarkdown_output_%widget_id% <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
                
                # Show the rmarkdown output container
                shinyjs::show("rmarkdown_output_div_%widget_id%")
                
            }, error = function(e) {
                # If there's an error, display it with centered UI styling
                error_message <- paste("Rmarkdown", i18np$t("error_executing_code"), e$message, sep = " - ")
                
                output$ui_output_%widget_id% <- renderUI({
                    div(
                        style = "display: flex; justify-content: center; align-items: center; height: 100%; text-align: center; padding: 10px;",
                        div(
                            style = "font-size: 14px; color: #6c757d;",
                            error_message
                        )
                    )
                })
                
                shinyjs::show("ui_output_div_%widget_id%")
            })
        }
        
        # Other output types - with R code evaluation
        else {
            tryCatch({
                # Execute the code and get the result
                result <- eval(parse(text = code))
                
                # Route to appropriate output renderer
                if (code_output == "ui") {
                    output$ui_output_%widget_id% <- renderUI(result)
                }
                else if (code_output == "figure") {
                    output$figure_output_%widget_id% <- renderPlot(result)
                }
                else if (code_output == "table") {
                    output$table_output_%widget_id% <- renderTable(result)
                }
                else if (code_output == "datatable") {
                    output$datatable_output_%widget_id% <- DT::renderDT(
                        DT::datatable(
                            result,
                            rownames = FALSE,
                            options = list(
                                dom = "<'datatable_length'l><'top't><'bottom'p>",
                                compact = TRUE, hover = TRUE
                            ),
                            # CSS styling for datatable
                            callback = htmlwidgets::JS(
                                "table.on('draw.dt', function() {",
                                "  $('.dataTable tbody tr td').css({",
                                "    'height': '12px',",
                                "    'padding': '2px 5px'",
                                "  });",
                                "  $('.dataTable thead tr td div .form-control').css('font-size', '12px');",
                                "  $('.dataTable thead tr td').css('padding', '5px');",
                                "});"
                            )
                        )
                    )
                }
                else if (code_output == "dygraphs") {
                    output$dygraphs_output_%widget_id% <- dygraphs::renderDygraph(result)
                }
                else if (code_output == "plotly") {
                    output$plotly_output_%widget_id% <- plotly::renderPlotly(result)
                }
                
                # Show the selected output container
                shinyjs::show(paste0(code_output, "_output_div_%widget_id%"))
                
            }, error = function(e) {
                # If there's an error, display it with centered UI styling
                error_message <- paste(i18np$t("error_executing_code"), e$message, sep = ": ")
                
                output$ui_output_%widget_id% <- renderUI({
                    div(
                        style = "display: flex; justify-content: center; align-items: center; height: 100%; text-align: center; padding: 10px;",
                        div(
                            style = "font-size: 14px; color: #6c757d;",
                            error_message
                        )
                    )
                })
                
                shinyjs::show("ui_output_div_%widget_id%")
            })
        }
    }
    
    # Python Language Execution
    else if (language == "python") {
        
        tryCatch({
            # Console Output for Python
            if (code_output == "console") {
                output$console_output_%widget_id% <- renderText(capture_python_output(code))
            }
            
            # Matplotlib Output for Python (implementation needed)
            # else if (code_output == "matplotlib") {
            #     # Implementation for matplotlib output
            #     # This would require Python integration setup
            # }
            
            # Show the selected output container
            shinyjs::show(paste0(code_output, "_output_div_%widget_id%"))
            
        }, error = function(e) {
            # If there's an error, display it with centered UI styling
            error_message <- paste("Python", i18np$t("error_executing_code"), e$message, sep = " - ")
            
            output$ui_output_%widget_id% <- renderUI({
                div(
                    style = "display: flex; justify-content: center; align-items: center; height: 100%; text-align: center; padding: 10px;",
                    div(
                        style = "font-size: 14px; color: #6c757d;",
                        error_message
                    )
                )
            })
            
            shinyjs::show("ui_output_div_%widget_id%")
        })
    }
    
    # ====================
    # AUTO-NAVIGATION
    # ====================
    # Optional: automatically switch to output tab after execution
    if (isFALSE(input$output_and_settings_side_by_side_%widget_id%)) shinyjs::click("output_button_%widget_id%")
    
    # ====================
    # SAVE AFTER DISPLAY LOGIC
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