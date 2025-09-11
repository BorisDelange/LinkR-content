# ==========================================
# server_output_settings.R - Output Configuration Server Logic
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

# PLUGIN TEMPLATE - OUTPUT SETTINGS SERVER FILE
# 
# This file handles the server-side logic for the output configuration interface.
# It manages user interactions with the no-code settings panel.
# 
# WHEN CREATING A NEW PLUGIN WITH THIS TEMPLATE:
# - Customize the dynamic UI observers for your specific input elements
# - Implement validation logic for your parameter combinations
# 
# CORE FUNCTIONALITY:
# - Dynamic UI updates based on user selections
# - Bulk selection helpers (select all/clear all functionality)
# - Input validation and error handling
# - Conditional UI display using shinyjs::show() and shinyjs::hide() to dynamically
#   show or hide UI elements based on user selections (e.g., showing title inputs
#   only when certain output types are selected)
# 
# COMMON CONFIGURATION PATTERNS:
# 
# CONDITIONAL UI DISPLAY:
#   Show/hide settings based on other selections (e.g., output type affects available options)
# 
# CASCADING UPDATES:
#   Filter available options based on previous selections (e.g., dataset affects variables)
# 
# UPDATE LOCK MECHANISM:
#   Prevent automatic field updates during configuration loading using input$update_lock_%widget_id%
#   This solves conflicts where observe_event reactions overwrite values being loaded from saved configs
#   Pattern: Check if (isTRUE(input$update_lock_%widget_id%)) { return() } at start of observe_event

# ======================================
# CENTRALIZED INPUT DEFINITIONS
# ======================================

# Define all inputs for this plugin in one centralized location.
# This configuration automatically generates the saving and loading logic for user configurations.
# 
# IMPORTANT: All UI elements must be added to ui_output_settings.R first, then registered here
# to enable automatic persistence of user preferences through the configuration system (server_user_configurations.R).
#
# STRUCTURE:
# Each input is defined as a list with the following required fields:
# - id: unique identifier (will be suffixed with _%widget_id%)
# - type: input type (see available types below)
# - default: default value when no configuration is loaded
#
# AVAILABLE INPUT TYPES:
# - "dropdown": Single selection dropdown (shiny.fluent::Dropdown)
# - "multiselect": Multiple selection dropdown (shiny.fluent::Dropdown with multiSelect = TRUE)
# - "text": Text input field (shiny.fluent::TextField)
# - "toggle": Boolean toggle switch (shiny.fluent::Toggle)
# - "code": Code editor (shinyAce::aceEditor)
# - "date": Date picker (shiny.fluent::DatePicker)
# - "number": Numeric input (shiny.fluent::SpinButton)
#
# WORKFLOW:
# 1. Create UI elements in ui_output_settings.R
# 2. Register all inputs here in all_inputs_%widget_id%
# 3. User configuration save/load is handled automatically
# 4. Add custom logic below for dynamic behavior and validation

all_inputs_%widget_id% <- list(
    # CSV Import
    list(id = "csv_file", type = "file", default = NULL),
    list(id = "selected_dataset", type = "dropdown", default = NULL),
    # Visualization
    list(id = "x_axis", type = "dropdown", default = NULL),
    list(id = "y_axis", type = "dropdown", default = ""),
    list(id = "plot_type", type = "dropdown", default = "histogram"),
    list(id = "plot_title", type = "text", default = i18np$t("data_analysis_results")),
    list(id = "x_legend", type = "text", default = ""),
    list(id = "y_legend", type = "text", default = ""),
    # Statistics
    list(id = "statistics_type", type = "dropdown", default = "table_one"),
    list(id = "grouping_variable", type = "dropdown", default = NULL),
    list(id = "variable_1", type = "dropdown", default = NULL),
    list(id = "variable_2", type = "dropdown", default = NULL),
    # General
    list(id = "auto_update", type = "toggle", default = TRUE),
    # Code editors for each tab
    list(id = "code_import_data", type = "code", default = ""),
    list(id = "code_visualization", type = "code", default = ""),
    list(id = "code_statistics", type = "code", default = ""),
    list(id = "code_report", type = "code", default = "")
)

# ======================================
# TAB SWITCHING LOGIC  
# ======================================

# Define tab names for navigation
sub_tabs <- c("import_data", "visualization", "statistics", "report")

# Handle tab switching
observe_event(input$current_figure_settings_tab_trigger_%widget_id%, {
    
    current_sub_tab <- 
        input$current_figure_settings_tab_%widget_id% %>%
        gsub(paste0(id, "-"), "", .) %>%
        gsub("_%widget_id%", "", .)
    
    sapply(sub_tabs, function(sub_tab) {
        if (current_sub_tab == sub_tab){
            shinyjs::addClass(class = "selected_widget_pivot_item", selector = paste0("#", id, "-", sub_tab, "_%widget_id%"))
            shinyjs::delay(50, shinyjs::show(paste0(sub_tab, "_div_%widget_id%")))
        }
        else {
            shinyjs::removeClass(class = "selected_widget_pivot_item", selector = paste0("#", id, "-", sub_tab, "_%widget_id%"))
            shinyjs::hide(paste0(sub_tab, "_div_%widget_id%"))
        }
    })
    
    # Show appropriate output for the current tab
    if (current_sub_tab == "import_data") {
        # Show datatable if dataset is loaded
        if (length(input$selected_dataset_%widget_id%) > 0 && !is.null(input$selected_dataset_%widget_id%)) {
            shinyjs::show("datatable_div_%widget_id%")
            shinyjs::hide("plot_div_%widget_id%")
            shinyjs::hide("table_div_%widget_id%")
            shinyjs::hide("dynamic_output_div_%widget_id%")
        }
    } else if (current_sub_tab == "visualization") {
        # Show visualization helper or plot depending on state
        shinyjs::hide("datatable_div_%widget_id%")
        shinyjs::hide("table_div_%widget_id%")
        shinyjs::hide("dynamic_output_div_%widget_id%")
        # Show helper by default, plot will override when executed
        if (!is.null(input$x_axis_%widget_id%) && input$x_axis_%widget_id% != "") {
            shinyjs::hide("plot_div_%widget_id%")
            shinyjs::show("visualization_helper_div_%widget_id%")
        } else {
            shinyjs::hide("plot_div_%widget_id%")
            shinyjs::hide("visualization_helper_div_%widget_id%")
        }
    } else if (current_sub_tab == "statistics") {
        # Show statistics table
        shinyjs::hide("datatable_div_%widget_id%")
        shinyjs::hide("plot_div_%widget_id%")
        shinyjs::show("table_div_%widget_id%")
        shinyjs::hide("dynamic_output_div_%widget_id%")
    } else if (current_sub_tab == "report") {
        # Show report output
        shinyjs::hide("datatable_div_%widget_id%")
        shinyjs::hide("plot_div_%widget_id%")
        shinyjs::hide("table_div_%widget_id%")
        shinyjs::show("dynamic_output_div_%widget_id%")
    }
})

# ======================================
# CSV FILE UPLOAD LOGIC
# ======================================

# Store current dataset and options
current_dataset_%widget_id% <- reactiveVal(NULL)
column_options_%widget_id% <- reactiveVal(list())

# Get project data folder path
get_project_data_folder <- function() {
    # Use temp_files/projects_data folder since r$ is not accessible from plugins
    data_folder <- file.path(m$app_folder, "temp_files", "projects_data")
    
    # Create data folder if it doesn't exist
    if (!dir.exists(data_folder)) {
        dir.create(data_folder, recursive = TRUE)
    }
    
    return(data_folder)
}

# Function to scan CSV files in data folder
scan_csv_files <- function() {
    data_folder <- get_project_data_folder()
    csv_files <- list.files(data_folder, pattern = "\\.csv$", full.names = FALSE)
    return(csv_files)
}

# Initialize dropdown with existing CSV files
observe({
    csv_files <- scan_csv_files()
    if (length(csv_files) > 0) {
        dataset_options <- lapply(csv_files, function(x) list(key = x, text = x))
        shiny.fluent::updateDropdown.shinyInput(session, "selected_dataset_%widget_id%", options = dataset_options)
    }
})

# Handle CSV file upload
observe_event(input$csv_file_%widget_id%, {
    
    file_info <- input$csv_file_%widget_id%
    
    if (!is.null(file_info)) {
        tryCatch({
            # Get project data folder
            data_folder <- get_project_data_folder()
            
            # Copy uploaded file to project data folder
            file_destination <- file.path(data_folder, file_info$name)
            file.copy(file_info$datapath, file_destination, overwrite = TRUE)
            
            # Update dataset selection dropdown with all CSV files in folder
            csv_files <- scan_csv_files()
            dataset_options <- lapply(csv_files, function(x) list(key = x, text = x))
            shiny.fluent::updateDropdown.shinyInput(session, "selected_dataset_%widget_id%", 
                                                  options = dataset_options, 
                                                  value = file_info$name)
            
        }, error = function(e) {
            # Handle file upload errors
            cat("Error uploading CSV file:", e$message, "\n")
        })
    }
})

# Handle dataset selection change
observe_event(input$selected_dataset_%widget_id%, {
    
    selected_name <- input$selected_dataset_%widget_id%
    
    if (!is.null(selected_name)) {
        tryCatch({
            # Get project data folder and read selected CSV file
            data_folder <- get_project_data_folder()
            file_path <- file.path(data_folder, selected_name)
            
            if (file.exists(file_path)) {
                # Read CSV file
                data <- read.csv(file_path, stringsAsFactors = FALSE)
                current_dataset_%widget_id%(data)
                
                # Create options for dropdowns
                column_names <- names(data)
                options <- lapply(column_names, function(x) list(key = x, text = x))
                column_options_%widget_id%(options)
                
                # Update visualization dropdowns
                shiny.fluent::updateDropdown.shinyInput(session, "x_axis_%widget_id%", options = options, value = column_names[1])
                # Add "Aucun" option for Y axis
                y_options <- c(list(list(key = "", text = i18np$t("none"))), options)
                shiny.fluent::updateDropdown.shinyInput(session, "y_axis_%widget_id%", options = y_options, value = "")
                
                # Update statistics dropdowns
                shiny.fluent::updateDropdown.shinyInput(session, "grouping_variable_%widget_id%", options = options, value = NULL)
                shiny.fluent::updateDropdown.shinyInput(session, "variable_1_%widget_id%", options = options, value = NULL)
                shiny.fluent::updateDropdown.shinyInput(session, "variable_2_%widget_id%", options = options, value = NULL)
                
                # Display datatable in Results tab
                output$datatable_%widget_id% <- DT::renderDT({
                    DT::datatable(
                        data,
                        options = list(
                            pageLength = 25,
                            scrollX = TRUE,
                            scrollY = "400px",
                            dom = 'Brtip',  # Removed 'f' to hide search
                            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                            searching = FALSE  # Disable search functionality
                        ),
                        class = "display nowrap compact",
                        filter = "none",  # Remove column filters
                        rownames = FALSE
                    )
                })
                
                # Show datatable and hide other outputs
                shinyjs::show("datatable_div_%widget_id%")
                shinyjs::hide("plot_div_%widget_id%")
                shinyjs::hide("dynamic_output_div_%widget_id%")
                
                # Auto-generate and update code in ACE editor for import_data tab
                shinyjs::delay(200, {
                    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-display_output_%widget_id%', Math.random());"))
                })
                shinyjs::hide("error_message_div_%widget_id%")
            }
            
        }, error = function(e) {
            # Handle file read errors
            cat("Error reading selected dataset:", e$message, "\n")
        })
    }
})

# ======================================
# STATISTICS TYPE CONDITIONAL DISPLAY
# ======================================

# Show/hide statistics options based on type
observe_event(input$statistics_type_%widget_id%, {
    
    if (isTRUE(input$update_lock_%widget_id%)) {
        return()
    }
    
    stats_type <- input$statistics_type_%widget_id%
    
    if (!is.null(stats_type)) {
        if (stats_type == "table_one") {
            shinyjs::show("table_one_options_%widget_id%")
            shinyjs::hide("variable_comparison_options_%widget_id%")
        } else if (stats_type == "variable_comparison") {
            shinyjs::hide("table_one_options_%widget_id%")
            shinyjs::show("variable_comparison_options_%widget_id%")
        }
    }
})

# ======================================
# VISUALIZATION HELPER UI
# ======================================

# Create helper UI when X variable changes
observe_event(input$x_axis_%widget_id%, {
    
    # Only run if we have data
    req(input$selected_dataset_%widget_id%)
    req(current_dataset_%widget_id%())
    
    x_var <- input$x_axis_%widget_id%
    y_var <- input$y_axis_%widget_id%
    plot_type <- input$plot_type_%widget_id%
    
    # Only create helper if at least X variable is selected
    if (!is.null(x_var) && x_var != "") {
        
        data <- current_dataset_%widget_id%()
        
        # Generate helper UI
        helper_ui <- create_visualization_helper_ui(data, x_var, y_var, plot_type)
        
        # Update the visualization helper output
        output$visualization_helper_%widget_id% <- renderUI({
            helper_ui
        })
        
        # Show helper UI when in visualization tab
        current_sub_tab <- if (length(input$current_figure_settings_tab_%widget_id%) > 0) {
            input$current_figure_settings_tab_%widget_id% %>%
                gsub(paste0(id, "-"), "", .) %>%
                gsub("_%widget_id%", "", .)
        } else {
            "import_data"
        }
        
        if (current_sub_tab == "visualization") {
            shinyjs::hide("plot_div_%widget_id%")
            shinyjs::show("visualization_helper_div_%widget_id%")
        }
    }
})

# Update helper UI when Y variable changes
observe_event(input$y_axis_%widget_id%, {
    
    # Only run if we have data and X variable
    if (is.null(input$selected_dataset_%widget_id%) || is.null(current_dataset_%widget_id%()) || is.null(input$x_axis_%widget_id%)) {
        return()
    }
    
    x_var <- input$x_axis_%widget_id%
    y_var <- input$y_axis_%widget_id%
    plot_type <- input$plot_type_%widget_id%
    
    if (!is.null(x_var) && x_var != "") {
        
        data <- current_dataset_%widget_id%()
        
        # Generate helper UI
        helper_ui <- create_visualization_helper_ui(data, x_var, y_var, plot_type)
        
        # Update the visualization helper output
        output$visualization_helper_%widget_id% <- renderUI({
            helper_ui
        })
        
        # Show helper UI when in visualization tab
        current_sub_tab <- if (length(input$current_figure_settings_tab_%widget_id%) > 0) {
            input$current_figure_settings_tab_%widget_id% %>%
                gsub(paste0(id, "-"), "", .) %>%
                gsub("_%widget_id%", "", .)
        } else {
            "import_data"
        }
        
        if (current_sub_tab == "visualization") {
            shinyjs::hide("plot_div_%widget_id%")
            shinyjs::show("visualization_helper_div_%widget_id%")
        }
    }
})

# Update helper UI when plot type changes
observe_event(input$plot_type_%widget_id%, {
    
    # Only run if we have data and X variable
    req(input$selected_dataset_%widget_id%)
    req(current_dataset_%widget_id%())
    req(input$x_axis_%widget_id%)
    
    x_var <- input$x_axis_%widget_id%
    y_var <- input$y_axis_%widget_id%
    plot_type <- input$plot_type_%widget_id%
    
    if (!is.null(x_var) && x_var != "") {
        
        data <- current_dataset_%widget_id%()
        
        # Generate helper UI
        helper_ui <- create_visualization_helper_ui(data, x_var, y_var, plot_type)
        
        # Update the visualization helper output
        output$visualization_helper_%widget_id% <- renderUI({
            helper_ui
        })
        
        # Show helper UI when in visualization tab (unless plot was just executed)
        current_sub_tab <- if (length(input$current_figure_settings_tab_%widget_id%) > 0) {
            input$current_figure_settings_tab_%widget_id% %>%
                gsub(paste0(id, "-"), "", .) %>%
                gsub("_%widget_id%", "", .)
        } else {
            "import_data"
        }
        
        if (current_sub_tab == "visualization") {
            shinyjs::hide("plot_div_%widget_id%")
            shinyjs::show("visualization_helper_div_%widget_id%")
        }
    }
})

# Function to create visualization helper UI
create_visualization_helper_ui <- function(data, x_var, y_var = NULL, plot_type = "histogram") {
    
    # Get variable information
    x_info <- get_variable_info(data, x_var)
    y_info <- if (!is.null(y_var) && y_var != "") get_variable_info(data, y_var) else NULL
    
    # Get plot recommendations and feasibility
    recommendations <- get_plot_recommendations(x_info$type, if (!is.null(y_info)) y_info$type else NULL, plot_type)
    
    # Determine if variables work with current plot type
    x_feasible <- is_variable_feasible_for_plot(x_info$type, if (!is.null(y_info)) y_info$type else NULL, plot_type, "x")
    y_feasible <- if (!is.null(y_info)) is_variable_feasible_for_plot(x_info$type, y_info$type, plot_type, "y") else TRUE
    
    div(
        style = "padding: 20px; border-radius: 8px; margin: 10px 0;",
        
        # Variable information
        div(
            style = "display: flex; flex-wrap: wrap; gap: 20px; margin-bottom: 20px;",
            
            # X Variable info
            div(
                style = "flex: 1; min-width: 300px;",
                create_variable_card("X", x_var, x_info, x_feasible)
            ),
            
            # Y Variable info (if exists)
            if (!is.null(y_info)) {
                div(
                    style = "flex: 1; min-width: 300px;",
                    create_variable_card("Y", y_var, y_info, y_feasible)
                )
            } else {
                div(
                    style = "flex: 1; min-width: 300px; padding: 15px; background-color: #e1f3d8; border-radius: 6px; border-left: 4px solid #107c10;",
                    div(
                        style = "font-weight: bold; color: #107c10;",
                        i18np$t("y_axis_none_selected")
                    )
                )
            }
        ),
        
        # Plot recommendations
        div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 6px; border-left: 4px solid #0078d4;",
            h4(i18np$t("plot_recommendations"), style = "margin-top: 0; color: #323130;"),
            recommendations$message
        )
    )
}

# Function to get variable information
get_variable_info <- function(data, var_name) {
    
    if (is.null(var_name) || var_name == "" || !var_name %in% names(data)) {
        return(NULL)
    }
    
    values <- data[[var_name]]
    
    # Determine type
    type <- if (is.numeric(values)) {
        "numeric"
    } else if (is.factor(values)) {
        "factor"
    } else if (is.character(values)) {
        "character"
    } else if (is.logical(values)) {
        "logical"
    } else {
        "other"
    }
    
    # Get sample values (first few unique values)
    unique_vals <- unique(values[!is.na(values)])
    sample_values <- if (length(unique_vals) > 5) {
        c(head(unique_vals, 3), "...")
    } else {
        unique_vals
    }
    
    # Get statistics for numeric variables
    stats <- NULL
    if (type == "numeric") {
        stats <- list(
            min = min(values, na.rm = TRUE),
            max = max(values, na.rm = TRUE),
            median = median(values, na.rm = TRUE),
            mean = mean(values, na.rm = TRUE)
        )
    }
    
    list(
        name = var_name,
        type = type,
        sample_values = sample_values,
        stats = stats,
        n_unique = length(unique_vals),
        n_missing = sum(is.na(values))
    )
}

# Function to create variable info card
create_variable_card <- function(axis_label, var_name, var_info, is_feasible = TRUE) {
    
    # Feasibility-based styling (green if feasible, red if not)
    if (is_feasible) {
        colors <- list(bg = "#e1f3d8", text = "#107c10", label_bg = "#107c10")  # Green
    } else {
        colors <- list(bg = "#fef0f0", text = "#d13438", label_bg = "#d13438")  # Red
    }
    
    # Use single color for all types with icons to differentiate
    label_color <- "#0078d4"  # Blue for all types
    
    # Type-based FontAwesome icons
    type_icons <- list(
        numeric = tags$i(class = "fas fa-hashtag", style = "margin-right: 4px;"),
        character = tags$i(class = "fas fa-font", style = "margin-right: 4px;"),
        factor = tags$i(class = "fas fa-list", style = "margin-right: 4px;"),
        logical = tags$i(class = "fas fa-check", style = "margin-right: 4px;"),
        other = tags$i(class = "fas fa-question", style = "margin-right: 4px;")
    )
    
    type_icon <- type_icons[[var_info$type]]
    
    div(
        style = paste0("padding: 15px; background-color: ", colors$bg, "; border-radius: 6px; border-left: 4px solid ", colors$label_bg, ";"),
        
        # Variable name and type
        div(
            style = "display: flex; align-items: center; margin-bottom: 10px;",
            div(
                style = "font-weight: bold; margin-right: 10px; color: #323130;",
                var_name
            ),
            span(
                tagList(type_icon, var_info$type),
                style = paste0("background-color: ", label_color, "; color: white; padding: 2px 8px; border-radius: 12px; font-size: 12px; font-weight: bold;")
            )
        ),
        
        # Sample values
        div(
            style = paste0("margin-bottom: 8px; color: ", colors$text, ";"),
            strong(paste0(i18np$t("sample_values"), " : ")),
            paste(var_info$sample_values, collapse = ", ")
        ),
        
        # Statistics for numeric variables
        if (var_info$type == "numeric" && !is.null(var_info$stats)) {
            div(
                style = paste0("color: ", colors$text, ";"),
                strong(paste0(i18np$t("min"), " : ")), round(var_info$stats$min, 2), " | ",
                strong(paste0(i18np$t("max"), " : ")), round(var_info$stats$max, 2), " | ",
                strong(paste0(i18np$t("mean"), " : ")), round(var_info$stats$mean, 2), " | ",
                strong(paste0(i18np$t("median"), " : ")), round(var_info$stats$median, 2)
            )
        } else {
            div(
                style = paste0("color: ", colors$text, ";"),
                strong(paste0(i18np$t("unique_values"), " : ")), var_info$n_unique
            )
        },
        
        # Missing values info
        if (var_info$n_missing > 0) {
            div(
                style = "margin-top: 5px; font-size: 12px; color: #d13438;",
                strong(paste0(i18np$t("missing_values"), " : ")), var_info$n_missing
            )
        }
    )
}

# Function to determine if a variable is feasible for the current plot type
is_variable_feasible_for_plot <- function(x_type, y_type = NULL, plot_type = "histogram", axis = "x") {
    
    # Single variable plots
    if (is.null(y_type)) {
        if (plot_type == "histogram") {
            return(x_type == "numeric")
        } else if (plot_type == "scatter") {
            return(FALSE)  # Scatter needs both variables
        } else {
            return(TRUE)  # Bar plot and box plot work with any single variable
        }
    }
    # Two variable plots
    else {
        if (plot_type == "histogram") {
            return(FALSE)  # Histogram doesn't work with two variables
        } else if (plot_type == "scatter") {
            return(x_type == "numeric" && y_type == "numeric")
        } else if (plot_type == "boxplot") {
            # Box plot works with at least one numeric variable
            return(x_type == "numeric" || y_type == "numeric")
        } else {
            return(TRUE)  # Bar plot works with any combination
        }
    }
}

# Function to get plot recommendations
get_plot_recommendations <- function(x_type, y_type = NULL, current_plot_type = "histogram") {
    
    # Single variable plots
    if (is.null(y_type)) {
        if (x_type == "numeric") {
            list(
                message = div(
                    i18np$t("with_numeric_variable_can_create"),
                    tags$ul(
                        tags$li(tags$b(i18np$t("histogram_plot")), " - ", i18np$t("histogram_description")),
                        tags$li(tags$b(i18np$t("boxplot")), " - ", i18np$t("boxplot_description")),
                        tags$li(tags$b(i18np$t("barplot")), " - ", i18np$t("barplot_numeric_description"))
                    )
                )
            )
        } else {
            list(
                message = div(
                    i18np$t("with_categorical_variable_can_create"),
                    tags$ul(
                        tags$li(tags$b(i18np$t("barplot")), " - ", i18np$t("barplot_categorical_description")),
                        tags$li(tags$b(i18np$t("boxplot")), " - ", i18np$t("boxplot_not_recommended_categorical"))
                    )
                )
            )
        }
    }
    # Two variable plots  
    else {
        if (x_type == "numeric" && y_type == "numeric") {
            list(
                message = div(
                    i18np$t("with_numeric_x_numeric_y_can_create"),
                    tags$ul(
                        tags$li(tags$b(i18np$t("scatter_plot")), " - ", i18np$t("scatter_description")),
                        tags$li(tags$b(i18np$t("boxplot")), " - ", i18np$t("boxplot_two_numeric_description"))
                    )
                )
            )
        } else if (x_type != "numeric" && y_type == "numeric") {
            list(
                message = div(
                    i18np$t("with_categorical_x_numeric_y_can_create"),
                    tags$ul(
                        tags$li(tags$b(i18np$t("boxplot")), " - ", i18np$t("boxplot_categorical_x_description")),
                        tags$li(tags$b(i18np$t("barplot")), " - ", i18np$t("barplot_average_description"))
                    )
                )
            )
        } else {
            list(
                message = div(
                    i18np$t("with_two_categorical_variables"),
                    tags$ul(
                        tags$li(tags$b(i18np$t("barplot")), " - ", i18np$t("barplot_crosstab_description"))
                    )
                )
            )
        }
    }
}

# ======================================
# PLOT TYPE CONDITIONAL LOGIC
# ======================================

# Update Y axis requirement based on plot type
observe_event(input$plot_type_%widget_id%, {
    
    if (isTRUE(input$update_lock_%widget_id%)) {
        return()
    }
    
    plot_type <- input$plot_type_%widget_id%
    
    if (!is.null(plot_type)) {
        if (plot_type == "histogram") {
            # Histogram only needs X axis
            shiny.fluent::updateDropdown.shinyInput(session, "y_axis_%widget_id%", value = NULL)
        }
    }
})


# Hide Display + Save button if user doesn't have save permissions
if (!("projects_widgets_settings" %in% user_accesses)) {
    shinyjs::hide("display_and_save_%widget_id%")
}
