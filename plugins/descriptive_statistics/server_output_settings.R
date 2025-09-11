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
    list(id = "y_axis", type = "dropdown", default = NULL),
    list(id = "plot_type", type = "dropdown", default = "histogram"),
    list(id = "plot_title", type = "text", default = i18np$t("data_analysis_results")),
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
    
    # Auto-execute when switching tabs
    shinyjs::delay(100, {
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-display_output_%widget_id%', Math.random());"))
    })
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
                if(length(column_names) > 1) {
                    shiny.fluent::updateDropdown.shinyInput(session, "y_axis_%widget_id%", options = options, value = column_names[2])
                }
                
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
