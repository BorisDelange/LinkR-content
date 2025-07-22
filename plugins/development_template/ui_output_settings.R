# ==========================================
# ui_output_settings.R - Output Configuration Panel
# ==========================================

# PLUGIN TEMPLATE - OUTPUT SETTINGS UI FILE
# 
# This file defines the no-code configuration interface for the widget plugin template.
# It provides user-friendly controls that automatically generate and modify the underlying
# R code based on user selections, eliminating the need for manual coding.
# 
# WHEN CREATING A NEW PLUGIN WITH THIS TEMPLATE:
# - Remove unused configuration sections and keep only relevant controls
# - Customize dropdown options, labels, and default values for your specific use case
# - Add validation logic in server.R to ensure valid configuration combinations
# - Connect each input to code generation logic that updates the R code accordingly
# 
# COMMON CONFIGURATION PATTERNS:
# 
# DATA SOURCE SELECTION:
#   Use when: Plugin can work with different datasets, database tables, or file sources
#   Examples: Patient vs Visit data, Different time periods, Various data formats
# 
# OUTPUT TYPE SELECTION:
#   Use when: Plugin supports multiple visualization or analysis methods
#   Examples: Chart types (bar, line, scatter), Statistical tests, Report formats
# 
# PARAMETER CONFIGURATION:
#   Use when: Analysis requires user-specified parameters
#   Examples: Statistical thresholds, Time ranges, Grouping variables
# 
# FILTERING/SELECTION CONTROLS:
#   Use when: Users need to subset or focus on specific data elements
#   Examples: Variable selection, Category filtering, Date ranges
# 
# DISPLAY OPTIONS:
#   Use when: Output appearance can be customized
#   Examples: Colors, Labels, Formatting options, Interactive features

div(
    # ====================
    # EXAMPLE: DATA SOURCE SELECTION
    # ====================
    # Uncomment and customize for plugins that work with multiple data sources
    # div(
    #     div(
    #         shiny.fluent::Dropdown.shinyInput(
    #             ns("data_source_%widget_id%"), 
    #             options = list(
    #                 list(key = "dataset1", text = "Primary Dataset"),
    #                 list(key = "dataset2", text = "Secondary Dataset"),
    #                 list(key = "combined", text = "Combined Data")
    #             ), 
    #             value = "dataset1",
    #             label = "Data Source"
    #         ),
    #         style = "width: 200px;"
    #     ),
    #     style = "padding-bottom: 15px; border-bottom: solid 1px #808080;"
    # ),
    
    # ====================
    # EXAMPLE: OUTPUT TYPE SELECTION
    # ====================
    # Uncomment and customize for plugins with multiple output formats
    div(
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("output_type_%widget_id%"), 
                options = list(
                    list(key = "plot", text = "Visualization"),
                    list(key = "table", text = "Data Table"),
                    list(key = "summary", text = "Summary Statistics")
                ), 
                value = "plot",
                label = "Output Type"
            ),
            style = "width: 200px;"
        ),
        style = "padding-bottom: 15px; border-bottom: solid 1px #808080;"
    ),
    
    # ====================
    # EXAMPLE: VARIABLE SELECTION
    # ====================
    # Uncomment and customize for data analysis plugins
    div(
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("variables_%widget_id%"), 
                label = "Variables to Analyze",
                options = list(
                    list(key = "Sepal.Length", text = "Sepal Length"),
                    list(key = "Sepal.Width", text = "Sepal Width"),
                    list(key = "Petal.Length", text = "Petal Length"),
                    list(key = "Petal.Width", text = "Petal Width")
                ),
                multiSelect = TRUE,
                value = c("Sepal.Length")  # Default selection
            ),
            style = "width: 200px;"
        ),
        div(
            create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("variables_check_all_%widget_id%"), iconProps = list(iconName = "CheckboxComposite")), text = "Select All Variables"),
            create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("variables_uncheck_all_%widget_id%"), iconProps = list(iconName = "Checkbox")), text = "Clear Selection"),
            style = "margin: 27px 0 0 5px; display: flex;"
        ),
        class = "small_icon_button",
        style = "display: flex; margin-top: 5px;"
    ),
    
    # ====================
    # EXAMPLE: NUMERIC PARAMETERS
    # ====================
    # Uncomment for plugins requiring numeric input parameters
    # div(
    #     shiny.fluent::SpinButton.shinyInput(
    #         ns("threshold_%widget_id%"),
    #         label = "Threshold Value",
    #         value = 0.05,
    #         min = 0,
    #         max = 1,
    #         step = 0.01
    #     ),
    #     style = "margin-top: 15px; width: 150px;"
    # ),
    
    # ====================
    # EXAMPLE: TEXT INPUT PARAMETERS
    # ====================
    # Uncomment for plugins requiring text input (titles, labels, etc.)
    # div(
    #     shiny.fluent::TextField.shinyInput(
    #         ns("title_%widget_id%"),
    #         label = "Chart Title",
    #         value = "Analysis Results"
    #     ),
    #     style = "margin-top: 15px; width: 200px;"
    # ),
    
    # ====================
    # EXAMPLE: DATE RANGE SELECTION
    # ====================
    # Uncomment for time-series or temporal analysis plugins
    # div(
    #     div(
    #         shiny.fluent::DatePicker.shinyInput(
    #             ns("start_date_%widget_id%"),
    #             label = "Start Date",
    #             value = Sys.Date() - 30
    #         ),
    #         style = "width: 150px; margin-right: 10px;"
    #     ),
    #     div(
    #         shiny.fluent::DatePicker.shinyInput(
    #             ns("end_date_%widget_id%"),
    #             label = "End Date", 
    #             value = Sys.Date()
    #         ),
    #         style = "width: 150px;"
    #     ),
    #     style = "margin-top: 15px; display: flex;"
    # ),
    
    # ====================
    # EXAMPLE: BOOLEAN TOGGLES
    # ====================
    # Common settings that can be toggled on/off
    div(
        shiny.fluent::Toggle.shinyInput(
            ns("show_legend_%widget_id%"), 
            label = "Show Legend",
            value = TRUE
        ),
        style = "margin-top: 15px;"
    ),
    
    div(
        shiny.fluent::Toggle.shinyInput(
            ns("interactive_mode_%widget_id%"), 
            label = "Interactive Mode",
            value = FALSE
        ),
        style = "margin-top: 15px;"
    ),
    
    div(
        shiny.fluent::Toggle.shinyInput(
            ns("auto_update_%widget_id%"), 
            label = "Automatic Updates",
            value = TRUE
        ),
        style = "margin-top: 15px;"
    ),
    
    # ====================
    # EXAMPLE: COLOR/STYLE SELECTION
    # ====================
    # Uncomment for plugins with customizable appearance
    # div(
    #     shiny.fluent::Dropdown.shinyInput(
    #         ns("color_scheme_%widget_id%"),
    #         label = "Color Scheme",
    #         options = list(
    #             list(key = "default", text = "Default"),
    #             list(key = "viridis", text = "Viridis"),
    #             list(key = "blues", text = "Blues"),
    #             list(key = "custom", text = "Custom")
    #         ),
    #         value = "default"
    #     ),
    #     style = "margin-top: 15px; width: 150px;"
    # ),
    
    # ====================
    # EXAMPLE: GROUPING/FACETING OPTIONS
    # ====================
    # Uncomment for plugins that support data grouping
    # div(
    #     shiny.fluent::Dropdown.shinyInput(
    #         ns("group_by_%widget_id%"),
    #         label = "Group By",
    #         options = list(
    #             list(key = "none", text = "No Grouping"),
    #             list(key = "category", text = "Category"),
    #             list(key = "type", text = "Type"),
    #             list(key = "status", text = "Status")
    #         ),
    #         value = "none"
    #     ),
    #     style = "margin-top: 15px; width: 200px;"
    # ),
    
    # ====================
    # ACTION BUTTONS
    # ====================
    # Standard action buttons - customize labels as needed
    div(
        # Primary action - Generate/Update output
        shiny.fluent::PrimaryButton.shinyInput(
            ns("generate_output_%widget_id%"), 
            "Generate Output",
            onClick = htmlwidgets::JS(paste0(
                "item => { Shiny.setInputValue('", id, "-display_output_%widget_id%', Math.random()); }"
            ))
        ),
        
        # Secondary action - Save configuration (if user has permissions)
        # This button is shown/hidden based on user access in the main UI file
        shiny.fluent::DefaultButton.shinyInput(
            ns("save_configuration_%widget_id%"), 
            "Save Configuration",
            onClick = htmlwidgets::JS(paste0(
                "item => { Shiny.setInputValue('", id, "-save_output_settings_and_code_%widget_id%', Math.random()); }"
            ))
        ),
        
        style = "margin-top: 20px; display: flex; gap: 10px; flex-wrap: wrap;"
    ),
    
    # ====================
    # ADDITIONAL CONFIGURATION EXAMPLES
    # ====================
    # Add more sections as needed for your specific plugin:
    #
    # STATISTICAL PARAMETERS:
    # - Confidence levels, significance thresholds, test types
    #
    # FILTERING OPTIONS:
    # - Date ranges, category filters, numerical ranges
    #
    # DISPLAY PREFERENCES:
    # - Chart dimensions, font sizes, themes
    #
    # EXPORT SETTINGS:
    # - File formats, resolution, compression options
    #
    # PERFORMANCE OPTIONS:
    # - Sample sizes, processing limits, caching preferences
)
