# ==========================================
# ui_output_settings.R - Output Configuration Panel
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

# PLUGIN TEMPLATE - OUTPUT SETTINGS UI FILE
# 
# This file defines the no-code configuration interface for the widget plugin template.
# It provides user-friendly controls that automatically generate and modify the underlying
# R code based on user selections, eliminating the need for manual coding.
# 
# WHEN CREATING A NEW PLUGIN WITH THIS TEMPLATE:
# - Remove unused configuration sections and keep only relevant controls
# - Customize dropdown options, labels, and default values for your specific use case
# - Add validation logic in server_output_settings.R to ensure valid configuration combinations
# - Connect each input to code generation logic that updates the R code accordingly
# 
# IMPORTANT NOTES:
# - AVOID using conditionalPanel() to show/hide UI elements. Instead, manage this logic
#   in the server file (server_output_settings.R) using shinyjs::show and shinyjs::hide functions.
#   This provides better control and maintains consistency with the reactive framework.
# - Each div element includes an ID attribute to enable dynamic show/hide functionality
#   using shinyjs::show() and shinyjs::hide() from the server logic. This allows for
#   conditional display of UI elements based on user selections.
# - When adding or removing configuration elements, make sure to update both the
#   CONFIGURATION LOADING FROM DATABASE and CONFIGURATION SAVING TO DATABASE sections
#   in server_user_configurations.R to ensure user choices are properly saved and restored.
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
    # Use for plugins that work with multiple data sources
    # div(
        # id = ns("data_source_section_%widget_id%"),
        # div(
            # shiny.fluent::Dropdown.shinyInput(
                # ns("data_source_%widget_id%"), 
                # options = list(
                    # list(key = "iris", text = "Iris Dataset"),
                    # list(key = "mtcars", text = "Motor Trend Cars"),
                    # list(key = "diamonds", text = "Diamonds Dataset")
                # ), 
                # value = "iris",
                # label = "Data Source"
            # ),
            # style = "width: 200px;"
        # ),
        # style = "padding-bottom: 15px; border-bottom: solid 1px #808080;"
    # ),
    
    # ====================
    # EXAMPLE: OUTPUT TYPE SELECTION
    # ====================
    # Configure the type of visualization or analysis output
    div(
        id = ns("output_type_div_%widget_id%"),
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("output_type_%widget_id%"), 
                options = list(
                    list(key = "histogram", text = i18np$t("histogram")),
                    list(key = "table", text = i18np$t("data_table")),
                    list(key = "summary", text = i18np$t("summary_statistics"))
                ), 
                value = "histogram",
                label = i18np$t("output_type")
            ),
            style = "width: 200px;"
        ),
        style = "padding-bottom: 15px; border-bottom: solid 1px #808080;"
    ),
    
    # ====================
    # EXAMPLE: VARIABLE SELECTION
    # ====================
    # Responsive container for variables and plot title
    div(
        id = ns("variables_and_title_container_%widget_id%"),
        # Multi-select dropdown for choosing analysis variables
        div(
            id = ns("variables_div_%widget_id%"),
            div(
                shiny.fluent::Dropdown.shinyInput(
                    ns("variables_%widget_id%"), 
                    label = i18np$t("variables"),
                    options = list(
                        list(key = "Sepal.Length", text = "Sepal Length"),
                        list(key = "Sepal.Width", text = "Sepal Width"),
                        list(key = "Petal.Length", text = "Petal Length"),
                        list(key = "Petal.Width", text = "Petal Width"),
                        list(key = "Species", text = "Species")
                    ),
                    multiSelect = TRUE,
                    value = c("Sepal.Length")  # Default selection
                ),
                style = "width: 200px;"
            ),
            div(
                id = ns("variables_buttons_%widget_id%"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("variables_check_all_%widget_id%"), iconProps = list(iconName = "CheckboxComposite")), text = "Select All Variables"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("variables_uncheck_all_%widget_id%"), iconProps = list(iconName = "Checkbox")), text = "Clear Selection"),
                style = "margin: 27px 0 0 5px; display: flex;"
            ),
            class = "small_icon_button",
            style = "display: flex; flex: 0 0 auto; margin-right: 15px;"
        ),
        # Plot title input
        div(
            id = ns("plot_title_div_%widget_id%"),
            shiny.fluent::TextField.shinyInput(
                ns("plot_title_%widget_id%"),
                label = i18np$t("plot_title"),
                value = i18np$t("data_analysis_results")
            ),
            style = "width: 250px; flex: 0 0 auto;"
        ),
        style = "display: flex; flex-wrap: wrap; gap: 5px; padding-bottom: 15px; border-bottom: solid 1px #808080;"
    ),
    
    # ====================
    # EXAMPLE: GROUPING/FACETING OPTIONS
    # ====================
    # Select variable for grouping or color coding
    # div(
        # id = ns("group_by_div_%widget_id%"),
        # shiny.fluent::Dropdown.shinyInput(
            # ns("group_by_%widget_id%"),
            # label = i18np$t("group_by_variable"),
            # options = list(
                # list(key = "none", text = "No Grouping"),
                # list(key = "Species", text = "Species"),
                # list(key = "size_category", text = "Size Category"),
                # list(key = "custom", text = "Custom Groups")
            # ),
            # value = "Species"
        # ),
        # style = "width: 200px;"
    # ),
    
    # ====================
    # EXAMPLE: NUMERIC PARAMETERS
    # ====================
    # Numeric input for statistical or display parameters
    # div(
        # id = ns("alpha_level_div_%widget_id%"),
        # shiny.fluent::SpinButton.shinyInput(
            # ns("alpha_level_%widget_id%"),
            # label = i18np$t("transparency_level"),
            # value = 0.7,
            # min = 0.1,
            # max = 1.0,
            # step = 0.1
        # ),
        # style = "width: 150px;"
    # ),
    
    # ====================
    # EXAMPLE: COLOR/STYLE SELECTION
    # ====================
    # Choose color palette or styling options
    # div(
        # id = ns("color_scheme_div_%widget_id%"),
        # shiny.fluent::Dropdown.shinyInput(
            # ns("color_scheme_%widget_id%"),
            # label = i18np$t("color_palette"),
            # options = list(
                # list(key = "default", text = "Default Colors"),
                # list(key = "viridis", text = "Viridis"),
                # list(key = "plasma", text = "Plasma"),
                # list(key = "blues", text = "Blues"),
                # list(key = "set1", text = "Set1 (Qualitative)")
            # ),
            # value = "viridis"
        # ),
        # style = "width: 200px;"
    # ),
    
    # ====================
    # EXAMPLE: BOOLEAN TOGGLES
    # ====================
    # Common on/off settings
    div(
        # div(
            # id = ns("show_legend_div_%widget_id%"),
            # shiny.fluent::Toggle.shinyInput(
                # ns("show_legend_%widget_id%"), 
                # label = i18np$t("show_legend"),
                # value = TRUE
            # ),
            # style = "margin-right: 20px;"
        # ),
        div(
            id = ns("auto_update_div_%widget_id%"),
            shiny.fluent::Toggle.shinyInput(
                ns("auto_update_%widget_id%"), 
                label = i18np$t("automatic_updates"),
                value = TRUE
            )
        ),
        style = "margin-top: 15px; display: flex; align-items: center;"
    ),
    
    # ====================
    # EXAMPLE: DATE RANGE SELECTION
    # ====================
    # For time-series or temporal analysis plugins
    # div(
        # id = ns("date_range_div_%widget_id%"),
        # div(
            # id = ns("start_date_div_%widget_id%"),
            # shiny.fluent::DatePicker.shinyInput(
                # ns("start_date_%widget_id%"),
                # label = i18np$t("start_date"),
                # value = Sys.Date() - 30
            # ),
            # style = "width: 150px; margin-right: 15px;"
        # ),
        # div(
            # id = ns("end_date_div_%widget_id%"),
            # shiny.fluent::DatePicker.shinyInput(
                # ns("end_date_%widget_id%"),
                # label = i18np$t("end_date"), 
                # value = Sys.Date()
            # ),
            # style = "width: 150px;"
        # ),
        # style = "margin-top: 20px; display: flex; padding-top: 15px; border-top: solid 1px #808080;"
    # ),
    
    # ====================
    # EXAMPLE: TEXT INPUT PARAMETERS
    # ====================
    # Custom labels and annotations
    # div(
        # id = ns("axis_labels_div_%widget_id%"),
        # div(
            # id = ns("x_axis_label_div_%widget_id%"),
            # shiny.fluent::TextField.shinyInput(
                # ns("x_axis_label_%widget_id%"),
                # label = i18np$t("x_axis_label"),
                # value = "X Variable"
            # ),
            # style = "width: 150px; margin-right: 15px;"
        # ),
        # div(
            # id = ns("y_axis_label_div_%widget_id%"),
            # shiny.fluent::TextField.shinyInput(
                # ns("y_axis_label_%widget_id%"),
                # label = i18np$t("y_axis_label"),
                # value = "Y Variable"
            # ),
            # style = "width: 150px;"
        # ),
        # style = "margin-top: 15px; display: flex;"
    # ),
    
    # ====================
    # ACTION BUTTONS
    # ====================
    # Standard action buttons - customize labels as needed
    div(
        id = ns("action_buttons_div_%widget_id%"),
        # Default action - Generate/Update output only
        shiny.fluent::DefaultButton.shinyInput(
            ns("display_output_2_%widget_id%"), 
            i18np$t("display_output"), iconProps = list(iconName = "Play"),
            onClick = htmlwidgets::JS(paste0(
                "item => { Shiny.setInputValue('", id, "-display_output_%widget_id%', Math.random()); }"
            ))
        ),
        
        # Primary action - Display and Save (if user has permissions)
        # This button is shown/hidden based on user access in the main UI file
        shiny.fluent::PrimaryButton.shinyInput(
            ns("display_and_save_%widget_id%"), 
            i18np$t("display_and_save"), 
            iconProps = list(iconName = "SaveAs"),
            onClick = htmlwidgets::JS(paste0(
                "item => { ",
                "Shiny.setInputValue('", id, "-display_and_save_%widget_id%', Math.random()); ",
                "}"
            ))
        ),
        
        style = "margin-top: 15px; display: flex; gap: 10px; flex-wrap: wrap; padding-top: 15px; border-top: solid 1px #808080;"
    ),
    
    # ====================
    # ADDITIONAL CONFIGURATION EXAMPLES
    # ====================
    # Add more sections as needed for your specific plugin:
    #
    # STATISTICAL PARAMETERS:
    # - Confidence intervals, significance levels, test types, sample sizes
    #
    # ADVANCED FILTERING OPTIONS:
    # - Numerical ranges, categorical filters, regex patterns, exclusion rules
    #
    # DISPLAY PREFERENCES:
    # - Chart dimensions, font sizes, themes, grid options, annotation styles
    #
    # EXPORT SETTINGS:
    # - File formats (PNG, PDF, SVG), resolution, DPI, compression options
    #
    # PERFORMANCE OPTIONS:
    # - Data sampling limits, processing timeouts, caching preferences, parallel processing
    #
    # INTERACTIVE FEATURES:
    # - Tooltips, zoom controls, brush selection, click actions, hover effects
    #
    # REMEMBER: When adding new configuration elements, update server_user_configurations.R
    # in both the CONFIGURATION LOADING and CONFIGURATION SAVING sections to ensure
    # user preferences are properly persisted and restored across sessions.
)
