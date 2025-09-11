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
    # IMPORT DATA TAB
    # ====================
    div(
        id = ns("import_data_div_%widget_id%"),
        div(
            fileInput(
                ns("csv_file_%widget_id%"),
                i18np$t("csv_file"),
                accept = c(".csv"),
                buttonLabel = i18np$t("browse"),
                placeholder = "No file selected"
            ),
            div(
                shiny.fluent::Dropdown.shinyInput(
                    ns("selected_dataset_%widget_id%"),
                    label = i18np$t("select_dataset"),
                    options = list(),
                    value = NULL
                ),
                style = "width: 250px; margin-top: 15px;"
            ),
            style = "margin-top: 15px;"
        )
    ),
    
    # ====================
    # VISUALIZATION TAB
    # ====================
    shinyjs::hidden(
        div(
            id = ns("visualization_div_%widget_id%"),
            div(
                div(
                    shiny.fluent::Dropdown.shinyInput(
                        ns("x_axis_%widget_id%"),
                        label = i18np$t("x_axis"),
                        options = list(),
                        value = NULL
                    ),
                    style = "width: 200px; margin-right: 15px;"
                ),
                div(
                    shiny.fluent::Dropdown.shinyInput(
                        ns("y_axis_%widget_id%"),
                        label = i18np$t("y_axis"),
                        options = list(),
                        value = NULL
                    ),
                    style = "width: 200px; margin-right: 15px;"
                ),
                div(
                    shiny.fluent::Dropdown.shinyInput(
                        ns("plot_type_%widget_id%"),
                        label = i18np$t("plot_type"),
                        options = list(
                            list(key = "histogram", text = i18np$t("histogram_plot")),
                            list(key = "scatter", text = i18np$t("scatter_plot")),
                            list(key = "boxplot", text = i18np$t("boxplot")),
                            list(key = "barplot", text = i18np$t("barplot"))
                        ),
                        value = "histogram"
                    ),
                    style = "width: 200px;"
                ),
                style = "display: flex; flex-wrap: wrap; gap: 10px; margin-top: 15px;"
            ),
            div(
                shiny.fluent::TextField.shinyInput(
                    ns("plot_title_%widget_id%"),
                    label = i18np$t("plot_title"),
                    value = i18np$t("data_analysis_results")
                ),
                style = "width: 300px; margin-top: 15px;"
            )
        )
    ),
    
    # ====================
    # STATISTICS TAB
    # ====================
    shinyjs::hidden(
        div(
            id = ns("statistics_div_%widget_id%"),
            div(
                shiny.fluent::Dropdown.shinyInput(
                    ns("statistics_type_%widget_id%"),
                    label = i18np$t("statistics_type"),
                    options = list(
                        list(key = "table_one", text = i18np$t("table_one")),
                        list(key = "variable_comparison", text = i18np$t("variable_comparison"))
                    ),
                    value = "table_one"
                ),
                style = "width: 200px; margin-top: 15px;"
            ),
            # TABLE ONE OPTIONS
            div(
                id = ns("table_one_options_%widget_id%"),
                shiny.fluent::Dropdown.shinyInput(
                    ns("grouping_variable_%widget_id%"),
                    label = i18np$t("grouping_variable"),
                    options = list(),
                    value = NULL
                ),
                style = "width: 200px; margin-top: 15px;"
            ),
            # VARIABLE COMPARISON OPTIONS
            shinyjs::hidden(
                div(
                    id = ns("variable_comparison_options_%widget_id%"),
                    div(
                        div(
                            shiny.fluent::Dropdown.shinyInput(
                                ns("variable_1_%widget_id%"),
                                label = i18np$t("variable_1"),
                                options = list(),
                                value = NULL
                            ),
                            style = "width: 200px; margin-right: 15px;"
                        ),
                        div(
                            shiny.fluent::Dropdown.shinyInput(
                                ns("variable_2_%widget_id%"),
                                label = i18np$t("variable_2"),
                                options = list(),
                                value = NULL
                            ),
                            style = "width: 200px;"
                        ),
                        style = "display: flex; margin-top: 15px;"
                    )
                )
            )
        )
    ),
    
    # ====================
    # REPORT TAB
    # ====================
    shinyjs::hidden(
        div(
            id = ns("report_div_%widget_id%"),
            div(
                shiny.fluent::Label("Report generation options will be available here", style = "margin-top: 15px;")
            )
        )
    ),
    
    # ====================
    # AUTO UPDATE TOGGLE
    # ====================
    div(
        div(
            id = ns("auto_update_div_%widget_id%"),
            shiny.fluent::Toggle.shinyInput(
                ns("auto_update_%widget_id%"), 
                label = i18np$t("automatic_updates"),
                value = TRUE
            )
        ),
        style = "margin-top: 15px; display: flex; align-items: center; padding-top: 15px; border-top: solid 1px #808080;"
    ),
    
    # ====================
    # ACTION BUTTONS
    # ====================
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
    
    style = "height: 100%; display: flex; flex-direction: column;"
)
