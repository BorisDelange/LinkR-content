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
        
        # Dataset selection section (first)
        div(
            style = "margin-bottom: 20px;",
            shiny.fluent::Dropdown.shinyInput(
                ns("selected_dataset_%widget_id%"),
                label = i18np$t("select_dataset"),
                options = list(),
                value = NULL
            ),
            style = "width: 300px;"
        ),
        
        # File management section (separated with border)
        div(
            style = "border: 1px solid #d1d1d1; border-radius: 8px; padding: 15px; background-color: #fafafa;",
            
            # Upload new file subsection
            div(
                div(
                    class = "file-browser-button",
                    fileInput(
                        ns("csv_file_%widget_id%"),
                        label = NULL,
                        buttonLabel = i18np$t("browse"),
                        placeholder = i18np$t("select_csv_file"),
                        accept = ".csv",
                        width = "300px"
                    )
                ),
                style = "margin-bottom: 15px;"
            ),
            
            # File management subsection
            div(
                id = ns("file_management_div_%widget_id%"),
                tags$h4(i18np$t("manage_files"), style = "margin-bottom: 10px; color: #323130; font-size: 14px; font-weight: 600;"),
                uiOutput(ns("file_list_%widget_id%"))
            )
        )
    ),
    
    # ====================
    # VISUALIZATION TAB
    # ====================
    shinyjs::hidden(
        div(
            id = ns("visualization_div_%widget_id%"),
            # Variables selection section
            div(
                style = "border: 1px solid #d1d1d1; border-radius: 8px; padding: 5px 15px 15px 15px; margin-bottom: 20px; background-color: #fafafa;",
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
                            options = list(list(key = "", text = i18np$t("none"))),
                            value = ""
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
                    style = "display: flex; flex-wrap: wrap; gap: 10px;"
                )
            ),
            # Labels and titles section
            div(
                style = "border: 1px solid #d1d1d1; border-radius: 8px; padding: 5px 15px 15px 15px; background-color: #fafafa;",
                div(
                    div(
                        shiny.fluent::TextField.shinyInput(
                            ns("plot_title_%widget_id%"),
                            label = i18np$t("plot_title"),
                            value = i18np$t("data_analysis_results")
                        ),
                        style = "width: 200px; margin-right: 15px;"
                    ),
                    div(
                        shiny.fluent::TextField.shinyInput(
                            ns("x_legend_%widget_id%"),
                            label = i18np$t("x_axis_legend"),
                            value = ""
                        ),
                        style = "width: 200px; margin-right: 15px;"
                    ),
                    div(
                        shiny.fluent::TextField.shinyInput(
                            ns("y_legend_%widget_id%"),
                            label = i18np$t("y_axis_legend"),
                            value = ""
                        ),
                        style = "width: 200px;"
                    ),
                    style = "display: flex; flex-wrap: wrap; gap: 10px;"
                )
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
                style = "width: 200px;"
            ),
            # TABLE ONE OPTIONS
            div(
                id = ns("table_one_options_%widget_id%"),
                div(
                    style = "display: flex; flex-wrap: wrap; gap: 10px; margin-top: 15px;",
                    div(
                        div(
                            shiny.fluent::Dropdown.shinyInput(
                                ns("table_variables_%widget_id%"),
                                label = i18np$t("variables_to_include"),
                                options = list(),
                                multiSelect = TRUE,
                                value = NULL
                            ),
                            style = "width: 200px;"
                        ),
                        div(
                            create_hover_card(
                                ui = shiny.fluent::IconButton.shinyInput(
                                    ns("table_variables_check_all_%widget_id%"), 
                                    iconProps = list(iconName = "CheckboxComposite"),
                                    styles = list(
                                        root = list(color = "#000000"),
                                        icon = list(color = "#000000")
                                    )
                                ), 
                                text = i18np$t("select_all_variables")
                            ),
                            create_hover_card(
                                ui = shiny.fluent::IconButton.shinyInput(
                                    ns("table_variables_uncheck_all_%widget_id%"), 
                                    iconProps = list(iconName = "Checkbox"),
                                    styles = list(
                                        root = list(color = "#000000"),
                                        icon = list(color = "#000000")
                                    )
                                ), 
                                text = i18np$t("unselect_all_variables")
                            ),
                            style = "margin: 27px 0 0 5px; display: flex;"
                        ),
                        style = "display: flex;"
                    ),
                    div(
                        shiny.fluent::Dropdown.shinyInput(
                            ns("grouping_variable_%widget_id%"),
                            label = i18np$t("grouping_variable"),
                            options = list(),
                            value = NULL
                        ),
                        style = "width: 200px;"
                    ),
                    div(
                        shiny.fluent::TextField.shinyInput(
                            ns("table_title_%widget_id%"),
                            label = i18np$t("table_title"),
                            value = "Table 1. Statistiques descriptives"
                        ),
                        style = "width: 200px;"
                    )
                )
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
    
    style = "height: 100%; display: flex; flex-direction: column;",
    
    # ====================
    # DELETE FILE CONFIRMATION MODAL
    # ====================
    # Hidden confirmation modal for deleting files (same style as user configurations)
    shinyjs::hidden(
        div(
            id = ns("delete_file_modal_%widget_id%"),
            div(
                # Confirmation dialog content
                div(
                    # Dialog title
                    tags$h1(i18np$t("delete_file"), style = "font-size: 14px;"),
                    
                    # Warning message
                    tags$p(i18np$t("confirm_delete_file")),
                    
                    # Action buttons (Cancel + Delete)
                    div(
                        # Cancel button
                        shiny.fluent::DefaultButton.shinyInput(
                            ns("close_delete_file_modal_%widget_id%"), 
                            i18np$t("dont_delete"), iconProps = list(iconName = "Cancel")
                        ),
                        
                        # Delete confirmation button (styled as dangerous action)
                        div(
                            shiny.fluent::PrimaryButton.shinyInput(
                                ns("confirm_delete_file_%widget_id%"), 
                                i18np$t("delete"), iconProps = list(iconName = "Delete")
                            ), 
                            class = "delete_button"
                        ),
                        
                        style = "position: absolute; right: 10px; bottom: 8px; display: flex; gap: 5px;"
                    ),
                    
                    # Dialog content styling
                    style = "background: #fff; padding: 5px 10px 10px 15px; position: relative; width: 400px; height: 120px;"
                ),
                
                # Dialog overlay styling
                style = "display: flex; align-items: center; justify-content: center; position: absolute; left: 0; top: 0; width: 100%; height: 100%; background-color: rgba(0,0,0,0.2); z-index: 1000;"
            )
        )
    )
)
