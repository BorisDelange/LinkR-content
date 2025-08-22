# ==========================================
# ui_output_settings.R - Output Configuration Panel
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 CONSOLE PLUGIN - CUSTOMIZED IMPLEMENTATION  🔧                         ██
# ██                                                                            ██
# ██  This file provides the Console plugin output settings interface with     ██
# ██  programming language and output type selection. Based on template.       ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# CONSOLE PLUGIN - OUTPUT SETTINGS UI FILE
# 
# This file defines the no-code configuration interface for the Console plugin.
# It provides user-friendly controls for programming language and output type selection.
# 
# CONSOLE PLUGIN INTERFACE:
# - Programming Language Selection (R or Python)
# - Output Type Selection (console, figure, table, etc.) - dynamically filtered by language
# - Auto-update Toggle for automatic code execution
# - Action Buttons for code execution and configuration saving
# 
# IMPORTANT NOTES:
# - UI elements are managed dynamically via server logic in server_output_settings.R
# - Each div includes an ID attribute for show/hide functionality via shinyjs
# - Output type options are filtered based on selected programming language
# - Save button visibility is controlled by user permissions

div(
    # ====================
    # LANGUAGE AND OUTPUT SELECTION
    # ====================
    # Programming language and output type in the same section
    div(
        id = ns("language_and_output_div_%widget_id%"),
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("prog_language_%widget_id%"), 
                label = i18np$t("language"),
                options = list(
                    list(key = "r", text = i18np$t("r"))
                ),
                value = "r"
            ),
            style = "width: 150px;"
        ),
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("output_%widget_id%"), 
                label = i18np$t("output"),
                options = list(
                    list(key = "console", text = i18np$t("console")),
                    list(key = "ui", text = i18np$t("ui_html")),
                    list(key = "figure", text = i18np$t("figure")),
                    list(key = "table", text = i18np$t("table")),
                    list(key = "datatable", text = i18np$t("datatable")),
                    list(key = "dygraphs", text = i18np$t("dygraphs")),
                    list(key = "plotly", text = i18np$t("plotly")),
                    list(key = "rmarkdown", text = i18np$t("rmarkdown"))
                ),
                value = "console"
            ),
            style = "width: 150px;"
        ),
        style = "display: flex; gap: 10px; flex-wrap: wrap; padding-bottom: 15px; border-bottom: solid 1px #808080;"
    ),
    
    # ====================
    # AUTO-UPDATE TOGGLE
    # ====================
    # Controls automatic code execution when data context changes
    div(
        id = ns("auto_update_section_%widget_id%"),
        div(
            id = ns("auto_update_div_%widget_id%"),
            shiny.fluent::Toggle.shinyInput(
                ns("auto_update_%widget_id%"), 
                label = i18np$t("auto_update_output"),
                value = TRUE
            )
        ),
        style = "display: flex; align-items: center; margin-top: 15px;"
    ),
    
    # ====================
    # ACTION BUTTONS
    # ====================
    # Standard action buttons - customize labels as needed
    div(
        id = ns("action_buttons_div_%widget_id%"),
        # Default action - Execute code only
        shiny.fluent::DefaultButton.shinyInput(
            ns("display_output_2_%widget_id%"), 
            i18np$t("display_output"), iconProps = list(iconName = "Play"),
            onClick = htmlwidgets::JS(paste0(
                "item => { Shiny.setInputValue('", id, "-display_output_%widget_id%', Math.random()); }"
            ))
        ),
        
        # Primary action - Execute and Save (if user has permissions)
        # This button is shown/hidden based on user access in server_output_settings.R
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
    )
)
