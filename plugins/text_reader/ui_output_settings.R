# ==========================================
# ui_output_settings.R - Output Configuration Panel
# ==========================================

# ████████████████████████████████████████████████████████████████████████████████
# ██                                                                            ██
# ██  🔧 OPTIONAL CUSTOMIZATION - PLUGIN ENHANCEMENT  🔧                        ██
# ██                                                                            ██
# ██  This file provides default functionality that works out-of-the-box.       ██
# ██  Customize only if you need specific features or modifications.            ██
# ██  Safe to use as-is for standard plugin requirements.                       ██
# ██                                                                            ██
# ████████████████████████████████████████████████████████████████████████████████

# TEXT READER PLUGIN - OUTPUT SETTINGS UI FILE
# 
# This file defines the no-code configuration interface for the text reader plugin.
# It provides user-friendly controls for managing clinical note display, keyword search,
# and text analysis settings without requiring manual coding.
# 
# PLUGIN INTERFACE FEATURES:
# - Tab-based navigation between Notes, Keyword Search, and Layout sections
# - Word set management with creation, editing, and deletion capabilities
# - Search configuration with multi-select word set dropdown
# - Display preferences for raw text vs formatted viewing
# - Auto-update toggle for patient selection changes
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
    # TABS FOR TEXT READER
    # ====================
    div(
        id = ns("figure_settings_tabs_%widget_id%"),
        tags$button(id = ns("select_notes_%widget_id%"), i18np$t("notes"), class = "widget_pivot_item selected_widget_pivot_item", onclick = figure_settings_tab_item_js),
        tags$button(id = ns("keyword_search_%widget_id%"), i18np$t("keyword_search"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js),
        # tags$button(id = ns("chatbot_%widget_id%"), i18np$t("chatbot"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js), # LLM functionality commented out
        tags$button(id = ns("layout_%widget_id%"), i18np$t("layout"), class = "widget_pivot_item", onclick = figure_settings_tab_item_js),
        class = "pivot"
    ),
    
    # ====================
    # NOTES DATATABLE TAB
    # ====================
    div(
        id = ns("select_notes_div_%widget_id%"),
        DT::DTOutput(ns("notes_datatable_%widget_id%")), 
        style = "margin-top: 15px;"
    ),
    
    # ====================
    # KEYWORD SEARCH TAB
    # ====================
    shinyjs::hidden(
        div(
            id = ns("keyword_search_div_%widget_id%"),
            div(
                div(
                    div(shiny.fluent::Dropdown.shinyInput(ns("search_word_sets_%widget_id%"), label = i18np$t("search_word_sets"), options = word_sets, multiSelect = TRUE), style = "width: 200px;"),
                    div(shiny.fluent::Toggle.shinyInput(ns("filter_notes_with_matches_%widget_id%"), label = i18np$t("filter_notes_with_matches")), style = "margin-top: 29px;"),
                    style = "display: flex; gap: 10px;"
                ),
                tags$hr(style = "border: none; border-top: 1px solid #ccc; height: 1px; margin: 20px 0 10px 0;"),
                div(
                    div(shiny.fluent::TextField.shinyInput(ns("word_set_name_%widget_id%"), label = i18np$t("create_word_set")), style = "width: 200px;"),
                    div(
                        shiny.fluent::IconButton.shinyInput(ns("create_word_set_%widget_id%"), iconProps = list(iconName = "Add")),
                        style = "margin-top: 26px;", class = "widget_icon"
                    ),
                    style = "display: flex; gap: 5px;"
                ),
                tags$hr(style = "border: none; border-top: 1px solid #ccc; height: 1px; margin: 20px 0 10px 0;"),
                div(
                    div(shiny.fluent::Dropdown.shinyInput(ns("edit_word_set_%widget_id%"), label = i18np$t("edit_word_set"), options = word_sets), style = "width: 200px;"),
                    shinyjs::hidden(
                        div(
                            id = ns("delete_word_set_div_%widget_id%"),
                            shiny.fluent::IconButton.shinyInput(ns("delete_word_set_%widget_id%"), iconProps = list(iconName = "Delete")),
                            style = "margin-top: 26px;", class = "widget_icon"
                        )
                    ),
                    style = "display: flex; gap: 5px;"
                ),
                shinyjs::hidden(
                    div(
                        id = ns("edit_word_set_details_div_%widget_id%"),
                        div(
                            div(shiny.fluent::TextField.shinyInput(ns("word_name_%widget_id%"), label = i18np$t("add_new_word")), style = "width: 200px;"),
                            div(
                                shiny.fluent::IconButton.shinyInput(ns("add_new_word_%widget_id%"), iconProps = list(iconName = "Add")),
                                style = "margin-top: 26px;", class = "widget_icon"
                            ),
                            style = "display: flex; gap: 5px;"
                        ),
                        uiOutput(ns("words_list_%widget_id%"), style = "margin-top: 15px; display: flex; flex-wrap: wrap; gap: 5px;")
                    )
                ),
                style = "height: 100%; margin-top: 10px;"
            )
        )
    ),
    
    # ====================
    # DELETE WORD SET MODAL
    # ====================
    shinyjs::hidden(
      div(
        id = ns("delete_word_set_modal_%widget_id%"),
        div(
            tags$h1(i18np$t("delete_word_set_title"), style = "font-size: 14px;"),
            tags$p(i18np$t("delete_word_set_text")),
            div(
                shiny.fluent::DefaultButton.shinyInput(ns("close_word_set_deletion_modal_%widget_id%"), i18np$t("dont_delete")),
                div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_word_set_deletion_%widget_id%"), i18np$t("delete")), class = "delete_button"),
                style = "position: absolute; right: 10px; bottom: 8px; display: flex; gap: 5px;"
            ),
            style = "background: #fff; padding: 5px 10px 10px 15px; position: relative; width: 400px; height: 120px;"
        ),
        style = "display: flex; align-items: center; justify-content: center; position: absolute; left: 0; top: 0;  width: 100%; height: 100%; background-color: rgba(0,0,0,0.2); z-index: 1000;"
      )
    ),
    
    # ====================
    # LAYOUT TAB  
    # ====================
    shinyjs::hidden(
        div(
            id = ns("layout_div_%widget_id%"),
            div(
                shiny.fluent::Toggle.shinyInput(ns("display_raw_text_%widget_id%"), value = FALSE),
                tags$label(i18np$t("display_raw_text"), `for` = ns("display_raw_text_%widget_id%"), style = "margin-left: 5px;"),
                style = "display: flex; margin-top: 15px;" 
            ),
            div(
                shiny.fluent::Toggle.shinyInput(ns("auto_update_%widget_id%"), value = TRUE),
                tags$label(i18np$t("automatic_updates"), `for` = ns("auto_update_%widget_id%"), style = "margin-left: 5px;"),
                style = "display: flex; margin-top: 15px;" 
            ),
            style = "height: 100%;"
        )
    ),
    
    style = "height: 100%; display: flex; flex-direction: column;"
)
