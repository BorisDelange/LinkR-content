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
    list(id = "search_word_sets", type = "multiselect", default = c()),
    list(id = "filter_notes_with_matches", type = "toggle", default = FALSE),
    list(id = "display_raw_text", type = "toggle", default = FALSE),
    list(id = "auto_update", type = "toggle", default = TRUE)
    # LLM inputs commented out:
    # list(id = "llm_provider", type = "dropdown", default = ""),
    # list(id = "llm_model", type = "dropdown", default = ""),
    # list(id = "include_notes", type = "dropdown", default = "none")
)

# ======================================
# TEXT READER SPECIFIC LOGIC
# ======================================

# Tab switching logic
sub_tabs <- c("select_notes", "keyword_search", "layout") # "chatbot" removed

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
})

# Display raw text toggle
observe_event(input$display_raw_text_%widget_id%, {
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_note_%widget_id%', Math.random());"))
})

# ======================================
# WORD SETS MANAGEMENT
# ======================================

# Create a word set
observe_event(input$create_word_set_%widget_id%, {
    
    word_set_name <- input$word_set_name_%widget_id%

    empty_name <- TRUE
    if (length(word_set_name) > 0) if (!is.na(word_set_name) && word_set_name != "") empty_name <- FALSE
    
    if (empty_name){
        shiny.fluent::updateTextField.shinyInput(session, "word_set_name_%widget_id%", errorMessage = i18np$t("provide_valid_name"))
        return()
    }
    
    shiny.fluent::updateTextField.shinyInput(session, "word_set_name_%widget_id%", errorMessage = NULL)
    
    sql <- glue::glue_sql("SELECT value FROM widgets_options WHERE widget_id = %widget_id% AND category = 'word_sets' AND name = 'word_set_name' AND LOWER(value) = {tolower(word_set_name)}", .con = m$db)
    name_already_used <- nrow(DBI::dbGetQuery(m$db, sql)) > 0
    
    if (name_already_used){
        shiny.fluent::updateTextField.shinyInput(session, "word_set_name_%widget_id%", errorMessage = i18np$t("name_already_used"))
        return()
    }
        
    shiny.fluent::updateTextField.shinyInput(session, "word_set_name_%widget_id%", errorMessage = NULL)
    
    # Add new word set in app db
    new_id <- get_last_row(m$db, "widgets_options") + 1
    
    new_data <- tibble::tibble(
        id = new_id, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
        category = "word_sets", name = "word_set_name", value = word_set_name, value_num = NA_real_, creator_id = m$user_id, datetime = now(), deleted = FALSE
    )
    DBI::dbAppendTable(m$db, "widgets_options", new_data)
    
    # Reset field & update dropdowns
    shiny.fluent::updateTextField.shinyInput(session, "word_set_name_%widget_id%", value = "")
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_word_sets_dropdowns_%widget_id%', Math.random());"))
})

# Update word set dropdowns  
observe_event(input$update_word_sets_dropdowns_%widget_id%, {

    sql <- glue::glue_sql("SELECT id, value FROM widgets_options WHERE widget_id = %widget_id% AND category = 'word_sets' AND name = 'word_set_name'", .con = m$db)
    word_sets <- DBI::dbGetQuery(m$db, sql) %>% convert_tibble_to_list(key_col = "id", text_col = "value")
    
    # Update search dropdown - preserve current selection
    current_search_selection <- input$search_word_sets_%widget_id%
    shiny.fluent::updateDropdown.shinyInput(session, "search_word_sets_%widget_id%", options = word_sets, value = current_search_selection)
    
    # Update edit dropdown - preserve current selection too
    current_edit_selection <- input$edit_word_set_%widget_id%
    shiny.fluent::updateDropdown.shinyInput(session, "edit_word_set_%widget_id%", options = word_sets, value = current_edit_selection)
})


# Edit a word set
observe_event(input$edit_word_set_%widget_id%, {
    
    sapply(c("edit_word_set_details_div_%widget_id%", "delete_word_set_div_%widget_id%"), shinyjs::show)
    
    # Load words of this word set
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_words_list_%widget_id%', Math.random());"))
})

# Delete word set modal handlers
observe_event(input$delete_word_set_%widget_id%, {
    if (length(input$edit_word_set_%widget_id%) == 0) return()
    shinyjs::show("delete_word_set_modal_%widget_id%")
})

observe_event(input$close_word_set_deletion_modal_%widget_id%, {
    shinyjs::hide("delete_word_set_modal_%widget_id%")
})

observe_event(input$confirm_word_set_deletion_%widget_id%, {

    word_set_id <- input$edit_word_set_%widget_id%
    
    # Delete row in db
    sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE id = {word_set_id}", .con = m$db))
    
    # Update dropdown
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_word_sets_dropdowns_%widget_id%', Math.random());"))
    
    # Update word set details
    shinyjs::hide("edit_word_set_details_div_%widget_id%")
    
    # Close modal
    shinyjs::hide("delete_word_set_modal_%widget_id%")
    
    # Hide delete button
    shinyjs::hide("delete_word_set_div_%widget_id%")
    
    # Notify user
    show_message_bar("word_set_deleted", "warning")
})

# Update words list
observe_event(input$update_words_list_%widget_id%, {
    
    word_set_id <- input$edit_word_set_%widget_id%
    sql <- glue::glue_sql("SELECT id, value FROM widgets_options WHERE widget_id = %widget_id% AND category = 'word_sets' AND link_id = {word_set_id} AND name = 'word_name'", .con = m$db)
    words_list <- DBI::dbGetQuery(m$db, sql)
    
    words_list_ui <- tagList()
    
    if (nrow(words_list) > 0){
    
        for (i in 1:nrow(words_list)){
        
            row <- words_list[i, ]
            
            words_list_ui <- tagList(
                words_list_ui,
                div(
                    div(
                        shiny.fluent::IconButton.shinyInput(ns(paste0("remove_row_", row$id, "_%widget_id%")), iconProps = list(iconName = "Cancel"), style = "height: 20px; margin: 0; font-size: 10px;"),
                        onclick = paste0(
                            "Shiny.setInputValue('", id, "-remove_word_trigger_%widget_id%', Math.random());",
                            "Shiny.setInputValue('", id, "-remove_word_%widget_id%', ", row$id, ");"
                        ),
                        class = "small_icon_button", style = "width: 18px; margin-right: 3px;"
                    ),
                    create_hover_card(ui = 
                        div(
                            row$value,
                            style = paste0(
                                "display: inline-block; color: white; max-width: 320px; border-radius: 8px; padding: 1px 5px; align-items: center; height: 18px;",
                                "font-weight: 600; white-space: nowrap; overflow: hidden; background-color: #FF8C00;"
                            )
                        ), 
                        text = row$value
                    ),
                    style = "display: flex; align-items: center; margin-right: 8px;"
                )
            )
        }
    }
    
    output$words_list_%widget_id% <- renderUI(words_list_ui)
})

# Add a new word
observe_event(input$add_new_word_%widget_id%, {
    
    word_set_id <- input$edit_word_set_%widget_id%
    
    if (length(word_set_id) > 0){
    
        word_name <- input$word_name_%widget_id%
        
        empty_name <- TRUE
        if (length(word_name) > 0) if (!is.na(word_name) && word_name != "") empty_name <- FALSE
        
        if (empty_name) shiny.fluent::updateTextField.shinyInput(session, "word_name_%widget_id%", errorMessage = i18np$t("provide_valid_name"))
        else {
            shiny.fluent::updateTextField.shinyInput(session, "word_name_%widget_id%", errorMessage = NULL)
            
            sql <- glue::glue_sql(paste0(
                "SELECT value FROM widgets_options WHERE widget_id = %widget_id% AND category = 'word_sets' AND link_id = {word_set_id} ",
                "AND name = 'word_name' AND LOWER(value) = {tolower(word_name)}"), .con = m$db)
            name_already_used <- nrow(DBI::dbGetQuery(m$db, sql)) > 0
            
            if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "word_name_%widget_id%", errorMessage = i18np$t("name_already_used"))
            else {
                shiny.fluent::updateTextField.shinyInput(session, "word_name_%widget_id%", errorMessage = NULL)
                
                # Add new word in app db
                new_id <- get_last_row(m$db, "widgets_options") + 1
                
                new_data <- tibble::tibble(
                    id = new_id, widget_id = %widget_id%, person_id = NA_integer_, link_id = word_set_id,
                    category = "word_sets", name = "word_name", value = word_name, value_num = NA_real_, creator_id = m$user_id, datetime = now(), deleted = FALSE
                )
                DBI::dbAppendTable(m$db, "widgets_options", new_data)
                
                # Reset field & update dropdowns
                shiny.fluent::updateTextField.shinyInput(session, "word_name_%widget_id%", value = "")
                shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_words_list_%widget_id%', Math.random());"))
            }
        }
    }
})

# Remove a word
observe_event(input$remove_word_trigger_%widget_id%, {
        
    word_id <- input$remove_word_%widget_id%
    
    if (length(word_id) > 0){
    
        # Remove word from database
        sql <- glue::glue_sql("DELETE FROM widgets_options WHERE id = {word_id}", .con = m$db)
        DBI::dbExecute(m$db, sql)
    
        # Update words list
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_words_list_%widget_id%', Math.random());"))
    }
})
