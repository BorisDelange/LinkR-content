# ==========================================
# server_code.R - Code Editor Server Logic
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

# PLUGIN TEMPLATE - CODE EDITOR SERVER FILE
# 
# This file handles the server-side logic for the code editor and output generation.
# It provides the foundation for automatic code generation from UI settings and
# manual code execution, creating a seamless no-code to code experience.
# 
# WHEN CREATING A NEW PLUGIN WITH THIS TEMPLATE:
# - Implement the generate_output_code_%widget_id%() function for your specific analysis
# - Customize the code execution logic in the run_code observer
# - Add patient selection validation following the commented example (lines 307-318)
# - Configure UI message handling for user-friendly error display (lines 350-398)
# - Add translations for plugin-specific messages in translations.csv
# - Add any plugin-specific helper functions and variables
# - Modify auto-execution triggers based on your data dependencies
# 
# CORE FUNCTIONALITY:
# - Automatic R code generation from UI configuration settings
# - Code editor with syntax highlighting and keyboard shortcuts
# - Manual code execution with error handling and output display
# - Auto-execution when data context changes (optional)
# - Integration with the widget's database storage system
# 
# COMMON PLUGIN PATTERNS:
# 
# HEALTHCARE DATA ANALYSIS PLUGINS:
#   - Patient selection validation with user-friendly error messages
#   - OMOP medical data integration and filtering
#   - Healthcare indicator calculations (mortality, LOS, readmission rates)
#   - Generate statistical analysis code (t-tests, regression, ANOVA)
#   - Create summary statistics and descriptive analysis
# 
# VISUALIZATION PLUGINS:
#   - Timeline visualizations with dygraphs and plotly
#   - Healthcare dashboards with indicator cards
#   - Generate ggplot2 code for various chart types
#   - Create interactive plotly visualizations with medical data
#   - Build custom plotting functions with user parameters
# 
# MEDICAL DATA PROCESSING PLUGINS:
#   - OMOP CDM data filtering and transformation
#   - Patient cohort and visit detail processing  
#   - Medical concept mapping and vocabulary integration
#   - Care site and hospital unit filtering
#   - Generate data cleaning and preparation workflows
# 
# REPORTING PLUGINS:
#   - Healthcare indicator reports and dashboards
#   - Patient summary reports with medical timelines
#   - Generate markdown or HTML report code
#   - Create formatted table output code
#   - Build export and download functionality

# ======================================
# INITIALIZATION
# ======================================

# Initialize code storage variable
m$code_%widget_id% <- ""

# Fix ACE editor rendering issues on startup
# Delay ensures DOM is fully loaded before triggering resize
shinyjs::delay(300, shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);"))

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
    # For text reader, we directly run the code without auto-generation
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# ======================================
# TEXT READER HELPER FUNCTIONS  
# ======================================

# Function to highlight words in text
highlight_words <- function(text, words_to_highlight) {
    if (nrow(words_to_highlight) == 0) return(text)
    
    highlighted_text <- text
    for (word in words_to_highlight$value) {
        pattern <- word
        
        highlighted_text <- gsub(
            pattern,
            paste0("<span style='background-color: yellow; font-weight: bold;'>", 
                   word, 
                   "</span>"),
            highlighted_text,
            ignore.case = TRUE
        )
    }
    
    return(highlighted_text)
}

# ======================================
# AUTO-EXECUTION TRIGGERS
# ======================================

# Auto-run code when patient changes
observe_event(m$selected_person, {
    if (isTRUE(input$auto_update_%widget_id%)) {
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

# ======================================
# CODE EXECUTION ENGINE
# ======================================

# Main code execution handler
observe_event(input$run_code_%widget_id%, {
    
    # Reset UI
    output$notes_%widget_id% <- renderUI(div())
    
    # Check patient selection
    if (is.na(m$selected_person)){
        output$notes_%widget_id% <- renderUI({
            div(
                style = "display: flex; justify-content: center; align-items: center; height: 100%; text-align: center; padding: 10px;",
                div(
                    style = "font-size: 14px; color: #6c757d;",
                    i18np$t("select_a_patient")
                )
            )
        })
        shinyjs::hide("notes_datatable_%widget_id%")
        return()
    }
    
    # Show datatable and hide error
    shinyjs::show("notes_datatable_%widget_id%")
    
    # Get notes from database
    notes <- tibble::tibble(note_type_concept_name = character(), note_title = character(), note_datetime = character())
    
    sql <- glue::glue_sql("
        SELECT
            n.note_id,
            c.concept_name AS note_type_concept_name,
            n.note_title,
            n.note_text,
            n.note_datetime
        FROM note n
        LEFT JOIN concept c ON n.note_type_concept_id = c.concept_id
        WHERE person_id = {m$selected_person}
    ", .con = d$con)
    
    m$notes_%widget_id% <-
        DBI::dbGetQuery(d$con, sql) %>% 
        tibble::as_tibble() %>% 
        dplyr::mutate_at("note_datetime", format_datetime, language = "en", sec = FALSE)
    
    # Filter notes if filter_notes_with_matches is activated and search words are selected
    if (nrow(m$notes_%widget_id%) > 0) {
        
        # Check if filtering is enabled and search word sets are selected
        if (!is.null(input$filter_notes_with_matches_%widget_id%) && 
            input$filter_notes_with_matches_%widget_id% && 
            length(input$search_word_sets_%widget_id%) > 0) {
            
            # Get words to search for
            word_sets_ids <- input$search_word_sets_%widget_id%
            sql <- glue::glue_sql("SELECT DISTINCT(value) FROM widgets_options WHERE widget_id = %widget_id% AND category = 'word_sets' AND link_id IN ({word_sets_ids*}) AND name = 'word_name'", .con = m$db)
            words <- DBI::dbGetQuery(m$db, sql)
            
            if (nrow(words) > 0) {
                # Keep only notes that contain at least one of the words
                m$notes_%widget_id% <- m$notes_%widget_id% %>%
                    dplyr::filter(
                        purrr::map_lgl(note_text, function(text) {
                            any(purrr::map_lgl(words$value, function(word) {
                                grepl(word, text, ignore.case = TRUE)
                            }))
                        })
                    )
            }
        }
        
        notes <- m$notes_%widget_id% %>% dplyr::select(note_type_concept_name, note_title, note_datetime)
    }
    
    page_length <- 10
    if (length(input$notes_datatable_%widget_id%_state$length) > 0) page_length <- input$notes_datatable_%widget_id%_state$length
    else if (length(m$datatable_page_length_%widget_id%) > 0) page_length <- m$datatable_page_length_%widget_id%
    
    render_datatable(
      data = notes, page_length = page_length,
      output_name = "notes_datatable_%widget_id%", col_names = c(i18np$t("category"), i18np$t("title"), i18np$t("datetime")),
      datatable_dom = "<'datatable_length'l><'top't><'bottom'p>", sortable_cols = c("note_type_concept_name", "note_title", "note_datetime"),
      searchable_cols = c("note_type_concept_name", "note_title", "note_datetime"), factorize_cols = "note_type_concept_name", filter = TRUE,
      search_filters = input$notes_datatable_%widget_id%_search_columns
    )
    
    # Go to "select notes" tab
    shinyjs::runjs(paste0("
        Shiny.setInputValue('", id, "-current_figure_settings_tab_%widget_id%', 'select_notes_%widget_id%');
        Shiny.setInputValue('", id, "-current_figure_settings_tab_trigger_%widget_id%', Math.random());"
    ))
})

# A row (a note) is selected
observe_event(input$notes_datatable_%widget_id%_rows_selected, {
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_note_%widget_id%', Math.random());"))
})

# Display selected note
observe_event(input$reload_note_%widget_id%, {
    
    if (length(input$notes_datatable_%widget_id%_rows_selected) > 0){
        
        note <- m$notes_%widget_id%[input$notes_datatable_%widget_id%_rows_selected, ]
        
        # Search words
        word_sets_ids <- input$search_word_sets_%widget_id%
        words <- tibble::tibble()
        
        if (length(input$search_word_sets_%widget_id%) > 0){
            sql <- glue::glue_sql("SELECT DISTINCT(value) FROM widgets_options WHERE widget_id = %widget_id% AND category = 'word_sets' AND link_id IN ({word_sets_ids*}) AND name = 'word_name'", .con = m$db)
            words <- DBI::dbGetQuery(m$db, sql)
        }
        
        display_raw_note <- FALSE
        if (length(input$display_raw_text_%widget_id%) > 0) if (input$display_raw_text_%widget_id%) display_raw_note <- TRUE
        
        if (display_raw_note){
            note_text_div <- tags$pre(note$note_text)
        } else {
        
            highlighted_text <- highlight_words(note$note_text, words)
        
            note_text_div <-
                div(
                    tags$iframe(
                            srcdoc = div(HTML(highlighted_text), style = "white-space: pre-wrap; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; font-size: 12px;"),
                            style = "width: 100%; height: 100%; border: none;"
                        ),
                    style = "height: 100%;"
                )
        }
        
        output$notes_%widget_id% <- renderUI({
            div(
                div(strong(note$note_title), " - ", note$note_datetime, language = language, style="margin-left: 8px;", title = paste0("note_id = ", note$note_id)),
                note_text_div,
                style = "height: 100%;"
            )
        })
    }
})
