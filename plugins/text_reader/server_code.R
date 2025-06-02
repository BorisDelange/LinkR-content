# Server - Code

# Init code var
m$code_%widget_id% <- ""

# Prevent a bug with scroll into ace editor
shinyjs::delay(300, shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);"))

# Run code when button is clicked
observe_event(input$display_figure_%widget_id%, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());")))

# Run code at patient update
observe_event(m$selected_person, {
    if (isTRUE(input$run_code_on_data_update_%widget_id%)) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Run code
observe_event(input$run_code_%widget_id%, {
        
    # Reset UI
    output$notes_%widget_id% <- renderUI(div())
    
    # No patient selected
    if (is.na(m$selected_person)){
        output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("select_a_patient"), messageBarType = 5), style = "display: inline-block;"))
        shinyjs::hide("notes_datatable_%widget_id%")
        shinyjs::show("error_message_div_%widget_id%")
    }
    
    else {
    
        shinyjs::hide("error_message_div_%widget_id%")
        shinyjs::show("notes_datatable_%widget_id%")
        output$error_message_%widget_id% <- renderUI(div())
    
        # Get notes
        
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
            # We keep the EN datetime format for the column sorting to work (does not work with the french format).
            #dplyr::mutate_at("note_datetime", format_datetime, language = language, sec = FALSE)
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
    }
    
    # Go to "select notes" tab
    shinyjs::runjs(paste0("
        Shiny.setInputValue('", id, "-current_figure_settings_tab_%widget_id%', 'select_notes_%widget_id%');
        Shiny.setInputValue('", id, "-current_figure_settings_tab_trigger_%widget_id%', Math.random());"
    ))
})

# A row (a note) is selected

observe_event(input$notes_datatable_%widget_id%_rows_selected, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_note_%widget_id%', Math.random());")))

# Display note

## Function to highlight words in the HTML text
highlight_words <- function(text, words_to_highlight) {
    # If no words to highlight, return the original text
    if (nrow(words_to_highlight) == 0) return(text)
    
    # Use a simple for loop to replace each word individually
    highlighted_text <- text
    for (word in words_to_highlight$value) {
        # Simple pattern matching for each word individually
        pattern <- word
        
        # Replace the word with its highlighted version
        highlighted_text <- gsub(
            pattern,
            paste0("<span style='background-color: yellow; font-weight: bold;'>", 
                   word, 
                   "</span>"),
            highlighted_text,
            ignore.case = TRUE # Case-insensitive search
        )
    }
    
    return(highlighted_text)
}

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
        
            # Apply highlighting to the note text
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
