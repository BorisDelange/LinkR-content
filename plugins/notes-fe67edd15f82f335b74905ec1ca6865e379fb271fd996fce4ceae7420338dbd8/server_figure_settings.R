##############
# TABS       #
##############

sub_tabs <- c("select_notes", "words_sets", "layout")

observeEvent(input$current_figure_settings_tab_trigger_%widget_id%, {
    %req%
     if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$current_figure_settings_tab_trigger_%widget_id%"))
    
    tryCatch({
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
    }, error = function(e) cat(paste0("\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

##############
# DATATABLE  #
##############

# Reload datatable of all notes

observeEvent(input$reload_notes_datatable_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$reload_notes_datatable_%widget_id%"))
    
    tryCatch({
    
        if (nrow(m$notes_%widget_id%) > 0){
            
            notes <-
                m$notes_%widget_id% %>%
                dplyr::select(note_type_concept_id, note_title, note_datetime) %>%
                dplyr::left_join(
                    d$dataset_all_concepts %>% dplyr::select(note_type_concept_id = concept_id_1, note_type_concept_name = concept_name_1),
                    by = "note_type_concept_id"
                ) %>%
                dplyr::relocate("note_type_concept_name", .before = "note_type_concept_id") %>%
                dplyr::select(-note_type_concept_id) %>%
                dplyr::mutate_at("note_datetime", as.character) %>%
                dplyr::mutate_at("note_type_concept_name", as.factor)
        }
        else {
            notes <- tibble::tibble(note_type_concept_name = character(), note_title = character(), note_datetime = character())
        }
        
        # If DT proxy doesn't exist, create it
        if (length(m$notes_datatable_proxy_%widget_id%) == 0){
        
            render_datatable(
              output = output, ns = ns, i18n = i18n, data = notes,
              output_name = "notes_datatable_%widget_id%", col_names = c(i18np$t("category"), i18np$t("title"), i18np$t("datetime")),
              datatable_dom = "<'datatable_length'l><'top't><'bottom'p>", sortable_cols = c("note_type_concept_name", "note_title", "note_datetime"),
              searchable_cols = c("note_type_concept_name", "note_title", "note_datetime"), factorize_cols = "note_type_concept_name", filter = TRUE
            )
            
            # Create datatable proxy
            m$notes_datatable_proxy_%widget_id% <- DT::dataTableProxy("notes_datatable_%widget_id%", deferUntilFlush = FALSE)
        }
        else {
            DT::replaceData(m$notes_datatable_proxy_%widget_id%, notes, resetPaging = FALSE, rownames = FALSE)
        }
        
    }, error = function(e) cat(paste0("\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# A row is selected

observeEvent(input$notes_datatable_%widget_id%_rows_selected, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$notes_datatable_%widget_id%_rows_selected"))
    
    tryCatch({
        
        selected_notes <- m$notes_%widget_id%[input$notes_datatable_%widget_id%_rows_selected, ]
        m$show_notes_%widget_id% <- m$notes_%widget_id% %>% dplyr::filter(note_id %in% selected_notes$note_id)
        
        # Update ace editor
        #code <- paste0("m$notes_%widget_id% %>%\n    dplyr::filter(note_id %in% ", selected_notes$note_id, ")")
        #shinyAce::updateAceEditor(session, "code_editor_%widget_id%", value = code)
        
        # Show selected notes
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_notes_%widget_id%', Math.random())"))
        
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

##############
# WORDS SETS #
##############

# Initiate words list
m$words_list_%widget_id% <- tibble::tibble(id = integer(), text = character())

# Reload words sets

observeEvent(input$reload_words_sets_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$reload_words_sets_%widget_id%"))
    
    tryCatch({
        sql <- glue::glue_sql("SELECT id, value FROM widgets_options WHERE widget_id = %widget_id% AND name = 'word_set'", .con = m$db)
        words_sets <- DBI::dbGetQuery(m$db, sql) %>% convert_tibble_to_list(key_col = "id", text_col = "value")
        
        shiny.fluent::updateDropdown.shinyInput(session, "words_set_%widget_id%", options = words_sets)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_words_sets_%widget_id%', Math.random())"))

# A words set is selected

observeEvent(input$words_set_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$words_set_%widget_id%"))
    
    shinyjs::show("words_set_details_div_%widget_id%")
    
    tryCatch({
        
        # Load words list
        m$words_list_%widget_id% <- tibble::tibble(id = integer(), text = character())
        
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

## Add a new word

observeEvent(input$add_new_word_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$add_new_word_%widget_id%"))
    
    tryCatch({
        
        if (input$new_word_%widget_id% == "" | nchar(input$new_word_%widget_id%) < 2) stop()
        
        # Add new word to words list
        m$words_list_%widget_id% <- m$words_list_%widget_id% %>% dplyr::bind_rows(tibble::tibble(id = sample(1:100, 1), text = input$new_word_%widget_id%))
        
        # Reset textfield
        shiny.fluent::updateTextField.shinyInput(session, "new_word_%widget_id%", value = "")
        
        # Reload words UI
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_words_ui_%widget_id%', Math.random())"))
        
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

## Reload words UI

selected_word_style <- paste0("
    display: inline-block;
    color: white;
    background-color: #FF8C00;
    max-width: 320px;
    border-radius: 8px;
    padding: 1px 5px;
    align-items: center;
    height: 18px;
    font-weight: 600;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
")

observeEvent(input$reload_words_ui_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$reload_words_ui_%widget_id%"))
    
    tryCatch({
        
        words_ui <- tagList()
        
        if (nrow(m$words_list_%widget_id%) > 0){
            for (i in 1:nrow(m$words_list_%widget_id%)){
                row <- m$words_list_%widget_id%[i, ]
            
                words_ui <- tagList(
                    words_ui,
                    div(
                      div(
                        shiny.fluent::IconButton.shinyInput(ns(paste0("remove_word_", row$id)), iconProps = list(iconName = "Cancel"), style = "height: 20px; margin: 0; font-size: 10px;"),
                        onclick = paste0(
                          "Shiny.setInputValue('", id, "-remove_word_trigger_%widget_id%', Math.random());",
                          "Shiny.setInputValue('", id, "-remove_word_%widget_id%', ", row$id, ");"
                        ),
                        class = "small_icon_button"
                      ),
                      create_hover_card(ui = div(row$text, style = selected_word_style), text = row$text),
                      style = "display: flex; margin: 2px 0;"
                    )
                )
            }
        }
        
        output$words_ui_%widget_id% <- renderUI(words_ui)
        
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Add a new words set

## Show add new words set div
observeEvent(input$new_words_set_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$new_words_set_%widget_id%"))
    
    shinyjs::hide("edit_words_sets_div_%widget_id%")
    shinyjs::show("new_words_set_div_%widget_id%")
})

## Cancel new words set
observeEvent(input$cancel_new_words_set_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$cancel_new_words_set_%widget_id%"))
    
    shinyjs::hide("new_words_set_div_%widget_id%")
    shinyjs::show("edit_words_sets_div_%widget_id%")
})

## Add button clicked
observeEvent(input$add_new_words_set_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$add_new_words_set_%widget_id%"))
    
    tryCatch({
        if (input$new_words_set_name_%widget_id% == ""){
            shiny.fluent::updateTextField.shinyInput(session, "new_words_set_name_%widget_id%", errorMessage = i18np$t("provide_valid_name"))
            stop()
        }
        shiny.fluent::updateTextField.shinyInput(session, "new_words_set_name_%widget_id%", errorMessage = NULL)
        new_name <- input$new_words_set_name_%widget_id%
        
        # Check if the name is already used
        sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id% AND name = 'word_set' AND value = {new_name}", .con = m$db)
        result <- DBI::dbGetQuery(m$db, sql)
        if (nrow(result) > 0){
            show_message_bar(output, "name_already_used", "warning", i18n = i18np, ns = ns)
            stop()
        }
            
        # Add new words set in database
        last_row <- get_last_row(m$db, "widgets_options")
        sql <- glue::glue_sql("SELECT COALESCE(MAX(value_num), 0) FROM widgets_options WHERE widget_id = %widget_id% AND name = 'word_set'", .con = m$db)
        last_id <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull()
        
        new_options <- tibble::tibble(
            id = last_row + 1, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
            category = NA_character_, name = "word_set", value = new_name, value_num = last_id + 1,
            creator_id = NA_integer_, datetime = now(), deleted = FALSE)
            
        DBI::dbAppendTable(m$db, "widgets_options", new_options)
        
        # Update dropdown of words sets
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_words_sets_%widget_id%', Math.random())"))
        
        # Reset textfield
        shiny.fluent::updateTextField.shinyInput(session, "new_words_set_name_%widget_id%", value = "")
        
        # Notify user
        show_message_bar(output, "new_words_set_added", "success", i18n = i18np, ns = ns)
        
        # Return to all words sets div
        shinyjs::hide("new_words_set_div_%widget_id%")
        shinyjs::show("edit_words_sets_div_%widget_id%")
        
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})
