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
                dplyr::mutate_at("note_datetime", as.character)
        }
        else {
            notes <- tibble::tibble(note_type_concept_name = character(), note_title = character(), note_datetime = character())
        }
        
        render_datatable(
          output = output, ns = ns, i18n = i18n, data = notes,
          output_name = "notes_datatable_%widget_id%", col_names = c(i18np$t("category"), i18np$t("title"), i18np$t("datetime")),
          datatable_dom = "<'datatable_length'l><'top't><'bottom'p>", sortable_cols = c("note_type_concept_name", "note_title", "note_datetime"),
          searchable_cols = c("note_type_concept_name", "note_title", "note_datetime"), filter = TRUE
        )
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

# Add a new words set

# Show add new words set div
observeEvent(input$new_words_set_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$new_words_set_%widget_id%"))
    
    shinyjs::hide("edit_words_sets_div_%widget_id%")
    shinyjs::show("new_words_set_div_%widget_id%")
})

# Cancel new words set
observeEvent(input$cancel_new_words_set_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$cancel_new_words_set_%widget_id%"))
    
    shinyjs::hide("new_words_set_div_%widget_id%")
    shinyjs::show("edit_words_sets_div_%widget_id%")
})

# Add button clicked
observeEvent(input$add_new_words_set_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$add_new_words_set_%widget_id%"))
    
    tryCatch({
        if (input$new_words_set_name_%widget_id% == "") shiny.fluent::updateTextField.shinyInput(session, "new_words_set_name_%widget_id%", errorMessage = i18np$t("provide_valid_name"))
        else {
            shiny.fluent::updateTextField.shinyInput(session, "new_words_set_name_%widget_id%", errorMessage = NULL)
            
            # Check if the name is already used
            
            # Add new words set in database
            
            # Update dropdown of words sets
            
            # Notify user
            show_message_bar(output, "new_words_set_added", "success", i18n = i18np, ns = ns)
            
            # Return to all words sets div
            shinyjs::hide("new_words_set_div_%widget_id%")
            shinyjs::show("edit_words_sets_div_%widget_id%")
        }
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})
