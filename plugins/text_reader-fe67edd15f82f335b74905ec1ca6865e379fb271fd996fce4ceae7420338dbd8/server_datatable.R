# Reload datatable of all notes

observeEvent(input$reload_notes_datatable_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$reload_notes_datatable_%widget_id%"))
    
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
            
            # Create datatable proxy only if notes is not empty (if we create DT with an empty tibble, it freezes search textfields)
            if (nrow(notes) > 0) m$notes_datatable_proxy_%widget_id% <- DT::dataTableProxy("notes_datatable_%widget_id%", deferUntilFlush = FALSE)
        }
        else {
            DT::replaceData(m$notes_datatable_proxy_%widget_id%, notes, resetPaging = FALSE, rownames = FALSE)
        }
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# A row is selected

observeEvent(input$notes_datatable_%widget_id%_rows_selected, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$notes_datatable_%widget_id%_rows_selected"))
    
    tryCatch({
        
        selected_notes <- m$notes_%widget_id%[input$notes_datatable_%widget_id%_rows_selected, ]
        m$show_notes_%widget_id% <- m$notes_%widget_id% %>% dplyr::filter(note_id %in% selected_notes$note_id)
        
        # Update ace editor
        #code <- paste0("m$notes_%widget_id% %>%\n    dplyr::filter(note_id %in% ", selected_notes$note_id, ")")
        #shinyAce::updateAceEditor(session, "code_editor_%widget_id%", value = code)
        
        # Show selected notes
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_notes_%widget_id%', Math.random())"))
        
    }, error = function(e) cat(paste0("\\n", now(), " - ", toString(e))))
})
