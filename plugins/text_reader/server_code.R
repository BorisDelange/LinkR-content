# Server - Code

# Init code var
m$code_%widget_id% <- ""

# Prevent a bug with scroll into ace editor
shinyjs::delay(300, shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);"))

# Run code when button is clicked
observeEvent(input$display_figure_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$display_figure"))
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Run code at patient update
observeEvent(m$selected_person, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$selected_person"))
    
    if (isTRUE(input$run_code_on_data_update_%widget_id%)) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Run code
observeEvent(input$run_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_code"))
    
    tryCatch({
        
        # Reset UI
        output$notes_%widget_id% <- renderUI(div())
        
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
            dplyr::mutate_at("note_datetime", format_datetime, language = language, sec = FALSE)
        
        if (nrow(m$notes_%widget_id%) > 0) notes <- m$notes_%widget_id% %>% dplyr::select(note_type_concept_name, note_title, note_datetime)
        
        # If DT proxy doesn't exist, create it
        # if (length(m$notes_datatable_proxy_%widget_id%) == 0){
        
        render_datatable(
          output = output, ns = ns, i18n = i18n, data = notes,
          output_name = "notes_datatable_%widget_id%", col_names = c(i18np$t("category"), i18np$t("title"), i18np$t("datetime")),
          datatable_dom = "<'datatable_length'l><'top't><'bottom'p>", sortable_cols = c("note_type_concept_name", "note_title", "note_datetime"),
          searchable_cols = c("note_type_concept_name", "note_title", "note_datetime"), factorize_cols = "note_type_concept_name", filter = TRUE
        )
            
            # Create datatable proxy only if notes is not empty (if we create DT with an empty tibble, it freezes search textfields)
            # if (nrow(notes) > 0) m$notes_datatable_proxy_%widget_id% <- DT::dataTableProxy("notes_datatable_%widget_id%", deferUntilFlush = FALSE)
        # }
        # else DT::replaceData(m$notes_datatable_proxy_%widget_id%, notes, resetPaging = FALSE, rownames = FALSE)
        
        # Go to figure tab
        if (!input$figure_and_settings_side_by_side_%widget_id%) shinyjs::click("figure_button_%widget_id%")
        
    }, error = function(e){
        show_message_bar(id, output, "error_displaying_figure", "severeWarning", i18n = i18np, ns = ns)
        cat(paste0("\\n", now(), " - widget %widget_id% - input$display_figure - error = ", toString(e)))
    })
})

# A row (a note) is selected

observeEvent(input$notes_datatable_%widget_id%_rows_selected, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$notes_datatable_%widget_id%_rows_selected"))
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_note_%widget_id%', Math.random());"))
})

# Display note

observeEvent(input$reload_note_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$reload_note_%widget_id%"))
    
    tryCatch({
        
        if (length(input$notes_datatable_%widget_id%_rows_selected) > 0){
            
            note <- m$notes_%widget_id%[input$notes_datatable_%widget_id%_rows_selected, ]
            
            display_raw_note <- FALSE
            if (length(input$display_raw_text_%widget_id%) > 0) if (input$display_raw_text_%widget_id%) display_raw_note <- TRUE
            
            if (display_raw_note){
                note_text_div <- tags$pre(note$note_text)
            } else {
                note_text_div <-
                    div(
                        tags$iframe(
                                srcdoc = div(HTML(note$note_text), style = "white-space: pre-wrap; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; font-size: 12px;"),
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
        
    }, error = function(e) cat(paste0("\\n", now(), " - ", toString(e))))
})
