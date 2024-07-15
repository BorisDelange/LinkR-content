# Init observers

shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_notes_%widget_id%', Math.random())"))
shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_notes_type_%widget_id%', 'all_notes')"))

shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_notes_datatable_%widget_id%', Math.random())"))

# Reload notes and datatable when selected patient changes

observeEvent(m$selected_person, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$selected_person"))
    
    tryCatch({
    
        # Reload notes
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_notes_%widget_id%', Math.random())"))
        
    }, error = function(e) cat(paste0("\\n", now(), " - ", toString(e))))
})

# Reload notes

observeEvent(input$reload_notes_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$reload_notes_%widget_id%"))
    
    tryCatch({
        
        m$notes_%widget_id% <- d$data_person$note %>% dplyr::collect() %>% dplyr::arrange(note_datetime)
         
        # Reload note UI
        output$notes_%widget_id% <- renderUI(div(i18np$t("select_a_note_to_display"), style = "margin-top: 10px;"))
         
        # Reload datatable
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_notes_datatable_%widget_id%', Math.random())"))
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# Show notes

observeEvent(input$show_notes_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$show_notes_%widget_id%"))
    
    tryCatch({
        
        notes <- m$show_notes_%widget_id%
        
        if (nrow(notes) > 0){
        
            # Apply layout filters
            
            ## Transform multiple \n in one \n
            if (length(input$remove_multiple_line_breaks_%widget_id%) > 0) if (input$remove_multiple_line_breaks_%widget_id%) notes <-
                notes %>% dplyr::mutate(note_text = gsub("\n([[:space:]]*\n)+", "\\n", note_text))
            
            output$notes_%widget_id% <- renderUI({
            
                note_panels <- lapply(1:nrow(notes), function(i) {
                    note <- notes[i,]
                    
                    div(
                        strong(note$note_title), " - ", format_datetime(note$note_datetime, m$language), br(), br(), 
                        tags$pre(note$note_text, style = "white-space: pre-wrap;"),
                        style = "border:dashed 1px; padding:10px; margin-top:10px; overflow: auto;"
                    )
                })
                
                do.call(div, note_panels)
            })
        }
        else output$notes_%widget_id% <- renderUI(div(i18np$t("no_notes_to_display"), style = "margin-top: 10px;"))
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})
