observeEvent(m$selected_person, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer m$selected_person"))
    m$reload_notes_%widget_id% <- Sys.time()
})

observeEvent(input$chronological_order_%widget_id%, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer input$chronological_order_%widget_id%"))
    m$reload_notes_%widget_id% <- Sys.time()
})

observeEvent(m$reload_notes_%widget_id%, {
    %req%
    if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - observer m$reload_notes_%widget_id%"))
    
    req(d$data_person$note %>% dplyr::count() %>% dplyr::pull() > 0)
    
    # Get data
    selected_person <- m$selected_person
    notes <- d$data_person$note %>% dplyr::filter(person_id == selected_person) %>% dplyr::collect()
    
    chronological_order <- FALSE
    if (length(input$chronological_order_%widget_id%) > 0) if (input$chronological_order_%widget_id%) chronological_order <- TRUE
    if (chronological_order) notes <- notes %>% dplyr::arrange(note_datetime)
    else notes <- notes %>% dplyr::arrange(dplyr::desc(note_datetime))
    
    req(nrow(notes) > 0)
    
    output$notes_%widget_id% <- renderUI({
        %req%
        if (debug) print(paste0(Sys.time(), " - mod_", id, " - widget_id = %widget_id% - output$notes_%widget_id%"))
        
        note_panels <- lapply(1:nrow(notes), function(i) {
            note <- notes[i,]
            div(strong(note$note_title), " - ", format_datetime(note$note_datetime, m$language), br(), br(), 
                HTML(stringr::str_replace_all(note$note_text, "\n", "<br />")),
                style = "border:dashed 1px; padding:10px; margin-top:10px;")
        })
        
        do.call(div, note_panels)
    })
})
