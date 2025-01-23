# Init vars
m$notes_%widget_id% <- tibble::tibble()
m$filters_%widget_id% <- tibble::tibble()

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

observeEvent(input$display_figure_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$display_figure_%widget_id%"))
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_notes_%widget_id%', Math.random())"))
    shinyjs::click("select_notes_%widget_id%")
})

observeEvent(input$reload_notes_%widget_id%, {
    %req%
    
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$reload_notes_%widget_id%"))
    
    tryCatch({
        
        if (d$data_person$note %>% dplyr::count() %>% dplyr::pull() == 0) m$notes_%widget_id% <- tibble::tibble()
        else {
            
            m$notes_%widget_id% <- d$data_person$note %>% dplyr::collect() %>% dplyr::arrange(note_datetime)
            
            # Apply filters
            if (nrow(m$filters_%widget_id%) > 0){
            
                # Words filters (search on note_text)
                words_sets_filters <- m$filters_%widget_id% %>% dplyr::filter(name == "words_set")
                
                if (nrow(words_sets_filters) > 0){
                
                    words <- m$words_%widget_id% %>% dplyr::filter(link_id %in% m$filters_%widget_id%$value_num) %>% dplyr::pull(value)
                    pattern <- stringr::str_c(words, collapse = "|")
                    
                    m$notes_%widget_id% <-
                        m$notes_%widget_id% %>%
                        dplyr::filter(stringr::str_detect(tolower(note_text), tolower(pattern))) %>%
                        dplyr::mutate(note_text = stringr::str_replace_all(tolower(note_text), tolower(pattern), "<span style='background-color: yellow;'>\\\\0</span>"))
                }
                
                # Title filters (search on note_title)
                title_filters <- m$filters_%widget_id% %>% dplyr::filter(name == "title")
                
                if (nrow(title_filters) > 0){
                
                    words <- title_filters %>% dplyr::pull(value)
                    pattern <- stringr::str_c(words, collapse = "|")
                    
                    m$notes_%widget_id% <-
                        m$notes_%widget_id% %>%
                        dplyr::filter(stringr::str_detect(tolower(note_title), tolower(pattern)))
                }
            }
            
            # Calculate rows
            m$notes_%widget_id% <-
                m$notes_%widget_id% %>%
                dplyr::mutate(
                    previous_note_id = dplyr::lag(note_id),
                    next_note_id = dplyr::lead(note_id),
                    n_rows = dplyr::n(),
                    row_num = dplyr::row_number()
                )
        }
         
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
                notes %>% dplyr::mutate(note_text = gsub("\\n([[:space:]]*\\n)+", "\\n", note_text))
            
            # Render notes
            
#             output$notes_%widget_id% <- renderUI({
#             
#                 note_panels <- lapply(1:nrow(notes), function(i) {
#                     note <- notes[i,]
#                     
#                     note_content <-
#                         div(
#                             strong(note$note_title), " - ", format_datetime(note$note_datetime, m$language), br(), br(), 
#                             tags$pre(HTML(note$note_text), style = "white-space: pre-wrap;"),
#                             style = "padding: 10px 5px; overflow: auto; height: 100%"
#                         )
#                      tags$iframe(
#                          srcdoc = as.character(note_content),
#                          style = "width: 100%; height: 100%; border: none;"
#                      )
#                 })
#                 
#                 do.call(div, note_panels)
#             })
            
            # Render notes navigation
            if (nrow(notes) == 1){
            
                output$notes_%widget_id% <- renderUI({
                    
#                     note_content <-
#                         div(
#                             strong(notes$note_title), " - ", format_datetime(notes$note_datetime, m$language), br(), br(), 
#                             tags$pre(HTML(notes$note_text), style = "white-space: pre-wrap;"),
#                             style = "padding: 10px 5px; overflow: auto; height: 100%"
#                         )
                    div(
                        br(),
                        strong(notes$note_title), " - ", format_datetime(notes$note_datetime, m$language), br(), br(), 
                        tags$iframe(
                            srcdoc = div(HTML(notes$note_text), style = "white-space: pre-wrap; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; font-size: 12px;"),
                            style = "width: 100%; height: 100%; border: none;"
                        ),
                        style = "height: 100%;"
                    )
                })
                
                previous_note_div <- div(style = "width: 20px;")
                next_note_div <- div(style = "width: 20px;")
                
                if (!is.na(notes$previous_note_id)) previous_note_div <- div(
                    shiny.fluent::IconButton.shinyInput(ns("previous_note_%widget_id%"), iconProps = list(iconName = "ChevronLeftMed")),
                    class = "small_icon_button", style = "width: 20px; height: 20px;"
                )
                if (!is.na(notes$next_note_id)) next_note_div <- div(
                    shiny.fluent::IconButton.shinyInput(ns("next_note_%widget_id%"), iconProps = list(iconName = "ChevronRightMed")),
                    class = "small_icon_button", style = "width: 20px; height: 20px;"
                )
                
                output$notes_nav_%widget_id% <- renderUI(div(
                    previous_note_div,
                    div("(", notes$row_num, " / ", notes$n_rows, ")", style = "margin-top: 1px;"),
                    next_note_div,
                    style = "display: flex; margin-top: 10px;"
                ))
            }
        }
        else {
            output$notes_%widget_id% <- renderUI(div(i18np$t("no_notes_to_display"), style = "margin-top: 10px;"))
            output$notes_nav_%widget_id% <- renderUI(div())
        }
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# Show previous note

observeEvent(input$previous_note_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$previous_note_%widget_id%"))
    
    tryCatch({
        
        # Get previous row_id
        previous_row_num <- m$show_notes_%widget_id%$row_num - 1
        
        # Select previous note
        m$show_notes_%widget_id% <- m$notes_%widget_id% %>% dplyr::filter(row_num == previous_row_num)
        
        # Reload notes UI
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_notes_%widget_id%', Math.random())"))
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# Show next note

observeEvent(input$next_note_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$next_note_%widget_id%"))
    
    tryCatch({
        
        # Get next row_id
        next_row_num <- m$show_notes_%widget_id%$row_num + 1
        
        # Select next note
        m$show_notes_%widget_id% <- m$notes_%widget_id% %>% dplyr::filter(row_num == next_row_num)
        
        # Reload notes UI
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_notes_%widget_id%', Math.random())"))
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})
