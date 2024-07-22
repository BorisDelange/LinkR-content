# Change on filters category

observeEvent(input$filters_category_%widget_id%, {
    %req%
    req(length(input$filters_category_%widget_id%) > 0)
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$filters_category_%widget_id%"))
    
    filters_categories <- c("words_set", "title")
    categories_to_hide <- setdiff(filters_categories, input$filters_category_%widget_id%)
    
    sapply(paste0("filters_", categories_to_hide, "_div_%widget_id%"), shinyjs::hide)
    shinyjs::show(paste0("filters_", input$filters_category_%widget_id%, "_div_%widget_id%"))
})

# A words set is selected

observeEvent(input$filters_words_set_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$filters_words_set_%widget_id%"))
    
    # Show add button
    shinyjs::show("filters_add_words_set_div_%widget_id%")
})

# Add a words set

observeEvent(input$filters_add_words_set_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$filters_add_words_set_%widget_id%"))
    
    tryCatch({
        
        words_set <- m$words_sets_%widget_id% %>% dplyr::filter(id == input$filters_words_set_%widget_id%) %>% dplyr::transmute(id, category = "words_set", text)
        
        # Check if this word sets has already been added
        sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id% AND link_id = {input$saved_settings_%widget_id%} AND category = 'filter' AND name = 'words_set' AND value_num = {words_set$id}", .con = m$db)
        result <- DBI::dbGetQuery(m$db, sql)
        
        if (nrow(result) == 0){
        
            # Add new filter in m var and db
            
            new_options <- tibble::tibble(id =  get_last_row(m$db, "widgets_options") + 1, name = "words_set", value = words_set$text, value_num = words_set$id)
            m$filters_%widget_id% <- m$filters_%widget_id% %>% dplyr::bind_rows(new_options)
            
            new_options <- new_options %>% dplyr::transmute(
                id, widget_id = %widget_id%, person_id = NA_integer_, link_id = input$saved_settings_%widget_id%, category = "filter", name, value, value_num, 
                creator_id = NA_integer_, datetime = now(), deleted = FALSE
            )
            DBI::dbAppendTable(m$db, "widgets_options", new_options)
        }
        
    }, error = function(e) cat(paste0("\\n", now(), " - ", toString(e))))
})

# Add a title

observeEvent(input$filters_add_title_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$filters_add_title_%widget_id%"))
    
    tryCatch({
        
        new_title <- input$filters_title_%widget_id%
        
        # A title has to contain at least 2 chars
        if (new_title == "" | nchar(new_title) < 2){
            show_message_bar(output, "word_may_contain_at_least_two_chars", "warning", i18n = i18np, ns = ns)
            stop()
        }
        
        # Check if this title has already been added
        sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id% AND link_id = {input$saved_settings_%widget_id%} AND category = 'filter' AND name = 'title' AND value = {new_title}", .con = m$db)
        result <- DBI::dbGetQuery(m$db, sql)
        
        if (nrow(result) == 0){
        
            # Add new filter in m var and db
            
            new_options <- tibble::tibble(id =  get_last_row(m$db, "widgets_options") + 1, name = "title", value = new_title, value_num = NA_integer_)
            m$filters_%widget_id% <- m$filters_%widget_id% %>% dplyr::bind_rows(new_options)
            
            new_options <- new_options %>% dplyr::transmute(
                id, widget_id = %widget_id%, person_id = NA_integer_, link_id = input$saved_settings_%widget_id%, category = "filter", name, value, value_num, 
                creator_id = NA_integer_, datetime = now(), deleted = FALSE
            )
            DBI::dbAppendTable(m$db, "widgets_options", new_options)
        }
        
        # Reset textfield
        shiny.fluent::updateTextField.shinyInput(session, "filters_title_%widget_id%", value = "", errorMessage = NULL)
        
        # Reload wor
        
    }, error = function(e) cat(paste0("\\n", now(), " - ", toString(e))))
})

# Reload filters UI

observeEvent(m$filters_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$filters_%widget_id% "))
    
    tryCatch({
        
        filters_ui <- tagList()
        
        if (nrow(m$filters_%widget_id%) > 0){
            for (i in 1:nrow(m$filters_%widget_id%)){
                row <- m$filters_%widget_id%[i, ]
                
                if (row$name == "words_set") style <- selected_words_set_filter_style
                else if (row$name == "title") style <- selected_title_filter_style
            
                filters_ui <- tagList(
                    filters_ui,
                    div(
                      div(
                        shiny.fluent::IconButton.shinyInput(ns(paste0("remove_filter_", row$id)), iconProps = list(iconName = "Cancel"), style = "height: 20px; margin: 0; font-size: 10px;"),
                        onclick = paste0(
                          "Shiny.setInputValue('", id, "-remove_filter_trigger_%widget_id%', Math.random());",
                          "Shiny.setInputValue('", id, "-remove_filter_%widget_id%', ", row$id, ");"
                        ),
                        class = "small_icon_button"
                      ),
                      create_hover_card(ui = div(row$value, style = style), text = row$value),
                      style = "display: flex; margin: 2px 10px 2px 0;"
                    )
                )
            }
        }
        
        output$filters_ui_%widget_id% <- renderUI(filters_ui)
        
    }, error = function(e) cat(paste0("\\n", now(), " - ", toString(e))))
})

# Remove a filter

observeEvent(input$remove_filter_trigger_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$remove_filter_trigger_%widget_id%"))
    
    tryCatch({
        
        filter_id <- input$remove_filter_%widget_id%
        
        # Delete row in db
        sql <- glue::glue_sql("DELETE FROM widgets_options WHERE id = {filter_id}", .con = m$db)
        sql_send_statement(m$db, sql)
        
        # Update m var
        m$filters_%widget_id% <- m$filters_%widget_id% %>% dplyr::filter(id != filter_id)
        
    }, error = function(e) cat(paste0("\\n", now(), " - ", toString(e))))
})
