# Load words

sql <- glue::glue_sql("SELECT id, link_id, value FROM widgets_options WHERE widget_id = %widget_id% AND category = 'word'", .con = m$db)
m$words_%widget_id% <- DBI::dbGetQuery(m$db, sql)

# Add a new word

observeEvent(input$add_new_word_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$add_new_word_%widget_id%"))
    
    tryCatch({
        
        new_name <- input$new_word_%widget_id%
        
        # A word has to contain at least 2 chars
        if (new_name == "" | nchar(new_name) < 2){
            show_message_bar(output, "word_may_contain_at_least_two_chars", "warning", i18n = i18np, ns = ns)
            stop()
        }
        
        # Check if this word already exists
        words_set_id <- input$words_set_%widget_id%
        sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id% AND link_id = {words_set_id} AND category = 'word' AND value = {new_name}", .con = m$db)
        
        result <- DBI::dbGetQuery(m$db, sql)
        if (nrow(result) > 0){
            shiny.fluent::updateTextField.shinyInput(session, "new_word_%widget_id%", errorMessage = i18np$t("name_already_used"))
            stop()
        }
        
        # Add new word to words list and in db
        new_options <- tibble::tibble(id = get_last_row(m$db, "widgets_options") + 1, link_id = words_set_id, value = new_name)
        m$words_%widget_id% <- m$words_%widget_id% %>% dplyr::bind_rows(new_options)
        
        new_options <- new_options %>% dplyr::transmute(
            id, widget_id = %widget_id%, person_id = NA_integer_, link_id, category = "word", name = NA_character_, value, value_num = NA_integer_,
            creator_id = NA_integer_, datetime = now(), deleted = FALSE
        )
        DBI::dbAppendTable(m$db, "widgets_options", new_options)
        
        # Reset textfield
        shiny.fluent::updateTextField.shinyInput(session, "new_word_%widget_id%", value = "", errorMessage = NULL)
        
        # Reload words list
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_words_list_%widget_id%', Math.random())"))
        
    }, error = function(e) cat(paste0("\\n", now(), " - ", toString(e))))
})

# Delete a word

observeEvent(input$remove_word_trigger_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$remove_word_trigger_%widget_id%"))
    
    tryCatch({
        
        word_id <- input$remove_word_%widget_id%
        
        # Delete row in db
        sql <- glue::glue_sql("DELETE FROM widgets_options WHERE id = {word_id}", .con = m$db)
        sql_send_statement(m$db, sql)
        
        # Update m var
        m$words_%widget_id% <- m$words_%widget_id% %>% dplyr::filter(id != word_id)
        
        # Reload words list
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_words_list_%widget_id%', Math.random())"))
        
    }, error = function(e) cat(paste0("\\n", now(), " - ", toString(e))))
})

# Reload words UI

observeEvent(input$reload_words_list_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$reload_words_list_%widget_id% "))
    
    tryCatch({
        
        words_ui <- tagList()
        words_list <- m$words_%widget_id% %>% dplyr::filter(link_id == input$words_set_%widget_id%)
        
        if (nrow(words_list) > 0){
            for (i in 1:nrow(words_list)){
                row <- words_list[i, ]
            
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
                      create_hover_card(ui = div(row$value, style = selected_word_style), text = row$value),
                      style = "display: flex; margin: 2px 10px 2px 0;"
                    )
                )
            }
        }
        
        output$words_ui_%widget_id% <- renderUI(words_ui)
        
    }, error = function(e) cat(paste0("\\n", now(), " - ", toString(e))))
})
