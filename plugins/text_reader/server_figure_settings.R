# Server - Figure settings

library(promises)

# Load figure settings

observe_event(input$load_figure_settings_%widget_id%, {
    
    # Update figure settings UI
    
    link_id <- input$settings_file_%widget_id%
    sql <- glue::glue_sql("SELECT name, value, value_num FROM widgets_options WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", .con = m$db)
    figure_settings <- DBI::dbGetQuery(m$db, sql)
    
    if (nrow(figure_settings) > 0){
        sapply(figure_settings$name, function(name){
        
            value <- figure_settings %>% dplyr::filter(name == !!name) %>% dplyr::pull(value)
            value_num <- figure_settings %>% dplyr::filter(name == !!name) %>% dplyr::pull(value_num)
            
            if (name %in% c("llm_provider", "llm_model", "include_notes")) shiny.fluent::updateDropdown.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            else if (name == "search_word_sets"){
                value <- as.numeric(unlist(strsplit(value, ", ")))
                shiny.fluent::updateDropdown.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            }
            else if (name %in% c("filter_notes_with_matches", "display_raw_text")){
                value <- as.logical(value_num)
                shiny.fluent::updateToggle.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            }
            else if (name == "datatable_page_length") m$datatable_page_length_%widget_id% <- value_num
        })
    }
    
    # Run code if toggle is activated
    if (length(input$run_code_at_settings_file_load_%widget_id%) > 0){
        if (input$run_code_at_settings_file_load_%widget_id%){
            shinyjs::delay(500, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());")))
        }
    }
})

# Save current settings

observe_event(input$save_params_and_code_%widget_id%, {
    
    # If no settings file is selected, go to settings files management page
    if (length(input$settings_file_%widget_id%) == 0) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_settings_files_tab_%widget_id%', Math.random());"))
    
    if (length(input$settings_file_%widget_id%) > 0){
        
        link_id <- input$settings_file_%widget_id%
    
        # Delete old settings
        sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", .con = m$db))
        
        # Add new settings in db
        
        length_value <- if (!is.null(input[[paste0("notes_datatable_", widget_id, "_state")]]$length)) input[[paste0("notes_datatable_", widget_id, "_state")]]$length else NA_integer_
        
        new_data <- tibble::tribble(
            ~name, ~value, ~value_num,
            "search_word_sets", ifelse(length(input$search_word_sets_%widget_id%) > 0, input$search_word_sets_%widget_id% %>% toString(), NA_character_), NA_real_,
            "filter_notes_with_matches", NA_character_, as.integer(input$filter_notes_with_matches_%widget_id%),
            "llm_provider", ifelse(length(input$llm_provider_%widget_id%) > 0, input$llm_provider_%widget_id%, NA_character_), NA_real_,
            "llm_model", ifelse(length(input$llm_model_%widget_id%) > 0, input$llm_model_%widget_id%, NA_character_), NA_real_,
            "include_notes", input$include_notes_%widget_id%, NA_real_,
            "display_raw_text", NA_character_, as.integer(input$display_raw_text_%widget_id%),
            "datatable_page_length", NA_character_, length_value
        )
        
        new_data <-
            new_data %>%
            dplyr::transmute(
                id = get_last_row(m$db, "widgets_options") + 1:nrow(new_data), widget_id = %widget_id%, person_id = NA_integer_, link_id = link_id,
                category = "figure_settings", name, value, value_num, creator_id = m$user_id, datetime = now(), deleted = FALSE
            )
        DBI::dbAppendTable(m$db, "widgets_options", new_data)
        
        # Notify user
        show_message_bar("modif_saved", "success")
    }
        
})

# Figure settings tabs

sub_tabs <- c("select_notes", "keyword_search", "layout", "chatbot")

observe_event(input$current_figure_settings_tab_trigger_%widget_id%, {
    
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
})

# Display raw text

observe_event(input$display_raw_text_%widget_id%, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_note_%widget_id%', Math.random());")))

# Keyword search

## Create a word set

observe_event(input$create_word_set_%widget_id%, {
    
    word_set_name <- input$word_set_name_%widget_id%

    empty_name <- TRUE
    if (length(word_set_name) > 0) if (!is.na(word_set_name) && word_set_name != "") empty_name <- FALSE
    
    if (empty_name){
        shiny.fluent::updateTextField.shinyInput(session, "word_set_name_%widget_id%", errorMessage = i18np$t("provide_valid_name"))
        return()
    }
    
    shiny.fluent::updateTextField.shinyInput(session, "word_set_name_%widget_id%", errorMessage = NULL)
    
    sql <- glue::glue_sql("SELECT value FROM widgets_options WHERE widget_id = %widget_id% AND category = 'word_sets' AND name = 'word_set_name' AND LOWER(value) = {tolower(word_set_name)}", .con = m$db)
    name_already_used <- nrow(DBI::dbGetQuery(m$db, sql)) > 0
    
    if (name_already_used){
        shiny.fluent::updateTextField.shinyInput(session, "word_set_name_%widget_id%", errorMessage = i18np$t("name_already_used"))
        return()
    }
        
    shiny.fluent::updateTextField.shinyInput(session, "word_set_name_%widget_id%", errorMessage = NULL)
    
    # Add new word set in app db
    
    new_id <- get_last_row(m$db, "widgets_options") + 1
    
    new_data <- tibble::tibble(
        id = new_id, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
        category = "word_sets", name = "word_set_name", value = word_set_name, value_num = NA_real_, creator_id = m$user_id, datetime = now(), deleted = FALSE
    )
    DBI::dbAppendTable(m$db, "widgets_options", new_data)
    
    # Reset field & update dropdowns
    
    shiny.fluent::updateTextField.shinyInput(session, "word_set_name_%widget_id%", value = "")
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_word_sets_dropdowns_%widget_id%', Math.random());"))
})

## Update word set dropdowns

observe_event(input$update_word_sets_dropdowns_%widget_id%, {

    sql <- glue::glue_sql("SELECT id, value FROM widgets_options WHERE widget_id = %widget_id% AND category = 'word_sets' AND name = 'word_set_name'", .con = m$db)
    word_sets <- DBI::dbGetQuery(m$db, sql) %>% convert_tibble_to_list(key_col = "id", text_col = "value")
    shiny.fluent::updateDropdown.shinyInput(session, "search_word_sets_%widget_id%", options = word_sets, value = input$search_word_sets_%widget_id%)
    shiny.fluent::updateDropdown.shinyInput(session, "edit_word_set_%widget_id%", options = word_sets, value = NULL)
})

## Edit a word set

observe_event(input$edit_word_set_%widget_id%, {
    
    sapply(c("edit_word_set_details_div_%widget_id%", "delete_word_set_div_%widget_id%"), shinyjs::show)
    
    # Load words of this word set
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_words_list_%widget_id%', Math.random());"))
})

## Open delete a word set modal
observe_event(input$delete_word_set_%widget_id%, {
    
    if (length(input$edit_word_set_%widget_id%) == 0) return()
    shinyjs::show("delete_word_set_modal_%widget_id%")
})

## Close delete a word set modal
observe_event(input$close_word_set_deletion_modal_%widget_id%, shinyjs::hide("delete_word_set_modal_%widget_id%"))

## Confirm word set deletion
observe_event(input$confirm_word_set_deletion_%widget_id%, {

    word_set_id <- input$edit_word_set_%widget_id%
    
    # Delete row in db
    sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE id = {word_set_id}", .con = m$db))
    
    # Update dropdown
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_word_sets_dropdowns_%widget_id%', Math.random());"))
    
    # Update word set details
    shinyjs::hide("edit_word_set_details_div_%widget_id%")
    
    # Close modal
    shinyjs::hide("delete_word_set_modal_%widget_id%")
    
    # Hide delete button
    shinyjs::hide("delete_word_set_div_%widget_id%")
    
    # Notify user
    show_message_bar(output, "word_set_deleted", "warning")
})

## Update words list

observe_event(input$update_words_list_%widget_id%, {
    
    word_set_id <- input$edit_word_set_%widget_id%
    sql <- glue::glue_sql("SELECT id, value FROM widgets_options WHERE widget_id = %widget_id% AND category = 'word_sets' AND link_id = {word_set_id} AND name = 'word_name'", .con = m$db)
    words_list <- DBI::dbGetQuery(m$db, sql)
    
    words_list_ui <- tagList()
    
    if (nrow(words_list) > 0){
    
        for (i in 1:nrow(words_list)){
        
            row <- words_list[i, ]
            
            words_list_ui <- tagList(
                words_list_ui,
                div(
                    div(
                        shiny.fluent::IconButton.shinyInput(ns(paste0("remove_row_", row$id, "_%widget_id%")), iconProps = list(iconName = "Cancel"), style = "height: 20px; margin: 0; font-size: 10px;"),
                        onclick = paste0(
                            "Shiny.setInputValue('", id, "-remove_word_trigger_%widget_id%', Math.random());",
                            "Shiny.setInputValue('", id, "-remove_word_%widget_id%', ", row$id, ");"
                        ),
                        class = "small_icon_button", style = "width: 18px;"
                    ),
                    create_hover_card(ui = 
                        div(
                            row$value,
                            style = paste0(
                                "display: inline-block; color: white; max-width: 320px; border-radius: 8px; padding: 1px 5px; align-items: center; height: 18px;",
                                "font-weight: 600; white-space: nowrap; overflow: hidden; background-color: #FF8C00;"
                            )
                        ), 
                        text = row$value
                    ),
                    style = "display: flex; margin-right: 5px;"
                )
            )
        }
    }
    
    output$words_list_%widget_id% <- renderUI(words_list_ui)
})

## Add a new word

observe_event(input$add_new_word_%widget_id%, {
    
    word_set_id <- input$edit_word_set_%widget_id%
    
    if (length(word_set_id) > 0){
    
        word_name <- input$word_name_%widget_id%
        
        empty_name <- TRUE
        if (length(word_name) > 0) if (!is.na(word_name) && word_name != "") empty_name <- FALSE
        
        if (empty_name) shiny.fluent::updateTextField.shinyInput(session, "word_name_%widget_id%", errorMessage = i18np$t("provide_valid_name"))
        else {
            shiny.fluent::updateTextField.shinyInput(session, "word_name_%widget_id%", errorMessage = NULL)
            
            sql <- glue::glue_sql(paste0(
                "SELECT value FROM widgets_options WHERE widget_id = %widget_id% AND category = 'word_sets' AND link_id = {word_set_id} ",
                "AND name = 'word_name' AND LOWER(value) = {tolower(word_name)}"), .con = m$db)
            name_already_used <- nrow(DBI::dbGetQuery(m$db, sql)) > 0
            
            if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "word_name_%widget_id%", errorMessage = i18np$t("name_already_used"))
            else {
                shiny.fluent::updateTextField.shinyInput(session, "word_name_%widget_id%", errorMessage = NULL)
                
                # Add new word in app db
                
                new_id <- get_last_row(m$db, "widgets_options") + 1
                
                new_data <- tibble::tibble(
                    id = new_id, widget_id = %widget_id%, person_id = NA_integer_, link_id = word_set_id,
                    category = "word_sets", name = "word_name", value = word_name, value_num = NA_real_, creator_id = m$user_id, datetime = now(), deleted = FALSE
                )
                DBI::dbAppendTable(m$db, "widgets_options", new_data)
                
                # Reset field & update dropdowns
                
                shiny.fluent::updateTextField.shinyInput(session, "word_name_%widget_id%", value = "")
                shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_words_list_%widget_id%', Math.random());"))
            }
        }
    }
})

## Remove a word

observe_event(input$remove_word_trigger_%widget_id%, {
        
    word_id <- input$remove_word_%widget_id%
    
    if (length(word_id) > 0){
    
        # Remove word from database
        sql <- glue::glue_sql("DELETE FROM widgets_options WHERE id = {word_id}", .con = m$db)
        DBI::dbExecute(m$db, sql)
    
        # Update words list
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_words_list_%widget_id%', Math.random());"))
    }
})

# Chatbot

observe_event(input$llm_provider_%widget_id%, {
    
    if (input$llm_provider_%widget_id% == "ollama"){
        if (ollamar::test_connection(logical = TRUE)){
            models <- ollamar::list_models() %>% convert_tibble_to_list(key_col = "name", text_col = "name")
            shiny.fluent::updateDropdown.shinyInput(session, "llm_model_%widget_id%", options = models)
        }
    }
})

m$chatbot_response_%widget_id% <- reactiveVal("")  # Initialize with empty string
# m$debounced_chatbot_response_%widget_id% <- reactive(m$chatbot_response_%widget_id%()) %>% debounce(1000)

observe_event(input$send_message_%widget_id%, {
    
    if(requireNamespace("ellmer", quietly = TRUE) && requireNamespace("ollamar", quietly = TRUE)){
            
        if (length(m$chatbot_messages_%widget_id%) == 0) m$chatbot_messages_%widget_id% <- tagList()
        
        if (length(input$llm_provider_%widget_id%) == 0) output$chat_ui_%widget_id% <- renderUI(
            div(
                shiny.fluent::MessageBar(i18np$t("select_llm_provider"), messageBarType = 5),
                style = "display: inline-block; margin-top: 10px;"
            )
        )
        else if (length(input$llm_model_%widget_id%) == 0) output$chat_ui_%widget_id% <- renderUI(
            div(
                shiny.fluent::MessageBar(i18np$t("select_llm_model"), messageBarType = 5),
                style = "display: inline-block; margin-top: 10px;"
            )
        )
        
        if (length(input$llm_provider_%widget_id%) > 0 && length(input$llm_model_%widget_id%) > 0){
        
            if (length(m$chatbot_%widget_id%) == 0) m$chatbot_%widget_id% <- ellmer::chat_ollama(model = input$llm_model_%widget_id%)
            
            user_message <- input$user_input_%widget_id%
            
            # Create the user message bubble
            message_bubble <- div(
                class = "user-message-bubble",
                style = "
                    background-color: #0B93F6; color: white; border-radius: 20px; 
                    padding: 10px 15px; margin: 5px 0; display: inline-block; 
                    max-width: 60%; float: right; clear: both; word-wrap: break-word;
                    font-family: 'Helvetica Neue', Helvetica, sans-serif;",
                user_message
            )
            
            # Add the user message
            m$chatbot_messages_%widget_id% <- tagAppendChild(
                m$chatbot_messages_%widget_id%,
                div(style = "overflow: hidden; margin-bottom: 10px;", message_bubble)
            )
            
            # Create an empty bubble for the chatbot response (as placeholder)
            chatbot_placeholder_bubble <- div(
                class = "chatbot-message-bubble",
                style = "
                    background-color: #E5E5EA; color: #000000; border-radius: 20px; 
                    padding: 10px 15px; margin: 5px 0; display: block; 
                    width: 100%; clear: both; word-wrap: break-word;
                    font-family: 'Helvetica Neue', Helvetica, sans-serif;",
                "..." # Placeholder to indicate that the response is in progress
            )
            
            # Add the placeholder for the response (we will update it later)
            m$chatbot_messages_%widget_id% <- tagAppendChild(
                m$chatbot_messages_%widget_id%,
                div(style = "overflow: hidden; margin-bottom: 10px;", chatbot_placeholder_bubble)
            )
            
            # Reset the reactive value for a new response
            m$chatbot_response_%widget_id%("")
            
            # Update the UI immediately with the user message and placeholder
            output$chat_ui_%widget_id% <- renderUI(
                div(
                    class = "chat-container",
                    style = "display: flex; flex-direction: column; height: 100%; overflow-y: auto;",
                    m$chatbot_messages_%widget_id%
                )
            )
            
           # Add patient notes to the prompt if requested
           
           combined_prompt <- user_message
           
            if (input$include_notes_%widget_id% %in% c("selected_note", "all_notes")) {
            
                if (length(m$notes_%widget_id%) > 0){
                    # Prepare the notes to be added to the prompt
                    notes_prompt <- ""
                    
                    if (input$include_notes_%widget_id% == "selected_note") {
                        # Get the selected note from the datatable
                        selected_row <- input$notes_datatable_%widget_id%_rows_selected
                        if (length(selected_row) > 0) {
                            selected_note <- m$notes_%widget_id%[selected_row, ]
                            
                            # Format the selected note
                            notes_prompt <- paste0(
                                "Patient Note:\\n",
                                "Date: ", selected_note$note_datetime, "\\n",
                                "Type: ", selected_note$note_type_concept_name, "\\n",
                                "Title: ", selected_note$note_title, "\\n",
                                "Content: ", selected_note$note_text, "\\n\\n"
                            )
                        }
                    } else if (input$include_notes_%widget_id% == "all_notes") {
                        # Format all notes
                        notes_prompt <- "Patient Notes:\n\n"
                        
                        for (i in 1:nrow(m$notes_%widget_id%)) {
                            note <- m$notes_%widget_id%[i, ]
                            notes_prompt <- paste0(
                                notes_prompt,
                                "Note ", i, ":\\n",
                                "Date: ", note$note_datetime, "\\n",
                                "Type: ", note$note_type_concept_name, "\\n",
                                "Title: ", note$note_title, "\\n",
                                "Content: ", note$note_text, "\\n\\n"
                            )
                        }
                    }
                    
                    # Combine the notes and the user message
                    combined_prompt <- paste0(notes_prompt, "User Query: ", user_message)
                }
            }
            
            stream <- m$chatbot_%widget_id%$stream_async(combined_prompt)
            
            # Process the stream
            res <- coro::async(function() {
              for (chunk in await_each(stream)) {
                m$chatbot_response_%widget_id%(paste0(m$chatbot_response_%widget_id%(), chunk))
              }
            })()
            
            updateTextInput(session, paste0("user_input_%widget_id%"), value = "")
        }
    }
})

observe_event(m$chatbot_response_%widget_id%(), {

    # Get the current response text
    current_text <- m$chatbot_response_%widget_id%()
    
    # Only proceed if we have text
    if (!is.null(current_text) && nchar(current_text) > 0) {
        # Create the chatbot message bubble with current text
        chatbot_message_bubble <- div(
            class = "chatbot-message-bubble",
            style = "
                background-color: #E5E5EA; color: #000000; border-radius: 20px; 
                padding: 10px 15px; margin: 5px 0; display: block; 
                clear: both; word-wrap: break-word;
                font-family: 'Helvetica Neue', Helvetica, sans-serif;",
            tags$pre(
                style = "
                    margin: 0; 
                    white-space: pre-wrap; 
                    word-wrap: break-word;
                    background: none;
                    border: none;
                    padding: 0;
                    font-family: inherit;
                    font-size: inherit;
                    color: inherit;",
                current_text
            )
        )
        
        last_idx <- length(m$chatbot_messages_%widget_id%[[1]])
        m$chatbot_messages_%widget_id%[[1]][[last_idx]] <- chatbot_message_bubble
        
        # Update the UI with the latest messages
        output$chat_ui_%widget_id% <- renderUI(
            div(
                class = "chat-container",
                style = "display: flex; flex-direction: column; height: 100%; overflow-y: auto;",
                m$chatbot_messages_%widget_id%
            )
        )
    }
    
    # Intelligent scrolling - only auto-scroll if the user is already at the bottom
    shinyjs::runjs(paste0('
        (function() {
          var chatId = "', ns("chat_ui_%widget_id%"), '";
          var chatContainer = document.getElementById(chatId).parentNode;
          
          if (typeof window.chatManagers === "undefined") {
            window.chatManagers = {};
          }
          
          if (typeof window.chatManagers[chatId] === "undefined") {
            window.chatManagers[chatId] = {
              userScrolled: false,
              lastScrollTime: 0
            };
            
            chatContainer.addEventListener("scroll", function() {
              window.chatManagers[chatId].userScrolled = true;
            });
            
            chatContainer.scrollTop = chatContainer.scrollHeight;
          }
          
          var manager = window.chatManagers[chatId];
          var now = Date.now();
          var currentlyNearBottom = (chatContainer.scrollHeight - chatContainer.scrollTop - chatContainer.clientHeight) < 100;
          
          if (now - manager.lastScrollTime > 700) {
            if (!manager.userScrolled || currentlyNearBottom) {
              chatContainer.scrollTop = chatContainer.scrollHeight;
            }
            manager.lastScrollTime = now;
          }
        })();
    '))
})

## Clear chat

observe_event(input$clear_chat_%widget_id%, {
    
    if(requireNamespace("ellmer", quietly = TRUE) && requireNamespace("ollamar", quietly = TRUE)){
        if (length(input$llm_provider_%widget_id%) > 0 && length(input$llm_model_%widget_id%) > 0){
            m$chatbot_%widget_id% <- ellmer::chat_ollama(model = input$llm_model_%widget_id%)
            m$chatbot_messages_%widget_id% <- tagList()
            output$chat_ui_%widget_id% <- renderUI(div())
        }
    }
})
