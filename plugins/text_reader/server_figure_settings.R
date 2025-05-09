# Server - Figure settings

library(promises)

# Load figure settings

observeEvent(input$load_figure_settings_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$load_figure_settings"))
    
    # Update figure settings UI
    
    link_id <- input$settings_file_%widget_id%
    sql <- glue::glue_sql("SELECT name, value, value_num FROM widgets_options WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", .con = m$db)
    figure_settings <- DBI::dbGetQuery(m$db, sql)
    
    if (nrow(figure_settings) > 0){
        sapply(figure_settings$name, function(name){
        
            value <- figure_settings %>% dplyr::filter(name == !!name) %>% dplyr::pull(value)
            value_num <- figure_settings %>% dplyr::filter(name == !!name) %>% dplyr::pull(value_num)
            
            # Update figure settings UI here with loaded figure settings
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

observeEvent(input$save_params_and_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$save_params_and_code"))
    
    tryCatch({
    
        # If no settings file is selected, go to settings files management page
        if (length(input$settings_file_%widget_id%) == 0) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_settings_files_tab_%widget_id%', Math.random());"))
        
        if (length(input$settings_file_%widget_id%) > 0){
            
            link_id <- input$settings_file_%widget_id%
        
            # Delete old settings
            sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", .con = m$db))
            
            # Add new settings in db
            
            # new_data <- tibble::tribble(
            #     ~name, ~value, ~value_num,
            #     ...
            # )
            
            # new_data <-
            #     new_data %>%
            #     dplyr::transmute(
            #         id = get_last_row(m$db, "widgets_options") + 1:nrow(new_data), widget_id = %widget_id%, person_id = NA_integer_, link_id = link_id,
            #         category = "figure_settings", name, value, value_num, creator_id = m$user_id, datetime = now(), deleted = FALSE
            #     )
            
            # DBI::dbAppendTable(m$db, "widgets_options", new_data)
            
            # Notify user
            show_message_bar(id, output, "modif_saved", "success", i18n = i18n, ns = ns)
        }
        
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# Figure settings tabs

sub_tabs <- c("select_notes", "filters", "layout", "chatbot")

observeEvent(input$current_figure_settings_tab_trigger_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$current_figure_settings_tab_trigger_%widget_id%"))
    
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
    }, error = function(e) cat(paste0("\\n", now(), " - widget %widget_id% - error = ", toString(e))))
})

# Display raw text

observeEvent(input$display_raw_text_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$display_raw_text_%widget_id%"))
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_note_%widget_id%', Math.random());"))
})

# Chatbot

m$chatbot_response_%widget_id% <- reactiveVal("")  # Initialize with empty string
# m$debounced_chatbot_response_%widget_id% <- reactive(m$chatbot_response_%widget_id%()) %>% debounce(1000)

observeEvent(input$send_message_%widget_id%, {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$send_message_%widget_id%"))
    
    if (length(m$chatbot_messages_%widget_id%) == 0) m$chatbot_messages_%widget_id% <- tagList()
    
    if (length(m$chatbot_%widget_id%) == 0) m$chatbot_%widget_id% <- ellmer::chat_ollama(model = "llama3.2:3b")
    
    user_message <- input$user_input_%widget_id%
    
    # Créer la bulle du message utilisateur
    message_bubble <- div(
        class = "user-message-bubble",
        style = "
            background-color: #0B93F6; color: white; border-radius: 20px; 
            padding: 10px 15px; margin: 5px 0; display: inline-block; 
            max-width: 60%; float: right; clear: both; word-wrap: break-word;
            font-family: 'Helvetica Neue', Helvetica, sans-serif;",
        user_message
    )
    
    # Ajouter le message utilisateur
    m$chatbot_messages_%widget_id% <- tagAppendChild(
        m$chatbot_messages_%widget_id%,
        div(style = "overflow: hidden; margin-bottom: 10px;", message_bubble)
    )
    
    # Créer une bulle vide pour la réponse du chatbot (comme placeholder)
    chatbot_placeholder_bubble <- div(
        class = "chatbot-message-bubble",
        style = "
            background-color: #E5E5EA; color: #000000; border-radius: 20px; 
            padding: 10px 15px; margin: 5px 0; display: block; 
            width: 100%; clear: both; word-wrap: break-word;
            font-family: 'Helvetica Neue', Helvetica, sans-serif;",
        "..." # Placeholder pour indiquer que la réponse est en cours
    )
    
    # Ajouter le placeholder pour la réponse (nous le mettrons à jour plus tard)
    m$chatbot_messages_%widget_id% <- tagAppendChild(
        m$chatbot_messages_%widget_id%,
        div(style = "overflow: hidden; margin-bottom: 10px;", chatbot_placeholder_bubble)
    )
    
    # Reset the reactive value for a new response
    m$chatbot_response_%widget_id%("")
    
    # Mettre à jour l'UI immédiatement avec le message utilisateur et le placeholder
    output$chat_ui_%widget_id% <- renderUI(
        div(
            class = "chat-container",
            style = "display: flex; flex-direction: column; height: 100%; overflow-y: auto;",
            m$chatbot_messages_%widget_id%
        )
    )
    
    # Get the stream
    stream <- m$chatbot_%widget_id%$stream_async(user_message)
    
    # Process the stream
    res <- coro::async(function() {
      for (chunk in await_each(stream)) {
        m$chatbot_response_%widget_id%(paste0(m$chatbot_response_%widget_id%(), chunk))
      }
    })()
    
    updateTextInput(session, paste0("user_input_%widget_id%"), value = "")
})

observeEvent(m$chatbot_response_%widget_id%(), {
    %req%
    if (debug) cat(paste0("\\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$chatbot_response_%widget_id%"))
    
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
