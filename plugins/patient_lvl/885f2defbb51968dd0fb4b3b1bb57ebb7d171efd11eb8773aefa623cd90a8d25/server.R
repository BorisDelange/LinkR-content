# Get saved params for this widget
sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id%", .con = r$db)
widget_options <- DBI::dbGetQuery(m$db, sql)

m$widget_options_%widget_id% <- widget_options
m$scripts_%widget_id% <- widget_options %>% dplyr::filter(name == "script") %>% dplyr::select(id = value_num, name = value)
m$scripts_temp_%widget_id% <- m$scripts_%widget_id% %>% dplyr::mutate(modified = FALSE)
m$reload_dt_%widget_id% <- now()

# Load word sets & words
m$word_sets_%widget_id% <- widget_options %>% dplyr::filter(name == "word_set") %>% dplyr::select(id = value_num, name = value)
m$words_%widget_id% <- widget_options %>% dplyr::filter(name == "word") %>% dplyr::select(id = value_num, word_set_id = link_id, name = value)

# ------------------------
# --- Show / hide divs ---
# ------------------------

# observeEvent(input$current_tab_%widget_id%, {
#     %req%
#     if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$current_tab_%widget_id%"))
#     
#     tryCatch({
#         sapply(c("no_code_div_%widget_id%", "scripts_management_div_%widget_id%"), shinyjs::hide)
#         shinyjs::show(input$current_tab_%widget_id%)
#     }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
# })

observeEvent(input$params_current_tab_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$params_current_tab_%widget_id%"))
    
    tryCatch({
        sapply(c("notes_selection_div_%widget_id%", "word_sets_div_%widget_id%", "sub_params_div_%widget_id%"), shinyjs::hide)
        shinyjs::show(input$params_current_tab_%widget_id%)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$hide_params_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$hide_params_%widget_id%"))
    
    tryCatch({
        if (input$hide_params_%widget_id%){
            shinyjs::hide("params_div_%widget_id%")   
            shinyjs::runjs(glue::glue("$('#{id}-notes_div_%widget_id%').css('width', '100%');"))
        }
        else {
            shinyjs::show("params_div_%widget_id%")
            shinyjs::runjs(glue::glue("$('#{id}-notes_div_%widget_id%').css('width', '50%');"))
        }
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# -------------
# --- Notes ---
# -------------

# Render notes
observeEvent(m$selected_person, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$selected_person"))
    
    tryCatch({
        req(d$data_person$note %>% dplyr::count() %>% dplyr::pull() > 0)
        
        # Get data
        selected_person <- m$selected_person
        m$notes_%widget_id% <- d$data_person$note %>% dplyr::filter(person_id == selected_person) %>% dplyr::collect() %>%
            dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(note_type_concept_id = concept_id_1, note_type_concept_name = concept_name_1), by = "note_type_concept_id")
        
        # If a word set is selected
        if (input$word_set_filter_%widget_id% != 0) m$run_filter_%widget_id% <- now()
        
        m$reload_notes_%widget_id% <- now()
        m$reload_notes_type_%widget_id% <- "person_changed"
        m$reload_notes_datatable_%widget_id% <- now()
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$chronological_order_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$chronological_order_%widget_id%"))
    
    tryCatch({
        # Reload only we display all notes
        if (length(input$show_all_notes_%widget_id%) > 0){
            if (input$show_all_notes_%widget_id%){
                m$reload_notes_%widget_id% <- now()
                m$reload_notes_type_%widget_id% <- "reload_order"
            }
        }
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$show_all_notes_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$show_all_notes_%widget_id%"))
    
    tryCatch({
        m$reload_notes_datatable_%widget_id% <- now()
        m$reload_notes_%widget_id% <- now()
        m$reload_notes_type_%widget_id% <- "show_all_notes"
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$notes_datatable_%widget_id%_rows_selected, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$notes_datatable_%widget_id%_rows_selected"))
    
    tryCatch({
        m$reload_notes_%widget_id% <- now()
        m$reload_notes_type_%widget_id% <- "selected_row"
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$run_filter_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_filter_%widget_id%"))
    
    m$run_filter_%widget_id% <- now()
})

observeEvent(m$run_filter_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$run_filter_%widget_id%"))
    
    tryCatch({
        # Apply filters
        if (input$word_set_filter_%widget_id% != 0L){
            # Get words
            words <- m$words_%widget_id% %>% dplyr::filter(word_set_id == input$word_set_filter_%widget_id%) %>% dplyr::pull(name)
            if (length(words) > 0){
                pattern <- paste(words, collapse = "|")
                m$notes_%widget_id% <- 
                    m$notes_%widget_id% %>%
                    dplyr::filter(grepl(pattern, note_text, ignore.case = TRUE)) %>%
                    dplyr::mutate(note_text = stringr::str_replace_all(note_text, stringr::regex(pattern, ignore_case = TRUE), "<mark>\\0</mark>"))
            }
        }
        else {
            selected_person <- m$selected_person
            m$notes_%widget_id% <- d$data_person$note %>% dplyr::filter(person_id == selected_person) %>% dplyr::collect() %>%
                dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(note_type_concept_id = concept_id_1, note_type_concept_name = concept_name_1), by = "note_type_concept_id")
        }
    
        m$reload_notes_%widget_id% <- now()
        m$reload_notes_datatable_%widget_id% <- now()
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$display_format_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$display_format_%widget_id%"))
    
    tryCatch({
        m$reload_notes_%widget_id% <- now()
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(m$reload_notes_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$reload_notes_%widget_id%"))
    
    tryCatch({
        if (nrow(m$notes_%widget_id%) == 0) output$notes_%widget_id% <- renderUI(i18np$t("no_notes_to_display"))
    
        req(nrow(m$notes_%widget_id%) > 0)
        
        chronological_order <- FALSE
        if (length(input$chronological_order_%widget_id%) > 0) if (input$chronological_order_%widget_id%) chronological_order <- TRUE
        if (chronological_order) m$notes_%widget_id% <- m$notes_%widget_id% %>% dplyr::arrange(note_datetime)
        else m$notes_%widget_id% <- m$notes_%widget_id% %>% dplyr::arrange(dplyr::desc(note_datetime))
        
        notes <- m$notes_%widget_id%
        
        if (m$reload_notes_type_%widget_id% == "selected_row" | m$reload_notes_type_%widget_id% == "show_all_notes"){
            show_all_notes <- FALSE
            if (length(input$show_all_notes_%widget_id%) > 0) if (input$show_all_notes_%widget_id%) show_all_notes <- TRUE
            if (show_all_notes){
                selected_notes_type <- m$notes_types_%widget_id%[input$notes_datatable_%widget_id%_rows_selected, ]
                notes <- notes %>% dplyr::filter(note_type_concept_name == selected_notes_type$note_type_concept_name)
            }
            else {
                selected_notes <- m$notes_%widget_id%[input$notes_datatable_%widget_id%_rows_selected, ]
                notes <- notes %>% dplyr::filter(note_id %in% selected_notes$note_id)
            }
        }
        
        # Transform multiple \n in one \n
        # notes <- notes %>% dplyr::mutate(note_text = gsub("\\n(\\s*\\n)+", "\n", note_text))
        
        display_format <- "html"
        if (length(input$display_format_%widget_id%) > 0) display_format <- input$display_format_%widget_id%
        
        output$notes_%widget_id% <- renderUI({
            %req%
            if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - output$html_notes_%widget_id%"))
            
            note_panels <- lapply(1:nrow(notes), function(i) {
                note <- notes[i,]
                
                if (display_format == "html") note_text <- HTML(stringr::str_replace_all(note$note_text, "\n", "<br />"))
                else note_text <- stringr::str_replace_all(note$note_text, c("<" = "&lt;", ">" = "&gt;", "\n" = "\\\\n"))
                
                div(strong(note$note_title), " - ", format_datetime(note$note_datetime, m$language), br(), br(), 
                    note_text,
                    style = "border:dashed 1px; padding:10px; margin-top:10px;")
            })
            
            do.call(div, note_panels)
        })
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Datatable of all notes
observeEvent(m$reload_notes_datatable_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$reload_notes_datatable_%widget_id%"))
    
    tryCatch({
        if (nrow(m$notes_%widget_id%) == 0){
        
            notes <- tibble::tibble(note_type_concept_name = character(), note_title = character(), note_datetime = character())
            names(notes) <- c(i18np$t("category"), i18np$t("title"), i18np$t("datetime"))
            
            output$notes_datatable_%widget_id% <- DT::renderDT(
                DT::datatable(
                    notes,
                    options = list(
                        dom = "<'datatable_length'l><'top't><'bottom'p>",
                        pageLength = 10, displayStart = 0,
                        language = list(
                            paginate = list(previous = i18n$t("dt_previous"), `next` = i18n$t("dt_next")),
                            search = i18n$t("dt_search"),
                            lengthMenu = i18n$t("dt_entries"),
                            emptyTable = i18n$t("dt_empty")),
                        compact = TRUE, hover = TRUE
                    ),
                    filter = list(position = "top"),
                    selection = "single",
                    rownames = FALSE, escape = FALSE
                ),
                server = TRUE
            )
        }
        
        req(nrow(m$notes_%widget_id%) > 0)
        
        show_all_notes <- FALSE
        if (length(input$show_all_notes_%widget_id%) > 0) if (input$show_all_notes_%widget_id%) show_all_notes <- TRUE
        
        column_defs <- list()
        
        notes <- m$notes_%widget_id%
        
        if (show_all_notes){
            notes <- notes %>% 
                dplyr::group_by(note_type_concept_name) %>%
                dplyr::summarize(count = dplyr::n()) %>%
                dplyr::ungroup() %>%
                dplyr::arrange(dplyr::desc(count)) %>%
                dplyr::mutate_at("note_type_concept_name", as.factor)
                
            m$notes_types_%widget_id% <- notes
                
            column_widths <- c("count" = "200px")
            
            names(notes) <- c(i18np$t("category"), i18np$t("occurrences"))
        }
        else {
        
            # Filter on chronological order?
            
            chronological_order <- FALSE
            if (length(input$chronological_order_%widget_id%) > 0) if (input$chronological_order_%widget_id%) chronological_order <- TRUE
            if (chronological_order) m$notes_%widget_id% <- m$notes_%widget_id% %>% dplyr::arrange(note_datetime)
            else m$notes_%widget_id% <- m$notes_%widget_id% %>% dplyr::arrange(dplyr::desc(note_datetime))
            
            notes <- notes %>% dplyr::select(note_type_concept_name, note_title, note_datetime) %>% 
                dplyr::mutate_at("note_datetime", as.character) %>%
                dplyr::mutate_at(c("note_type_concept_name", "note_title"), as.factor)
            
            column_widths <- c("note_datetime" = "200px")
            for(name in names(column_widths)) column_defs <- rlist::list.append(column_defs, list(width = column_widths[[name]], targets = which(grepl(paste0("^", name, "$"), names(notes))) - 1))
                
            centered_cols_vec <- integer()
            centered_cols_vec <- c(centered_cols_vec, c(which(grepl("^note_datetime$", names(notes))) - 1))
            column_defs <- rlist::list.append(column_defs, list(className = "dt-body-center", targets = centered_cols_vec))
          
            sortable_cols_vec <- integer()
            cols <- c(1:length(names(notes))) - 1
            sortable_cols_vec <- c(sortable_cols_vec, c(which(grepl("^note_datetime$", names(notes))) - 1))
            non_sortable_cols_vec <- cols[!cols %in% sortable_cols_vec]
            column_defs <- rlist::list.append(column_defs, list(sortable = FALSE, targets = non_sortable_cols_vec))
          
            names(notes) <- c(i18np$t("category"), i18np$t("title"), i18np$t("datetime"))
        }
        
        output$notes_datatable_%widget_id% <- DT::renderDT(
            DT::datatable(
                notes,
                options = list(
                    dom = "<'datatable_length'l><'top't><'bottom'p>",
                    pageLength = 10, displayStart = 0,
                    columnDefs = column_defs,
                    language = list(
                        paginate = list(previous = i18n$t("dt_previous"), `next` = i18n$t("dt_next")),
                        search = i18n$t("dt_search"),
                        lengthMenu = i18n$t("dt_entries"),
                        emptyTable = i18n$t("dt_empty")),
                    compact = TRUE, hover = TRUE
                ),
                filter = list(position = "top"),
                selection = "single",
                rownames = FALSE, escape = FALSE
            ),
            server = TRUE
        )
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# ------------
# --- Code ---
# ------------

# -----------------
# --- Word sets ---
# -----------------

# Add a word set

observeEvent(input$new_word_set_add_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$new_word_set_add_%widget_id%"))
    
    tryCatch({
        new_name <- input$new_word_set_name_%widget_id%
        req(new_name != "")
        
        # Check if name already used
        sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id% AND name = 'word_set' AND value = {new_name}", .con = m$db)
        result <- DBI::dbGetQuery(m$db, sql)
        check_used_name <- !nrow(result) > 0
        
        if (!check_used_name) show_message_bar(output, "name_already_used", "warning", i18n = i18np, ns = ns)
        req(check_used_name)
        
        # Add name in database
        last_row <- get_last_row(m$db, "widgets_options")
        sql <- glue::glue_sql("SELECT COALESCE(MAX(value_num), 0) FROM widgets_options WHERE widget_id = %widget_id% AND name = 'word_set'", .con = m$db)
        last_id <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull()
        
        new_options <- tibble::tibble(
            id = last_row + 1, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
            category = NA_character_, name = "word_set", value = new_name, value_num = last_id + 1,
            creator_id = NA_integer_, datetime = as.character(now()), deleted = FALSE)
            
        DBI::dbAppendTable(m$db, "widgets_options", new_options)
        
        # Reset textfield
        shiny.fluent::updateTextField.shinyInput(session, "new_word_set_name_%widget_id%", value = "")
        
        # Add new word set to word sets vector
        m$word_sets_%widget_id% <- m$word_sets_%widget_id% %>% dplyr::bind_rows(tibble::tibble(id = last_id + 1, name = new_name))
        
        # Reset word dropdown
        shiny.fluent::updateDropdown.shinyInput(session, "word_%widget_id%", options = list(), value = NULL)
        
        show_message_bar(output, "word_set_added", "success", i18n = i18np, ns = ns)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Update word sets
observeEvent(m$word_sets_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$word_sets_%widget_id%"))
    
    tryCatch({
        shiny.fluent::updateDropdown.shinyInput(session, "word_set_%widget_id%", options = convert_tibble_to_list(m$word_sets_%widget_id%, key_col = "id", text_col = "name"), value = NULL)
        
        word_sets_filter <- tibble::tibble(id = 0L, name = i18np$t("none")) %>% dplyr::bind_rows(m$word_sets_%widget_id%)
        shiny.fluent::updateDropdown.shinyInput(session, "word_set_filter_%widget_id%", options = convert_tibble_to_list(word_sets_filter, key_col = "id", text_col = "name"), value = 0L)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Delete a word set
observeEvent(input$delete_word_set_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$delete_word_set_%widget_id%"))
    
    tryCatch({
        req(length(input$word_set_%widget_id%) > 0)
        shinyjs::show("delete_word_set_modal_%widget_id%")
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$close_delete_word_set_modal_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$close_delete_word_set_modal_%widget_id%"))
    
    tryCatch({
        shinyjs::hide("delete_word_set_modal_%widget_id%")
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$confirm_word_set_deletion_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$confirm_word_set_deletion_%widget_id%"))
    
    tryCatch({
        req(length(input$word_set_%widget_id%) > 0)
        
        sql <- glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND name = 'word_set' AND value_num = {input$word_set_%widget_id%}", .con = m$db)
        query <- DBI::dbSendStatement(m$db, sql)
        DBI::dbClearResult(query)
        
        shinyjs::hide("delete_word_set_modal_%widget_id%")
        
        show_message_bar(output, "word_set_deleted", "warning", i18n = i18np, ns = ns)
        
        m$word_sets_%widget_id% <- m$word_sets_%widget_id% %>% dplyr::filter(id != input$word_set_%widget_id%)
        
        # Reset word dropdown
        shiny.fluent::updateDropdown.shinyInput(session, "word_%widget_id%", options = list(), value = NULL)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Load words of a word set
observeEvent(input$word_set_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$add_word_%widget_id%"))
    
    tryCatch({
        if (length(input$word_set_%widget_id%) == 0) shiny.fluent::updateDropdown.shinyInput(session, "word_%widget_id%", options = list(), value = NULL)
        else {
            words <- m$words_%widget_id% %>% dplyr::filter(word_set_id == input$word_set_%widget_id%)
            shiny.fluent::updateDropdown.shinyInput(session, "word_%widget_id%", options = convert_tibble_to_list(words, key_col = "id", text_col = "name"), value = NULL)
        }
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Add a word

observeEvent(input$add_word_%widget_id%, {
     %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$add_word_%widget_id%"))
    
    tryCatch({
        req(length(input$word_set_%widget_id%) > 0, input$new_word_name_%widget_id% != "")
        
        word_set_id <- input$word_set_%widget_id%
        new_name <- input$new_word_name_%widget_id%
        
        # Check if name already used
        sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id% AND name = 'word' AND link_id = {word_set_id} AND value = {new_name}", .con = m$db)
        result <- DBI::dbGetQuery(m$db, sql)
        check_used_name <- !nrow(result) > 0
        
        if (!check_used_name) show_message_bar(output, "name_already_used", "warning", i18n = i18np, ns = ns)
        req(check_used_name)
        
         # Add name in database
        last_row <- get_last_row(m$db, "widgets_options")
        sql <- glue::glue_sql("SELECT COALESCE(MAX(value_num), 0) FROM widgets_options WHERE widget_id = %widget_id% AND name = 'word'", .con = m$db)
        last_id <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull()
        
        new_options <- tibble::tibble(
            id = last_row + 1, widget_id = %widget_id%, person_id = NA_integer_, link_id = word_set_id,
            category = NA_character_, name = "word", value = new_name, value_num = last_id + 1,
            creator_id = NA_integer_, datetime = as.character(now()), deleted = FALSE)
            
        DBI::dbAppendTable(m$db, "widgets_options", new_options)
        
        # Reset textfield
        shiny.fluent::updateTextField.shinyInput(session, "new_word_name_%widget_id%", value = "")
        
        # Add new word to words vector
        m$words_%widget_id% <- m$words_%widget_id% %>% dplyr::bind_rows(tibble::tibble(id = last_id + 1, word_set_id = word_set_id, name = new_name))
        
        show_message_bar(output, "word_added", "success", i18n = i18np, ns = ns)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Update words
observeEvent(m$words_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$words_%widget_id%"))
    
    tryCatch({
        req(length(input$word_set_%widget_id%) > 0)
        
        words <- m$words_%widget_id% %>% dplyr::filter(word_set_id == input$word_set_%widget_id%)
        shiny.fluent::updateDropdown.shinyInput(session, "word_%widget_id%", options = convert_tibble_to_list(words, key_col = "id", text_col = "name"), value = NULL)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Delete a word
observeEvent(input$delete_word_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$delete_word_%widget_id%"))
    
    tryCatch({
        req(length(input$word_%widget_id%) > 0)
        shinyjs::show("delete_word_modal_%widget_id%")
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$close_delete_word_modal_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$close_delete_word_modal_%widget_id%"))
    
    tryCatch({
        shinyjs::hide("delete_word_modal_%widget_id%")
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$confirm_word_deletion_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$confirm_word_deletion_%widget_id%"))
    
    tryCatch({
        req(length(input$word_%widget_id%) > 0, length(input$word_set_%widget_id%) > 0)
        
        sql <- glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND name = 'word' AND link_id = {input$word_set_%widget_id%} AND value_num = {input$word_%widget_id%}", .con = m$db)
        query <- DBI::dbSendStatement(m$db, sql)
        DBI::dbClearResult(query)
        
        shinyjs::hide("delete_word_modal_%widget_id%")
        
        show_message_bar(output, "word_deleted", "warning", i18n = i18np, ns = ns)
        
        m$words_%widget_id% <- m$words_%widget_id% %>% dplyr::filter(id != input$word_%widget_id%)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# ---------------
# --- Scripts ---
# ---------------

# Add a new script
observeEvent(input$add_script_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$add_script_%widget_id%"))
    
    tryCatch({
        # Check if name is not empty
        empty_name <- TRUE
        if (length(input$script_name_%widget_id%) > 0) if (input$script_name_%widget_id% != "") empty_name <- FALSE
        if (empty_name) shiny.fluent::updateTextField.shinyInput(session, "script_name_%widget_id%", errorMessage = i18n$t("provide_valid_name"))
        req(!empty_name)
        shiny.fluent::updateTextField.shinyInput(session, "script_name_%widget_id%", errorMessage = NULL)
        
        # Check if name is not already used
        sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id% AND name = 'script' AND value = {input$script_name_%widget_id%}", .con = m$db)
        already_used_name <- DBI::dbGetQuery(m$db, sql) %>% nrow() >= 1
        if (already_used_name) shiny.fluent::updateTextField.shinyInput(session, "script_name_%widget_id%", errorMessage = i18n$t("name_already_used"))
        req(!already_used_name)
        shiny.fluent::updateTextField.shinyInput(session, "script_name_%widget_id%", errorMessage = NULL)
        
        # Add script to database
        
        last_row <- get_last_row(m$db, "widgets_options")
        sql <- glue::glue_sql("SELECT COALESCE(MAX(value_num), 0) FROM widgets_options WHERE widget_id = %widget_id% AND name = 'script'", .con = m$db)
        last_id <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull()
        
        new_options <- tibble::tibble(
            id = last_row + 1, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
            category = NA_character_, name = "script", value = input$script_name_%widget_id%, value_num = last_id + 1,
            creator_id = NA_integer_, datetime = as.character(now()), deleted = FALSE)
            
        DBI::dbAppendTable(m$db, "widgets_options", new_options)
        
        # Reset TextField
        shiny.fluent::updateTextField.shinyInput(session, "script_name_%widget_id%", value = "")
        
        # Notify user
        show_message_bar(output, "script_added", "success", i18n = i18np, ns = ns)
        
        # Add new script to scripts vector
        m$scripts_%widget_id% <- m$scripts_%widget_id% %>% 
            dplyr::bind_rows(tibble::tibble(id = last_id + 1, name = input$script_name_%widget_id%))
        m$scripts_temp_%widget_id% <- m$scripts_%widget_id% %>% dplyr::mutate(modified = TRUE)
        
        # Update DT & dropdown
        m$reload_dt_%widget_id% <- now()
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Load a script
observeEvent(input$script_choice_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$script_choice_%widget_id%"))
    
    tryCatch({
        widget_options <- m$widget_options_%widget_id% %>% dplyr::filter(link_id == input$script_choice_%widget_id%)
        
    #     run_code_at_script_launch <- FALSE
    #     run_plot_at_script_launch <- FALSE
        code <- ""
        
    #     for (input_name in inputs){
    #         widget_option <- widget_options %>% dplyr::filter(name == input_name)
    #         
    #         # Update inputs with saved values
    #         if (nrow(widget_option) > 0){
    #             if (input_name %in% dropdowns){
    #                 if (input_name %in% c("x_variable", "y_variable")) value <- widget_option$value_num else value <- widget_option$value
    #                 shiny.fluent::updateDropdown.shinyInput(session, paste0(input_name, "_%widget_id%"), value = value)
    #             }
    #             if (input_name %in% textfields) shiny.fluent::updateTextField.shinyInput(session, paste0(input_name, "_%widget_id%"), value = widget_option$value)
    #             if (input_name %in% spin_buttons) shiny.fluent::updateSpinButton.shinyInput(session, paste0(input_name, "_%widget_id%"), value = widget_option$value_num)
    #             if (input_name %in% toggle_inputs) shiny.fluent::updateToggle.shinyInput(session, paste0(input_name, "_%widget_id%"), value = as.logical(widget_option$value))
    #             if (input_name %in% colour_inputs) shiny.fluent::updateSwatchColorPicker.shinyInput(session, paste0(input_name, "_%widget_id%"), value = widget_option$value)
    #             if (input_name %in% ace_inputs) shinyAce::updateAceEditor(session, paste0(input_name, "_%widget_id%"), value = widget_option$value)
    #             
    #             if (input_name == "run_code_at_script_launch") run_code_at_script_launch <- as.logical(widget_option$value)
    #             if (input_name == "run_plot_at_script_launch") run_plot_at_script_launch <- as.logical(widget_option$value)
    #             if (input_name == "code") code <- widget_option$value
    #         }
    #         if (nrow(widget_option) == 0){
    #             if (input_name %in% dropdowns) shiny.fluent::updateDropdown.shinyInput(session, paste0(input_name, "_%widget_id%"), value = default_values[[input_name]])
    #             if (input_name %in% textfields) shiny.fluent::updateTextField.shinyInput(session, paste0(input_name, "_%widget_id%"), value = default_values[[input_name]])
    #             if (input_name %in% spin_buttons) shiny.fluent::updateSpinButton.shinyInput(session, paste0(input_name, "_%widget_id%"), value = default_values[[input_name]])
    #             if (input_name %in% toggle_inputs) shiny.fluent::updateToggle.shinyInput(session, paste0(input_name, "_%widget_id%"), value = default_values[[input_name]])
    #             if (input_name %in% colour_inputs) shiny.fluent::updateSwatchColorPicker.shinyInput(session, paste0(input_name, "_%widget_id%"), value = default_values[[input_name]])
    #             if (input_name %in% ace_inputs) shinyAce::updateAceEditor(session, paste0(input_name, "_%widget_id%"), value = default_values[[input_name]])
    #         }
    #     }
        
    #     # Render this plot
    #     if (run_code_at_script_launch){
    #         m$run_code_%widget_id% <- code
    #         m$run_code_trigger_%widget_id% <- now()
    #     }
    #     else if (run_plot_at_script_launch) shinyjs::delay(500, shinyjs::click("show_%widget_id%"))
        
        # Save that this script is selected
        sql <- glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND name = 'selected_script'", .con = m$db)
        query <- DBI::dbSendStatement(m$db, sql)
        DBI::dbClearResult(query)
        
        last_row <- get_last_row(m$db, "widgets_options")
        
        new_options <- tibble::tibble(
            id = last_row + 1, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
            category = NA_character_, name = "selected_script", value = NA_character_, value_num = input$script_choice_%widget_id%,
            creator_id = NA_integer_, datetime = as.character(now()), deleted = FALSE)
            
        DBI::dbAppendTable(m$db, "widgets_options", new_options)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Var for delete confirm react
m$delete_open_dialog_%widget_id% <- FALSE

# Update scripts DT & dropdown
observeEvent(m$reload_dt_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$reload_dt_%widget_id%"))
    
    tryCatch({
        # Reload DT
        
        # Prepare data for the datatable
        m$scripts_datatable_temp_%widget_id% <- 
            m$scripts_%widget_id% %>%
            dplyr::rename(id_temp = id) %>%
            dplyr::mutate(action = as.character(actionButton("delete_%id%", "", icon = icon("trash-alt"), 
                onclick = paste0("Shiny.setInputValue('", id, "-deleted_pressed_%widget_id%', this.id, {priority: 'event'})")))) %>%
            dplyr::mutate(action = stringr::str_replace_all(action, "%id%", as.character(id_temp))) %>%
            dplyr::rename(id = id_temp)
        
        # If there is not already a proxy, create datatable
        if (length(m$datatable_proxy_%widget_id%) == 0){
            render_datatable(output = output, ns = ns, i18n = i18n, data = m$scripts_datatable_temp_%widget_id%,
                output_name = "scripts_management_datatable_%widget_id%", col_names = c(i18n$t("id"), i18n$t("name"), i18n$t("action")),
                editable_cols = "name", sortable_cols = c("id", "name"), centered_cols = c("id", "action"), column_widths = c("id" = "80px", "action" = "80px"),
                searchable_cols = "name", filter = TRUE, selection = "multiple")
            
            # Create a proxy for this datatable
            m$datatable_proxy_%widget_id% <- DT::dataTableProxy("scripts_management_datatable_%widget_id%", deferUntilFlush = FALSE)
        }
        if (length(m$datatable_proxy_%widget_id%) > 0) DT::replaceData(m$datatable_proxy_%widget_id%, m$scripts_datatable_temp_%widget_id%, resetPaging = FALSE, rownames = FALSE)
        
        # Update dropdown
        value <- NULL
        if (length(input$script_choice_%widget_id%) > 0) value <- input$script_choice_%widget_id%
        if (length(input$script_choice_%widget_id%) == 0 & nrow(m$scripts_%widget_id%) > 0){
            # Load last selected script
            selected_script <- m$widget_options_%widget_id%  %>% dplyr::filter(name == "selected_script")
            if (nrow(selected_script) > 0) value <- selected_script %>% dplyr::pull(value_num)
        }
        
        shiny.fluent::updateDropdown.shinyInput(session, "script_choice_%widget_id%", 
            options = convert_tibble_to_list(m$scripts_%widget_id%, key_col = "id", text_col = "name"), value = value)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Updates on scripts DT
observeEvent(input$scripts_management_datatable_%widget_id%_cell_edit, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_management_datatable_%widget_id%_cell_edit"))
    
    tryCatch({
        edit_info <- input$scripts_management_datatable_%widget_id%_cell_edit
        m$scripts_temp_%widget_id% <- DT::editData(m$scripts_temp_%widget_id%, edit_info, rownames = FALSE)
        
        # Store that this row has been modified
        m$scripts_temp_%widget_id%[[edit_info$row, "modified"]] <- TRUE
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Save updates on scripts
observeEvent(input$save_scripts_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$save_scripts_%widget_id%"))
    
    tryCatch({
        # Check if there are no duplicates in names
        duplicates <- m$scripts_temp_%widget_id% %>% dplyr::mutate_at("name", tolower) %>% dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow() >= 1
        
        if (duplicates) show_message_bar(output, "scripts_names_duplicates", "severeWarning", i18n = i18np, ns = ns)
        
        req(!duplicates)
        
        req(nrow(m$scripts_temp_%widget_id%) > 0)
        
        # Delete old options
        sql <- glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND name = 'script'", .con = m$db)
        query <- DBI::dbSendStatement(m$db, sql)
        DBI::dbClearResult(query)
        
        # Add new options
        last_row <- get_last_row(m$db, "widgets_options")
        
        new_options <- tibble::tibble(
            id = seq(last_row + 1, last_row + nrow(m$scripts_temp_%widget_id%)),
            widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
            category = NA_character_, name = "script", value = m$scripts_temp_%widget_id%$name, value_num = m$scripts_temp_%widget_id%$id,
            creator_id = NA_integer_, datetime = as.character(now()), deleted = FALSE)
            
        DBI::dbAppendTable(m$db, "widgets_options", new_options)
        
        # Update scripts dropdown
        value <- NULL
        if (length(input$script_choice_%widget_id%) > 0) value <- input$script_choice_%widget_id%
        shiny.fluent::updateDropdown.shinyInput(session, "script_choice_%widget_id%", 
            options = convert_tibble_to_list(m$scripts_temp_%widget_id%, key_col = "id", text_col = "name"), value = value)
        
        # Notify user
        show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Delete scripts

### Delete with trash icon
observeEvent(input$deleted_pressed_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$deleted_pressed_%widget_id%"))
    
    tryCatch({
        # Reload datatable (to unselect rows)
        DT::replaceData(m$datatable_proxy_%widget_id%, m$scripts_datatable_temp_%widget_id%, resetPaging = FALSE, rownames = FALSE)
        
        m$delete_scripts_%widget_id% <- as.integer(substr(input$deleted_pressed_%widget_id%, nchar("delete_") + 1, 100))
        m$delete_open_dialog_%widget_id% <- TRUE
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

### Delete with "delete selection" button
observeEvent(input$delete_scripts_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$delete_scripts_%widget_id%"))
    
    tryCatch({
        req(length(input$scripts_management_datatable_%widget_id%_rows_selected) > 0)
        m$delete_scripts_%widget_id% <- m$scripts_%widget_id%[input$scripts_management_datatable_%widget_id%_rows_selected, ] %>% dplyr::pull(id)
        m$delete_open_dialog_%widget_id% <- TRUE
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

### reactOutput for deletion confirmation
output$delete_confirm_%widget_id% <- shiny.fluent::renderReact({
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - output$delete_confirm_%widget_id%"))
    
    tryCatch({
        shiny.fluent::Dialog(
            hidden = !m$delete_open_dialog_%widget_id%,
            onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('scripts_hide_dialog_%widget_id%', Math.random()); }")),
            dialogContentProps = list(
                type = 0,
                title = i18np$t("confirm_deletion_title"),
                closeButtonAriaLabel = "Close",
                subText = tagList(i18np$t("confirm_deletion_subtext"), br(), br()
            )
        ),
        modalProps = list(),
        shiny.fluent::DialogFooter(
            shiny.fluent::PrimaryButton.shinyInput(ns("scripts_delete_confirmed_%widget_id%"), text = i18n$t("delete")),
            shiny.fluent::DefaultButton.shinyInput(ns("scripts_delete_canceled_%widget_id%"), text = i18n$t("dont_delete"))
            )
        )
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

### Close reactOutput
observeEvent(input$scripts_hide_dialog_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_hide_dialog_%widget_id%"))
    
    tryCatch({
        m$delete_open_dialog_%widget_id% <- FALSE
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})
observeEvent(input$scripts_delete_canceled_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_delete_canceled_%widget_id%"))
    
    tryCatch({
        m$delete_open_dialog_%widget_id% <- FALSE
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

### Deletion confirmed
observeEvent(input$scripts_delete_confirmed_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$scripts_delete_confirmed_%widget_id%"))
    
    tryCatch({
        m$delete_open_dialog_%widget_id% <- FALSE
        
        # Get scripts ids
        ids_to_del <- m$delete_scripts_%widget_id%
        
        # Delete scripts in DB
        sql <- glue::glue_sql(paste0("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND (",
            "(name = 'script' AND value_num IN ({ids_to_del*})) OR ",
            "(link_id IN ({ids_to_del*})))"), .con = m$db)
        query <- DBI::dbSendStatement(m$db, sql)
        DBI::dbClearResult(query)
        
        # Update m var
        m$scripts_%widget_id% <- m$scripts_%widget_id% %>% dplyr::filter(id %not_in% ids_to_del)
        m$scripts_temp_%widget_id% <- m$scripts_%widget_id% %>% dplyr::mutate(modified = TRUE)
        
        # Reload DT
        m$reload_dt_%widget_id% <- now()
        
        # Notify user
        show_message_bar(output, "script_deleted", "warning", i18n = i18np, ns = ns)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})
