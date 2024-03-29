# Get saved params for this widget
sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id%", .con = r$db)
widget_options <- DBI::dbGetQuery(m$db, sql)

m$widget_options_%widget_id% <- widget_options
m$scripts_%widget_id% <- widget_options %>% dplyr::filter(name == "script") %>% dplyr::select(id = value_num, name = value)
m$scripts_temp_%widget_id% <- m$scripts_%widget_id% %>% dplyr::mutate(modified = FALSE)
m$reload_dt_%widget_id% <- now()

# ------------------------
# --- Show / hide divs ---
# ------------------------

observeEvent(input$current_tab_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$current_tab_%widget_id%_run_selection"))
    
    tryCatch({
        sapply(c("script_div_%widget_id%", "scripts_management_div_%widget_id%"), shinyjs::hide)
        shinyjs::show(input$current_tab_%widget_id%)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# -------------------
# --- Script code ---
# -------------------

# When script code is executed
observeEvent(input$script_code_%widget_id%_run_selection, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$script_code_%widget_id%_run_selection"))
    
    tryCatch({
        if(!shinyAce::is.empty(input$script_code_%widget_id%_run_selection$selection)) m$script_final_code_%widget_id% <- input$script_code_%widget_id%_run_selection$selection
        else m$script_final_code_%widget_id% <- input$script_code_%widget_id%_run_selection$line
        
        m$script_code_trigger_%widget_id% <- now()
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$script_code_%widget_id%_run_all, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$script_code_%widget_id%_run_all"))
    
    tryCatch({
        m$script_final_code_%widget_id% <- input$script_code_%widget_id%
        m$script_code_trigger_%widget_id% <- now()
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$run_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$run_code_%widget_id%"))
    
    tryCatch({
        m$script_final_code_%widget_id% <- input$script_code_%widget_id%
        m$script_code_trigger_%widget_id% <- now()
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(m$script_code_trigger_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$script_code_trigger_%widget_id%"))
    
    tryCatch({
        divs <- c("r_script_result_div_%widget_id%", "rmarkdown_script_result_div_%widget_id%", "plot_script_result_div_%widget_id%")
        
        code <- m$script_final_code_%widget_id%
        
        # R code
        if (input$script_type_%widget_id% == "r"){
            options("cli.num_colors" = 1)
            captured_output <- capture.output(tryCatch(eval(parse(text = code)), error = function(e) cat(e), warning = function(w) cat(w)))
            options("cli.num_colors" = NULL)
            
            shinyjs::show("r_script_result_div_%widget_id%")
            sapply(c("rmarkdown_script_result_div_%widget_id%", "plot_script_result_div_%widget_id%"), shinyjs::hide)
            
            output$r_script_result_%widget_id% <- renderText({
                %req%
                if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - output$r_script_result_%widget_id%"))
                paste(captured_output, collapse = "\n")
            })
        }
        
        # RMarkdown code
        if (input$script_type_%widget_id% == "rmarkdown"){
        
            shinyjs::show("rmarkdown_script_result_div_%widget_id%")
            sapply(c("r_script_result_div_%widget_id%", "plot_script_result_div_%widget_id%"), shinyjs::hide)
            
            tryCatch({
            
                markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", 
                    m$app_folder, "/temp_files')\n",
                    "knitr::opts_chunk$set(root.dir = '", m$app_folder, "/temp_files', fig.path = '", m$app_folder, "/temp_files')\n```\n")
                
                markdown_file <- paste0(markdown_settings, code)
                
                # Create temp dir
                dir <- paste0(m$app_folder, "/temp_files")
                file <- paste0(dir, "/", as.character(now()) %>% stringr::str_replace_all(":", "_"), ".Md")
                
                # Create the markdown file
                knitr::knit(text = markdown_file, output = file, quiet = TRUE)
                
                output$rmarkdown_script_result_%widget_id% <- renderUI({
                    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - output$rmarkdown_script_result_%widget_id%"))
                    div(class = "markdown", withMathJax(includeMarkdown(file)))
                })
            }, error = function(e) "")
        }
        
        # Plot code
        if (input$script_type_%widget_id% == "plot"){
            
            shinyjs::show("plot_script_result_div_%widget_id%")
            sapply(c("r_script_result_div_%widget_id%", "rmarkdown_script_result_div_%widget_id%"), shinyjs::hide)
            
            output$plot_script_result_%widget_id% <- renderPlot({
                %req%
                if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - output$plot_script_result_%widget_id%"))
                eval(parse(text = paste(code, collapse = "\n")))
            })
        }
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Save updates on current script
observeEvent(input$script_code_%widget_id%_save, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$script_code_%widget_id%_save"))
    
    tryCatch({
        m$save_code_trigger_%widget_id% <- now()
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(input$save_code_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$save_code_%widget_id%"))
    
    tryCatch({
        m$save_code_trigger_%widget_id% <- now()
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

observeEvent(m$save_code_trigger_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer m$save_code_trigger_%widget_id%"))
    
    tryCatch({
        req(input$script_choice_%widget_id%)
        
        # Delete old options
        sql <- glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND name IN ('script_code', 'script_type') AND link_id = {input$script_choice_%widget_id%}", .con = m$db)
        query <- DBI::dbSendStatement(m$db, sql)
        DBI::dbClearResult(query)
        
        # Add new options
        last_row <- get_last_row(m$db, "widgets_options")
        
        new_options <- tibble::tibble(
            id = seq(last_row + 1, last_row + 2),
            widget_id = %widget_id%, person_id = NA_integer_, link_id = input$script_choice_%widget_id%,
            category = NA_character_, name = NA_character_, value = NA_character_, value_num = NA_integer_,
            creator_id = NA_integer_, datetime = as.character(now()), deleted = FALSE)
            
        new_options$name <- c("script_code", "script_type")
        new_options$value <- c(input$script_code_%widget_id%, input$script_type_%widget_id%)
        
        DBI::dbAppendTable(m$db, "widgets_options", new_options)
        
        sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = %widget_id%", .con = r$db)
        widget_options <- DBI::dbGetQuery(m$db, sql)
        
        m$widget_options_%widget_id% <- widget_options
        m$scripts_%widget_id% <- widget_options %>% dplyr::filter(name == "script") %>% dplyr::select(id = value_num, name = value)
        m$scripts_temp_%widget_id% <- m$scripts_%widget_id% %>% dplyr::mutate(modified = TRUE)
        
        # Notify user
        show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Load a script
observeEvent(input$script_choice_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$script_choice_%widget_id%"))
    
    tryCatch({
        script_code <- m$widget_options_%widget_id% %>% dplyr::filter(name == "script_code" & link_id == input$script_choice_%widget_id%)
        script_type <- m$widget_options_%widget_id% %>% dplyr::filter(name == "script_type" & link_id == input$script_choice_%widget_id%)
        
        if (nrow(script_code) == 0) script_code <- "" else script_code <- script_code %>% dplyr::pull(value)
        if (nrow(script_type) == 0) script_type <- "r" else script_type <- script_type %>% dplyr::pull(value)
        
        # Load script type
        shinyAce::updateAceEditor(session, "script_code_%widget_id%", value = script_code)
        
        # Load script content
        shiny.fluent::updateChoiceGroup.shinyInput(session, "script_type_%widget_id%", value = script_type)
        
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

# Plot width
# observeEvent(input$plot_width_%widget_id%, {
#     %req%
#     if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$plot_width_%widget_id%"))
#     
#     shinyjs::runjs(glue::glue("$('#{id}-plot_div_%widget_id%').css('width', '{isolate(input$plot_width_%widget_id%)}%');")) %>% throttle(1000)
# })

# ---------------
# --- Scripts ---
# ---------------

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
        show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
        
        # Add new script to scripts vector
        m$scripts_%widget_id% <- m$scripts_%widget_id% %>% 
            dplyr::bind_rows(tibble::tibble(id = last_id + 1, name = input$script_name_%widget_id%))
        m$scripts_temp_%widget_id% <- m$scripts_%widget_id% %>% dplyr::mutate(modified = TRUE)
        
        # Update DT & dropdown
        m$reload_dt_%widget_id% <- now()
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
        show_message_bar(output, "script_deleted", "severeWarning", i18n = i18np, ns = ns)
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})

# Change script type
observeEvent(input$script_type_%widget_id%, {
    %req%
    if (debug) cat(paste0("\n", now(), " - mod_", id, " - widget_id = %widget_id% - observer input$script_type_%widget_id%"))
    
    tryCatch({
        if (input$script_type_%widget_id% == "rmarkdown") shinyAce::updateAceEditor(session, "script_code_%widget_id%", mode = "markdown")
        else shinyAce::updateAceEditor(session, "script_code_%widget_id%", mode = "r")
    }, error = function(e) cat(paste0("\n", now(), " - ", toString(e))))
})
