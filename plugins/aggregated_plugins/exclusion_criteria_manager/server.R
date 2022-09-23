##########################################
# Translations                           #
##########################################

# Create our dictionnary
new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
  "EN", "exclusion_criterion_added", "New exclusion criterion added",
  "FR", "exclusion_criterion_added", "Nouveau critère d'exclusion ajouté",
  "EN", "delete_exclusion_criterion", "Delete an exclusion criterion",
  "FR", "delete_exclusion_criterion", "Supprimer un critère d'exclusion",
  "EN", "delete_exclusion_criterion_subtext", "Are you sure you want to delete this exclusion criterion ?",
  "FR", "delete_exclusion_criterion_subtext", "Etes-vous sûr de vouloir supprimer ce critère d'exclusion ?",
  "EN", "exclusion_criterion_deleted", "Exclusion criterion deleted",
  "FR", "exclusion_criterion_deleted", "Critère d'exclusion supprimé"
)

##########################################
# Show / hide divs                       #
##########################################

divs_%group_id%_%study_id% <- c("div_manage_exclusion_criteria_%group_id%_%study_id%", "div_add_exclusion_criterion_%group_id%_%study_id%")

observeEvent(input$div_%group_id%_%study_id%, {
  
  sapply(divs_%group_id%_%study_id% %>% setdiff(., input$div_%group_id%_%study_id%), shinyjs::hide)
  shinyjs::show(input$div_%group_id%_%study_id%)
})

##########################################
# Load current exclusion criteria        #
##########################################

plugins_load_%group_id%_%study_id% <- function(r){
  
  # Load exclusion criteria
  sql <- "SELECT id, value, creator_id, datetime FROM modules_elements_options WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id% AND category = 'aggregated' AND name = 'exclusion_reason_name'"
  exclusion_criteria <- DBI::dbGetQuery(r$db, sql)
  
  # Load display orders
  sql <- "SELECT link_id, value_num FROM modules_elements_options WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id% AND category = 'aggregated' AND name = 'display_order'"
  display_orders <- DBI::dbGetQuery(r$db, sql) %>% dplyr::select(id = link_id, display_order = value_num)
  
  # Merge the two tables
  # Reset display_order column (if we have display orders 1, 2 & 4 in database - if on criterion has been deleted - we will have 1, 2 & 3)
  exclusion_criteria <- exclusion_criteria %>% dplyr::left_join(display_orders, by = "id") %>% dplyr::relocate(display_order, .after = "value") %>% dplyr::arrange(display_order)
  
  # Get creator name
  exclusion_criteria <- 
    exclusion_criteria %>% 
    dplyr::left_join(
      isolate(r$users) %>% dplyr::transmute(creator_id = id, creator_name = paste0(firstname, " ", lastname)),
      by = "creator_id") %>%
    dplyr::relocate(creator_name, .after = "display_order") %>% dplyr::select(-creator_id)
  
  r$plugins_data_%group_id%_%study_id% <- exclusion_criteria
}

plugins_load_%group_id%_%study_id%(r)

# Dropdowns observers already created
r$plugins_dropdowns_observers_%group_id%_%study_id% <- integer()

##########################################
# Add a new exclusion criterion          #
##########################################

observeEvent(input$add_%group_id%_%study_id%, {
  
  tryCatch({
    
    new_name <- input$name_%group_id%_%study_id%
      
      if (length(new_name) == 0) shiny.fluent::updateTextField.shinyInput(session, "name_%group_id%_%study_id%", errorMessage = translate(language, paste0("provide_valid_name"), r$words))
    else shiny.fluent::updateTextField.shinyInput(session, "name_%group_id%_%study_id%", errorMessage = NULL)
    
    req(length(new_name) > 0)
    
    sql <- "SELECT DISTINCT(value) FROM modules_elements_options WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id% AND name = 'exclusion_reason_name'" 
    distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull() %>% tolower()
    
    if (tolower(new_name) %in% distinct_values) show_message_bar(output, 2, "name_already_used", "severeWarning", language)
    req(tolower(new_name) %not_in% distinct_values)
    
    last_row <- get_last_row(r$db, "modules_elements_options")
    datetime <- as.character(Sys.time())
    
    # Insert new exclusion criterion in database
    
    sql <- glue::glue_sql("INSERT INTO modules_elements_options(id, group_id, study_id, category, name, value, creator_id, datetime, deleted)
            SELECT {last_row + 1}, %group_id%, %study_id%, 'aggregated', 'exclusion_reason_name', {new_name}, {r$user_id}, {datetime}, FALSE", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    # Insert also its display order
    sql <- glue::glue_sql(paste0("SELECT COALESCE(MAX(value_num), 0) FROM modules_elements_options WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id%",
      " AND category = 'aggregated' AND name = 'display_order'"), .con = r$db)
    last_display_order <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
    
    sql <- glue::glue_sql("INSERT INTO modules_elements_options(id, group_id, study_id, link_id, category, name, value_num, creator_id, datetime, deleted)
            SELECT {last_row + 2}, %group_id%, %study_id%, {last_row + 1}, 'aggregated', 'display_order', {last_display_order + 1}, {r$user_id}, {datetime}, FALSE", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    # Reload data
    plugins_load_%group_id%_%study_id%(r)
    
    # Reset textfield
    shiny.fluent::updateTextField.shinyInput(session, "name_%group_id%_%study_id%", value = NULL)
    
    show_message_bar(output, 1, translate(language, "exclusion_criterion_added", new_words), "success", language)
    
    # Send a message to flowchart's plugin, to reload data
    r$reload_flowchart_%study_id% <- TRUE
    
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language),
    warning = function(w) if (nchar(w[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Warning", error_report = toString(w), language = language))
})

##########################################
# Management datatable                   #
##########################################

col_names <- c("Name", "Display order", "Creator", "Datetime", "Action")
sortable_cols <- c("value", "display_order", "creator_name", "datetime")
centered_cols <- c("datetime", "display_order", "action", "creator_name")
column_widths <- c("datetime" = "180px", "action" = "80px", "display_order" = "100px", "creator_name" = "100px", "name" = "400px")
searchable_cols <- c("value", "datetime", "creator_name")


observeEvent(r$plugins_data_%group_id%_%study_id%, {
  
  tryCatch({
    
    # Get page_id for dropdowns & delete button
    page_id <- id
    
    # Add dropdown for display order & delete buttons
    
    options <- list()
    if (nrow(r$plugins_data_%group_id%_%study_id%) > 0){
      
      options <- convert_tibble_to_list(data = r$plugins_data_%group_id%_%study_id% %>% dplyr::mutate(n = 1:dplyr::n()), key_col = "n", text_col = "n")
      
      # If this is not an update of the dropdowns
      
      if (length(unique(r$plugins_data_%group_id%_%study_id%$display_order)) == nrow(r$plugins_data_%group_id%_%study_id%)){
        r$plugins_data_%group_id%_%study_id% <- r$plugins_data_%group_id%_%study_id% %>% dplyr::arrange(display_order) %>% dplyr::mutate(n = 1:dplyr::n())} 
      else r$plugins_data_%group_id%_%study_id% <- r$plugins_data_%group_id%_%study_id% %>% dplyr::mutate(n = display_order)
      
      data_temp <- r$plugins_data_%group_id%_%study_id% %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          display_order = as.character(
            div(
              shiny.fluent::Dropdown.shinyInput(ns(paste0("dropdown_%group_id%_%study_id%_", id)),
                options = options, value = as.integer(n),
                onclick = paste0("Shiny.setInputValue('", page_id, "-dropdown_updated', '", paste0("dropdown_%group_id%_%study_id%_value_", id), "', {priority: 'event'})"),
                style = "width:120px;"
              )
            )
          ),
          action = as.character(
            div(
              shiny::actionButton(ns(paste0("delete_%group_id%_%study_id%_", id)), "", icon = icon("trash-alt"),
                onclick = paste0("Shiny.setInputValue('", page_id, "-deleted_pressed_%group_id%_%study_id%', this.id, {priority: 'event'})"))
            )
          )
        ) %>%
        dplyr::select(-id, -n)
    }
    else data_temp <- tibble::tibble()
    
    # Render datatable
    render_datatable(
      data = data_temp,
      output = output,
      r = r,
      ns = ns,
      language = language,
      output_name = "datatable_%group_id%_%study_id%",
      col_names = col_names,
      page_length = 20,
      sortable_cols = sortable_cols,
      centered_cols = centered_cols,
      column_widths = column_widths,
      filter = TRUE,
      searchable_cols = searchable_cols
    )
    
    # Make an observer by dropdown
    sapply(r$plugins_data_%group_id%_%study_id% %>% dplyr::pull(id), function(id){
      
      if (id %not_in% r$plugins_dropdowns_observers_%group_id%_%study_id%){
        
        observeEvent(input[[paste0("dropdown_%group_id%_%study_id%_", id)]], {
          
          new_value <- input[[paste0("dropdown_%group_id%_%study_id%_", id)]]
          old_value <- r$plugins_data_%group_id%_%study_id%[[which(r$plugins_data_%group_id%_%study_id%["id"] == id), "display_order"]]
          
          # If value is changed
          if (old_value != new_value){
            
            r$plugins_data_%group_id%_%study_id%[[which(r$plugins_data_%group_id%_%study_id%["id"] == id), "display_order"]] <- new_value
            
            if (new_value > old_value){
              ids <- r$plugins_data_%group_id%_%study_id% %>% dplyr::filter(display_order >= old_value & display_order <= new_value & id != !!id) %>% dplyr::pull(id)
              adding <- -1L
            }
            if (new_value < old_value){
              ids <- r$plugins_data_%group_id%_%study_id% %>% dplyr::filter(display_order >= new_value & display_order <= old_value & id != !!id) %>% dplyr::pull(id)
              adding <- 1L
            } 
            
            # For each row to update, change value in r variable & update dropdown
            sapply(ids, function(id_bis){
              
              display_order <- r$plugins_data_%group_id%_%study_id%[[which(r$plugins_data_%group_id%_%study_id%["id"] == id_bis), "display_order"]]
              r$plugins_data_%group_id%_%study_id%[[which(r$plugins_data_%group_id%_%study_id%["id"] == id_bis), "display_order"]] <- display_order + adding
              shiny.fluent::updateDropdown.shinyInput(session, paste0("dropdown_%group_id%_%study_id%_", id_bis), value = display_order + adding)
            })
          }
          
        }, ignoreInit = TRUE)
        
        r$plugins_dropdowns_observers_%group_id%_%study_id% <- c(r$plugins_dropdowns_observers_%group_id%_%study_id%, id)
      }
    })
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language),
    warning = function(w) if (nchar(w[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Warning", error_report = toString(w), language = language))
})

##########################################
# Change display order                   #
##########################################

observeEvent(input$save_%group_id%_%study_id%, {
  
  tryCatch({
    duplicates_display_order <- r$plugins_data_%group_id%_%study_id% %>% 
      dplyr::group_by(display_order) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
    
    if (duplicates_display_order > 0) show_message_bar(output, 1, "modif_display_order_duplicates", "severeWarning", language)
    
    req(duplicates_display_order == 0)
    
    # Delete data from table
    ids_to_del <- r$plugins_data_%group_id%_%study_id% %>% dplyr::pull(id)
    DBI::dbSendStatement(r$db, paste0("DELETE FROM modules_elements_options WHERE link_id IN (", paste(ids_to_del, collapse = ","), ")")) -> query
    DBI::dbClearResult(query)
    
    # Append data to table
    last_row <- get_last_row(r$db, "modules_elements_options")
    
    new_data <- r$plugins_data_%group_id%_%study_id% %>%
      dplyr::transmute(group_id = %group_id%, study_id = %study_id%, patient_id = NA_integer_, link_id = id, category = "aggregated", name = "display_order", 
        value = NA_character_, value_num = display_order, creator_id = r$user_id, datetime, deleted = FALSE) %>%
      dplyr::mutate(id) %>% dplyr::relocate(id)
    new_data$id <- seq.int(nrow(new_data)) + last_row
    
    DBI::dbAppendTable(r$db, "modules_elements_options", new_data)
    
    # Notification to the user
    show_message_bar(output, 2, "modif_saved", "success", language)
    
    # Send a message to flowchart's plugin, to reload data
    r$reload_flowchart_%study_id% <- TRUE
  }, 
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language),
    warning = function(w) if (nchar(w[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Warning", error_report = toString(w), language = language))
})

##########################################
# Delete a row                           #
##########################################

# Delete button is pressed
observeEvent(input$deleted_pressed_%group_id%_%study_id%, r$delete_dialog_%group_id%_%study_id% <- TRUE)

# Rendering react output
observeEvent(r$delete_dialog_%group_id%_%study_id% , {
  output$delete_confirm_%group_id%_%study_id% <- shiny.fluent::renderReact({
    dialogContentProps <- list(
      type = 0,
      title = translate(language, "delete_exclusion_criterion", new_words),
      closeButtonAriaLabel = "Close",
      subText = translate(language, "delete_exclusion_criterion_subtext", new_words)
    )
    shiny.fluent::Dialog(
      hidden = !r$delete_dialog_%group_id%_%study_id%,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('hide_dialog_%group_id%_%study_id%', Math.random()); }")),
      dialogContentProps = dialogContentProps,
      modalProps = list(),
      shiny.fluent::DialogFooter(
        shiny.fluent::PrimaryButton.shinyInput(ns("delete_confirmed_%group_id%_%study_id%"), text = translate(language, "delete", r$words)),
        shiny.fluent::DefaultButton.shinyInput(ns("delete_canceled_%group_id%_%study_id%"), text = translate(language, "dont_delete", r$words))
      )
    )
  })
})

# Whether to close or not delete dialog box
observeEvent(input$hide_dialog_%group_id%_%study_id%, r$delete_dialog_%group_id%_%study_id% <- FALSE)
observeEvent(input$delete_canceled_%group_id%_%study_id%, r$delete_dialog_%group_id%_%study_id% <- FALSE)

# When the delete is confirmed
observeEvent(input$delete_confirmed_%group_id%_%study_id%, {
  
  tryCatch({
    
    # Get value of deleted row
    row_deleted <- as.integer(substr(input$deleted_pressed_%group_id%_%study_id%, nchar(paste0(id, "-delete_%group_id%_%study_id%_")) + 1, nchar(input$deleted_pressed_%group_id%_%study_id%)))
    
    # Delete row in database
    sql <- glue::glue_sql("UPDATE modules_elements_options SET deleted = TRUE WHERE id = {row_deleted} OR link_id = {row_deleted}", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    # Change display orders, depending on the display order of deleted row
    sql <- glue::glue_sql("SELECT value_num FROM modules_elements_options WHERE link_id = {row_deleted}", .con = r$db)
    display_order_deleted <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
    sql <- glue::glue_sql(paste0("UPDATE modules_elements_options SET value_num = value_num - 1",
      " WHERE deleted IS FALSE AND group_id = %group_id% AND study_id = %study_id% AND category = 'aggregated' AND name = 'display_order' AND value_num > {display_order_deleted}"), .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    # Close dialog box
    r$delete_dialog_%group_id%_%study_id% <- FALSE
    
    # Reload r variable
    plugins_load_%group_id%_%study_id%(r)
    
    # Notification to user
    show_message_bar(output = output, id = 3, translate(language, "exclusion_criterion_deleted", new_words), type ="severeWarning", language = language)
    
    # Send a message to flowchart's plugin, to reload data
    r$reload_flowchart_%study_id% <- TRUE
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language),
    warning = function(w) if (nchar(w[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Warning", error_report = toString(w), language = language))
})