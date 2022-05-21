##########################################
# Translations                           #
##########################################

new_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
  "EN", "new_feature_added", "New feature added",
  "FR", "new_feature_added", "Nouveau paramètre ajouté",
  "FR", "choose_feature_type", "Choisissez un type de paramètre",
  "EN", "choose_feature", "Choose a feature",
  "FR", "choose_feature", "Choisissez un paramètre",
  "EN", "new_feature_option_added", "New feature option added",
  "FR", "new_feature_option_added", "Nouveau choix ajouté",
  "EN", "feature", "Feature",
  "FR", "feature", "Paramètre",
  "EN", "feature_type", "Feature type",
  "FR", "feature_type", "Type de paramètre",
  "EN", "dropdown", "Dropdown",
  "FR", "dropdown", "Menu déroulant",
  "EN", "textfield", "Textfield",
  "FR", "textfield", "Texte libre",
  "EN", "toggle", "Toggle",
  "FR", "toggle", "Binaire",
  "EN", "delete_feature", "Delete a feature",
  "FR", "delete_feature", "Supprimer un paramètre",
  "EN", "delete_feature_subtext", "Are you sure you want to delete this feature ?",
  "FR", "delete_feature_subtext", "Etes-vous sûr de vouloir supprimer ce paramètre ?",
  "EN", "feature_deleted", "Feature deleted",
  "FR", "feature_deleted", "Paramètre supprimé",
  "EN", "option", "Option",
  "FR", "option", "Choix"
)

##########################################
# Show / hide divs                       #
##########################################

divs_%group_id%_%study_id% <- c("div_all_features_%group_id%_%study_id%", "div_features_options_%group_id%_%study_id%",
  "div_new_feature_%group_id%_%study_id%", "div_new_feature_option_%group_id%_%study_id%")

observeEvent(input$div_%group_id%_%study_id%, {
  
  sapply(divs_%group_id%_%study_id% %>% setdiff(., input$div_%group_id%_%study_id%), shinyjs::hide)
  shinyjs::show(input$div_%group_id%_%study_id%)
})

##########################################
# Get current features                   #
##########################################

column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px")

tryCatch({
  
  ##########################################
  # Update features datatable              #
  ##########################################
  
  # Get current features
  
  sql <- "SELECT f1.id, f1.value AS feature_name, f2.value AS feature_type, f1.datetime 
            FROM modules_elements_options f1
            LEFT JOIN modules_elements_options f2 ON f1.id = f2.link_id AND f2.category = 'aggregated' AND f2.name = 'feature_type'
            WHERE f1.deleted IS FALSE AND f1.study_id = %study_id% AND f1.category = 'aggregated' AND f1.name = 'feature_name'"
  features_datatable <- DBI::dbGetQuery(r$db, sql) 
  
  if (nrow(features_datatable) > 0) features_datatable <- features_datatable %>% dplyr::rowwise() %>% dplyr::mutate(feature_type = translate(language, feature_type, new_words))
  
  features_col_names <- c(translate(language, "id", r$words), translate(language, "feature", new_words), translate(language, "feature_type", new_words),
    translate(language, "datetime", r$words), translate(language, "action", r$words))
  
  # Add delete button
  # Get page_id for delete button
  page_id <- id
  features_datatable <- features_datatable %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      action = as.character(
        div(
          shiny::actionButton(ns(paste0("delete_%group_id%_%study_id%_", id)), "", icon = icon("trash-alt"),
            onclick = paste0("Shiny.setInputValue('", page_id, "-deleted_pressed_%group_id%_%study_id%', this.id, {priority: 'event'})"))
        )
      )
    )
  
  features_default_tibble <- tibble::tibble(id = integer(), feature_name = character(), feature_type = character(), datetime = character(), action = character())
  
  # Render features datatable
  
  render_datatable(output = output, r = r, ns = ns, language = language, data = features_datatable, output_name = "features_datatable_%group_id%_%study_id%",
    col_names = features_col_names, sortable_cols = c("id", "feature_name", "feature_type"), centered_cols = c("id", "datetime", "action"), 
    column_widths = column_widths, default_tibble = features_default_tibble)
  
  # Create a proxy for features datatable
  
  r$agg_features_datatable_%group_id%_%study_id%_proxy <- DT::dataTableProxy("features_datatable_%group_id%_%study_id%", deferUntilFlush = FALSE)
  
  ##########################################
  # Update features options datatable      #
  ##########################################
  
  # Get current features options
  
  features_options <- tibble::tibble()
  
  if (length(input$features_options_feature_%group_id%_%study_id%) > 0){
    
    sql <- glue::glue_sql("SELECT f1.id, f1.value AS feature_option_name, f1.datetime 
                FROM modules_elements_options f1
                WHERE f1.deleted IS FALSE AND f1.study_id = %study_id% AND f1.category = 'aggregated' AND f1.name = 'feature_option'
                AND f1.link_id = {input$features_options_feature_%group_id%_%study_id%}", .con = r$db)
    features_options <- DBI::dbGetQuery(r$db, sql) 
  }
  
  features_options_col_names <- c(translate(language, "id", r$words), translate(language, "option", new_words), translate(language, "datetime", r$words), translate(language, "action", r$words))
  
  # Add delete button
  # Get page_id for delete button
  page_id <- id
  features_options <- features_options %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      action = as.character(
        div(
          shiny::actionButton(ns(paste0("delete_%group_id%_%study_id%_", id)), "", icon = icon("trash-alt"),
            onclick = paste0("Shiny.setInputValue('", page_id, "-deleted_pressed_%group_id%_%study_id%', this.id, {priority: 'event'})"))
        )
      )
    )
  
  features_options_default_tibble <- tibble::tibble(id = integer(), option = character(), datetime = character(), action = character())
  
  # Render features options datatable
  
  render_datatable(output = output, r = r, ns = ns, language = language, data = features_options, output_name = "features_options_datatable_%group_id%_%study_id%",
    col_names = features_options_col_names, sortable_cols = c("id", "feature_name", "feature_type"), centered_cols = c("id", "datetime", "action"), 
    column_widths = column_widths, default_tibble = features_options_default_tibble)
  
  # Create a proxy for features options datatable
  
  r$agg_features_options_datatable_%group_id%_%study_id%_proxy <- DT::dataTableProxy("features_options_datatable_%group_id%_%study_id%", deferUntilFlush = FALSE)
  
},
  error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))

##########################################
# Update dropdowns & datatables          #
##########################################

# When a feature is chosen to show its options in datatable
observeEvent(input$features_options_feature_%group_id%_%study_id%, r$agg_reload_%group_id%_%study_id% <- Sys.time())

observeEvent(r$agg_reload_%group_id%_%study_id%, {
  
  tryCatch({
    
    ##########################################
    # Update features datatable              #
    ##########################################
    
    sql <- "SELECT f1.id, f1.value AS feature_name, f2.value AS feature_type, f1.datetime 
            FROM modules_elements_options f1
            LEFT JOIN modules_elements_options f2 ON f1.id = f2.link_id AND f2.category = 'aggregated' AND f2.name = 'feature_type'
            WHERE f1.deleted IS FALSE AND f1.study_id = %study_id% AND f1.category = 'aggregated' AND f1.name = 'feature_name'"
    
    features_datatable <- DBI::dbGetQuery(r$db, sql)
    
    if (nrow(features_datatable) > 0){
      features_datatable <- features_datatable %>%
        dplyr::rowwise() %>%
        dplyr::mutate(feature_type = translate(language, feature_type, new_words)) %>%
        dplyr::mutate_at("id", as.integer)  
      
      # Add delete button
      # Get page_id for delete button
      page_id <- id
      
      features_datatable <- features_datatable %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          action = as.character(
            div(
              shiny::actionButton(ns(paste0("delete_%group_id%_%study_id%_", id)), "", icon = icon("trash-alt"),
                onclick = paste0("Shiny.setInputValue('", page_id, "-deleted_pressed_%group_id%_%study_id%', this.id, {priority: 'event'})"))
            )
          )
        )
      
    }
    if (nrow(features_datatable) == 0) features_datatable <- tibble::tibble(id = integer(), feature = character(), feature_type = character(), action = character())
    
    names(features_datatable) <- c(translate(language, "id", r$words), translate(language, "feature", new_words), translate(language, "feature_type", new_words),
      translate(language, "datetime", r$words), translate(language, "action", r$words))
    
    DT::replaceData(r$agg_features_datatable_%group_id%_%study_id%_proxy, features_datatable, resetPaging = FALSE, rownames = FALSE)
    
    ##########################################
    # Update features options datatable      #
    ##########################################
    
    features_options_datatable <- tibble::tibble()
    
    if (length(input$features_options_feature_%group_id%_%study_id%) > 0){
      
      sql <- glue::glue_sql("SELECT f1.id, f1.value AS feature_option_name, f1.datetime 
                FROM modules_elements_options f1
                WHERE f1.deleted IS FALSE AND f1.study_id = %study_id% AND f1.category = 'aggregated' AND f1.name = 'feature_option'
                AND f1.link_id = {input$features_options_feature_%group_id%_%study_id%}", .con = r$db)
      features_options_datatable <- DBI::dbGetQuery(r$db, sql) 
    }
    
    if (nrow(features_options_datatable) > 0){
      features_options_datatable <- features_options_datatable %>% dplyr::mutate_at("id", as.integer)  
      
      # Add delete button
      # Get page_id for delete button
      page_id <- id
      
      features_options_datatable <- features_options_datatable %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          action = as.character(
            div(
              shiny::actionButton(ns(paste0("delete_%group_id%_%study_id%_", id)), "", icon = icon("trash-alt"),
                onclick = paste0("Shiny.setInputValue('", page_id, "-deleted_pressed_%group_id%_%study_id%', this.id, {priority: 'event'})"))
            )
          )
        )
    }
    if (nrow(features_options_datatable) == 0) features_options_datatable <- tibble::tibble(id = integer(), option = character(), datetime = character(), action = character())
    
    names(features_options_datatable) <- c(translate(language, "id", r$words), translate(language, "option", new_words), translate(language, "datetime", r$words), translate(language, "action", r$words))
    
    DT::replaceData(r$agg_features_options_datatable_%group_id%_%study_id%_proxy, features_options_datatable, resetPaging = FALSE, rownames = FALSE)
    
    ##########################################
    # Update dropdowns                       #
    ##########################################
    
    features <- convert_tibble_to_list(DBI::dbGetQuery(r$db, "SELECT o1.id, o1.value FROM modules_elements_options o1 
            INNER JOIN modules_elements_options o2 ON o1.id = o2.link_id AND o2.name = 'feature_type'
            WHERE o1.deleted IS FALSE AND o1.study_id = %study_id% AND o1.name = 'feature_name' AND o2.value = 'dropdown'"), key_col = "id", text_col = "value")
    
    shiny.fluent::updateDropdown.shinyInput(session, "feature_%group_id%_%study_id%", options = features)
    shiny.fluent::updateDropdown.shinyInput(session, "features_options_feature_%group_id%_%study_id%", options = features)
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

##########################################
# Add a new feature                      #
##########################################

observeEvent(input$new_feature_add_%group_id%_%study_id%, {
  
  tryCatch({
    
    new_name <- input$new_feature_name_%group_id%_%study_id%
      
      if (length(new_name) == 0) shiny.fluent::updateTextField.shinyInput(session, "new_feature_name_%group_id%_%study_id%", errorMessage = translate(language, paste0("provide_valid_name"), r$words))
    else shiny.fluent::updateTextField.shinyInput(session, "new_feature_name_%group_id%_%study_id%", errorMessage = NULL)
    
    feature_type_options <- list(
      list(key = "dropdown", text = translate(language, "dropdown", new_words)),
      list(key = "textfield", text = translate(language, "textfield", new_words)),
      list(key = "toggle", text = translate(language, "toggle", new_words)))
    
    new_type <- input$new_feature_type_%group_id%_%study_id%
      
      if (length(new_type) == 0) shiny.fluent::updateDropdown.shinyInput(session, "new_feature_type_%group_id%_%study_id%",
        errorMessage = translate(language, paste0("choose_feature_type"), new_words))
    else shiny.fluent::updateDropdown.shinyInput(session, "new_feature_type_%group_id%_%study_id%", errorMessage = NULL)
    
    req(length(new_name) > 0, length(new_type) > 0)
    
    sql <- "SELECT DISTINCT(value) FROM modules_elements_options WHERE deleted IS FALSE AND category = 'aggregated' AND study_id = %study_id% AND name = 'feature_name'" 
    distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull() %>% tolower()
    
    if (tolower(new_name) %in% distinct_values) show_message_bar(output, 2, "name_already_used", "severeWarning", language)
    req(tolower(new_name) %not_in% distinct_values)
    
    last_row <- get_last_row(r$db, "modules_elements_options")
    sql <- glue::glue_sql("SELECT COALESCE(MAX(value_num), 0) AS display_order FROM modules_elements_options 
            WHERE category = 'aggregated' AND name = 'feature_display_order' AND group_id = %group_id% AND study_id = %study_id%", .con = r$db)
    last_display_order <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(display_order)
    
    # Insert new feature in database
    # One row for the feature, one row for its type
    
    sql <- glue::glue_sql("INSERT INTO modules_elements_options(id, group_id, study_id, link_id, category, name, value, creator_id, datetime, deleted)
            SELECT {last_row + 1}, %group_id%, %study_id%, {NA_integer_}, 'aggregated', 'feature_name', {new_name}, {r$user_id}, {as.character(Sys.time())}, FALSE
      UNION SELECT {last_row + 2}, %group_id%, %study_id%, {last_row + 1}, 'aggregated', 'feature_type', {new_type}, {r$user_id}, {as.character(Sys.time())}, FALSE", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    sql <- glue::glue_sql("INSERT INTO modules_elements_options(id, group_id, study_id, link_id, category, name, value_num, creator_id, datetime, deleted)
            SELECT {last_row + 3}, %group_id%, %study_id%, {last_row + 1}, 'aggregated', 'feature_display_order', {last_display_order + 1}, {r$user_id}, {as.character(Sys.time())}, FALSE", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    # Reset textfield
    shiny.fluent::updateTextField.shinyInput(session, "new_feature_name_%group_id%_%study_id%", value = NULL)
    
    # Update dropdowns
    r$agg_reload_%group_id%_%study_id% <- Sys.time()
    
    show_message_bar(output, 1, translate(language, "new_feature_added", new_words), "success", language)
    
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})

##########################################
# Add a new feature option               #
##########################################

observeEvent(input$new_feature_option_add_%group_id%_%study_id%, {
  
  tryCatch({
    
    new_name <- input$new_feature_option_name_%group_id%_%study_id%
      
      if (length(new_name) == 0) shiny.fluent::updateTextField.shinyInput(session, "new_feature_option_name_%group_id%_%study_id%", errorMessage = translate(language, paste0("provide_valid_name"), r$words))
    else shiny.fluent::updateTextField.shinyInput(session, "new_feature_option_name_%group_id%_%study_id%", errorMessage = NULL)
    
    feature <- input$feature_%group_id%_%study_id%
      
      if (length(feature) == 0) shiny.fluent::updateDropdown.shinyInput(session, "feature_%group_id%_%study_id%",
        errorMessage = translate(language, paste0("choose_feature"), new_words))
    else {
      features <- convert_tibble_to_list(DBI::dbGetQuery(r$db, "SELECT o1.id, o1.value FROM modules_elements_options o1 
                INNER JOIN modules_elements_options o2 ON o1.id = o2.link_id AND o2.category = 'aggregated' AND o2.name = 'feature_type'
                WHERE o1.deleted IS FALSE AND o1.study_id = %study_id% AND o1.category = 'aggregated' AND o1.name = 'feature_name' AND o2.value = 'dropdown'"), key_col = "id", text_col = "value")
      
      shiny.fluent::updateDropdown.shinyInput(session, "feature_%group_id%_%study_id%", errorMessage = NULL, options = features)
    }
    
    req(length(new_name) > 0, length(feature) > 0)
    
    sql <- glue::glue_sql("SELECT DISTINCT(value) FROM modules_elements_options WHERE deleted IS FALSE AND study_id = %study_id% 
            AND link_id = {as.integer(feature)} AND name = 'feature_option'", .con = r$db)
    distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull() %>% tolower()
    
    if (tolower(new_name) %in% distinct_values) show_message_bar(output, 2, "name_already_used", "severeWarning", language)
    req(tolower(new_name) %not_in% distinct_values)
    
    last_row <- get_last_row(r$db, "modules_elements_options")
    
    # Insert new feature in database
    
    sql <- glue::glue_sql("INSERT INTO modules_elements_options(id, group_id, study_id, link_id, category, name, value, creator_id, datetime, deleted)
            SELECT {last_row + 1}, %group_id%, %study_id%, {as.integer(feature)}, 'aggregated', 'feature_option', {new_name}, {r$user_id}, {as.character(Sys.time())}, FALSE", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    # Reset textfield
    shiny.fluent::updateTextField.shinyInput(session, "new_feature_option_name_%group_id%_%study_id%", value = NULL)
    
    show_message_bar(output, 1, translate(language, "new_feature_option_added", new_words), "success", language)
    
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language))
})


##########################################
# Delete a row                           #
##########################################

# Delete button is pressed
observeEvent(input$deleted_pressed_%group_id%_%study_id%, r$agg_delete_dialog_%group_id%_%study_id% <- TRUE)

# Rendering react output
observeEvent(r$agg_delete_dialog_%group_id%_%study_id% , {
  output$delete_confirm_%group_id%_%study_id% <- shiny.fluent::renderReact({
    dialogContentProps <- list(
      type = 0,
      title = translate(language, "delete_feature", new_words),
      closeButtonAriaLabel = "Close",
      subText = translate(language, "delete_feature_subtext", new_words)
    )
    shiny.fluent::Dialog(
      hidden = !r$agg_delete_dialog_%group_id%_%study_id%,
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
observeEvent(input$hide_dialog_%group_id%_%study_id%, r$agg_delete_dialog_%group_id%_%study_id% <- FALSE)
observeEvent(input$delete_canceled_%group_id%_%study_id%, r$agg_delete_dialog_%group_id%_%study_id% <- FALSE)

# When the delete is confirmed
observeEvent(input$delete_confirmed_%group_id%_%study_id%, {
  
  tryCatch({
    
    # Get value of deleted row
    row_deleted <- as.integer(substr(input$deleted_pressed_%group_id%_%study_id%, nchar(paste0(id, "-delete_%group_id%_%study_id%_")) + 1, nchar(input$deleted_pressed_%group_id%_%study_id%)))
    
    # Delete row in database
    sql <- glue::glue_sql("UPDATE modules_elements_options SET deleted = TRUE WHERE id = {row_deleted} OR link_id = {row_deleted}", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
    
    # Close dialog box
    r$agg_delete_dialog_%group_id%_%study_id% <- FALSE
    
    # Notification to user
    show_message_bar(output = output, id = 3, translate(language, "feature_deleted", new_words), type ="severeWarning", language = language)
    
    # Reload data
    r$agg_reload_%group_id%_%study_id% <- Sys.time()
  },
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Error", error_report = toString(e), language = language),
    warning = function(w) if (nchar(w[1]) > 0) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code", 
      error_name = "%group_id% - run server code", category = "Warning", error_report = toString(w), language = language))
})