# Load general settings

sql <- glue::glue_sql("SELECT name, value, value_num FROM widgets_options WHERE widget_id = %widget_id% AND category = 'general_settings'", .con = m$db)
general_settings <- DBI::dbGetQuery(m$db, sql)

toggle_values <- list()

if (nrow(general_settings) == 0){

    toggle_values$show_saved_file <- TRUE
    toggle_values$figure_and_settings_side_by_side <- TRUE
    toggle_values$run_code_at_patient_update <- TRUE
    
    dropdown_options <- list()
    selected_file <- NULL
    
} else if (nrow(general_settings) > 0){

    # Toggles values
    
    for (name in c("show_saved_file", "figure_and_settings_side_by_side", "run_code_at_patient_update")){
    
        toggle_value <- general_settings %>% dplyr::filter(name == !!name) %>% dplyr::pull(value_num)
        if (is.na(toggle_value)) toggle_value <- FALSE
        else (toggle_value <- as.logical(toggle_value))
        toggle_values[[name]] <- toggle_value
    }
    
    # Selected saved settings file
    sql <- glue::glue_sql("SELECT id, value AS name FROM widgets_options WHERE widget_id = %widget_id% AND category = 'saved_settings' AND name = 'file_name'", .con = m$db)
    m$settings_filenames_%widget_id% <- DBI::dbGetQuery(m$db, sql)
    dropdown_options <- convert_tibble_to_list(m$settings_filenames_%widget_id%, key_col = "id", text_col = "name")
    selected_file <- general_settings %>% dplyr::filter(name == "selected_file_id") %>% dplyr::pull(value_num)
}

if (toggle_values$figure_and_settings_side_by_side) div_width <- "50%" else div_width <- "100%"
