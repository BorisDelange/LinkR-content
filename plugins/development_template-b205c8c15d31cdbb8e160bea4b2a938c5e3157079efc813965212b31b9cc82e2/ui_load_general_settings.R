# UI - Load general settings
#
# Insert the UI components for configuring the figure settings here.

sql <- glue::glue_sql("SELECT link_id, name, value, value_num FROM widgets_options WHERE widget_id = %widget_id% AND category = 'general_settings'", .con = m$db)
general_settings <- DBI::dbGetQuery(m$db, sql)

toggle_values <- list()

if (nrow(general_settings) == 0){

    toggle_values$show_settings_file <- TRUE
    toggle_values$figure_and_settings_side_by_side <- TRUE
    
    dropdown_options <- list()
    selected_file <- NULL
    
} else if (nrow(general_settings) > 0){

    # Toggles values
    
    sapply(c("show_settings_file", "figure_and_settings_side_by_side"), function(name){
        
        toggle_value <- FALSE
        
        row <- general_settings %>% dplyr::filter(name == !!name)
        if (nrow(row) > 0){
            if (is.na(row %>% dplyr::pull(value_num))) toggle_value <- FALSE
            else (toggle_value <- as.logical(row %>% dplyr::pull(value_num)))
        }
        
        toggle_values[[name]] <<- toggle_value
    })
    
    # Selected settings file
    
    sql <- glue::glue_sql("SELECT id, value AS name FROM widgets_options WHERE widget_id = %widget_id% AND category = 'settings_files' AND name = 'file_name'", .con = m$db)
    m$settings_filenames_%widget_id% <- DBI::dbGetQuery(m$db, sql)
    dropdown_options <- convert_tibble_to_list(m$settings_filenames_%widget_id%, key_col = "id", text_col = "name")
    selected_file <- general_settings %>% dplyr::filter(name == "selected_file_id") %>% dplyr::pull(link_id)
}

if (toggle_values$figure_and_settings_side_by_side) div_width <- "50%" else div_width <- "100%"
