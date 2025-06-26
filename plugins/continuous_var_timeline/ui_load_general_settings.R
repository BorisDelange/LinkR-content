# ==========================================
# ui_load_general_settings.R - Settings Data Loader
# ==========================================

# Database query to retrieve existing general settings for this widget
sql <- glue::glue_sql(
    "SELECT link_id, name, value, value_num 
     FROM widgets_options 
     WHERE widget_id = %widget_id% AND category = 'general_settings'", 
    .con = m$db
)
general_settings <- DBI::dbGetQuery(m$db, sql)

# Initialize toggle values container
toggle_values <- list()

# ====================
# HANDLE FIRST-TIME SETUP (NO EXISTING SETTINGS)
# ====================
if (nrow(general_settings) == 0) {
    # Set default values for all toggle options
    toggle_values$show_settings_file <- TRUE
    toggle_values$figure_and_settings_side_by_side <- TRUE
    toggle_values$run_code_on_data_update <- TRUE
    toggle_values$run_code_at_settings_file_load <- TRUE
    
    # Initialize empty settings file options
    dropdown_options <- list()
    selected_file <- NULL

# ====================
# LOAD EXISTING SETTINGS FROM DATABASE
# ====================
} else if (nrow(general_settings) > 0) {
    
    # Define all available toggle setting names
    general_settings_vec <- c(
        "show_settings_file", 
        "figure_and_settings_side_by_side", 
        "run_code_on_data_update", 
        "run_code_at_settings_file_load"
    )
    
    # Process each toggle setting
    sapply(general_settings_vec, function(name) {
        # Default to TRUE if not found in database
        toggle_value <- TRUE
        
        # Look for this setting in the database results
        row <- general_settings %>% dplyr::filter(name == !!name)
        
        if (nrow(row) > 0) {
            # Handle NULL/NA values (interpret as FALSE)
            if (is.na(row %>% dplyr::pull(value_num))) {
                toggle_value <- FALSE
            } else {
                # Convert numeric value to logical
                toggle_value <- as.logical(row %>% dplyr::pull(value_num))
            }
        }
        
        # Store in global toggle_values list (<<- for parent scope assignment)
        toggle_values[[name]] <<- toggle_value
    })
    
    # ====================
    # LOAD SETTINGS FILES DROPDOWN OPTIONS
    # ====================
    
    # Query available settings files for this widget
    sql <- glue::glue_sql(
        "SELECT id, value AS name 
         FROM widgets_options 
         WHERE widget_id = %widget_id% AND category = 'settings_files' AND name = 'file_name'", 
        .con = m$db
    )
    m$settings_filenames_%widget_id% <- DBI::dbGetQuery(m$db, sql)
    
    # Convert to dropdown format (list of key-text pairs)
    dropdown_options <- convert_tibble_to_list(
        m$settings_filenames_%widget_id%, 
        key_col = "id", 
        text_col = "name"
    )
    
    # Get currently selected settings file ID
    selected_file <- general_settings %>% 
        dplyr::filter(name == "selected_file_id") %>% 
        dplyr::pull(link_id)
}

# ====================
# CALCULATE LAYOUT WIDTH BASED ON SETTINGS
# ====================
# Determine container width based on side-by-side setting
if (toggle_values$figure_and_settings_side_by_side) {
    div_width <- "50%"    # Split screen layout
} else {
    div_width <- "100%"   # Full width layout
}
