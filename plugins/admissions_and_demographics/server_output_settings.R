# ==========================================
# server_output_settings.R - Output Configuration Server Logic
# ==========================================

# ======================================
# INPUT CONFIGURATION LIST
# ======================================
all_inputs_%widget_id% <- list(
    list(id = "indicator", type = "dropdown", default = "patient_count"),
    list(id = "indicator_scope", type = "dropdown", default = "hospitalization"),
    list(id = "hospital_unit", type = "multiselect", default = NULL),
    list(id = "legend_1", type = "text", default = i18np$t("hospitalized_patients")),
    list(id = "legend_2", type = "text", default = ""),
    list(id = "auto_update", type = "toggle", default = TRUE),
    list(id = "code", type = "code", default = "")
)

# ======================================
# HOSPITAL UNITS DATA LOADING
# ======================================
# Update hospital unit dropdown with care site data
observe_event(input$indicator_scope_%widget_id%, {
    shiny.fluent::updateDropdown.shinyInput(
        session, 
        "hospital_unit_%widget_id%", 
        options = get_hospital_unit_options_%widget_id%()
    )
})

get_hospital_unit_options_%widget_id% <- function(){
    
    # Load hospital units from care_site table
    care_sites <- NULL
    tryCatch({
        care_sites <-
            d$con %>%
            run_query("
                SELECT DISTINCT
                    c.care_site_id,
                    c.care_site_name
                FROM visit_detail v
                INNER JOIN care_site c ON v.care_site_id = c.care_site_id
                ORDER BY c.care_site_name
            ")
    }, error = function(e) {
        # Handle database connection errors gracefully
        care_sites <<- data.frame(care_site_id = character(0), care_site_name = character(0))
    })
    
    # Prepare dropdown options for hospital units
    hospital_unit_options <- list()
    
    # Add care sites to dropdown options
    if (!is.null(care_sites) && nrow(care_sites) > 0) {
        care_site_options <- lapply(1:nrow(care_sites), function(i) {
            list(
                key = as.character(care_sites$care_site_id[i]), 
                text = care_sites$care_site_name[i]
            )
        })
        hospital_unit_options <- c(hospital_unit_options, care_site_options)
    }
    
    return(hospital_unit_options)
}

# ======================================
# LEGEND VALUES UPDATE LOGIC
# ======================================
# Update legend values based on indicator and scope selection
observe_event(c(input$indicator_%widget_id%, input$indicator_scope_%widget_id%), {
    indicator <- input$indicator_%widget_id%
    indicator_scope <- input$indicator_scope_%widget_id%
    
    if (!is.null(indicator) && !is.null(indicator_scope)) {
        legend_1_value <- ""
        legend_2_value <- ""
        
        # Determine legend values based on indicator and scope combination
        if (indicator == "patient_count") {
            if (indicator_scope == "hospitalization") {
                legend_1_value <- i18np$t("hospitalized_patients")
                legend_2_value <- ""
            } else {  # hospital_units
                legend_1_value <- i18np$t("admitted_patients")
                legend_2_value <- i18np$t("in_selected_units")
            }
        } else if (indicator == "admission_count") {
            if (indicator_scope == "hospitalization") {
                legend_1_value <- i18np$t("admission_count")
                legend_2_value <- ""
            } else {  # hospital_units
                legend_1_value <- i18np$t("stays")
                legend_2_value <- i18np$t("in_selected_units")
            }
        } else if (indicator == "mortality") {
            if (indicator_scope == "hospitalization") {
                legend_1_value <- i18np$t("mortality")
                legend_2_value <- ""
            } else {  # hospital_units
                legend_1_value <- i18np$t("deaths")
                legend_2_value <- i18np$t("in_selected_units")
            }
        } else if (indicator == "average_length_of_stay") {
            if (indicator_scope == "hospitalization") {
                legend_1_value <- i18np$t("average_length_of_stay")
                legend_2_value <- ""
            } else {  # hospital_units
                legend_1_value <- i18np$t("average_length_of_stay")
                legend_2_value <- i18np$t("in_selected_units")
            }
        }
        
        # Update text field values
        shiny.fluent::updateTextField.shinyInput(
            session, 
            "legend_1_%widget_id%", 
            value = legend_1_value
        )
        
        shiny.fluent::updateTextField.shinyInput(
            session, 
            "legend_2_%widget_id%", 
            value = legend_2_value
        )
    }
})

# ======================================
# CONDITIONAL UI DISPLAY LOGIC
# ======================================
# Show/hide hospital unit dropdown based on indicator scope selection
observe_event(input$indicator_scope_%widget_id%, {
    indicator_scope <- input$indicator_scope_%widget_id%
    
    if (!is.null(indicator_scope)) {
        if (indicator_scope == "hospital_units") {
            # Show hospital unit selection when scope is hospital units
            shinyjs::show("hospital_unit_div_%widget_id%")
        } else {
            # Hide hospital unit selection when scope is hospitalization (global)
            shinyjs::hide("hospital_unit_div_%widget_id%")
        }
    }
})

# ======================================
# HOSPITAL UNIT CHECK/UNCHECK BUTTONS
# ======================================
# Select all hospital units button
observe_event(input$hospital_unit_check_all_%widget_id%, {
    # Get all available unit IDs
    hospital_unit_options <- get_hospital_unit_options_%widget_id%()
    all_unit_ids <- sapply(hospital_unit_options, function(x) x$key)
    shiny.fluent::updateDropdown.shinyInput(session, "hospital_unit_%widget_id%", options = hospital_unit_options, value = all_unit_ids)
})

# Clear all hospital units button
observe_event(input$hospital_unit_uncheck_all_%widget_id%, {
    hospital_unit_options <- get_hospital_unit_options_%widget_id%()
    shiny.fluent::updateDropdown.shinyInput(session, "hospital_unit_%widget_id%", options = hospital_unit_options, value = character(0))
})
