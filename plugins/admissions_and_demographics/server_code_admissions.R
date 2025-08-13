# ==========================================
# server_code_admissions.R
# ==========================================

# Generate HTML code for healthcare indicators using OMOP data
generate_admissions_code_%widget_id% <- function(indicator = "patient_count", indicator_scope = "hospitalization", hospital_units = c(), legend_1 = "", legend_2 = "") {
    
    code_lines <- c()
    
    # Define reusable CSS styles
    card_container_style <- "text-align: center; height: 100%; display: flex; align-items: center; flex-direction: column; justify-content: center;"
    icon_style <- "margin-bottom: 10px;"
    value_style <- "font-size: 36px; font-weight: bold; margin: 10px 0;"
    label_style <- "color: #666; font-size: 14px; text-transform: uppercase;"
    tooltip_style <- "visibility: hidden; opacity: 0; position: fixed; background-color: rgba(0,0,0,0.9); color: white; padding: 8px 12px; border-radius: 4px; font-size: 12px; z-index: 9999; transition: opacity 0.3s; min-width: 200px; max-width: 300px; text-align: left; box-shadow: 0 4px 12px rgba(0,0,0,0.4); line-height: 1.3; pointer-events: none;"
    
    # Define color schemes for different indicators
    patient_color <- "#2C699A"
    admission_color <- "#1e7e34"
    mortality_color <- "#dc3545"
    length_stay_color <- "#fd7e14"
    
    # Define tooltip variables for direct integration
    tooltip_class_name <- paste0("custom-tooltip-%widget_id%")
    tooltip_class_name_units <- paste0("custom-tooltip-units-%widget_id%")
    
    # JavaScript for dynamic positioning
    generate_tooltip_js <- function(tooltip_selector, parent_element = FALSE) {
        element_ref <- if(parent_element) "this.parentElement" else "this"
        paste0("
                    var tooltip = ", element_ref, ".querySelector('", tooltip_selector, "');
                    var rect = this.getBoundingClientRect();
                    var viewportWidth = window.innerWidth;
                    var viewportHeight = window.innerHeight;
                    
                    var left = rect.left + (rect.width / 2) - (200 / 2);
                    var top = rect.bottom + 8;
                    
                    if (left + 200 > viewportWidth - 10) {
                        left = viewportWidth - 210;
                    }
                    if (left < 10) {
                        left = 10;
                    }
                    
                    if (top + 100 > viewportHeight - 10) {
                        top = rect.top - 100 - 8;
                    }
                    
                    tooltip.style.left = left + 'px';
                    tooltip.style.top = top + 'px';
                    tooltip.style.visibility = 'visible';
                    tooltip.style.opacity = '1';
        ")
    }
    
    generate_tooltip_hide_js <- function(tooltip_selector, parent_element = FALSE) {
        element_ref <- if(parent_element) "this.parentElement" else "this"
        paste0(element_ref, ".querySelector('", tooltip_selector, "').style.visibility = 'hidden'; ", element_ref, ".querySelector('", tooltip_selector, "').style.opacity = '0';")
    }
    
    tooltip_js_show_value <- generate_tooltip_js(paste0(".", tooltip_class_name))
    tooltip_js_hide_value <- generate_tooltip_hide_js(paste0(".", tooltip_class_name))
    tooltip_js_show_units <- generate_tooltip_js(paste0(".", tooltip_class_name_units), TRUE)
    tooltip_js_hide_units <- generate_tooltip_hide_js(paste0(".", tooltip_class_name_units), TRUE)
    
    # Extract unit IDs and names
    if (length(hospital_units) > 0) {
        if (is.list(hospital_units) && length(hospital_units) > 0) {
            if (!is.null(names(hospital_units[[1]])) && "key" %in% names(hospital_units[[1]])) {
                unit_ids <- sapply(hospital_units, function(x) x$key)
                unit_names <- sapply(hospital_units, function(x) x$text)
            } else {
                unit_ids <- unlist(hospital_units)
                unit_names <- unlist(hospital_units)
            }
        } else {
            unit_ids <- hospital_units
            unit_names <- hospital_units
        }
        
        unit_ids <- as.character(unit_ids)
        unit_names <- as.character(unit_names)
    } else {
        unit_ids <- c()
        unit_names <- c()
    }
    
    # Get care site names for selected units when scope is hospital units
    if (indicator_scope == "hospital_units" && length(unit_ids) > 0) {
        code_lines <- c(code_lines,
            "# Get care site names for selected units",
            "care_sites <-",
            "    d$care_site %>%",
            paste0("    dplyr::filter(care_site_id %in% c(", paste(unit_ids, collapse = ", "), ")) %>%"),
            "    dplyr::distinct(care_site_id, care_site_name) %>%",
            "    dplyr::arrange(care_site_name) %>%",
            "    dplyr::collect()",
            "",
            "# Create unit names list for tooltip",
            "if (nrow(care_sites) > 0) {",
            "    unit_names_for_tooltip <- paste0(\"- \", care_sites$care_site_name, collapse = \"<br>\")",
            "} else {",
            "    unit_names_for_tooltip <- \"\"",
            "}",
            ""
        )
    } else if (indicator_scope == "hospital_units") {
        code_lines <- c(code_lines,
            "# Validation: Check if units are selected when scope is hospital units",
            'error_msg <- i18np$t("please_select_at_least_one_hospital_unit")',
            'stop(error_msg)',
            ""
        )
    }
    
    if (indicator == "patient_count") {
        if (indicator_scope == "hospitalization") {
            code_lines <- c(code_lines,
                "# Extract distinct patient count from all hospital visits",
                "patients <-",
                "    d$data_subset$visit_occurrence %>%",
                "    dplyr::distinct(person_id) %>%",
                "    dplyr::collect()",
                "",
                "value <- nrow(patients)",
                "",
                "# Generate HTML card result",
                "res <- div()",
                "",
                "if (value > 0) {",
                generate_admissions_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"hospitalized_patients\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", patient_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-hospital-user fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", patient_color, "; ", value_style, "\","),
                "            value",
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, "\","),
                "            do.call(div, legend_div_content)",
                "        )",
                "    )",
                "}",
                ""
            )
        } else {
            code_lines <- c(code_lines,
                "# Extract distinct patient count from selected hospital units",
                "patients <-",
                "    d$data_subset$visit_detail %>%",
                paste0("    dplyr::filter(care_site_id %in% c(", paste(unit_ids, collapse = ", "), ")) %>%"),
                "    dplyr::distinct(person_id) %>%",
                "    dplyr::collect()",
                "",
                "value <- nrow(patients)",
                "",
                "# Generate HTML card result with unit info",
                "res <- div()",
                "",
                "if (value > 0) {",
                generate_admissions_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"patients\")", "i18np$t(\"in_selected_units\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", patient_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-hospital-user fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", patient_color, "; ", value_style, "\","),
                "            value",
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, "\","),
                "            div(",
                "                do.call(div, legend_div_content),",
                paste0("                div(class = \"", tooltip_class_name, "\", div("),
                "                    paste0(i18np$t(\"hospital_units\"), \" :\"),",
                "                    div(",
                "                        HTML(unit_names_for_tooltip),",
                "                        style = \"margin-top: 10px;\"",
                "                    )",
                paste0("                ), style = \"", tooltip_style, "\"),"),
                "                style = \"position: relative; cursor: pointer;\",",
                paste0("                onmouseover = \"", tooltip_js_show_value, "        \","),
                paste0("                onmouseout = \"", tooltip_js_hide_value, "\""),
                "            )",
                "        )",
                "    )",
                "}",
                ""
            )
        }
        
    } else if (indicator == "admission_count") {
        if (indicator_scope == "hospitalization") {
            code_lines <- c(code_lines,
                "# Extract hospital admissions count",
                "admissions <-",
                "    d$data_subset$visit_occurrence %>%",
                "    dplyr::collect()",
                "",
                "value <- nrow(admissions)",
                "",
                "# Generate HTML card result",
                "res <- div()",
                "",
                "if (value > 0) {",
                generate_admissions_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"admission_count\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", admission_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-bed fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", admission_color, "; ", value_style, "\","),
                "            value",
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, "\","),
                "            do.call(div, legend_div_content)",
                "        )",
                "    )",
                "}",
                ""
            )
        } else {
            code_lines <- c(code_lines,
                "# Extract hospital unit stays count",
                "admissions <-",
                "    d$data_subset$visit_detail %>%",
                paste0("    dplyr::filter(care_site_id %in% c(", paste(unit_ids, collapse = ", "), ")) %>%"),
                "    dplyr::collect()",
                "",
                "value <- nrow(admissions)",
                "",
                "# Generate HTML card result with unit info",
                "res <- div()",
                "",
                "if (value > 0) {",
                generate_admissions_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"stays\")", "i18np$t(\"in_selected_units\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", admission_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-bed fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", admission_color, "; ", value_style, "\","),
                "            value",
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, "\","),
                "            div(",
                "                do.call(div, legend_div_content),",
                paste0("                div(class = \"", tooltip_class_name, "\", div("),
                "                    paste0(i18np$t(\"hospital_units\"), \" :\"),",
                "                    div(",
                "                        HTML(unit_names_for_tooltip),",
                "                        style = \"margin-top: 10px;\"",
                "                    )",
                paste0("                ), style = \"", tooltip_style, "\"),"),
                "                style = \"position: relative; cursor: pointer;\",",
                paste0("                onmouseover = \"", tooltip_js_show_value, "        \","),
                paste0("                onmouseout = \"", tooltip_js_hide_value, "\""),
                "            )",
                "        )",
                "    )",
                "}",
                ""
            )
        }
        
    } else if (indicator == "mortality_rate") {
        if (indicator_scope == "hospitalization") {
            code_lines <- c(code_lines,
                "# Extract hospital mortality during hospitalization",
                "deaths <-",
                "    d$data_subset$death %>%",
                "    dplyr::inner_join(d$data_subset$visit_occurrence, by = \"person_id\") %>%",
                "    dplyr::filter(",
                "        death_datetime >= visit_start_datetime,",
                "        is.na(visit_end_datetime) | death_datetime <= visit_end_datetime",
                "    ) %>%",
                "    dplyr::distinct(person_id, death_datetime) %>%",
                "    dplyr::collect()",
                "",
                "total_patients <-",
                "    d$data_subset$visit_occurrence %>%",
                "    dplyr::distinct(person_id) %>%",
                "    dplyr::collect()",
                "",
                "death_count <- nrow(deaths)",
                "total_count <- nrow(total_patients)",
                "value <- if(total_count > 0) round((death_count / total_count) * 100, 1) else 0",
                "",
                "# Generate HTML card result",
                "res <- div()",
                "",
                "if (total_count > 0) {",
                generate_admissions_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"mortality_rate\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", mortality_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-heart-broken fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", mortality_color, "; ", value_style, " position: relative; cursor: pointer;\","),
                "            paste0(value, \"%\"),",
                paste0("            onmouseover = \"", tooltip_js_show_value, "        \","),
                paste0("            onmouseout = \"", tooltip_js_hide_value, "\","),
                paste0("            div(class = \"", tooltip_class_name, "\", paste0(death_count, \" \", i18np$t(\"deaths_on_total_patients\"), \" \", total_count, \" \", tolower(i18np$t(\"patients\"))), style = \"", tooltip_style, "\")"),
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, "\","),
                "            do.call(div, legend_div_content)",
                "        )",
                "    )",
                "}",
                ""
            )
        } else {
            code_lines <- c(code_lines,
                "# Extract unit-specific mortality during hospitalization",
                "deaths <-",
                "    d$data_subset$death %>%",
                "    dplyr::inner_join(d$data_subset$visit_detail, by = \"person_id\") %>%",
                "    dplyr::filter(",
                paste0("        care_site_id %in% c(", paste(unit_ids, collapse = ", "), "),"),
                "        death_datetime >= visit_detail_start_datetime,",
                "        is.na(visit_detail_end_datetime) | death_datetime <= visit_detail_end_datetime",
                "    ) %>%",
                "    dplyr::distinct(person_id, death_datetime, visit_detail_id) %>%",
                "    dplyr::collect()",
                "",
                "total_patients <-",
                "    d$data_subset$visit_detail %>%",
                paste0("    dplyr::filter(care_site_id %in% c(", paste(unit_ids, collapse = ", "), ")) %>%"),
                "    dplyr::distinct(person_id) %>%",
                "    dplyr::collect()",
                "",
                "death_count <- nrow(deaths)",
                "total_count <- nrow(total_patients)",
                "value <- if(total_count > 0) round((death_count / total_count) * 100, 1) else 0",
                "",
                "# Generate HTML card result with unit info",
                "res <- div()",
                "",
                "if (total_count > 0) {",
                generate_admissions_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"deaths\")", "i18np$t(\"in_selected_units\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", mortality_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-heart-broken fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", mortality_color, "; ", value_style, " position: relative; cursor: pointer;\","),
                "            paste0(value, \"%\"),",
                paste0("            onmouseover = \"", tooltip_js_show_value, "\","),
                paste0("            onmouseout = \"", tooltip_js_hide_value, "\","),
                paste0("            div(class = \"", tooltip_class_name, "\", paste0(death_count, \" \", i18np$t(\"deaths_on_total_patients\"), \" \", total_count, \" \", tolower(i18np$t(\"patients\"))), tags$br(), tags$br(), paste0(i18np$t(\"deaths_during_hospitalization\")), style = \"", tooltip_style, "\")"),
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, " position: relative;\","),
                "            div(",
                "                tagList(legend_div_content),",
                "                style = \"cursor: pointer;\",",
                paste0("                onmouseover = \"", tooltip_js_show_units, "\","),
                paste0("                onmouseout = \"", tooltip_js_hide_units, "\""),
                "            ),",
                paste0("            div(class = \"", tooltip_class_name_units, "\", div("),
                "                paste0(i18np$t(\"hospital_units\"), \" :\"),",
                "                div(",
                "                    HTML(unit_names_for_tooltip),",
                "                    style = \"margin-top: 10px;\"",
                "                )",
                paste0("            ), style = \"", tooltip_style, "\")"),
                "        )",
                "    )",
                "}",
                ""
            )
        }
        
    } else if (indicator == "average_length_of_stay") {
        if (indicator_scope == "hospitalization") {
            code_lines <- c(code_lines,
                "# Extract hospital length of stay data",
                "stays <-",
                "    d$data_subset$visit_occurrence %>%",
                "    dplyr::filter(!is.na(visit_end_date)) %>%",
                "    dplyr::collect() %>%",
                "    dplyr::mutate(length_of_stay = as.numeric(difftime(visit_end_date, visit_start_date, units = \"days\")))",
                "",
                "# Calculate average length of stay",
                "value <- if(nrow(stays) > 0) round(mean(stays$length_of_stay, na.rm = TRUE), 1) else 0",
                "",
                "# Generate HTML card result",
                "res <- div()",
                "",
                "if (value > 0) {",
                generate_admissions_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"average_length_of_stay\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", length_stay_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-calendar-day fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", length_stay_color, "; ", value_style, "\","),
                "            paste0(value, \" \"), tags$span(i18np$t(\"days\"), style = \"font-size: 18px;\")",
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, "\","),
                "            do.call(div, legend_div_content)",
                "        )",
                "    )",
                "}",
                ""
            )
        } else {
            code_lines <- c(code_lines,
                "# Extract unit length of stay data",
                "stays <-",
                "    d$data_subset$visit_detail %>%",
                "    dplyr::filter(",
                paste0("        care_site_id %in% c(", paste(unit_ids, collapse = ", "), "),"),
                "               !is.na(visit_detail_end_date)",
                "    ) %>%",
                "    dplyr::collect() %>%",
                "    dplyr::mutate(length_of_stay = as.numeric(difftime(visit_detail_end_date, visit_detail_start_date, units = \"days\")))",
                "",
                "# Calculate average length of stay",
                "value <- if(nrow(stays) > 0) round(mean(stays$length_of_stay, na.rm = TRUE), 1) else 0",
                "",
                "# Generate HTML card result with unit info",
                "res <- div()",
                "",
                "if (value > 0) {",
                generate_admissions_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"average_length_of_stay\")", "i18np$t(\"in_selected_units\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", length_stay_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-calendar-day fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", length_stay_color, "; ", value_style, "\","),
                "            paste0(value, \" \"), tags$span(i18np$t(\"days\"), style = \"font-size: 18px;\")",
                "        ),",
                "        div(",
                paste0("            style = \"", label_style, "\","),
                "            div(",
                "                do.call(div, legend_div_content),",
                paste0("                div(class = \"", tooltip_class_name, "\", div("),
                "                    paste0(i18np$t(\"hospital_units\"), \" :\"),",
                "                    div(",
                "                        HTML(unit_names_for_tooltip),",
                "                        style = \"margin-top: 10px;\"",
                "                    )",
                paste0("                ), style = \"", tooltip_style, "\"),"),
                "                style = \"position: relative; cursor: pointer;\",",
                paste0("                onmouseover = \"", tooltip_js_show_value, "        \","),
                paste0("                onmouseout = \"", tooltip_js_hide_value, "\""),
                "            )",
                "        )",
                "    )",
                "}",
                ""
            )
        }
    }
    
    code_lines <- c(code_lines, "res")
    
    generated_code <- paste(code_lines, collapse = "\n")
    
    return(generated_code)
}

# Helper function to generate legend content based on text fields
generate_admissions_legend_code_%widget_id% <- function(legend_1, legend_2, default_legend_1, default_legend_2 = NULL) {
    legend_lines <- c()
    
    legend_lines <- c(legend_lines,
        "    # Determine legend content based on text field values",
        paste0("    legend_1_text <- \"", legend_1, "\""),
        paste0("    legend_2_text <- \"", legend_2, "\""),
        "    ",
        "    # Build legend display",
        "    legend_div_content <- tagList()",
        "    if (nchar(legend_1_text) > 0) {",
        "        legend_div_content <- tagList(legend_div_content, legend_1_text)",
        "        if (nchar(legend_2_text) > 0) {",
        "            legend_div_content <- tagList(legend_div_content, tags$br(), legend_2_text)",
        "        }",
        "    } else if (nchar(legend_2_text) > 0) {",
        "        legend_div_content <- tagList(legend_div_content, legend_2_text)",
        "    }",
        "    ",
        "    # Use default text if both legends are empty",
        "    if (length(legend_div_content) == 0) {"
    )
    
    if (!is.null(default_legend_2)) {
        legend_lines <- c(legend_lines,
            paste0("        legend_div_content <- tagList(", default_legend_1, ", tags$br(), ", default_legend_2, ")"))
    } else {
        legend_lines <- c(legend_lines,
            paste0("        legend_div_content <- tagList(", default_legend_1, ")"))
    }
    
    legend_lines <- c(legend_lines,
        "    }",
        "    "
    )
    
    return(legend_lines)
}

# Generate Plotly histogram code for admission trends using OMOP data
# Generate Plotly histogram code for admission trends using OMOP data
generate_timeline_code_%widget_id% <- function(indicator = "admission_timeline", indicator_scope = "hospitalization", hospital_units = c(), title = "", x_label = "", y_label = "", nb_bins = "") {
    
    code_lines <- c()
    
    # Extract unit IDs when scope is hospital units
    if (length(hospital_units) > 0) {
        if (is.list(hospital_units) && length(hospital_units) > 0) {
            if (!is.null(names(hospital_units[[1]])) && "key" %in% names(hospital_units[[1]])) {
                unit_ids <- sapply(hospital_units, function(x) x$key)
                unit_names <- sapply(hospital_units, function(x) x$text)
            } else {
                unit_ids <- unlist(hospital_units)
                unit_names <- unlist(hospital_units)
            }
        } else {
            unit_ids <- hospital_units
            unit_names <- hospital_units
        }
        
        unit_ids <- as.character(unit_ids)
        unit_names <- as.character(unit_names)
    } else {
        unit_ids <- c()
        unit_names <- c()
    }
    
    # Validation for hospital units scope
    if (indicator_scope == "hospital_units" && length(unit_ids) == 0) {
        code_lines <- c(code_lines,
            "# Validation: Check if units are selected when scope is hospital units",
            'error_msg <- i18np$t("please_select_at_least_one_hospital_unit")',
            'stop(error_msg)',
            ""
        )
        generated_code <- paste(code_lines, collapse = "\n")
        return(generated_code)
    }
    
    # Set default labels if empty
    code_lines <- c(code_lines,
        "# Set timeline parameters",
        paste0("plot_title <- \"", title, "\""),
        paste0("x_axis_label <- \"", x_label, "\""),
        paste0("y_axis_label <- \"", y_label, "\""),
        paste0("nb_bins <- \"", nb_bins, "\""),
        "",
        "# Use default labels if empty",
        "if (nchar(plot_title) == 0) {",
        "    plot_title <- i18np$t(\"admission_timeline_title\")",
        "}",
        "if (nchar(x_axis_label) == 0) {",
        "    x_axis_label <- i18np$t(\"timeline_x_axis_default\")",
        "}",
        "if (nchar(y_axis_label) == 0) {",
        "    y_axis_label <- i18np$t(\"timeline_y_axis_default\")",
        "}",
        "if (nchar(nb_bins) == 0 || !is.numeric(as.numeric(nb_bins))) {",
        "    nb_bins <- 12  # Default to 12 bins (monthly)",
        "} else {",
        "    nb_bins <- as.numeric(nb_bins)",
        "}",
        ""
    )
    
    # Generate data extraction code based on scope
    if (indicator_scope == "hospitalization") {
        code_lines <- c(code_lines,
            "# Extract hospital admissions data for histogram (all hospitalizations)",
            "timeline_data <-",
            "    d$data_subset$visit_occurrence %>%",
            "    dplyr::select(visit_start_date) %>%",
            "    dplyr::filter(!is.na(visit_start_date)) %>%",
            "    dplyr::collect() %>%",
            "    dplyr::mutate(",
            "        admission_date = as.Date(visit_start_date)",
            "    )",
            "",
            "# Create time bins based on data range and nb_bins parameter",
            "if (nrow(timeline_data) > 0) {",
            "    date_range <- range(timeline_data$admission_date)",
            "    date_breaks <- seq(from = date_range[1], to = date_range[2], length.out = nb_bins + 1)",
            "    ",
            "    # Assign each date to a bin",
            "    timeline_data <- timeline_data %>%",
            "        dplyr::mutate(",
            "            bin_start = cut(admission_date, breaks = date_breaks, include.lowest = TRUE, labels = FALSE),",
            "            bin_start_date = date_breaks[bin_start],",
            "            bin_end_date = date_breaks[bin_start + 1],",
            "            bin_label = paste0(format(bin_start_date, \"%b %Y\"), \" - \", format(bin_end_date, \"%b %Y\"))",
            "        ) %>%",
            "        dplyr::filter(!is.na(bin_start))",
            "    ",
            "    # Aggregate by bins",
            "    histogram_data <- timeline_data %>%",
            "        dplyr::group_by(bin_label, bin_start_date) %>%",
            "        dplyr::summarise(",
            "            admission_count = dplyr::n(),",
            "            .groups = \"drop\"",
            "        ) %>%",
            "        dplyr::arrange(bin_start_date)",
            "} else {",
            "    histogram_data <- data.frame(",
            "        bin_label = character(0),",
            "        bin_start_date = as.Date(character(0)),",
            "        admission_count = numeric(0)",
            "    )",
            "}",
            ""
        )
    } else {
        # Get care site names for selected units
        code_lines <- c(code_lines,
            "# Get care site names for selected units",
            "care_sites <-",
            "    d$care_site %>%",
            paste0("    dplyr::filter(care_site_id %in% c(", paste(unit_ids, collapse = ", "), ")) %>%"),
            "    dplyr::distinct(care_site_id, care_site_name) %>%",
            "    dplyr::arrange(care_site_name) %>%",
            "    dplyr::collect()",
            "",
            "# Extract hospital unit admissions data for histogram",
            "timeline_data <-",
            "    d$data_subset$visit_detail %>%",
            paste0("    dplyr::filter(care_site_id %in% c(", paste(unit_ids, collapse = ", "), ")) %>%"),
            "    dplyr::select(visit_detail_start_date, care_site_id) %>%",
            "    dplyr::filter(!is.na(visit_detail_start_date)) %>%",
            "    dplyr::left_join(care_sites, by = \"care_site_id\") %>%",
            "    dplyr::collect() %>%",
            "    dplyr::mutate(",
            "        admission_date = as.Date(visit_detail_start_date)",
            "    )",
            "",
            "# Create time bins based on data range and nb_bins parameter",
            "if (nrow(timeline_data) > 0) {",
            "    date_range <- range(timeline_data$admission_date)",
            "    date_breaks <- seq(from = date_range[1], to = date_range[2], length.out = nb_bins + 1)",
            "    ",
            "    # Assign each date to a bin",
            "    timeline_data <- timeline_data %>%",
            "        dplyr::mutate(",
            "            bin_start = cut(admission_date, breaks = date_breaks, include.lowest = TRUE, labels = FALSE),",
            "            bin_start_date = date_breaks[bin_start],",
            "            bin_end_date = date_breaks[bin_start + 1],",
            "            bin_label = paste0(format(bin_start_date, \"%b %Y\"), \" - \", format(bin_end_date, \"%b %Y\"))",
            "        ) %>%",
            "        dplyr::filter(!is.na(bin_start))",
            "    ",
            "    # Aggregate by bins and care sites",
            "    histogram_data <- timeline_data %>%",
            "        dplyr::group_by(bin_label, bin_start_date, care_site_name) %>%",
            "        dplyr::summarise(",
            "            admission_count = dplyr::n(),",
            "            .groups = \"drop\"",
            "        ) %>%",
            "        dplyr::arrange(bin_start_date, care_site_name)",
            "    ",
            "    # Create total by bin for overall height",
            "    histogram_total <- histogram_data %>%",
            "        dplyr::group_by(bin_label, bin_start_date) %>%",
            "        dplyr::summarise(",
            "            total_admissions = sum(admission_count),",
            "            detail_text = paste0(",
            "                care_site_name, \": \", admission_count,",
            "                collapse = \"<br>\"",
            "            ),",
            "            .groups = \"drop\"",
            "        ) %>%",
            "        dplyr::arrange(bin_start_date)",
            "} else {",
            "    histogram_data <- data.frame(",
            "        bin_label = character(0),",
            "        bin_start_date = as.Date(character(0)),",
            "        care_site_name = character(0),",
            "        admission_count = numeric(0)",
            "    )",
            "    histogram_total <- data.frame(",
            "        bin_label = character(0),",
            "        bin_start_date = as.Date(character(0)),",
            "        total_admissions = numeric(0),",
            "        detail_text = character(0)",
            "    )",
            "}",
            ""
        )
    }
    
    # Generate Plotly histogram code
    code_lines <- c(code_lines,
        "# Create interactive histogram plot",
        "if (nrow(histogram_data) > 0) {"
    )
    
    if (indicator_scope == "hospitalization") {
        code_lines <- c(code_lines,
            "    # Single histogram for overall hospitalizations",
            "    timeline_plot <- plotly::plot_ly(",
            "        data = histogram_data,",
            "        x = ~bin_label,",
            "        y = ~admission_count,",
            "        type = 'bar',",
            "        marker = list(color = '#2C699A', opacity = 0.8),",
            "        hovertemplate = paste0(",
            "            '<b>%{x}</b><br>',",
            "            paste0(i18np$t(\"admission_count\"), \" : %{y}\"),",
            "            '<extra></extra>'",
            "        )",
            "    ) %>%",
            "    plotly::layout(",
            "        title = list(",
            "            text = plot_title,",
            "            font = list(size = 16, color = '#2C699A')",
            "        ),",
            "        xaxis = list(",
            "            title = x_axis_label,",
            "            tickangle = -45,",
            "            font = list(size = 11)",
            "        ),",
            "        yaxis = list(",
            "            title = y_axis_label,",
            "            font = list(size = 11)",
            "        ),",
            "        plot_bgcolor = 'white',",
            "        paper_bgcolor = 'white',",
            "        showlegend = FALSE,",
            "        margin = list(b = 80)",
            "    )"
        )
    } else {
        code_lines <- c(code_lines,
            "    # Create hover text with unit details",
            "    timeline_plot <- plotly::plot_ly(",
            "        data = histogram_total,",
            "        x = ~bin_label,",
            "        y = ~total_admissions,",
            "        type = 'bar',",
            "        marker = list(color = '#2C699A', opacity = 0.8),",
            "        hovertemplate = paste0(",
            "            '<b>%{x}</b><br>',",
            "            paste0(i18np$t(\"admission_count\"), \" : %{y}\"),",
            "            '<br><br>%{customdata}',",
            "            '<extra></extra>'",
            "        ),",
            "        customdata = ~detail_text",
            "    ) %>%",
            "    plotly::layout(",
            "        title = list(",
            "            text = plot_title,",
            "            font = list(size = 16, color = '#2C699A')",
            "        ),",
            "        xaxis = list(",
            "            title = x_axis_label,",
            "            tickangle = -45,",
            "            font = list(size = 11)",
            "        ),",
            "        yaxis = list(",
            "            title = y_axis_label,",
            "            font = list(size = 11)",
            "        ),",
            "        plot_bgcolor = 'white',",
            "        paper_bgcolor = 'white',",
            "        showlegend = FALSE,",
            "        margin = list(b = 80)",
            "    )"
        )
    }
    
    code_lines <- c(code_lines,
        "} else {",
        "    # Create empty plot with message when no data",
        "    timeline_plot <- plotly::plot_ly() %>%",
        "        plotly::add_annotations(",
        "            text = i18np$t(\"no_data_available\"),",
        "            x = 0.5,",
        "            y = 0.5,",
        "            xref = 'paper',",
        "            yref = 'paper',",
        "            showarrow = FALSE,",
        "            font = list(size = 16, color = '#666666')",
        "        ) %>%",
        "        plotly::layout(",
        "            plot_bgcolor = 'white',",
        "            paper_bgcolor = 'white',",
        "            xaxis = list(visible = FALSE),",
        "            yaxis = list(visible = FALSE)",
        "        )",
        "}",
        "",
        "# Return the plot object",
        "timeline_plot"
    )
    
    generated_code <- paste(code_lines, collapse = "\n")
    
    return(generated_code)
}
