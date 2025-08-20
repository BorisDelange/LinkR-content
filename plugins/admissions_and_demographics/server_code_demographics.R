# ==========================================
# server_code_demographics.R
# ==========================================

# Generate HTML code for demographics indicators using OMOP data
generate_demographics_ui_code_%widget_id% <- function(indicator = "average_age", indicator_scope = "hospitalization", hospital_units = c(), title = "", legend_1 = "", legend_2 = "") {
    
    code_lines <- c()
    
    # Define reusable CSS styles
    card_container_style <- "text-align: center; height: 100%; display: flex; align-items: center; flex-direction: column; justify-content: center;"
    icon_style <- "margin-bottom: 10px;"
    value_style <- "font-size: 36px; font-weight: bold; margin: 10px 0;"
    label_style <- "color: #666; font-size: 14px; text-transform: uppercase;"
    tooltip_style <- "visibility: hidden; opacity: 0; position: fixed; background-color: rgba(0,0,0,0.9); color: white; padding: 8px 12px; border-radius: 4px; font-size: 12px; z-index: 9999; transition: opacity 0.3s; min-width: 200px; max-width: 300px; text-align: left; box-shadow: 0 4px 12px rgba(0,0,0,0.4); line-height: 1.3; pointer-events: none;"
    
    # Define color schemes for demographics indicators (using plugin color palette)
    age_color <- "#fd7e14"  # Orange like length of stay
    gender_color <- "#2C699A"  # Blue like patient count
    
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
    
    if (indicator == "average_age") {
        if (indicator_scope == "hospitalization") {
            code_lines <- c(code_lines,
                "# Extract patient age data from all hospital visits",
                "patients_age <-",
                "    d$data_subset$person %>%",
                "    dplyr::inner_join(d$data_subset$visit_occurrence, by = \"person_id\") %>%",
                "    dplyr::collect() %>%",
                "    dplyr::mutate(",
                "        # Use birth_datetime if available, otherwise fall back to year_of_birth",
                "        age_at_admission = dplyr::case_when(",
                "            !is.na(birth_datetime) ~ as.numeric(difftime(visit_start_date, birth_datetime, units = \"days\")) / 365.25,",
                "            !is.na(year_of_birth) ~ lubridate::year(visit_start_date) - year_of_birth,",
                "            TRUE ~ NA_real_",
                "        ),",
                "        # Track which method was used for age calculation",
                "        age_method = dplyr::case_when(",
                "            !is.na(birth_datetime) ~ \"datetime\",",
                "            !is.na(year_of_birth) ~ \"year\",",
                "            TRUE ~ \"unknown\"",
                "        )",
                "    ) %>%",
                "    dplyr::filter(!is.na(age_at_admission)) %>%",
                "    dplyr::distinct(person_id, .keep_all = TRUE)",
                "",
                "# Calculate age statistics",
                "value <- if(nrow(patients_age) > 0) round(mean(patients_age$age_at_admission, na.rm = TRUE), 1) else 0",
                "total_patients <- nrow(patients_age)",
                "age_stats <- if(nrow(patients_age) > 0) {",
                "    list(",
                "        min = round(min(patients_age$age_at_admission, na.rm = TRUE), 1),",
                "        q1 = round(quantile(patients_age$age_at_admission, 0.25, na.rm = TRUE), 1),",
                "        median = round(median(patients_age$age_at_admission, na.rm = TRUE), 1),",
                "        q3 = round(quantile(patients_age$age_at_admission, 0.75, na.rm = TRUE), 1),",
                "        max = round(max(patients_age$age_at_admission, na.rm = TRUE), 1),",
                "        year_only_count = sum(patients_age$age_method == \"year\", na.rm = TRUE)",
                "    )",
                "} else { NULL }",
                "",
                "# Format minimum age display",
                "min_age_display <- if(nrow(patients_age) > 0) {",
                "    if(age_stats$min < 1) {",
                "        if(age_stats$min * 365.25 < 30) {",
                "            paste0(round(age_stats$min * 365.25), ' jours')",
                "        } else {",
                "            paste0(round(age_stats$min * 12), ' mois')",
                "        }",
                "    } else {",
                "        paste0(age_stats$min, ' ans')",
                "    }",
                "} else { '0 ans' }",
                "",
                "# Generate HTML card result",
                "res <- div()",
                "",
                "if (value > 0) {",
                generate_demographics_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"average_age\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", age_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-user-clock fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", age_color, "; ", value_style, " position: relative; cursor: pointer;\","),
                "            paste0(value, \" \"), tags$span(i18np$t(\"years\"), style = \"font-size: 18px;\"),",
                paste0("            onmouseover = \"", tooltip_js_show_value, "        \","),
                paste0("            onmouseout = \"", tooltip_js_hide_value, "\","),
                paste0("            div(class = \"", tooltip_class_name, "\", HTML(", 
                "                paste0(",
                "                    i18np$t(\"based_on\"), \" \", total_patients, \" \", tolower(i18np$t(\"patients\")), \"<br><br>\",",
                "                    \"<strong>Statistiques d'âge :</strong><br>\",",
                "                    \"<table style='border-collapse: collapse; width: 100%; font-size: 11px;'>\",",
                "                    \"<tr><td style='padding: 2px 8px 2px 0; width: 60px;'>Min :</td><td style='padding: 2px 0;'>\", min_age_display, \"</td></tr>\",",
                "                    \"<tr><td style='padding: 2px 8px 2px 0;'>Q1 :</td><td style='padding: 2px 0;'>\", age_stats$q1, \" ans</td></tr>\",",
                "                    \"<tr><td style='padding: 2px 8px 2px 0;'>Médiane :</td><td style='padding: 2px 0;'>\", age_stats$median, \" ans</td></tr>\",",
                "                    \"<tr><td style='padding: 2px 8px 2px 0;'>Q3 :</td><td style='padding: 2px 0;'>\", age_stats$q3, \" ans</td></tr>\",",
                "                    \"<tr><td style='padding: 2px 8px 2px 0;'>Max :</td><td style='padding: 2px 0;'>\", age_stats$max, \" ans</td></tr>\",",
                "                    \"</table>\",",
                "                    if(age_stats$year_only_count > 0) paste0(\"<br><em style='font-size: 10px;'>\", age_stats$year_only_count, \" patients: âge calculé avec l'année de naissance seulement</em>\") else \"\"",
                "                )",
                "            ), style = \"", tooltip_style, "\")"),
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
                "# Extract patient age data from selected hospital units",
                "patients_age <-",
                "    d$data_subset$person %>%",
                "    dplyr::inner_join(d$data_subset$visit_detail, by = \"person_id\") %>%",
                paste0("    dplyr::filter(care_site_id %in% c(", paste(unit_ids, collapse = ", "), ")) %>%"),
                "    dplyr::collect() %>%",
                "    dplyr::mutate(",
                "        # Use birth_datetime if available, otherwise fall back to year_of_birth",
                "        age_at_admission = dplyr::case_when(",
                "            !is.na(birth_datetime) ~ as.numeric(difftime(visit_detail_start_date, birth_datetime, units = \"days\")) / 365.25,",
                "            !is.na(year_of_birth) ~ lubridate::year(visit_detail_start_date) - year_of_birth,",
                "            TRUE ~ NA_real_",
                "        ),",
                "        # Track which method was used for age calculation",
                "        age_method = dplyr::case_when(",
                "            !is.na(birth_datetime) ~ \"datetime\",",
                "            !is.na(year_of_birth) ~ \"year\",",
                "            TRUE ~ \"unknown\"",
                "        )",
                "    ) %>%",
                "    dplyr::filter(!is.na(age_at_admission)) %>%",
                "    dplyr::distinct(person_id, .keep_all = TRUE)",
                "",
                "# Calculate age statistics",
                "value <- if(nrow(patients_age) > 0) round(mean(patients_age$age_at_admission, na.rm = TRUE), 1) else 0",
                "total_patients <- nrow(patients_age)",
                "age_stats <- if(nrow(patients_age) > 0) {",
                "    list(",
                "        min = round(min(patients_age$age_at_admission, na.rm = TRUE), 1),",
                "        q1 = round(quantile(patients_age$age_at_admission, 0.25, na.rm = TRUE), 1),",
                "        median = round(median(patients_age$age_at_admission, na.rm = TRUE), 1),",
                "        q3 = round(quantile(patients_age$age_at_admission, 0.75, na.rm = TRUE), 1),",
                "        max = round(max(patients_age$age_at_admission, na.rm = TRUE), 1),",
                "        year_only_count = sum(patients_age$age_method == \"year\", na.rm = TRUE)",
                "    )",
                "} else { NULL }",
                "",
                "# Format minimum age display",
                "min_age_display <- if(nrow(patients_age) > 0) {",
                "    if(age_stats$min < 1) {",
                "        if(age_stats$min * 365.25 < 30) {",
                "            paste0(round(age_stats$min * 365.25), ' jours')",
                "        } else {",
                "            paste0(round(age_stats$min * 12), ' mois')",
                "        }",
                "    } else {",
                "        paste0(age_stats$min, ' ans')",
                "    }",
                "} else { '0 ans' }",
                "",
                "# Generate HTML card result with unit info",
                "res <- div()",
                "",
                "if (value > 0) {",
                generate_demographics_legend_code_%widget_id%(legend_1, legend_2, "i18np$t(\"average_age\")", "i18np$t(\"in_selected_units\")"),
                "    res <- div(",
                paste0("        style = \"", card_container_style, "\","),
                "        div(",
                paste0("            style = \"color: ", age_color, "; ", icon_style, "\","),
                "            tags$i(class = \"fas fa-user-clock fa-2x\")",
                "        ),",
                "        div(",
                paste0("            style = \"color: ", age_color, "; ", value_style, " position: relative; cursor: pointer;\","),
                "            paste0(value, \" \"), tags$span(i18np$t(\"years\"), style = \"font-size: 18px;\"),",
                paste0("            onmouseover = \"", tooltip_js_show_value, "\","),
                paste0("            onmouseout = \"", tooltip_js_hide_value, "\","),
                paste0("            div(class = \"", tooltip_class_name, "\", HTML(", 
                "                paste0(",
                "                    i18np$t(\"based_on\"), \" \", total_patients, \" \", tolower(i18np$t(\"patients\")), \"<br><br>\",",
                "                    \"<strong>Statistiques d'âge :</strong><br>\",",
                "                    \"<table style='border-collapse: collapse; width: 100%; font-size: 11px;'>\",",
                "                    \"<tr><td style='padding: 2px 8px 2px 0; width: 60px;'>Min :</td><td style='padding: 2px 0;'>\", min_age_display, \"</td></tr>\",",
                "                    \"<tr><td style='padding: 2px 8px 2px 0;'>Q1 :</td><td style='padding: 2px 0;'>\", age_stats$q1, \" ans</td></tr>\",",
                "                    \"<tr><td style='padding: 2px 8px 2px 0;'>Médiane :</td><td style='padding: 2px 0;'>\", age_stats$median, \" ans</td></tr>\",",
                "                    \"<tr><td style='padding: 2px 8px 2px 0;'>Q3 :</td><td style='padding: 2px 0;'>\", age_stats$q3, \" ans</td></tr>\",",
                "                    \"<tr><td style='padding: 2px 8px 2px 0;'>Max :</td><td style='padding: 2px 0;'>\", age_stats$max, \" ans</td></tr>\",",
                "                    \"</table>\",",
                "                    if(age_stats$year_only_count > 0) paste0(\"<br><em style='font-size: 10px;'>\", age_stats$year_only_count, \" patients: âge calculé avec l'année de naissance seulement</em>\") else \"\"",
                "                )",
                "            ), style = \"", tooltip_style, "\")"),
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
        
    } else if (indicator == "gender") {
        if (indicator_scope == "hospitalization") {
            code_lines <- c(code_lines,
                "# Extract patient gender data from all hospital visits",
                "patients_gender <-",
                "    d$data_subset$person %>%",
                "    dplyr::inner_join(d$data_subset$visit_occurrence, by = \"person_id\") %>%",
                "    dplyr::filter(!is.na(gender_concept_id), gender_concept_id != 0) %>%",
                "    dplyr::left_join(",
                "        d$concept %>% dplyr::select(concept_id, concept_name),",
                "        by = c(\"gender_concept_id\" = \"concept_id\")",
                "    ) %>%",
                "    dplyr::collect() %>%",
                "    dplyr::distinct(person_id, .keep_all = TRUE)",
                "",
                "# Calculate gender distribution",
                "gender_counts <- patients_gender %>%",
                "    dplyr::count(concept_name, sort = TRUE)",
                "",
                "total_patients <- nrow(patients_gender)",
                "",
                "# Generate ggplot pie chart",
                "if (total_patients > 0 && nrow(gender_counts) > 0) {",
                "    # Calculate percentages",
                "    gender_counts <- gender_counts %>%",
                "        dplyr::mutate(",
                "            percentage = round((n / total_patients) * 100, 1),",
                "            label = paste0(concept_name, \"\\n\", percentage, \"% (\", n, \")\")",
                "        )",
                "    ",
                "    # Create pie chart",
                "    res <- ggplot2::ggplot(gender_counts, ggplot2::aes(x = \"\", y = n, fill = concept_name)) +",
                "        ggplot2::geom_col(width = 1, color = \"white\", size = 1) +",
                "        ggplot2::coord_polar(\"y\", start = 0) +",
                paste0("        ggplot2::scale_fill_manual(values = c(\"", gender_color, "\", \"#fd7e14\", \"#28a745\", \"#dc3545\")) +"),
                "        ggplot2::geom_text(",
                "            ggplot2::aes(label = label),",
                "            position = ggplot2::position_stack(vjust = 0.5),",
                "            color = \"white\",",
                "            fontface = \"bold\",",
                "            size = 4",
                "        ) +",
                "        ggplot2::theme_void() +",
                "        ggplot2::theme(",
                "            legend.position = \"none\",",
                "            plot.margin = ggplot2::margin(10, 10, 10, 10),",
                "            plot.title = ggplot2::element_text(hjust = 0.5)",
                "        ) +",
                paste0("        ggplot2::labs(title = if(nchar(\"", title, "\") > 0) \"", title, "\" else NULL)"),
                "} else {",
                "    res <- NULL",
                "}",
                ""
            )
        } else {
            code_lines <- c(code_lines,
                "# Extract patient gender data from selected hospital units",
                "patients_gender <-",
                "    d$data_subset$person %>%",
                "    dplyr::inner_join(d$data_subset$visit_detail, by = \"person_id\") %>%",
                paste0("    dplyr::filter(care_site_id %in% c(", paste(unit_ids, collapse = ", "), ")) %>%"),
                "    dplyr::filter(!is.na(gender_concept_id), gender_concept_id != 0) %>%",
                "    dplyr::left_join(",
                "        d$concept %>% dplyr::select(concept_id, concept_name),",
                "        by = c(\"gender_concept_id\" = \"concept_id\")",
                "    ) %>%",
                "    dplyr::collect() %>%",
                "    dplyr::distinct(person_id, .keep_all = TRUE)",
                "",
                "# Calculate gender distribution",
                "gender_counts <- patients_gender %>%",
                "    dplyr::count(concept_name, sort = TRUE)",
                "",
                "total_patients <- nrow(patients_gender)",
                "",
                "# Generate ggplot pie chart with unit info",
                "if (total_patients > 0 && nrow(gender_counts) > 0) {",
                "    # Calculate percentages",
                "    gender_counts <- gender_counts %>%",
                "        dplyr::mutate(",
                "            percentage = round((n / total_patients) * 100, 1),",
                "            label = paste0(concept_name, \"\\n\", percentage, \"% (\", n, \")\")",
                "        )",
                "    ",
                "    # Create pie chart",
                "    res <- ggplot2::ggplot(gender_counts, ggplot2::aes(x = \"\", y = n, fill = concept_name)) +",
                "        ggplot2::geom_col(width = 1, color = \"white\", size = 1) +",
                "        ggplot2::coord_polar(\"y\", start = 0) +",
                paste0("        ggplot2::scale_fill_manual(values = c(\"", gender_color, "\", \"#fd7e14\", \"#28a745\", \"#dc3545\")) +"),
                "        ggplot2::geom_text(",
                "            ggplot2::aes(label = label),",
                "            position = ggplot2::position_stack(vjust = 0.5),",
                "            color = \"white\",",
                "            fontface = \"bold\",",
                "            size = 4",
                "        ) +",
                "        ggplot2::theme_void() +",
                "        ggplot2::theme(",
                "            legend.position = \"none\",",
                "            plot.margin = ggplot2::margin(10, 10, 10, 10),",
                "            plot.title = ggplot2::element_text(hjust = 0.5)",
                "        ) +",
                paste0("        ggplot2::labs(title = if(nchar(\"", title, "\") > 0) \"", title, "\" else NULL)"),
                "} else {",
                "    res <- NULL",
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
generate_demographics_legend_code_%widget_id% <- function(legend_1, legend_2, default_legend_1, default_legend_2 = NULL) {
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
