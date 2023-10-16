# BigQuery authentification
# bigrquery::bq_auth()

# Get BigQuery project ID
bd_config <- vroom::vroom("%vocabulary_folder%/bq_config.csv", col_types = "cc", progress = FALSE)

# Check if these concepts have already been added

sql <- glue::glue_sql("SELECT * FROM concept WHERE vocabulary_id = 'MIMIC-III' AND concept_name = 'CCU'", .con = m$db)
vocab_already_imported <- DBI::dbGetQuery(m$db, sql) %>% nrow() > 0

if (!vocab_already_imported){

    # Get last row
    # If inferior to 2B, start at 2B
    concept_last_row <- DBI::dbGetQuery(m$db, "SELECT COALESCE(MAX(concept_id), 0) AS concept_id FROM concept") %>% dplyr::pull(concept_id)
    if (concept_last_row < 2000000000) concept_last_row <- 1999999999L

    # Add visit_detail_concept_ids
    
    sql <- glue::glue(paste0(
        "WITH visit_detail_concepts AS (",
        "  SELECT 'CCU' AS concept_code, 'Coronary care unit' AS concept_name_full UNION ALL ",
        "  SELECT 'CSRU', 'Cardiac surgery recovery unit' UNION ALL ",
        "  SELECT 'MICU', 'Medical intensive care unit' UNION ALL ",
        "  SELECT 'NICU', 'Neonatal intensive care unit' UNION ALL ",
        "  SELECT 'NWARD', 'Neonatal ward' UNION ALL ",
        "  SELECT 'SICU', 'Surgical intensive care unit' UNION ALL ",
        "  SELECT 'TSICU', 'Trauma/surgical intensive care unit' ",
        ") ",
        "SELECT DISTINCT ",
        "c.concept_name_full AS concept_name ",
        "FROM physionet-data.mimiciii_clinical.icustays i ",
        "LEFT JOIN visit_detail_concepts c ON i.FIRST_CAREUNIT = c.concept_code"
    ))

    query <- bigrquery::bq_project_query(bd_config %>% dplyr::filter(key == "bq_project_id") %>% dplyr::pull(value), sql)
    concept <-
        bigrquery::bq_table_download(query) %>%
        dplyr::transmute(concept_id = concept_last_row + 1:dplyr::n(), concept_name, domain_id = "Visit", vocabulary_id = "MIMIC-III",
            concept_class_id = "Visit", standard_concept = NA_character_, concept_code = "",
            valid_start_date = lubridate::today(), valid_end_date = lubridate::ymd("2099-12-31"), invalid_reason = NA_character_)
    
#     # Add mechanical ventilation concept
#     concept <- concept %>%
#         dplyr::bind_rows(
#             tibble::tibble(concept_id = max(concept$concept_id) + 1L, concept_name = "Mechanical ventilator", domain_id = "Device", vocabulary_id = "MIMIC-III",
#             concept_class_id = "Physical Object", standard_concept = NA_character_, concept_code = "",
#             valid_start_date = lubridate::today(), valid_end_date = lubridate::ymd("2099-12-31"), invalid_reason = NA_character_
#         ))

    # Import vocabulary
    import_vocabulary_table(output = output, ns = ns, i18n = i18n, r = r, m = m, table_name = "concept", data = concept)
}
