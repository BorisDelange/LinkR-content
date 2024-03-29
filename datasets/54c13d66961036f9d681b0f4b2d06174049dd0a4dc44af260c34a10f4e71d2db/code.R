# BigQuery authentification
# bigrquery::bq_auth()

# Get BigQuery project ID
bd_config <- vroom::vroom("%dataset_folder%/bq_config.csv", col_types = "cc", progress = FALSE)

# VISIT_OCCURRENCE
cat("<strong>VISIT_OCCURRENCE</strong>\n\n")

visit_occurrence <- function(){
    sql <- paste0(
    "SELECT ",
        "HADM_ID AS visit_occurrence_id,",
        "SUBJECT_ID AS person_id,",
        "0 AS visit_concept_id,",
        "CAST(ADMITTIME AS DATE) AS visit_start_date,",
        "ADMITTIME AS visit_start_datetime,",
        "CAST(DISCHTIME AS DATE) AS visit_end_date,",
        "DISCHTIME AS visit_end_datetime,",
        "0 AS visit_type_concept_id,",
        "NULL AS provider_id,",
        "NULL AS care_site_id,",
        "ADMISSION_TYPE AS visit_source_value,",
        "HADM_ID AS visit_source_concept_id,",
        "0 AS admitted_from_concept_id,",
        "ADMISSION_LOCATION AS admitted_from_source_value,",
        "0 AS discharge_to_concept_id,",
        "DISCHARGE_LOCATION AS discharge_to_source_value,",
        "NULL AS preceding_visit_occurrence_id ",
        "FROM physionet-data.mimiciii_clinical.admissions ",
        "ORDER BY ADMITTIME ",
        "LIMIT 5000"
    )

    query <- bigrquery::bq_project_query(bd_config %>% dplyr::filter(key == "bq_project_id") %>% dplyr::pull(value), sql)
    bigrquery::bq_table_download(query, quiet = TRUE)
}

import_dataset(output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = %dataset_id%, data = visit_occurrence(),
    type = "visit_occurrence", omop_version = %omop_version%, read_with = "vroom", save_as = "csv", rewrite = FALSE)


# PERSON
cat("\n\n<strong>PERSON</strong>\n\n")

person <- function(){
    sql <- glue::glue(paste0(
        "SELECT ",
        "SUBJECT_ID AS person_id,",
        "CASE WHEN GENDER = 'F' THEN 8532 WHEN GENDER = 'M' THEN 8507 ELSE NULL END AS gender_concept_id,",
        "EXTRACT(YEAR FROM DOB) AS year_of_birth,",
        "EXTRACT(MONTH FROM DOB) AS month_of_birth,",
        "EXTRACT(DAY FROM DOB) AS day_of_birth,",
        "DOB AS birth_datetime,",
        "DOD AS death_datetime, ",
        "NULL AS race_concept_id,",
        "NULL AS ethnicity_concept_id,",
        "NULL AS location_id,",
        "NULL AS provider_id,",
        "NULL AS care_site_id,",
        "SUBJECT_ID AS person_source_value,",
        "GENDER AS gender_source_value,",
        "NULL AS gender_source_concept_id,",
        "NULL AS race_source_value,",
        "NULL AS race_source_concept_id,",
        "NULL AS ethnicity_source_value,",
        "NULL AS ethnicity_source_concept_id ",
        "FROM physionet-data.mimiciii_clinical.patients ",
        "WHERE SUBJECT_ID IN ({glue::glue_collapse(visit_occurrence() %>% dplyr::pull(person_id), sep = ', ')})"
    ))

    query <- bigrquery::bq_project_query(bd_config %>% dplyr::filter(key == "bq_project_id") %>% dplyr::pull(value), sql)
    bigrquery::bq_table_download(query, quiet = TRUE) %>%
        dplyr::mutate_at(c("person_id", "gender_concept_id", "year_of_birth", "month_of_birth", "day_of_birth", "race_concept_id", "ethnicity_concept_id",
            "location_id", "provider_id", "care_site_id", "gender_source_concept_id", "race_source_concept_id", "ethnicity_source_concept_id"), as.integer) %>%
        dplyr::mutate_at(c("person_source_value", "gender_source_value", "race_source_value", "ethnicity_source_value"), as.character)
}

import_dataset(output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = %dataset_id%, data = person(),
    type = "person", omop_version = %omop_version%, read_with = "vroom", save_as = "csv", rewrite = FALSE)

# VISIT_DETAIL
cat("\n\n<strong>VISIT_DETAIL</strong>\n\n")

visit_detail <- function(){
    sql <- glue::glue(paste0(
        "SELECT ",
        "ICUSTAY_ID AS visit_detail_id,",
        "SUBJECT_ID AS person_id,",
        "0 AS visit_detail_concept_id,",
        "CAST(INTIME AS DATE) AS visit_detail_start_date,",
        "INTIME AS visit_detail_start_datetime,",
        "CAST(OUTTIME AS DATE) AS visit_detail_end_date,",
        "OUTTIME AS visit_detail_end_datetime,",
        "0 AS visit_detail_type_concept_id,",
        "NULL AS provider_id,",
        "NULL AS care_site_id,",
        "NULL AS visit_detail_source_value,",
        "NULL AS visit_detail_source_concept_id,",
        "NULL AS admitted_from_concept_id,",
        "NULL AS admitted_from_source_value,",
        "NULL AS discharge_to_source_value,",
        "NULL AS discharge_to_concept_id,",
        "NULL AS preceding_visit_detail_id,",
        "NULL AS visit_detail_parent_id,",
        "HADM_ID AS visit_occurrence_id, ",
        "CASE ",
            "WHEN FIRST_CAREUNIT = 'CCU' THEN 'Coronary care unit' ",
            "WHEN FIRST_CAREUNIT = 'CSRU' THEN 'Cardiac surgery recovery unit' ",
            "WHEN FIRST_CAREUNIT = 'MICU' THEN 'Medical intensive care unit' ",
            "WHEN FIRST_CAREUNIT = 'NICU' THEN 'Neonatal intensive care unit' ",
            "WHEN FIRST_CAREUNIT = 'NWARD' THEN 'Neonatal ward' ",
            "WHEN FIRST_CAREUNIT = 'SICU' THEN 'Surgical intensive care unit' ",
            "WHEN FIRST_CAREUNIT = 'TSICU' THEN 'Trauma/surgical intensive care unit' ",
        "END AS first_careunit ",
        "FROM physionet-data.mimiciii_clinical.icustays ",
        "WHERE HADM_ID IN ({glue::glue_collapse(visit_occurrence() %>% dplyr::pull(visit_occurrence_id), sep = ', ')})"
    ))

    query <- bigrquery::bq_project_query(bd_config %>% dplyr::filter(key == "bq_project_id") %>% dplyr::pull(value), sql)
    data <- bigrquery::bq_table_download(query, quiet = TRUE) %>%
        dplyr::mutate_at(c("visit_detail_id", "person_id", "visit_detail_concept_id", "visit_detail_type_concept_id", "provider_id", "care_site_id",
            "visit_detail_source_concept_id", "admitted_from_concept_id", "discharge_to_concept_id", "visit_detail_parent_id", "visit_occurrence_id"), as.integer) %>%
        dplyr::mutate_at(c("visit_detail_source_value", "admitted_from_source_value", "discharge_to_source_value"), as.character)

    # Get visit_detail concept_ids
    sql <- glue::glue_sql(paste0(
        "SELECT concept_id AS visit_detail_concept_id, concept_name AS first_careunit FROM concept ",
        "WHERE vocabulary_id = 'MIMIC-III' AND domain_id = 'Visit'"), .con = m$db)
    concept <- DBI::dbGetQuery(m$db, sql)

    data <- data %>%
        dplyr::select(-visit_detail_concept_id) %>%
        dplyr::left_join(concept, by = "first_careunit") %>%
        dplyr::select(-first_careunit) %>%
        dplyr::relocate(visit_detail_concept_id, .after = "person_id")

    data
}

import_dataset(output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = %dataset_id%, data = visit_detail(),
    type = "visit_detail", omop_version = %omop_version%, read_with = "vroom", save_as = "csv", rewrite = FALSE)

# NOTE
cat("\n\n<strong>NOTE</strong>\n\n")

note <- function(){
    sql <- glue::glue(paste0(
        "SELECT ",
        "ROW_ID AS note_id,",
        "SUBJECT_ID AS person_id,",
        "NULL AS note_event_id,",
        "NULL AS note_event_field_concept_id,",
        "CAST(CHARTDATE AS DATE) AS note_date,",
        "CAST(CONCAT(CAST(CHARTDATE AS DATE), ' 00:00:00') AS DATETIME) AS note_datetime,",
        "NULL AS note_type_concept_id,",
        "NULL AS note_class_concept_id,",
        "CONCAT(CATEGORY, ' - ', DESCRIPTION) AS note_title,",
        "TEXT AS note_text,",
        "NULL AS encoding_concept_id,",
        "NULL AS language_concept_id,",
        "NULL AS provider_id,",
        "HADM_ID AS visit_occurrence_id,",
        "NULL AS visit_detail_id,",
        "NULL AS note_source_value ",
        "FROM physionet-data.mimiciii_notes.noteevents ",
        "WHERE HADM_ID IN ({glue::glue_collapse(visit_occurrence() %>% dplyr::pull(visit_occurrence_id), sep = ', ')})"
    ))

    query <- bigrquery::bq_project_query(bd_config %>% dplyr::filter(key == "bq_project_id") %>% dplyr::pull(value), sql)
    data <- bigrquery::bq_table_download(query, quiet = TRUE) %>%
        dplyr::mutate_at(c("note_id", "note_event_field_concept_id", "note_type_concept_id", "note_class_concept_id", 
            "encoding_concept_id", "language_concept_id", "provider_id", "visit_occurrence_id", "visit_detail_id"), as.integer) %>%
        dplyr::mutate_at(c("note_title", "note_text", "note_source_value"), as.character)
}

import_dataset(output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = %dataset_id%, data = note(),
    type = "note", omop_version = %omop_version%, read_with = "duckdb", save_as = "parquet", rewrite = FALSE)

# DEVICE_EXPOSURE
# cat("\n\n<strong>DEVICE_EXPOSURE</strong>\n\n")
# 
# device_exposure <- function(){
#     # Get concept for mechanical ventilator
#     concept_id <- 0L
#     sql <- glue::glue_sql(paste0(
#         "SELECT concept_id FROM concept WHERE vocabulary_id = 'MIMIC-III' AND concept_name = 'Mechanical ventilator'"), .con = m$db)
#     concept <- DBI::dbGetQuery(m$db, sql)
#     if (nrow(concept) > 0) concept_id <- concept %>% dplyr::pull(concept_id)
# 
#     sql <- glue::glue(paste0(
#         "SELECT ",
#         "ROW_NUMBER() OVER() AS device_exposure_id,",
#         "icu.SUBJECT_ID AS person_id,",
#         "{concept_id} AS device_concept_id,",
#         "CAST(starttime AS DATE) AS device_exposure_start_date,",
#         "starttime AS device_exposure_start_datetime,",
#         "CAST(endtime AS DATE) AS device_exposure_end_date,",
#         "endtime AS device_exposure_end_datetime,",
#         "0 AS device_type_concept_id,",
#         "NULL AS unique_device_id,",
#         "NULL AS quantity,",
#         "NULL AS provider_id,",
#         "icu.HADM_ID AS visit_occurrence_id,",
#         "vd.icustay_id AS visit_detail_id,",
#         "NULL AS device_source_value,",
#         "NULL AS device_source_concept_id ",
#         "FROM physionet-data.mimiciii_derived.ventilation_durations vd ",
#         "LEFT JOIN physionet-data.mimiciii_clinical.icustays icu ON vd.icustay_id = icu.icustay_id ",
#         "WHERE vd.icustay_id IN ({glue::glue_collapse(visit_detail() %>% dplyr::pull(visit_detail_id), sep = ', ')})"
#     ))
# 
#     query <- bigrquery::bq_project_query(bd_config %>% dplyr::filter(key == "bq_project_id") %>% dplyr::pull(value), sql)
#     data <- bigrquery::bq_table_download(query, quiet = TRUE) %>%
#         dplyr::mutate_at(c("device_exposure_id", "person_id", "device_concept_id", "device_type_concept_id", "quantity", "provider_id",
#             "visit_occurrence_id", "visit_detail_id", "device_source_concept_id"), as.integer) %>%
#         dplyr::mutate_at(c("unique_device_id", "device_source_value"), as.character)
# 
#     data
# }
# 
# import_dataset(output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = %dataset_id%, data = device_exposure(),
#     type = "device_exposure", omop_version = %omop_version%, read_with = "duckdb", save_as = "parquet", rewrite = FALSE)
