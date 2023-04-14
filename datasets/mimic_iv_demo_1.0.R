# PERSON

cat("PERSON\n\n")

person <- vroom::vroom("https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/person.csv", col_types = "niiiiTiiiiiccicici")

person_ids <-
  person %>%
  dplyr::distinct(person_id) %>%
  dplyr::mutate(new_person_id = 1:dplyr::n())

person <- 
  person %>%
  dplyr::left_join(person_ids, by = "person_id") %>%
  dplyr::select(-person_id, person_id = new_person_id) %>%
  dplyr::relocate(person_id)

import_dataset(output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = %dataset_id%, data = person, type = "person", 
  omop_version = "5.3", save_as_csv = TRUE, rewrite = FALSE)

# CARE_SITE

cat("\n\nCARE_SITE\n\n")

care_site <- vroom::vroom("https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/care_site.csv", col_types = "nciicc")

care_site_ids <-
  care_site %>%
  dplyr::distinct(care_site_id) %>%
  dplyr::mutate(new_care_site_id = 1:dplyr::n())

care_site <-
  care_site %>%
  dplyr::left_join(care_site_ids, by = "care_site_id") %>%
  dplyr::select(-care_site_id) %>%
  dplyr::rename(care_site_id = new_care_site_id) %>%
  dplyr::relocate(care_site_id)

import_dataset(output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = %dataset_id%, data = care_site, type = "care_site", 
  omop_version = "5.3", save_as_csv = TRUE, rewrite = FALSE)

# VISIT_OCCURRENCE

cat("\n\nVISIT_OCCURRENCE\n\n")

visit_occurrence <- vroom::vroom("https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/visit_occurrence.csv", col_types = "nniDTDTiiiciicicn")

visit_occurrence_ids <- 
  visit_occurrence %>%
  dplyr::distinct(visit_occurrence_id) %>%
  dplyr::mutate(new_visit_occurrence_id = 1:dplyr::n())

preceding_visit_occurrence_ids <-
  visit_occurrence %>%
  dplyr::distinct(preceding_visit_occurrence_id) %>%
  dplyr::left_join(
    visit_occurrence_ids %>% 
      dplyr::select(preceding_visit_occurrence_id = visit_occurrence_id, new_preceding_visit_occurrence_id = new_visit_occurrence_id), by = "preceding_visit_occurrence_id")

visit_occurrence <-
  visit_occurrence %>%
  dplyr::left_join(visit_occurrence_ids, by = "visit_occurrence_id") %>%
  dplyr::left_join(person_ids, by = "person_id") %>%
  dplyr::left_join(preceding_visit_occurrence_ids, by = "preceding_visit_occurrence_id") %>%
  dplyr::select(-person_id, -visit_occurrence_id, -preceding_visit_occurrence_id) %>%
  dplyr::rename(person_id = new_person_id, visit_occurrence_id = new_visit_occurrence_id, preceding_visit_occurrence_id = new_preceding_visit_occurrence_id) %>%
  dplyr::relocate(visit_occurrence_id, person_id)

import_dataset(output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = %dataset_id%, data = visit_occurrence, type = "visit_occurrence", 
  omop_version = "5.3", save_as_csv = TRUE, rewrite = FALSE)

# VISIT_DETAIL

cat("\n\nVISIT_DETAIL\n\n")

visit_detail <- vroom::vroom("https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/visit_detail.csv", col_types = "nniDTDTiiniinciccin")

visit_detail_ids <- 
  visit_detail %>%
  dplyr::distinct(visit_detail_id) %>%
  dplyr::mutate(new_visit_detail_id = 1:dplyr::n())

preceding_visit_detail_ids <-
  visit_detail %>%
  dplyr::distinct(preceding_visit_detail_id) %>%
  dplyr::left_join(
    visit_detail_ids %>% 
      dplyr::select(preceding_visit_detail_id = visit_detail_id, new_preceding_visit_detail_id = new_visit_detail_id), by = "preceding_visit_detail_id")

visit_detail <-
  visit_detail %>%
  dplyr::left_join(visit_detail_ids, by = "visit_detail_id") %>%
  dplyr::left_join(person_ids, by = "person_id") %>%
  dplyr::left_join(care_site_ids, by = "care_site_id") %>%
  dplyr::left_join(preceding_visit_detail_ids, by = "preceding_visit_detail_id") %>%
  dplyr::left_join(visit_occurrence_ids, by = "visit_occurrence_id") %>%
  dplyr::select(-visit_detail_id, -person_id, -care_site_id, -preceding_visit_detail_id, -visit_occurrence_id) %>%
  dplyr::rename(visit_detail_id = new_visit_detail_id, person_id = new_person_id, care_site_id = new_care_site_id,
    preceding_visit_detail_id = new_preceding_visit_detail_id, visit_occurrence_id = new_visit_occurrence_id) %>%
  dplyr::relocate(visit_detail_id, person_id) %>%
  dplyr::relocate(care_site_id, .after = "provider_id") %>%
  dplyr::relocate(visit_detail_source_value, visit_detail_source_concept_id, .after = "care_site_id") %>%
  dplyr::relocate(admitting_source_value, admitting_source_concept_id, discharge_to_source_value, 
    discharge_to_concept_id, preceding_visit_detail_id, .after = "visit_detail_source_concept_id")

import_dataset(output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = %dataset_id%, data = visit_detail, type = "visit_detail", 
  omop_version = "5.3", save_as_csv = TRUE, rewrite = FALSE)

# MEASUREMENT

#measurement <- vroom::vroom("https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/measurement.csv", col_types = "nniDTciiniinninicicc")
#measurement <- vroom::vroom("/Users/borisdelange/Downloads/mimic-iv-demo-data-in-the-omop-common-data-model-0.9/1_omop_data_csv/measurement.csv", col_types = "nniDTciiniinninicncc")

#measurement <-
#measurement %>%
#  dplyr::mutate(new_measurement_id = 1:dplyr::n()) %>%
#  dplyr::left_join(person_ids, by = "person_id") %>%
#  dplyr::left_join(visit_occurrence_ids, by = "visit_occurrence_id") %>%
#  dplyr::select(-measurement_id, -person_id, -visit_occurrence_id) %>%
#  dplyr::rename(measurement_id = new_measurement_id, person_id = new_person_id, visit_occurrence_id = new_visit_occurrence_id) %>%
#  dplyr::relocate(measurement_id, person_id) %>%
#  dplyr::relocate(visit_occurrence_id, .after = "provider_id")

#import_dataset(output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = %dataset_id%, data = measurement, type = "measurement", 
#    omop_version = "5.3", save_as_csv = TRUE, rewrite = FALSE)