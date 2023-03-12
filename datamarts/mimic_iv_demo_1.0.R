########## PATIENTS ##########
patients <- function(){
  vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/icu/icustays.csv.gz", show_col_types = FALSE) %>% 
    dplyr::select(patient_id = subject_id) %>%
    dplyr::group_by(patient_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(
      vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/core/patients.csv.gz", show_col_types = FALSE) %>% 
        dplyr::select(patient_id = subject_id, gender, dod),
      by = "patient_id"
    ) %>%
    dplyr::mutate_at("patient_id", as.integer) %>%
    dplyr::mutate_at("dod", lubridate::ymd_hms)
}

import_datamart(output = output, ns = ns, r = r, d = d, datamart_id = %datamart_id%, data = patients(), type = "patients", save_as_csv = TRUE, rewrite = FALSE, i18n = r$i18n)

########## THESAURUS ##########
thesaurus <- function(){
  # Add the thesaurus to recover item_id corresponding to unit names
  vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/icu/d_items.csv.gz", show_col_types = FALSE) %>%
    dplyr::transmute(item_id = itemid, name = label, display_name = "", category, unit = unitname) %>%
    dplyr::bind_rows(
      vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/hosp/d_labitems.csv.gz", show_col_types = FALSE) %>%
        dplyr::transmute(item_id = itemid, name = label, display_name = "", category, unit = "")
    ) %>%
    dplyr::mutate_at("item_id", as.integer) -> temp
  
  temp %>%
    dplyr::bind_rows(
      vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/icu/icustays.csv.gz", show_col_types = FALSE) %>%
        dplyr::distinct(first_careunit) %>%
        dplyr::transmute(item_id = 1:dplyr::n() + max(temp$item_id) + 100000, name = first_careunit, display_name = "", category = "Hospital units", unit = "")
    )
}

########## STAYS ##########
stays <- function(){
  vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/icu/icustays.csv.gz", show_col_types = FALSE) %>%
    dplyr::transmute(patient_id = subject_id, stay_id = hadm_id, thesaurus_name = "MIMIC-IV", unit_name = first_careunit, 
      admission_datetime = intime, discharge_datetime = outtime) %>%
    dplyr::inner_join(patients() %>% dplyr::select(patient_id), by = "patient_id") %>%
    dplyr::left_join(
      thesaurus() %>% dplyr::select(item_id, unit_name = name),
      by = "unit_name"
    ) %>%
    dplyr::inner_join(
      vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/core/patients.csv.gz", show_col_types = FALSE) %>% 
        dplyr::select(patient_id = subject_id, age = anchor_age),
      by = "patient_id"
    ) %>%
    dplyr::select(-unit_name) %>%
    dplyr::relocate(item_id, .after = "thesaurus_name") %>%
    dplyr::relocate(age, .after = "stay_id") %>%
    dplyr::mutate_at(c("patient_id", "stay_id", "item_id"), as.integer)
}

cat("\n\n")
import_datamart(output = output, ns = ns, r = r, d = d, datamart_id = %datamart_id%, data = stays(), type = "stays", save_as_csv = TRUE, rewrite = FALSE, i18n = r$i18n)

########## LABS_VITALS ##########
labs_vitals <- function(){
  vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/icu/chartevents.csv.gz", show_col_types = FALSE) %>%
    dplyr::transmute(patient_id = subject_id, thesaurus_name = "MIMIC-IV", item_id = itemid, datetime_start = charttime, datetime_stop = "", 
      value, value_num = valuenum, unit = valueuom, comments = "") %>%
    dplyr::inner_join(patients() %>% dplyr::select(patient_id), by = "patient_id") %>%
    dplyr::bind_rows(
      vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/hosp/labevents.csv.gz", show_col_types = FALSE) %>%
        dplyr::transmute(patient_id = subject_id, thesaurus_name = "MIMIC-IV", item_id = itemid, datetime_start = charttime, datetime_stop = "", 
          value, value_num = valuenum, unit = valueuom, comments = "") %>%
        dplyr::inner_join(patients() %>% dplyr::select(patient_id), by = "patient_id")
    ) %>%
    dplyr::bind_rows(
      vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/icu/datetimeevents.csv.gz", show_col_types = FALSE) %>%
        dplyr::transmute(patient_id = subject_id, thesaurus_name = "MIMIC-IV", item_id = itemid, datetime_start = charttime, datetime_stop = "", 
          value, value_num = NA_real_, unit = valueuom, comments = "") %>%
        dplyr::inner_join(patients() %>% dplyr::select(patient_id), by = "patient_id") %>%
        dplyr::mutate_at("value", as.character)
    ) %>%
    dplyr::bind_rows(
      vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/icu/outputevents.csv.gz", show_col_types = FALSE) %>%
        dplyr::transmute(patient_id = subject_id, thesaurus_name = "MIMIC-IV", item_id = itemid, datetime_start = charttime, datetime_stop = "", 
          value_temp = "", value_num = value, unit = valueuom, comments = "") %>%
        dplyr::rename(value = value_temp) %>%
        dplyr::inner_join(patients() %>% dplyr::select(patient_id), by = "patient_id")
    ) %>%
    dplyr::mutate_at(c("patient_id", "item_id"), as.integer) %>%
    dplyr::mutate_at("value", as.character) %>%
    dplyr::mutate_at("value_num", as.numeric) %>%
    dplyr::mutate_at("datetime_stop", lubridate::ymd_hms) %>%
    dplyr::mutate(thesaurus_name = dplyr::case_when(
      grepl("^51", as.character(item_id)) ~ "MIMIC-IV-bis",
      TRUE ~ thesaurus_name
    ))
}

cat("\n\n")
import_datamart(output = output, ns = ns, r = r, d = d, datamart_id = %datamart_id%, data = labs_vitals(), type = "labs_vitals", save_as_csv = TRUE, rewrite = FALSE, i18n = r$i18n)

########## ORDERS ##########
orders <- function(){
  vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/icu/inputevents.csv.gz", show_col_types = FALSE) %>%
    dplyr::transmute(patient_id = subject_id, thesaurus_name = "MIMIC-IV", item_id = itemid, datetime_start = starttime, datetime_stop = endtime,
      route = "", continuous = NA_integer_, amount, amount_unit = amountuom, rate, rate_unit = rateuom, 
      concentration = NA_real_, concentration_unit = "", comments = "") %>%
    dplyr::inner_join(patients() %>% dplyr::select(patient_id), by = "patient_id") %>%
    dplyr::mutate_at(c("patient_id", "item_id"), as.integer) %>%
    dplyr::mutate_at("continuous", as.integer) %>%
    dplyr::mutate_at("concentration", as.numeric)
}

cat("\n\n")
import_datamart(output = output, r = r, d = d, datamart_id = %datamart_id%, data = orders(), type = "orders", save_as_csv = TRUE, rewrite = FALSE, i18n = r$i18n)