create_ids <- function(col_name = character(), data = tibble::tibble(), table_name = character(), new_cols = list(), col_left_join = character()){

  new_cols[[col_name]] <<- 
    data[[table_name]] %>%
    dplyr::distinct(get(col_name)) %>%
    dplyr::rename_at(1, ~col_name)
  
  if (length(col_left_join) > 0) new_cols[[col_name]] <<- 
      new_cols[[col_name]] %>%
      dplyr::left_join(
        new_cols[[col_left_join]] %>%
          dplyr::rename_at(1, ~col_name) %>%
          dplyr::rename_at(2, ~paste0("new_", col_name)),
        by = col_name
      )
    
  else new_cols[[col_name]] <<- 
      new_cols[[col_name]] %>% 
      dplyr::mutate(!!paste0("new_", col_name) := 1:dplyr::n())
}

join_new_ids <- function(col_name = character(), data = tibble::tibble(), table_name = character()){
  data[[table_name]] <<-
    data[[table_name]] %>%
    dplyr::left_join(new_cols[[col_name]], by = col_name) %>%
    dplyr::relocate(!!paste0("new_", col_name), .before = !!col_name) %>%
    dplyr::select(-!!col_name) %>%
    dplyr::rename_at(paste0("new_", col_name), ~col_name)
}

new_cols <- list()
data <- list()

cols_left_join <- c(
  "preceding_visit_occurrence_id" = "visit_occurrence_id",
  "preceding_visit_detail_id" = "visit_detail_id"
  )

prefix <- "https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/"

tables <- tibble::tribble(
  ~table_name, ~col_types, ~ids_to_join, ~ids_to_create, 
  "person", "niiiiTiiiiiccicici", "person_id", "person_id",
  "care_site", "nciicc", "care_site_id", "care_site_id",
  "visit_occurrence", "nniDTDTiiiciicicn", 
    c("visit_occurrence_id", "person_id", "preceding_visit_occurrence_id"), c("visit_occurrence_id", "preceding_visit_occurrence_id"),
  "visit_detail", "nniDTDTiiniinciccin",
    c("visit_detail_id", "person_id", "care_site_id", "preceding_visit_detail_id", "visit_occurrence_id"),
    c("visit_detail_id", "preceding_visit_detail_id")
)

for (i in 1:nrow(tables)){
  table <- tables[i, ]
  cat(paste0(toupper(table$table_name), "\n\n"))
  data[[table$table_name]] <- vroom::vroom(paste0(prefix, table$table_name, ".csv"), col_types = table$col_types)
  
  ids_to_join <- table$ids_to_join %>% unlist()
  ids_to_create <- table$ids_to_create %>% unlist()
  
  if (length(ids_to_join) > 0){
    for (j in 1:length(ids_to_join)){
      id_to_join <- ids_to_join[j]
      
      if (id_to_join %in% ids_to_create){
        col_left_join <- character()
        if (id_to_join %in% names(cols_left_join)) col_left_join <- cols_left_join[id_to_join]
        
        create_ids(col_name = id_to_join, data = data, table_name = table$table_name, new_cols = new_cols, col_left_join = col_left_join)
      }
      
      join_new_ids(col_name = id_to_join, data = data, table_name = table$table_name) 
    }
  }
  
  # import_dataset(output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = %dataset_id%, data = data[[table$table_name]], 
  #   type = data$table_name, omop_version = "5.3", save_as_csv = TRUE, rewrite = FALSE)
}

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