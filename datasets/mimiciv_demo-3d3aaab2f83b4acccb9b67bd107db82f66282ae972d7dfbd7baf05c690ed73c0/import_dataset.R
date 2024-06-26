# create_ids <- function(col_name = character(), data = tibble::tibble(), new_cols = list(), col_left_join = character()){
#   
#   new_cols[[col_name]] <<-
#     data %>%
#     dplyr::distinct(get(col_name)) %>%
#     dplyr::rename_at(1, ~col_name)
#   
#   if (length(col_left_join) > 0) new_cols[[col_name]] <<- 
#       new_cols[[col_name]] %>%
#       dplyr::left_join(
#         new_cols[[col_left_join]] %>%
#           dplyr::rename_at(1, ~col_name) %>%
#           dplyr::rename_at(2, ~paste0("new_", col_name)),
#         by = col_name
#       )
#   
#   else new_cols[[col_name]] <<- 
#       new_cols[[col_name]] %>% 
#       dplyr::mutate(!!paste0("new_", col_name) := 1:dplyr::n())
# }
# 
# join_new_ids <- function(col_name = character(), data = tibble::tibble()){
#   
#   data <<-
#     data %>%
#     dplyr::left_join(new_cols[[col_name]], by = col_name) %>%
#     dplyr::relocate(!!paste0("new_", col_name), .before = !!col_name) %>%
#     dplyr::select(-!!col_name) %>%
#     dplyr::rename_at(paste0("new_", col_name), ~col_name)
# }
# 
# cols_left_join <- c(
#   "preceding_visit_occurrence_id" = "visit_occurrence_id",
#   "preceding_visit_detail_id" = "visit_detail_id"
# )
# 
# new_cols <- list()
# data <- tibble::tibble()
# 
# prefix <- "https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/"
# 
tables <- tibble::tribble(
  ~table_name, ~col_types, ~ids_to_join, ~ids_to_create, 
  "person", "niiiiTiiiiiccicici", "person_id", "person_id",
  "observation_period", "nnDDi", 
  c("observation_period_id", "person_id"),
  c("observation_period_id"),
  "care_site", "nciicc", 
  c("care_site_id"), 
  c("care_site_id"),
  "visit_occurrence", "nniDTDTiiiciicicn", 
  c("visit_occurrence_id", "person_id", "preceding_visit_occurrence_id"), 
  c("visit_occurrence_id", "preceding_visit_occurrence_id"),
  "visit_detail", "nniDTDTiiniinciccin",
  c("visit_detail_id", "person_id", "care_site_id", "preceding_visit_detail_id", "visit_occurrence_id"),
  c("visit_detail_id", "preceding_visit_detail_id"),
  "condition_occurrence", "nniDTDTicinncici",
  c("condition_occurrence_id", "person_id", "visit_occurrence_id", "visit_detail_id"),
  c("condition_occurrence_id"),
  "drug_exposure", "nniDTDTDicinicicinicicc",
  c("drug_exposure_id", "person_id", "visit_occurrence_id"),
  c("drug_exposure_id"),
  "procedure_occurrence", "nniDTiiiinicic", c("procedure_occurrence_id", "person_id", "visit_occurrence_id"), c("procedure_occurrence_id"),
  "device_exposure", "nniDTDTiciinnci", c("device_exposure_id", "person_id", "visit_occurrence_id", "visit_detail_id"), c("device_exposure_id"),
  "measurement", "nniDTciiniinninncicc",
  c("measurement_id", "person_id", "visit_occurrence_id", "visit_detail_id"),
  c("measurement_id"),
  "observation", "nniDTinciiiinicicc",
  c("observation_id", "person_id", "visit_occurrence_id"),
  c("observation_id"),
  "death", "nDTiici",
  c("person_id"),
  character(),
  "specimen", "nniiDTniiiccccc",
  c("specimen_id", "person_id"),
  c("specimen_id"),
  "location", "iccccccc",
  character(),
  character(),
  "drug_era", "nniDDii",
  c("drug_era_id", "person_id"),
  c("drug_era_id"),
  "dose_era", "nniinDD",
  c("dose_era_id", "person_id"),
  c("dose_era_id"),
  "condition_era", "nniDDi",
  c("condition_era_id", "person_id"),
  c("condition_era_id")
)

# Have all tables already been saved as csv ?
# all_tables_saved_as_csv <- all(paste0(tables$table_name, ".parquet") %in% list.files(paste0(r$app_folder, "/datasets/", %dataset_id%)))

for (i in 1:nrow(tables)){
  table <- tables[i, ]
#   if (i != 1) cat("\n\n")
#   cat(paste0(toupper(table$table_name), "\n\n"))
#   
#   get_data <- function(){
#     data <<- vroom::vroom(paste0(prefix, table$table_name, ".csv"), col_types = table$col_types, progress = FALSE)
#     
#     if (table$table_name == "visit_detail") data <<- data %>%
#         dplyr::relocate(visit_detail_source_value, visit_detail_source_concept_id, admitting_source_value, admitting_source_concept_id,
#           discharge_to_source_value, discharge_to_concept_id, preceding_visit_detail_id, visit_detail_parent_id, visit_occurrence_id, .after = "care_site_id")
#     if (table$table_name == "condition_occurrence") data <<- data %>%
#         dplyr::relocate(condition_status_concept_id, .after = "condition_type_concept_id")
#     
#     ids_to_join <- table$ids_to_join %>% unlist()
#     ids_to_create <- table$ids_to_create %>% unlist()
#     
#     if (length(ids_to_join) > 0){
#       for (j in 1:length(ids_to_join)){
#         id_to_join <- ids_to_join[j]
#         
#         if (id_to_join %in% ids_to_create){
#           col_left_join <- character()
#           if (id_to_join %in% names(cols_left_join)) col_left_join <- cols_left_join[id_to_join]
#           
#           create_ids(col_name = id_to_join, data = data, new_cols = new_cols, col_left_join = col_left_join)
#         }
#         
#         join_new_ids(col_name = id_to_join, data = data)
#       }
#     }
#     
#     data
#   }
#   
#   if (table$table_name %in% c("person", "care_site", "visit_occurrence", "visit_detail") & !all_tables_saved_as_csv) get_data()
  
  import_dataset(output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = %dataset_id%, data = get_data(), 
    omop_table = table$table_name, omop_version = %omop_version%, read_with = "duckdb", save_as = "parquet", rewrite = FALSE)
}
