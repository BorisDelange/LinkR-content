folder <- ""

for (table_name in c("concept", "concept_class", "concept_relationship", "relationship", "concept_synonym", "concept_ancestor", "drug_strength")){
  
  cat(paste0(toupper("CONCEPT"), "\n\n"))
  
  data <- vroom::vroom(paste0(folder, toupper(table_name), ".csv"), col_types = col_types[[table_name]])
  
  if ("valid_start_date" %in% names(data)) data <- data %>% 
    dplyr::mutate_at(c("valid_start_date", "valid_end_date"), lubridate::ymd) %>%
    dplyr::mutate_at(c("valid_start_date", "valid_end_date"), as.character)
  
  cat("\n\n")
  import_vocabulary_table(output = output, ns = ns, i18n = i18n, r = r, m = m, vocabulary_id = %vocabulary_id%, table_name = table_name, data = data) %>% print()
}