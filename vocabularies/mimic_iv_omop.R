folder <- "https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/"

col_types <- list()
col_types$concept <- "iccccccDDc"
col_types$concept_relationship <- "iicDDc"

for (table_name in c("concept", "concept_relationship")){
  
  cat(paste0(toupper(table_name), "\n\n"))
  
  data <-
    vroom::vroom(paste0(folder, "2b_", table_name, ".csv"), col_types = col_types[[table_name]], progress = FALSE) %>% 
    dplyr::rename(valid_start_date = valid_start_DATE, valid_end_date = valid_end_DATE)
  
  import_vocabulary_table(output = output, ns = ns, i18n = i18n, r = r, m = m, table_name = table_name, data = data, vocabulary_id = "%vocabulary_id%") %>% print()
  cat("\n\n") 
}

for (i in 1:nrow(vocabulary_concepts_row_details)){
  row <- vocabulary_concepts_row_details[i, ]
  # print(names(row$cols %>% unlist()))
  cols <- row$cols %>% unlist()
  for (name in names(cols)){
    print(cols[[name]])
  }
}