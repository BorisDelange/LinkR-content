## Folder

folder <- ""

## CONCEPT

cat("CONCEPT\n\n")

#concept <- 
#    readr::read_delim(paste0(folder, "CONCEPT.csv"), delim = "\t", show_col_types = FALSE) %>%
#    dplyr::mutate_at("concept_id", as.integer) %>%
#    dplyr::mutate_at(dplyr::vars(-concept_id), as.character)

cat("\n\n")
#import_vocabulary_table(output = output, ns = ns, i18n = i18n, r = r, m = m, vocabulary_id = %vocabulary_id%, table_name = "concept", data = concept) %>% print()

## CONCEPT_CLASS

cat("\n\nCONCEPT_CLASS\n\n")

#concept_class <- 
#    readr::read_delim(paste0(folder, "CONCEPT_CLASS.csv"), delim = "\t", show_col_types = FALSE) %>%
#    dplyr::mutate_at("concept_class_concept_id", as.integer) %>%
#    dplyr::mutate_at(dplyr::vars(-concept_class_concept_id), as.character)

cat("\n\n")
#import_vocabulary_table(output = output, ns = ns, i18n = i18n, r = r, m = m, vocabulary_id = %vocabulary_id%, table_name = "concept_class", data = concept_class) %>% print()

## CONCEPT_RELATIONSHIP

cat("\n\nCONCEPT_RELATIONSHIP\n\n")

#concept_relationship <- 
#readr::read_delim(paste0(folder, "CONCEPT_RELATIONSHIP.csv"), delim = "\t", show_col_types = FALSE) %>%
#    dplyr::mutate_at(c("concept_id_1", "concept_id_2"), as.integer) %>%
#    dplyr::mutate_at(dplyr::vars(-concept_id_1, -concept_id_2), as.character)

cat("\n\n")
#import_vocabulary_table(output = output, ns = ns, i18n = i18n, r = r, m = m, vocabulary_id = %vocabulary_id%, table_name = "concept_relationship", data = concept_relationship) %>% print()

## RELATIONSHIP

cat("\n\nRELATIONSHIP\n\n")

#relationship <- 
#    readr::read_delim(paste0(folder, "RELATIONSHIP.csv"), delim = "\t", show_col_types = FALSE) %>%
#    dplyr::mutate_at("relationship_concept_id", as.integer) %>%
#    dplyr::mutate_at(dplyr::vars(-relationship_concept_id), as.character)

cat("\n\n")
#import_vocabulary_table(output = output, ns = ns, i18n = i18n, r = r, m = m, vocabulary_id = %vocabulary_id%, table_name = "relationship", data = relationship) %>% print()

## CONCEPT_SYNONYM

cat("\n\nCONCEPT_SYNONYM\n\n")

#concept_synonym <- 
#    readr::read_delim(paste0(folder, "CONCEPT_SYNONYM.csv"), delim = "\t", show_col_types = FALSE) %>%
#    dplyr::mutate_at(c("concept_id", "language_concept_id"), as.integer) %>%
#    dplyr::mutate_at(dplyr::vars(-concept_id, -language_concept_id), as.character)

cat("\n\n")
#import_vocabulary_table(output = output, ns = ns, i18n = i18n, r = r, m = m, vocabulary_id = %vocabulary_id%, table_name = "concept_synonym", data = concept_synonym) %>% print()

## CONCEPT_ANCESTOR

cat("\n\nCONCEPT_ANCESTOR\n\n")

concept_ancestor <- 
  readr::read_delim(paste0(folder, "CONCEPT_ANCESTOR.csv"), delim = "\t", show_col_types = FALSE) %>%
  dplyr::mutate_all(as.integer)

cat("\n\n")
import_vocabulary_table(output = output, ns = ns, i18n = i18n, r = r, m = m, vocabulary_id = %vocabulary_id%, table_name = "concept_ancestor", data = concept_ancestor) %>% print()

## DRUG_STRENGTH

cat("\n\nDRUG_STRENGTH\n\n")

drug_strength <- 
  readr::read_delim(paste0(folder, "DRUG_STRENGTH.csv"), delim = "\t", show_col_types = FALSE) %>%
  dplyr::mutate_at(c("drug_concept_id", "ingredient_concept_id", "amount_unit_concept_id", "numerator_unit_concept_id", "denominator_unit_concept_id", "box_size"), as.integer) %>%
  dplyr::mutate_at(c("amount_value", "numerator_value", "denominator_value"), as.numeric) %>%
  dplyr::mutate_at(c("valid_start_date", "valid_end_date", "invalid_reason"), as.character)

cat("\n\n")
import_vocabulary_table(output = output, ns = ns, i18n = i18n, r = r, m = m, vocabulary_id = %vocabulary_id%, table_name = "drug_strength", data = drug_strength) %>% print()