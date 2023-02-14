# Create a function which, when executed, gets thesaurus data
vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/icu/d_items.csv.gz") %>%
  dplyr::transmute(item_id = itemid, category, name = label, display_name = "", unit = unitname) %>%
  dplyr::bind_rows(
    vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/hosp/d_labitems.csv.gz") %>%
      dplyr::transmute(item_id = itemid, category, name = label, display_name = "", unit = "")
  ) %>%
  dplyr::mutate_at("item_id", as.integer) -> thesaurus

thesaurus %>%
  dplyr::bind_rows(
    vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/icu/icustays.csv.gz") %>%
      dplyr::distinct(first_careunit) %>%
      dplyr::transmute(item_id = 1:dplyr::n() + max(thesaurus$item_id) + 100000, category = "Unit", name = first_careunit, display_name = "", unit = "")
  ) %>%
  dplyr::mutate_at("item_id", as.integer)

categories <- 
  thesaurus %>%
  dplyr::distinct(category) %>%
  dplyr::select(name = category) %>%
  dplyr::mutate(item_id = 1:dplyr::n() + max(thesaurus$item_id) + 200000, .before = "name") %>%
  dplyr::mutate(display_name = "", unit = "") %>%
  dplyr::mutate_at("item_id", as.integer)

thesaurus <- thesaurus %>%
  dplyr::bind_rows(categories) %>%
  dplyr::mutate_at("item_id", as.integer)

thesaurus_mapping <- 
  thesaurus %>% 
  dplyr::rename(item_id_1 = item_id) %>%
  dplyr::left_join(
    # 2 = included_in
    categories %>% dplyr::transmute(item_id_2 = item_id, relation_id = 2, category = name),
    by = "category"
  ) %>%
  dplyr::select(item_id_1, relation_id, item_id_2) %>%
  dplyr::mutate_at(c("item_id_1", "relation_id", "item_id_2"), as.integer)

thesaurus_import <- 
  thesaurus %>%
  dplyr::select(item_id, name, display_name, unit)

# Run import_thesaurus function
import_thesaurus_new(output = output, ns = ns, r = r, category = "items", thesaurus_id = %thesaurus_id%, thesaurus = thesaurus_import, i18n = i18n)

# Import thesaurus mappings
import_thesaurus_new(output = output, ns = ns, r = r, category = "items_mapping", thesaurus_id = %thesaurus_id%, thesaurus = thesaurus_mapping, i18n = i18n)