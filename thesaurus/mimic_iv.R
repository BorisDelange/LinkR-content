# Create a function which, when executed, gets thesaurus data
thesaurus <- function(){
  vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/icu/d_items.csv.gz") %>%
    dplyr::transmute(item_id = itemid, name = label, display_name = "", category, unit = unitname) %>%
    dplyr::bind_rows(
      vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/hosp/d_labitems.csv.gz") %>%
        dplyr::transmute(item_id = itemid, name = label, display_name = "", category, unit = "")
    ) %>%
    dplyr::mutate_at("item_id", as.integer) -> temp
  
  temp %>%
    dplyr::bind_rows(
      vroom::vroom("https://physionet.org/files/mimic-iv-demo/1.0/icu/icustays.csv.gz") %>%
        dplyr::distinct(first_careunit) %>%
        dplyr::transmute(item_id = 1:dplyr::n() + max(temp$item_id) + 100000, name = first_careunit, display_name = "", category = "Hospital units", unit = "")
    ) %>%
    dplyr::mutate_at("item_id", as.integer)
}

# Run import_thesaurus function
import_thesaurus(output = output, r = r, thesaurus_id = %thesaurus_id%, thesaurus = thesaurus(), language = language)