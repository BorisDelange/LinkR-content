data <- list()

data$person <- function(){
    # Utilisation de l'argument col_types en précisant le type de colonne attendu pour chaque colonne
    vroom::vroom("https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/person.csv", col_types = "niiiiTiiiiiccicici", progress = FALSE) %>%
        dplyr::mutate(person_id = 1:dplyr::n())
}

data$visit_detail <- function(){
    vroom::vroom("https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/visit_detail.csv", col_types = "nniDTDTiiniinciccin", progress = FALSE) %>%
        # Nous faisons une jointure avec la table person afin de récupérer les person_id que nous avons modifiés
        dplyr::left_join(
            vroom::vroom("https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/person.csv", progress = FALSE) %>%
            dplyr::transmute(person_id, new_person_id = 1:dplyr::n()),
            by = "person_id"
        ) %>%
        dplyr::relocate(new_person_id, .before = "person_id") %>%
        dplyr::select(-person_id) %>%
        dplyr::rename(person_id = new_person_id) %>%
        # Les colonnes ne sont pas dans l'ordre dans le CSV importé, nous les remettons à la bonne place
        dplyr::relocate(visit_detail_source_value, visit_detail_source_concept_id, .after = "care_site_id") %>%
        dplyr::relocate(admitting_source_value, admitting_source_concept_id, discharge_to_source_value, discharge_to_concept_id, .after = "visit_detail_source_concept_id") %>%
        # Nous modifions visit_detail_id, de la même façon que nous avons modifié person_id plus tôt
        dplyr::mutate(visit_detail_id = 1:dplyr::n())
}

data$death <- function(){
    vroom::vroom("https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/death.csv", col_types = "nDTiici", progress = FALSE) %>%
        dplyr::left_join(
            vroom::vroom("https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/person.csv", progress = FALSE) %>%
            dplyr::transmute(person_id, new_person_id = 1:dplyr::n()),
            by = "person_id"
        ) %>%
        dplyr::relocate(new_person_id, .before = "person_id") %>%
        dplyr::select(-person_id) %>%
        dplyr::rename(person_id = new_person_id)
}

for (omop_table in c("person", "visit_detail", "death")){ # Nous créons une boucle pour appliquer la variable import_dataset à chacune de nos tables
    if (omop_table != "person") cat("\n")
    cat(paste0(strong(toupper(omop_table)), "\n\n"))
    import_dataset(
        dataset_id = %dataset_id%,
        data = data[omop_table],
        omop_table = omop_table,
        omop_version = %omop_version%,
        read_with = "vroom",
        save_as = "csv",
        # Dans la variable visit_detail, il reste la colonne visit_occurrence_id qui est au format numeric, que nous n'avons pas modifiée
        # Nous autorisons donc le chargement de cette colonne au format numeric plutôt que integer, parce que cette colonne ne nous sera pas utile dans notre exemple
        # En pratique, il faut s'efforcer d'obtenir le bon type de colonne lorsque c'est possible
        allow_numeric_instead_integer = TRUE,
        output = output, ns = ns, i18n = i18n, r = r, d = d
    )
}
