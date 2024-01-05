### <i class="fa fa-info-circle" style="color:steelblue;"></i> Présentation

Dans ce tutorial, nous allons voir comment **importer des données** dans l'application.

### <i class="fa fa-hospital-o" style="color:steelblue;"></i> Entrepôts de données de santé et modèles de données

Pour savoir ce qu'est un entrepôt de données de santé, lisez le tutoriel ***Entrepôts de données de santé et collecte des données médicales*** dans la rubrique ***Données de santé*** de la page *Ressources*.

Lisez égalemet le tutoriel *Modèles de données*, également dans la rubrique *Données de santé* de la page *Ressources*.

Le modèle de données utilisé par LinkR est le modèle **<a href = "https://ohdsi.github.io/CommonDataModel/" target = "_blank">OMOP</a>**.

### <i class="fa fa-plus-square" style="color:steelblue;"></i> Créer un set de données

Pour commencer, rendez-vous sur la page *Set de données*, depuis la page des *Paramètres* en haut à droite de la page.

*Screenshot*

Allez ensuite dans l'onglet *Gestion des sets de données*, puis créez un nouveau set, que vous appellerez par exemple 'Set de données tutoriel'.

*Screenshot*

Une fois le set de données créé, vous devriez le voir apparaître dans le tableau au milieu de la page.

Sur la ligne correspondant à votre nouveau set de données, cliquez sur l'icône de rouages afin d'**accéder aux options** de votre set.

Choisissez la version OMOP 6.0 dans le menu déroulant correspondant.

Pour en savoir plus sur les options, cliquez sur le point d'interrogation en haut à droite de la page, puis sur *Options du set*. Il existe une page d'aide pour chaque onglet.

*Screenshot*

Nous allons maintenant pouvoir **éditer le code** de notre set de données. Rendez-vous pour cela dans l'onglet *Editer le code*.

L'éditeur auquel vous avez accès fonctionne comme une **console R**, executez le code en cliquant sur 'Exécuter' ou en utilisant les raccourcis :

- CMD/CTRL + SHIFT + ENTER : exécute l'ensemble du code
- CMD/CTRL + ENTER : exécute le code sélectionné
- CMD/CTRL + SHIFT + C : commente le code sélectionné

Pensez à sauvegarder votre code. Vous pouvez également utiliser le raccourdi CMD/CTRL + S.

### <i class="fa fa-puzzle-piece" style="color:steelblue;"></i> Créer des données OMOP

Pour commencer, nous allons créer des données factices au format OMOP, nous verrons ensuite comment des données réelles via la base de données MIMIC-IV.

Créons une fonction *person*, qui contiendra les données de 100 patients.

```{r}
person <- function(){
  tibble::tibble(
    person_id = 1:100,
    gender_concept_id = sample(c(8507L, 8532L), 100, replace = TRUE),
    year_of_birth = sample(1920:2010, 100, replace = TRUE),
    month_of_birth = sample(1:12, 100, replace = TRUE),
    day_of_birth = sample(1:28, 100, replace = TRUE),
    race_concept_id = NA_integer_,
    ethnicity_concept_id = NA_integer_,
    location_id = sample(1:10, 100, replace = TRUE),
    provider_id = sample(1:10, 100, replace = TRUE),
    care_site_id = sample(1:10, 100, replace = TRUE),
    person_source_value = paste("Source", 1:100),
    gender_source_value = NA_character_,
    gender_source_concept_id = NA_integer_,
    race_source_value = NA_character_,
    race_source_concept_id = NA_integer_,
    ethnicity_source_value = NA_character_,
    ethnicity_source_concept_id = NA_integer_
  ) %>%
  dplyr::mutate(
      birth_datetime = lubridate::ymd_hms(paste0(paste(year_of_birth, month_of_birth, day_of_birth, sep = "-"), " 00:00:00")),
      death_datetime = dplyr::case_when(runif(100) < 2/3 ~ as.POSIXct(NA), TRUE ~ birth_datetime + lubridate::years(sample(30:80, 100, replace = TRUE))),
      .after = "day_of_birth"
  )
}

person()
```

En exécutant ce code, vous devriez voir apparaître votre set de données de 100 patients en bas de l'écran.

*Screenshot*

Pour plus d'informations sur la structure de la base de données OMOP, rendez-vous dans la page d'aide *Modèles de données* via le point d'interrogation en haut de l'écran. Vous y trouverez un lien vers le site d'OHDSI, détaillant le modèle OMOP.

Regardez la structure de la table *Person*, vous retrouvez bien les colonnes que nous avons créées pour notre variable *person*.

*Screenshot*

Attention, les colonnes et les tables peuvent changer en fonction des versions d'OMOP.

Par exemple, le version 5.3 ne comprend par la colonne *death_datetime* dans la table *Person*, elle a à la place une table *Death*.

*Screenshot* (pour afficher version 5.3)

Voilà nos données prêtes, nous allons pouvoir les importer dans LinkR.

### <i class="fa fa-upload" style="color:steelblue;"></i> Importer les données dans LinkR

### <i class="fa fa-eye" style="color:steelblue;"></i> Afficher nos données

Créer une étude ...

### <i class="fa fa-table" style="color:steelblue;"></i> Avec un peu plus de tables

Tables observation_period, visit_occurrence, visit_detail.

### <i class="fa fa-vial" style="color:steelblue;"></i> Test avec les données MIMIC-IV

### <i class="fa fa-share-alt" style="color:steelblue;"></i> Partageons notre code