### <i class="fa fa-info-circle" style="color:steelblue;"></i> Introduction

Dans ce tutorial, nous allons voir comment **importer des données** dans l'application.

Nous verrons d'abord comment **créer un set de données** dans LinkR, puis nous **importerons un premier set de donneés**.

Nous **testerons** nos données en **créant une étude**.

Nous importerons ensuite un plus grand set de données au format OMOP, en utilisant les données de la base **MIMIC-IV**.

Nous finirons en **partageant** notre code via notre dépôt git.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_intro.png" alt="Settings icon" style="height:200px;" />

**Plan** :

- Entrepôts de données de santé et modèles de données
- Créer un set de données
- Créer des données au format OMOP
- Importer les données dans LinkR
- Avec un peu plus de tables
- Test avec les données MIMIC-IV
- Afficher nos données
- Partageons notre code

<br /><hr />
<div style = "text-align:center;">
  <div style = "background-color:#0076ba; font-size:16px; font-weight:bold; color:white; font-family: 'Helvetica Neue';
    padding:10px 20px; border-radius:5px; display:inline-block;">Créer un set de données</div>
</div>

### <i class="fa fa-database" style="color:steelblue;"></i> Entrepôts de données de santé et modèles de données

Pour savoir ce qu'est un entrepôt de données de santé, lisez le tutoriel ***Entrepôts de données de santé et collecte des données médicales*** dans la rubrique *Données de santé* de la page *Ressources*.

Lisez également le tutoriel ***Modèles de données***, également dans la rubrique *Données de santé* de la page *Ressources*.

Le modèle de données utilisé par LinkR est le modèle **<a href = "https://ohdsi.github.io/CommonDataModel/" target = "_blank">OMOP</a>**.

<br />
### <i class="fa fa-table" style="color:steelblue;"></i> Créer un set de données

Pour commencer, rendez-vous sur la page ***Set de données***, depuis la page des *Paramètres* en haut à droite de la page.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_settings_icon.png" alt="Settings icon" style="height:50px; border:dashed 1px; margin:5px 0px 5px 0px; padding:5px 0px 5px 0px;" /><br />
<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_settings_menu.png" alt="Settings menu" style="height:400px; border:dashed 1px; margin:5px 0px 5px 0px;" />

Allez ensuite dans l'onglet ***Gestion des sets***, puis créez un nouveau set, que vous appellerez par exemple 'Set de données tutoriel'.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_create_dataset.png" alt="Create a dataset" style="height:200px; border:dashed 1px; margin:5px 0px 5px 0px;" />

Une fois le set de données créé, vous devriez le voir apparaître dans le tableau au milieu de la page.

Sur la ligne correspondant à votre nouveau set de données, cliquez sur l'icône de rouages afin d'**accéder aux options** de votre set.

Choisissez la **version OMOP 6.0** dans le menu déroulant correspondant.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_dataset_options.png" alt="Dataset options" style="height:400px; border:dashed 1px; margin:5px 0px 5px 0px;" />

Pour **en savoir plus sur les options**, cliquez sur le point d'interrogation en haut à droite de la page, puis sur *Options du set*. Il existe **une page d'aide pour chaque onglet**.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_help_icon.png" alt="Help icon" style="height:50px; border:dashed 1px; margin:5px 0px 5px 0px;" /><br />
<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_dataset_help.png" alt="Dataset options" style="height:400px; border:dashed 1px; margin:5px 0px 5px 0px; padding-bottom:5px;" />

Nous allons maintenant pouvoir **éditer le code** de notre set de données. Rendez-vous pour cela dans l'onglet *Editer le code*.

L'éditeur auquel vous avez accès fonctionne comme une **console R**, exécutez le code en cliquant sur 'Exécuter' ou en utilisant les raccourcis :

- CMD/CTRL + SHIFT + ENTER : exécute l'ensemble du code
- CMD/CTRL + ENTER : exécute le code sélectionné
- CMD/CTRL + SHIFT + C : commente le code sélectionné

Pensez à sauvegarder votre code. Vous pouvez également utiliser le raccourci CMD/CTRL + S.

<br /><hr />
<div style = "text-align:center;">
  <div style = "background-color:#feae03; font-size:16px; font-weight:bold; color:white; font-family: 'Helvetica Neue';
    padding:10px 20px; border-radius:5px; display:inline-block;">Importer des données</div>
</div>

### <i class="fa fa-table" style="color:steelblue;"></i> Créer des données au format OMOP

Pour commencer, nous allons créer des **données factices** au **format OMOP**, nous verrons ensuite comment des données réelles via la base de données MIMIC-IV.

Créons une fonction *person*, qui contiendra les **données de 100 patients**.

<pre><code class = "r code_highlight" style = "font-size:12px;">person <- function(){
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
</code></pre>

Rendez-vous sur la page **Editer le code du set**, puis **copiez-y le code** ci-dessus.

En **exécutant** ce code, vous devriez voir apparaître votre set de données de 100 patients en bas de l'écran.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_execute_person_code.png" alt="Result of code execution" style="height:900px; border:dashed 1px; margin:5px 0px 5px 0px; padding:5px 0px 5px 0px;" />

Pour plus d'informations sur la **structure** de la **base de données OMOP**, rendez-vous dans la **page d'aide** ***Modèles de données*** via le point d'interrogation en haut de l'écran. Vous y trouverez un lien vers le <a href = "https://ohdsi.github.io/CommonDataModel/" target = "_blank">site d'OHDSI, détaillant le modèle OMOP</a>.

Regardez la structure de la table *Person*, vous retrouvez bien les colonnes que nous avons créées pour notre variable *person*.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_omop_person_detail.png" alt="OMOP person table documentation" style="height:650px; border:dashed 1px; margin:5px 0px 5px 0px; padding:5px 0px 5px 0px;" />

Attention, les **colonnes** et les **tables** peuvent **changer** en fonction des **versions** d'OMOP.

Par exemple, le version 5.3 ne comprend par la colonne *death_datetime* dans la table *Person*, elle a à la place une table *Death*.

Vous pouvez choisir la version via le menu déroulant en haut de la page présentée ci-dessus.

Voilà **nos données prêtes**, nous allons pouvoir les **importer** dans LinkR.

<br />
### <i class="fa fa-upload" style="color:steelblue;"></i> Importer les données dans LinkR

Pour importer des données dans LinkR, nous allons utiliser la fonction <a href = "https://interhop.frama.io/linkr/linkr/reference/import_dataset.html" target = "_blank">***import_dataset***</a>.

Consultez également la **documentation depuis l'application**, via le point d'interrogation.

La fonction *import_dataset* comprend les arguments suivants :

- *output, ns, i18n, r, d* : qui sont les variables permettant le fonctionnement de l'application
- *dataset_id* : où vous indiquez **l'ID du dataset** actuel, via la balise *%dataset_id%*
- *data* : où vous indiquez la **fonction qui chargera les données** pour une variable (exemple : *person()* de notre code ci-dessus)
- *omop_table* : où vous indiquez la **variable que vous souhaitez importer** (*person*, *measurement*...)
- *omop_version* : où vous indiquez la **version utilisée** du modèle de données OMOP. Utilisez la balise *%omop_version%* qui prendra la version configurée dans les options du set de données.
- *read_with* : indiquez avec quelle **librairie R** vous voulez **lire les données** importées
- *save_as* : indiquez sous quel **format** vous voulez enregistrer les données après les avoir importées
- *rewrite* : indiquez si vous souhaitez **écraser** l'ancien fichier de données pour le remplacer par le nouveau
- *allow_numeric_instead_integer* : indiquez si vous autorisez que les colonnes au format numérique puissent être laissées telles quelles plutôt que converties au format integer
- *allow_dttm_instead_date* : indiquez si vous autorisez que les colonnes au format datetime puissent être laissées telles quelles plutôt que converties au format date

Voici le code qui nous permettra d'importer nos données.

<pre><code class = "r code_highlight" style = "font-size:12px;">import_dataset(
    dataset_id = %dataset_id%, # Cette balise sera remplacée par la valeur du set de données actuellement sélectionné
    data = person(), # En appelant notre fonction person(), nous obtiendrons les données que nous avons créées
    omop_table = %omop_table%, # Cette balise sera remplacée par la valeur de la version OMOP du set de données actuellement sélectionné
    output = output, ns = ns, i18n = i18n, r = r, d = d
)
</code></pre>

<br />
### <i class="fa fa-table" style="color:steelblue;"></i> Avec un peu plus de tables

Tables observation_period, visit_occurrence, visit_detail.

<br />
### <i class="fa fa-vial" style="color:steelblue;"></i> Test avec les données MIMIC-IV

<br /><hr />
<div style = "text-align:center;">
  <div style = "background-color:#1bb100; font-size:16px; font-weight:bold; color:white; font-family: 'Helvetica Neue';
    padding:10px 20px; border-radius:5px; display:inline-block;">Tester notre set</div>
</div>

### <i class="fa fa-eye" style="color:steelblue;"></i> Afficher nos données

Créer une étude ...

<br /><hr />
<div style = "text-align:center;">
  <div style = "background-color:#00a1ff; font-size:16px; font-weight:bold; color:white; font-family: 'Helvetica Neue';
    padding:10px 20px; border-radius:5px; display:inline-block;">Partager notre code</div>
</div>

### <i class="fa fa-share-alt" style="color:steelblue;"></i> Partageons notre code
