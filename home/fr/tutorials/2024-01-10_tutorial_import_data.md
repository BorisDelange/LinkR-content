<span style = "color:grey;">Auteur : Boris Delange</span><br />
<span style = "color:grey;">Dernière modification : 10/01/2023</span>

### <i class="fa fa-info-circle" style="color:steelblue;"></i> Introduction

Dans ce tutoriel, nous allons voir comment **importer des données** dans LinkR.

Nous verrons d'abord comment **créer un set de données**, puis nous **importerons** un premier **set de donneés**.

Nous **testerons** nos données en **créant une étude**.

Nous importerons ensuite un plus grand set de données au format OMOP, en utilisant les données de la base **MIMIC-IV**.

Nous finirons en **partageant** notre code via notre dépôt git.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_intro.png" alt="Settings icon" style="height:200px;" />

**Plan** :

- Entrepôts de données de santé et modèles de données
- Créer un set de données
- Créer des données au format OMOP
- Importer les données dans LinkR
- Test avec les données de la base MIMIC-IV
- Afficher nos données
- Partageons notre code

<br /><hr />
<div style = "text-align:center;">
  <div style = "background-color:#0076ba; font-size:16px; font-weight:bold; color:white; font-family: 'Helvetica Neue';
    padding:10px 20px; border-radius:5px; display:inline-block;">Créer un set de données</div>
</div>

### <i class="fa fa-database" style="color:steelblue;"></i> Entrepôts de données de santé et modèles de données

Pour savoir ce qu'est un entrepôt de données de santé, lisez le tutoriel ***Entrepôts de données de santé et collecte des données médicales*** dans la rubrique *Données de santé* de la page *Ressources*.

Lisez également le tutoriel ***Modèles de données*** dans la même rubrique.

Le modèle de données utilisé par LinkR est le modèle **<a href = "https://ohdsi.github.io/CommonDataModel/" target = "_blank">OMOP</a>**.

<br />
### <i class="fa fa-table" style="color:steelblue;"></i> Créer un set de données

Pour commencer, rendez-vous sur la page ***Set de données***, depuis la page *Paramètres* en haut à droite de l'écran.

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

L'éditeur auquel vous avez accès fonctionne comme une **console R**, exécutez le code en cliquant sur *Exécuter* ou en utilisant les raccourcis :

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

Pour commencer, nous allons créer des **données factices** au **format OMOP**, nous verrons ensuite comment importer les données la base MIMIC-IV.

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

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_omop_version.png" alt="Result of code execution" style="height:180px; border:dashed 1px; margin:5px 0px 5px 0px; padding:0px 0px 5px 0px;" />

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
- *allow_numeric_instead_integer* : indiquez si vous autorisez que les colonnes au format numérique soient laissées telles quelles plutôt que converties au format integer
- *allow_dttm_instead_date* : indiquez si vous autorisez que les colonnes au format datetime soient laissées telles quelles plutôt que converties au format date

Voici le code qui nous permettra d'importer nos données.

<pre><code class = "r code_highlight" style = "font-size:12px;">import_dataset(
    dataset_id = %dataset_id%, # Cette balise sera remplacée par la valeur du set de données actuellement sélectionné
    data = person(), # En appelant notre fonction person(), nous obtiendrons les données que nous avons créées
    omop_table = "person", # Le nom de la table OMOP que nous souhaitons importer
    omop_version = %omop_version%, # Cette balise sera remplacée par la valeur de la version OMOP du set de données actuellement sélectionné
    output = output, ns = ns, i18n = i18n, r = r, d = d
)
</code></pre>

Vous devriez voir apparaître un message vous indiquant que vos données ont bien été importées.

Si vous cochez 'Afficher les données importées' en dessous de l'éditeur de code, vous verrez **combien de lignes** sont **importées** par table OMOP.

Nous voyons que nous avons bien **importé 100 lignes** dans la table person.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_table_imported_data.png" alt="Table showing imported data" style="height:250px; border:dashed 1px; margin:5px 0px 5px 0px; padding:5px 0px 5px 0px;" />

A chaque fois que vous chargerez un set de données depuis la page *Données*, c'est ce **code** qui sera **exécuté**.

On comprend alors l'intérêt d'**utiliser une fonction** pour charger nos données : les données ne seront chargées **que si la fonction est appelée**.

Si j'ai utilisé l'argument *save_as*, par exemple avec la valeur 'csv', mes données seront enregistrées au format CSV si le fichier n'existe pas déjà.

La fonction *person()* ne sera appelée que la première fois : elle ne sera plus appelée si le fichier CSV correspondant à notre table (*person.csv*) existe.

Si les données que j'importe ont changé entre-temps, je peux toujours **remplacer le fichier** CSV en mettant l'argument *rewrite* à 'TRUE'.

Voici un exemple.

<pre><code class = "r code_highlight" style = "font-size:12px;">import_dataset(
    dataset_id = %dataset_id%,
    data = person(),
    omop_table = "person",
    omop_version = %omop_version%,
    save_as = "csv", # Les données contenues dans la fonction person() seront sauvegardées au format CSV
    read_with = "vroom", # Les données enregistrées en CSV seront lues avec la librairie vroom
    rewrite = FALSE, # Si le fichier person.csv existe dans le dossier de notre set de données, le fichier existant sera conservé
    output = output, ns = ns, i18n = i18n, r = r, d = d
)
</code></pre>

Si maintenant les données contenues dans la fonction *person()* changent, par exemple parce qu'il s'agit d'une connexion à une base de données avec des données mises à jour régulièrement, je peux vouloir remplacer la fichier *person.csv* existant.

Je mettrai l'argument *rewrite* à TRUE pour remplacer le fichier, puis modifierai de nouveau l'argument *rewrite* pour FALSE, afin que la fonction chargeant les données ne soit pas exécutée à chaque fois.

<pre><code class = "r code_highlight" style = "font-size:12px;">import_dataset(
    dataset_id = %dataset_id%,
    data = person(),
    omop_table = "person",
    omop_version = %omop_version%,
    save_as = "csv",
    read_with = "vroom",
    rewrite = TRUE, # Je modifie cet argument juste une fois, le temps que le fichier CSV soit remplacé avec mes nouvelles données
    output = output, ns = ns, i18n = i18n, r = r, d = d
)
</code></pre>

Voyons maintenant les **différentes façons** d'**importer des données** dans LinkR.

La fonction qui charge nos données (par exemple la fonction *person()*) doit charger les données sous forme :

- de data.frame
- de tibble
- de lazy tibble, dans le cas d'une connexion à une base de données

Je décide alors si je veux **sauvegarder ces données**, et si oui avec **quel format**.

L'argument *save_as* peut prendre les valeurs suivantes : 'none' (par défaut), 'csv' et 'parquet'.

Je décide ensuite avec **quelle librairie** je veux **lire ces données**, avec l'argument *read_with*.

*read_with* peut prendre les valeurs suivantes : 'none', 'vroom', 'duckdb', 'spark' et 'arrow'.

Toutes les **associations** entre *save_as* et *read_width* ne sont pas possibles.

Voici les associations possibles (entre *read_with* et *save_as*) :

- vroom / csv
- arrow / parquet
- duckdb / csv
- duckdb / parquet
- duckdb / none
- spark / csv
- spark / parquet
- spark / none

L'avantage du format *parquet* est que c'est un **format de stockage optimisé** pour les **grands volumes** de données.

Utiliser *duckdb* permet de ne **pas charger toutes les données en mémoire**, les données ne seront chargées qu'au moment de la "collecte".

Nous pouvons ainsi filtrer nos données sans charger les tables entières, ce qui **optimise les performances**.

En pratique :

- **chargez des données** à partir de **bases de données** autant que possible : ceci permet de charger le moins possible les données en mémoire, les performances seront optimisées
- si vous avez besoin de **stocker les données** localement, par exemple si vous devez faire des modifications sur les données après les avoir chargées depuis une base de données, utilisez le stockage par ***parquet*** et la lecture par ***duckdb***
- si vous avez besoin de **réaliser du calcul distribué** sur **plusieurs serveurs**, utilisez la lecture par ***spark***, avec une connexion à une base de données (argument 'none' pour *save_as*)

<br />
### <i class="fa fa-database" style="color:steelblue;"></i> Test avec les données de la base MIMIC-IV

Nous allons maintenant charger des données depuis la **base de données <a href = "https://mimic.mit.edu/" target = "_blank">MIMIC-IV</a>**.

Il s'agit d'une base de données des services de soins intensifs du BIDMC (Beth Israel Deaconess Medical Center).

Nous avons accès publiquement aux **données de 100 patients** via <a href = "https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/" target = "_blank">ce lien</a>.

Nous allons **importer** quelques-unes de ces tables puis **tester nos données** en **créant une étude** au sein de l'application.

Commençons avec la table ***person***.

<pre><code class = "r code_highlight" style = "font-size:12px;">
person <- function(){
  # Chargement de la table person.csv depuis le site physionet.org
  vroom::vroom("https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/person.csv", progress = FALSE) %>%
    dplyr::mutate(person_id = 1:dplyr::n()) # Les index de person_id n'étant pas adéquats, nous les réindexons de 1 à 100
}

import_dataset(
    dataset_id = %dataset_id%,
    data = person(),
    omop_table = "person",
    omop_version = %omop_version%,
    output = output, ns = ns, i18n = i18n, r = r, d = d
)
</pre></code>

Si vous exécutez ce code depuis le même set de données que nous avons créé au début du tutoriel, vous devriez avoir ce message d'erreur :

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_error_message_1.png" alt="Settings icon" style="height:70px; border:dashed 1px; margin:5px 0px 5px 0px; padding:5px 0px 5px 0px;" />

Ceci est dû au fait que les colonnes de notre table *person* ne correspondent pas avec la version OMOP sélectionée.

Allez dans les options, **changez la version OMOP** pour 5.3, pensez à **sauvegarder** les options puis **exécutez** de nouveau le **code**.

Vous devriez avoir un nouveau message d'erreur, vous indiquant que la colonne *ethnicity_source_concept_id* doit être de type integer.

En effet, cette colonne a été chargée au format *numeric*, étant donné que l'on n'a pas précisé à la fonction *vroom* quels étaient les types de colonnes attendus (ce que l'on peut faire avec l'argument *col_types*).

Deux solutions :

- soit nous changeons la colonne pour la transformer en *integer*
- soit nous acceptons de charger des colonnes au format *numeric* plutôt qu'*integer* avec l'argument *allow_numeric_instead_integer*

Pourquoi accepter le format *numeric* plutôt qu'*integer* ?

Parfois, en chargeant des données depuis une base de données, il est impossible de transformer les types de colonnes, en fonction de la librairie utilisée pour la connexion à la base de données.

Pour notre exemple, il est plus simple de préciser le **type de colonne attendu** pour chaque colonne.

<pre><code class = "r code_highlight" style = "font-size:12px;">person <- function(){
    # Utilisation de l'argument col_types en précisant le type attendu pour chaque colonne
    vroom::vroom("https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/person.csv", col_types = "niiiiTiiiiiccicici", progress = FALSE) %>%
        dplyr::mutate(person_id = 1:dplyr::n())
}

import_dataset(
    dataset_id = %dataset_id%,
    data = person(),
    omop_table = "person",
    omop_version = %omop_version%,
    output = output, ns = ns, i18n = i18n, r = r, d = d
)
</pre></code>

Le chargement devrait se faire correctement.

Chargeons maintenant les **autres tables**.

<pre><code class = "r code_highlight" style = "font-size:12px;">data <- list()

data$person <- function(){
    # Utilisation de l'argument col_types en précisant le type attendu pour chaque colonne
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
        data = data[[omop_table]](),
        omop_table = omop_table,
        omop_version = %omop_version%,
        read_with = "vroom",
        save_as = "csv",
        # Dans la variable visit_detail, il reste la colonne visit_occurrence_id qui est au format numeric, que nous n'avons pas modifiée
        # Nous autorisons donc le chargement de cette colonne au format numeric plutôt que integer, parce que cette colonne ne nous sera pas utile dans notre exemple
        # En pratique, il faut s'efforcer d'obtenir le bon type de colonne lorsque cela est possible
        allow_numeric_instead_integer = TRUE,
        output = output, ns = ns, i18n = i18n, r = r, d = d
    )
}
</pre></code>

Nous avons donc **chargé** les tables ***person*** et ***visit_detail***, qui sont les tables contenant les patients et les séjours dans les services hospitaliers.

Nous pourrions également charger les autres tables. Consultez pour cela le code du set de données ***MIMIC-IV demo*** qui a été téléchargé lors du premier chargement de l'application.

Nous allons maintenant **créer une étude** pour afficher nos données.

<br /><hr />
<div style = "text-align:center;">
  <div style = "background-color:#1bb100; font-size:16px; font-weight:bold; color:white; font-family: 'Helvetica Neue';
    padding:10px 20px; border-radius:5px; display:inline-block;">Tester notre set</div>
</div>

### <i class="fa fa-eye" style="color:steelblue;"></i> Afficher nos données

Commençons par **créer une étude**.

Rendez-vous sur la page ***Mes études*** depuis le menu *Données* en haut de l'écran.

Chargez le set de données que nous venons de créer en le sélectionnant dans le menu déroulant à gauche de l'écran.

Allez dans l'onglet ***Gestion des études*** puis créez une étude, par exemple 'Etude test'.

De la même façon que pour les sets de données, vous pouvez accéder aux **options de l'étude** avec l'icône de rouages, et vous pouvez consulter les **pages d'aides** en cliquant sur le point d'interrogation en haut à droite de l'écran.

Une fois l'**étude créée**, allez sur la page ***Accéder aux données*** depuis le menu *Données*.

Vous aurez une **étude vide**, qui est séparée en deux parties :

- **Données individuelles** : où vous accéderez aux données patient par patient, le but est de construire ici l'équivalent d'un dossier médical pour consulter les données de chaque patient
- **Données agrégées** : où vous réaliserez les différentes étapes de votre étude, telles que la visualisation de la distribution des données, l'exclusion des données aberrantes, la réalisation des statistiques etc

A gauche de l'écran, sélectionnez les **données agrégées**.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_selected_aggregated_data.png" alt="Selected aggregated data button" style="height:300px; border:dashed 1px; margin:5px 0px 5px 0px; padding:5px 0px 5px 0px;" />

Créez un onglet en cliquant sur ***Ajouter un onglet***, nommez-le 'Démographie' par exemple.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_create_tab.png" alt="Create a new tab" style="height:300px; border:dashed 1px; margin:5px 0px 5px 0px; padding:5px 0px 5px 0px;" />

Une fois l'onglet créé, ajoutez un **widget** en cliquant sur *Ajouter un widget*.

Le principe est simple :

- Vous choisissez un **nom**
- Vous choisissez un **plugin** dans le menu déroulant
- Vous choisissez les **concepts** que vous voulez afficher : choisissez une terminologie puis choisissez les concepts

Vous trouverez **plus d'informations** sur les widgets et les plugins dans la page d'aide via le point d'interrogation, ou dans les tutoriels dédiés depuis la page *Accueil*.

Nous devons donc **choisir un plugin**.

Interrompons la création de notre widget le temps de **télécharger** le plugin *Données démographiques* depuis le **dépôt git d'InterHop**.

Rendez-vous sur la page *Plugins > Données agrégées* en haut de l'écran.

Depuis l'onglet *Tous les plugins*, choisissez les plugins sur dépôt git distant, et choisissez 'Interhop' dans le menu déroulant.

Vous allez voir tous les plugins de données agrégées présents sur le dépôt git d'InterHop.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_plugins_catalog.png" alt="Remote git plugins catalog" style="height:600px; border:dashed 1px; margin:5px 0px 5px 0px; padding:5px 0px 5px 0px;" />

Cliquez sur le plugin 'Données démographiques', puis cliquez sur *Installer le plugin*.

Retournons maintenant sur notre étude.

**Ajoutez un widget** en sélectionnant le plugin que nous venons d'installer.

Il n'y a pas besoin de sélectionner de concepts pour ce plugin.

Cliquez sur *Ajouter le widget*.

Vous devriez voir apparaître la **distribution de l'âge et du sexe** des patients de votre set de données.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_demographics_widget.png" alt="Demographics widget" style="height:700px; border:dashed 1px; margin:5px 0px 5px 0px; padding:5px 0px 5px 0px;" />

Essayez maintenant de télécharger le **plugin 'Console R'** et d'ajouter un widget avec.

Vous pouvez afficher les données dans la console via ce code.

<pre><code class = "r code_highlight" style = "font-size:12px;">d$person
</pre></code>

Pour plus d'informations sur l'accès aux données de notre set depuis la console, rendez vous sur la page d'aide 'Modèle de données'.

Vous devriez maintenant être en mesure d'**importer des données** depuis **n'importe quelle source** !

Comme tout travail réalisé sur LinkR, nous pouvons le **partager**, ce que nous allons faire dans le prochain paragraphe.

<br /><hr />
<div style = "text-align:center;">
  <div style = "background-color:#00a1ff; font-size:16px; font-weight:bold; color:white; font-family: 'Helvetica Neue';
    padding:10px 20px; border-radius:5px; display:inline-block;">Partager notre code</div>
</div>

### <i class="fa fa-share-alt" style="color:steelblue;"></i> Partageons notre code

Il n'est pas toujours utile de partager le code source d'un script d'import de données.

Cela peut être utile si vous importez une **base de données** qui est **accessible à plusieurs personnes**, notamment pour les bases de données "ouvertes" (sous réserve d'une demande sur leurs sites respectifs), telles que la MIMIC ou AmsterdamUMCdb.

Il peut également être utile de placer votre script sur un dépôt git privé à usage personnel, afin de **faire une sauvegarde** et de récupérer ce code facilement.

Pour cela, allez dans les paramètres en cliquant sur l'onglet de rouages en haut à droite de la page.

Cliquez sur *Dépôts git distants* à gauche, puis sur l'onglet *Ajouter un dépôt git > Avec un lien*.

Vous devrez au préalable avoir créé un dépôt git (sur gitlab, framagit ou github par exemple).

Il peut être public, dans ce cas il n'y a pas besoin d'ajouter une clef API ici, qui sert à la lecture.

Il peut aussi être privé, vous devrez dans ce cas ajouter une **clef API de lecture** (il ne faut pas une clef d'écriture ici).

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_add_git_repo.png" alt="Add a git repo" style="height:500px; border:dashed 1px; margin:5px 0px 5px 0px; padding:5px 0px 5px 0px;" />

Une fois le dépôt git ajouté, allez sur l'onglet *Modifier un dépôt git*.

Sélectionnez votre dépôt git, ajoutez une clef API : cette fois il s'agit d'une **clef de lecture et d'écriture**..

Sélectionnez la catégorie *Sets de données*, sélectionnez votre set dans le menu *Ajouter des fichiers*, puis cliquez sur *Ajouter*.

Votre set s'affichera dans le tableau.

Il ne reste plus qu'à **réaliser un commit**, en écrivant un message dans *Message de commit* puis en cliquant sur *Commit & push*.

Pour s'assurer que cela a fonctionné, rechargez l'application puis allez sur la page *Paramètres > Sets de données*.

En sélectionnant votre dépôt git, vous devriez **voir apparaître votre set de données**, ce qui signifie qu'il sera accessible à toute personne ayant l'adresse de votre git et, s'il s'agit d'un dépôt privé, d'une clef API.

<img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_import_data_remote_git_datasets.png" alt="Remote git datasets" style="height:400px; border:dashed 1px; margin:5px 0px 5px 0px; padding:5px 0px 5px 0px;" />

Pour plus d'informations sur la gestion des dépôts git depuis LinkR, consultez le **tutoriel dédié**.

<br /><hr />
<div style = "text-align:center;">
  <div style = "background-color:#0076ba; font-size:16px; font-weight:bold; color:white; font-family: 'Helvetica Neue';
    padding:10px 20px; border-radius:5px; display:inline-block;">Conclusion</div>
</div>

Vous disposez maintenant de tous les éléments pour importer vos données sur LinkR.

Efforcez-vous au maximum d'**importer** des données **depuis une connexion** à **une base de données** plutôt qu'avec des fichiers, cela **améliorera les performances** de l'application.

Si vous partagez le code du script de l'import de données, attention à **ne pas y laisser de logs** de connexion.

Si vous avez des questions ou des remarques sur ce tutoriel, **contactez-nous** à l'adresse suivante : <a href="mailto:linkr-app@pm.me">linkr-app@pm.me</a>.
