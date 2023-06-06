### <i class="fa fa-list" style="color: steelblue;"></i> Concepts

Allez dans l'onglet *Terminologies* en haut de la page puis dans l'onglet *Concepts*.

En choisissant une terminologie, vous pouvez afficher les **concepts** composant cette terminologie.

Vous pouvez lez **trier** par nombre de patients ayant ce concept au moins une fois, ou par nombre de lignes au total dans le set de données.

En cliquant sur un concept, vous pouvez **afficher ses détails**, avec notamment la distribution des valeurs du concept dans le set de données.

### <i class="fa fa-arrows-left-right" style="color: steelblue;"></i> Alignement de concepts

Rendez-vous dans l'onglet *Alignement de concepts*.

Vous pouvez :

- <i class="fa fa-plus" style="color: steelblue;"></i> Ajouter de nouveaux alignements
- <i class="fa fa-thumbs-up" style="color: steelblue;"></i> Evaluer des alignements
- <i class="fa fa-trash" style="color: steelblue;"></i> Supprimer des alignements

#### <i class="fa fa-plus" style="color: steelblue;"></i> Ajouter un alignement

Pour ajouter un alignement, allez dans l'onglet *Ajouter* puis sélectionnez deux terminologies (elles peuvent être distinctes ou non).

Classiquement, une terminologie **non-standard** est alignée à une terminologie **standard**. Dans ce cas, choisissez la terminologie non-standard à gauche.

<i class="fa fa-check" style="color: steelblue;"></i> ***Terminologie de gauche***

Vous pouvez filtrer la terminologie de gauche en cochant *Concepts non alignés uniquement*, afin de retirer les concepts déjà alignés. Cela concerne *tous les alignements de concepts*, qu'ils aient été importés ou qu'ils aient été ajoutés par les utilisateurs de LinkR.

Le nombre de lignes pour la terminologie de gauche concerne le **set de données sélectionné**.

<i class="fa fa-check" style="color: steelblue;"></i> ***Terminologie de droite***

Vous pouvez filtrer la terminologie de droite en cochant *N'afficher que les concepts utilisés*, afin de retirer les concepts qui n'ont jamais été utilisés dans **aucun set de données** chargé sur l'application.

Le nombre de lignes pour la terminologie de droite concerne l'**ensemble des sets de données** chargés sur l'application.

Est également renseigné le **nombre de sets de données** contenant chaque concept.

L'intérêt est de pouvoir identifier rapidement les **concepts fréquemment utilisés**.

En effet, il est parfois difficile d'identifier le **concept standard** correspondant au concept que l'on veut aligner.

Par exemple, la requête <a href = "https://athena.ohdsi.org/search-terms/terms?vocabulary=LOINC&page=1&pageSize=15&query=heart+rate" target = "_blank">"heart rate" dans Athena</a>, en filtrant sur la terminologie LOINC, affiche **plusieurs milliers** de résultats. Il est difficile de savoir lequel choisir.

Dans LinkR, en sélectionnant la terminologie LOINC, en cherchant "heart rate" et en triant par le nombre de lignes ou de set de données, j'obtiens le **concept adéquat** en première position.

<i class="fa fa-check" style="color: steelblue;"></i> ***Relation entre les concepts***

Choisissez ensuite la relation entre le concept de gauche (concept 1) et le concept de droite (concept 2), via le menu déroulant *Le concept 1 par rapport au concept 2*.

Les relations suivantes sont disponibles :

- **Correspond à (non-std vers std)** : le concept 1 est équivalent au concept 2, le concept 1 n'est pas standard, le concept 2 l'est
- **Correspond à (std vers non-std)** : c'est l'inverse, le concept 1 est standard, le concept 2 ne l'est pas
- **Est inclus dans** : le concept 1 est inclus dans le concept 2
- **Inclut** : le concept 1 inclut le concept 2

#### <i class="fa fa-thumbs-up" style="color: steelblue;"></i> Evaluer des alignements

Pour évaluer des alignements, allez dans l'onglet *Evaluer & éditer*.

Vous pouvez :

- filtrer les alignements que vous n'avez pas encore évalués
- afficher le détail de l'alignement de concept sélectionné

Cliquez sur <i class="fa fa-thumbs-up"></i> pour valider un alignement conforme et sur <i class="fa fa-thumbs-down"></i> pour l'invalider.

Les alignements qui comportent plus d'évaluations négatives que positives ou aucune évaluation positive ne seront pas utilisés dans l'application.

Les **alignements validés** (avec au moins une évaluation positive et plus d'évaluations positives que négatives) seront automatiquement utilisés par l'application, dans la création de widgets et de plugins.

N'oubliez pas de **sauvegarder** vos évaluations.

#### <i class="fa fa-trash" style="color: steelblue;"></i> Supprimer des alignements

Pour supprimer des alignements, rendez-vous dans l'onglet *Evaluer & éditer*.

Si vous n'avez pas coché *Afficher le détail de l'alignement sélectionné*, vous pouvez sélectionner plusieurs alignements pour les supprimer, en cliquant sur *Supprimer la sélection*.

Vous pouvez également supprimer les alignements via l'icône <i class="fa fa-trash-can"></i> dans le tableau.
