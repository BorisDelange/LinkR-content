<span style = "color:grey;">Auteur : Boris Delange</span><br />
<span style = "color:grey;">Dernière modification : 29/01/2023</span>

<h3><i class="fa fa-info-circle" style="color:steelblue;"></i> Introduction</h3>

Ce tutoriel a pour but de vous apprendre à **créer un plugin** dans LinkR.

Nous commencerons par définir **ce qu'est un plugin**.

Nous **détaillerons** le plugin que nous voulons **développer**.

Nous commencerons par créer l'**interface utilisateur**. Cette interface sera ensuite rendue fonctionnelle grâce au développement de la **logique serveur**.

Nous remplirons le fichier de **traductions**, ce qui facilitera le **partage** de notre plugin.

Après avoir **testé** notre plugin, nous le mettrons à disposition sur notre **dépôt git**.

<a href="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_create_plugins_intro.png" target = "_blank"><img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_create_plugins_intro.png" alt="Tutorial plan" style="width:700px;" /></a>

**Plan** :

- Qu'est-ce qu'un plugin ?
- Spécifications du plugin
- Creation du plugin
- UI - Interface utilisateur / frontend
- Serveur - backend
- Traductions
- Tester le plugin
- Partager le plugin

<br /><hr />
<h2 style = "text-align:center;">
  <div style = "background-color:#0076ba; font-size:16px; font-weight:bold; color:white;
    padding:10px 20px; border-radius:5px; display:inline-block;">Qu'est-ce qu'un plugin ?</div>
</h2>

<h3><i class="fa fa-question-circle" style="color:steelblue;"></i> Qu'est-ce qu'un plugin ?</h3>

Un plugin est un **script** composé de code écrit avec la librairie **Shiny** en R, permettant d'**ajouter des fonctionnalités** à l'application.

Par exemple, le plugin 'Séries temporelles {dygraphs}' présenté ci-dessous permettra aux utilisateurs d'afficher les données de leurs patients sous forme de **séries temporelles**, à l'aide de la librairie R <a href="https://rstudio.github.io/dygraphs/" target="_blank">{dygraphs}</a>.

<a href="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_create_plugins_plugin_card.png" target = "_blank"><img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_create_plugins_plugin_card.png" alt="Dygraphs plugin" style="width:300px;" /></a>

<a href="https://framagit.org/interhop/linkr/LinkR/-/raw/master/man/figures/dygraphs_plugin.gif" target = "_blank"><img src="https://framagit.org/interhop/linkr/LinkR/-/raw/master/man/figures/dygraphs_plugin.gif" alt="Dygraphs plugin" style="width:1000px;" /></a>

Un plugin est composé de **trois parties** :

- **Interface utilisateur** (user interface - UI) : il s'agit du *front-end*, vous développerez ici l'interface graphique à laquelle l'utilisateur aura accès
- **Server** : il s'agit du *backend*, vous développerez ici la logique côté serveur du plugin : la manipulation des données etc
- **Traductions** : il s'agit d'un fichier CSV qui comprendra les traductions de l'interface utilisateur

Passons maintenant aux **spécifications** de notre plugin.

<br /><hr />
<h2 style = "text-align:center;">
  <div style = "background-color:#feae03; font-size:16px; font-weight:bold; color:white;
    padding:10px 20px; border-radius:5px; display:inline-block;">Création du plugin</div>
</h2>

<h3><i class="fa fa-list-ul" style="color:steelblue;"></i> Spécifications du plugin</h3>

J'aimerais créer une **interface graphique** qui me permette de **visualiser** la **distribution d'une variable**, sous forme d'un **histogramme**.

Il n'y a plus qu'à **rendre cela possible** en créant un plugin !

Je dois faire un premier **choix** : s'agit-il d'un plugin de données individuelles (patient par patient) ou agrégées (sur un groupe de patients) ?

Il est plus fréquent de vouloir visualiser la distribution d'une variable sur un groupe de patients plutôt que sur un patient seul, nous allons donc créer un **plugin de données agrégées**.

Ensuite, à quoi devra ressembler mon **interface graphique** ?

J'aimerais bien séparer l'écran en deux, à gauche on visualiserait mon **histogramme**, et à droite on pourrait régler les paramètres de ma figure, avec un menu déroulant pour **choisir la variable** et un champ pour choisir le **nombre de barres** sur mon histogramme.

<a href="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_create_plugins_plugin_schema.png" target = "_blank"><img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_create_plugins_plugin_schema.png" alt="Schema of the plugin" style="width:800px;" /></a>

**Côté serveur** maintenant.

Un histogramme n'est pas adapté pour visualiser tout type de données : je pourrai visualiser la distribution de données numériques, et de données catégorielles à condition que le nombre de catégories ne soit pas trop important.

Pour simplifier, je ne vais autoriser l'**affichage que des données numériques**. Je vais donc restreindre l'affichage à la variable *d$measurement*.

Pour rappel, le modèle de données utilisé par LinkR est le modèle **<a href = "https://ohdsi.github.io/CommonDataModel/" target = "_blank">OMOP</a>**. Consultez <a href = "https://ohdsi.github.io/CommonDataModel/cdm60.html#MEASUREMENT">cette page</a> pour en savoir plus sur la table measurement. Il s'agit de la table qui comprend les données de **laboratoires** et les **constantes physiologiques**.

Lorsque je vais changer la variable du **nombre de barres** de mon histogramme, les **modifications** devront être prises en compte **après validation**, pour ne pas réaliser de calculs inutiles. Je devrai également donner des bornes de valeurs possibles.

Résumons donc les **spécifications** de notre plugin :

- **Côté UI**
  - Visualisation de l'histogramme à gauche de l'écran
  - Paramètres à droite de l'écran
    - Variable à afficher
    - Nombre de barres composant l'histogramme, avec des bornes inférieure et supérieure
    - Validation des modifications
- **Côté serveur**
  - N'autoriser que les données de la variable *d$measurement*
  - Modifier le nombre de barres de l'histogramme en fonction de la valeur renseignée
  - Lancer le code du plot une fois le bouton de validation cliqué

Nous voilà prêts pour **aller coder tout ça** !

<h3><i class="fa fa-cogs" style="color:steelblue;"></i> Création du plugin</h3>

Rendez-vous sur la page *Plugins > Données individuelles* puis sur l'onglet *Gestion des plugins*.

Appelez-le 'Histogramme' par exemple.

<a href="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_create_plugins_create_plugin.png" target = "_blank"><img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_create_plugins_create_plugin.png" alt="Create a dataset" style="width:800px; border:dashed 1px black; margin:5px 0px 5px 0px;" /></a>

Une fois le plugin créé, vous devriez le voir apparaître dans le tableau au milieu de la page.

Sur la ligne correspondant à votre nouveau plugin, cliquez sur l’icône de rouages afin d’**accéder aux options** de votre plugin.

<a href="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_create_plugins_plugin_options.png" target = "_blank"><img src="https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/fr/tutorials/tutorial_create_plugins_plugin_options.png" alt="Create a dataset" style="width:800px; border:dashed 1px black; margin:5px 0px 5px 0px;" /></a>

Pour **en savoir plus sur les options**, cliquez sur le point d’interrogation en haut à droite de la page, puis sur *Options du plugin*.

Nous allons maintenant pouvoir <strong>éditer le code</strong> de notre plugin. Rendez-vous pour cela dans l'onglet *Editer le code*.<br />

L'éditeur auquel vous avez accès fonctionne comme une <strong>console R</strong>, exécutez le code en cliquant sur <em>Exécuter</em> ou en utilisant les raccourcis :
<ul>
<li>CMD/CTRL + SHIFT + ENTER : exécute l'ensemble du code</li>
<li>CMD/CTRL + ENTER : exécute le code sélectionné</li>
<li>CMD/CTRL + SHIFT + C : commente le code sélectionné</li>
</ul><br />
Pensez à sauvegarder votre code. Vous pouvez également utiliser le raccourci CMD/CTRL + S.

<h3><i class="fa fa-desktop" style="color:steelblue;"></i> UI - Interface utilisateur / frontend</h3>

<h3><i class="fa fa-server" style="color:steelblue;"></i> Serveur - backend</h3>

<h3><i class="fa fa-language" style="color:steelblue;"></i> Traductions</h3>

<br /><hr />
<h2 style = "text-align:center;">
  <div style = "background-color:#1bb100; font-size:16px; font-weight:bold; color:white;
    padding:10px 20px; border-radius:5px; display:inline-block;">Test du plugin</div>
</h2>

<h3><i class="fa fa-check-square" style="color:steelblue;"></i> Tester le plugin</h3>

<br /><hr />
<h2 style = "text-align:center;">
  <div style = "background-color:#00a1ff; font-size:16px; font-weight:bold; color:white;
    padding:10px 20px; border-radius:5px; display:inline-block;">Partage du plugin</div>
</h2>

<h3><i class="fa fa-share-alt" style="color:steelblue;"></i> Partager le plugin</h3>
