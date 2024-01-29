<span style = "color:grey;">Auteur : Boris Delange</span><br />
<span style = "color:grey;">Dernière modification : 29/01/2023</span>

### <i class="fa fa-cogs" style="color: brackets-curly;"></i> Introduction

Ce tutoriel a pour but de vous apprendre à **créer un plugin** dans LinkR.

Nous commencerons par définir **ce qu'est un plugin**.

Nous **détaillerons** le plugin que nous voulons **développer**.

Nous commencerons par créer l'**interface utilisateur**. Cette interface sera ensuite rendue fonctionnelle grâce au développement de la **logique serveur**.

Nous remplirons le fichier de **traductions**, ce qui facilitera le **partage** de notre plugin.

Après avoir **testé** notre plugin, nous le mettrons à disposition sur notre **dépôt git**.

*Image plan*

**Plan** :

- Qu'est-ce qu'un plugin ?
- Spécifications de notre plugin
- Créons notre plugin
- UI - Interface utilisateur / frontend
- Serveur - backend
- Traductions
- Testons notre plugin
- Partageons notre plugin

### Qu'est ce qu'un plugin ?

Un plugin est un **script** composé de code écrit avec la librairie **Shiny** en R, permettant d'**ajouter des fonctionnalités** à l'application.

Par exemple, le plugin 'Séries temporelles {dygraphs}' présenté ci-dessous permettra aux utilisateurs d'afficher les données de leurs patients sous forme de **séries temporelles**, à l'aide de la librairie R <a href="https://rstudio.github.io/dygraphs/" target="_blank">{dygraphs}</a>.

*Screenshots dygraphs (DocumentCard et utilisation)*

Un plugin est composé de **trois parties** :

- **Interface utilisateur** (user interface - UI) : il s'agit du *front-end*, vous développerez ici l'interface graphique à laquelle l'utilisateur aura accès
- **Server** : il s'agit du *backend*, vous développerez ici la logique côté serveur du plugin : la manipulation des données etc
- **Traductions** : il s'agit d'un fichier CSV qui comprendra les traductions de l'interface utilisateur

Réfléchissons maintenant à ce que notre plugin sera capable de faire.

### Spécifications de notre plugin

Je pars d'une **idée simple** : je me dis que ce serait bien que, quand je veux visualiser une variable, je ne sois pas obligé de réécrire le code à chaque fois.

Pourquoi ne pas **créer** une **interface graphique** où je n'aurai qu'à **choisir ma variable** dans un menu déroulant pour l'**afficher** ?

Et voilà, j'ai l'idée, il n'y a plus qu'à **rendre cela possible** en créant un plugin !

Je dois faire un premier **choix** : s'agit-il d'un plugin de données individuelles (patient par patient) ou agrégées (sur un groupe de patients) ?

Il est plus fréquent de vouloir visualiser la distribution d'une variable sur un groupe de patients plutôt que sur un patient seul, nous allons donc créer un **plugin de données agrégées**.

Ensuite, à quoi devra ressembler mon **interface graphique** ?

J'aimerais bien séparer l'écran en deux, à gauche on visualiserait mon **histogramme**, et à droite on pourrait régler les paramètres de ma figure, avec un menu déroulant pour **choisir la variable** et un champ pour choisir le **nombre de barres** sur mon histogramme.

*Screenshot dessin avec les cadres du plugin*

**Côté serveur** maintenant.

Un histogramme n'est pas adapté pour visualiser tout type de données : je pourrai visualiser la distribution de données numériques, et de données catégorielles à condition que le nombre de catégories ne soit pas trop important.

Pour simplifier, je ne vais autoriser l'**affichage que des données numériques**. Je vais donc restreindre l'affichage à la variable *d$measurement*.

Pour rappel, le modèle de données utilisé par LinkR est le modèle **<a href = "https://ohdsi.github.io/CommonDataModel/" target = "_blank">OMOP</a>**. Consultez cette page pour en savoir plus sur la table measurement. Il s'agit de la table qui comprend les données de laboratoires et les constantes physiologiques.

Lorsque je vais changer la variable du nombre de barre de mon histogramme, les **modifications** devront être prises en compte **après validation**, pour ne pas réaliser de calculs inutiles. Je devrai également donner des bornes de valeurs possibles.

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

### Créons notre plugin

Rendez-vous sur la page *Plugins > Données individuelles* puis sur l'onglet *Gestion de plugins*.

*Screenshot*



### UI - Interface utilisateur

### Serveur - backend

### Traductions

### Partageons notre plugin
