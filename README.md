---
editor_options: 
  markdown: 
    wrap: sentence
---

# Bayacao

## Description

Bayacao Transit est un projet d'analyse et de visualisation des données de transactions et de performances des producteurs de cacao.
Ce projet utilise R avec Shiny et flexdashboard pour fournir des outils interactifs permettant de visualiser les variations et les performances des acheteurs et des producteurs de cacao.

## Structure du Projet

Le projet est structuré comme suit :

-   **datasets/** : Contient les fichiers de données Excel et shapefiles.

-   **dom_shp/** : Contient des fichiers shapefiles.

-   **html_dependencies/** : Contient les fichiers HTML pour les dépendances.

-   **kml/** : Contient les fichiers KML.

-   **README.md** : Documentation du projet.

-   **Rmd/** : Contient les fichiers R Markdown pour les rapports et les visualisations.

-   **Rscripts/** : Contient des scripts R pour différentes tâches.

-   **run_dashboard.R** : Script pour exécuter le tableau de bord.

-   **www/** : Contient les fichiers CSS pour le style du projet.

## Installation 

Pour utiliser ce projet, assurez-vous que les packages suivants sont installés :

```         
install.packages(c("shiny", "flexdashboard", "plotly", "ggplot2", "dplyr", "tidyr", "data.table"))
```

## Utilisation

-   **Charger les données** : Placez vos fichiers de données dans les répertoires appropriés (`datasets/`, `dom_shp/`, etc.).

-   **Exécuter le Dashboard** :
    Ouvrez le fichier `run_dashboard.R` dans RStudio et exécutez-le.
    Ce script lancera l'application Shiny.

    ```         
    source("run_dashboard.R")
    ```

```{=html}
<!-- -->
```
-   **Accéder au Dashboard** :
    Une fois le script exécuté, le tableau de bord sera disponible à l'adresse locale indiquée dans la console R.

-   **Interagir avec le Dashboard** :

    -   **Sélecteurs** : Utilisez les sélecteurs de conditions, périodes et acheteurs pour filtrer les données.

    -   **Graphiques** : Visualisez les données via des graphiques interactifs créés avec `plotly`.

    -   **Tableaux** : Consultez les tableaux récapitulatifs pour obtenir des insights détaillés.

## Fonctionnalités

-   **Analyse des acheteurs** : Comparez les différences de poids acheteur par acheteur et explorez les variations au fil du temps.

-   **Analyse des producteurs** : Classez les producteurs selon les kilos vendus et les différences en kilos.

-   **Filtres dynamiques** : Sélectionnez des périodes, des conditions et des acheteurs pour ajuster les visualisations en temps réel.

## Contribuer

Les contributions sont les bienvenues !
Pour contribuer à ce projet :

1.  Forkez le dépôt.

2.  Créez une branche pour votre fonctionnalité (`git checkout -b feature/AmazingFeature`).

3.  Commitez vos changements (`git commit -am 'Add some AmazingFeature'`).

4.  Poussez la branche (`git push origin feature/AmazingFeature`).

5.  Ouvrez une Pull Request.

## Licence

Ce projet est sous la licence MIT.
Voir le fichier [Licence] pour plus de détails.
