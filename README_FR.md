---
title: "README_FR"
author: "FG"
date: "2026-06-23"
output: html_document
---

*swimming_analysis_project_and_report*

# Analyse de nage : projet et rapport

![reconstruction of fast-start actuation](00_documents/illustration_fast_start.png "reconstruction of fast-start actuation")

## Présentation

**Resumé de l'étude**:

On distingue plusieurs morphotypes adaptés à des environnements distincts. Evaluer les différences de performance entre les individus peut s'avérer complexe à l'échelle microévolutive. Nous avons cherché à évaluer les différences potentielles dans la nage entre stabilité et manoeuvrabilité chez la truite commune (Salmo trutta) soumise à différents régimes hydrologiques. Face aux augmentations de l'intensité et de la fréquence des crues, nous avons testé leurimpact via l’expérimentation animale. Un groupe est soumis à un régime hydrologique fluctuant, tandis que l'autre est maintenu en conditions stables. Des données sont recueillies sur les performances de stabilité, et de manoeuvrabilité, incluant comportement et réactions de fuite. Plusieurs paramètres de performance et de comportement ont été mesurés afin d'évaluer leur potentiel de détection des différences entre les traitements. L'origine génétique a été considérée (élevage vs. sauvage). L'analyse et la modélisation statistique n'ont révélé aucun paramètre suffisamment significatif pour distinguer clairement le traitement parmi les individus pour l'analyse de stabilité/manoeuvrabilité et pour l'analyse comportementale. Cependant, la modélisation statistique a révélé une importance significative de l'origine génétique dans les réponses de fuite, suggérant de meilleures performances des truites sauvages lors de la phase initiale du fast-start. Le traitement n'a pas été identifié comme un facteur discriminant dans toutes les analyses, ce qui laisse penser qu'il était insuffisant ou que les conditions expérimentales permettaient aux individus de trouver refuge dans le milieu seminaturel. Ces résultats pourraient s'avérer pertinents pour pré-évaluer l'efficacité des efforts de repeuplements concernant les populations de Salmonidae dans les rivières.

**Mots-clés** : Morpho-fonctionnalité, Compromis de nage stabilité/manoeuvrabilité, Performances/cinématiques de nage, Micro-evolutive scale, Salmo trutta

Sont mis à disposition ici : les données utilisées pour l'analyse, le code d'analyse statistique et de modélisation, le code d'utilisation de l'algorithme de segmentation, le code pour l'animation des démarrages rapides, ainsi que les protocoles et la description de chaque projet (extraction et acquisition des données, analyse de la nage en régime établi, analyse des comportements en régime transitoire, analyse des démarrages rapides, comparaison des méthodes de numérisation et méthode de segmentation).

Logiciels/languages utilisés (**R**, **Python**, **MATLAB**, **ImageJ**, **Avidemux**, **EthoVisionXT**)

------------------------------------------------------------------------

## Structure du répertoire

``` text
swimming_analysis_project_and_report/
│
├── 00_documents/                             # rapports et presentation
├── 01_fast_start_extraction/                 # processus pour extraire les séquences de fast-start
├── 02_fast_start_acquisition/                # processus pour digitaliser les séquences de fast-start en utilisant MATLAB
├── 03.1_ImageJ_acquisition/                  # processus pour digitaliser les séquences de fast-start en utilisant ImageJ pour la comparaison de méthode
├── 03.2_MATLAB_acquisition/                  # processus pour digitaliser les séquences de fast-start en utilisant MATLAB pour la comparaison de méthode
├── 03.3_R_comparison_digitalization_method/  # projet pour comparer la variabilité des méthodes
├── 04_Python_segmentation_algorithm/         # processus pour recréer une segmentation pendant les cinématiques de mouvements
├── 05_R_analysis_Ptut                        # projet pour analyser la nage steady (stabilité) <UPPA étudiants>
├── 06_R_fast_start_analysis.R                # projet pour analyser la variabilité des évenements de fast start
├── 07_EthoVisionXT_information               # processus et paramètres pour utiliser le logiciel de tracking EthoVisionXT
├── 08_R_tracking_EthoVisionXT_analysis       # projet pour analyser la variabilité de comportement
├── 09_R_fish_silhouette_animation            # processus pour animer les cinématiques de poisson pendant les phases de comportement ou de fast start
├── README_FR.md                              
└── README_ENG.md                             # english version
```

------------------------------------------------------------------------

## Informations supplémentaires

Le rapport de l'étude `00_documents/Study of the effect of hydrological events on the steady unsteady swimming perfomances of Salmo trutta - Guillaud Felix.pdf` rend compte du travail et des analyses effectuées pendant ce projet avec toutes les informations et ressources nécessaires à la réalisation des analyses. En plus sont fournies deux présentations `.pdf` synthétisant la comparaison des méthodes (benchmark) et l'étude dans on ensemble. Deux thèmes `.RSTHEME` sont aussi fournis pour être réutilisés ou non (modifie uniquement l'aspect de RStudio).

Chaque dossier/répertoire est construit de la même manière pour faciliter la reproductibilité, et accomapgné d'une description *README* en version française ou anglaise.

Le répertoire `05_R_analysis_Ptut` a été réalisé et construit lors d'un projet collaboratif entre INRAE et par des étudiants de l'université UPPA. 


------------------------------------------------------------------------

## Auteur

Felix Guillaud

Mail: [felix.guillaud.work\@gmail.com](mailto:felix.guillaud.work@gmail.com){.email}

## License

Ce projet est distribué sous la licence MIT.
