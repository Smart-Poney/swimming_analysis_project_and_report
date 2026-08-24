---
title: "README"
author: "FG"
date: "2026-06-23"
output: html_document
---

*swimming_analysis_project_and_report*

# Swimming analysis : project and report (english)

![reconstruction of fast-start actuation](00_documents/illustration_fast_start.png "reconstruction of fast-start actuation")

## Overview

**Resume of the study**:

Morphology is a major physical constraint on fish locomotion. Several swimmers morphotypes are described because of their specialisation in distinct environment with different swimming capacities (periodic swimming, cruisers, manoeuvrers). Therefore, assessing correct differences in fish performances can become a real challenge when observed at the micro-evolutive scale with similar morphology. Here we tried to evaluate possible differences in the steady/unsteady swimming of the species Salmo trutta facing different flow regimes conditions. As floods are predicted to increase in term of intensity and frequency, we tried to detect the impact of this increase through animal experimentation. One group faced fluctuant flow regime while the other stayed in stable conditions. We collected data on steady swimming against a constant flow, and on unsteady swimming mixing behaviour, kinematics and induced escape reactions (fast-start). We measured several performance and behaviour metrics to evaluate the best potential in detecting differences across treatment. Genetic lineage was also considered with high influence factor as the first batch of individuals collected were reared trout and the other one from wild origin. Statistical analysis and modelling showed no significant enough metric to distinct clearly between origin nor treatment for the steady swimming and the behaviour analysis. However, statistical modelling showed high significant importance of genetic origin in fast-start actuation across time suggesting better performances of the wild trout in the early stage of fast start. Treatment remained undetected as a discriminative factor in all analysis suggesting that the treatment was not intense enough or that the experimental conditions allow individuals to find shelters in the semi-natural environment and avoid the treatment effect. This could also indicates high robustness of the developmental patterns. Results could be valuable to pre-evaluate repopulations efficiency of Salmonidae populations in river ecosystems.

**Keywords**: Morpho-functionality, Swimming performances/kinematics, Steady/unsteady performances, Salmo trutta, Micro-evolution

Here is shared: the data used for the analysis, the code for statistical analysis and modelling, the code for using the segmentation algorithm, the code for fast-start animation, the protocols and description of each project (data extraction, data acquisition, analysis of steady swimming, analysis of unsteady behaviour, analysis of fast-start, comparison of digitising method, and the segmentation method).

Softwares/languages used (**R**, **Python**, **MATLAB**, **ImageJ**, **Avidemux**, **EthoVisionXT**)

------------------------------------------------------------------------

## Repository structure

``` text
swimming_analysis_project_and_report/
│
├── 00_documents/                             # reports and presentation
├── 01_fast_start_extraction/                 # workflow for extracting the fast-start sequences
├── 02_fast_start_acquisition/                # workflow to digitize the fast-start sequences using MATLAB
├── 03.1_ImageJ_acquisition/                  # workflow to digitize the fast-start sequences using ImageJ for method comparison
├── 03.2_MATLAB_acquisition/                  # workflow to digitize the fast-start sequences using MATLAB for method comparison
├── 03.3_R_comparison_digitalization_method/  # project to compare variability of the digitizing method
├── 04_Python_segmentation_algorithm/         # workflow to create segmentation of the fish kinematics
├── 05_R_analysis_Ptut                        # project to analyse steady swimming <UPPA students>
├── 06_R_fast_start_analysis.R                # project to analyse fast start variability
├── 07_EthoVisionXT_information               # workflow and parameters to use EthoVisionXT tracking software
├── 08_R_tracking_EthoVisionXT_analysis       # project to analyse behaviour variability
├── 09_R_fish_silhouette_animation            # workflow to animate fish kinematics during behaviour/fast start events
├── README_FR.md                              # french version
└── README_ENG.md
```

------------------------------------------------------------------------

## Additional Information

The study report `00_documents/Study of the effect of hydrological events on the steady unsteady swimming perfomances of Salmo trutta - Guillaud Felix.pdf` documents the work and analyses carried out during this project, including all information and resources required to perform the analyses. Additionally, two `.pdf` presentations are provided: one summarizing the method comparison (benchmark) and the other outlining the study as a whole. Two `.RSTHEME` files are also included for optional use (these only modify the appearance of RStudio).

Each folder/directory is structured consistently to facilitate reproducibility and includes a *README* description in either french or english version.

The `05_R_analysis_Ptut` directory was created and developed as part of a collaborative project between INRAE and students from UPPA University.

------------------------------------------------------------------------

## Author

Felix Guillaud

Mail: [felix.guillaud.work\@gmail.com](mailto:felix.guillaud.work@gmail.com){.email}

## License

This project is distributed Under the MIT license.

------------------------------------------------------------------------

# Analyse de nage : projet et rapport (français)

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
