---
title: "README_FR"
author: "FG"
date: "2026-06-23"
output: html_document
---

# Python_segmentation_algorithm

## Présentation 

Ce répertoire contient le workflow permettant d’utiliser la méthode **Segment Growing Method (SGM)** (`algorithms_segmentgrowing`) fournie par Robert Sterling (<ros46@aber.ac.uk>). L’algorithme est utilisé via plusieurs scripts Python personnalisés afin de produire une segmentation de la ligne médiane du corps du poisson au cours d’un événement de *fast-start*. La SGM est normalement utilisée lors de séquences de nage stabilisée, et **5 segments** suffisent généralement à capturer **99 %** des mouvements ondulatoires des poissons (sub-)carangiformes (Fetherstonhaugh *et al.*, 2021). Dans ce projet, nous utilisons la SGM afin d’étudier le nombre de segments ainsi que leur taille nécessaires pour reconstruire l’activation en *fast-start* de juvéniles subcarangiformes (*Salmo trutta*). L’algorithme reconstruit en moyenne **12 segments**, avec une longueur décroissante de la tête vers la queue, configuration utilisée dans les analyses ultérieures. 

------------------------------------------------------------------------

## Structure du projet

``` text
Python_segmentation_algorithm/
│
├── data_matlab/      # données brutes MATLAB
├── output/           # animations, graphiques, tables
├── scripts/          # Python scripts, incluant 'algorithms_segmentgrowing'
│
├── README_FR.md      
└── README_ENG.md     # english version
```

------------------------------------------------------------------------

## Processus

Nous utilisons les données brutes acquises avec la méthode de digitalisation MATLAB (voir `/fast_start_acquisition/README_FR.txt`) qui fournissent les coordonnées spatiales de 200 points le long de la ligne médiane du corps.

Dans le répertoire Python_segmentation_algorithm, ouvrir l’invite de commande.

Run:

``` python
python scripts\formatage.py             # formater les fichiers pour utiliser SGM
python scripts\auto_seg_norm.py         # appeler SGM et définir automatiquement la valeur seuil
python scripts\animated_seg.py          # pour animer chaque fast_start avec origine fixe
python scripts\animation_segments.py    # pour animer chaque fast_start dans l'espace
python scripts\animated_glob.py         # pour animer toutes les séquences de fast_start
python scripts\animated_glob_paused.py  # pour animer toutes les séquences de fast_start en gardant une trace de chacun
```

En addition, pour tester l'importance de la valeur seuil, Run :

``` python
python scripts\test_valeur_thresh.py  # génère les graphiques pour chaque seuil de 0.1 à 20 (indentation = 0.1)
python scripts\animation.py           # produit 'animation_thresh.GIF'
```

Si l'on souhaite refaire toutes les étapes avec une valeur seuil précise, Run : 

``` python
...
python scripts\auto_seg.py         # to call the SGM and define the threshold value
...
```


------------------------------------------------------------------------

## Output

In the output file, you'll find:

``` text
Python_segmentation_algorithm/output
│
├── animation/            # animations GIF pour chaque fast-start acquis et les animations globales
├── formated_data/        # les données fomatées
├── frames/               # les segments recrées sur chaque frame de chaque fast-start
├── segments/             # les coodronnées de chaque segment/joints + le joints_summary.csv
└── value_threshold/      # les graphes utilisés pour visualiser l'importance de la valeur de seuil
```

------------------------------------------------------------------------

## Python dépendances

Packages externes requis :

- numpy
- pandas
- matplotlib
- imageio
- tqdm

Installation:

```bash
pip install numpy pandas matplotlib imageio tqdm
```
Ou:

Install dependencies with:

```bash
pip install -r requirements.txt
```


NOTE : si cela ne marche pas, essayer d'importer les fonction/package en utilisant l'invite commande dans le menu démarrer, Run :
```python 
python -m pip install 'package'
```

------------------------------------------------------------------------

## Auteur

Felix Guillaud

Mail: [felix.guillaud.work\@gmail.com](mailto:felix.guillaud.work@gmail.com){.email}

## License

Ce projet est distribué sous licence MIT.





