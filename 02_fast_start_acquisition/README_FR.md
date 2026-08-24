---
title: "README_FR"
author: "FG"
date: "2026-06-23"
output: html_document
---

# fast_start_acquisition

## Présentation

Utiliser les séquences de fast start extraites avec '01_fast_start_extraction' pour digitaliser les body midlines. Obtenir des données de coordonnées spatiales en utilisant la méthode MATLAB (cf rapport). Pour la partie extraction des séquences, se référer à `01_fast_start_extraction\README.md`.

------------------------------------------------------------------------

## Protocole

``` text
1) Copier tous les sous-dossiers du dossier '01_fast_start_extraction' hormis les fichiers 'README.txt' et '\videos':
'\date'                 #pour chaque jour d'enregistrement
'\date\heure'           # pour chaque poisson
'\labeled-data\194_fs2' # pour chaque fast start, ici  le poisson ID = 194, 2eme fast start

2) Créer le dossier :
'02_fast_start_acquisition\data_matlab' # qui contiendra toutes les données acquises

3) Importer les fichiers suivants depuis <https://zenodo.org/records/4623882> d'après l'article de Di Santo et al., 2021
'CurveMapper6.m' # contient le code de 'CurveMapper4.m' et est appelé via Matlab avec '>>CurveMapper4'
'CurveMapper5.doc' # pour les instructions

3.2) (facultatif) Ajouter :
     (facultatif) 'CurveMapper4Mod.m' # obtenir 15 points avec MATLAB
     (facultatif) 'remove_png.py' # fourni avec le README.txt pour supprimer les fichiers inutiles sorties par Matlab

4) Modifier le fichier 'CurveMapper4.m', cf. plus bas 'MODIFICATION DU FICHIER 'CurveMapper4.m'

5) Protocole d'utilisation de MATLAB :
- Lancer MATLAB, attendre l'apparition  de '>>' dans la console et taper 'CurveMapper4' pour 200 points ou 'CurveMapperMod' pour 15 points
- Sélectionner 'Multiple Frame'
- Sélectionner la vidéo d'intérêt
- Décocher 'Unit Conversion'
- Le dossier de sortie est déjà \data_matlab, vérifier
- Le fichier de sortie est déjà correctement nommé ID_fs1 ou ID_fs2 etc..., vérifier
- Start Frame : 1 ; Increment : 1 ; End Frame : chercher dans 'labeled-data' le sous dossier contenant le nombre de frames de la vidéo d'intérêt
- 'GO'
- Utiliser 'clique gauche' pour ajouter un point, 'clique droit' pour retirer le dernier point, 'entrée' pour passer à la frame suivante
- Utiliser '-' et '=' pour zoomer/dézoomer (dépend du clavier, '-'/'+')
- Pour tous les autres détails de la méthode d'acquisition, se référer au document 'CurveMapper5.doc' par G. Lauder
- Fermer l'image et ouvrir la séquence suivante
```

------------------------------------------------------------------------

## Information supplémentaires

Test de variabilité opérateur MATLAB :

``` text
rep 1 ; rep 2 ; rep 3 -> répétition acquisition fast start 206_fs1 ; 206_fs2 ; 206_fs3 ; 207_fs1 ; 207_fs2 ; 207_fs3 ; 208_fs1 ; 208_fs2 ; 208_fs3 par FG pour comparer la variabilité intra opérateur
rep 4 - 15 pts        -> acquisition des même fast start avec 'CurvemapperMod' pour acquérir 15 pts et comparer avec méthode Matthias ImageJ
clara ; lili ; maxime -> acquisition fast start 206_fs1 ; 206_fs2 ; 206_fs3 pour comparer la variabilité inter opérateur
```

Test de durée - chronomètre :

``` text
Felix :
3 fasts start sur Matlab avec 12 frames chacun en utilisant 'CurveMapperMod' -> 11 minutes 35.01 secondes, total 36 frames soit ~19.3 spf (second per frame)
3 fasts start sur Matlab avec 10 frames chacun en utilisant 'CurveMapperMod' -> 10 minutes 05.18 secondes, total 30 frames soit ~20.2 spf
3 fasts start sur Matlab avec ~11.7 frames chacun en utilisant 'CurveMapper4' -> 10 minutes 09.99 secondes, total 35 frames soit ~17.4 spf
3 fasts start sur Matlab avec ~11.7 frames chacun en utilisant 'CurveMapper4' -> 08 minutes 42.56 secondes, total 35 frames soit ~13.2 spf
3 fasts start sur Matlab avec 11 frames chacun en utilisant 'CurveMapper4' -> 09 minutes 14.33 secondes, total 33 frames soit ~16.8 spf
3 fasts start sur Matlab avec ~11.7 frames chacun en utilisant 'CurveMapper4' -> 09 minutes 17.05 secondes, total 35 frames soit ~15.9 spf

autre opérateur :
3 fasts start sur Matlab avec 12 frames chacun en utilisant 'CurveMapper4' -> 15 minutes 53.11 secondes soit ~26.5 spf <- Clara
3 fasts start sur Matlab avec 12 frames chacun en utilisant 'CurveMapper4' -> 16 minutes 16.50 secondes soit ~27.1 spf <- Lili
```

------------------------------------------------------------------------

## Modification du fichier `CurveMapper4.m`

``` matlab
ligne 387, dans le bloc else, remplacer :

        else
            s.FileName= FileName;
            s.PathName= PathName;
            s.OutputFileName= [s.FileName '_CURVES.xls'];
            s.OutputPathName= s.PathName;
            s.MovObj= VideoReader([PathName FileName]);

par :

        else
            s.FileName= FileName;
            s.PathName= PathName;
            %MODIFICATION : next line is added to define the name wihtout '.avi'
            [~ ,fileNameNoExt, ~] = fileparts(FileName);
            %MODIFICATION : Original line -> s.OutputFileName= [s.FileName '_CURVES.xls']; 
            s.OutputFileName= [fileNameNoExt '.xls'];
            s.OutputPathName = 'C:\ [your_path_here] \data_matlab\';
            %MODIFICATION : Original line -> s.OutputPathName= s.PathName;
            s.MovObj= VideoReader([PathName FileName]);

et remplacer [your_path_here] par le chemin d'accès au dossier racine pour sauvegarder directement les sorties dans \data_matlab

ligne 488, pour éviter de sauver des fichiers supplémentaires prenant un temps de calcul pas nécessaire pour tous les projets, remplacer :

                saveas(gcf,[s.OutputPathName s.OutputFileName '.jpg'],'jpg')
                saveas(gcf,[s.OutputPathName s.OutputFileName '.emf'],'emf')

par :

                %MODIFICATION : added percentage to avoid loading files not needed (jpg and emf)
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.jpg'],'jpg')
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.emf'],'emf')

ligne 533, pour éviter de sauver des fichiers supplémentaires prenant un temps de calcul pas nécessaire pour tous les projets, remplacer :

                saveas(gcf,[s.OutputPathName s.OutputFileName '.jpg'],'jpg')
                saveas(gcf,[s.OutputPathName s.OutputFileName '.emf'],'emf')

par :

                %MODIFICATION : added percentage to avoid loading files not needed (jpg and emf)
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.jpg'],'jpg')
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.emf'],'emf')

ligne 570, pour éviter de sauver des fichiers supplémentaires prenant un temps de calcul pas nécessaire pour tous les projets, remplacer :

                saveas(gcf,[s.OutputPathName s.OutputFileName '.jpg'],'jpg')
                saveas(gcf,[s.OutputPathName s.OutputFileName '.emf'],'emf')

par :

                %MODIFICATION : added percentage to avoid loading files not needed (jpg and emf)
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.jpg'],'jpg')
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.emf'],'emf')
```

toutes les modifications sont indexées avec '%MODIFICATION' permettant de facilement les retrouver dans le code

Fin de modification du fichier `CurveMapper4.m`.

------------------------------------------------------------------------


## Auteur

Felix Guillaud

Mail: [felix.guillaud.work\@gmail.com](mailto:felix.guillaud.work@gmail.com){.email}

## License

Ce projet est distribué sous la licence MIT.

