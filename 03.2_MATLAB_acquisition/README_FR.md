---
title: "README_FR"
author: "FG"
date: "2026-06-23"
output: html_document
---

# MATLAB_acquisition

## Présentation

Acquisition des coordonnées de body midlines en utilisant MATLAB pour tester la variabilité inter-opérateur et la variabilité intra-opérateur. Le but étant de comparer les méthodes de digitalisation pour sélectionner l'outil le plus adapté à nos données. Le même protocole a été appliqué pour `03.1_ImageJ_acquisition` pour comparer la variabilité inter-méthode. Si possible, espacer les mesures en mesurant la variabilité intra opérateur (éviter biais habituation)

------------------------------------------------------------------------

## Structure

``` text
rep 1 ; rep 2 ; rep 3 -> répétition acquisition fast start 206_fs1 ; 206_fs2 ; 206_fs3 ; 207_fs1 ; 207_fs2 ; 207_fs3 ; 208_fs1 ; 208_fs2 ; 208_fs3 par FG pour la variabilité intra opérateur
rep 4 - 15 pts        -> acquisition des même fast start avec « CurvemapperMod » pour acquérir 15 points et comparaison avec la méthode ImageJ (cf report)
clara ; lili ; maxime -> acquisition fast start 206_fs1 ; 206_fs2 ; 206_fs3 pour comparer de la variabilité inter-opérateur
```

------------------------------------------------------------------------

## Protocole 
(pour plus de détails, cf `02_fast_start_acquisition/README_FR.txt`) :

``` text
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

## Test de durée

``` text
FG :
3 fasts start sur Matlab avec 12 images chacun, en utilisant « CurveMapperMod »  -> 11 minutes 35.01 seconds, total 36 frames, or ~19.3 spf (seconds per frame)
3 fasts start sur Matlab avec 10 images chacun, en utilisant « CurveMapperMod »  -> 10 minutes 05.18 seconds, total 30 frames, or ~20.2 spf
3 fasts start sur Matlab avec ~11,7 images chacun, en utilisant « CurveMapper4 » -> 10 minutes 09.99 seconds, total 35 frames, or ~17.4 spf
3 fasts start sur Matlab avec ~11,7 images chacun, en utilisant « CurveMapper4 » -> 8 minutes 42.56 seconds, total 35 frames, or ~13.2 spf
3 fasts start sur Matlab avec 11 images par séquence avec « CurveMapper4 »       -> 9 minutes 14.33 seconds, total 33 frames, or ~16.8 spf
3 fasts start sur Matlab avec ~11,7 images par séquence avec « CurveMapper4 »    -> 9 minutes 17.05 seconds, total 35 frames, or ~15.9 spf

Autre opérateur :
3 fasts start sur Matlab avec 12 images par séquence avec « CurveMapper4 » -> 15 minutes 53.11 seconds, or ~26.5 spf <- Clara
3 fasts start sur Matlab avec 12 images par séquence avec « CurveMapper4 » -> 16 minutes 16.50 seconds, or ~27.1 spf <- Lili
```


## Auteur

Felix Guillaud

Mail: [felix.guillaud.work\@gmail.com](mailto:felix.guillaud.work@gmail.com){.email}

## License

Ce projet est distribué sous la licence MIT.
