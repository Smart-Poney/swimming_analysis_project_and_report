---
title: "README_FR"
author: "FG"
date: "2026-06-23"
output: html_document
---

# ImageJ_acquisition

## Présentation

Acquisition des coordonnées de body midlines en utilisant ImageJ pour tester la variabilité inter-opérateur et la variabilité intra-opérateur. Le but étant de comparer les méthodes de digitalisation pour sélectionner l'outil le plus adapté à nos données. Le même protocole a été appliqué pour `03.2_MATLAB_acquisition` pour comparer la variabilité inter-méthode. Si possible, espacer les mesures en mesurant la variabilité intra opérateur (éviter biais habituation)

------------------------------------------------------------------------

## Structure

``` text
rep 1 ; rep 2 ; rep 3 -> répétition acquisition fast start 206_fs1 ; 206_fs2 ; 206_fs3 ; 207_fs1 ; 207_fs2 ; 207_fs3 ; 208_fs1 ; 208_fs2 ; 208_fs3 par FG pour la variabilité intra opérateur
rep 4 - 200 pts -> acquisition des même fast start pour acquérir 200 pts et comparer avec méthode MATLAB (cf report)
clara ; lili ; maxime -> acquisition fast start 206_fs1 ; 206_fs2 ; 206_fs3 pour comparer la variabilité inter opérateur
```

------------------------------------------------------------------------

## Protocole

``` text
- Pour la partie extraction des sequences, cf 'Acquisition fast start'
- Insérer la macro 'Segmented Action Tool - Multilines (cm)' dans le fichier 'ij154-win-java8\ImageJ\macros\StartupMacros.txt' et enregistrer
- Dans le même dossier que ce 'README', créer un dossier 'sequence' contentant toutes les frames de la séquence d'intérêt, dans l'ordre voulu
- Lancer ImageJ, et ouvrir la première frame du dossier 'sequence'
- La macro devrait s'être correctement ajouté à ImageJ et s'ajouter automatiquement à la barre d'outil comme 'Segmented Tool'. 
  Rajouter '- C0a0L18f8L818f [1]' dans le titre de la macro (fichier .txt) permet d'afficher une croix verte à l'emplacement de la macro dans ImageJ
- Utiliser l'outil 'Freehand line' (clique-droit sur straight-line) et tracer une courbe de la tête à la queue du sujet en restant le plus au milieu possible du corps du sujet
- Une fois fini, cliquer sur la macro 'Segmented Tool', une fenêtre de résultat devrait s'ouvrir et contenir les coordonnées des points composant la courbe
  Si on utilise 15 points -> bien vérifier l'enregistrement des 15 points, sinon recommencer les étapes
  Si on utilise ~200 points -> considérer un nombre d'environ 200 points suffisants (+- 10 pts)
- Passer à l'image suivante (Ctrl+Maj+o) et recommencer les étapes. Clique sur l'image réinitialise la courbe
- Une fois toutes les frames traitées, enregistrer les coordonnées ('Save As') qui exporte un fichier excel dans le dossier souhaité (bien s'organiser dans les noms/rangement)
- Fermer l'image, effacer les coordonnées ('Results\Clear results') et ouvrir la première image de la séquence suivante
``` text


------------------------------------------------------------------------

## Modification du fichier `Segmented_final_cm.txt`


Note : dans macro "Segmented Action Tool - Multilines (cm) - C0a0L18f8L818f [1]"

      // Nombre de points fixe
  ->  N = 201;

Cela permet de modifier le nombre de points enregistrés. Les étudiants Ptut on utlisé 16 points pour enregistrer 15 segments. Ici on utilise 201 points pour produire théoriquement 200 segments pour comparaison avec la méthode MATLAB. Utiliser 201 points permet aussi d'éviter le problème d'enregistrement des 15 coordonnées qui marche une fois sur trois, qui aboutissait souvent sur un biais dans le tracé de la courbure du poisson (rallonger pour bien considérer 15 coordonnées).

## Test de durée

``` text
3 fasts start sur ImageJ en utilisant "Segmented Action Tool - Multilines (cm)" N = 201 -> 08 minutes 15.16 secondes, total 35 frames soit ~14.14 spf (seconds per frame)
3 fasts start sur ImageJ en utilisant "Segmented Action Tool - Multilines (cm)" N = 201 -> 07 minutes 30.34 secondes, total 33 frames soit ~13.63 spf
3 fasts start sur ImageJ en utilisant "Segmented Action Tool - Multilines (cm)" N = 201 -> 07 minutes 41.23 secondes, total 35 frames soit ~13.17 spf
3 fasts start sur ImageJ en utilisant "Segmented Action Tool - Multilines (cm)" N = 16 ->  06 minutes 59.49 secondes, total 35 frames soit ~11.97 spf
3 fasts start sur ImageJ en utilisant "Segmented Action Tool - Multilines (cm)" N = 16 ->  06 minutes 21.38 secondes, total 33 frames soit ~11.54 spf
3 fasts start sur ImageJ en utilisant "Segmented Action Tool - Multilines (cm)" N = 16 ->  07 minutes 00.72 secondes, total 35 frames soit ~12 spf
1 fasts start sur ImageJ en utilisant "Segmented Action Tool - Multilines (cm)" N = 16 ->  06 minutes 37.03 secondes, total 11 frames soit ~36.09 spf
```


## Auteur

Felix Guillaud

Mail: [felix.guillaud.work\@gmail.com](mailto:felix.guillaud.work@gmail.com){.email}

## License

Ce projet est distribué sous la licence MIT.



