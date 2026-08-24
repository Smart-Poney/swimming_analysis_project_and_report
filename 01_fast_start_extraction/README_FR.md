---
title: "README_FR"
author: "FG"
date: "2026-06-23"
output: html_document
---

# fast_start_extraction

## Présentation

Extraire les séquences de fast-start en .avi et en .jpg frame par frame. Les séquences vidéos proviennent des tests de nages réalisés en conditions artificielles (cf rapport). Les fast start on été provoqués par l'expérimentateur après 5 minutes de comportement.

------------------------------------------------------------------------

## Protocole

``` text
1) Créer les dossiers :
'01_fast_start_extraction\date' #pour chaque jour d'enregistrement
'01_fast_start_extractiont\date\heure' # pour chaque poisson
'01_fast_start_extraction\labeled-data\194_fs2' # pour chaque fast start, ici  le poisson ID = 194, 2eme fast start
'01_fast_start_extraction\videos' # qui contiendra toutes les videos finales

2) Charger sur Avidemux une video contenant une sequence de fast start et l'identifier (poisson, date, heure) en se référant au carnet d'expérimentation (M. Descat).
'Avidemux 2.8.1 - Release: 2001-2022 Mean / eumagga0x2a. <http://www.avidemux.org>'

3) Sur Avidemux, isoler une séquence de fast start avec les boutons marqueurs A et B et la découper.
Le carnet d'expérimentation permet de retrouver les fast-start à 1-2s près.
Le marqueur B permet le découpage à la frame exacte, et pour le A il faut se placer un frame après celle souhaitée.

4) Exporter la séquence de frames 'fichier\enregistrer la selection en JPEG' dans le dossier '01_fast_start_extraction\labeled-data\194_fs2' # exemple pour le poisson ID = 194, 2eme fast start

5) Exporter la séquence vidéo du fast start en AVI (sortie Mpeg4 AVC ; format de sortie AVI Muxer) dans le dossier '01_fast_start_extraction\date\heure' avec le nom '194_fs2' # exemple pour le poisson ID = 194, 2eme fast start

6) Enlever le découpage de la vidéo avec Ctrl+z, passer au fast start suivant et recommencer étape 3) 4) 5) jusqu'au dernier fast start de la vidéo # en général 2-3 fast-start max

7) Copier les vidéo de fast-start AVI dans le dossier '01_fast_start_extraction\videos' avec leur nom '194_fs2' # exemple pour le poisson ID = 194, 2eme fast start

8) Passer à une autre vidéo en recommençant toutes les étapes
``` text

------------------------------------------------------------------------

## Information supplémentaires

Précision : Le début des fast-start correspond à la frame précédant le premier mouvement du corps, pas de detection de latence au stimulus possible. La fin du fast-start est à determiner pour chaque mouvement, normalement 2 mouvements du corps au total (bend, counter-bend) ou bien  individu en position initiale (statique ou inertie). En général un fast-start dure 8-16 frames soit 7.5-15 ms

Comparaison de la qualité de traitement :
```text
vidéo originale MP4, 958*720 pixels en 120.11 fps
enregistrement en JPEG via Avidemux -> 958*720 pixels
propriétés de la vidéo exportée en AVI via Avidemux -> 958*720 pixels en 120.12 fps
pas de biais de dégradation de la qualité par transformation du fichier.
possible flou lors du traitement des fast-start, limite du design expérimental
La qualité de l'enregistrement a été grandement baissé a profit de la vitesse d'acquisition : 120 fps, nécessaire pour visualiser les fast-start
```text

------------------------------------------------------------------------


## Auteur

Felix Guillaud

Mail: [felix.guillaud.work\@gmail.com](mailto:felix.guillaud.work@gmail.com){.email}

## License

Ce projet est distribué sous la licence MIT.

