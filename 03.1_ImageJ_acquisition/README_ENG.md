---
title: "README_ENG"
author: "FG"
date: "2026-06-23"
output: html_document
---

# ImageJ_acquisition

## Overview

Acquisition of body midline coordinates using ImageJ to assess inter-operator and intra-operator variability. The aim was to compare digitization methods in order to select the tool best suited to our data. The same protocol was applied for `03.2_MATLAB_acquisition` to compare inter-method variability. If possible, measurements need to be spaced out when assessing intra-operator variability (to avoid habituation bias).

------------------------------------------------------------------------

## Structure

``` text
rep 1; rep 2; rep 3 -> Repeat fast start acquisition: 206_fs1; 206_fs2; 206_fs3; 207_fs1; 207_fs2; 207_fs3; 208_fs1; 208_fs2; 208_fs3 using FG for intra-operator variability
rep 4 - 200 points  -> Acquisition of the same fast start sequence to acquire 200 points and compare with the MATLAB method (see report)
clara; lili; maxime -> Fast start acquisition: 206_fs1; 206_fs2; 206_fs3 to compare inter-operator variability
``` 

------------------------------------------------------------------------

## Protocol

``` text
- For the sequence extraction part, see 'Acquisition fast start'
- Insert the macro 'Segmented Action Tool - Multilines (cm)' into the file 'ij154-win-java8\ImageJ\macros\StartupMacros.txt' and save it
- In the same folder as this 'README', create a folder 'sequence' containing all the frames of the sequence of interest, in the desired order
- Launch ImageJ, and open the first frame in the 'sequence' folder
- The macro should have been correctly added to ImageJ and should automatically appear in the toolbar as 'Segmented Tool'. Adding '- C0a0L18f8L818f [1]' to the macro title (.txt file) will display a green cross at the macro's location in ImageJ.
- Use the 'Freehand line' tool (right-click on straight-line) and draw a curve from the subject's head to tail, staying as close to the center of the subject's body as possible.
- Once finished, click on the 'Segmented Tool' macro. A results window should open, containing the coordinates of the points that make up the curve.
If using 15 points, double-check that all 15 points are recorded; otherwise, repeat the steps.
If using approximately 200 points, consider approximately 200 points sufficient (± 10 points).
- Move to the next image (Ctrl+Shift+O) and repeat the steps. Clicking on the image resets the curve.
- Once all frames are processed, save the coordinates ("Save As"), which exports an Excel file to the desired folder (organize the file names and layout).
- Close the image, clear the coordinates ("Results\Clear results"), and open the first image of the next sequence.
```

------------------------------------------------------------------------

## Modification of the `Segmented_final_cm.txt` file


Note: in the macro "Segmented Action Tool - Multilines (cm) - C0a0L18f8L818f [1]"

/ Fixed number of points
-> N = 201;

This allows you to modify the number of points recorded. The Ptut students used 16 points to record 15 segments. Here, we use 201 points to theoretically produce 200 segments for comparison with the MATLAB method. Using 201 points also avoids the problem of recording 15 coordinates, which only works one time out of three, and which often resulted in a bias in the curve of the fish (lengthening the segment to accurately represent 15 coordinates).


## DURATION TEST

``` text
3 fast starts in ImageJ using "Segmented Action Tool - Multilines (cm)" N = 201 -> 08 minutes 15.16 seconds, total 35 frames, or ~14.14 spf (seconds per frame)
3 fast starts in ImageJ using "Segmented Action Tool - Multilines (cm)" N = 201 -> 07 minutes 30.34 seconds, total 33 frames, or ~13.63 spf
3 fast starts in ImageJ using "Segmented Action Tool - Multilines (cm)" N = 201 -> 07 minutes 41.23 seconds, total 35 frames, or ~13.17 spf
3 fast starts in ImageJ using "Segmented Action Tool - Multilines (cm)" N = 16 -> 06 minutes 59.49 seconds, total 35 frames, or ~11.97 spf
3 fast starts on ImageJ using "Segmented Action Tool - Multilines (cm)" N = 16 -> 06 minutes 21.38 seconds, total 33 frames, or ~11.54 spf
3 fast starts on ImageJ using "Segmented Action Tool - Multilines (cm)" N = 16 -> 07 minutes 00.72 seconds, total 35 frames, or ~12 spf
1 fast start on ImageJ using "Segmented Action Tool - Multilines (cm)" N = 16 -> 06 minutes 37.03 seconds, total 11 frames, or ~36.09 spf
```


## Author

Felix Guillaud

Mail: [felix.guillaud.work\@gmail.com](mailto:felix.guillaud.work@gmail.com){.email}

## License

This project is distributed Under the MIT license.
