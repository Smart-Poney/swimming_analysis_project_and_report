---
title: "README_ENG"
author: "FG"
date: "2026-06-23"
output: html_document
---

# MATLAB_acquisition

## Overview

Acquisition of body midline coordinates using MATLAB to assess inter-operator and intra-operator variability. The aim was to compare digitization methods in order to select the tool best suited to our data. The same protocol was applied for `03.1_ImageJ_acquisition` to compare inter-method variability. If possible, measurements need to be spaced out when assessing intra-operator variability (to avoid habituation bias).

------------------------------------------------------------------------

## Structure

``` text
rep 1; rep 2; rep 3 -> Repeat fast start acquisition: 206_fs1; 206_fs2; 206_fs3; 207_fs1; 207_fs2; 207_fs3; 208_fs1; 208_fs2; 208_fs3 using FG for intra-operator variability
rep 4 - 15 pts      -> Acquisition of the same fast start sequence with 'CurvemapperMod' to acquire 15 pts and compare with the ImageJ method (see report)
clara; lili; maxime -> acquisition fast start 206_fs1 ; 206_fs2 ; 206_fs3 to compare inter-operator variability
```

------------------------------------------------------------------------

## Protocol
(for more details, refers to '02_fast_start_acquisition/README_ENG.txt'):

``` text
- Launch MATLAB, wait for '>>' to appear in the console and type 'CurveMapper4' for 200 points or 'CurveMapperMod' for 15 points
- Select 'Multiple Frame'
- Select the video of interest
- Uncheck 'Unit Conversion'
- The output folder is already \data_matlab, verify
- The output file is already correctly named ID_fs1 or ID_fs2 etc..., verify
- Start Frame: 1; Increment: 1; End Frame: Locate the subfolder containing the number of frames in the video of interest within 'labeled-data'.
- 'GO'
- Use 'left click' to add a point, 'right click' to remove the last point, and 'enter' to advance to the next frame.
- Use '-' and '=' to zoom in/out (depending on the keyboard, '-'/'+').
- For all other details of the acquisition method, refer to the document 'CurveMapper5.doc' by G. Lauder.
- Close the image and open the next sequence.
```

## Duration test

``` text
FG:
3 fast starts on Matlab with 12 frames each using 'CurveMapperMod'   -> 11 minutes 35.01 seconds, total 36 frames, or ~19.3 spf (seconds per frame)
3 fast starts on Matlab with 10 frames each using 'CurveMapperMod'   -> 10 minutes 05.18 seconds, total 30 frames, or ~20.2 spf
3 fast starts on Matlab with ~11.7 frames each using 'CurveMapper4'  -> 10 minutes 09.99 seconds, total 35 frames, or ~17.4 spf
3 fast starts on Matlab with ~11.7 frames each using 'CurveMapper4'  -> 8 minutes 42.56 seconds, total 35 frames, or ~13.2 spf
3 fast starts on Matlab with 11 frames each using 'CurveMapper4'     -> 9 minutes 14.33 seconds, total 33 frames, or ~16.8 spf
3 fast starts on Matlab with ~11.7 frames each using 'CurveMapper4'  -> 9 minutes 17.05 seconds, total 35 frames, or ~15.9 spf

Other operator:
3 fast starts on Matlab with 12 frames each using 'CurveMapper4' -> 15 minutes 53.11 seconds, or ~26.5 spf <- Clara
3 fast starts on Matlab with 12 frames each using 'CurveMapper4' -> 16 minutes 16.50 seconds, or ~27.1 spf <- Lili
```


## Author

Felix Guillaud

Mail: [felix.guillaud.work\@gmail.com](mailto:felix.guillaud.work@gmail.com){.email}

## License

This project is distributed Under the MIT license.

