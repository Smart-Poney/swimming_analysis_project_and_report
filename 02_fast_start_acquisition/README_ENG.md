---
title: "README_ENG"
author: "FG"
date: "2026-06-23"
output: html_document
---

# fast_start_acquisition

## Overview

Use the fast start sequences extracted with '01_fast_start_extraction' to digitize the body midlines and obtain spatial coordinate data using the MATLAB method (see report). About the sequence extraction section, refer to the file `01_fast_start_extraction\README.md`


------------------------------------------------------------------------

## Protocol

``` text
1) Copy all subfolders from the '01_fast_start_extraction' folder except for the 'README.txt' and '\videos' files:
'\date' # for each recording day
'\date\time' # for each fish
'\labeled-data\194_fs2' # for each fast start, here fish ID = 194, 2nd fast start

2) Create the folder:
'02_fast_start_acquisition\data_matlab' # containing all the acquired data

3) Import the following files from <https://zenodo.org/records/4623882> based on the article by Di Santo et al., 2021
'CurveMapper6.m' # contains the code for 'CurveMapper4.m' and is called via Matlab with '>>CurveMapper4'
'CurveMapper5.doc' # for instructions

3.2) (optional) Add:
     (optional) 'CurveMapper4Mod.m' # to get 15 points with MATLAB
     (optional) 'remove_png.py' # provided with the README.txt to remove unnecessary files output by Matlab

4) Modify the 'CurveMapper4.m' file, see below. Further down, 'MODIFICATION OF THE 'CurveMapper4.m' FILE'

5) MATLAB usage protocol:
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

------------------------------------------------------------------------

## Supplementary information

Test of operator variability for MATLAB

``` text
rep 1; rep 2; rep 3 -> Fast start acquisition repetition: 206_fs1; 206_fs2; 206_fs3; 207_fs1; 207_fs2; 207_fs3; 208_fs1; 208_fs2; 208_fs3 using FG to compare intra-operator variability
rep 4 - 15 points   -> Acquisition of the same fast start with 'CurvemapperMod' to acquire 15 points and compare with the Matthias ImageJ method
clara; lili; maxime -> Fast start acquisition: 206_fs1; 206_fs2; 206_fs3 to compare inter-operator variability
```

Duration test - stopwatch:

``` text
Felix:
3 fast starts on Matlab with 12 frames each using 'CurveMapperMod' -> 11 minutes 35.01 seconds, total 36 frames, or ~19.3 spf (seconds per frame)
3 fast starts on Matlab with 10 frames each using 'CurveMapperMod' -> 10 minutes 05.18 seconds, total 30 frames, or ~20.2 spf
3 fast starts on Matlab with ~11.7 frames each using 'CurveMapper4' -> 10 minutes 09.99 seconds, total 35 frames, or ~17.4 spf
3 fast starts on Matlab with ~11.7 frames each using 'CurveMapper4' -> 8 minutes 42.56 seconds, total 35 frames, or ~13.2 spf
3 fast starts on Matlab with 11 frames each using 'CurveMapper4' -> 9 minutes 14.33 seconds, total 33 frames, or ~16.8 spf
3 fast starts on Matlab with ~11.7 frames each using 'CurveMapper4' -> 9 minutes 17.05 seconds, total 35 frames, or ~15.9 spf

other operator:
3 fast starts on Matlab with 12 frames each using 'CurveMapper4' -> 15 minutes 53.11 seconds, or ~26.5 spf <- Clara
3 fast starts on Matlab with 12 frames each using 'CurveMapper4' -> 16 minutes 16.50 seconds, or ~27.1 spf <- Lili
```

------------------------------------------------------------------------

## File modification `CurveMapper4.m`

``` matlab
line 387, in the “else” part, replace:
 
        else
            s.FileName= FileName;
            s.PathName= PathName;
            s.OutputFileName= [s.FileName '_CURVES.xls'];
            s.OutputPathName= s.PathName;
            s.MovObj= VideoReader([PathName FileName]);

by:

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

and replace [your_path_here] by the path to your root folder to save directly ouput files in ‘\data_matlab’

line 488, to avoid saving unnecessary (in our project) supplementary files that requires power and long-time processing, replace:
 
                saveas(gcf,[s.OutputPathName s.OutputFileName '.jpg'],'jpg')
                saveas(gcf,[s.OutputPathName s.OutputFileName '.emf'],'emf')

by:

                %MODIFICATION : added percentage to avoid loading files not needed (jpg and emf)
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.jpg'],'jpg')
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.emf'],'emf')

line 533, to avoid saving unnecessary (in our project) supplementary files that requires power and long-time processing, replace:
 
                saveas(gcf,[s.OutputPathName s.OutputFileName '.jpg'],'jpg')
                saveas(gcf,[s.OutputPathName s.OutputFileName '.emf'],'emf')

by:

                %MODIFICATION : added percentage to avoid loading files not needed (jpg and emf)
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.jpg'],'jpg')
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.emf'],'emf')

line 570, to avoid saving unnecessary (in our project) supplementary files that requires power and long-time processing, replace:

                saveas(gcf,[s.OutputPathName s.OutputFileName '.jpg'],'jpg')
                saveas(gcf,[s.OutputPathName s.OutputFileName '.emf'],'emf')

by:

                %MODIFICATION : added percentage to avoid loading files not needed (jpg and emf)
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.jpg'],'jpg')
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.emf'],'emf')
```

All modifications have been indexed with '%MODIFICATION' in the script to allow easier navigation through the custom script.

End of modification `CurveMapper4.m`

------------------------------------------------------------------------


## Author

Felix Guillaud

Mail: [felix.guillaud.work\@gmail.com](mailto:felix.guillaud.work@gmail.com){.email}

## License

This project is distributed Under the MIT license.

