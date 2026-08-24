---
title: "README_ENG"
author: "FG"
date: "2026-06-23"
output: html_document
---

# Python_segmentation_algorithm

## Overview 

This repository contains the workflow to use the segment Growing Method (SGM) `algorithms_segmentgrowing` provided by Robert Sterling <ros46@aber.ac.uk>. The algorithm is used and call via other custom Python scripts to produce segmentation of the fish body midline  trhough the actuation of a fast-start event. The SGM is normally used during steady swimming sequences and 5 segments is enough to capture 99% of (sub-)carangiforms ondulatory swimming movements (Fetherstonhaugh *et al.*, 2021). Here we use the SGM to investigate the number of segments and their size to recreate fast-start actuation of juvenile subcarangiforms (*Salmo trutta*). The algorithm recreates 12 segments (mean) with decreasing lentgh from head to tail as used in further analysis. 

------------------------------------------------------------------------

## Project structure

``` text
Python_segmentation_algorithm/
│
├── data_matlab/      # raw MATLAB data
├── output/           # animation, graphes, tables
├── scripts/          # Python scripts, also containing 'algorithms_segmentgrowing'
│
├── README_FR.md      # french version
└── README_ENG.md
```

------------------------------------------------------------------------

## Processus

We use the raw data acquired with the MATLAB digitalization method (see `/fast_start_acquisition/README_ENG.txt`) that provides us with spatial coordinates of the body midlines for 200 points.


In the repositery `Python_segmentation_algorithm`, open the command prompt:

Run:

``` python
python scripts\formatage.py             # to formate the file into using the SGM
python scripts\auto_seg_norm.py         # to call the SGM and define automatically the threshold value
python scripts\animated_seg.py          # to animate each fast_start actuation with a fixed origin
python scripts\animation_segments.py    # to animate each fast_start actuation trhough space
python scripts\animated_glob.py         # to animate every fast_start actuation
python scripts\animated_glob_paused.py  # to animate every fast_start actuation with track of each one
```

In addition, to test the threshold value indentation, Run :

``` python
python scripts\test_valeur_thresh.py  # produce graph for each value from 0.1 to 20 (step = 0.1)
python scripts\animation.py           # produce 'animation_thresh.GIF'
```

If you want to redo each steps while using a precise threshold value, Run : 

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
├── animation/            # GIF animations for each fast-start acquired and global animations
├── formated_data/        # the formated data
├── frames/               # the frames of each fast-start acquired with the segments recreated
├── segments/             # the coordinates of each segments/joints positions + the joints_summary.csv
└── value_threshold/      # the graphes used to visualize the effect of threshold variation
```

------------------------------------------------------------------------

## Python dependencies

Some preprocessing and segmentation scripts are written in Python.

Required external packages:

- numpy
- pandas
- matplotlib
- imageio
- tqdm

Install with:

```bash
pip install numpy pandas matplotlib imageio tqdm
```
Or:

Install dependencies with:

```bash
pip install -r requirements.txt
```


NOTE : if it doesn't work, try to import function/package by using command prompt in starting menu, and Run:
```python 
python -m pip install 'package'
```

------------------------------------------------------------------------

## Author

Felix Guillaud

Mail: [felix.guillaud.work\@gmail.com](mailto:felix.guillaud.work@gmail.com){.email}

## License

This project is distributed under the MIT License.





