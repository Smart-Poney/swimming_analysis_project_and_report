---
title: "README_ENG"
author: "FG"
date: "2026-06-23"
output: html_document
---

# fast_start_extraction

## Overview

Extract the fast-start sequences in .avi and .jpg formats, frame by frame. The video sequences come from swimming tests conducted under artificial conditions (see report). The fast starts were triggered by the experimenter after 5 minutes of behavior.


------------------------------------------------------------------------

## Protocol

``` text
1) Create the following folders:
'01_fast_start_extraction\date' # for each day of recording
'01_fast_start_extraction\date\time' # for each fish
'01_fast_start_extraction\labeled-data\194_fs2' # for each fast start, here fish ID = 194, 2nd fast start
'01_fast_start_extraction\videos' # containing all the final videos

2) Load a video containing a fast-start sequence into Avidemux and label it (fish, date, time) by referring to the experiment notebook (M. Descat). 
'Avidemux 2.8.1 - Release: 2001-2022 Mean / eumagga0x2a. <http://www.avidemux.org>'

3) In Avidemux, isolate a fast start sequence using marker buttons A and B and cut it.
The experiment notebook allows you to find the fast starts to within 1-2 seconds.
Marker B allows you to cut to the exact frame, and for A you must position yourself one frame after the desired frame.

4) Export the frame sequence 'File\Save Selection as JPEG' to the folder '01_fast_start_extraction\labeled-data\194_fs2' # example for fish ID = 194, 2nd fast start

5) Export the fast start video sequence as AVI (MPEG4 AVC output; AVI Muxer output format) to the folder '01_fast_start_extraction\date\time' with the name '194_fs2' # example for fish ID = 194, 2nd fast start

6) Remove the video clipping with Ctrl+z, move to the next fast start, and repeat steps 3) 4) 5) until the last fast start in the video # generally 2-3 fast starts max

7) Copy the AVI fast start videos to the folder '01_fast_start_extraction\videos' with their names '194_fs2' # Example for fish ID = 194, 2nd fast start

8) Switch to another video by repeating all the steps
``` text

------------------------------------------------------------------------

## Supplementary information

Note: the start of the fast start corresponds to the frame preceding the first body movement; no latency detection is possible. The end of the fast start must be determined for each movement, normally 2 body movements in total (bend, counter-bend) or the individual in the initial position (static or inertial). Generally, a fast start lasts 8-16 frames, or 7.5-15 ms.

Comparison of processing quality:
```text
original video MP4, 958*720 pixels at 120.11 fps
recorded as JPEG via Avidemux -> 958*720 pixels
properties of the video exported as AVI via Avidemux -> 958*720 pixels at 120.12 fps
no bias for quality degradation due to file transformation.
possible blurring during fast-start processing, a limitation of the experimental design.
recording quality was significantly reduced in favor of the acquisition speed: 120 fps, necessary for viewing fast-starts.
```text

------------------------------------------------------------------------


## Author

Felix Guillaud

Mail: [felix.guillaud.work\@gmail.com](mailto:felix.guillaud.work@gmail.com){.email}

## License

This project is distributed Under the MIT license.

