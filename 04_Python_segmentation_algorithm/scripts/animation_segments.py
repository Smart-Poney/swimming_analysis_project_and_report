import os
import csv
import numpy as np
import matplotlib.pyplot as plt
import imageio.v2 as imageio
from tqdm import tqdm


SEGMENTS_FOLDER = "output/segments"
ANIMATION_FOLDER = "output/animation/origin_free"
os.makedirs(ANIMATION_FOLDER, exist_ok=True)

seg_files = [f for f in os.listdir(SEGMENTS_FOLDER) if f.endswith("_seg.csv")]

outer_bar = tqdm(seg_files,
                 desc='All sequences of fast-start',
                 unit='seq',
                 position=0,
                 leave=True)

for seg_file in outer_bar:
    if not seg_file.endswith("_seg.csv"):
        continue

    csv_name = seg_file.replace("_seg.csv", "")
    #outer_bar.write(f"Animation pour : {csv_name}")

    seg_path = os.path.join(SEGMENTS_FOLDER, seg_file)

    anim_folder = ANIMATION_FOLDER
    os.makedirs(anim_folder, exist_ok=True)

    # Lire CSV
    segments = []
    with open(seg_path, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            segments.append(row)

    frames = sorted(set(int(s["frame"]) for s in segments))

    # Origine fixe = premier segment de la frame 0
    first_seg_frame0 = next(
        s for s in segments if int(s["frame"]) == 0 and int(s["segment"]) == 0
    )

    images = []

    # limites fixes
    XMIN, XMAX = -50, 150
    YMIN, YMAX = -100, 100

    images = []
    images_paths = []
    all_x = []
    all_y = []

    inner_bar = tqdm(frames,
                     desc=f"Treatment of {csv_name}",
                     unit='frame',
                     position=1,
                     leave=False)

    for frame in inner_bar:
        plt.figure(figsize=(6,6))
        plt.clf()
    
        frame_segments = [s for s in segments if int(s["frame"]) == frame]
    
        first_seg_frame = next(s for s in frame_segments if int(s["segment"]) == 0)
    
        dx = float(first_seg_frame["x0"])
        dy = float(first_seg_frame["y0"])

        for s in frame_segments:
            x0 = float(s["x0"]) - dx
            y0 = float(s["y0"]) - dy
            x1 = float(s["x1"]) - dx
            y1 = float(s["y1"]) - dy
    
            plt.plot([x0, x1], [y0, y1], linewidth=3)

        # repère fixe
        plt.axhline(0, color="gray", linewidth=0.8)
        plt.axvline(0, color="gray", linewidth=0.8)

        # limites fixes pour que (0,0) soit au milieu à gauche
        #XMIN, XMAX = 0, 150      # origine à gauche
        #YMIN, YMAX = -75, 75    # origine au milieu vertical

        plt.xlim(XMIN, XMAX)
        plt.ylim(YMIN, YMAX)

        plt.axis("equal")
        plt.grid(True)

        plt.title(f"{csv_name} - Frame {frame}")
        plt.xlabel("X")
        plt.ylabel("Y")

        img_path = os.path.join(anim_folder, f"frame_{frame}.png")
        plt.savefig(img_path)   # pas de bbox_inches="tight"
        plt.close()

        images.append(imageio.imread(img_path))
        images_paths.append(img_path) 

    # GIF avec durée 0.5s
    gif_path = os.path.join(anim_folder, f"{csv_name}.gif")
    imageio.mimsave(gif_path, images, duration=1.5)
    for img in images_paths:
        os.remove(img)

    #print(f"GIF créé : {gif_path}")

print("end.")
