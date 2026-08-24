import os
import csv
import matplotlib.pyplot as plt
import imageio.v2 as imageio
import math
from tqdm import tqdm

SEGMENTS_FOLDER = "output/segments"
ANIMATION_FOLDER = "output/animation"
os.makedirs(ANIMATION_FOLDER, exist_ok=True)

# ---------
# 1) Charger et normaliser toutes les séquences
# ---------
all_sequences = {}
global_x = []
global_y = []

for seg_file in tqdm(os.listdir(SEGMENTS_FOLDER), desc="Chargement des séquences"):
    if not seg_file.endswith("_seg.csv"):
        continue

    csv_name = seg_file.replace("_seg.csv", "")
    #print(f"Traitement de : {csv_name}")

    seg_path = os.path.join(SEGMENTS_FOLDER, seg_file)

    segments = []
    with open(seg_path, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            segments.append(row)

    frames = sorted(set(int(s["frame"]) for s in segments))

    normalized_frames = {}
    for frame in tqdm(frames, desc=f"Frames {csv_name}", leave=False):
        frame_segments = [s for s in segments if int(s["frame"]) == frame]
        ref_segment_index = 0  # <-- AJOUT

        ref_seg = next(s for s in frame_segments if int(s["segment"]) == ref_segment_index)

        # Translation
        xh0 = float(ref_seg["x0"])
        yh0 = float(ref_seg["y0"])

        # Vecteur du segment de référence (après translation)
        vx = float(ref_seg["x1"]) - xh0
        vy = float(ref_seg["y1"]) - yh0

        # Calcul angle
        angle = math.atan2(vy, vx)

        # Rotation pour aligner sur axe X
        theta = -angle
        cos_t = math.cos(theta)
        sin_t = math.sin(theta)

        norm_segments = []
        for s in frame_segments:
            # Translation
            x0 = float(s["x0"]) - xh0
            y0 = float(s["y0"]) - yh0
            x1 = float(s["x1"]) - xh0
            y1 = float(s["y1"]) - yh0

            # Rotation
            x0r = x0 * cos_t - y0 * sin_t
            y0r = x0 * sin_t + y0 * cos_t
            x1r = x1 * cos_t - y1 * sin_t
            y1r = x1 * sin_t + y1 * cos_t
            # CORRECTION ORIENTATION
            if x1r < 0:
                x0r, y0r = -x0r, -y0r
                x1r, y1r = -x1r, -y1r

            norm_segments.append((x0r, y0r, x1r, y1r))

            global_x.extend([x0r, x1r])
            global_y.extend([y0r, y1r])

        normalized_frames[frame] = norm_segments

    all_sequences[csv_name] = normalized_frames

# ---------
# 2) Déterminer limites globales fixes
# ---------
margin = 10
XMIN, XMAX = min(global_x) - margin, max(global_x) + margin
YMIN, YMAX = min(global_y) - margin, max(global_y) + margin

# ---------
# 3) Générer les images pour le GIF global
# ---------
images = []
images_paths = []

pause_frames = 5  # nombre de frames pour la pause entre séquences
frame_duration = 1.5  # secondes par frame

frame_counter = 0  # pour donner un nom unique à chaque PNG

previous_segments = []

for csv_name, frames_dict in tqdm(all_sequences.items(), desc="Génération GIF"):
    frames = sorted(frames_dict.keys())
    for frame in frames:
        fig, ax = plt.subplots(figsize=(6,6))

        # Dessiner les segments des séquences précédentes en gris
        for seq_segments in previous_segments:
            for (x0, y0, x1, y1) in seq_segments:
                ax.plot([x0, x1], [y0, y1], color="gray", linewidth=2)

        # Dessiner la séquence en cours (couleurs originales)
        for (x0, y0, x1, y1) in frames_dict[frame]:
            ax.plot([x0, x1], [y0, y1], linewidth=3)

        # Repère fixe
        ax.axhline(0, color="gray", linewidth=0.8)
        ax.axvline(0, color="gray", linewidth=0.8)
        ax.scatter(0, 0, color="black", s=40)

        # Limites globales fixes et aspect égal
        ax.set_xlim(XMIN, XMAX)
        ax.set_ylim(YMIN, YMAX)
        ax.set_aspect('equal', adjustable='box')
        ax.grid(True)

        ax.set_title(f"{csv_name} - Frame {frame}")
        ax.set_xlabel("X")
        ax.set_ylabel("Y")

        img_path = os.path.join(ANIMATION_FOLDER, f"frame_{frame_counter:04d}.png")
        plt.savefig(img_path)
        plt.close()

        images.append(imageio.imread(img_path))
        images_paths.append(img_path)
        frame_counter += 1

    # Ajouter pause en répétant la dernière frame
    last_frame_segments = frames_dict[frames[-1]]
    previous_segments.append(last_frame_segments)
    for _ in range(pause_frames):
        fig, ax = plt.subplots(figsize=(6,6))

        # Toutes les séquences terminées en gris
        for seq_segments in previous_segments:
            for (x0, y0, x1, y1) in seq_segments:
                ax.plot([x0, x1], [y0, y1], color="gray", linewidth=2)

        # Repère fixe
        ax.axhline(0, color="gray", linewidth=0.8)
        ax.axvline(0, color="gray", linewidth=0.8)
        ax.scatter(0, 0, color="black", s=40)

        ax.set_xlim(XMIN, XMAX)
        ax.set_ylim(YMIN, YMAX)
        ax.set_aspect('equal', adjustable='box')
        ax.grid(True)

        ax.set_title(f"{csv_name} - Pause")
        ax.set_xlabel("X")
        ax.set_ylabel("Y")

        img_path = os.path.join(ANIMATION_FOLDER, f"frame_{frame_counter:04d}.png")
        plt.savefig(img_path)
        plt.close()

        images.append(imageio.imread(img_path))
        images_paths.append(img_path)
        frame_counter += 1

# ---------
# 4) Créer le GIF final
# ---------
gif_path = os.path.join(ANIMATION_FOLDER, "all_sequences_with_pause_colors.gif")
imageio.mimsave(gif_path, images, duration=frame_duration)
print(f"GIF global créé : {gif_path}")

# Supprimer les PNG temporaires
for img in images_paths:
    os.remove(img)

print("END - GIF exported")
