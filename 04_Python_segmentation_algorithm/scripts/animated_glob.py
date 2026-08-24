import os
import csv
import matplotlib.pyplot as plt
import imageio.v2 as imageio

SEGMENTS_FOLDER = "output/segments"
ANIMATION_FOLDER = "output/animation"
os.makedirs(ANIMATION_FOLDER, exist_ok=True)

# ---------
# 1) Charger et normaliser toutes les séquences
# ---------
all_sequences = {}
global_x = []
global_y = []

for seg_file in os.listdir(SEGMENTS_FOLDER):
    if not seg_file.endswith("_seg.csv"):
        continue

    csv_name = seg_file.replace("_seg.csv", "")
    print(f"Traitement de : {csv_name}")

    seg_path = os.path.join(SEGMENTS_FOLDER, seg_file)

    # Lire CSV
    segments = []
    with open(seg_path, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            segments.append(row)

    frames = sorted(set(int(s["frame"]) for s in segments))

    normalized_frames = {}
    for frame in frames:
        frame_segments = [s for s in segments if int(s["frame"]) == frame]
        head_seg = next(s for s in frame_segments if int(s["segment"]) == 0)
        xh0 = float(head_seg["x0"])
        yh0 = float(head_seg["y0"])

        norm_segments = []
        for s in frame_segments:
            x0 = float(s["x0"]) - xh0
            y0 = float(s["y0"]) - yh0
            x1 = float(s["x1"]) - xh0
            y1 = float(s["y1"]) - yh0
            norm_segments.append((x0, y0, x1, y1))
            global_x.extend([x0, x1])
            global_y.extend([y0, y1])
        normalized_frames[frame] = norm_segments

    all_sequences[csv_name] = normalized_frames

# ---------
# 2) Déterminer les limites globales fixes pour tous les GIFs
# ---------
margin = 10
XMIN, XMAX = min(global_x) - margin, max(global_x) + margin
YMIN, YMAX = min(global_y) - margin, max(global_y) + margin

# ---------
# 3) Générer les images pour toutes les séquences
# ---------
images = []
images_paths = []

for csv_name, frames_dict in all_sequences.items():
    frames = sorted(frames_dict.keys())
    for frame in frames:
        fig, ax = plt.subplots(figsize=(6,6))

        # Dessin des segments normalisés
        for (x0, y0, x1, y1) in frames_dict[frame]:
            ax.plot([x0, x1], [y0, y1], linewidth=3)

        # repère fixe
        ax.axhline(0, color="gray", linewidth=0.8)
        ax.axvline(0, color="gray", linewidth=0.8)
        ax.scatter(0, 0, color="black", s=40)

        # limites globales fixes
        ax.set_xlim(XMIN, XMAX)
        ax.set_ylim(YMIN, YMAX)
        ax.set_aspect('equal', adjustable='box')
        ax.grid(True)

        ax.set_title(f"{csv_name} - Frame {frame}")
        ax.set_xlabel("X")
        ax.set_ylabel("Y")

        img_path = os.path.join(ANIMATION_FOLDER, f"{csv_name}_frame{frame}.png")
        plt.savefig(img_path)
        plt.close()

        images.append(imageio.imread(img_path))
        images_paths.append(img_path)

# ---------
# 4) Créer le GIF unique qui défile toutes les séquences
# ---------
gif_path = os.path.join(ANIMATION_FOLDER, "all_sequences.gif")
imageio.mimsave(gif_path, images, duration=1.0)  # ajuster duration si besoin
print(f"GIF global créé : {gif_path}")

# Supprimer les images PNG temporaires
for img in images_paths:
    os.remove(img)

print("Toutes les animations sont terminées.")
