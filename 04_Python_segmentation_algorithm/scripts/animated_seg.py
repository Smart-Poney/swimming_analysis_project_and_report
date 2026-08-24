import os
import csv
import matplotlib.pyplot as plt
import imageio.v2 as imageio

SEGMENTS_FOLDER = "output/segments"
ANIMATION_FOLDER = "output/animation/origin_fixed"
os.makedirs(ANIMATION_FOLDER, exist_ok=True)

for seg_file in os.listdir(SEGMENTS_FOLDER):
    if not seg_file.endswith("_seg.csv"):
        continue

    csv_name = seg_file.replace("_seg.csv", "")
    print(f"Animation pour : {csv_name}")

    seg_path = os.path.join(SEGMENTS_FOLDER, seg_file)

    # Lire CSV
    segments = []
    with open(seg_path, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            segments.append(row)

    frames = sorted(set(int(s["frame"]) for s in segments))

    # ---------
    # 1) Translation de la tête et pré-calcul des segments normalisés
    # ---------
    normalized_frames = {}
    all_x = []
    all_y = []

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
            all_x.extend([x0, x1])
            all_y.extend([y0, y1])
        normalized_frames[frame] = norm_segments

    # ---------
    # 2) Calcul des limites globales fixes pour tout le GIF
    # ---------
    margin = 10  # ajouter une marge pour que le poisson ne touche pas les bords
    XMIN, XMAX = min(all_x) - margin, max(all_x) + margin
    YMIN, YMAX = min(all_y) - margin, max(all_y) + margin

    # ---------
    # 3) Génération des images avec repère fixe et aspect égal
    # ---------
    images = []
    images_paths = []

    for frame in frames:
        fig, ax = plt.subplots(figsize=(6,6))

        # Dessin des segments normalisés
        for (x0, y0, x1, y1) in normalized_frames[frame]:
            ax.plot([x0, x1], [y0, y1], linewidth=3)

        # repère fixe (origine)
        ax.axhline(0, color="gray", linewidth=0.8)
        ax.axvline(0, color="gray", linewidth=0.8)
        ax.scatter(0, 0, color="black", s=40)

        # limites globales fixes
        ax.set_xlim(XMIN, XMAX)
        ax.set_ylim(YMIN, YMAX)

        # aspect égal garanti
        ax.set_aspect('equal', adjustable='box')

        # grille et labels
        ax.grid(True)
        ax.set_title(f"{csv_name} - Frame {frame}")
        ax.set_xlabel("X")
        ax.set_ylabel("Y")

        # sauvegarde de l'image
        img_path = os.path.join(ANIMATION_FOLDER, f"{csv_name}frame{frame}.png")
        plt.savefig(img_path)
        plt.close()

        images.append(imageio.imread(img_path))
        images_paths.append(img_path)

    # ---------
    # 4) Création du GIF
    # ---------
    gif_path = os.path.join(ANIMATION_FOLDER, f"{csv_name}.gif")
    imageio.mimsave(gif_path, images, duration=1.5)

    # Suppression PNG
    for img in images_paths:
        os.remove(img)

    print(f"GIF créé : {gif_path}")

print("Toutes les animations sont terminées.")
