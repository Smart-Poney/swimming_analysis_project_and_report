#automatisation de la segmentation et de la visualisation pour chaque CSV

#bibliotheque
import numpy as np
from algorithms_segmentgrowing import segment_growing
import numpy as np
import matplotlib.pyplot as plt
from algorithms_segmentgrowing import segment_growing
import os
import csv
from tqdm import tqdm

# creation des chemins
DATA_FOLDER = "output/formated_data"
OUTPUT_FRAMES = "output/frames"
OUTPUT_SEGMENTS = "output/segments"
os.makedirs(OUTPUT_FRAMES, exist_ok=True)
os.makedirs(OUTPUT_SEGMENTS, exist_ok=True)

####### parametre a modifier pour la precision
# Demander le seuil à l'utilisateur
while True:
    try:
        THRESH = float(input("Veuillez entrer la valeur du seuil (thresh default = 5.0)  : "))
        break
    except ValueError:
        print("Erreur : veuillez entrer un nombre valide.")

#charger les donnees
csv_files = [f for f in os.listdir(DATA_FOLDER) if f.endswith(".csv")]

for csv_file in csv_files:
    csv_name = os.path.splitext(csv_file)[0]  # nom sans extension
    print(f"Traitement du fichier : {csv_file}")

    # Charger les données
    data_path = os.path.join(DATA_FOLDER, csv_file)
    data = np.loadtxt(data_path, delimiter=",", skiprows=1)

    # Dimensions et colonnes
    num_rows, num_cols = data.shape
    num_frames = num_cols // 2
    xs = np.arange(0, num_cols, 2)
    ys = np.arange(1, num_cols, 2)

    # Segment Growing
    joints, evals, datapoints = segment_growing(
        data, xs, ys, num_rows, num_cols, THRESH
    )
    print(f"Joints : {joints}")
    print(f"Évaluations : {evals}, points traités : {datapoints}")

    # Créer dossier pour les frames
    frames_folder = os.path.join(OUTPUT_FRAMES, csv_name)
    os.makedirs(frames_folder, exist_ok=True)

    # Cycle de couleurs pour les segments
    colors = plt.cm.tab10.colors
    num_colors = len(colors)

    all_segments = []

    # Boucle sur les frames
    for frame in range(num_frames):
        x = data[:, xs[frame]]
        y = data[:, ys[frame]]

        plt.figure()
        plt.plot(x, y, 'k.-', alpha=0.5)  # courbe originale

        start = 0
        frame_segments = []

        for idx, j in enumerate(joints):
            color = colors[idx % num_colors]
            seg_x = [x[start], x[j]]
            seg_y = [y[start], y[j]]

            plt.plot(seg_x, seg_y, color=color, linewidth=3)

            # Stocker le segment pour export CSV
            frame_segments.append((seg_x, seg_y))
            start = j

        all_segments.append(frame_segments)

        # Export PNG
        frame_filename = os.path.join(frames_folder, f"{csv_name}_frame_{frame}.png")
        plt.axis("equal")
        plt.title(f"{csv_name} - Frame {frame}")
        plt.savefig(frame_filename)
        plt.close()

    #export donnees segments
    segments_csv_path = os.path.join(OUTPUT_SEGMENTS, f"{csv_name}_seg.csv")
    os.makedirs(OUTPUT_SEGMENTS, exist_ok=True)

    with open(segments_csv_path, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(["frame", "segment", "x0", "y0", "x1", "y1"])

        for frame_idx, frame_segments in enumerate(all_segments):
            for seg_idx, (seg_x, seg_y) in enumerate(frame_segments):
                writer.writerow([frame_idx, seg_idx, seg_x[0], seg_y[0], seg_x[1], seg_y[1]])

    print(f"Export CSV segments terminé : {segments_csv_path}")
    print(f"Export CSV segments terminé : {segments_csv_path}")