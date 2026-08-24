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

# stockage global des joints
all_joints_results = []

####### parametre a modifier pour la precision
# Demander le seuil à l'utilisateur
#while True:
#    try:
#        THRESH = float(input("Veuillez entrer la valeur du seuil (thresh default = 5.0)  : "))
#        break
#    except ValueError:
#        print("Erreur : veuillez entrer un nombre valide.")


###### calcul de la longueur d'une courbe
def curve_length(x, y):
    dx = np.diff(x)
    dy = np.diff(y)
    return np.sum(np.sqrt(dx**2 + dy**2))


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

    # calcul auto de la longueur du poisson
    lengths = []
    for frame in range(num_frames):

        x = data[:, xs[frame]]
        y = data[:, ys[frame]]
        L = curve_length(x, y)
        lengths.append(L)

    mean_length = np.mean(lengths)

    # seuil basé sur la racine de la longueur
    THRESH = np.sqrt(mean_length)

    print(f"Longueur moyenne du poisson : {mean_length:.2f}")
    print(f"Seuil THRESH calculé automatiquement : {THRESH:.2f}")

    # Segment Growing
    joints, evals, datapoints = segment_growing(
        data, xs, ys, num_rows, num_cols, THRESH
    )

    print(f"Joints : {joints}")
    print(f"Évaluations : {evals}, points traités : {datapoints}")

    # stockage des joints 
    row = [csv_name] + list(joints)
    all_joints_results.append(row)

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


# ===== export global des joints =====

# trouver max joints
max_joints = max(len(r) - 1 for r in all_joints_results)
# header
header = ["ID"] + [f"joint_{i}" for i in range(max_joints)]

# compléter lignes
for r in all_joints_results:
    while len(r) < max_joints + 1:
        r.append("")

# export CSV
output_joints_csv = os.path.join(OUTPUT_SEGMENTS, "joints_summary.csv")

with open(output_joints_csv, 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(header)
    writer.writerows(all_joints_results)

print(f"Export des joints terminé : {output_joints_csv}")

