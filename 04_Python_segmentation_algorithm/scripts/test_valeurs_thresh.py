import numpy as np
from algorithms_segmentgrowing import segment_growing
import matplotlib.pyplot as plt
import csv
import os
from tqdm import tqdm  # barre de progression

# Charger les données (un seul CSV)
data = np.loadtxt("output/formated_data/206_fs1.csv", delimiter=",", skiprows=1)

# Définition des dimensions
num_rows, num_cols = data.shape
num_frames = num_cols // 2

xs = np.arange(0, num_cols, 2)
ys = np.arange(1, num_cols, 2)

# Liste des seuils à tester
THRESH_VALUES = np.round(np.linspace(0, 30.1, 301), 1) 
# np.linspace permet d'obtenir 201 valeurs espacées de la même valeur, de 0.1 donc, 
# sans cela le stockage de snombres n'est pas binaire

# Préparer un cycle de couleurs
colors = plt.cm.tab10.colors
num_colors = len(colors)

# Créer dossier output si besoin
os.makedirs("output", exist_ok=True)

output_folder = "output/value_threshold"
os.makedirs(output_folder, exist_ok=True)

frame = 2 # seulement tester sur la 3e image

for thresh in tqdm(THRESH_VALUES, desc="Seuils testés", unit="seuil"):

    #print(f"\n===== Test du seuil : {thresh} =====")

    joints, evals, datapoints = segment_growing(
        data, xs, ys, num_rows, num_cols, thresh
    )

    #print("Positions des joints :", joints)
    #print("Nombre d'évaluations :", evals)
    #print("Nombre de points traités :", datapoints)

    all_segments = []

    x = data[:, xs[frame]]
    y = data[:, ys[frame]]

    plt.figure()
    plt.plot(x, y, 'k.-', alpha=0.5)

    start = 0
    frame_segments = []

    for idx, j in enumerate(joints):
        color = colors[idx % num_colors]
        seg_x = [x[start], x[j]]
        seg_y = [y[start], y[j]]

        plt.plot(seg_x, seg_y, color=color, linewidth=3)

        frame_segments.append((seg_x, seg_y))
        start = j

    all_segments.append(frame_segments)

    plt.title(f"Frame {frame} - thresh={thresh}")
    plt.axis("equal")

    # sauvegarde image avec seuil dans le nom
    thresh_str = f"{thresh:05.1f}"
    plt.savefig(os.path.join(output_folder, f"206_fs1_thresh_thresh_{thresh_str}_frame_{frame}.png"))
    #plt.savefig(f"output/value_threshold/206_fs1_thresh_{thresh}_frame_{frame}.png")
    plt.close()

# Export CSV des segments pour ce seuil
# filename = f"output/202_fs1_thresh_{thresh}_seg.csv"
# with open(filename, 'w', newline='') as f:
#    writer = csv.writer(f)
#    writer.writerow(["frame", "segment", "x0", "y0", "x1", "y1"])

#    for frame_idx, frame_segments in enumerate(all_segments):
#        for seg_idx, (seg_x, seg_y) in enumerate(frame_segments):
#            writer.writerow([frame_idx, seg_idx,
#                             seg_x[0], seg_y[0],
#                             seg_x[1], seg_y[1]])

print(f"Export terminé pour thresh = {thresh}")
