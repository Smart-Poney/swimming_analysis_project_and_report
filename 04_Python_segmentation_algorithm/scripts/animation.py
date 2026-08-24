import imageio.v2 as imageio
import os
import re
from tqdm import tqdm

image_folder = "output/value_threshold"
output_folder = "output/animation"
os.makedirs(output_folder, exist_ok=True)
output_gif = "output/animation/animation_thresh.gif"

files = [f for f in os.listdir(image_folder) if f.endswith(".png")]

# fonction pour extraire le seuil depuis le nom de fichier
def extract_thresh(filename):
    match = re.search(r"thresh_([0-9.]+)", filename)
    return float(match.group(1))

# tri numérique par thresh
files_sorted = sorted(files, key=extract_thresh)

images = []
for filename in tqdm(files_sorted, desc='Img treated', unit='img'):
    path = os.path.join(image_folder, filename)
    images.append(imageio.imread(path))

imageio.mimsave(output_gif, images, duration=0.1)

print("GIF créé :", output_gif)