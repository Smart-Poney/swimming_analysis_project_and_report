# to remove all no-use data (img/curves)

import os
from tqdm import tqdm

# Dossiers
input_folder = '.'

# liste fichiers image
folder = [f.path for f in os.scandir(input_folder) if f.is_dir()]

# boucle suppression image

for folder in tqdm(folder, desc="Img suppresion", unit="folder"):
    
    for file in os.listdir(folder):

        file_path = os.path.join(folder, file)

        if file.lower().endswith(".jpg"):
            os.remove(file_path)

print("End.")
