# to remove all no-use data (img/curves)
# careful before using it
# be aware that it removes ALL PNG without warning

import os
from tqdm import tqdm

while True:
    WARNING = (input("Running this will remove ALL PNG included in this folder, are you sure ? : y/n "))
    
    if WARNING == "y":
        print("Proceeding...")
        break
    elif WARNING == "n":
        print("Operation cancelled.")
        exit()
    else:
        print("Erreur : y/n")

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
