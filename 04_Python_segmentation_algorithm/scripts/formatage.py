import os
import pandas as pd

# Dossiers
input_folder = 'data_matlab'
output_folder = 'output/formated_data'

# Crée le dossier de sortie s'il n'existe pas
os.makedirs(output_folder, exist_ok=True)

# Parcours des fichiers xls/xlsx
for filename in os.listdir(input_folder):
    if filename.endswith('.xls') or filename.endswith('.xlsx'):
        file_path = os.path.join(input_folder, filename)
        
        # Lecture du fichier Excel
        df = pd.read_excel(file_path, dtype=str)  # tout en str
        
        # Remplacement des ',' par '.' dans toutes les colonnes
        for col in df.columns:
            df[col] = df[col].astype(str).str.replace(',', '.', regex=False)
        
        # Remplacer les colonnes 'Unnamed' par des chaînes vides
        new_columns = []
        for col in df.columns:
            col_str = str(col)  # conversion en str pour éviter l'erreur
            if 'Unnamed' in col_str:
                new_columns.append('')
            else:
                new_columns.append(col_str)
        df.columns = new_columns
        
        # Chemin du fichier CSV de sortie
        output_file = os.path.join(output_folder, os.path.splitext(filename)[0] + '.csv')
        
        # Export en CSV avec ',' comme séparateur
        df.to_csv(output_file, index=False, sep=',', encoding='utf-8')

print("Formatage OK")