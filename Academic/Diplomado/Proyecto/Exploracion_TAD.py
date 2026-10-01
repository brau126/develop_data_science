%time
# sistema
import os

# manipulacion datos
import pandas as pd
import numpy as np

# graficación
import matplotlib.pyplot as plt
import plotly.express as px
import seaborn as sns

# ruta con archivos
ruta_bases = "/home/brauliocb126/Documents/Diplomado/Proyecto/Bases" #cpu
#ruta_bases = "/home/brauliocbd15/Documents/Diplomado/Proyecto/Bases" #laptop

## diccionario dataframes
#dfs = {}
#
## 2. Recorrer de forma recursiva subcarpetas y archivos con os.walk
#for raiz, carpetas, archivos in os.walk(ruta_bases):
#    for archivo in archivos:
#        # Obtener la ruta completa del archivo
#        ruta_completa = os.path.join(raiz, archivo)
#        
#        # Extraer el nombre sin extensión y la extensión en minúsculas
#        nombre_sin_ext, extension = os.path.splitext(archivo)
#        extension = extension.lower()
#
#        try:
#            # Identificar el formato por la extensión
#            if extension in ['.csv', '.txt']:
#                dfs[nombre_sin_ext] = pd.read_csv(ruta_completa)
#            elif extension in ['.parquet', '.pq']:
#                dfs[nombre_sin_ext] = pd.read_parquet(ruta_completa)
#            elif extension in ['.xlsx', '.xls']:
#                dfs[nombre_sin_ext] = pd.read_excel(ruta_completa)
#            elif extension == '.json':
#                dfs[nombre_sin_ext] = pd.read_json(ruta_completa)
#            elif extension in ['.feather', '.ft']:
#                dfs[nombre_sin_ext] = pd.read_feather(ruta_completa)
#            else:
#                print(f"⚠️ Formato omitido: {archivo}")
#                continue
#
#            print(f"✅ Cargado exitosamente: {os.path.relpath(ruta_completa, ruta_bases)}")
#
#        except Exception as e:
#            print(f"❌ Error al cargar {archivo}: {e}")
#
## 3. Ver resumen de bases cargadas
#print("\n--- Resumen de Bases Cargadas ---")
#for nombre, df in dfs.items():
#    print(f"- {nombre}: {df.shape[0]} filas, {df.shape[1]} columnas")

#for nombre_var, df in dfs.items():
#    # Limpiamos el nombre para asegurar que sea un nombre de variable válido en Python
#    nombre_limpio = nombre_var.replace(" ", "_").replace("-", "_")
#    
#    # Asignamos el DataFrame a una variable global con ese nombre
#    globals()[nombre_limpio] = df
#    print(f"Variable creada: {nombre_limpio}")

#nom_bases = list(dfs.keys())
#nom_bases

# información de créditos vigentes banxico
ruta_cred_banxico = "/banxico_20260906"

for raiz, carpteas, archivos in os.walk(os.path.join(ruta_bases, ruta_cred_banxico)):
    for i in archivos:
        ruta_full = os.path.join(ruta_bases, ruta_cred_banxico, i)

        nombre, extension = os.path.splitext(i)
        extension = extension.lower()
        
        try:
            if extension in ['.csv', '.txt']:
                nombre = pd.read_csv(ruta_full)
            elif extension in ['.parquet', '.pq']:
                nombre = pd.read_parquet(ruta_full)
            elif extension in ['.xlsx', '.xls']:
                nombre = pd.read_excel(ruta_full)
            elif extension == '.json':
                nomrbe = pd.read_json(ruta_full)
            elif extension in ['.feather', '.ft']:
                nombre = pd.read_feather(ruta_full)
            else:
                print(f"⚠️ Formato omitido: {i}")
                continue

            print(f"✅ Cargado exitosamente: {os.path.relpath(ruta_full, ruta_bases)}")

        except Exception as e:
            print(f"❌ Error al cargar {i}: {e}")



for i in credito_banxico:
    n = i.split(sep="_")[1]
    columnas_credito = {'Banco de México': 'fecha',
                        'Unnamed: 1': 'saldo_vigente_'+n}
    mod =  globals()[i]
    mod.drop(index=np.arange(0,17,1), inplace=True)
    mod.rename(columns=columnas_credito, inplace=True)
    mod['fecha'] = pd.to_datetime(mod['fecha'])
    mod['saldo_vigente_'+n] = mod['saldo_vigente_'+n].astype('float64')
    display(mod)

credito_tarjeta_credito.dtypes