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

def camina_carpetas(ruta_busqueda):
    frames_ = {}

    # Recorrer de forma recursiva subcarpetas y archivos con os.walk
    for raiz, carpetas, archivos in os.walk(ruta_busqueda):
        for archivo in archivos:
            # Obtener la ruta completa del archivo
            ruta_completa = os.path.join(raiz, archivo)
            
            # Extraer el nombre sin extensión y la extensión en minúsculas
            nombre_sin_ext, extension = os.path.splitext(archivo)
            extension = extension.lower()

            try:
                # Identificar el formato por la extensión
                if extension in ['.csv', '.txt']:
                    frames_[nombre_sin_ext] = pd.read_csv(ruta_completa)
                elif extension in ['.parquet', '.pq']:
                    frames_[nombre_sin_ext] = pd.read_parquet(ruta_completa)
                elif extension in ['.xlsx', '.xls']:
                    frames_[nombre_sin_ext] = pd.read_excel(ruta_completa)
                elif extension == '.json':
                    frames_[nombre_sin_ext] = pd.read_json(ruta_completa)
                elif extension in ['.feather', '.ft']:
                    frames_[nombre_sin_ext] = pd.read_feather(ruta_completa)
                else:
                    print(f"⚠️ Formato omitido: {archivo}")
                    continue

                print(f"✅ Cargado exitosamente: {os.path.relpath(ruta_completa, ruta_busqueda)}")

            except Exception as e:
                print(f"❌ Error al cargar {archivo}: {e}")

    for nombre_var, f in frames_.items():
        # Limpiamos el nombre para asegurar que sea un nombre de variable válido en Python
        nombre_limpio = nombre_var.replace(" ", "_").replace("-", "_")
        
        # Asignamos el DataFrame a una variable global con ese nombre
        globals()[nombre_limpio] = f
        print(f"Variable creada: {nombre_limpio}")

    return frames_

# información de créditos vigentes banxico
ruta_cred_banxico = ruta_bases + "/banxico_20260906"

frames_banxico = camina_carpetas(ruta_busqueda = ruta_cred_banxico)

nom = list(frames_banxico.keys())
nom

for i in nom:
    n = i.split(sep="_")[1]
    columnas_credito = {'Banco de México': 'fecha',
                        'Unnamed: 1': 'saldo_vigente_'+n}
    mod =  globals()[i]
    mod.drop(index=np.arange(0,17,1), inplace=True)
    mod.rename(columns=columnas_credito, inplace=True)
    mod['fecha'] = pd.to_datetime(mod['fecha'])
    mod['saldo_vigente_'+n] = mod['saldo_vigente_'+n].astype('float64')
    display(i, mod)

# información de comision de seguros y fianzas
ruta_cnsf = ruta_bases + "/cnsf_20260904"

frames_cnsf = camina_carpetas(ruta_busqueda= ruta_cnsf)

nom_cnsf = list(frames_cnsf.keys())
nom_cnsf

for i in nom_cnsf:
    display(i, globals()[i])

# información de comision de defensa a usuarios de entidades financieras
ruta_condusef = ruta_bases + "/condusef_20260906"

frames_condusef = camina_carpetas(ruta_busqueda= ruta_condusef)

nom_condusef = list(frames_condusef.keys())
nom_condusef

for i in nom_condusef:
    display(i, globals()[i])