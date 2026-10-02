%time
# sistema
import os

# manipulacion datos
import pandas as pd
import numpy as np
from dbfread import DBF

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
                elif extension == '.dbf':
                    tabla_aux = DBF(ruta_completa)
                    frames_[nombre_sin_ext] = pd.DataFrame(iter(tabla_aux))
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

nom_banxico = list(frames_banxico.keys())
nom_banxico

for i in nom_banxico:
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

ruta_enif_24 = ruta_bases + "/inegi_20260906/enif_2024_csv"

frames_enif_24 = camina_carpetas(ruta_busqueda= ruta_enif_24)

TVIVIENDA_24 = TVIVIENDA
del TVIVIENDA
TVIVIENDA_24.sample(n=10, random_state=787)

TSDEM_24 = TSDEM
del TSDEM
TSDEM_24.sample(n=10, random_state=787)

TMODULO_24 = TMODULO
del TMODULO
TMODULO_24.sample(n=10, random_state=787)

THOGAR_24 = THOGAR
del THOGAR
THOGAR_24.sample(n=10, random_state=787)

ruta_enif_21 = ruta_bases + "/inegi_20260906/enif_2021_csv"

frames_enif_21 = camina_carpetas(ruta_busqueda= ruta_enif_21)

TSDEM_21 = TSDEM
del TSDEM
TSDEM_21.sample(n=10, random_state=787)

TMODULO_21 = TMODULO
del TMODULO
TMODULO_21.sample(n=10, random_state=787)

TVIVIENDA_21 = TVIVIENDA
del TVIVIENDA
TVIVIENDA_21.sample(n=10, random_state=787)

THOGAR_21 = THOGAR
del THOGAR
THOGAR_21.sample(n=10, random_state=787)

ruta_enif_18 = ruta_bases + "/inegi_20260906/enif_2018_dbf"

frames_enif_18 = camina_carpetas(ruta_busqueda= ruta_enif_18)

TSDEM_18 = tsdem
del tsdem
TSDEM_18.sample(n=10, random_state=787)

TMODULO_18 = tmodulo
del tmodulo
TMODULO_18.sample(n=10, random_state=787)

TMODULO2_18 = tmodulo2
del tmodulo2
TMODULO2_18.sample(n=10, random_state=787)

TVIVIENDA_18 = tvivienda
del tvivienda
TVIVIENDA_18.sample(n=10, random_state=787)

ruta_enif_15 = ruta_bases + "/inegi_20260906/enif_2015_dbf"

frames_enif_15 = camina_carpetas(ruta_busqueda= ruta_enif_15)

TSDEM_15 = tsdem
del tsdem
TSDEM_15.sample(n=10, random_state=787)

TMODULO1_15 = tmodulo1
del tmodulo1
TMODULO1_15.sample(n=10, random_state=787)

TMODULO2_15 = tmodulo2
del tmodulo2
TMODULO2_15.sample(n=10, random_state=787)

TMODULO3_15 = tmodulo3
del tmodulo3
TMODULO3_15.sample(n=10, random_state=787)

TVIVIENDA_15 = tvivienda
del tvivienda
TVIVIENDA_15.sample(n=10, random_state=787)

ruta_enif_12 = ruta_bases + "/inegi_20260906/enif_2012_dbf"

frames_enif_12 = camina_carpetas(ruta_busqueda= ruta_enif_12)

TSDEM_12 = stsdem_e2
del stsdem_e2
TSDEM_12.sample(n=10, random_state=787)

TMODULO1_12 = stmodulo1_e2
del stmodulo1_e2
TMODULO1_12.sample(n=10, random_state=787)

TMODULO2_12 = stmodulo2_e2
del stmodulo2_e2
TMODULO2_12.sample(n=10, random_state=787)

TVIVIENDA_12 = stvivienda_e2
del stvivienda_e2
TVIVIENDA_12.sample(n=10, random_state=787)

nom_ = nom_banxico + nom_condusef + nom_cnsf
nom_