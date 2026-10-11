#%time
# sistema
import os

# regular expresions
import re

# herrameintas
from functools import reduce

# manipulacion datos
import pandas as pd
import numpy as np
from dbfread import DBF

# graficación
import matplotlib.pyplot as plt
import plotly.express as px
import seaborn as sns

# estadística
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_auc_score,accuracy_score,confusion_matrix
from sklearn.linear_model import LogisticRegression
from scipy.stats import ks_2samp
from sklearn.feature_selection import VarianceThreshold
from varclushi import VarClusHi

#
from sklearn.preprocessing import KBinsDiscretizer
from sklearn.impute import SimpleImputer


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

# cambiamos el nombre de algunas columnas para no tener repetidos
globals()['credito_hipotecario_media_residencial'].rename(columns={'saldo_vigente_hipotecario':'saldo_vigente_hipotecario_residencial'}, inplace=True)
globals()['credito_hipotecario_vivienda_social'].rename(columns={'saldo_vigente_hipotecario':'saldo_vigente_hipotecario_social'}, inplace=True)

globals()['credito_hipotecario_vivienda_social'].columns

globals()['credito_hipotecario_media_residencial'].columns

for i in np.arange(0,len(nom_banxico)-1,1):
    if i == 0:
        aux1 = globals()[nom_banxico[i]]
        aux2 = globals()[nom_banxico[i+1]]
        df_credito_banxico = pd.merge(left=aux1,
                                      right=aux2,
                                      how='outer',
                                      on='fecha',
                                      validate='one_to_one'
                                      )
    else:
        aux3 = globals()[nom_banxico[i+1]]
        df_credito_banxico = pd.merge(left=df_credito_banxico,
                                      right=aux3,
                                      how='outer',
                                      on='fecha',
                                      validate='one_to_one'
                                      )

df_credito_banxico.sample(n=10, random_state=787)

df_credito_banxico['saldo_vigente_pf'] = df_credito_banxico['saldo_vigente_personal'] + df_credito_banxico['saldo_vigente_hipotecario_residencial'] + df_credito_banxico['saldo_vigente_nomina'] + df_credito_banxico['saldo_vigente_consumo'] + df_credito_banxico['saldo_vigente_tarjeta'] + df_credito_banxico['saldo_vigente_hipotecario_social']

df_credito_banxico.columns

for i in nom_banxico:
    del globals()[i]

mapeo_enif = ruta_bases + "/inegi_20260906/ENIF_CATALOGO_GLOBAL_NOMBRES_COMPLETOS.xlsx"

mapeo_global = pd.read_excel(mapeo_enif, sheet_name="mapeo_global")

mapeo_global = mapeo_global[['anio','tabla','mnemonico_original','snake_case']]

mapeo_global

ruta_enif_24 = ruta_bases + "/inegi_20260906/enif_2024_csv"

frames_enif_24 = camina_carpetas(ruta_busqueda= ruta_enif_24)

mapeo_2024_tsdem        = mapeo_global[(mapeo_global['anio']==2024) & (mapeo_global['tabla']=='TSDEM')].reset_index(drop=True)
mapeo_2024_tmodulo      = mapeo_global[(mapeo_global['anio']==2024) & (mapeo_global['tabla']=='TMODULO')].reset_index(drop=True)
mapeo_2024_tvivienda    = mapeo_global[(mapeo_global['anio']==2024) & (mapeo_global['tabla']=='TVIVIENDA')].reset_index(drop=True)
mapeo_2024_thogar       = mapeo_global[(mapeo_global['anio']==2024) & (mapeo_global['tabla']=='THOGAR')].reset_index(drop=True)

display(mapeo_2024_thogar)
display(mapeo_2024_tmodulo)
display(mapeo_2024_tsdem)
display(mapeo_2024_tvivienda)

dic_mapeo_2024_thogar       = dict(zip(mapeo_2024_thogar['mnemonico_original'],     mapeo_2024_thogar['snake_case']))
dic_mapeo_2024_tmodulo      = dict(zip(mapeo_2024_tmodulo['mnemonico_original'],    mapeo_2024_tmodulo['snake_case']))
dic_mapeo_2024_tsdem        = dict(zip(mapeo_2024_tsdem['mnemonico_original'],      mapeo_2024_tsdem['snake_case']))
dic_mapeo_2024_tvivienda    = dict(zip(mapeo_2024_tvivienda['mnemonico_original'],  mapeo_2024_tvivienda['snake_case']))

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

TMODULO_24 = TMODULO_24.rename(columns=dic_mapeo_2024_tmodulo)

TMODULO_24.sample(n=10,random_state=787)

TSDEM_24 = TSDEM_24.rename(columns=dic_mapeo_2024_tsdem)

TSDEM_24.sample(n=10,random_state=787)

TVIVIENDA_24 = TVIVIENDA_24.rename(columns=dic_mapeo_2024_tvivienda)

TVIVIENDA_24.sample(n=10,random_state=787)

THOGAR_24 = TMODULO_24.rename(columns=dic_mapeo_2024_thogar)

THOGAR_24.sample(n=10,random_state=787)

# 2024, identificamos las llaves primarias y foraneas de la tablas deacuerdo a la figura de entidad relación de ENIF 2024
print(f"TMODULO: {TMODULO_24.shape[0]}")
print(f" - persona elegida (llavemod): {TMODULO_24.drop_duplicates(subset='id_persona_elegida').shape[0]}")
print(f" - vivienda: {TMODULO_24.drop_duplicates(subset='id_vivienda').shape[0]}")
print(f" - hogar: {TMODULO_24.drop_duplicates(subset='id_hogar').shape[0]}")

print(f"TSDEM: {TSDEM_24.shape[0]}")
print(f" - persona hogar (llavesde): {TSDEM_24.drop_duplicates(subset='id_persona_hogar').shape[0]}")
print(f" - vivienda: {TSDEM_24.drop_duplicates(subset='id_vivienda').shape[0]}")
print(f" - hogar: {TSDEM_24.drop_duplicates(subset='id_hogar').shape[0]}")

print(f"THOGAR: {THOGAR_24.shape[0]}")
print(f" - hogar: {THOGAR_24.drop_duplicates(subset='id_hogar').shape[0]}")
print(f" - vivienda: {THOGAR_24.drop_duplicates(subset='id_vivienda').shape[0]}")

print(f"TVIVIENDA: {TVIVIENDA_24.shape[0]}")
print(f" - vivienda: {TVIVIENDA_24.drop_duplicates(subset='id_vivienda').shape[0]}")

# sdem con modulo
llaves = ['id_hogar',
          'id_vivienda',
          'id_persona_elegida']

TSDEM_24 = TSDEM_24.rename(columns={'id_persona_hogar':'id_persona_elegida'})

cols_dup = TMODULO_24.columns.intersection(TSDEM_24.columns).difference(llaves)

enif_2024_t2 = pd.merge(left=TSDEM_24,
                        right=TMODULO_24.drop(columns=cols_dup),
                        how='right',
                        on=llaves
                        )

enif_2024_t2

# hogar con sdem-modulo
llaves = ['id_hogar',
          'id_vivienda']

cols_dup = THOGAR_24.columns.intersection(enif_2024_t2.columns).difference(llaves)

enif_2024_t3 = pd.merge(left=enif_2024_t2,
                        right=THOGAR_24.drop(columns=cols_dup),
                        how='left',
                        on=llaves
                        )

enif_2024_t3

# vivienda con sdem-modulo-hogar
llaves = ['id_vivienda']

cols_dup = TVIVIENDA_24.columns.intersection(enif_2024_t3.columns).difference(llaves)

enif_2024_t4 = pd.merge(left=enif_2024_t3,
                        right=TVIVIENDA_24.drop(columns=cols_dup),
                        how='left',
                        on=llaves
                        )

enif_2024_t4

enif_2024_full = enif_2024_t4
enif_2024_full['anio'] = '2024'
enif_2024_full.columns

del TMODULO_24, TSDEM_24, THOGAR_24, enif_2024_t2, TVIVIENDA_24, enif_2024_t3, enif_2024_t4

ruta_enif_21 = ruta_bases + "/inegi_20260906/enif_2021_csv"

frames_enif_21 = camina_carpetas(ruta_busqueda= ruta_enif_21)

mapeo_2021_tsdem        = mapeo_global[(mapeo_global['anio']==2021) & (mapeo_global['tabla']=='TSDEM')].reset_index(drop=True)
mapeo_2021_tmodulo      = mapeo_global[(mapeo_global['anio']==2021) & (mapeo_global['tabla']=='TMODULO')].reset_index(drop=True)
mapeo_2021_tvivienda    = mapeo_global[(mapeo_global['anio']==2021) & (mapeo_global['tabla']=='TVIVIENDA')].reset_index(drop=True)
mapeo_2021_thogar       = mapeo_global[(mapeo_global['anio']==2021) & (mapeo_global['tabla']=='THOGAR')].reset_index(drop=True)

display(mapeo_2021_thogar)
display(mapeo_2021_tmodulo)
display(mapeo_2021_tsdem)
display(mapeo_2021_tvivienda)

dic_mapeo_2021_thogar    = dict(zip(mapeo_2021_thogar['mnemonico_original'],    mapeo_2021_thogar['snake_case']))
dic_mapeo_2021_tmodulo   = dict(zip(mapeo_2021_tmodulo['mnemonico_original'],   mapeo_2021_tmodulo['snake_case']))
dic_mapeo_2021_tsdem     = dict(zip(mapeo_2021_tsdem['mnemonico_original'],     mapeo_2021_tsdem['snake_case']))
dic_mapeo_2021_tvivienda = dict(zip(mapeo_2021_tvivienda['mnemonico_original'], mapeo_2021_tvivienda['snake_case']))

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

TMODULO_21 = TMODULO_21.rename(columns=dic_mapeo_2021_tmodulo)

TMODULO_21.sample(n=10,random_state=787)

TSDEM_21 = TSDEM_21.rename(columns=dic_mapeo_2021_tsdem)

TSDEM_21.sample(n=10,random_state=787)

TVIVIENDA_21 = TVIVIENDA_21.rename(columns=dic_mapeo_2021_tvivienda)

TVIVIENDA_21.sample(n=10,random_state=787)

THOGAR_21 = TMODULO_21.rename(columns=dic_mapeo_2021_thogar)

THOGAR_21.sample(n=10,random_state=787)

# 2021, identificamos las llaves primarias y foraneas de la tablas deacuerdo a la figura de entidad relación de ENIF 2021
print(f"TMODULO: {TMODULO_21.shape[0]}")
print(f" - folio (llavemod): {TMODULO_21.drop_duplicates(subset='folio').shape[0]}")
print(f" - vivienda: {TMODULO_21.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")
print(f" - hogar: {TMODULO_21.drop_duplicates(subset='numero_hogar_vivienda').shape[0]}")
print(f" - renglon persona: {TMODULO_21.drop_duplicates(subset='numero_renglon_persona_elegida').shape[0]}")

print(f"TSDEM: {TSDEM_21.shape[0]}")
print(f" - folio: {TSDEM_21.drop_duplicates(subset='folio').shape[0]}")
print(f" - vivienda: {TSDEM_21.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")
print(f" - hogar: {TSDEM_21.drop_duplicates(subset='numero_hogar_vivienda').shape[0]}")
print(f" - renglon persona: {TSDEM_21.drop_duplicates(subset='numero_renglon_persona').shape[0]}")

print(f"THOGAR: {THOGAR_21.shape[0]}")
print(f" - folio: {THOGAR_21.drop_duplicates(subset='folio').shape[0]}")
print(f" - vivienda: {THOGAR_21.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")
print(f" - hogar: {THOGAR_21.drop_duplicates(subset='numero_hogar_vivienda').shape[0]}")

print(f"TVIVIENDA: {TVIVIENDA_21.shape[0]}")
print(f" - folio: {TVIVIENDA_21.drop_duplicates(subset='folio').shape[0]}")
print(f" - vivienda: {TVIVIENDA_21.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")

# sdem con modulo
llaves = ['folio',
          'numero_vivienda_seleccionada',
          'numero_hogar_vivienda',
          'numero_renglon_persona_elegida']

TSDEM_21.rename(columns={'numero_renglon_persona':'numero_renglon_persona_elegida'}, inplace=True)

cols_dup = TMODULO_21.columns.intersection(TSDEM_21.columns).difference(llaves)

enif_2021_t2 = pd.merge(left=TSDEM_21,
                        right=TMODULO_21.drop(columns=cols_dup),
                        how='right',
                        on=llaves
                        )

enif_2021_t2

# hogar con sdem-modulo
llaves = ['folio',
          'numero_vivienda_seleccionada',
          'numero_hogar_vivienda']

cols_dup = THOGAR_21.columns.intersection(enif_2021_t2.columns).difference(llaves)

enif_2021_t3 = pd.merge(left=enif_2021_t2,
                        right=THOGAR_21.drop(columns=cols_dup),
                        how='left',
                        on=llaves
                        )

enif_2021_t3

# vivienda con sdem-modulo-hogar
llaves = ['folio',
          'numero_vivienda_seleccionada']

cols_dup = TVIVIENDA_21.columns.intersection(enif_2021_t3.columns).difference(llaves)

enif_2021_t4 = pd.merge(left=enif_2021_t3,
                        right=TVIVIENDA_21.drop(columns=cols_dup),
                        how='left',
                        on=llaves
                        )

enif_2021_t4

enif_2021_full = enif_2021_t4
enif_2021_full['anio'] = '2021'
enif_2021_full.columns

del TMODULO_21, TSDEM_21, THOGAR_21, enif_2021_t2, TVIVIENDA_21, enif_2021_t3, enif_2021_t4

ruta_enif_18 = ruta_bases + "/inegi_20260906/enif_2018_dbf"

frames_enif_18 = camina_carpetas(ruta_busqueda= ruta_enif_18)

mapeo_global[mapeo_global['anio']==2018].drop_duplicates(subset='tabla')[['anio','tabla']]

mapeo_2018_tsdem        = mapeo_global[(mapeo_global['anio']==2018) & (mapeo_global['tabla']=='TSDEM')].reset_index(drop=True)
mapeo_2018_tmodulo      = mapeo_global[(mapeo_global['anio']==2018) & (mapeo_global['tabla']=='TMODULO')].reset_index(drop=True)
mapeo_2018_tmodulo2     = mapeo_global[(mapeo_global['anio']==2018) & (mapeo_global['tabla']=='TMODULO2')].reset_index(drop=True)
mapeo_2018_tvivienda    = mapeo_global[(mapeo_global['anio']==2018) & (mapeo_global['tabla']=='TVIVIENDA')].reset_index(drop=True)

display(mapeo_2018_tmodulo)
display(mapeo_2018_tmodulo2)
display(mapeo_2018_tsdem)
display(mapeo_2018_tvivienda)

dic_mapeo_2018_tmodulo      = dict(zip(mapeo_2018_tmodulo['mnemonico_original'],    mapeo_2018_tmodulo['snake_case']))
dic_mapeo_2018_tmodulo2     = dict(zip(mapeo_2018_tmodulo2['mnemonico_original'],   mapeo_2018_tmodulo2['snake_case']))
dic_mapeo_2018_tsdem        = dict(zip(mapeo_2018_tsdem['mnemonico_original'],      mapeo_2018_tsdem['snake_case']))
dic_mapeo_2018_tvivienda    = dict(zip(mapeo_2018_tvivienda['mnemonico_original'],  mapeo_2018_tvivienda['snake_case']))

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

TMODULO_18 = TMODULO_18.rename(columns=dic_mapeo_2018_tmodulo)

TMODULO_18.sample(n=10,random_state=787)

TMODULO2_18 = TMODULO2_18.rename(columns=dic_mapeo_2018_tmodulo2)

TMODULO2_18.sample(n=10,random_state=787)

TSDEM_18 = TSDEM_18.rename(columns=dic_mapeo_2018_tsdem)

TSDEM_18.sample(n=10,random_state=787)

TVIVIENDA_18 = TVIVIENDA_18.rename(columns=dic_mapeo_2018_tvivienda)

TVIVIENDA_18.sample(n=10,random_state=787)

# 2021, identificamos las llaves primarias y foraneas de la tablas deacuerdo a la figura de entidad relación de ENIF 2021
print(f"TMODULO: {TMODULO_18.shape[0]}")
print(f" - upm identificador: {TMODULO_18.drop_duplicates(subset='upm_identificador').shape[0]}")
print(f" - vivienda: {TMODULO_18.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")
print(f" - hogar: {TMODULO_18.drop_duplicates(subset='numero_hogar_vivienda').shape[0]}")
print(f" - renglon persona: {TMODULO_18.drop_duplicates(subset='numero_renglon_persona_elegida').shape[0]}")
#
print(f"TMODULO2: {TMODULO2_18.shape[0]}")
print(f" - upm identificador: {TMODULO2_18.drop_duplicates(subset='upm_identificador').shape[0]}")
print(f" - vivienda: {TMODULO2_18.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")
print(f" - hogar: {TMODULO2_18.drop_duplicates(subset='numero_hogar_vivienda').shape[0]}")
print(f" - renglon persona: {TMODULO2_18.drop_duplicates(subset='numero_renglon_persona_elegida').shape[0]}")
#
print(f"TSDEM: {TSDEM_18.shape[0]}")
print(f" - upm identificador: {TSDEM_18.drop_duplicates(subset='upm_identificador').shape[0]}")
print(f" - vivienda: {TSDEM_18.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")
print(f" - hogar: {TSDEM_18.drop_duplicates(subset='numero_hogar_vivienda').shape[0]}")
#
print(f"TVIVIENDA: {TVIVIENDA_18.shape[0]}")
print(f" - upm identificador: {TVIVIENDA_18.drop_duplicates(subset='upm_identificador').shape[0]}")
print(f" - vivienda: {TVIVIENDA_18.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")

# modulo2 con modulo
llaves = ['upm_identificador',
          'numero_vivienda_seleccionada',
          'numero_hogar_vivienda',
          'numero_renglon_persona_elegida'
          ]

cols_dup = (TMODULO_18.columns.intersection(TMODULO2_18.columns).difference(llaves))

enif_2018_t2 = pd.merge(left=TMODULO_18,
                        right=TMODULO2_18.drop(columns=cols_dup),
                        how='left',
                        on=llaves,
                        validate='one_to_one'#,
                        #indicator=True
                        )

enif_2018_t2

# sdem con modulo2-modulo
llaves = ['upm_identificador',
          'numero_vivienda_seleccionada',
          'numero_hogar_vivienda',
          'numero_renglon_persona_elegida'
          ]

TSDEM_18 = TSDEM_18.rename(columns={'numero_renglon_persona':'numero_renglon_persona_elegida'})

cols_dup = TSDEM_18.columns.intersection(enif_2018_t2.columns).difference(llaves)

enif_2018_t3 = pd.merge(left=TSDEM_18,
                        right=enif_2018_t2.drop(columns=cols_dup),
                        how='right',
                        on=llaves,
                        )

enif_2018_t3

# vivienda con sdem-modulo2-modulo
llaves = ['upm_identificador',
          'numero_vivienda_seleccionada'
          ]

cols_dup = TVIVIENDA_18.columns.intersection(enif_2018_t3.columns).difference(llaves)

enif_2018_t4 = pd.merge(left=TVIVIENDA_18,
                        right=enif_2018_t3.drop(columns=cols_dup),
                        how='right',
                        on=llaves
                        )

enif_2018_t4

enif_2018_full = enif_2018_t4
enif_2018_full['anio'] = '2018'
enif_2018_full.columns

# libereacion de memoria
del TMODULO_18, TMODULO2_18, TSDEM_18, TVIVIENDA_18, enif_2018_t2, enif_2018_t3, enif_2018_t4

ruta_enif_15 = ruta_bases + "/inegi_20260906/enif_2015_dbf"

frames_enif_15 = camina_carpetas(ruta_busqueda= ruta_enif_15)

mapeo_global[mapeo_global['anio']==2015].drop_duplicates(subset='tabla')[['anio','tabla']]

mapeo_2015_tsdem        = mapeo_global[(mapeo_global['anio']==2015) & (mapeo_global['tabla']=='TSDEM')].reset_index(drop=True)
mapeo_2015_tmodulo1     = mapeo_global[(mapeo_global['anio']==2015) & (mapeo_global['tabla']=='TMODULO1')].reset_index(drop=True)
mapeo_2015_tmodulo2     = mapeo_global[(mapeo_global['anio']==2015) & (mapeo_global['tabla']=='TMODULO2')].reset_index(drop=True)
mapeo_2015_tmodulo3     = mapeo_global[(mapeo_global['anio']==2015) & (mapeo_global['tabla']=='TMODULO3')].reset_index(drop=True)
mapeo_2015_tvivienda    = mapeo_global[(mapeo_global['anio']==2015) & (mapeo_global['tabla']=='TVIVIENDA')].reset_index(drop=True)

display(mapeo_2015_tmodulo1)
display(mapeo_2015_tmodulo2)
display(mapeo_2015_tmodulo3)
display(mapeo_2015_tsdem)
display(mapeo_2015_tvivienda)

dic_mapeo_2015_tmodulo1     = dict(zip(mapeo_2015_tmodulo1['mnemonico_original'],   mapeo_2015_tmodulo1['snake_case']))
dic_mapeo_2015_tmodulo2     = dict(zip(mapeo_2015_tmodulo2['mnemonico_original'],   mapeo_2015_tmodulo2['snake_case']))
dic_mapeo_2015_tmodulo3     = dict(zip(mapeo_2015_tmodulo3['mnemonico_original'],   mapeo_2015_tmodulo3['snake_case']))
dic_mapeo_2015_tsdem        = dict(zip(mapeo_2015_tsdem['mnemonico_original'],      mapeo_2015_tsdem['snake_case']))
dic_mapeo_2015_tvivienda    = dict(zip(mapeo_2015_tvivienda['mnemonico_original'],  mapeo_2015_tvivienda['snake_case']))

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

TMODULO1_15 = TMODULO1_15.rename(columns=dic_mapeo_2015_tmodulo1)

TMODULO1_15.sample(n=10,random_state=787)

TMODULO2_15 = TMODULO2_15.rename(columns=dic_mapeo_2015_tmodulo2)

TMODULO2_15.sample(n=10,random_state=787)

TMODULO3_15 = TMODULO3_15.rename(columns=dic_mapeo_2015_tmodulo3)

TMODULO3_15.sample(n=10,random_state=787)

TSDEM_15 = TSDEM_15.rename(columns=dic_mapeo_2015_tsdem)

TSDEM_15.sample(n=10,random_state=787)

TVIVIENDA_15 = TVIVIENDA_15.rename(columns=dic_mapeo_2015_tvivienda)

TVIVIENDA_15.sample(n=10,random_state=787)

TVIVIENDA_15.columns

# 2015, identificamos las llaves primarias y foraneas de la tablas deacuerdo a la figura de entidad relación de ENIF 2021
print(f"TMODULO1: {TMODULO1_15.shape[0]}")
print(f" - upm identificador: {TMODULO1_15.drop_duplicates(subset='upm_identificador').shape[0]}")
print(f" - vivienda: {TMODULO1_15.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")
print(f" - hogar: {TMODULO1_15.drop_duplicates(subset='numero_hogar_vivienda').shape[0]}")
print(f" - renglon persona: {TMODULO1_15.drop_duplicates(subset='numero_renglon_persona_elegida').shape[0]}")

print(f"TMODULO2: {TMODULO2_15.shape[0]}")
print(f" - upm identificador: {TMODULO2_15.drop_duplicates(subset='upm_identificador').shape[0]}")
print(f" - vivienda: {TMODULO2_15.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")
print(f" - hogar: {TMODULO2_15.drop_duplicates(subset='numero_hogar_vivienda').shape[0]}")
print(f" - renglon persona: {TMODULO2_15.drop_duplicates(subset='numero_renglon_persona_elegida').shape[0]}")

print(f"TMODULO133 {TMODULO3_15.shape[0]}")
print(f" - upm identificador: {TMODULO3_15.drop_duplicates(subset='upm_identificador').shape[0]}")
print(f" - vivienda: {TMODULO3_15.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")
print(f" - hogar: {TMODULO3_15.drop_duplicates(subset='numero_hogar_vivienda').shape[0]}")
print(f" - renglon persona: {TMODULO3_15.drop_duplicates(subset='numero_renglon_persona_elegida').shape[0]}")

print(f"TSDEM: {TSDEM_15.shape[0]}")
print(f" - upm identificador: {TSDEM_15.drop_duplicates(subset='upm_identificador').shape[0]}")
print(f" - vivienda: {TSDEM_15.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")
print(f" - hogar: {TSDEM_15.drop_duplicates(subset='numero_hogar_vivienda').shape[0]}")
print(f" - renglon persona: {TSDEM_15.drop_duplicates(subset='numero_renglon_persona').shape[0]}")

print(f"TVIVIENDA: {TVIVIENDA_15.shape[0]}")
print(f" - upm identificador: {TVIVIENDA_15.drop_duplicates(subset='upm_identificador').shape[0]}")
print(f" - vivienda: {TVIVIENDA_15.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")

# modulo2 con modulo1
llaves = ['upm_identificador',
          'numero_vivienda_seleccionada',
          'numero_hogar_vivienda',
          'numero_renglon_persona_elegida'
          ]

cols_dup = (TMODULO1_15.columns.intersection(TMODULO2_15.columns).difference(llaves))

enif_2015_t2 = pd.merge(left=TMODULO1_15,
                        right=TMODULO2_15.drop(columns=cols_dup),
                        how='left',
                        on=llaves,
                        validate='one_to_one'#,
                        #indicator=True
                        )

enif_2015_t2

# modulo3 con modulo2-modulo1
llaves = ['upm_identificador',
          'numero_vivienda_seleccionada',
          'numero_hogar_vivienda',
          'numero_renglon_persona_elegida'
          ]

cols_dup = (TMODULO3_15.columns.intersection(enif_2015_t2.columns).difference(llaves))

enif_2015_t3 = pd.merge(left=TMODULO3_15,
                        right=enif_2015_t2.drop(columns=cols_dup),
                        how='left',
                        on=llaves,
                        validate='one_to_one'#,
                        #indicator=True
                        )

enif_2015_t3

# sdem con modulo3-modulo2-modulo1
llaves = ['upm_identificador',
          'numero_vivienda_seleccionada',
          'numero_hogar_vivienda',
          'numero_renglon_persona_elegida'
          ]

TSDEM_15 = TSDEM_15.rename(columns={'numero_renglon_persona':'numero_renglon_persona_elegida'})

cols_dup = TSDEM_15.columns.intersection(enif_2015_t3.columns).difference(llaves)

enif_2015_t4 = pd.merge(left=TSDEM_15,
                        right=enif_2015_t3.drop(columns=cols_dup),
                        how='right',
                        on=llaves,
                        )

enif_2015_t4

# vivienda con sdem-modulo3-modulo2-modulo1
llaves = ['upm_identificador',
          'numero_vivienda_seleccionada'
          ]

cols_dup = TVIVIENDA_15.columns.intersection(enif_2015_t4.columns).difference(llaves)

enif_2015_t5 = pd.merge(left=enif_2015_t4,
                        right=TVIVIENDA_15.drop(columns=cols_dup),
                        how='left',
                        on=llaves
                        )

enif_2015_t5

enif_2015_full = enif_2015_t5
enif_2015_full['anio'] = '2015'
enif_2015_full.columns

del TMODULO1_15, TMODULO2_15, TMODULO3_15, TSDEM_15, TVIVIENDA_15, enif_2015_t2, enif_2015_t3, enif_2015_t4, enif_2015_t5

ruta_enif_12 = ruta_bases + "/inegi_20260906/enif_2012_dbf"

frames_enif_12 = camina_carpetas(ruta_busqueda= ruta_enif_12)

mapeo_global[mapeo_global['anio']==2012].drop_duplicates(subset='tabla')[['anio','tabla']]

mapeo_2012_tsdem        = mapeo_global[(mapeo_global['anio']==2012) & (mapeo_global['tabla']=='TSDEM')].reset_index(drop=True)
mapeo_2012_tmodulo1     = mapeo_global[(mapeo_global['anio']==2012) & (mapeo_global['tabla']=='TMODULO1')].reset_index(drop=True)
mapeo_2012_tmodulo2     = mapeo_global[(mapeo_global['anio']==2012) & (mapeo_global['tabla']=='TMODULO2')].reset_index(drop=True)
mapeo_2012_tvivienda    = mapeo_global[(mapeo_global['anio']==2012) & (mapeo_global['tabla']=='TVIVIENDA')].reset_index(drop=True)

display(mapeo_2012_tmodulo1)
display(mapeo_2012_tmodulo2)
display(mapeo_2012_tsdem)
display(mapeo_2012_tvivienda)

dic_mapeo_2012_tmodulo1     = dict(zip(mapeo_2012_tmodulo1['mnemonico_original'],   mapeo_2012_tmodulo1['snake_case']))
dic_mapeo_2012_tmodulo2     = dict(zip(mapeo_2012_tmodulo2['mnemonico_original'],   mapeo_2012_tmodulo2['snake_case']))
dic_mapeo_2012_tsdem        = dict(zip(mapeo_2012_tsdem['mnemonico_original'],      mapeo_2012_tsdem['snake_case']))
dic_mapeo_2012_tvivienda    = dict(zip(mapeo_2012_tvivienda['mnemonico_original'],  mapeo_2012_tvivienda['snake_case']))

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

TMODULO1_12 = TMODULO1_12.rename(columns=dic_mapeo_2012_tmodulo1)

TMODULO1_12.sample(n=10,random_state=787)

TMODULO2_12 = TMODULO2_12.rename(columns=dic_mapeo_2012_tmodulo2)

TMODULO2_12.sample(n=10,random_state=787)

TSDEM_12 = TSDEM_12.rename(columns=dic_mapeo_2012_tsdem)

TSDEM_12.sample(n=10,random_state=787)

TVIVIENDA_12 = TVIVIENDA_12.rename(columns=dic_mapeo_2012_tvivienda)

TVIVIENDA_12.sample(n=10,random_state=787)

# 2012, identificamos las llaves primarias y foraneas de la tablas deacuerdo a la figura de entidad relación de ENIF 2021
print(f"TMODULO1: {TMODULO1_12.shape[0]}")
print(f" - control: {TMODULO1_12.drop_duplicates(subset='numero_control_vivienda').shape[0]}")
print(f" - vivienda: {TMODULO1_12.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")
print(f" - hogar: {TMODULO1_12.drop_duplicates(subset='numero_hogar_vivienda').shape[0]}")
print(f" - renglon persona: {TMODULO1_12.drop_duplicates(subset='numero_renglon_persona_elegida').shape[0]}")

print(f"TMODULO2: {TMODULO2_12.shape[0]}")
print(f" - control: {TMODULO2_12.drop_duplicates(subset='numero_control_vivienda').shape[0]}")
print(f" - vivienda: {TMODULO2_12.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")
print(f" - hogar: {TMODULO2_12.drop_duplicates(subset='numero_hogar_vivienda').shape[0]}")
print(f" - renglon persona: {TMODULO2_12.drop_duplicates(subset='numero_renglon_persona_elegida').shape[0]}")

print(f"TSDEM: {TSDEM_12.shape[0]}")
print(f" - control: {TSDEM_12.drop_duplicates(subset='numero_control_vivienda').shape[0]}")
print(f" - vivienda: {TSDEM_12.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")
print(f" - hogar: {TSDEM_12.drop_duplicates(subset='numero_hogar_vivienda').shape[0]}")
print(f" - renglon persona: {TSDEM_12.drop_duplicates(subset='numero_renglon_persona').shape[0]}")

print(f"TVIVIENDA: {TVIVIENDA_12.shape[0]}")
print(f" - control: {TVIVIENDA_12.drop_duplicates(subset='numero_control_vivienda').shape[0]}")
print(f" - vivienda: {TVIVIENDA_12.drop_duplicates(subset='numero_vivienda_seleccionada').shape[0]}")

# modulo2 con modulo1
llaves = ['numero_control_vivienda',
          'numero_vivienda_seleccionada',
          'numero_hogar_vivienda',
          'numero_renglon_persona_elegida'
          ]

cols_dup = (TMODULO1_12.columns.intersection(TMODULO2_12.columns).difference(llaves))

enif_2012_t2 = pd.merge(left=TMODULO1_12,
                        right=TMODULO2_12.drop(columns=cols_dup),
                        how='left',
                        on=llaves,
                        validate='one_to_one'#,
                        #indicator=True
                        )

enif_2012_t2

# sdem con modulo2-modulo1
llaves = ['numero_control_vivienda',
          'numero_vivienda_seleccionada',
          'numero_hogar_vivienda',
          'numero_renglon_persona_elegida'
          ]

TSDEM_12 = TSDEM_12.rename(columns={'numero_renglon_persona':'numero_renglon_persona_elegida'})

cols_dup = TSDEM_12.columns.intersection(enif_2012_t2.columns).difference(llaves)

enif_2012_t3 = pd.merge(left=TSDEM_12,
                        right=enif_2012_t2.drop(columns=cols_dup),
                        how='right',
                        on=llaves,
                        )

enif_2012_t3

# vivienda con sdem-modulo2-modulo1
llaves = ['numero_control_vivienda',
          'numero_vivienda_seleccionada'
          ]

cols_dup = TVIVIENDA_12.columns.intersection(enif_2012_t3.columns).difference(llaves)

enif_2012_t4 = pd.merge(left=enif_2012_t3,
                        right=TVIVIENDA_12.drop(columns=cols_dup),
                        how='left',
                        on=llaves
                        )

enif_2012_t4

enif_2012_full = enif_2012_t4
enif_2012_full['anio'] = '2012'
enif_2012_full.columns

del TMODULO1_12, TMODULO2_12, TSDEM_12, TVIVIENDA_12, enif_2012_t2, enif_2012_t3, enif_2012_t4

enif_2012_full.shape, enif_2015_full.shape, enif_2018_full.shape, enif_2021_full.shape, enif_2024_full.shape

# primero se igualan los nombres de las llaves para que no se pierda el identificador unico
#df_enif = pd.concat([enif_2012_full, enif_2015_full, enif_2018_full, enif_2021_full, enif_2024_full], ignore_index=True, axis=0)
#df_enif.columns.unique(), df_enif.shape[1]

#del enif_2012_full, enif_2015_full, enif_2018_full,enif_2021_full, enif_2024_full

mapeo = pd.read_csv(
    ruta_bases + "/inegi_20260906/ENIF_mapeo_cantidad_creditos_2012_2024.csv"
)

# Siete categorías comunes a los cuatro años
tipos = [
    "tarjeta_credito_bancaria",
    "tarjeta_credito_departamental",
    "credito_nomina",
    "credito_personal",
    "credito_automotriz",
    "credito_vivienda",
    "credito_grupal"
]

# Obtener las columnas de cantidad
columnas_cantidad = {}

for anio in [2012, 2015, 2018, 2021]:

    seleccion = mapeo.loc[
        (mapeo["anio"] == anio)
        & (mapeo["tipo_medicion"] == "cantidad")
        & (mapeo["tipo_producto"].isin(tipos))
    ]

    columnas_cantidad[anio] = (
        seleccion.set_index("tipo_producto")
        .loc[tipos, "snake_case_final"]
        .tolist()
    )

columnas_tenencia = {
    2012: [
        "credito_tarjeta_credito_banco",
        "credito_tarjeta_credito_departamental_tienda",
        "credito_nomina",
        "credito_personal",
        "credito_automotriz",
        "credito_hipotecario",
        "credito_grupal_comunal_compartamos"
    ],

    2015: [
        "tiene_tarjeta_credito_bancaria",
        "tiene_tarjeta_credito_departamental",
        "tiene_credito_nomina",
        "tiene_credito_personal",
        "tiene_credito_auto",
        "tiene_credito_hipotecario",
        "tiene_credito_grupal"
    ],

    2018: [
        "tenencia_tarjeta_credito_bancaria_categorias_historicas",
        "tiene_tarjeta_credito_departamental",
        "tiene_credito_nomina",
        "credito_personal",
        "credito_automotriz",
        "tiene_credito_vivienda",
        "tiene_credito_grupal"
    ],

    2021: [
        "credito_contratado_internet_aplicacion_prestadero_doopla_tarjeta_banco",
        "tiene_tarjeta_credito_departamental",
        "tiene_credito_nomina",
        "tiene_credito_personal",
        "tiene_credito_automotriz",
        "tiene_credito_vivienda",
        "tiene_credito_grupal"
    ]
}

def calcular_productos_credito(df, anio):

    df = df.copy()

    cantidades = columnas_cantidad[anio]
    tenencias = columnas_tenencia[anio]

    # Comprobar que existan las columnas
    columnas = cantidades + tenencias

    faltantes = [
        c for c in columnas
        if c not in df.columns
    ]

    if faltantes:
        raise KeyError(
            f"ENIF {anio}: faltan columnas: {faltantes}"
        )

    resultados = pd.DataFrame(index=df.index)

    for tipo, col_cantidad, col_tenencia in zip(
        tipos, cantidades, tenencias
    ):

        cantidad = pd.to_numeric(
            df[col_cantidad],
            errors="coerce"
        )

        tenencia = pd.to_numeric(
            df[col_tenencia],
            errors="coerce"
        )

        # Si tiene el producto, utilizar cantidad positiva
        valor = cantidad.where(
            tenencia.eq(1) & cantidad.gt(0)
        )

        # Si declara que NO tiene el producto, asignar cero
        valor.loc[tenencia.eq(2)] = 0

        resultados[tipo] = valor

    # Sumar solo cuando las siete categorías sean conocidas
    df["numero_productos_credito"] = (
        resultados.sum(
            axis=1,
            min_count=len(tipos)
        )
        .astype("Int64")
    )

    print(f"ENIF {anio}")
    print("Observaciones:", len(df))
    print(
        "Totales calculados:",
        df["numero_productos_credito"].notna().sum()
    )
    print(
        "Totales pendientes:",
        df["numero_productos_credito"].isna().sum()
    )

    return df

enif_2012_full = calcular_productos_credito(
    enif_2012_full, 2012
)

enif_2015_full = calcular_productos_credito(
    enif_2015_full, 2015
)

enif_2018_full = calcular_productos_credito(
    enif_2018_full, 2018
)

enif_2021_full = calcular_productos_credito(
    enif_2021_full, 2021
)

enif_2018_full[
    "numero_productos_credito"
].value_counts(dropna=False).sort_index()

bases = {
    2012: enif_2012_full,
    2015: enif_2015_full,
    2018: enif_2018_full,
    2021: enif_2021_full
}

resumen = []

for anio, df in bases.items():

    y = df["numero_productos_credito"]

    resumen.append({
        "anio": anio,
        "observaciones": len(df),
        "validos": y.notna().sum(),
        "sin_creditos": y.eq(0).sum(),
        "promedio": y.mean(),
        "mediana": y.median(),
        "maximo": y.max()
    })

pd.DataFrame(resumen)

print(enif_2012_full['numero_productos_credito'].describe())

print(enif_2015_full['numero_productos_credito'].describe())

print(enif_2018_full['numero_productos_credito'].describe())

print(enif_2021_full['numero_productos_credito'].describe())

del enif_2024_full

# MacroVariable de variable objetivo
y_ = ['numero_productos_credito']

print(f'2012\n{enif_2012_full[y_].value_counts()}')
print(f'2015\n{enif_2015_full[y_].value_counts()}')
print(f'2018\n{enif_2018_full[y_].value_counts()}')
print(f'2021\n{enif_2021_full[y_].value_counts()}')

print(f'2012\n{enif_2012_full[y_].isna().sum()}\n')
print(f'2015\n{enif_2015_full[y_].isna().sum()}\n')
print(f'2018\n{enif_2018_full[y_].isna().sum()}\n')
print(f'2021\n{enif_2021_full[y_].isna().sum()}\n')

enif_2012_full[y_] = enif_2012_full[y_].fillna(0)
enif_2015_full[y_] = enif_2015_full[y_].fillna(0)
enif_2018_full[y_] = enif_2018_full[y_].fillna(0)
enif_2021_full[y_] = enif_2021_full[y_].fillna(0)

print(f'2012\n{enif_2012_full[y_].value_counts()}')
print(f'2015\n{enif_2015_full[y_].value_counts()}')
print(f'2018\n{enif_2018_full[y_].value_counts()}')
print(f'2021\n{enif_2021_full[y_].value_counts()}')

# <%-!!-%> Chat GPT aquí vas a realizar los modificaciones con el nuevo snake_case  <%-!!-%> #
# Todos los mapeos vienen integrados en esta celda; no necesitas cargar un CSV adicional.
cambios_snake_aprobados = {2012: {'ahorro_cuenta_ahorro': 'tiene_cuenta_ahorro',
        'credito_abrilde_atraso_pago_credito_automotriz': 'atraso_pago_12m_credito_automotriz',
        'credito_abrilde_atraso_pago_credito_grupal_comunal_compartamos': 'atraso_pago_12m_credito_grupal',
        'credito_abrilde_atraso_pago_credito_hipotecario': 'atraso_pago_12m_credito_hipotecario',
        'credito_abrilde_atraso_pago_credito_nomina': 'atraso_pago_12m_credito_nomina',
        'credito_abrilde_atraso_pago_credito_personal': 'atraso_pago_12m_credito_personal',
        'credito_abrilde_atraso_pago_tarjeta_credito_banco': 'atraso_pago_12m_tarjeta_credito_bancaria',
        'credito_abrilde_atraso_pago_tarjeta_credito_departamental_tienda': 'atraso_pago_12m_tarjeta_credito_departamental',
        'credito_automotriz': 'tiene_credito_automotriz',
        'credito_grupal_comunal_compartamos': 'tiene_credito_grupal',
        'credito_hipotecario': 'tiene_credito_hipotecario',
        'credito_nomina': 'tiene_credito_nomina',
        'credito_personal': 'tiene_credito_personal',
        'credito_tarjeta_credito_banco': 'tiene_tarjeta_credito_bancaria',
        'credito_tarjeta_credito_departamental_tienda': 'tiene_tarjeta_credito_departamental',
        'credito_uso_tarjeta_credito_banco_departamental_hacer_compras_establecimientos_comerciales': 'usa_tarjeta_credito_para_compras',
        'credito_uso_tarjeta_credito_banco_departamental_pagar_gasto_negocio': 'usa_tarjeta_credito_para_gasto_negocio',
        'credito_uso_tarjeta_credito_banco_departamental_pagar_servicios_agua_luz': 'usa_tarjeta_credito_para_pago_servicios',
        'cuenta_cheques': 'tiene_cuenta_cheques',
        'cuenta_sabe_derecho_cambiar_cuenta_nomina_banco_prefiera_gratuita': 'conoce_derecho_portabilidad_nomina',
        'finanzas_deposito_plazo_fijo': 'tiene_deposito_plazo_fijo',
        'finanzas_fondo_inversion': 'tiene_fondo_inversion',
        'numero_credito_automotriz': 'cantidad_creditos_automotrices',
        'numero_credito_grupal_comunal_compartamos': 'cantidad_creditos_grupales',
        'numero_credito_hipotecario': 'cantidad_creditos_hipotecarios',
        'numero_credito_nomina': 'cantidad_creditos_nomina',
        'numero_credito_personal': 'cantidad_creditos_personales',
        'numero_cuenta_ahorro': 'cantidad_ahorro',
        'numero_cuenta_cheques': 'cantidad_cheques',
        'numero_cuenta_nomina_depositan_sueldo_pension': 'cantidad_cuentas_nomina_pension',
        'numero_deposito_plazo_fijo': 'cantidad_depositos_plazo_fijo',
        'numero_fondo_inversion': 'cantidad_fondos_inversion',
        'numero_otros_creditos': 'cantidad_otros_creditos',
        'numero_tarjeta_credito_banco': 'cantidad_tarjetas_credito_bancarias',
        'numero_tarjeta_credito_departamental_tienda_autoservicio': 'cantidad_tarjetas_credito_departamentales',
        'retiro_cuenta_nomina_depositan_sueldo': 'tiene_cuenta_nomina_o_pension',
        'retiro_uso_tarjeta_credito_banco_departamental_hacer_retiros_cajeros_automaticos': 'usa_tarjeta_credito_para_retiro_efectivo'},
 2015: {'ahorro_cuantas_os_cuenta_ahorro': 'cantidad_ahorro',
        'ahorro_cuenta_ahorro': 'tiene_cuenta_ahorro',
        'atrasar_pago_credito_auto': 'atraso_pago_12m_credito_automotriz',
        'atrasar_pago_credito_bancaria': 'atraso_pago_12m_tarjeta_credito_bancaria',
        'atrasar_pago_credito_departamental': 'atraso_pago_12m_tarjeta_credito_departamental',
        'atrasar_pago_credito_grupal': 'atraso_pago_12m_credito_grupal',
        'atrasar_pago_credito_hipotecario': 'atraso_pago_12m_credito_hipotecario',
        'atrasar_pago_credito_nomina': 'atraso_pago_12m_credito_nomina',
        'atrasar_pago_credito_personal': 'atraso_pago_12m_credito_personal',
        'cantidad_credito_auto': 'cantidad_creditos_automotrices',
        'cantidad_credito_grupal': 'cantidad_creditos_grupales',
        'cantidad_credito_hipotecario': 'cantidad_creditos_hipotecarios',
        'cantidad_credito_nomina': 'cantidad_creditos_nomina',
        'cantidad_credito_otro_abierto': 'cantidad_otros_creditos',
        'cantidad_credito_personal': 'cantidad_creditos_personales',
        'cantidad_tarjeta_credito_bancaria': 'cantidad_tarjetas_credito_bancarias',
        'cantidad_tarjeta_credito_departamental': 'cantidad_tarjetas_credito_departamentales',
        'cuenta_cheques': 'tiene_cuenta_cheques',
        'cuenta_cuantas_os_cuenta_cheques': 'cantidad_cheques',
        'finanzas_cuantas_os_depositos_plazo_fijo': 'cantidad_depositos_plazo_fijo',
        'finanzas_cuantas_os_fondos_inversion': 'cantidad_fondos_inversion',
        'retiro_cuantas_os_cuenta_nomina_pension': 'cantidad_cuentas_nomina_pension',
        'retiro_cuenta_nomina_pension_depositan': 'tiene_cuenta_nomina_o_pension',
        'sabe_puede_cambiar_cuenta_tarjeta_nomina_banco_prefiera_forma_gratuita': 'conoce_derecho_portabilidad_nomina',
        'tiempo_resolucion_queja_judicial': 'tiempo_resolucion_queja_autoridad_judicial',
        'tiene_credito_auto': 'tiene_credito_automotriz',
        'tiene_cuenta_deposito_plazo_fijo': 'tiene_deposito_plazo_fijo',
        'tiene_cuenta_fondo_inversion': 'tiene_fondo_inversion',
        'usa_tarjeta_credito_compras': 'usa_tarjeta_credito_para_compras',
        'usa_tarjeta_credito_negocio': 'usa_tarjeta_credito_para_gasto_negocio',
        'usa_tarjeta_credito_retiro_efectivo': 'usa_tarjeta_credito_para_retiro_efectivo',
        'usa_tarjeta_credito_servicios': 'usa_tarjeta_credito_para_pago_servicios'},
 2018: {'atraso_pago_ultimos_12_meses_credito_automotriz': 'atraso_pago_12m_credito_automotriz',
        'atraso_pago_ultimos_12_meses_credito_grupal': 'atraso_pago_12m_credito_grupal',
        'atraso_pago_ultimos_12_meses_credito_nomina': 'atraso_pago_12m_credito_nomina',
        'atraso_pago_ultimos_12_meses_credito_personal': 'atraso_pago_12m_credito_personal',
        'atraso_pago_ultimos_12_meses_tarjeta_credito_departamental': 'atraso_pago_12m_tarjeta_credito_departamental',
        'credito_atraso_pago_credito_vivienda_infonavit_fovissste_banco_otra_institucion': 'atraso_pago_12m_credito_vivienda',
        'credito_atraso_pago_tarjeta_credito_banco': 'atraso_pago_12m_tarjeta_credito_bancaria',
        'credito_automotriz': 'tiene_credito_automotriz',
        'credito_cuantas_tarjetas_credito_bancarias': 'cantidad_tarjetas_credito_bancarias',
        'credito_cuantas_tarjetas_credito_departamental_tienda_autoservicio': 'cantidad_tarjetas_credito_departamentales',
        'credito_cuantos_creditos_grupales_comunales_solidarios_compartamos': 'cantidad_creditos_grupales',
        'credito_cuantos_creditos_vivienda_infonavit_fovissste_banco_otra_institucion': 'cantidad_creditos_vivienda',
        'credito_personal': 'tiene_credito_personal',
        'cuenta_cuantas_cuenta_tarjetas_nomina_depositan_sueldo': 'cantidad_cuentas_nomina',
        'cuenta_cuantas_cuenta_tarjetas_recibir_apoyos_gobierno': 'cantidad_cuentas_apoyos_gobierno',
        'finanzas_cuantos_depositos_plazo_fijo_puede_retirar_determinadas_fechas': 'cantidad_depositos_plazo_fijo',
        'finanzas_cuantos_fondos_inversion_acciones_casa_bolsa': 'cantidad_fondos_inversion',
        'frecuencia_atraso_credito_grupal': 'frecuencia_atraso_12m_credito_grupal',
        'frecuencia_atraso_credito_nomina': 'frecuencia_atraso_12m_credito_nomina',
        'frecuencia_atraso_credito_personal': 'frecuencia_atraso_12m_credito_personal',
        'frecuencia_atraso_credito_vivienda': 'frecuencia_atraso_12m_credito_vivienda',
        'frecuencia_atraso_otro_credito': 'frecuencia_atraso_12m_credito_automotriz',
        'frecuencia_atraso_tarjeta_bancaria': 'frecuencia_atraso_12m_tarjeta_credito_bancaria',
        'frecuencia_atraso_tarjeta_departamental': 'frecuencia_atraso_12m_tarjeta_credito_departamental',
        'numero_cuantas_cuenta_ahorro': 'cantidad_ahorro',
        'numero_cuantas_cuenta_cheques': 'cantidad_cheques',
        'numero_cuantos_creditos_automotrices': 'cantidad_creditos_automotrices',
        'numero_cuantos_creditos_nomina': 'cantidad_creditos_nomina',
        'numero_cuantos_creditos_personales': 'cantidad_creditos_personales',
        'retiro_cuantas_cuenta_tarjetas_pension_depositan_pension': 'cantidad_cuentas_pension',
        'sabe_puede_cambiar_cuenta_tarjeta_nomina_banco_prefiera_forma_gratuita': 'conoce_derecho_portabilidad_nomina',
        'tenencia_tarjeta_credito_bancaria_categorias_historicas': 'tiene_tarjeta_credito_bancaria',
        'tiene_cuenta_deposito_plazo_fijo': 'tiene_deposito_plazo_fijo',
        'tiene_cuenta_fondo_inversion': 'tiene_fondo_inversion'},
 2021: {'ahorro_cuenta_ahorro_tarjeta_debito_tarjeta_plastico_puede_retirar_dinero': 'tarjeta_debito_asociada_ahorro',
        'atraso_pago_ultimos_12_meses_credito_automotriz': 'atraso_pago_12m_credito_automotriz',
        'atraso_pago_ultimos_12_meses_credito_grupal': 'atraso_pago_12m_credito_grupal',
        'atraso_pago_ultimos_12_meses_credito_nomina': 'atraso_pago_12m_credito_nomina',
        'atraso_pago_ultimos_12_meses_credito_personal': 'atraso_pago_12m_credito_personal',
        'atraso_pago_ultimos_12_meses_credito_vivienda': 'atraso_pago_12m_credito_vivienda',
        'atraso_pago_ultimos_12_meses_tarjeta_credito_departamental': 'atraso_pago_12m_tarjeta_credito_departamental',
        'credito_atraso_pago_credito_automotriz': 'frecuencia_atraso_12m_credito_automotriz',
        'credito_atraso_pago_credito_grupal_comunal_solidario_compartamos': 'frecuencia_atraso_12m_credito_grupal',
        'credito_atraso_pago_credito_nomina': 'frecuencia_atraso_12m_credito_nomina',
        'credito_atraso_pago_tarjeta_credito_banco': 'atraso_pago_12m_tarjeta_credito_bancaria',
        'credito_contratado_internet_aplicacion_prestadero_doopla_tarjeta_banco': 'tiene_tarjeta_credito_bancaria',
        'credito_cuantas_os_credito_grupal_comunal_solidario_compartamos': 'cantidad_creditos_grupales',
        'credito_cuantas_os_credito_vivienda_infonavit_fovissste_banco_otra_institucion': 'cantidad_creditos_vivienda',
        'credito_cuantas_os_tarjeta_credito_banco': 'cantidad_tarjetas_credito_bancarias',
        'credito_cuantas_os_tarjeta_credito_departamental_tienda_autoservicio': 'cantidad_tarjetas_credito_departamentales',
        'cuenta_cheques_tarjeta_debito_tarjeta_plastico_puede_retirar_dinero': 'tarjeta_debito_asociada_cheques',
        'cuenta_contrato_ultima_cuenta': 'canal_contratacion_ultima_cuenta',
        'cuenta_cuantas_cuenta_tarjetas_nomina_depositan_sueldo': 'cantidad_cuentas_nomina',
        'cuenta_cuantas_cuenta_tarjetas_recibir_apoyos_gobierno': 'cantidad_cuentas_apoyos_gobierno',
        'cuenta_tarjeta_nomina_debito_tarjeta_plastico_puede_retirar_dinero': 'tarjeta_debito_asociada_nomina',
        'cuenta_tarjeta_recibir_debito_tarjeta_plastico_puede_retirar_dinero': 'tarjeta_debito_asociada_apoyos_gobierno',
        'finanzas_cuantos_depositos_plazo_fijo_puede_retirar_determinadas_fechas': 'cantidad_depositos_plazo_fijo',
        'finanzas_cuantos_fondos_inversion_acciones_casa_bolsa': 'cantidad_fondos_inversion',
        'frecuencia_atraso_credito_personal': 'frecuencia_atraso_12m_credito_personal',
        'frecuencia_atraso_credito_vivienda': 'frecuencia_atraso_12m_credito_vivienda',
        'frecuencia_atraso_tarjeta_credito_bancaria': 'frecuencia_atraso_12m_tarjeta_credito_bancaria',
        'frecuencia_atraso_tarjeta_departamental': 'frecuencia_atraso_12m_tarjeta_credito_departamental',
        'numero_cuantas_cuenta_ahorro': 'cantidad_ahorro',
        'numero_cuantas_cuenta_cheques': 'cantidad_cheques',
        'numero_cuantas_os_credito_automotriz': 'cantidad_creditos_automotrices',
        'numero_cuantas_os_credito_nomina': 'cantidad_creditos_nomina',
        'numero_cuantas_os_credito_personal': 'cantidad_creditos_personales',
        'retiro_cuantas_cuenta_tarjetas_pension_depositan_pension': 'cantidad_cuentas_pension',
        'retiro_cuenta_tarjeta_pension_debito_tarjeta_plastico_puede_retirar_dinero': 'tarjeta_debito_asociada_pension',
        'sabe_puede_cambiar_cuenta_tarjeta_nomina_banco_prefiera_forma_gratuita': 'conoce_derecho_portabilidad_nomina',
        'tiene_cuenta_deposito_plazo_fijo': 'tiene_deposito_plazo_fijo',
        'tiene_cuenta_fondo_inversion': 'tiene_fondo_inversion'},
 2024: {'atraso_pago_ultimos_12_meses_credito_automotriz': 'atraso_pago_12m_credito_automotriz',
        'atraso_pago_ultimos_12_meses_credito_grupal': 'atraso_pago_12m_credito_grupal',
        'atraso_pago_ultimos_12_meses_credito_nomina': 'atraso_pago_12m_credito_nomina',
        'atraso_pago_ultimos_12_meses_credito_personal': 'atraso_pago_12m_credito_personal',
        'atraso_pago_ultimos_12_meses_credito_vivienda': 'atraso_pago_12m_credito_vivienda',
        'atraso_pago_ultimos_12_meses_tarjeta_credito_bancaria': 'atraso_pago_12m_tarjeta_credito_bancaria',
        'atraso_pago_ultimos_12_meses_tarjeta_credito_departamental': 'atraso_pago_12m_tarjeta_credito_departamental',
        'frecuencia_atraso_pago_ultimos_12_meses_credito_automotriz': 'frecuencia_atraso_12m_credito_automotriz',
        'frecuencia_atraso_pago_ultimos_12_meses_credito_grupal': 'frecuencia_atraso_12m_credito_grupal',
        'frecuencia_atraso_pago_ultimos_12_meses_credito_nomina': 'frecuencia_atraso_12m_credito_nomina',
        'frecuencia_atraso_pago_ultimos_12_meses_credito_personal': 'frecuencia_atraso_12m_credito_personal',
        'frecuencia_atraso_pago_ultimos_12_meses_credito_vivienda': 'frecuencia_atraso_12m_credito_vivienda',
        'frecuencia_atraso_pago_ultimos_12_meses_tarjeta_credito_bancaria': 'frecuencia_atraso_12m_tarjeta_credito_bancaria',
        'frecuencia_atraso_pago_ultimos_12_meses_tarjeta_credito_departamental': 'frecuencia_atraso_12m_tarjeta_credito_departamental',
        'sabe_puede_cambiar_cuenta_tarjeta_nomina_banco_prefiera_forma_gratuita': 'conoce_derecho_portabilidad_nomina',
        'tiene_cuenta_deposito_plazo_fijo': 'tiene_deposito_plazo_fijo',
        'tiene_cuenta_fondo_inversion': 'tiene_fondo_inversion'}}

llaves_enif_protegidas = {
    'folio', 'numero_control_vivienda', 'upm_identificador',
    'numero_vivienda_seleccionada', 'numero_hogar_vivienda',
    'numero_renglon_persona', 'numero_renglon_persona_elegida',
    'numero_renglon_informante', 'id_vivienda', 'id_hogar', 'id_persona_hogar',
}

def aplicar_snake_case_aprobado(df, anio):
    """Renombra columnas presentes, sin cambiar llaves, datos, filas o número de columnas."""
    if not isinstance(df, pd.DataFrame):
        raise TypeError(f"ENIF {anio}: se esperaba un DataFrame")
    if not df.columns.is_unique:
        raise ValueError(f"ENIF {anio}: hay columnas duplicadas ANTES del renombrado")
    renombres = cambios_snake_aprobados[anio]
    cambios_presentes = {viejo: nuevo for viejo, nuevo in renombres.items()
                         if viejo in df.columns and viejo != nuevo}
    llaves_afectadas = set(cambios_presentes) & llaves_enif_protegidas
    if llaves_afectadas:
        raise ValueError(f"Intento de renombrar llaves protegidas: {llaves_afectadas}")
    nuevas_columnas = [cambios_presentes.get(c, c) for c in df.columns]
    if len(nuevas_columnas) != len(set(nuevas_columnas)):
        repetidas = sorted({c for c in nuevas_columnas if nuevas_columnas.count(c) > 1})
        raise ValueError(f"ENIF {anio}: el renombrado generaría columnas duplicadas: {repetidas}")
    resultado = df.rename(columns=cambios_presentes)
    assert resultado.shape == df.shape, 'Cambiaron las dimensiones del DataFrame'
    assert all(k in resultado.columns for k in llaves_enif_protegidas if k in df.columns)
    print(f"ENIF {anio}: {len(cambios_presentes)} columnas renombradas de {len(renombres)} propuestas; "
          f"{len(df)} registros sin modificación")
    ausentes = sorted(set(renombres).difference(df.columns))
    if ausentes:
        print(f"  Nota: {len(ausentes)} nombres anteriores no están en este DataFrame "
              '(pueden haberse eliminado como duplicados durante los merges, o estar ya renombrados).')
    return resultado

# Aplicación a los cuatro DataFrames existentes:
enif_2012_full = aplicar_snake_case_aprobado(enif_2012_full, 2012)
enif_2015_full = aplicar_snake_case_aprobado(enif_2015_full, 2015)
enif_2018_full = aplicar_snake_case_aprobado(enif_2018_full, 2018)
enif_2021_full = aplicar_snake_case_aprobado(enif_2021_full, 2021)

# ENIF 2024: configuración preparada, pero llamada COMENTADA porque eliminaste enif_2024_full.
# Cuando vuelvas a cargarlo, elimina solamente el # de la línea siguiente:
# enif_2024_full = aplicar_snake_case_aprobado(enif_2024_full, 2024)

print('Homologación de nombres terminada: los valores originales se conservan.')


# primero se igualan los nombres de las llaves para que no se pierda el identificador unico
#df = pd.concat([enif_2012_full, enif_2015_full, enif_2018_full, enif_2021_full, enif_2024_full], ignore_index=True, axis=0)
df = pd.concat([enif_2012_full, enif_2015_full, enif_2018_full, enif_2021_full], ignore_index=True, axis=0)
print(f'{df.shape}\n {df.columns.unique()}')

del enif_2012_full, enif_2015_full, enif_2018_full, enif_2021_full

# primero se genera la llave única por encuesta y se eliminan las demás variables de identificación de unicidad
df.sample(n=10, random_state=787)

# se genera llave única con reset_index
df = df.copy()

df = df.reset_index(drop=False)

# verificamos que sean únicos
print(f'Valores únicos index: {df['index'].drop_duplicates().shape[0]}\nValores únicos dataframe: {df.shape[0]}')

um_ = ['index']

# identificamos las columnas que solo son llaves y no son informacion real
print(llaves_enif_protegidas)

todas_ = df.columns.to_list()

def obtener_llaves(obj):
    """
    Permite recibir las llaves protegidas como
    lista, conjunto o diccionario por año.
    """

    if isinstance(obj, dict):
        resultado = set()

        for valor in obj.values():
            resultado.update(obtener_llaves(valor))

        return resultado

    if isinstance(obj, (list, tuple, set, pd.Index)):
        return set(obj)

    return {obj}


excluidas = obtener_llaves(llaves_enif_protegidas)

excluidas.update({
    "index",
    "numero_productos_credito"
})

def contiene(nombre, *palabras):

    return any(
        re.search(
            rf"(^|_){re.escape(palabra)}(_|$)",
            nombre
        )
        for palabra in palabras
    )

def clasificar_variable(nombre):

    nombre = nombre.lower()

    # Variables previamente clasificadas
    if nombre in excluidas:
        return "excluida"

    # Identificadores adicionales
    if (
        nombre.startswith("id_")
        or contiene(
            nombre,
            "folio",
            "identificador",
            "control",
            "upm",
            "renglon",
            "consecutivo",
            "clave"
        )
    ):
        return "identificador"

    # Sexo
    if contiene(nombre, "sexo", "genero"):
        return "sexo"

    # Rangos de edad: son categorías
    if (
        "rango_edad" in nombre
        or "grupo_edad" in nombre
        or "categoria_edad" in nombre
    ):
        return "categorica"

    # Edad en años
    if contiene(nombre, "edad", "edades"):
        return "edad"

    # Fechas, diferentes de tiempo transcurrido
    if contiene(nombre, "fecha"):
        return "fecha"

    # Variables de ponderación
    if (
        "factor_expansion" in nombre
        or "ponderador" in nombre
    ):
        return "ponderador"

    # Tiempo transcurrido
    if contiene(
        nombre,
        "tiempo",
        "duracion",
        "minutos",
        "horas",
        "dias",
        "meses",
        "anios",
        "anos",
        "plazo",
        "antiguedad",
        "transcurso"
    ):
        return "tiempo"

    # Distancias
    if contiene(
        nombre,
        "distancia",
        "kilometros",
        "metros",
        "km"
    ):
        return "distancia"

    # Variables cuya semántica no permite
    # inferir con seguridad una cantidad
    if nombre == "numero_servicio_internet_fijo":
        return "revision"

    # Cantidades
    if (
        nombre.startswith((
            "numero_",
            "num_",
            "cantidad_",
            "total_",
            "cuantos_",
            "cuantas_"
        ))
        or contiene(nombre, "cantidad", "conteo", "cuantos")
        or "cuantas_viven" in nombre
    ):
        return "cantidad"

    # Tener o no tener / Sí o No
    if nombre.startswith((
        "tiene_",
        "tuvo_",
        "posee_",
        "dispone_",
        "cuenta_con_",
        "es_",
        "esta_",
        "utiliza_",
        "usa_",
        "conoce_",
        "sabe_",
        "recibio_",
        "solicito_"
    )):
        return "binaria"

    # Variables monetarias o continuas
    if contiene(
        nombre,
        "monto",
        "importe",
        "ingreso",
        "gasto",
        "saldo",
        "tasa",
        "porcentaje",
        "precio",
        "valor"
    ):
        return "continua"

    # Variables categóricas
    if contiene(
        nombre,
        "categoria",
        "clasificacion",
        "tipo",
        "clase",
        "nivel",
        "grupo",
        "estado",
        "entidad",
        "municipio",
        "localidad",
        "pais",
        "region",
        "lugar",
        "ubicacion",
        "institucion",
        "banco",
        "sector",
        "ocupacion",
        "escolaridad",
        "parentesco",
        "motivo",
        "razon",
        "frecuencia",
        "estrato",
        "tamano",
        "aprobo",
        "compro",
        "otro",
        "anio",
        "uso",
        "leccion",
        "negacion",
        "favor_digame",
        "credito_ocupo",
        "credito_no_",
        "credito_negaron_",
        "queja",
        "_tiene_",
        "covid",
        "fuente",
        "ha_",
        "reclamado",
        "emergencia",
        "urgencia",
        "atraso",
        "no_vende_",
        "pidio_dinero",
        "no_tiene"
    ):
        return "categorica"

    # Cuando el nombre no es suficiente
    return "revision"

tabla_clasificacion = pd.DataFrame({
    "variable": todas_
})

tabla_clasificacion["categoria"] = (
    tabla_clasificacion["variable"]
    .apply(clasificar_variable)
)

print(tabla_clasificacion)

print(tabla_clasificacion[tabla_clasificacion['categoria']=='revision'])

# tabal que se uso para revisar
#tabla_clasificacion[tabla_clasificacion['categoria']=='revision'].to_excel(ruta_bases+'/tabla_clasficiacion.xlsx', index=False)

tabla_clasificacion['categoria'] = tabla_clasificacion['categoria'].replace('revision','categorica')

print(tabla_clasificacion[tabla_clasificacion['categoria']=='revision'])

print(tabla_clasificacion)

categorias = tabla_clasificacion['categoria'].drop_duplicates()

print(categorias)

variables_por_categoria = {
    categoria: grupo["variable"].tolist()

    for categoria, grupo in (
        tabla_clasificacion.groupby("categoria")
    )
}

# cambiamos el nombre de la variable stroke que es la vairable objetivo
#columna_objetivo = y_[0]
#df.rename(columns={columna_objetivo : 'y'}, inplace=True)
df['y'] = df[y_]

#col_unidad_muestral = um_[0]
#df.rename(columns={col_unidad_muestral : 'um'}, inplace=True
df['um'] = df['index']

# Unidad Muestral
um_ = ['um']

# Variable Objetivo
y_ = ['y']

# Variables eliminadas
dilit_ = (variables_por_categoria.get("identificador", [])
    + variables_por_categoria.get("excluida", [])
    + variables_por_categoria.get("ponderador", [])
)

# Variables categóricas
c_ = (
    variables_por_categoria.get("sexo", [])
    + variables_por_categoria.get("binaria", [])
    + variables_por_categoria.get("categorica", [])
)

# Variables numéricas
v_ = (
    variables_por_categoria.get("edad", [])
    + variables_por_categoria.get("tiempo", [])
    + variables_por_categoria.get("distancia", [])
    + variables_por_categoria.get("cantidad", [])
    + variables_por_categoria.get("continua", [])
)



print(f"{df.shape[1]}, {len(dilit_)+len(c_)+len(v_)}")

df = df.drop(columns=dilit_)

# Aplica la conversión a lo largo de las columnas especificadas en la lista v_
df[v_] = df[v_].apply(pd.to_numeric, errors='coerce').astype('Int64')

df[c_] = df[c_].astype('str')

df.rename(columns={c:'v_'+c for c in v_}, inplace=True)
df.rename(columns={c:'c_'+c for c in c_}, inplace=True)

df.sample(n=10, random_state=787)

c_ = [c for c in  df.columns.tolist() if c.startswith('c_') ]
v_ = [c for c in  df.columns.tolist() if c.startswith('v_') ]

c_

v_

# distribucion de respuesta
df[y_].value_counts()

df[y_].value_counts()/len(df)

df['v_cantidad_productos_credito'] = df[y_]
#v_.append('cantidad_prouctos_credito') # esto es un error porque tendría una variable descriptiva que incluye la respuesta

df[y_] = np.where(df[y_] == 0, 0, 1)

df[y_].value_counts()

def calculo_iv(df , v, tgt, um):
    #v = 'n_week_day_2'
    aux = df.pivot_table( index = v , columns=tgt, values=um[0], aggfunc='count', fill_value=0 )
    aux[ list(range(2)) ] = aux/aux.apply(np.sum)
    aux['w'] = np.log( aux[0] / aux[1] )      ### AQUÍ ESTÁ LA FORMULA DEL WOE #####
    aux['iv'] = (aux[0] - aux[1])*aux['w']

    return v, aux['iv'].sum()

def clasificacion_woe(df , v, tgt, um):
    #v = 'n_week_day_2'
    aux = df.pivot_table( index = v ,
                          columns=tgt,
                         values=um[0],
                         aggfunc='count',
                         fill_value=0 )

    aux[ list(range(2)) ] = aux/aux.apply(np.sum)

    aux['w'] = np.log( aux[0] / aux[1] ) ### AQUÍ ESTÁ LA FORMULA DEL WOE #####

    aux.drop(range(2),axis=1,inplace=True)

    aux = aux.to_dict()['w']

    return v, aux

def freq(df, var):
    if type(var) != list:
        var = [var]
    for v in var:
        #v = 'state'
        aux = df[v].value_counts().to_frame().rename(columns={'count':'FA'})
        aux['FR'] = aux['FA'] / aux['FA'].sum()
        aux[['FAA','FRA']] = aux.apply(  np.cumsum )
        print(f"La variable: {v}")
        display(aux)
        print("\n")

def normalizar(df, v, umbral):
    # 1. Validar que la columna exista y tenga datos
    if v not in df.columns or df[v].dropna().empty:
        print(f"Advertencia: La columna '{v}' está vacía o no existe.")
        return df
    
    # 2. Calcular proporciones
    aux = df[v].value_counts(normalize=True).to_frame(name='proportion')
    
    # 3. Asignar 'CAT_PEQUE' si es menor al umbral, de lo contrario mantener el índice
    aux[f"n_{v}"] = np.where(aux['proportion'] < umbral, 'CAT_PEQUE', aux.index)
    
    # 4. Obtener la moda (la categoría con mayor proporción) de forma segura
    if not aux.empty:
        moda = aux.iloc[0][f'n_{v}']
        
        # Si la suma de todas las 'CAT_PEQUE' es menor al umbral, reasignarlas a la moda
        mask_peque = aux[f'n_{v}'] == 'CAT_PEQUE'
        if aux.loc[mask_peque, 'proportion'].sum() < umbral:
            aux.loc[mask_peque, f'n_{v}'] = moda
            
    # 5. Crear un diccionario de mapeo rápido y seguro (Evita merges que alteren filas)
    mapeo = pd.Series(aux[f'n_{v}'].values, index=aux.index).to_dict()
    
    # 6. Aplicar el mapeo a la columna original o crear la nueva columna n_v
    df[f'n_{v}'] = df[v].map(mapeo)
    
    return df

def discretizar(df_aux, v, k):

    kb = KBinsDiscretizer(n_bins=k, encode='ordinal', strategy='quantile', subsample=None)
    df_aux = df_aux.copy()

    # Asegurar que no haya valores NaN antes de discretizar
    df_aux[v] = pd.to_numeric(df_aux[v], errors='coerce')
    df_aux = df_aux.dropna(subset=[v])

    # Ajustar el discretizador
    kb.fit(df_aux[[v]])

    # Obtener los intervalos como strings usando pd.cut()
    bins = kb.bin_edges_[0]  # Extraer los límites de los bins
    df_aux[f'd_{v}_{k}'] = pd.cut(df_aux[v], bins=bins, include_lowest=True).astype(str)

    return df_aux

def variables_correlacionadas(dataframe, columnas_num, umbral):
    matriz_corr = dataframe[columnas_num].corr().abs()

    mask = matriz_corr < 1.0
    
    columnas_con_alta_corr = matriz_corr[mask].max() > umbral
    
    lista_variables = columnas_con_alta_corr[columnas_con_alta_corr].index.tolist()
    
    return lista_variables


def imputar_columnas(df, columnas, estrategia="most_frequent"):

    df = df.copy()

    if isinstance(columnas, str):
        columnas = [columnas]

    columnas = list(dict.fromkeys(columnas))

    # Comprobar si faltan columnas
    faltantes = [
        col for col in columnas
        if col not in df.columns
    ]

    if faltantes:
        raise KeyError(
            f"Estas columnas no existen: {faltantes}"
        )

    # Identificar columnas completamente vacías
    vacias = [
        col for col in columnas
        if df[col].isna().all()
    ]

    # Excluir columnas sin información
    validas = [
        col for col in columnas
        if col not in vacias
    ]

    if not validas:
        print("No hay columnas con información para imputar.")
        return df

    # Preparar imputador
    im = SimpleImputer(
        strategy=estrategia,
        add_indicator=False
    )

    # Aplicar imputación
    datos_imputados = im.fit_transform(
        df[validas]
    )

    # Reconstruir DataFrame
    resultado = pd.DataFrame(
        datos_imputados,
        columns=validas,
        index=df.index
    )

    # Actualizar las columnas
    df[validas] = resultado

    print(f"Columnas imputadas: {len(validas)}")
    print(f"Columnas vacías conservadas: {len(vacias)}")

    return df

for c in c_:
    freq(df, c)

k = 0.03 # lo recomendable es un valor entre 0.03 y 0.05
for c in c_:
    df = normalizar(df, c, k)    

n_ = [c for c in df.columns if c.startswith('n_')]

len(c_), len(n_)

for c in n_:
    freq(df, c)

df[n_].sample(n=10, random_state=787315)

unarias = [ v for v, conteo in zip( n_  , [df[v2].unique().shape[0] for v2 in n_ ] ) if conteo == 1 ]

len(unarias) , unarias

# quitamos las variables unarias
n_ = [ c for c in n_ if c not in unarias ]

len(c_) , len(n_)

df[v_].describe(percentiles=np.arange(0,1.01,0.1))

df[v_].hist(
    color='#008080',
    grid=False,
    figsize=(50, 50)
)

miss = (1 - df[v_].describe().T[['count']] /len(df)) *100

miss

miss[miss['count']>99]

df[v_] = df[v_].replace([np.inf,-np.inf], np.nan)

df[v_]

# asignamos la función Simple Imputer
im = SimpleImputer(strategy='mean')

im.fit(df[v_])

df = imputar_columnas(
    df,
    v_,
    estrategia="mean"
)

df_ks = pd.DataFrame( map( lambda x: ( x , ks_2samp( df[x].dropna() , df[v_][x] ).statistic ) , v_  ) , columns = ['var','ks']   )

# Si el factor ks > 0.1 , la variable esta "rota" ( se alteró la distribución )
df_ks

# Si el factor ks > 0.1 , la variable esta "rota" ( se alteró la distribución )
df_ks[df_ks['ks']>0.1]

df[v_].hist(
    color='#008080',
    grid=False,
    figsize=(50, 50)
)

miss = (1 - df[v_].describe().T[['count']] /len(df)) *100
miss[miss['count']>0.10]

# eliminamos dichas columnas
miss = df[v_].isna().mean().to_frame(name='count').reset_index()
dilit_ = miss[miss['count'] > 0.10]['index'].tolist()
print(dilit_)

v_ = [col for col in v_ if col not in dilit_]

vt = VarianceThreshold(threshold=1.0)

vt.fit(df[v_])

sin_varianza = [ v for v, u in zip(v_ , vt.get_support() ) if not(u) ]

print(f'{len(sin_varianza)}\n{sin_varianza}')

df[sin_varianza].hist(
    color='#008080',
    grid=False,
    figsize=(18, 14)
)

v_ = [i for i in v_ if i not in sin_varianza]

print(f'{len(v_)}\n{v_}')

ext = df[v_].describe( percentiles=[.01,.99] ).T[['1%','99%']].reset_index()

for v, li, ls in ext.values:
    df[f'ol_{v}'] = (( df[v] < li ) | ( df[v] > ls )).astype(int)

df['ext'] = df.filter(like='ol_').max(axis=1)

df.drop(df.filter(like='ol_').columns, axis=1, inplace=True)

df['ext'].value_counts(True)*100

df['ext'].value_counts()

# Selecciono los que NO son extremos o Eliminar extremos
df = df.loc[df['ext'] == 0].reset_index(drop=True).drop(['ext'],axis=1)

df.shape

df[v_].corr()

corr_df = df[v_].corr()
plt.figure(figsize=(15,15))
sns.heatmap( corr_df , annot=True )
plt.show()

# definimos las varibales e interes para analizar a profundidad
lst_var_corrs_interes = variables_correlacionadas(df, v_, umbral=0.4)

# siempre graficar esto con una muestra
sns.pairplot(df[lst_var_corrs_interes].sample(100, random_state=787315),
             plot_kws={'color': '#008080', 'alpha': 0.6},
             diag_kws={'color': '#008080', 'fill': True}
             )

vc = VarClusHi(df=df[v_], feat_list=v_)

vc.varclus()

rs = vc.rsquare
rs = rs.sort_values( by = ['Cluster','RS_Ratio'] ,ascending=[1,1] ).reset_index(drop=True)
rs['id'] = rs.groupby('Cluster').cumcount() + 1
rs

v_ = rs.loc[ rs['id'] == 1  ]['Variable'].tolist()

len(v_), v_

df[v_].hist(
    color='#008080',
    grid=False,
    figsize=(10,10)
)


%%time
for c in v_:
    print("Discretizando columna: ",c)
    for k in range(2,7): ## Los bines se trabajan de manera iterativa aquí van de 2 a 6
        df = discretizar( df, c , k )

df.sample(n=10, random_state=787)

d_ = df.filter(like='d_').columns.tolist()

d_

Xt, Xv, yt, yv = train_test_split( df[d_+c_+n_+um_], df[um_+y_], train_size=0.7 , random_state=787315)

Xt.shape , yt.shape, Xv.shape, yv.shape

Xt = Xt.merge( yt, on=um_ , how='inner'  ).reset_index(drop=True)

Xt

iv =  pd.DataFrame( map( lambda v: calculo_iv( Xt , v , y_ , um_  ) , d_ ) , columns= ['variable','iv']  )

iv

iv['raiz'] = iv['variable'].map( lambda x: '_'.join( x.split('_')[1:-1] ) )

iv

iv = iv.sort_values( by=['raiz','iv'],ascending=[1,0] ).reset_index(drop=True)
iv['id'] = iv.groupby( 'raiz' ).cumcount()+1

iv

mejor_d_ = iv.loc[iv.id == 1]['variable'].tolist()

## Estás son mis mejores variables discretizadas para mi WoE. 
## Tienen el mayor poder predictivo por cada variable raiz. 
len(mejor_d_) , mejor_d_

df[mejor_d_]

ivd =  pd.DataFrame( map( lambda v: calculo_iv( Xt , v , y_ , um_  ) , n_ ) , columns= ['variable','iv']  )

ivd.sort_values('iv',ascending=False).reset_index(drop=True)

Xt[['y','n_c_gender']].value_counts().reset_index()

Xt[['y','n_c_metformin.pioglitazone']].value_counts().reset_index()

Xt[['y','n_c_glimepiride.pioglitazone']].value_counts().reset_index()

Xt[['y','n_c_acetohexamide']].value_counts().reset_index()

mejor_n_ = ivd[ (ivd['iv'] >= 0.02 ) & (ivd['iv'] < 0.9 ) ]['variable'].tolist()

len(mejor_n_), mejor_n_

mejores_ = mejor_d_ + mejor_n_

len(mejores_),mejores_

df[mejores_].head()

mapa_woe = list( map( lambda v: clasificacion_woe( Xt , v , y_ , um_) , mejores_ ) )

mapa_woe

for v, mapa in mapa_woe:
    Xt[f'w_{v}'] = Xt[v].replace(mapa)
    Xv[f'w_{v}'] = Xv[v].replace(mapa)

w_ = Xt.filter(like='w_').columns.tolist()

X = pd.concat([Xt,Xv],ignore_index=True)
y = pd.concat([yt,yv],ignore_index=True)

X.shape, y.shape

tad = X[um_+w_].merge( y , on = um_ , how='inner').reset_index(drop=True)

tad

tad[y_[0]].value_counts(1)*100

tad.drop(columns=um_+y_).dtypes

to_num = [col for col in tad.columns if col not in (um_ + y_)]

tad[to_num] = tad[to_num].astype('float64')

tad.drop(columns=um_+y_).dtypes

tad.to_parquet('Bases/CANEDO_BELTRAN_BRAULIO_Examen.parquet',
               engine = 'fastparquet')

tad[to_num].hist(
    color='#008080',
    grid=False,
    figsize=(10,10)
)

tad