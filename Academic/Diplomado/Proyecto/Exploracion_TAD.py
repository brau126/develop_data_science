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
                        how='left',
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
llaves = ['folio']

cols_dup = TMODULO_21.columns.intersection(TSDEM_21.columns).difference(llaves)

enif_2021_t2 = pd.merge(left=TSDEM_21,
                        right=TMODULO_21.drop(columns=cols_dup),
                        how='left',
                        on=llaves
                        )

enif_2021_t2

# hogar con sdem-modulo
llaves = ['folio']

cols_dup = THOGAR_21.columns.intersection(enif_2021_t2.columns).difference(llaves)

enif_2021_t3 = pd.merge(left=enif_2021_t2,
                        right=THOGAR_21.drop(columns=cols_dup),
                        how='left',
                        on=llaves
                        )

enif_2021_t3

# vivienda con sdem-modulo-hogar
llaves = ['folio']

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
                        how='left',
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
                        how='left',
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
                        how='left',
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
                        how='left',
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

# priemro se igualan los nombres de las llaves para que no se pierda el id_unicoidentificador unico

nom_banxico

nom_cnsf

nom_condusef

nom_ = nom_banxico + nom_condusef + nom_cnsf + nom_inegi
nom_