###################################################################################
# Title: Proyección de Volumen de Ventas de Gasolina en México con Series de Tiempo
# Authors:   Canedo Beltran, Braulio
#            Dominguez Ortiz, Mariana
#            Monroy Romero, Iris P
#            _ _, Oswaldo
# Fecha: 
###################################################################################
########################## Librerias ##########################
# datos
library(dplyr)
library(tidyverse)
library(lubridate)

# importar
library(readxl)

# graficas
library(ggplot2)

########################## Resumen y Abstract ##########################


########################## Introduccion ##########################


########################## Marco Teorico ##########################


########################## Metodologia ##########################
########################## Importacion Datos ##########################
# se importan los datos de un archivo de excel
ruta_raw_data <- "Volumenesdeventadeexpendioalpublico.xlsx" # porfavor aquí cada quien colocar la ruta donde está guardando el archivo
raw_data <- read_excel(ruta_raw_data, sheet = "ventas_Regular")

# modificar a un data frame utilizable
df_regular_og <- raw_data
df_regular_og <- df_regular_og[-(72112:72115),]
df_regular_og <- df_regular_og[-(1:5),]
colnames(df_regular_og) <- c("anio","mes","subproducto","entidad","municipio","volumen")
df_regular_og$volumen <- as.numeric(df_regular_og$volumen)

# meses en espaniol
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# coloca columna con el tiempo en forma numerica
df_regular_og <- df_regular_og %>%
  mutate(fecha = as.numeric(anio) + (match(mes, meses) / 12))

# agrupa el volumen mensual sumando todos los municipios y estados
df_regular_mes <- df_regular_og %>%
  group_by(fecha) %>%
  summarise(volumen_mensual = sum(volumen, na.rm = TRUE))

# se crea el objeto tipo time series
ts_regular_mes <- ts(df_regular_mes$volumen_mensual, start = c(2016,1))

########################## Analisis Empirico ##########################


########################## Conclusiones ##########################


########################## Referencias ##########################


########################## Anexos ##########################


