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

# importar
library(readxl)

# graficas
library(ggplot2)

########################## Resumen y Abstract ##########################


########################## Inroduccion ##########################


########################## Marco Teorico ##########################


########################## Metodologia ##########################
########################## Importacion Datos ##########################
# se importan los datos de un archivo de excel
ruta_raw_data <- "Series_T/Volumenesdeventadeexpendioalpublico.xlsx" # porfavor aquí cada quien colocar la ruta donde está guardando el archivo
raw_data <- read_excel(ruta_raw_data, sheet = "ventas_Regular")

# modificar a un data frame utilizable
df_regular_og <- raw_data
df_regular_og <- df_regular_og[-(72112:72115),]
df_regular_og <- df_regular_og[-(1:5),]
colnames(df_regular_og) <- c("anio","mes","subproducto","entidad","municipio","volumen")
df_regular_og$volumen <- as.numeric(df_regular_og$volumen)

# sumar datos de acuerdo a su mes y años
df_regular_mes <- df_regular_og %>%
  group_by(anio, mes) %>%
  summarise(volumen_mensual = sum(volumen, na.rm = TRUE))

########################## Analisis Empirico ##########################


########################## Conclusiones ##########################


########################## Referencias ##########################


########################## Anexos ##########################


