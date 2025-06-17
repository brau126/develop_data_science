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

# estadistica
library(moments)

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

# sumar datos de acuerdo a su mes y años
df_regular_og <- df_regular_og %>%
  mutate(fecha = as.numeric(anio) + (match(mes, meses) / 12))

# se genera el data frame y se usan millones para facilitar interpretacion y lectura
df_regular_mes <- df_regular_og %>%
  group_by(fecha) %>%
  summarise(volumen_mensual_mll = sum(volumen, na.rm = TRUE)) / 1000000

# se crea el objeto tipo time series
ts_regular_mes <- ts(df_regular_mes$volumen_mensual_mll, start = c(2016,1), frequency = 12)

# configuracion anti notación cinetifica
options(scipen = 999)

# visualizacion del correlograma
acf(ts_regular_mes, col = "palegreen4")

# prueba box cox extendida para tranformar datos
# Box Cox
bc_conf <- boxcox (ts_regular_mes~1,lambda = seq(-5,5,0.001),plotit=TRUE)
# Lambda óptimo
lambda <- bc_conf$x[which.max(bc_conf$y)]
title(main = "Figure 2: Box-Cox of Yt")
# Agregar nota al pie de página
mtext(side = 1, line = 4, at = -5, adj = 0, "Source: Authors work")
# Mostrar el valor de lambda óptimo
cat("Lambda optima:", lambda)

########################## Analisis Empirico ##########################
# tabla de estadisticos descriptivos
descripcion <- data.frame(
  Media = mean(df_regular_mes$volumen_mensual_mll),
  Mediana = median(df_regular_mes$volumen_mensual_mll),
  Min = min(df_regular_mes$volumen_mensual_mll),
  Max = max(df_regular_mes$volumen_mensual_mll),
  Varianza = var(df_regular_mes$volumen_mensual_mll),
  Sesgo = skewness(df_regular_mes$volumen_mensual_mll),
  Curtosis = kurtosis(df_regular_mes$volumen_mensual_mll)
  )

descripcion

# primera visualizacion de los datos
ggplot(df_regular_mes) +
  geom_line(mapping = aes(x = fecha, y = volumen_mensual_mll), color = "palegreen4") +
  labs(title = "Volumen de Gas Regular Mensual ene 2016 - feb 2025", x = "Fecha", y = "Volumen en Millones") +
  theme_minimal()

# revisamos si con los valores
df_regular_mes$volumen_indice <- df_regular_mes$volumen_mensual / mean(df_regular_mes$volumen_mensual)

# graficamos el tras haber tranformado en indice
ggplot(df_regular_mes) +
  geom_line(mapping = aes(x = fecha, y = volumen_indice), color = "palegreen4") +
  labs(title = "Indice de Volumen ene 2016 - feb 2025", x = "Fecha", y = "Indice del Volumen") +
  theme_minimal()

# observamos los ciclos anuales del volumen
boxplot(ts_regular_mes ~ cycle(ts_regular_mes), main ="Boxplot Volumenes en Millones por Mes", xlab = "Mes Numérico", ylab = "Volumen en Millones", col = "palegreen2")

# se realiza la descomposicion de los componentes de la serie temporal
regular_mes_decomp = decompose(ts_regular_mes, type = "mult")

# se genera la grafica de sus componentes
plot(regular_mes_decomp, col = "palegreen4")

########################## Conclusiones ##########################


########################## Referencias ##########################


########################## Anexos ##########################


