# Identificación
# Titulo del programa handleInitialData
# Descripción: 
# Autores:
# - Ricardo Morillo Segovia
# - 
# Fecha 2019-02-27
# Version 0.1

#Instalar librerias
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("readOffice")
library(readxl)
library(tidyverse)
library(readOffice)
library(ggplot2)

# Definir nombre del archivo
baseDeDatos <- "quejas_clientes_electrico_v1.xlsx"
# Cargar datos desde Excel
datos <- read_excel(baseDeDatos)

# Observar datos iniciales
View(datos)

#Renombrar nombres de columnas para mejor manipulacion
datosYColumnasMejorados <- datos %>% rename_all(funs(str_replace_all(tolower(.), " ", "_")))
datosYColumnasMejorados <- datosYColumnasMejorados %>% rename_all(funs(str_replace_all(., regex("[()]+"), "")))

#Observar datos con nuevos nombres de columnas
View(datosYColumnasMejorados)

#Analisis inicial de datos

# Contar datos

totalDeDatosComoLista <- datosYColumnasMejorados %>% count()

totalDeDatos <- totalDeDatosComoLista$n

cat("El total de datos es => ", totalDeDatos)

# Agrupar y contar incidencias por millas
incidenciasPorMillas <- datosYColumnasMejorados %>% filter(mileage < 1000) %>% group_by(mileage) %>% count()
# Agrupar y contar incidencias por parte
incidenciasPorParte <- datosYColumnasMejorados %>% filter(str_length(part_item) < 20) %>% group_by(part_item) %>% count()
# Agrupar y contar incidencias por version de Sync
incidenciasPorVersionSync <- datosYColumnasMejorados %>% group_by(sync_system) %>% count()
# Agrupar y contar incidencias por concern
incidenciasPorConcern <- datosYColumnasMejorados %>% group_by(concern) %>% count()

# Graficar incidencias por millas menores a 1000
ggplot(incidenciasPorMillas, aes(x=incidenciasPorMillas$mileage, y=incidenciasPorMillas$n)) +
  geom_segment( aes(x=incidenciasPorMillas$mileage, xend=incidenciasPorMillas$mileage, y=0, yend=incidenciasPorMillas$n), color="green") +
  geom_point( color="magenta", size=1, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
  ) + xlab("Millas") + ylab("Número de Incidencias")
# Graficar incidencias por version de sync
barplot(height=incidenciasPorVersionSync$n, names=incidenciasPorVersionSync$sync_system, xlab="Versión de Sync", col = "#b400b6", main = "Incidencias por Versión de Sync")
# Graficar incidencias por concern
ggplot(incidenciasPorConcern, aes(x=incidenciasPorConcern$concern, y=incidenciasPorConcern$n)) +
  geom_segment( aes(x=incidenciasPorConcern$concern, xend=incidenciasPorConcern$concern, y=0, yend=incidenciasPorConcern$n), color="skyblue") +
  geom_point( color="blue", size=1, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
  ) + xlab("Concern") + ylab("Número de Incidencias")

# Graficar incidencias por parte
ggplot(incidenciasPorParte, aes(x=incidenciasPorParte$part_item, y=incidenciasPorParte$n)) +
  geom_segment( aes(x=incidenciasPorParte$part_item, xend=incidenciasPorParte$part_item, y=0, yend=incidenciasPorParte$n), color="cyan") +
  geom_point( color="red", size=1, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
  ) + xlab("Parte") + ylab("Número de Incidencias")
