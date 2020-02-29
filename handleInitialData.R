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
incidenciasPorMillas <- datosYColumnasMejorados %>% group_by(mileage) %>% count()
# Agrupar y contar incidencias por parte
incidenciasPorParte <- datosYColumnasMejorados %>% group_by(part_item) %>% count()
# Agrupar y contar incidencias por version de Sync
incidenciasPorVersionSync <- datosYColumnasMejorados %>% group_by(sync_system) %>% count()
# Agrupar y contar incidencias por concern
incidenciasPorConcern <- datosYColumnasMejorados %>% group_by(concern) %>% count()
# Agrupar y contar incidencias por parte y concern
incidenciasPorParteYConcern <- datosYColumnasMejorados %>% group_by(part_item, concern) %>% count()

