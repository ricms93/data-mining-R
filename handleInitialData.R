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

data <- as.data.frame(datos)

for (i in data) {
  print(i)
  break
}
