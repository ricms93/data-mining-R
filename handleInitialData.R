# Identificación
# Titulo del programa handleInitialData
# Descripción: 
# Autor Ricardo Morillo Segovia
# Fecha 2019-02-27
# Version 0.1

#Instalar librerias
# install.packages("readxl")
# install.packages("tidyverse")
library(readxl)
library(tidyverse)

# install.packages(“readxl”)
# install.packages("readOffice")
library(readOffice)
library(readxl)

# Cargar datos desde Excel
datos <- read_excel("quejas_clientes_electrico_v1.xlsx")

# Observar datos iniciales
View(datos)

table <- table(datos$...6)

