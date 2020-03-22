#Identificación
#Titulo del programa: Código para evaluación de datos de calidad Ford
#Descripcion: Modelado y Evaluación 
#Autor: Equipo Ford
#Fecha: 20/03/2020
#Versión: 2.3

#Cargar las librerias
library(pdftools)
library(wordcloud)
library(tm)
library(readxl)
library(rvest)
library(broom)
library(tidyverse)

#Esta leyendo del archivo de excel una hoja determinada
datosiniciales <- read_excel("Quejas de clientes_ Electrico_ v1.xlsx",sheet = "Datos iniciales")
#Observación de datos iniciales
View(datosiniciales)
class(datosiniciales)
#Que me muestre solo la columna Parte y Costo
Partes <- table(datosiniciales$Parte)
Costos <- table(datosiniciales$Costo)
view(Partes)
class(Partes)

#Gráfica de frecuencia de componentes con mas fallas
fqs <- read_excel("frecuencia.xlsx")
View(fqs)
dl_plot1 <- fqs %>%
  ggplot(aes(x=ratio,
             y=componente)) +
  geom_point() + geom_text (aes(label=ratio), check_overlap = TRUE)

dl_plot1 +
  labs(title = "Componentes con alto índice de fallas",
       x="Indice de falla (%)",
       y="Componente") +
  theme_minimal()


#Plot 1. Grafica de los valores iniciales sin procesar
dl_plot <- datosiniciales %>%
  ggplot(aes(x=Mileage,
             y=Costo)) +
  geom_point() + geom_text (aes(label=Parte), check_overlap = TRUE)

dl_plot +
  labs(title = "Tabla 1",
       x="Millas",
       y="Costo total de reparación") +
  theme_minimal()


#Modelado
#Agrupar Clusters
make_clusters <- datosiniciales %>% 
  #Se agregan mas variables para el analisis de clustering
  select(Mileage, Costo) %>% 
  kmeans (centers=5)

#Tomo el cluster que acabo de crear y voy a aumentarlo con todos los datos de la tabla limpia 
#Reviso cada uno de los datos con lo que tenia el cluster para ver a que pertenecia
data_aug<-make_clusters %>% augment(datosiniciales)

#Graficar la nueva tabla con el cluster agregado
dl_plot_k <- data_aug %>%
  ggplot(aes(x=Mileage,
             y=Costo,
             color=.cluster)) +
  geom_point() + geom_text (aes(label=Parte), check_overlap = TRUE)

dl_plot_k +
  labs(title="Modelo de Cluster",
       x="Millas",
       y="Costo") +
  theme_minimal()

