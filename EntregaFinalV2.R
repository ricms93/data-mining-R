#Identificación
#Titulo del programa: Código para evaluación de datos de calidad Ford
#Descripcion: Modelado y Evaluación 
#Autor: Equipo Ford
#Fecha: 21/03/2020
#Versión: 2.4

# Cargar las librerias
# install.package("factoextra")
library(pdftools)
library(wordcloud)
library(tm)
library(readxl)
library(rvest)
library(broom)
library(tidyverse)
library(factoextra)

#Esta leyendo del archivo de excel una hoja determinada
datosiniciales <- read_excel("quejas_clientes_electrico_v2.xlsx",sheet = "Datos iniciales")
partesConId <- read_excel("partes_sistema_electrico_v1.xlsx")
#Observación de datos iniciales
View(datosiniciales)
class(datosiniciales)
#Que me muestre solo la columna Parte y Costo
Partes <- table(datosiniciales$Parte)
Costos <- table(datosiniciales$Costo)
View(Partes)
class(Partes)

# Gráfica de frecuencia de componentes con mas fallas
fqs <- read_excel("frecuencia.xlsx")
View(fqs)
dl_plot1 <- fqs %>%
  ggplot(aes(x = ratio,
             y = componente)) +
  geom_point() + geom_text (aes(label = ratio), check_overlap = TRUE)

dl_plot1 +
  labs(title = "Componentes con alto índice de fallas",
       x = "Indice de falla (%)",
       y = "Componente") +
  theme_minimal()


#Plot 1. Gráfica de los valores iniciales sin procesar
dl_plot <- datosiniciales %>%
  ggplot(aes(x = Mileage,
             y = Costo)) +
  geom_point() + geom_text (aes(label = Parte), check_overlap = TRUE)

dl_plot +
  labs(title = "Tabla 1",
       x = "Millas",
       y = "Costo total de reparación") +
  theme_minimal()


# Modelado

# Clustering Millas vs Costo

# Definir numero optimo de clusters

# Por medio de silhouette
datosInicialesSegmentados <- datosiniciales %>% inner_join(partesConId, "Parte") %>% select(Mileage, Costo)
fviz_nbclust(datosInicialesSegmentados, kmeans, method = "silhouette")
# Por medio de Elbow
fviz_nbclust(datosInicialesSegmentados, kmeans, method = "wss")
# Por medio de Gap Statistic Method
gap_stat <- clusGap(datosInicialesSegmentados, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Agrupar Clusters
fviz_gap_stat(gap_stat)
make_clusters <- datosInicialesSegmentados %>% 
  kmeans (centers = 2)

# Tomo el cluster que acabo de crear y voy a aumentarlo con todos los datos de la tabla limpia 
# Reviso cada uno de los datos con lo que tenia el cluster para ver a que pertenecia
data_aug <- make_clusters %>% augment(datosiniciales)

View(data_aug)

# Graficar la nueva tabla con el cluster agregado
dl_plot_k <- data_aug %>%
  ggplot(aes(x = Mileage,
             y = Costo,
             color = .cluster)) +
  geom_point() +
  scale_color_discrete(name="",
                       breaks=c("1", "2", "3", "4", "5"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))
# geom_text(aes(label = Parte), size = 2)

dl_plot_k +
  labs(title="Cluster",
       x="Millas",
       y="Costo") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, hjust = 1))
