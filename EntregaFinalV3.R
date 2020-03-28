# Identificación
# Título del programa: Código para evaluación de datos de calidad Ford
# Descripción: Script Final para la materia de Análisis y Minería de Datos
# Autor: Equipo Ford
# Fecha: 30/03/2020
# Versión: 2.5

# Instalar librerias
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("readOffice")
# install.package("factoextra")

# Cargar las librerias
library(readOffice)
library(ggplot2)
library(tm)
library(readxl)
library(rvest)
library(broom)
library(tidyverse)
library(factoextra)
library(cluster)

# Parte 1: Análisis inicial de los datos

# Definir nombre del archivo
baseDeDatos <- "quejas_clientes_electrico_v1.xlsx"
# Cargar datos desde Excel
datos <- read_excel(baseDeDatos)

# Observar datos iniciales
View(datos)

# Renombrar nombres de columnas para mejor manipulación
datosYColumnasMejorados <- datos %>% rename_all(funs(str_replace_all(tolower(.), " ", "_")))
datosYColumnasMejorados <- datosYColumnasMejorados %>% rename_all(funs(str_replace_all(., regex("[()]+"), "")))

# Observar datos con nuevos nombres de columnas
View(datosYColumnasMejorados)

# Análisis inicial de datos

# Contar datos

totalDeDatosComoLista <- datosYColumnasMejorados %>% count()

totalDeDatos <- totalDeDatosComoLista$n

cat("El total de datos es => ", totalDeDatos)

# Agrupar y contar incidencias por millas
incidenciasPorMillas <- datosYColumnasMejorados %>% filter(mileage < 1000) %>% group_by(mileage) %>% count()
incidenciasPorMillasMayor1000 <- datosYColumnasMejorados %>% filter(mileage > 1000) %>% group_by(mileage) %>% count()
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

# Graficar incidencias por millas mayores a 1000
ggplot(incidenciasPorMillasMayor1000, aes(x=incidenciasPorMillasMayor1000$mileage, y=incidenciasPorMillasMayor1000$n)) +
  geom_segment( aes(x=incidenciasPorMillasMayor1000$mileage, xend=incidenciasPorMillasMayor1000$mileage, y=0, yend=incidenciasPorMillasMayor1000$n), color="blue") +
  geom_point( color="magenta", size=1, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
  ) + xlab("Millas") + ylab("Número de Incidencias")

# Graficar incidencias por versión de sync
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

# Parte 2: Modelado

# Cargar archivo de excel con una hoja determinada
datosiniciales <- read_excel("quejas_clientes_electrico_v2.xlsx",sheet = "Datos iniciales")
# Cargar archivo de partes
partesConId <- read_excel("partes_sistema_electrico_v1.xlsx")

# Observación de datos iniciales
View(datosiniciales)
# Tipo de datos iniciales
class(datosiniciales)
# Segmentar en tabla con  columnas Parte y Costo
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


# Plot 1. Gráfica de los valores iniciales sin procesar
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

# Crear variable utilitaria y unirla con los identificadores de partes
datosInicialesSegmentados <- datosiniciales %>% inner_join(partesConId, "Parte") %>% select(Mileage, Costo)
# Por medio de silhouette
fviz_nbclust(datosInicialesSegmentados, kmeans, method = "silhouette")
# Por medio de Elbow
fviz_nbclust(datosInicialesSegmentados, kmeans, method = "wss")
# Por medio de Gap Statistic Method
gap_stat <- clusGap(datosInicialesSegmentados, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# Agrupar Clusters

# NOTA: A pesar de que el número de clusters óptimo obtenido
# programaticamente fue de 2, se utilizó 5 para un mejor análisis
make_clusters <- datosInicialesSegmentados %>% 
  kmeans (centers = 5)

# Agregar información de la tabla a los cluster 
data_aug <- make_clusters %>% augment(datosiniciales %>% inner_join(partesConId, "Parte"))

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
  labs(title="Modelo de Cluster",
       x="Millas",
       y="Costo") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, hjust = 1))

# Graficar clusters
fviz_cluster(make_clusters, data = datosInicialesSegmentados, geom = "point") +
labs(title="Modelo de Cluster",
       x="Millas",
       y="Costo")

# Modelo de Regresión

# Declarar variable para lm
datosParaModelo <-  datosInicialesSegmentados
# Aplicar lm
modeloT <- lm(datosParaModelo)
# Imprimir resultado
summary(modeloT)
# Graficar datos
plot(datosParaModelo)



