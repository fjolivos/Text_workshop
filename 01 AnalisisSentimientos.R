######################################################
#     Workshop Análisis computational de texto       #
#                                                    #
#                  Francisco Olivos                  #
######################################################


#Contenido
# 1. Análisis de sentimiento
# 2. Modelamiento de tópico


#            Análisis de sentimientos

# Instala los paquetes necesarios que no están en R básico:
install.packages("syuzhet")
install.packages("RColorBrewer")
install.packages("tibble")

#Carga los paquetes
library(syuzhet) # Análisis de sentimientos
library(RColorBrewer) # Colores para gráfico
library(tibble) # Para generar un identificador único

#Reemplazar el url por sus datos
urlfile<-"https://raw.githubusercontent.com/fjolivos/Text_workshop/main/Olivosetal2022.csv"

#Importar la base de datos
reclamos<-read.csv(urlfile)

#La base tiene muchos reclamos y el análisis tarda mucho
#Seleccionemos los primeros 15 reclamos
reclamos10<-reclamos[1:10,]

#Explorar la base de datos
View(reclamos10)

#Explorar algunos reclamos
head(reclamos10$Complain)

#Transformar el reclamo a palabras separadas ("tokens")
texto_palabras10 <- get_tokens(reclamos10$Complain)

#Explorar las palabras
head(texto_palabras10)

#Contar número de palabras
length(texto_palabras10)

#Analisamos los sentimientos en las palabras DE TODO EL TEXTO
#Función get_nrc_sentimientodel paquete syuzhet
#El idioma por defecto es inglés. Así que agregamos español en lang
sentimientos_df10 <- get_nrc_sentiment(texto_palabras10, lang="spanish")

#Exploremos la nueva base de datos
View(sentimientos_df10)

#Revisar match
#Ver por ejemplo 32, 174, 441
Check <- as.data.frame(texto_palabras10)

#Visualización con gráfico de barras de todos los 15 reclamos
barplot(
  colSums(prop.table(sentimientos_df10[, 1:10])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Reclamos contra Carabineros de Chile, 2013-2020",
  sub = "Workshop Análisis de Texto, UCLM",
  xlab="Emociones", ylab = NULL)

#Podemos estimar sentimientos para cada uno de los reclamos
#Creamos dos columnas vacías 
reclamos10$positive <- NA
reclamos10$negative <- NA

#Este código va a hacer el mismo análisis para cada reclamo
#Lo repetirá 15 veces y agregará el promedio de negativo y positivo a la columna vacía
#Primero creamos in ID correlativo
reclamos10<-rowid_to_column(reclamos10, "ID2")

#Definimos un rango de IDs
range <- 1:10

#Pedimos un loop para todos los valores del rango
for(i in range) { 
reclamos<-reclamos10[i,]
texto_palabras10 <- get_tokens(reclamos10$Complain)
sentimientos_df10 <- get_nrc_sentiment(texto_palabras10, lang="spanish")
reclamos10$negative[reclamos10$ID2 == i] <- mean(sentimientos_df10$negative)
reclamos10$positive[reclamos10$ID2 == i] <- mean(sentimientos_df10$positive)
}

#Exploremos la base de datos con las nuevas columnas
View(reclamos10)

#Calcular promedios
mean(reclamos10$negative)
mean(reclamos10$positive)


