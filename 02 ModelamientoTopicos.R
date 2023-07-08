######################################################
#     Workshop Análisis computational de texto       #
#                                                    #
#                  Francisco Olivos                  #
######################################################

#Contenido
# 1. Análisis de sentimientos
# 2. Modelamiento de tópicos

#            Modelamiento de tópicos

# Instala los paquetes necesarios que no están en R básico:
install.packages("stm")

#Carga los paquetes
library(stm) # Principal paquete para modelamiento de tópicos structurales

#Reemplazar el url por sus datos
urlfile<-"https://raw.githubusercontent.com/fjolivos/Text_workshop/main/Olivosetal2022.csv"

#Importar la base de datos
reclamos<-read.csv(urlfile)

#Limpiamos la columna de reclamos para quedarnos con palabras con "sentido"
#Indicamos que es en español
#Metadata lo veremos en unos minutos
processed <- textProcessor(reclamos$Complain, metadata = reclamos, language = "sp")

#No es perfecto en español y aún mantiene adverbios y conjunciones
#Agregamos palabras adicionales a eliminar
processed <- textProcessor(reclamos$Complain, 
                           metadata = reclamos, stem = FALSE, language = "sp",
                           customstopwords = c("aun","mas", "solo", "reclamo", "asi", "ahi", "ahí", "según", "así", "veces", "aqui", "atras", "vez", "dos",
                                               "luego", "mientras","anterior", "ningun","tambien", "ademas", "segun","despues", "frente"))
#Pre-procesamiento
#Lower and upper elimina las palabras menos y más comunes
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 15, upper.thresh = 400)

docs <- out$documents
vocab <- out$vocab
meta <-out$meta

#Número de tópicos ideal 
#Algunas métricas que pueden asistir
#En K pedimos que evalue identificar de 3 a 30 tópicos
#En prevalence incluimos otras variables de la base de datos que pueden explicar
#prevalencia de tópicos
findingk <- searchK(out$documents, out$vocab, K = c(3:30),
                    prevalence =~ Province + Year + Month + nse4,
                    data = meta, verbose=FALSE) 

#Exploramos las métricas
#Importante: Siempre es la validación de los investigadores lo más importante
plot(findingk)

#Solución escogida
#Solo 50 iteraciones para hacerlo más rápido
police_TM8 <- stm(documents = docs, vocab = vocab,
                  K = 8, prevalence =~ Province + Year + Month,
                  max.em.its = 50, data = meta,
                  init.type = "Spectral", verbose = FALSE)

#Prevalencia de tópicos y palabras representativas
plot(police_TM8)

#Una forma de hacer sentido de los tópicos es ver los más representativos
#Los reclamos con la prevalencia más alta de cada tópico
#De qué hablan?

thougts4<-findThoughts(police_TM8, texts = reclamos$Complain, n = 3, topics = 4)
thougts4

thougts2<-findThoughts(police_TM8, texts = reclamos$Complain, n = 3, topics = 2)
thougts2

thougts7<-findThoughts(police_TM8, texts = reclamos$Complain, n = 3, topics = 7)
thougts7

#Podemos pedir las palabras más representativas para cada tópico
#Highest Prob and Frex son las más informativas
labelTopics(police_TM8, c(1, 2, 3, 4, 5, 6, 7, 8), n=15)
