library(readxl) 
library(ggplot2) #Paqueteparaconfeccionardibujos
library(GGally) #Paquetequeextiendefuncionesdeggplot2
library(dplyr)
library(reshape2)
library(ggbiplot)


setwd("~/MAESTRIA/AID")

chalets <- read_excel("chalets.xlsx")
df = data.frame(chalets)
colnames(df) = c('promotora', 'duracion', 'precio', 'superficie')

# 1. Graficar el boxplot de cada una de las variables, indicando si se observa la presencia de valores atípicos.

ggplot(df)+
  geom_boxplot(aes(y=duracion))  + 
  coord_flip() +
  theme_light()

ggplot(df)+
  geom_boxplot(aes(y=precio))  + 
  coord_flip() +
  theme_light()

ggplot(df)+
  geom_boxplot(aes(y=superficie))  + 
  coord_flip() +
  theme_light()


# 2. Graficar los diagramas de dispersión de las variables de a pares. Estimar la presencia de correlación entre variables a partir de estos gráficos, indicando si la misma
# puede considerarse fuerte y el signo de las mismas.

plot(df[c('precio','duracion','superficie')])

# 3. Calcular el vector de medias y la matriz de varianzas y covarianzas muestral.

medias = summarise_all(df, .funs =mean)
medias['promotora'] <- NULL
medias = as.numeric(medias)

matrizdf = as.matrix(df[-1])


mcov = cov(matrizdf)

# 4. Hallar la matriz de correlación muestral. Verificar las estimaciones realizadas visualmente.

mcor = cor(matrizdf)

# 5. A partir de estas observaciones, ¿resulta razonable pensar en un análisis de componentes principales para reducir la dimensión del problema?
#si obvio

#6. Hallar la primera componente principal y graficar sus coeficientes mediante barras verticales.

#METODO 1: A MANO

autovectores = eigen(mcor)$vectors
autovalores = eigen(mcor)$values

PPC = matrizdf %*% autovectores[,1]
proporcion =  autovalores[1]/sum(autovalores)

#METODO 2: Funcion prcomp

chalets.pca <- prcomp(df[-1], center = TRUE,scale. = TRUE)
summary(chalets.pca)

# 7. Indicar qué porcentaje de la variabilidad total logra explicar esta componente. Explicar si se trata de una componente de tamaño o de forma. Es posible ordenar las
# promotoras en función de esta componente? Si la respuesta es afirmativa, ¿cuál
# es la mayor y cuál la menor? En caso contrario, explicar por qué no es posible
# ordenarlos.

ggbiplot(chalets.pca, ellipse=TRUE, labels=df$promotora)

proporcion =  autovalores[1]/sum(autovalores)

