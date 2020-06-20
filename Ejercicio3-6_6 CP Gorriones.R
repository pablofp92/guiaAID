
library(readxl) 
library(ggplot2) #Paqueteparaconfeccionardibujos
library(GGally) #Paquetequeextiendefuncionesdeggplot2
library(plyr)


setwd("~/MAESTRIA/AID")

gorriones <- read_excel("gorriones.xlsx")
df <- data.frame(gorriones[-1])


#1. Estandarizar las variables y calcular la matriz de covarianzas para las variables estandarizadas.
dfnorm <- scale(df)

mcov = cov(dfnorm)


#2. Verificar que ésta es la matriz de correlación de las variables originales.
# son iguales, uso esta funcion porque  si comparo con == da False, por pequeñas diferencias en el rango de los floats.
mcor = cor(df)
all.equal(mcov,mcor)

#3. ¿Parece adecuado en este caso un análisis de componentes principales? ¿Qué indica el autovalor para una componente principal?

#Rta: hay algunas variables que tienen correlaciones altas, puedo aplicar PCA para reducir la dimensión del problema
# El autovalor de la mcov/mcor indica el % variabilidad explicada por la componente principal

#  4. ¿Cuántas componentes son necesarias para explicar el 80% de la varianza total?Realizar el grafico de sedimentación, fundamentando la respuesta con este gráfico.


autovector_cov = eigen(mcov)$vectors
autovalor_cov  = eigen(mcov)$values
sedimentacion = data.frame(autovalor_cov/sum(autovalor_cov), seq(1:6))
colnames(sedimentacion) = c('porcentaje', 'componente')

#grafico de sedimentacion
ggplot(sedimentacion, aes(componente, porcentaje))  + geom_col() + geom_line() + theme_light()


#5. ¿Cuál es la expresión de la primera componente principal?

#Rta: Me está pidiendo los loadings solamente?

autovector_cov[,1]


#6. ¿Cómo queda expresada la primera componente principal en función del autovector correspondiente y de las variables?
#Rta: no entiendo cual es la diferencia entre esta pregunta y la anterior....

# 7. Encontrar las coordenadas del pájaro 11 en las nuevas componentes.

gorriones.pca = prcomp(df, scale. = TRUE)

ggbiplot(gorriones.pca, ellipse=TRUE, labels=rownames(df), groups = df$Sobrevida, obs.scale = 1)


# como puedo obtener las coordenadas de forma precisa en R?

#Rta: estarían en df.pca$x...pero no son las que muestra el biplot salvo que indique obs.scale = 1

#8. Representar gráficamente en el plano: Eje 1 vs. Eje 2, Eje 1 vs. Eje 3, Eje 2 vs. Eje3. Interpretar los tres primeros ejes.



#9. Realizar un gráfico donde se observen las aves en los nuevos ejes 1 y 2, resaltando con distinto color el grupo de los que sobrevivieron.
# Contestado en punto 7


#10. Utilizar el Análisis en Componentes Principales como método para encontrar outliers.

#Rta: se puede ver outliers manualmente: 37, 25, 30, 40, 29.  
