library(ggplot2) #Paqueteparaconfeccionardibujos
library(GGally) #Paquetequeextiendefuncionesdeggplot2
library(dplyr)
#library(reshape2)
library(ggbiplot)



setwd("~/MAESTRIA/AID")

matriz = matrix( c(3, 6, 5, 6, 10, 12), nrow = 3, ncol = 2, byrow = T)


# 1 Calcular la matriz de covarianza, sus autovalores y autovectores.

mcov =cov(matriz)
autovectores = eigen(mcov)$vectors
autovalores = eigen(mcov)$values


#2. Hallar las componentes principales y su contribución porcentual a la varianza total.

#metodo prcomp
matriz.pca = prcomp(matriz)
summary(matriz.pca)

#metodo manual

contribucionY1 = autovalores[1]/ sum(diag(mcov))
contribucionY2 = autovalores[2]/ sum(diag(mcov))


#3. Graficar los datos en R teniendo en cuenta la base original y luego la base de los dos primeros ejes.

#NO ENTENDI QUE PIDE!

plot(matriz)
ggbiplot(matriz.pca, var.axes = FALSE)


#4. Repetir los cálculos con los datos estandarizados e interpretar los resultados obtenidos

#No entiendo que pide! 

#5. Verificar que los dos primeros autovectores son ortogonales entre sí.

ifelse(((autovectores[,1] %*% autovectores[,2]) == 0), print("son ortogonales"), print("No son ortogonales"))

#Representar gráficamente estos dos vectores en un gráfico bidimensional y trazar rectas desde el origen hasta la ubicación de cada uno de los vectores en el gráfico.


#cual seria la diferencia con el biplot?