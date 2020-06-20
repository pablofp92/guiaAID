library(ggplot2) #Paqueteparaconfeccionardibujos
library(GGally) #Paquetequeextiendefuncionesdeggplot2
library(dplyr)
library(ggbiplot)

mcov = matrix( c(3, 1, 1, 1, 4, 0, 1, 0, 2), nrow = 3, ncol = 3, byrow = T)

#1. Calcular los autovalores de la matriz Σ.

autovectores  = eigen(mcov)$vectors
autovalores  = eigen(mcov)$values

#2 da importancia a Econometria (X1) y asignaturas libres (X3) y resta importancia a derecho (X2) ???

Y2 = paste(toString(autovectores[1,2]), '* X1 +', toString(autovectores[2,2]), '* X2 +', toString(autovectores[3,2]), '* X3',  sep = " ")



#3 ???

