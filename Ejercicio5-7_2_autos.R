library(readxl)
library(ggplot2)
library(dplyr)
library(ggbiplot)
library(ca)
library(fastDummies)
library(FactoMineR)
library(factoextra)


setwd("~/MAESTRIA/AID")


autos = read_xlsx("autos.xlsx")





#1. Elegir tres variables y construir la matriz disyuntiva y la matriz de Burt, explicando el significado de los valores diagonales y verificando las propiedades de la matriz.

base = autos[c("Sexo", "Casa", "Ingreso")]
dummies = dummy_cols(base, ignore_na=TRUE)
dummies = dummies[,-(1:3)]

#disyuntiva
g = as.matrix(dummies)

#matriz de burt
burt = t(g) %*% g
burtdf = as.data.frame(burt)

#propiedades: es positiva y simétrica. Puede ser de rango incompleto
burt >= 0
dim(burt)
qr(burt)$rank <= dim(burt)


#2. Realizar un análisis de correspondencias múltiples con estas variables y explicar los resultados.

base$indice = rownames(base)

analisis = MCA(base[1:3],  graph = FALSE)


# biplot simétrico

fviz_mca_var(analisis, repel=TRUE, col.var ="royalblue")
  xlab(paste0('Dim.1 (', round(analisis$eig[1,2], 1), ' %)'))+
  ylab(paste0('Dim.2 (', round(analisis$eig[2,2], 1), ' %)'))+
  ggtitle('') +
  theme_gray()

  
  
