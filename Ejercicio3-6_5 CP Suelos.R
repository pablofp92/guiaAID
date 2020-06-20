library(ggplot2) 
library(GGally) #Paquetequeextiendefuncionesdeggplot2
library(dplyr)
library(ggbiplot)
library(readxl) 

setwd("~/MAESTRIA/AID")

suelos <- read_excel("suelos.xlsx")
df = data.frame(suelos)

# 1. Comparar los resultados del Análisis en Componentes Principales para la matriz decovarianza y para la matriz de correlación.

mcov = cov(df)
mcor = cor(df)


autovector_cov = eigen(mcov)$vectors
autovalor_cov  = eigen(mcov)$values


autovector_cor = eigen(mcor)$vectors
autovalor_cor  = eigen(mcor)$values


# Rta: no dan lo mismo, de hecho con la matriz de covarianza da igual* que el prcomp con .scale = FALSE 
#con correlación es el equivalente a .scale = TRUE

#* no da igual sino muy parecido, por qué?

pca_scale_true = prcomp(df, scale. = TRUE)
pca_scale_false  = prcomp(df, scale. = FALSE)

pca_scale_true$rotation 
pca_scale_false$rotation


# 2. Los porcentajes de variabilidad que logran explicar cada una de las componentes,¿son los mismos?
# Rta:  Claramente no. Los autovalores son cada vez más chicos
summary(pca_scale_false)


#  3. ¿Cambia el orden de las componentes?
# Rta: el orden no parece cambiar

#  4. ¿Cambian los loadings de las componentes?
# Rta: sí, los loadings cambian porque son distintos autovectores

autovector_cor == autovector_cov

#  5. ¿Cuál de los dos análisis parece más adecuado? ¿Por qué?

#Rta: el normalizado porque estoy comparando unidades distintas. 
  

