
#creación de la matriz
matriz = matrix( c(3, 1, 1, 1, 3, 1 ,1 ,1, 5), nrow = 3, ncol = 3, byrow = T)

#1. Hallar los autovalores y autovectores de la matriz de varianzas y covarianzas.

mcov = cov(matriz)
mvar = var(matriz)

autovalores_mcov <- eigen(mcov)$values
autovectores_mcov <-  eigen(mcov)$vectors

#esto es lo mismo!
autovalores_mvar <- eigen(mvar)$values
autovectores_mvar <-  eigen(mvar)$vectors


#2. Dar la expresión de las componentes principales Y = (Y1, Y2, Y3) t e indicar la proporción de la variabilidad explicada por cada una de ellas.

Y1 = matriz %*% autovectores_mvar[,1]
Y2 = matriz %*% autovectores_mvar[,2]
Y3 = matriz %*% autovectores_mvar[,3]

# "las componentes del autovector a1 son los coeficientes de la combinación lineal que define la primera componente principal"
# "segunda componente principal corresponde al autovector asociado al segundo de los autovalores ordenados en forma decreciente; es decir, λ2.
# la varianza de cada componente principal es el autovalor correspondiente
# la proporcion de variabilidad se calcula como el autovalor sobre la traza de la matriz de covarianza

traza = sum(diag(mcov)) 

proporcion_Y1 =  autovalores_mcov[1] /  traza
#75% 
proporcion_Y2 =  autovalores_mcov[2] /  traza
proporcion_Y3 =  autovalores_mcov[3] /  traza


#3. Hallar los loadings de la primera componente principal.

loadings_Y1 = autovalores_mcov[1]

#4. Hallar los scores de las primeras dos componentes principales correspondientes a la observación X = (2, 2, 1)t

#### ESTE NO LO ENTIENDO! 


