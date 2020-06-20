
library(readxl) 
library(dplyr)
library(purrr)

setwd("~/MAESTRIA/AID")

gorriones <- read_excel("gorriones.xlsx")

df <- data.frame(gorriones)
df <- df[-1]


# Calcular la dimensión de la base de datos notando por n al número de observaciones y por p a la cantidad de variables observadas sobre cada individuo.

n = nrow(df)
p = ncol(df)


# 2. Hallar el vector de medias, la matriz de varianzas y covarianzas y la matriz de correlaciones. ¿Qué características tienen estas matrices?

medias <- summarise_all(df, .funs =mean)[-c(7)]

matriz_correlacion = cor(df)

matriz_covarianzas = cov(df)

## la matriz de varianzas no me queda claro que explica y cual es la diferencia con la de covarianza...
matriz_varianzas = var(df)


# 3. Explicar qué representan los elementos m11 y m31 de la matriz de varianzas y covarianzas.

# 4. Explicar qué representa los elementos m22 y m13 de la matriz de correlaciones.

# 5. Relacionar los elementos m21, m11 y m22 de la matriz de varianzas y covarianzas  con el elemento m12 de la matriz de correlaciones.

# 6. Hallar una nueva variable e incorporarla en la base de datos, llamada Diferencia y que mida la diferencia entre el largo total y el largo del húmero.
#asumo que el humero es la medida del ala/4...? 

df2 <- df %>%
  mutate(diferencia = Largo-(Alas/4))


# 7. Calcular el vector de medias y las matrices de varianzas y covarianzas y la matriz  de correlaciones de la nueva base de datos, relacionando el nuevo vector de medias
# con el anterior.

medias2 <- summarise_all(df2, .funs =mean)
matriz_correlacion2 = cor(df2)
matriz_covarianzas2 = cov(df2)
matriz_varianzas2 = var(df2)

# 8. Hallar la traza de las cuatro matrices calculadas, explicando el significado de cada uno de los resultados obtenidos. ¿Qué trazas no aumentan al agregar una variable?

