library(readxl) 
library(ggplot2) 
library(GGally) 
library(plyr)


setwd("~/MAESTRIA/AID")

hospitales <- read_excel("hospitales.xlsx")
df <- (hospitales[-1])

pca <- prcomp(df, scale. = TRUE )

#2. Graficar las cargas y explicar la interpretación de las componentes principales.

summary(pca)

#No se como graficar las cargas
cargas = data.frame(pca$rotation)
cargas$var = rownames(cargas)

ggplot(cargas, aes(var, PC1))+
  geom_col() 

ggplot(cargas, aes(var, PC2))+
  geom_col() 

ggplot(cargas, aes(var, PC3))+
  geom_col() 

ggplot(cargas, aes(var, PC4))+
  geom_col() 

# 3 ¿Qué porcentaje de variabilidad logra captar cada una de ellas? Graficar el scree plot

screeplot(pca, type = c("barplot", "lines"))

# 4. ¿Parece adecuado considerar dos componentes principales?
#Rta: no porque las primeras dos solo explican el 68,1% de la variabilidad. Debería considerar las primeras 4

#5. Hallar la correlación entre las nuevas variables y las originales.

nuevasvar = pca$x

cor(df, nuevasvar)

