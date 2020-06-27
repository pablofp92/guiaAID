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
carga.1 = pca$rotation[,1]
carga.2 = pca$rotation[,2]
columnas = factor(colnames(df))
data = data.frame(cbind(carga.1, carga.2))

ggplot(data, aes(x=columnas, y=carga.1))+
  geom_bar(stat = 'identity', position = 'dodge', fill = 'royalblue', size=0.5)

# La componente 1 separa las distintas variables. Podríamos llamarla "Clase" del servicio

ggplot(data, aes(x=columnas, y=carga.2))+
  geom_bar(stat = 'identity', position = 'dodge', fill = 'royalblue', size=0.5)

# La componente 2 es toda positiva, daría indicación del "Tamaño" del servicio

# 3 ¿Qué porcentaje de variabilidad logra captar cada una de ellas? Graficar el scree plot

screeplot(pca, type = c("barplot", "lines"))

# 4. ¿Parece adecuado considerar dos componentes principales?
#Rta: no porque las primeras dos solo explican el 68,1% de la variabilidad. Debería considerar las primeras 4

#5. Hallar la correlación entre las nuevas variables y las originales.

nuevasvar = pca$x

cor(df, nuevasvar)

