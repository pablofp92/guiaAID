
library(readxl) 
library(ggplot2) #Paqueteparaconfeccionardibujos
library(GGally) #Paquetequeextiendefuncionesdeggplot2
library(plyr)


setwd("~/MAESTRIA/AID")

df<- read_excel("internet.xlsx")

#Clasificar las variables de la base de datos y, para las que sean numéricas, construir un gráfico de coordenadas paralelas.

ggparcoord(data=df,columns=c(8,9),mapping=aes(color=as.factor(Nacionalidad)))+
scale_color_discrete("Nacionalidad",labels=levels(df$Nacionalidad))+
xlab("")+
ylab("")+
scale_x_discrete(limit=c("Temperatura","Autos"),
labels=c("Edad","Estatura"))


# Produce diagrama de coordenadas p a r a l e l a s NO ME SALEEEEEEEEEEEEEEEEEEEEEEEEEE

#Construir la tabla de frecuencias de la variable Sexo/Edad/Temperatura, Autos y Cigarrillos.
# ¿Hay algún valor que pueda llamar la atención? ¿Qué tipo de error podría ser?

barplot(table(df$Sexo))
hist(df[df$Edad < 100], main =" Edad ")
barplot(table(df$Temperatura), main ="Temperatura")
barplot(table(df$Autos), main = "Autos" )
barplot(table(df$Cigarrillos), main = "Cigarrillos")

#se observa un valor que no es ni hombre ni mujer. Podría corresponde a un error de omisión en el llenado de datos


#Eliminar de la base de datos aquellos valores que no son posibles y que probablemente corresponden a un error de tipeo

df$Edad[df$Edad > 100] <- NA

df$Sexo[df$Sexo  %in% c(1,2) ] <- NA

#ETC...






