
library(readxl) 
library(ggplot2) #Paqueteparaconfeccionardibujos
library(GGally) #Paquetequeextiendefuncionesdeggplot2
library(plyr)


setwd("~/MAESTRIA/AID")

df<- read_excel("gorriones.xlsx")
data.frame(df)

#Produce un diagrama comparativo de boxplots

ggplot(data=df,aes(y=Largo),colour=factor(Sobrevida))+
geom_boxplot(aes(x=Sobrevida,fill=factor(Sobrevida)))+
xlab("")+
ylab("Largo")+
theme(axis.text.x=element_blank(),axis.ticks=element_blank(),
axis.line=element_line(colour="royalblue",size=0.5,linetype="solid"))+
labs(fill="Sobrevida")+
scale_fill_brewer(palette="BuPu")

#Repetir eso para el resto de las variables Alas, cabeza, etc... 


#Construir gráficos bivariados para todas las variables en cuestión, particionando por el grupo de supervivencia y considerando un color para cada grupo. 
#¿Se observa alguna regularidad que pueda explicar la supervivencia?


df$Sobrevida=factor(df$Sobrevida)

ggplot(df,aes(Largo,Alas))+
geom_point(aes(colour=Sobrevida))+
xlab("Largo")+
ylab("Alas")+
labs(colour='Sobrevida')

ggplot(df,aes(Cuerpo,Cabeza))+
  geom_point(aes(colour=Sobrevida))+
  xlab("Cuerpo")+
  ylab("Cabeza")+
  labs(colour='Sobrevida')


ggplot(df,aes(Pata,Cabeza))+
  geom_point(aes(colour=Sobrevida))+
  xlab("Pata")+
  ylab("Cabeza")+
  labs(colour='Sobrevida')


#Diagrama de dispersion de pares DISPERSOGRAMA => ES LO MISMO QUE MATRIZ DE DIAGRAMA DE DISPERSION NO??? 
#Faltaría agregar una refrencia para ver cuales son los vivos y los muertos

pairs(df[-c(1,7)], pch=19,cex=0.8,col=df$Sobrevida)



