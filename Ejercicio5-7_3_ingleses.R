library(readxl)
library(ggplot2)
library(dplyr)
library(ggbiplot)
library(ca)
library(fastDummies)
library(FactoMineR)
library(factoextra)
library(ade4)
library(anacor)

setwd("~/MAESTRIA/AID")


ingleses = read_xlsx("ingleses.xlsx")


# tuve que hacer toda esta transformación porque sino no me lo tomaba la funcion
ingleses$País = as.factor(ingleses$País)
paises = ingleses$País
adjetivos = colnames(ingleses)[-1]

base = as.matrix(ingleses[-1])
colnames(base) = adjetivos
row.names(base) =paises

#analisis de correspondencia
ing.ac = CA(base, graph=FALSE)
summary(ing.ac)

fviz_contrib(ing.ac,choice="col",axes=1,fill="royalblue",color="black")+
  theme_gray()+
  theme(axis.text.x=element_text(angle=0))+
  xlab('Características')+
  ylab('')+
  ggtitle('')


fviz_contrib(ing.ac,choice="row",axes=1,fill="royalblue",color="black")+
  theme_gray()+
  theme(axis.text.x=element_text(angle=0))+
  xlab('Paises')+
  ylab('')+
  ggtitle('')


# biplot simétrico
fviz_ca_biplot(ing.ac,repel=TRUE,col.row="royalblue",
               col.col="indianred")+
  theme_gray()+
  xlab(paste0('Dim.1 (', round(ing.ac$eig[1,2], 1), ' %)'))+
  ylab(paste0('Dim.2 (', round(ing.ac$eig[2,2], 1), ' %)'))+
  ggtitle('')



#1. ¿Qué características son las más usuales?
#Rta: Trabajadores



#2. ¿Qué características son las más raras?
#Rta: los adjetivos  codiciosos y ladinos no están fuertemente asociadas a nacionalidades


#3. En función de estos datos, ¿es justo decir que París es la ciudad del glamour?
#Rta: hay alta asociación entre Francia y los adjectivos sensuales y elegantes

  