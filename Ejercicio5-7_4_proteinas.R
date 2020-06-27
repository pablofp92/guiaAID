library(readxl)
library(ggplot2)
library(dplyr)
library(ggbiplot)
library(ca)
library(fastDummies)
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(ade4)
library()


setwd("~/MAESTRIA/AID")


proteinas = read_xlsx("proteinas.xlsx")
proteinas = data.frame(proteinas)


#Transformaciones para que quede usable
proteinas = proteinas %>% 
  mutate_all(funs(str_replace(., ",", ".")))

proteinas[,2:10] = as.numeric(unlist(proteinas[,2:10]))
proteinas[24,2:10] = proteinas[24,2:10] + proteinas[7,2:10]
proteinas = proteinas[-c(7),]

proteinas$region[proteinas$País %in% c("Portugal", "España", "Italia")] = 'Europa del Sur'
proteinas$region[proteinas$País %in% c("Francia", "Inglaterra", "P.Bajos", "Bélgica", "Irlanda")] = 'Europa del Oeste'
proteinas$region[proteinas$País %in% c("Alemania", "Polonia", "Austria", "Checosl", "Hungría", "Suiza")] = 'Europa Central'
proteinas$region[proteinas$País %in% c("Grecia", "Rumania", "Bulgaria", "Albania")] = 'Europa del Sureste'
proteinas$region[proteinas$País %in% c("Ucrania", "Rusia", "Italia")] = 'Europa del Este'
proteinas$region[proteinas$País %in% c("Finlandia", "Noruega", "Suecia", "Dinamarca")] = 'Europa del Norte'
proteinas$País = as.factor(proteinas$País)
proteinas$region = as.factor(proteinas$region)


#ANALISIS DE CORRESPONDENCIA SIMPLE

paises = proteinas$País
alimentos = colnames(proteinas)[-c(1,11)]


base1 = as.matrix(proteinas[2:10])
colnames(base1) = alimentos
row.names(base1) =paises

proteinas.ac = CA(base1, graph = FALSE)

# biplot simétrico
fviz_ca_biplot(proteinas.ac ,repel=TRUE,col.row="royalblue",
               col.col="indianred")+
  theme_gray()+
  xlab(paste0('Dim.1 (', round(proteinas.ac$eig[1,2], 1), ' %)'))+
  ylab(paste0('Dim.2 (', round(proteinas.ac$eig[2,2], 1), ' %)'))+
  ggtitle('')

#ANALISIS CORRESPONDENCIAS MULTIPLES 



base = as.matrix(proteinas)


disjunta = acm.disjonctif(base)
disjunta = as.factor(disjunta)


#con dudi...?
proteinas.acm2 = dudi.acm(disjunta, scannf = FALSE)

#esto está bien? puedo meterle esta tabla directamente?
proteinas.acm = MCA(base, graph = FALSE)




fviz_mca_var(proteinas.acm, repel=TRUE, col.var ="royalblue")
xlab(paste0('Dim.1 (', round(proteinas.acm$eig[1,2], 1), ' %)'))+
  ylab(paste0('Dim.2 (', round(proteinas.acm$eig[2,2], 1), ' %)'))+
  ggtitle('') +
  theme_gray()




