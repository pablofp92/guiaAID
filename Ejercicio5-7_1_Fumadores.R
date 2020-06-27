library(readxl)
library(ggplot2)
library(dplyr)
library(purrr)
library(reshape2)
library(ggbiplot)
library(FactoMineR)
library(factoextra)
library(ca)


setwd("~/MAESTRIA/AID")

#construyotabla
tabla=as.table(rbind(c(4,2,3,2),c(4,3,7,4),c(25,10,12,4),c(18,24,33,13),c(10,6,7,2)))
dimnames(tabla)=list(puesto=c('Gerente Senior','Gerente Junior','Empleado Senior','Empleado Junior','Secretaria'),
Fumador=c('No fuma','fuma poco','fuma moderado','fuma mucho'))


# Analizar si la distribución de la variable fumador es similar en todos los niveles de la variable puesto de desempeño


#convierto en tabla de distribuciones condicional (perfiles fila)
tablar=tabla/rowSums(tabla)
perfilcol = tabla/colSums(tabla)

df2=melt(tablar)


    


#2.Realizar análisis de correspondencia con la tabla de frecuencias relativas


fum.ac=CA(tablar,graph=FALSE)
summary(fum.ac)


#idem pero con la tabla original
fum.ac2=CA(tabla,graph=FALSE)


#4. Relaciones entre las variables y los ejes
        #Grafica las categorías de las filas
        
        fviz_contrib(fum.ac,choice="row",axes=1,fill="royalblue",color="black")+
          theme_gray()+
          theme(axis.text.x=element_text(angle=0))+
          xlab('Puesto')+
          ylab('')+
          ggtitle('')
        
        #Grafica las categorías de lasc olumnas
        
        fviz_contrib(fum.ac,choice="col",axes=1,fill="royalblue",color="black")+
          theme_gray()+
          theme(axis.text.x=element_text(angle=0))+
          xlab('Fumador')+
          ylab('')+
          ggtitle('')

# biplot simétrico
  fviz_ca_biplot(fum.ac,repel=TRUE,col.row="royalblue",
  col.col="indianred")+
  theme_gray()+
  xlab(paste0('Dim.1 (', round(fum.ac$eig[1,2], 1), ' %)'))+
  ylab(paste0('Dim.2 (', round(fum.ac$eig[2,2], 1), ' %)'))+
  ggtitle('')

  
  fviz_ca_biplot(fum.ac2,repel=TRUE,col.row="royalblue",
                 col.col="indianred")+
    theme_gray()+
    xlab(paste0('Dim.1 (', round(fum.ac2$eig[1,2], 1), ' %)'))+
    ylab(paste0('Dim.2 (', round(fum.ac2$eig[2,2], 1), ' %)'))+
    ggtitle('')
  


#3. Realizar los gráficos de perfiles que pueden considerarse adecuados.

  
  #grafico perfiles fila
  ggplot(data=df2,aes(x=Fumador,y=value,group=puesto,color=puesto))+
    geom_line(linetype="dashed")+
    geom_point(color="royalblue",size=1)+
    xlab("")
    
m_perfilcol =   melt(perfilcol)
       
    #grafico perfiles columna
    ggplot(data=m_perfilcol,aes(x=puesto,y=value,group=Fumador,color=Fumador))+
    geom_line(linetype="dashed")+
    geom_point(color="royalblue",size=1)+
    xlab("")+
        ylab("Proporción")
    
# 6. ¿Cuál es la inercia total? # ASI HECHO ESTA BIEN????

Inercia_total = sum(fum.ac$row[['inertia']])




