
library(readxl) 
setwd("~/MAESTRIA/AID")

recep<- read_csv("recepcionistas.csv")


colnames(recep)<-c("candidatos","cordialidadJuez1","presenciaJuez1","idiomaJuez1","cordialidadJuez2","presenciaJuez2","idiomaJuez2")

attach(recep)

# Graficos de cajas para visualizar diferencias entre los jueces
par(mfrow=c(1,1))
boxplot(recep[,c(2,5)],horizontal=T,col=c("seagreen1","salmon"),main="Puntaje de cordialidad segun juez")


#UNI TABLAS, lo hice de otra manera, sin cbind 

recep2 <- recep
recep2[,c(2,3,4)] <- NULL 
recep2$juez <- 2
colnames(recep2)<-c("candidatos","cordialidad","presencia","idioma","juez")


recep1 <- recep
recep1[,c(5,6,7)] <- NULL 
recep1$juez <- 1
colnames(recep1)<-c("candidatos","cordialidad","presencia","idioma","juez")


recepUnion <- rbind(recep1, recep2)

### Transformacion de datos por fila

mediasF=apply(recep[,-1],1,mean)
rangosF=apply(recep[,-1],1,max)-apply(recep[,-1],1,min)
deviosF=apply(recep[,-1],1,sd)

rec.transF=(recep[,-1]-mediasF)/rangosF

rec.transF.2=(recep[,-1]-mediasF)/deviosF
#verifico que tienen media 0 y desv estÃ¡ndar 1
apply(rec.transF.2,1,mean)
apply(rec.transF.2,1,sd)

# scale transforma los datos (de las columnas de una matriz dada) para obtener media 0 y desvÃ?o 1
estandarizoFil<-scale(t(recep[,-1]),center=T,scale=TRUE)# Notar que se transpone para afectar las filas originales
#verifico que tienen media 0 y desv estÃ¡ndar 1
apply(t(estandarizoFil),1,mean)
apply(t(estandarizoFil),1,sd)




### Transformacion de datos por fila separando por juez
medias=apply(recepUnion[,2:4],1,mean)
rangos=apply(recepUnion[,2:4],1,max)-apply(recepUnion[,2:4],1,min)
desvios=apply(recepUnion[,2:4],1,sd)

rec.trans=(recepUnion[,2:4]-medias)/rangos
rec.trans.2=(recepUnion[,2:4]-medias)/desvios

#grÃ¡fico de coordenadas paralelas
plot(1:3,rec.trans.2[1,1:3],type="l",col=4,lwd=2,xlab=" ",
     ylim=c(-2,2),ylab="Puntuacion estandarizada",xlim=c(1,3.5),xaxt="n")
axis(1, at=1:3,labels=c("Cordialidad","Presencia","Idioma"), las=2)
for(i in 2:6){
  points(1:3,rec.trans.2[i,1:3],type="l",col=4,lwd=2)
}
for(j in 7:12){
  points(1:3,rec.trans.2[j,1:3],type="l",col=6,lwd=2)
}

mtext("ComparaciÃ³n de candidatas segun graico de coordenadas paralelas",line=1,font=2)
legend.text=c("Juez 1","Juez 2")
legend(3.1,0,legend.text,text.col=c(4,6),lty=1,col=c(4,6),lwd=2,
       cex=0.7,text.width=1.5,box.lty=0,bty="n")

#grÃ¡fico de perfiles
MediaJuez1<-apply(recepUnion[1:6,2:4],2,mean)
MediaJuez2<-apply(recepUnion[7:12,2:4],2,mean)

plot(1:3,MediaJuez1,type="l",col=4,lwd=2,xlab=" ",
     ylim=c(50,90),ylab="Media de Puntajes",xlim=c(1,3.5),xaxt="n")
axis(1, at=1:3,labels=c("Cordialidad","Presencia","Idioma"), las=2)
points(1:3,MediaJuez2,type="l",col=6,lwd=2)

mtext("Comparación de puntajes por Juez según grÃ¡fico de perfiles",line=1,font=2)
legend.text=c("Juez 1","Juez 2")
legend(3.1,70,legend.text,text.col=c(4,6),lty=1,col=c(4,6),lwd=2,
       cex=0.7,text.width=1.5,box.lty=0,bty="n")

## Visualizacion de diferencias entre jueces

#Rearmo la matriz de variables transformadas agregando la columna que identifica al juez para hacer boxplot
J1<-cbind(rec.transF.2[,1:3],rep(1,nrow(rec.transF.2)))
colnames(J1)<-c("cordialidad","presencia","idioma","juez")
J2<-cbind(rec.transF.2[,4:6],rep(2,nrow(rec.transF.2)))
colnames(J2)<-c("cordialidad","presencia","idioma","juez")
J1J2<-rbind(J1,J2)

boxplot(split(J1J2$cordialidad,J1J2$juez),horizontal=T,col=c("royalblue","navajowhite"),main="Puntaje de cordialidad segun juez")
boxplot(split(J1J2$presencia,J1J2$juez),horizontal=T,col=c("royalblue","navajowhite"),main="Puntaje de presencia segun juez")
boxplot(split(J1J2$idioma,J1J2$juez),horizontal=T,col=c("royalblue","navajowhite"),main="Puntaje de idioma segun juez")

plot(1:12,rec.trans$cordialidad,type="o",col="red1",lwd=2,xlab="Candidatas",
     ylim=c(-1,1),ylab="PuntuaciÃ³n estandarizada",xlim=c(1,12))
points(1:12,rec.trans$presencia,type="o",col="olivedrab1",lwd=2)
points(1:12,rec.trans$idioma,type="o",col="turquoise1",lwd=2)
title("Comparación de perfiles")
legend.text=c("Cordialidad","Presencia","Idioma")
legend(10,1,legend.text,text.col=c("red1","olivedrab1","turquoise1"),
       cex=0.7,text.width=1.5,box.lty=0,bty="n")

plot(1:12,rec.trans$cordialidad,type="o",col="red1",lwd=2,xlab=" ",
     ylim=c(-1,1),ylab="PuntuaciÃ³n estandarizada",xlim=c(1,12),xaxt="n")
Map(axis, side=1, at=1:13, col.axis=c(rep(4,6),rep(6,6)), labels=recepUnion[,1], las=2)
#axis(1, at=1:12,labels=FALSE, las=2)
points(1:12,rec.trans$presencia,type="o",col="olivedrab1",lwd=2)
points(1:12,rec.trans$idioma,type="o",col="turquoise1",lwd=2)
title("Comparación de perfiles")
legend.text=c("Cordialidad","Presencia","Idioma")
legend(10,1,legend.text,text.col=c("red1","olivedrab1","turquoise1"),
       cex=0.7,text.width=1.5,box.lty=0,bty="n")
legend(2,-0.8,"Juez 1",text.col=4,
       cex=0.7,text.width=1.5,box.lty=0,bty="n")
legend(7,-0.8,"Juez 2",text.col=6,
       cex=0.7,text.width=1.5,box.lty=0,bty="n")


#################################
## Transformaciones por columna
##################################

estandarizoCol<-scale(recepUnion[,2:4],center=T,scale=TRUE)
#verifico que tienen media 0 y desv estndar 1
apply(estandarizoCol,2,mean)
apply(estandarizoCol,2,sd)


