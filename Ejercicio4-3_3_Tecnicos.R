Norte<-c(20,18,12,17,67)
Sur<-c(6,22,15,13,56)
Este<-c(4,6,14,11,35)
Oeste<-c(10,19,23,40,92)
Tab<-as.table(rbind(Norte,Sur,Este,Oeste))
dimnames(Tab)<-list(Zona=c("Norte","Sur","Este","Oeste"),
                    Especialidad=c('A','B','C','D','E'))
Tab

# 1. ¿Puede considerarse adecuado un test de homogeneidad o de independencia?
# Fundamentar la respuesta considerando el tipo de muestreo realizado.

# Independencia: Las especialidades están igualmente distribuidas en las distintas zonas

# 2. Establecer las hipótesis de interés, realizar el contraste y concluir considerando un
# nivel de significación del 1%.

# H0: La distribución de especialidades es independiente de las zonas (para todo i y j 
# P(X=xi,Y=yj)=P(X=xi).P(Y=yj) i= 1, 2, 3, 4, 5; j= 1, 2,3 4).
# H1: las variables no son independientes (existe al menos algún par i y j tal que 
# P(X=xi,Y=yj)!=P(X=xi).P(Y=yj) i= 1, 2, 3, 4, 5; j= 1, 2,3 4).

test <- chisq.test(Tab)
test

# Rechazo la hipótesis con nivel de significancia de 0,01 (p=0,007). Existe al menos un par 
# que no cumple con H0

