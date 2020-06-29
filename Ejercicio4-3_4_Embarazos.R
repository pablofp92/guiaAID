Normal<-c(37,1334)
Patologico<-c(11,223)
Tab<-as.table(rbind(Normal,Patologico))
dimnames(Tab)<-list(Embarazo=c("Usa_Twitter","No_Usa"),Angioma=c("Con","Sin"))
Tab

# Plantear y testear las hipótesis correspondientes considerando un nivel de significación 
# del 5%.

# Test de homogeneidad - la presencia de angioma difiere entre las poblaciones?
# H0: la presencia de angioma es homogénea en las dos poblaciones (emb normal vs patológico)
# (para todo i y j P(X=xi|Y=Pobj)=P(X=xi) i= 1, 2; j= 1, 2).
# H1: la presencia de angioma no es homogénea en las dos poblaciones (emb normal vs patológico)
# (exite al menos algún par i y j tal que P(X=xi|Y=yj)!=P(X=xi) i= 1, 2; j= 1, 2)

test <- chisq.test(Tab)
test

# No puedo concluir que haya diferencias con un nivel de significación de 0.05 (p~0.1459)

