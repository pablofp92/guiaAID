CantTwitter<-c(14,25)
CantNoTwitter<-c(159,152)
Tab<-as.table(rbind(CantTwitter,CantNoTwitter))
dimnames(Tab)<-list(Stress=c("Usa_Twitter","No_Usa"),Sexo=c("Fem","Masc"))
Tab

# 1. Representar gráficamente esta información e interpretar el gráfico
par(bg="lightcyan")
mosaicplot(t(Tab),col=c("aquamarine3","tan1"),main="Nivel de stress según estado civil",ylab="Nivel de stress",xlab="Estado civil",
           cex=0.8)

# 2. Obtener los porcentajes por filas y comparar las diferentes zonas.

#no entiendo si es esto lo que pide
Tab1<-Tab[,-1]
chisq.test(Tab1)

Tab2<-Tab[,-2]
chisq.test(Tab2)

# 3. Obtener los porcentajes por uso y comparar las diferentes usos.

# 4. Calcular las frecuencias esperadas bajo independencia y compararlas con las observadas.
chisq.test(Tab)

chisq.test(Tab)$expected
chisq.test(Tab)$residuals

# 5. ¿Sugieren estos datos que existe diferencia de proporciones entre mujeres y hombres que 
# acceden o no a Twitter? (Considerar α = 0.05.)

chisq.test(Tab)$p.value
# No hay evidencia suficiente (p~0.1)

