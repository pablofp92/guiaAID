
library(readxl) 
library(dplyr)

setwd("~/MAESTRIA/AID")

perros <- read_excel("perros.xlsx")
df <- data.frame(perros) 

df <- df[,1:6]

#Colocaetiquetas
row.names(df)=c(df$Raza)

stars(df,full=F,cex=0.8,flip.labels=T,len=0.9,col.stars=cm.colors(27))
#Produceundiagramadeestrellas

#. Idem al inciso anterior por funci�n. NO ENTIENDO, COMO PUEDO HACERLO SI SON CATEGORIAS? HAGO UN PROMEDIO???? NO ME SALE. 

df <- data.frame(perros) 
df <- df[,2:8]

df$Funci�n = factor(df$Funci�n)

df2 <- df %>%
  group_by(Funci�n) %>%
  summarise_each(funs(mean))


row.names(df2)=c(df2$Funci�n)
stars(df2,full=F,cex=0.8,flip.labels=T,len=0.9,col.stars=cm.colors(3))



