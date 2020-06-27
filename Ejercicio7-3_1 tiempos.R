library(MASS)
library(car)
library(RVAideMemoire)

tabla =cbind(c(0.17, 0.26, 0.19, 0.34, 0.52, 0.33, 0.23, 0.20, 0.18, 0.22 , 0.21, 0.22, 0.28, 0.25, 0.90, 0.33, 0.22, 0.17, 0.39 , 0.27),
                      c(0.18, 0.33, 0.23, 0.16, 0.19, 0.30, 0.21, 0.20, 0.16, 0.21, 0.20, 0.3, 0.32, 0.2, 0.19, 0.22, 0.27, 0.24, 0.29, 0.27))

tabla = data.frame(tabla)
colnames(tabla) =c('T1', 'T2')

#1. ¿Satisfacen los datos el supuesto de normalidad?
shapiro.test(tabla$T1) #T1 no lo pasa
shapiro.test(tabla$T2) 


#transforamción box cox
boxcox(tabla$T1 ~ tabla$T2, plotit=TRUE)

T1 = tabla$T1
T2 = tabla$T2

T1_ = T1^(-1.5)
T2_ = T2^(-1.5)


shapiro.test(T1_) 


#test t
t.test(T2_,T1_,  alternative = "two.sided", mu = 0, var.equal = TRUE,  conf.level = 0.95)
#pasa el test

# test t a mano para ejercitar y ver que da lo mismo

var1 = var(T1_)
var2 = var(T2_)

testt = (mean(T2_)-mean(T1_))/sqrt(0.5*(var1+var2)*(2/20)) 

#2. Aplicar una prueba no paramétrica y comparar los resultados con los obtenidos en el ítem anterior.

wilcox.test(T1_, T2_, alternative = "two.sided")
#tambien lo pasa, con un p valor similar


