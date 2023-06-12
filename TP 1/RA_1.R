library(readxl)#lectura
library(dplyr) #manipulacion de datos
library(kableExtra) #tablas
library(ggplot2) #graficos
library(tidyverse) #manipulacion de datos
library(ggpubr) #para juntar
library(ggforce) # grafico de violin y jitter
library(GGally) # ggpairs
library(corrplot) # para correlogramas
library(ggbeeswarm)
library(gridExtra)
library(sos)
library(data.table)
library(scales)
library(readr)
library(ggcorrplot)
library(gridExtra)  
library(reshape2)
library(modeest)
library(magrittr)
library(MASS)  
library(plyr)
library(ggcorrplot)
library(moments)
library(knitr)
library(fmsb)
library(psych)
library(caret)
library(graphics)
library(cowplot)
library(rpart)
library(rpart.plot)
library(randomForest)
library(C50)
library(rJava)
library(RWeka)
library(party)
library(partykit)
library(C50)
library(yardstick)
library(cvms)
library(tibble) 
library(ROCR)
library(MVN) #Para test de Henze-Zirkler
library(chatgpt)
library(aod) #Para test de wald
library(nortest) # Para test de Shapiro y demas 
library(lmtest)

Sys.setenv(OPENAI_API_KEY = "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")

# Capitulo 1. REGRESION LINEAL SIMPLE

# Correlacion

grasacerdos <-  read_excel("grasacerdos.xlsx")
grasacerdos

plot(grasacerdos)
dim(grasacerdos)
head(grasacerdos)
class(grasacerdos)
describe(grasacerdos)
str(grasacerdos)

#Revisando se ve que PV y EGD estan en character y se tienen que pasar a numeric pero como usan comas y R no entiende antes cambio las comas por puntos

grasacerdos$PV <- gsub(",", ".", grasacerdos$PV)
grasacerdos$EGD <- gsub(",",".",grasacerdos$EGD)

head(grasacerdos)

grasacerdos$PV<- as.numeric(grasacerdos$PV)
grasacerdos$EGD <- as.numeric(grasacerdos$EGD)
str(grasacerdos)
head(grasacerdos)

plot1 <- ggplot(grasacerdos, aes(x = Obs, y = PV)) +
  geom_point(shape = 16, size = 3, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(x = "Cerdos", y = "Peso Vivo (kg)", title = "Dispersion Plot 1")

plot2 <- ggplot(grasacerdos, aes(x = Obs, y = EGD)) +
  geom_point(shape = 16, size = 3, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(x = "Cerdos", y = "Espesor Grasa Dorsal (mm)", title = "Dispersion Plot 2")

# (a) Dibujar el diagrama de dispersión e interpretarlo.

plot3 <- ggplot(grasacerdos, aes(x = PV, y = EGD)) +
  geom_point(shape = 16, size = 3, color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(x = "Peso Vivo (kg)", y = "Espesor Grasa Dorsal (mm)", title = "Dispersion Plot 3")
plot3
# Interpretacion:
# Plot_3: Hay una leve correlacion positiva entre PV y EGD, es decir, 
# cuando aumenta el peso del animal, aumenta el espesor de la grasa dorsal

# (b) Calcular el coeficiente de correlación muestral y explíquelo.

cor(grasacerdos$PV,grasacerdos$EGD)

# Interpretacion: Un valor de 0.25 en una escala de -1 a 1 indica una correlacion levemente positiva entre ambas variables. 

# (c) ¿Hay suficiente evidencia para admitir asociación entre el peso y el espesor de grasa? (α = 0,05).
# Verifique los supuestos para decidir el indicador que va a utilizar.

# Supuestos para la validez de la inferencia
# a) Los n pares han sido seleccionados aleatoriamente.
# b) Los pares de observaciones de la muestra son independientes.
# c) Ambas variables X y Y tienen distribucióon conjunta normal bivariada.

TestHenzeZirkler <- mvn(grasacerdos,mvnTest = "hz")
TestHenzeZirkler
TestHenzeZirkler$multivariateNormality

# Test HZ: Se usa para testear la normalidad multivariada. Es para saber si las variables siguen una distribucion normal multivariada.
# HZ statistic: 0.637. Este estadistico mide la desviacion de la normalidad multivariada. Valores grandes indicar una mayor desviacion.
# p value: 0.389. Indica que no hay evidencia suficiente para rechazar la hipotesis nula de normalidad multivariada.
# MVN: YES. Indica si los datos siguen una distribucion multivariada. En este caso es correcto


cor.test(grasacerdos$PV,grasacerdos$EGD)


# Interpretacion: El test lo que intenta hacer es saber si hay una correlacion significativa entre ambas variables. Un p-value de 0.175 >= 0.05 --> NO SE RECHAZA Ho. No hay evidencia suficiente para rechazar la hipotesis nula de que la correlacion verdadera es igual a 0.
# Alternative hyphotesis: La hipotesis alternativa indica que la verdadera correlacion no es igual 0
# 95% CI: El intervalo de confianza provee un rango de valores donde el verdadera correlacion es probable que caiga con un 95% de confianza. 
# Este valor esta entre -0.11 y 0.56.
# Sample estimates: cor = 0.254. Esto indica una correlacion positiva que es debil ya que esta cercano al 0.
# Conclusion final: No hay evidencias suficientes para concluir que hay una correlacion significativa entre las variables PV y EGD. Aunque sea positiva la correlacion es muy leve.

# 1.2

# Se pide explorar los datos de la siguiente manera:
# (a) Graficar los cuatro pares de datos en un diagrama de dispersión cada uno.

anscombe <- read_excel("anscombe.xlsx")
anscombe

plot4 <- ggplot(anscombe, aes(x = x1, y = y1)) +
  geom_point(shape = 16, size = 3, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(x = "x1", y = "y1", title = "Dispersion Plot 1")
plot4

plot5 <- ggplot(anscombe, aes(x = x2, y = y2)) +
  geom_point(shape = 16, size = 3, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(x = "x2", y = "y2", title = "Dispersion Plot 2")
plot5

plot6 <- ggplot(anscombe, aes(x = x3, y = y3)) +
  geom_point(shape = 16, size = 3, color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(x = "x3", y = "y3", title = "Dispersion Plot 3")
plot6

plot7 <- ggplot(anscombe, aes(x = x4, y = y4)) +
  geom_point(shape = 16, size = 3, color = "orange") +
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(x = "x4", y = "y4", title = "Dispersion Plot 4")
plot7

grid.arrange(plot4,plot5,plot6,plot7, ncol = 4)                                              

# (b) Hallar los valores medios de las variables para cada par de datos.
mean(anscombe$x1)
mean(anscombe$y1)

mean(anscombe$x2)
mean(anscombe$y2)

mean(anscombe$x3)
mean(anscombe$y3)

mean(anscombe$x4)
mean(anscombe$y4)

describe(anscombe)
# (c) Hallar los valores de la dispersión para cada conjunto de datos.
summary(anscombe)


# (d) Hallar el coeficiente muestral de correlación lineal en cada caso
cor(anscombe$x1,anscombe$y1)

cor(anscombe$x2,anscombe$y2)

cor(anscombe$x3,anscombe$y3)

cor(anscombe$x4,anscombe$y4)

# (e) Observar, comentar y concluir.

# Se observa que la media y desvio estandar para cada par de datos es el mismo, al igual que el coeficiente de correlacion muestral.
# Se concluye lo importante que es observar graficamente al set de datos antes de analizarlos.

# Modelo Lineal Simple

# 1.3

peso_edad_colest <- read_excel('peso_edad_colest.xlsx')
peso_edad_colest

# (a) Realizar el diagrama de dispersión de colesterol en función de la edad y de colesterol en función de peso.
#     Le parece adecuado ajustar un modelo lineal para alguno de estos dos pares de variables?

plot8 <- ggplot(peso_edad_colest, aes(x = edad, y = peso)) +
  geom_point(shape = 16, size = 3, color = "cyan") +
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(x = "edad", y = "peso", title = "Dispersion Plot Edad-Peso")
plot8

plot9 <- ggplot(peso_edad_colest, aes(x = edad, y = colest)) +
  geom_point(shape = 16, size = 3, color = "magenta") +
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(x = "edad", y = "colesteros", title = "Dispersion Plot Edad-Colesterol")
plot9

grid.arrange(plot8,plot9,ncol = 2)

# Parece adecuado ajustar un modelo lineal para el par Edad - Colesterol debido a su alta correlacion.

# (b) Estime los coeficientes del modelo lineal para el colesterol en función de la edad.

modelo <- lm(colest ~ edad,data=peso_edad_colest)
modelo
coef(modelo)

# Se observa una fuerte correlacion lineal entre la edad y el colesterol

# (c) Estime intervalos de confianza del 95 % para los coeficientes del modelo
#     y compare estos resultados con el test de Wald para los coeficientes. 
#     Le parece que hay asociación entre estos test y el test de la regresión?
 
IC <- confint(modelo,level=0.95)
IC

wald_test <- wald.test(b = coef(modelo), Sigma = vcov(modelo), Terms = 2)
wald_test 

# El test de Wald testea la significancia de los coeficientes indivuales de un modelo
# El p-value es 0.0. Esto indica que hay una fuerte evidencia para rechazar la hipotesis nula (el coeficiente es igual a 0)
# En otras palabras, la variable testeada tiene un impacto significativo en la variable respuesta.

#Test de la Regresion 
anova <- anova(modelo)
anova

# Este test evalua la significancia total del modelo de regresion comparando la variacion explicada por la variacion residual.
# Se concluye que la edad impacta significativamente en el colesterol. 

# El F-value es un estadistico en ANOVA y representa el ratio entre Mean Square del modelo y el mean square de los residuos
# Cuantifica la extension en el que el modelo provee una mejora significativa frente a un modelo nulo sin predictores
# Valores altos de F indican una relacion mas signficativa enre los predictores y la variable respuesta.
# En este caso el F-value en el test de wald y ANOVA es el mismo. 

# (d) A partir de esta recta estime los valores de E(Y ) para x = 25 años y x = 48 años. 
#     Podría estimarse el valor de E(Y ) para x = 80 años?

edad_25 <- data.frame(edad=25)
predict(modelo,newdata=edad_25)

edad_48 <- data.frame(edad=48)
predict(modelo,newdata=edad_48)

edad_80 <- data.frame(edad=80)
predict(modelo,newdata=edad_80)

# (e) Testee la normalidad de los residuos y haga un gráfico para ver si son homocedásticos.

residuos <- resid(modelo)
residuos

# Hay varios tests para validar la normalidad de los residuos

# Test de Shapiro-Wilk
shapiro_test <- shapiro.test(residuos)
shapiro_test

# Interpretacion: P-value: 0.5 >= 0.05 --> No se rechaza Ho (Los residuos siguen una distribucion normal)

# Test de Kolmogorov-Smirnov
ks_test <- ks.test(residuos,"pnorm")
ks_test

# Interpretacion: P-value: 1.17e-07 <= 0.05 --> Se rechaza Ho (Los residuos no siguen una distribucion normal)

# Test de Lilliefors
lillie_test <- lillie.test(residuos)
lillie_test

# Interpretacion: P-value: 0.17 >= 0.05 --> No se rechaza Ho (Los residuos siguen una distribucion normal)

# Test de Anderson-Darling

anderson_test <- ad.test(residuos)
anderson_test

# Interpretacion: P-value: 0.045 >= 0.05 --> No se rechaza Ho (Los residuos siguen una distribucion normal)

# Segun los resultados de todos los tests no hay suficiente evidencia para rechazar la Ho salvo para el Ks_test.

# Test grafico para ver normalidad de residos (Q-Q Plot)

qqnorm(residuos)
qqline(residuos)

# Si los puntos siguen la linea de referencia quiere decir que los residuos estan normalmente distribuidos


# Grafico para ver homocedasticidad
valores_ajustados <- fitted(modelo)
plot(valores_ajustados,residuos,xlab = "Valores Ajustados", ylab = "Residuos", main = "Gráfico de Residuos")

abline(h=0,col="red")

# Se ve en el grafico que hay homocedasticidad. Los puntos estan dispersos aleatoriamente alrededor de la linea horizontal en 0.
# Esto indica que los residuos tienen una distribucion homogenea y no hay relacion sistematico entre los valores ajustados y los residuos.

# Transformacion de Variables

# 1.4 

energia <- read_excel('energia.xlsx')
energia

# (a) Realizar el diagrama de dispersión y evaluar si un modelo de regresión lineal es adecuado.

plot10 <- ggplot(energia, aes(x = Hora, y = Energía)) +
  geom_point(shape = 16, size = 3, color = "salmon") +
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(x = "Hora", y = "Enegia", title = "Dispersion Plot Hora-Energia")
plot10

# (b) Estimar un modelo lineal y verificar la normalidad de los residuos del mismo.

modelo2 <- lm(Energía ~ Hora,data=energia)
modelo2
coef(modelo2)

residuos2 <- resid(modelo2)
residuos2

# Test de Wald - Coeficientes 

IC2 <- confint(modelo2,level=0.95)
IC2

wald_test_2 <- wald.test(b = coef(modelo2), Sigma = vcov(modelo2), Terms = 2)
wald_test_2

# Interpretacion: No hay suficiente evidencia para rechazar Ho (de que el coeficiente es igual a 0)

# Test de Regresion

anova(modelo2)

# Interpretacion: No hay suficiente evidencia para rechazar Ho (de que hay un efecto significatientre Hora y Energia)

# Test de normalidad de residuos

# Test de Shapiro-Wilk
shapiro_test <- shapiro.test(residuos2)
shapiro_test

# Interpretacion: P-value: 0.006 <= 0.05 --> Se rechaza Ho (Los residuos no siguen una distribucion normal)

# Test de Kolmogorov-Smirnov
ks_test <- ks.test(residuos2,"pnorm")
ks_test

# Interpretacion: P-value: 1.029e-13 <= 0.05 --> Se rechaza Ho (Los residuos no siguen una distribucion normal)

# Test de Lilliefors
lillie_test <- lillie.test(residuos2)
lillie_test

# Interpretacion: P-value: 0.1243 >= 0.05 --> No se rechaza Ho (Los residuos siguen una distribucion normal)

# Test de Anderson-Darling

anderson_test <- ad.test(residuos2)
anderson_test

# Interpretacion: P-value: 0.016 >= 0.05 --> Se rechaza Ho (Los residuos no siguen una distribucion normal)

# Test Q-Q 

qqnorm(residuos2)
qqline(residuos2)

# Los puntos no siguen la liena al final del plot.

# Test de Homocedasticidad 

valores_ajustados2 <- fitted(modelo2)
plot(valores_ajustados2,residuos2,xlab = "Valores Ajustados", ylab = "Residuos", main = "Gráfico de Residuos")

abline(h=0,col="blue")


# Interpretacion: Se ve una leve heterocedasticidad.

# Test de Breush-Pagan --> para ver homocedasticidad

bptest(modelo2)

# Interpretacion: P-value: 0.067 >=0.05 No hay suf evidencia para rechazar la Ho de homocedasticidad.
# No se puede concluir que la homocedasticidad no esta presente en el modelo.

# (c) En caso de rechazar este supuesto buscar una transformación lineal para este modelo y aplicarla.

# Transformaciones Lineales: Logaritmica log() , reciproca (1/x) o raiz cuadrada sqrt()
# Se usa logartimica

energia$Hora_log <- log(energia$Hora)

modelo2_transformado <- lm(Energía ~ Hora_log, data=energia)
modelo2_transformado

coef(modelo2_transformado)

residuos2_transformado <- resid(modelo2_transformado)
residuos2_transformado

# (d) Realizar el análisis diagnóstico del nuevo modelo y estimar un intervalo de confianza y 
#     un intervalo de predicción para 27.5 hs con ambos modelos. Comparar los intervalos.

# Test de Wald - Coeficientes 

IC2 <- confint(modelo2_transformado,level=0.95)
IC2

wald_test_2_transf <- wald.test(b = coef(modelo2_transformado), Sigma = vcov(modelo2_transformado), Terms = 2)
wald_test_2_transf

# Interpretacion: No hay suficiente evidencia para rechazar Ho (de que el coeficiente es igual a 0)

# Test de Regresion

anova(modelo2_transformado)

# Interpretacion: No hay suficiente evidencia para rechazar Ho (de que hay un efecto significativo entre Hora_log y Energia)

# Test de normalidad de residuos

# Test de Shapiro-Wilk
shapiro_test <- shapiro.test(residuos2_transformado)
shapiro_test

# Interpretacion: P-value: 0.001 <= 0.05 --> Se rechaza Ho (Los residuos no siguen una distribucion normal)

# Test de Kolmogorov-Smirnov
ks_test <- ks.test(residuos2_transformado,"pnorm")
ks_test

# Interpretacion: P-value: 8.771e-15 <= 0.05 --> Se rechaza Ho (Los residuos no siguen una distribucion normal)

# Test de Lilliefors
lillie_test <- lillie.test(residuos2_transformado)
lillie_test

# Interpretacion: P-value: 0.026 >= 0.05 --> Se rechaza Ho (Los residuos no siguen una distribucion normal)

# Test de Anderson-Darling

anderson_test <- ad.test(residuos2_transformado)
anderson_test

# Interpretacion: P-value: 0.003 >= 0.05 --> Se rechaza Ho (Los residuos no siguen una distribucion normal)

# Test Q-Q 

qqnorm(residuos2_transformado)
qqline(residuos2_transformado)

# Los puntos no siguen la liena al final del plot.

# Test de Homocedasticidad 

valores_ajustados3 <- fitted(modelo2_transformado)
plot(valores_ajustados3,residuos2_transformado,xlab = "Valores Ajustados", ylab = "Residuos", main = "Gráfico de Residuos")

abline(h=0,col="blue")


# Interpretacion: Se ve una leve heterocedasticidad.

# Test de Breush-Pagan --> para ver homocedasticidad

bptest(modelo2_transformado)

# Interpretacion: P-value: 0.084 >=0.05 No hay suf evidencia para rechazar la Ho de homocedasticidad.
# No se puede concluir que la homocedasticidad no esta presente en el modelo.

# Estimar el intervalo de confianza para 27.5 horas con el modelo original
intervalo_confianza <- predict(modelo2, newdata = data.frame(Hora= 27.5), interval = "confidence", level = 0.95)
intervalo_confianza

# Estimar el intervalo de predicción para 27.5 horas con el modelo original
intervalo_prediccion <- predict(modelo2, newdata = data.frame(Hora = 27.5), interval = "prediction", level = 0.95)
intervalo_prediccion

# Estimar el intervalo de confianza para 27.5 horas con el modelo transformado
intervalo_confianza_transformado <- predict(modelo2_transformado, newdata = data.frame(Hora_log = log(27.5)), interval = "confidence", level = 0.95)
intervalo_confianza_transformado

# Estimar el intervalo de predicción para 27.5 horas con el modelo transformado
intervalo_prediccion_transformado <- predict(modelo2_transformado, newdata = data.frame(Hora_log = log(27.5)), interval = "prediction", level = 0.95)
intervalo_prediccion_transformado

# TRATAMIENTO DE HETEROCEDASTICIDAD

# 1.5

inmobiliaria <- read.csv('inmobiliaria.csv',sep=";")
inmobiliaria

# (a) Analizar si el precio depende de alguna de las variables.
str(inmobiliaria)
summary(inmobiliaria)

plot10 <- ggplot(inmobiliaria, aes(x = distancia, y = precio)) +
  geom_point(shape = 16, size = 3, color = "pink") +
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(x = "edad", y = "precio", title = "Dispersion Plot Edad-Precio")
plot10

modelo3 <- lm(precio  ~ distancia , data=inmobiliaria )
modelo3
summary(modelo3)
coef(modelo3)
# (b) Estudiar la linealidad de la relación precio-distancia.
cor(inmobiliaria$distancia,inmobiliaria$precio)
cor.test(inmobiliaria$distancia,inmobiliaria$precio)
# Interpretacion: P-value < 0.05, hay evidencias muy fuerte para rechazar la Ho de que el coeficiente de correlacion es igual a 0.
# Es decir, hay una correlacion significativa entre las variables

# (c) Estimar los coeficientes del modelo y realizar el análisis diagnóstico de
# los residuos del mismo. Utilizar para este análisis los gráficos de residuos
# versus valores ajustados, el qq-plot de los residuos, la grafica de residuos
# versus leverage
coef(modelo3)

plot(resid(modelo3))

# Grafico de residuos vs valores ajustados: permite verificar si hay patron en los residuos
plot(modelo3$fitted.values, modelo3$residuals, xlab = "Valores ajustados", ylab = "Residuos")
# Interpretacion: 

# QQ-plot
qqnorm(modelo3$residuals)
qqline(modelo3$residuals)

residuos3 <- residuals(modelo3)
plot(density(residuos3),main = "Distribution of Residuals", xlab = "Residuals")

# Interpretacion: se ve una desviacion de la normalidad de los residuos. Los residuos tienen una distribucion asimetrica hacia la derecha.

# Plot de Residuos vs leverage
plot(modelo3,which=5) #which=5 indica que se desea graficar residuos vs leverage


# (d) Aplicar los test de Durbin-Watson Breush-Pagan.
# (e) Analice la presencia de outlier y verifique si coinciden con los puntos
# influyentes.

##sjfkhdjkfashdjkfdf




