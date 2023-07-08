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
library(corrplot)
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
#library(chatgpt)
#Sys.setenv(OPENAI_API_KEY = "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")
library(aod) #Para test de wald
library(nortest) # Para test de Shapiro y demas 
library(lmtest)
library(car)
library(mgcv)
library(stats)
library(olsrr)
library(scatterplot3d)
library(Metrics)
library(robustbase)
library(Brq)

setwd("C:/Users/idott/Advanced_Regression/Advanced_Regression/TP 1")
# Capitulo 2. MODELO LINEAL MULTIVARIADO

# MODELO ADITIVO

# 2.1 

trees <-  read_excel("trees.xlsx")
trees

summary(trees)
str(trees)
# a) Visualizar la asociación entre las variables de a pares.

plot(trees)

correlation_plot <- cor(trees[, c("Girth", "Volume", "Height")])
corrplot::corrplot(correlation_plot, method = "circle")

# b) Ajuste un modelo lineal simple para cada una de las dos predictoras disponibles.

modelo_vg <- lm(Volume ~ Girth, data = trees)
summary(modelo_vg)

# Interpretacion: El modelo lineal simple para la variable Volume en funcion de Girth es significativo, ya que el p-valor es menor a 0.05. El coeficiente de Girth es 4.71, lo que significa que por cada unidad que aumenta Girth, Volume aumenta 4.71 unidades. El R2 es 0.92, lo que significa que el modelo explica el 95% de la variabilidad de Volume. El modelo es significativo, ya que el p-valor es menor a 0.05.

# Ajuste del modelo lineal para Altura
modelo_va <- lm(Volume ~ Height, data = trees)
summary(modelo_va)

# Interpretacion: El modelo lineal simple para la variable Volume en funcion de Height es significativo, ya que el p-valor es menor a 0.05. El coeficiente de Height es 0.34, lo que significa que por cada unidad que aumenta Height, Volume aumenta 0.34 unidades. El R2 es 0.12, lo que significa que el modelo explica el 12% de la variabilidad de Volume. El modelo es significativo, ya que el p-valor es menor a 0.05.

# c) Realice un análisis diagnóstico en cada caso y señale en caso de haberlos
#    puntos influyentes y outliers.

par(mfrow = c(2, 2))
plot(modelo_vg)

outlierTest(modelo_vg)
influenceIndexPlot(modelo_vg, vars='Bonf', las=1,col='blue')
influenceIndexPlot(modelo_vg, vars='Cook', las=1,col='blue')

par(mfrow = c(2, 2))
plot(modelo_va)

outlierTest(modelo_va)
influenceIndexPlot(modelo_va, vars='Bonf', las=1,col='blue')
influenceIndexPlot(modelo_va, vars='Cook', las=1,col='blue')

# d) Estime un intervalo de confianza para los coeficientes del modelo lineal estimado en cada caso.



# Intervalo de confianza para el modelo de Circunferencia
confint(modelo_vg)

# Intervalo de confianza para el modelo de Altura
confint(modelo_va)

# e) Ajuste un nuevo modelo sin la/s observaciones influyentes.

# Identificar puntos influyentes para el modelo de circunferencia
influencia_vg <- influence.measures(modelo_vg)
puntos_influyentes_vg <- which(influencia_vg$hat > 2 * mean(influencia_vg$hat))

# Ajustar nuevo modelo de circunferencia sin los puntos influyentes
nuevo_modelo_vg <- lm(Volume ~ Girth, data = trees[-puntos_influyentes_vg, ])
summary(nuevo_modelo_vg)

# Identificar puntos influyentes para el modelo de altura
influencia_va <- influence.measures(modelo_va)
puntos_influyentes_va <- which(influencia_va$hat > 2 * mean(influencia_va$hat))

# Ajustar nuevo modelo de altura sin los puntos influyentes
nuevo_modelo_va <- lm(Volume ~ Height, data = trees[-puntos_influyentes_va, ])
summary(nuevo_modelo_vs)



# f) Construya el intervalo de confianza y el de predicción del 95 % para un
#    árbol cuyo diámetro es 16.1 pulgadas.

diametro <- 16.1

# Intervalo de confianza
intervalo_confianza <- predict(nuevo_modelo_circunferencia, newdata = data.frame(Girth = diametro), interval = "confidence", level = 0.95)
intervalo_confianza

# Intervalo de predicción
intervalo_prediccion <- predict(nuevo_modelo_circunferencia, newdata = data.frame(Girth = diametro), interval = "prediction", level = 0.95)
intervalo_prediccion

# g) Ajuste un modelo utilizando conjuntamente las dos variables predictoras
# y compare este ajuste con el mejor de los modelos anteriores mediante un
# test de modelos anidados. Concluya.

ma_1<- lm(Volume ~ Girth + Height, data = trees)
anova(modelo_circunferencia, nuevo_modelo_circunferencia, modelo_completo)

summary(ma_1)
pred_data <- with(trees, expand.grid(Girth = seq(min(Girth), max(Girth), length.out = 100),
                                     Height = seq(min(Height), max(Height), length.out = 100)))
pred_data$Volume <- predict(ma_1, newdata = pred_data)

ggplot(trees, aes(x = Girth, y = Volume)) +
  geom_point() +
  geom_line(data = pred_data, aes(y = Volume), color = "red") +
  labs(x = "Girth", y = "Volume") +
  ggtitle("Additive Model: Volume ~ Girth + Height")

#### FALTA TERMINAR ESTE EJERCICIO


# MODELO CON INTERACCION

# 2.2

publicidad <- read_excel("publicidad.xlsx")
publicidad

str(publicidad)
summary(publicidad)
plot(publicidad)

# a) Ajustar un modelo de regresión lineal simple para cada una de las variables 
# predictoras por separado. Realizar a continuación el análisis diagnóstico de los modelos.

modelo_tv <- lm(ventas ~ tv, data = publicidad)
plot(modelo_tv)

plot1 <- ggplot(publicidad, aes(x = tv, y = ventas)) +
  geom_point(shape = 16, size = 3, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(x = "TV", y = "Ventas", title = "Dispersion Plot 1")
plot1

modelo_radio <- lm(ventas ~ radio, data = publicidad)
plot(modelo_radio)
plot2 <- ggplot(publicidad, aes(x = radio, y = ventas)) +
  geom_point(shape = 16, size = 3, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(x = "Radio", y = "Ventas", title = "Dispersion Plot 2")
plot2

modelo_periodico <- lm(ventas ~ periodico, data = publicidad)
plot(modelo_periodico)

plot3 <- ggplot(publicidad, aes(x = periodico, y = ventas)) +
  geom_point(shape = 16, size = 3, color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(x = "Periodicos", y = "Ventas", title = "Dispersion Plot 3")
plot3  

# Supuestos de un modelo

# Supuesto 1: Linealidad: Relacion lineal entre variable independiente X y la variable dependiente Y.

# Prueba Grafica: Correlacion
plot(publicidad$tv,publicidad$ventas)
plot(publicidad$radio,publicidad$ventas)
plot(publicidad$periodico,publicidad$ventas)

# Prueba analitica: Correlacion
cor(publicidad$tv,publicidad$ventas)
cor(publicidad$radio,publicidad$ventas)
cor(publicidad$periodico,publicidad$ventas)

# Interpretacion: Se visualiza correlacion alta para tv, media para radio y baja para periodico.

# Supuesto 2: Independencia de los residuos: no hay correlacion entre los residuos consecutivos.

# Prueba Grafica: Residuals vs Index (Orden)

ggplot(data = publicidad, aes(x = seq_along(modelo_tv$residuals), y = modelo_tv$residuals)) + 
  geom_point(aes(color = modelo_tv$residuals)) + 
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") + 
  geom_line(size = 0.3) + labs(title = "Distribución de los residuos", x = "index", y = "residuo")+ 
  geom_hline(yintercept = 0) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

ggplot(data = publicidad, aes(x = seq_along(modelo_radio$residuals), y = modelo_radio$residuals)) + 
  geom_point(aes(color = modelo_radio$residuals)) + 
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") + 
  geom_line(size = 0.3) + labs(title = "Distribución de los residuos", x = "index", y = "residuo")+ 
  geom_hline(yintercept = 0) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

ggplot(data = publicidad, aes(x = seq_along(modelo_periodico$residuals), y = modelo_periodico$residuals)) + 
  geom_point(aes(color = modelo_periodico$residuals)) + 
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") + 
  geom_line(size = 0.3) + labs(title = "Distribución de los residuos", x = "index", y = "residuo")+ 
  geom_hline(yintercept = 0) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Interpretacion: Para el plot de tv  no se ve un patron definido en los residuos, por lo que se puede decir que son independientes. 
# Para el plot de radio tampoco se ve un patron definido en los residuos, pero se ve claramente un outlier por lo que puede representar un punto influyente. 
# Para el plot de periodico no se ve un patron definido en los residuos, por lo que se puede decir que son independientes.

# Prueba analitica: Durbin-Watson
dwtest(modelo_tv, alternative="two.sided",iterations=1000)
dwt(modelo_tv)

dwtest(modelo_radio, alternative="two.sided",iterations=1000)
dwt(modelo_radio)

dwtest(modelo_periodico, alternative="two.sided",iterations=1000)
dwt(modelo_periodico)
# Interpretacion: Para TV, radio y periodico se muestra el valor DW es cercano a 2 por lo que sugiere que la autocorrelacion es debil y el p-value = 0.64 <= 0.05 por lo que se rechaza la hipotesis nula de que no hay autocorrelacion. Es decir, no hay autocorrelacion significativa en los residuos del modelo.

# Supuesto 3: Homocedasticidad de los residuos: Los residuos tienen la misma varianza para todos los valores de X.

# Prueba Grafica: Scale-Location
plot(modelo_tv,which=3)

plot(modelo_radio,which=3)

plot(modelo_periodico,which=3)

# Interpretacion: Para TV se ven que los datos forman un embudo, por lo que puede indicar presencia de heteroceadsticadad. Para radio y periodico los puntos se distribuyen aleatoriamente y forman una nube mas o menos homogenea. Pero la linea roja en las 3 graficas tiene una leve inclinacion hacia arriba

# Otro Test Grafico: Residuals vs Fitted
prediccion <- modelo_tv$fitted.values
residuos_est  <- modelo_tv$residuals 

ggplot(data = publicidad, aes(x = prediccion, y = residuos_est )) + 
  geom_point(aes(color = residuos_est)) + 
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") + 
  geom_hline(yintercept = 0) + geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) + 
  labs(title = "Distribución de los residuos", x = "predicción modelo", y = "residuo") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Se ve mas claro la presencia de heterocedasticidad aqui

prediccion2 <- modelo_radio$fitted.values
residuos_est2  <- modelo_radio$residuals 

ggplot(data = publicidad, aes(x = prediccion2, y = residuos_est2 )) + 
  geom_point(aes(color = residuos_est2)) + 
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") + 
  geom_hline(yintercept = 0) + geom_segment(aes(xend = prediccion2, yend = 0), alpha = 0.2) + 
  labs(title = "Distribución de los residuos", x = "predicción modelo", y = "residuo") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Se ve pero mas leve que en TV

prediccion3 <- modelo_periodico$fitted.values
residuos_est3  <- modelo_periodico$residuals 

ggplot(data = publicidad, aes(x = prediccion3, y = residuos_est3 )) + 
  geom_point(aes(color = residuos_est3)) + 
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") + 
  geom_hline(yintercept = 0) + geom_segment(aes(xend = prediccion3, yend = 0), alpha = 0.2) + 
  labs(title = "Distribución de los residuos", x = "predicción modelo", y = "residuo") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# No se ve presencia de heterocedasticidad.

# Prueba analitica: Breush-Pagan o White, Goldfeld-Quandt
bptest(modelo_tv)
bptest(modelo_radio)
bptest(modelo_periodico)
# Interpretacion: Para TV, el p-value es muy pequeño por lo que hay fuerte evidencia para rechazar hipotesis nula de NO heterocedadisticidad. Para radio y periodico, el p-value es mayor a 0.05 por lo que no hay evidencia para rechazar hipotesis nula de NO heterocedadisticidad. Es decir, hay presencia de heterocedasticidad en los residuos.
# Para radio p-value es mayor que en TV pero sigue siendo <= 0.05 por lo que hay evidencia de heterocedasticidad.
# Para periodico el p-value es de 0.11 >= 0.05. Hay evidencias suficientes para NO rechazar la hipotesis nula de no heterocedasticidad.

gqtest(modelo_tv)
gqtest(modelo_radio)
gqtest(modelo_periodico)

# Interpretacion: Los p-values son mayores a 0.05 por lo que muestran que hay evidencias suficientes para no rechazar la hipotesis nula de heterocedasticidad. Esto esta en conflicto con lo que se ve en las graficas y en el test de BP.

# Supuesto 4: Normalidad de los residuos: Los residuos estan normalmente distribuidos distribuidos.

# Prueba Grafica: Q-Q Plot
plot(modelo_tv,which=2)
plot(modelo_radio,which=2)
plot(modelo_periodico,which=2)

# Interpretacion: En TV y periodico se ve que el QQ plot los puntos siguen la recta diagonal. Por lo que indican que los residuos siguen una distribucion normal. En radio se ve que los puntos se alejan de la recta diagonal, por lo que no siguen una distribucion normal.

# Otra Prueba grafica: Histograma
ggplot(data = publicidad, aes(x = residuos_est)) + geom_histogram(aes(y = ..density..)) + 
  labs(title = "Histograma de los residuos") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = publicidad, aes(x = residuos_est2)) + geom_histogram(aes(y = ..density..)) + 
  labs(title = "Histograma de los residuos") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))  

ggplot(data = publicidad, aes(x = residuos_est3)) + geom_histogram(aes(y = ..density..)) + 
  labs(title = "Histograma de los residuos") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# Prueba analitica: Shapiro-Wilk, Anderson-Darling, Lillie
shapiro.test(modelo_tv$residuals)
shapiro.test(modelo_radio$residuals)
shapiro.test(modelo_periodico$residuals)

# Interpretacion: Para TV el p-value es mayor a 0.05 por lo que no hay evidencia para rechazar la hipotesis nula de normalidad. Para radio y periodico el p-value es menor a 0.05 por lo que hay evidencia para rechazar la hipotesis nula de normalidad. Es decir, los residuos de TV siguen una distribucion normal, pero los de radio y periodico no.

ad.test(modelo_tv$residuals)
ad.test(modelo_radio$residuals)
ad.test(modelo_periodico$residuals)

# Interpretacion: Muestra lo mismo que el Test de Shapiro-Wilk

lillie.test(modelo_tv$residuals)
lillie.test(modelo_radio$residuals)
lillie.test(modelo_periodico$residuals)

# Interpretacion: Muestra lo mismo que los test anteriores.
# Conclusion: Segun los resultados de las pruebas de normalidad, la dist de los resiudos no sigue una distribucion normal de manera estrica.

# Analisis de outliers y puntos influyentes

par(mfrow = c(2, 2))
plot(modelo_tv)

outlierTest(modelo_tv)
influenceIndexPlot(modelo_tv, vars='Bonf', las=1,col='blue')
influenceIndexPlot(modelo_tv, vars='Cook', las=1,col='blue')

par(mfrow = c(2, 2))
plot(modelo_radio)

outlierTest(modelo_radio)
influenceIndexPlot(modelo_radio, vars='Bonf', las=1,col='blue')
influenceIndexPlot(modelo_radio, vars='Cook', las=1,col='blue')

par(mfrow = c(2, 2))
plot(modelo_periodico)

outlierTest(modelo_periodico)
influenceIndexPlot(modelo_periodico, vars='Bonf', las=1,col='blue')
influenceIndexPlot(modelo_periodico, vars='Cook', las=1,col='blue')

# b) Ajustar un modelo aditivo con las tres variables y decidir si alguna de
#    ellas no es significativa (test de Wald).

modelo_medios <- lm(ventas ~ tv + radio + periodico, data = publicidad)
plot(modelo_medios)
summary(modelo_medios)

# Otro metodo --> GAM
modelo_medios_2 <- gam(ventas ~ tv + radio + periodico, data = publicidad)
modelo_medios_2
summary(modelo_medios_2)

# Test de Wald 

wald.test(b = coef(modelo_medios_2), Sigma = vcov(modelo_medios_2), Terms = 3)

wald_test <- anova(modelo_medios, test = "Chisq")
wald_test

# Interpretacion: Visualizando el p-value de tv y radio que son 2e-16 <= 0.05 denota una significancia estadistica alta, entre los pares ventas - tv y radio - tv. 
# En periodico el p-value = 0.86 >= 0.05 es decir, no podemos rechazar la Ho de que el coeficiente de las variables es igual a 0.
# Es decir, tv y radio son significativas, pero periodico no.

# c) Ajustar los modelos de a pares y quedarse con el que mejor explique la
#    variable respuesta utilizando el criterio de AIC, R2 y Cp_Mallows.



# Ajustar el modelo 1: tv + radio
modelo_tvradio <- lm(ventas ~ tv + radio, data = publicidad)

# Ajustar el modelo 2: tv + periodico
modelo_tvperiodico <- lm(ventas ~ tv + periodico, data = publicidad)

# Ajustar el modelo 3: radio + periodico
modelo_radioperiodico <- lm(ventas ~ radio + periodico, data = publicidad)

# Criterio de Informaciona de Akaike (AIC)  --> Cuanto menor sea el valor de este estadıstico menor es el error de
# ajuste del modelo.

# AIC: El AIC es un criterio de selección de modelos que penaliza la complejidad del modelo.
# Un valor más bajo de AIC indica un mejor ajuste del modelo. El AIC permite seleccionar el modelo
# que equilibra adecuadamente la bondad de ajuste y la parsimonia.

AIC1 <- AIC(modelo_tvradio)
AIC2 <- AIC(modelo_tvperiodico)
AIC3 <- AIC(modelo_radioperiodico)
AIC1
AIC2
AIC3
# El que tiene menor error de ajuste de modelo es el modelo de tv + radio.

# R2 Ajustado --> Valor alto indica un valor bajo de error. En este caso tv+radio es el mejor modelo.

# R^2 ajustado: El R^2 ajustado es una medida de la proporción de variabilidad de la variable 
# respuesta que es explicada por el modelo, teniendo en cuenta el número de variables predictoras
# y el tamaño de la muestra. Un valor más alto de R^2 ajustado indica un mejor ajuste del modelo.

R2_ajustado1 <- summary(modelo_tvradio)$adj.r.squared
R2_ajustado2 <- summary(modelo_tvperiodico)$adj.r.squared
R2_ajustado3 <- summary(modelo_radioperiodico)$adj.r.squared

R2_ajustado1
R2_ajustado2
R2_ajustado3

# Cp Mallows --> Mejor modelo el que tenga menor Cp

# Cp de Mallows: El Cp de Mallows es un criterio utilizado en la regresión lineal para evaluar 
# modelos con múltiples variables predictoras. Mide la calidad del ajuste del modelo y 
# penaliza la sobreajuste. Un valor de Cp de Mallows cercano a p (número de variables predictoras)
# indica un buen ajuste del modelo.

n <- nrow(publicidad)
p1 <- length(coefficients(modelo_tvradio))
Cp1 <- sum((residuals(modelo_tvradio) / sqrt(1 - R2_ajustado1))^2) - (n - 2 * p1)
p2 <- length(coefficients(modelo_tvperiodico))
Cp2 <- sum((residuals(modelo_tvperiodico) / sqrt(1 - R2_ajustado2))^2) - (n - 2 * p2)
p3 <- length(coefficients(modelo_radioperiodico))
Cp3 <- sum((residuals(modelo_radioperiodico) / sqrt(1 - R2_ajustado3))^2) - (n - 2 * p3)
Cp1
Cp2
Cp3

valores <- c(AIC1, AIC2, AIC3, R2_ajustado1, R2_ajustado2, R2_ajustado3, Cp1, Cp2, Cp3)
mejor_modelo <- which.min(valores)
mejor_modelo

modelo_todos <- lm(ventas ~ ., data = publicidad)

k_best <- ols_step_best_subset(modelo_todos)
k_best
# d) Grafique para el modelo seleccionado el plano de ajuste y evalue si le parece adecuado.

grid <- expand.grid(tv = seq(min(publicidad$tv), max(publicidad$tv), length.out = 100),
                    radio = seq(min(publicidad$radio), max(publicidad$radio), length.out = 100))

grid$ventas <- predict(modelo_tvradio, newdata = grid)


# Grafica el plano de ajuste
scatterplot3d(publicidad$tv, publicidad$radio, publicidad$ventas, color = "blue", pch = 16, main = "Plano de Ajuste")
plane3d(grid$tv, grid$radio, grid$ventas, alpha = 0.5, col = "red")


# e) Considere el mejor modelo pero ahora con interacción. Compare los modelos con y sin interacción.

# Modelo sin interacción
modelo_sin_interaccion <- lm(ventas ~ tv + radio, data = publicidad)

# Modelo con interacción
modelo_con_interaccion <- lm(ventas ~ tv * radio, data = publicidad)

# Comparar los modelos usando el AIC y el R^2 ajustado
AIC_sin_interaccion <- AIC(modelo_sin_interaccion)
AIC_con_interaccion <- AIC(modelo_con_interaccion)

R2_ajustado_sin_interaccion <- summary(modelo_sin_interaccion)$adj.r.squared
R2_ajustado_con_interaccion <- summary(modelo_con_interaccion)$adj.r.squared

# Imprimir los resultados de comparación
cat("Modelo sin interacción:\n")
cat("AIC:", AIC_sin_interaccion, "\n")
cat("R^2 ajustado:", R2_ajustado_sin_interaccion, "\n")

cat("\nModelo con interacción:\n")
cat("AIC:", AIC_con_interaccion, "\n")
cat("R^2 ajustado:", R2_ajustado_con_interaccion, "\n")

# Interpretacion: El modelo con interaccion tiene un menor valor de AIC, esto indica que este modelo proporciona un mejor ajuste a los datos en terminos de compensacion entre el ajuste y la complejidad del modelo.
# El R2 ajustado con interaccion es mas alto que el de sin interaccion esto significa que el modelo con interaccion explica mejor la variabilidad de la variable respuesta.
# El modelo con interaccion es el preferible

# REGRESORAS CATEGORICAS

# 2.3

salarios <- carData::Salaries
salarios
str(salarios)
summary(salarios)
plot(salarios)
plot(salarios$salary)
plot(salarios$sex)
plot(salarios$yrs.service)

plot(salarios$salary, salarios$yrs.service, xlab = "Salario", ylab = "Años de experiencia", main = "Salario vs. Años de experiencia")

hist(salarios$salary, breaks = 20, xlab = "Salario", ylab = "Frecuencia", main = "Distribución de salarios")

ggplot(salarios, aes(x = salary, fill = sex)) +
  geom_bar() +
  xlab("Salario") +
  ylab("Conteo") +
  ggtitle("Rangos salariales por género")

boxplot(salary ~ rank, data = salarios, xlab = "Rango académico", ylab = "Salario", main = "Salarios por rango académico")

# a) Ajustar un modelo lineal para estimar el salario en función del sexo.

modelo_salario_sexo <- lm(salary ~ sex, data = salarios)
plot(modelo_salario_sexo)
summary(modelo_salario_sexo)

# Calcular las medias de salario por categoría de sex
medias_salario <- aggregate(salary ~ sex, data = salarios, FUN = mean)


# Graficar la regresión lineal
barplot(medias_salario$salary, names.arg = medias_salario$sex, ylab = "Salario", xlab = "Sexo", ylim = range(salarios$salary))
abline(modelo_salario_sexo, col = "red")

# b) Ajustar un modelo lineal para estimar el salario en función de los años de servicio.

modelo_salario_años <- lm(salary ~ yrs.service, data = salarios)
plot(modelo_salario_años)
summary(modelo_salario_años)

# Graficar la regresión lineal
plot(salarios$yrs.service, salarios$salary, xlab = "Años Servicio", ylab = "Salario", main = "Regresión Lineal: Salario vs. Edad")
abline(modelo_salario_años, col = "red")

# c) Encontrar el modelo lineal que produzca el mejor ajuste con dos variables. Es necesario considerar 
#  interacción?

# Se puede probar con y sin interaccion para saber que modelo se ajusta mejor a los datos.

modelo_c <- lm(salary ~ yrs.service + sex, data = Salaries)
summary(modelo_c)
plot(modelo_c)
# d) Ajustar el modelo completo.

modelo_completo <- lm(salary ~ ., data = Salaries)
summary(modelo_completo)
plot(modelo_completo)
# e) Proponer un modelo y justificar que es mejor que el modelo completo. Realizar el análisis diagnóstico para este modelo.

modelo_propuesto <- lm(salary ~ rank + discipline + yrs.service + yrs.since.phd + sex, data = Salaries)
summary(modelo_propuesto)
par(mfrow = c(2, 2))
plot(modelo_propuesto)


# REGRESION POLINOMICA

# 2.4

boston <- MASS::Boston
head(boston,5)
str(boston)
summary(boston)
cor_matrix <- cor(boston)
corrplot(cor_matrix, method = "color")

g1 <- plot(boston$age, boston$dis)
g2 <- plot(boston$lstat, boston$medv)
g3 <- plot(boston$tax, boston$rad)

# a) Utilizar una regresión polinómica de grado 2, otra de grado 5 y otra de
# grado 10 para estimar la variable medv en función de la variable lstat.

# Modelo polinomico de grado 2
model_poly2 <- lm(medv ~ poly(lstat, 2), data = boston)
plot(model_poly2)
summary(model_poly2)

pred2 <- predict(model_poly2)
plot(boston$lstat, boston$medv, main = "Scatter plot of lstat vs. medv", xlab = "lstat", ylab = "medv")
lines(boston$lstat, pred2, col = "blue", lwd = 2)

# Modelo polinomico de grado 5
model_poly5 <- lm(medv ~ poly(lstat, 5), data = boston)
plot(model_poly5)
summary(model_poly5)

pred5 <- predict(model_poly5)
plot(boston$lstat, boston$medv, main = "Scatter plot of lstat vs. medv", xlab = "lstat", ylab = "medv")
lines(boston$lstat, pred5, col = "red", lwd = 2)

# Modelo Lineal

model_lineal <- lm(medv ~ lstat, data = boston)
summary(model_lineal)

pred_l <- predict(model_linear)
plot(boston$lstat, boston$medv, main = "Scatter plot of lstat vs. medv", xlab = "lstat", ylab = "medv")
lines(boston$lstat, pred_l, col = "green", lwd = 2)




# b) Comparar estos dos modelos utilizando el criterio de R2 , son mejores que un modelo lineal simple?

R2_poly2 <- summary(model_poly2)$adj.r.squared
R2_poly5 <- summary(model_poly5)$adj.r.squared
R2_lineal <- summary(model_lineal)$adj.r.squared
R2_poly2
R2_poly5
R2_lineal

# R^2 ajustado: El R^2 ajustado es una medida de la proporción de variabilidad de la variable 
# respuesta que es explicada por el modelo, teniendo en cuenta el número de variables predictoras
# y el tamaño de la muestra. Un valor más alto de R^2 ajustado indica un mejor ajuste del modelo.

# Interpretacion: Claramente los modelos polinomicos son mejores que el modelo lineal ya que se ajustan mejor a la curva de los datos 


# c) Estudie la incorporación de otra de las variables al modelo seleccionado.

model_poly_rm <- lm(medv ~ poly(lstat, 5) + rm, data = boston)
summary(model_poly_rm)

# Scatter plot of lstat vs. medv
plot(boston$lstat, boston$medv, main = "Scatter plot of lstat vs. medv", xlab = "lstat", ylab = "medv")
pred_poly_rm <- predict(model_poly_rm)
lines(boston$lstat, pred_poly_rm, col = "red", lwd = 2)
legend("topright", legend = c("Degree 5 (old model)", "Degree 5 + rm (updated model)"), col = c("blue", "red"), lwd = 2)

R2_poly_rm<- summary(model_poly_rm)$adj.r.squared
R2_poly_rm

# Agreg

# 2.5

fifa <-read.csv("datos_fifa.csv")
fifa
str(fifa)
summary(fifa)

# a) Visualizar la relación entre ambas variables.
plot(fifa$Overall, fifa$Valor, xlab = "Overall", ylab = "Valor", main = "Overall vs. Valor")
cor(fifa$Overall, fifa$Valor)

# b) Ajustar un modelo lineal simple.

ml_fifa <- lm(Valor ~ Overall, data = fifa)
summary(ml_fifa)

pred3 <- predict(ml_fifa)
plot(fifa$Overall, fifa$Valor, main = "Scatter plot", xlab = "Overall", ylab = "Valor")
lines(fifa$Overall, pred3, col = "red", lwd = 2)

# c) Ajustar un modelo lineal polinómico (seleccionar el grado adecuado).

mp_fifa <- lm(Valor ~ poly(Overall, 4), data = fifa)
summary(mp_fifa)

pred_mp_fifa <- predict(mp_fifa)
pred_mp_fifa
plot(fifa$Overall, fifa$Valor, main = "Scatter plot of Overall vs. Valor", xlab = "Overall", ylab = "Valor")
lines(fifa$Overall, pred_mp_fifa, col = "blue", lwd = 2)

# d) Definir la métrica RMSE y evalauar sobre un conjunto de validación los modelos ajustados.

set.seed(123)  # Set a seed for reproducibility
n <- nrow(fifa)
train_indices <- sample(1:n, 0.7 * n)  # 70% of the data for training
train_data <- fifa[train_indices, ]
validation_data <- fifa[-train_indices, ]

ml_fifa <- lm(Valor ~ Overall, data = train_data)
mp_fifa <- lm(Valor ~ poly(Overall, 4), data = train_data)

pred_ml <- predict(ml_fifa, newdata = validation_data)
pred_mp <- predict(mp_fifa, newdata = validation_data)

rmse_ml <- sqrt(mean((validation_data$Valor - pred_ml)^2))
rmse_mp <- sqrt(mean((validation_data$Valor - pred_mp)^2))

rmse_ml
rmse_mp

# ALTERNATIVA
rmse(fifa$Valor, pred3)
rmse(fifa$Valor, pred_mp_fifa)


# e) Realizar el análisis diagnóstico en cada caso.

# HECHO CON CHATGPT FALTAN MAS ANALISIS

residuals_ml <- validation_data$Valor - pred_ml
plot(pred_ml, residuals_ml, xlab = "Predicted Values", ylab = "Residuals", main = "Residual Plot (Linear Model)")

residuals_mp <- validation_data$Valor - pred_mp
plot(pred_mp, residuals_mp, xlab = "Predicted Values", ylab = "Residuals", main = "Residual Plot (Polynomial Model)")

hist(residuals_ml, main = "Histogram of Residuals (Linear Model)")
qqnorm(residuals_ml)
qqline(residuals_ml)

hist(residuals_mp, main = "Histogram of Residuals (Polynomial Model)")
qqnorm(residuals_mp)
qqline(residuals_mp)

plot(pred_ml, residuals_ml, xlab = "Predicted Values", ylab = "Residuals", main = "Residual Plot (Linear Model)")

plot(pred_mp, residuals_mp, xlab = "Predicted Values", ylab = "Residuals", main = "Residual Plot (Polynomial Model)")

plot(ml_fifa, which = 4)  # Cook's distance plot
plot(ml_fifa, which = 5)  # Leverage plot

plot(mp_fifa, which = 4)  # Cook's distance plot
plot(mp_fifa, which = 5)  # Leverage plot

# MODELO ROBUSTO

# 2.6

crime <- read.csv("crime.csv", sep = ";")
crime
str(crime)
summary(crime)

# Elimino variable categorica para hacer correlacion plot
crime_without_state <- crime[, -2]
crime_without_state
cor_matrix_3 <- cor(crime_without_state)
corrplot(cor_matrix_3, method = "color")


# a) Ajustar un modelos de regresión OLS y realizar analítica y gráficamente
#    un análisis diagnóstico examinando leverage y distancias de Cook.

ols_lm <- lm(crime ~ ., data = crime_without_state)
summary(ols_lm)

plot(crime$crime, ols_lm$fitted.values, xlab = "Observed Crime", ylab = "Predicted Crime", main = "OLS Regression: Observed vs. Predicted Crime")
abline(ols_lm, col = "red")

plot(ols_lm, which = 5, main = "Leverage Plot")
# Interpretacion: Este grafico se usa para identificar obs atipicas o influentes de un modelo de regresion.
# El leverage se referiere a la influencia relativa que tiene una observacion sobre la estimacion de los coeficientes del modelo.
# Las obs con un alto leverage tienen un impacto mayor en la estimacion de los coeficientes y lo por tanto pueden afectar significativamente
# los resultados del modelo. En general, se considera que una observación es influente si tiene un alto leverage y un residuo estandarizado grande. 
# Estas observaciones pueden tener un impacto desproporcionado en los resultados del modelo y, por lo tanto, pueden requerir una atención especial.
# Los valores que estan dentro de la linea gris punteada (Cook's Distance) indican que tienen un impacto sustancias en el modelo y por lo tanto se lo
# considera un punto influyente.
# Resumen: En este grafico se ven tres puntos influyentes 51, 11 y muy por el limite el 25
plot(ols_lm, which = 4, main = "Cook's Distance Plot")
# Interpretacion: Muestra lo mismo que el anterior pero en grafico de lineas

# b) Identificar las observaciones influyentes (recordar que 4/n es un valor de
#    corte muy utilizado para las distancias de Cook). Ajustar un modelo OLS
#    sin esas observaciones. Comparar los coeficientes estimados en ambos modelos.


cooksd <- cooks.distance(ols_lm)
cooksd

puntos_influyentes <-  which(cooksd > (4/nrow(crime)))
puntos_influyentes

# Ajustamos modelo OLS 
crime_without_influential <- crime_without_state[-puntos_influyentes, ]

adjusted_ols_lm <- lm(crime ~ ., data = crime_without_influential)

# Print the summary of the adjusted model
summary(adjusted_ols_lm)

# c) Generar una nueva variable con el valor absoluto de los residuos y señalar los diez residuos más altos. Coinciden con los valores influyentes?

# Generate a new variable with absolute residuals
crime$abs_residuals <- abs(ols_lm$residuals)

# Find the ten highest residuals
highest_residuals <- tail(sort(crime$abs_residuals), 10)

# Print the ten highest residuals
highest_residuals
puntos_influyentes

# No coinciden con los puntos influyentes


# d) Ajustar un modelo de regresión robusta mediante mínimos cuadrados
#    ponderados iterados (IRLS). El comando para ejecutar una regresión robusta está rlme n(library MASS)e. Se pueden utilizar varias funciones de
#    ponderación en IRLS uar en primera instancia los pesos de Huber.

robust_lm_huber <- rlm(crime ~ ., data = crime_without_state, psi = psi.huber)
summary(robust_lm_huber)

# e) Hacerlo ahora con los pesos de la función bicuadrada ( psi = psi.bisquare).
robust_lm_bisquare <- rlm(crime ~ ., data = crime_without_state, psi = psi.bisquare)
summary(robust_lm_bisquare)

# Interpretacion: Se observa que disminuye el desvio estandar residual. Esta metrica mide la cantidad media por la que las observaciones se desvian de las predicciones
# La funcion bicuadrada tienes menos RSE que Huber. Esto quiere decir que los residuos tienden a estar mas cerca de cero 
# y qeu el modelo se ajusta mejor a los datos.

# REGRESION CUANTILES

# 2.7

us <- data(USgirl)
us
summary(Usgirl)
# a) Graficar los pesos versus las edades. Qué se puede apreciar en este diagrama de dispersión?
# b) Ajustar un modelo para la mediana y graficar.
# c) Ajustar un modelo para los cuartiles y graficar.
# d) Ajustar un modelo para los deciles y graficar









