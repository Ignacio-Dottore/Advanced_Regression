library(dplyr) #manipulacion de datos
library(kableExtra) #tablas
library(ggplot2) #graficos
library(tidyverse) #manipulacion de datos
library(ggpubr) #para juntar
library(ggforce) # grafico de violin y jitter
library(GGally) # ggpairs
library(ggbeeswarm)
library(gridExtra)
library(sos)
library(data.table)
library(readr)
library(scales)
library(readr)
library(corrplot)
library(reshape2)
library(caret)
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
library(mvp)
library(leaps)
library(httpgd)
library(languageserver)
library(xfun)
library(faraway)
library(leaps)
library(lars)
library(glmnet)
library(coefplot)
library(factoextra)
library(pls)
library(AppliedPredictiveModeling)
library(PerformanceAnalytics)
library(outliers)
library(pgirmess)

setwd("C:/Users/idott/Advanced_Regression/Advanced_Regression/TP 1")



# Capitulo 4. ANALISIS DE VARIANZA

# DCA

# 4.1 

botellas <- read.csv("botellas.csv",sep = ";")
botellas

str(botellas)
summary(botellas)




# (a) Graficar la variable observada en los grupos y analizar la presencia de
# outliers y la igualdad grafica de las medias y las formas de las distribuciones.

cor_matrix <- cor(botellas)
cor_matrix
ggcorrplot(cor_matrix)

boxplot(botellas)
ggpairs(botellas)


lapply(botellas, grubbs.test)

analisis_outliers <- function(v_datos) {

Atributo <- colnames(v_datos[0,1:ncol(v_datos)])
df <- data.frame(Atributo)
df$Q1 <- NA
df$Q3 <- NA
df$IQR <- NA
df$ModSup <- NA
df$ModInf <- NA
df$SevSup <- NA
df$SevInf <- NA
df$OutModSup <- ""
df$OutModInf <- ""
df$OutSevSup <- ""
df$OutSevInf <- ""

for (i in 1:ncol(v_datos)){

  stats <- as.data.frame(t(summary(v_datos[, i])))
  
  q1 <- stats$Freq[stats$Var2 == "1st Qu."]
  q3 <- stats$Freq[stats$Var2 == "3rd Qu."]
  
  iqr <- q3-q1
  ModSup <- q3+(1.5*iqr)
  ModInf <- q1-(1.5*iqr)
  SevSup <- q3+(3*iqr)
  SevInf <- q1-(3*iqr)
  
  OutModSup <- v_datos[,i][v_datos[,i]<SevInf]
  if (length(OutModSup) == 0) {OutModSup<-'NA'} else {OutModSup <- paste(OutModSup,collapse = ",")}
  
  OutModInf <- v_datos[,i][v_datos[,i]<ModInf]
  if (length(OutModInf) == 0) {OutModInf<-'NA'} else {OutModInf <- paste(OutModInf,collapse = ",")}
  
  OutSevSup <- v_datos[,i][v_datos[,i]>ModSup]
  if (length(OutSevSup) == 0) {OutSevSup<-'NA'} else {OutSevSup <- paste(OutSevSup,collapse = ",")}
  
  OutSevInf <- v_datos[,i][v_datos[,i]>SevSup]  
  if (length(OutSevInf) == 0) {OutSevInf<-'NA'} else {OutSevInf <- paste(OutSevInf,collapse = ",")}

  df[i,2:12] <- c(q1,q3,iqr,ModSup,ModInf,SevSup,SevInf,OutModSup,OutModInf,OutSevSup,OutSevInf)
  
}
  
return(df)

rm(Atributo,i,stats,q1,q3,iqr,ModSup,ModInf,SevSup,SevInf,OutModSup,OutModInf,OutSevSup,OutSevInf,v_datos,df)
}

analisis_outliers(botellas)


# (b) Calcular la media y el desvio de cada uno de los grupos. Le parece que
# se satisface el supuesto de homogeneidad?
media <-lapply(botellas,mean)
sd <- lapply(botellas,sd)

resultados <- data.frame(Media = media, Desvio = sd)

grouped_data <- stack(botellas)
grouped_data

# Interpretacion: Por lo que se ve no se satisface el supuesto de homogeneidad ya que las medias son distintas y los desvios tambien.

# (c) Establecer las hipótesis estadísticas de interés.

# H0: Las medias de los grupos son iguales. No hay diferencias significativas entre el contenido de sodio entre las marcas de cerveza
# H1: Las medias de los grupos son distinta. Existen diferencias significativas entre el contenido de sodio entre las marcas de cerveza

# (d) Contrastar las hipótesis con un nivel α = 0,05.

botellas2 <- reshape2::melt(botellas)
botellas2
anova <- aov(value ~ variable, data = botellas2)
summary <- summary(anova)

p_value <- summary[[1]]$`Pr(>F)`[1]
p_value

alpha <- 0.05
if (p_value < alpha) {
  cat("Rechazamos la hipótesis nula. Hay evidencia suficiente para afirmar que las medias de los contenidos de sodio en las marcas de cerveza son diferentes.")
} else {
  cat("No hay suficiente evidencia para rechazar la hipótesis nula. No podemos afirmar que las medias de los contenidos de sodio en las marcas de cerveza sean diferentes.")
}

# (e) Verificar el cumplimiento de los supuestos de normalidad y homocedasticidad.

normality_tests <- lapply(botellas, shapiro.test)
normality_results <- sapply(normality_tests, function(x) x$p.value)
normality_results

plot(anova,which=2)

qqPlot(residuals(anova),ylab = "residuos", col = "coral",pch = 19, col.lines = "cadetblue",id=FALSE)
# Interpretacion: Como todos los p-values son >= 0.05 se rechaza la H0 de normalidad. Por lo tanto, no se cumple el supuesto de normalidad.
8231 702
homogeneity_test <- car::leveneTest(values ~ ind, grouped_data)
homogeneity_test

# Interpretacion: Como el p-value es >= 0.05 se rechaza la H0 de homogeneidad de las varianzas. Por lo tanto, no hay suficiente evidencia para sugerir que las variaciones de contenido de sodio
# difieren significativamente entre las marcas de cerveza.

# (f) Si se verifican concluir en el contexto del problema.

TukeyHSD(anova,conf.level=0.95)

# Interpretacion: Este test se usa para comparar las medias de multiples grupos y determinar si diferencias significativas entre ellas. Se usa seguido de un test de ANOVA para identifica si la media de un grupo especifico difiere significativamente de las otras.
# En este caso, se puede ver que las medias de las marcas de cerveza son diferentes entre si. 


# 4.2

vacas <- read.csv("vacas.csv",sep=";")
vacas

# (a) Realice un análisis gráfico y descriptivo de la eficiencia de conversión
# lograda por los distintos suplementos.

str(vacas)
summary(vacas)

cor_matrix2 <- cor(vacas)
cor_matrix2
ggcorrplot(cor_matrix2)

boxplot(vacas)
ggpairs(vacas)

media <-lapply(vacas,mean)
sd <- lapply(vacas,sd)

resultados2 <- data.frame(Media = media, Desvio = sd)

grouped_data2 <- stack(vacas)
grouped_data2

# (b) Establezca las hipótesis de interés del problema y explicite los supuestos
# necesarios.

#H0 : µ1 = µ2 = · · · = µk
#H1 : µi ̸= µj para algún par (i, j)

#H0: Las medias de los grupos son iguales. No hay diferencias significativas de la eficiencia de conversion entre los suplementos.
#H1: Las medias de los grupos son distintas. Existen diferencias significativas de la eficiencia de conversion entre los suplementos.

# (c) Testee las hipótesis con nivel de significación del 5 %.

vacas2 <- reshape2::melt(vacas)
vacas2
anova2 <- aov(value ~ variable, data = vacas2)
summary2 <- summary(anova2)
summary2

p_value2 <- summary2[[1]]$`Pr(>F)`[1]
p_value2

alpha <- 0.05
if (p_value2 < alpha) {
  cat("Rechazamos la hipótesis nula. Hay evidencia suficiente para afirmar que las medias de eficiencia de conversion entre los suplementos no son iguales.")
} else {
  cat("No hay suficiente evidencia para rechazar la hipótesis nula. No podemos afirmar que las medias de eficiencia de conversion entre los suplementos diferentes.")
}

# (d) Analice el cumplimiento de los supuestos del modelo.

normality_tests2 <- lapply(vacas, shapiro.test)
normality_results2 <- sapply(normality_tests2, function(x) x$p.value)
normality_results2

plot(anova2,which=2)

qqPlot(residuals(anova2),ylab = "residuos", col = "coral",pch = 19, col.lines = "cadetblue",id=FALSE)

# Interpretacion: Como todos los p-value son mayores a 0.05 no hay suf evidencia para rechazar la H0 de normalidad de los grupos. Por lo tanto, esto indica
# que los datos se distribuyen normalmente.

homogeneity_test2 <- car::leveneTest(values ~ ind, grouped_data2)
homogeneity_test2

# Interpretacion: Como el p-value es >= 0.05 NO se rechaza la H0 de homogeneidad de las varianzas.
# Por lo tanto, podemos asumir que las varianzas de los grupos no son significativamente diferentes.


# (e) Concluya en términos del problema y si rechazó H0, indique cuales medias son diferentes. Utilice para ello las comparaciones a posteriori de
# Tuckey

TukeyHSD(anova2,conf.level=0.95)

# Interpreatcion: El par de medias que son diferentes son los que tienen un p-value <= 0.05. Es decir, S3-S1, S4-S1, S3-S2 y S4-S2.

# 4.3

drogas <- data.frame(
  Tratamiento = c("Placebo", "Aspirina", "Droga"),
  media = c(2.5, 2.82, 3.2),
  desvio = c(0.13, 0.2, 0.17)
)
drogas

p_levene <- 0.18
shapiro <- 0.24

# (a) Identifique la variable dependiente y el factor de interes.

# Variable dependiente: Tiempo libre de dolor despues de ser tratado.

# Factor de interes: Tipo de tratamiento (Placebo, Aspirina, Droga)


# (b) Escriba el modelo, en general y en términos del problema.

# Yij = µi + τi  + εij

# Donde: Yij es la variable dependiente (n° de horas sin dolor) para el paciente j tratado con el i-esimo tipo de tratamiento
# µ es la media 
# ti: es el efecto del i-esimo tipo de tratamiento (i=1 (placebo), i=2 (aspirina), i=3 (droga))
# εij: es el error aleatorio asociado a la observación Yij

# En terminos del problema:

# Horas sin dolor = media + Efecto del tratamiento + Error aleatorio

# (c) Analice los resultados de las pruebas de hipótesis para los supuestos del
# modelo.2

# Prueba de Levene: El objetivo de la prueba de Levene es evaluar si las varianzas de los grupos son iguales.
# El p-value = 0.18 > 0.05. Por lo tanto, no hay pruebas de que las varianzas difieran significativamente. 
# Esto indica que se cumple el supuesto de homogeneidad de varianzas.

# Shapiro-Wilk Test: Se usa para evaluar la normalidad de los residuos del modelo. 
# El p-value = 0.24 > 0.05. Por lo tanto, no hay pruebas suf para afirmar que los residuos no siguen una distribucion normal. 
# Esto sugiere que el supuesto de normalidad se cumple


# (d) Plantee las hipótesis y construya la tabla de Anova sabiendo que (ver formula en PDF)

#H0 : µ1 = µ2 = · · · = µk
#H1 : ∃(i, j) : µi ̸= µj

#H0: El número de horas en que el paciente está libre de dolor es igual.
#H1: El número de horas en que el paciente está libre de dolor es distinto.

#+----------------+-----------+-------------+----------+----------+
#| Fuente de      | Suma de   | Grados de   | Media de | Valor F  |
#| variación      | cuadrados | libertad    | cuadrados|          |
#+----------------+-----------+-------------+----------+----------+
#| Tratamiento    |    SSTr   |    k-1      |  MST     |   F(A)   |
#| Error          |    SSE    |    N-k      |  MSE     |          |
#| Total          |    SST    |    N-1      |          |          |
#+----------------+-----------+-------------+----------+----------+

N <- 30
n1 <- 10
n2 <- 10
n3 <- 10

#Grados de libertad 

# k-1 = 3-1 = 2
# N-k = 30-3 = 27 
# N-1 = 30-1 = 29

k <- length(unique(drogas$Tratamiento))
k

dftr <- k-1
dftr
dfe <- N-k
dfe
dft <- N-1
dft

# SCerror = Suma Cuadrados Error = SSE

# Segun ejercicio SSE = Σ (ni - 1)*si^2 

SSE <- (n1-1)*drogas$desvio[1]^2 + (n2-1)*drogas$desvio[2]^2 + (n3-1)*drogas$desvio[3]^2
SSE

# (e) Compare los tratamientos y utilizando un test t con nivel global 0.05 es
# decir que como son 3 comparaciones α = 0,05/3 para cada una.
# (f) Adicionalmente se indagó a los pacientes sobre efectos colaterales gástricos como respuesta al tratamiento. Los encuestados respondieron según
# una escala entre 0 y 5 (0 = nunca, 5= siempre). Los resultados obtenidos
# fueron: (ver tabla en PDF)
    # (I) ¿Cree que los investigadores deberían utilizar la misma prueba estadística que la empleada para comparar el tiempo libre de dolor? Justifique.
    # (II) ¿Cuáles son las conclusiones de este estudio?

# ALTERNATIVA NO PARAMETRICA

# 4.4

alimento <- data.frame(
  a = c(25,36,36,25,36,16,25,36,49,36,25),
  b = c(121,36,36,64,36,81,49,25,64,49,121),
  c = c(81,81,36,9,25,36,9,49,169,1,81),
  d = c(25,25,36,25,36,25,25,25,25,25,25)
)
alimento
str(alimento)
summary(alimento)

cor_matrix3 <- cor(alimento)
cor_matrix3
ggcorrplot(cor_matrix3)

# (a) Grafique los tiempos de cocción por tratamiento. Calcule las medidas resumen de los mismos.

boxplot(alimento)
ggpairs(alimento)

media3 <-lapply(alimento,mean)
sd3 <- lapply(alimento,sd)

resultados3 <- data.frame(Media = media3, Desvio = sd3)

grouped_data3 <- stack(alimento)
grouped_data3

ggplot(data = grouped_data3, mapping = aes(x = values, colour = ind)) + 
  geom_histogram() + theme_bw() + facet_grid(. ~ ind) + 
  theme(legend.position = "none") + stat_bin(binwidth=30)

# (b) Establezca las hipótesis de interés, escriba el modelo detallando los supuestos.

#H0 : var1 = var2 = · · · = vark
#H1 : vari ̸= varj para algún par (i, j)

#H0: Las varianzas de los grupos son iguales. No hay diferencias significativas en el tiempo de coccion entre los grupos de alimentos.
#H1: Las varianzas de los grupos son distintas. Existen diferencias significativas en el tiempo de coccion entre los grupos de alimentos.

# (c) Realice la prueba y el diagnostico correspondiente. Son válidos los resultados de la prueba?



anova3 <- aov(values ~ ind, data = grouped_data3)
summary(anova3)

# Interpretacion: El p-value = 0.0255 es menor a 0.05. Por lo tanto, hay evidencia estadistica para rechazar la H0 y concluir que existen diferencias significativas.
# Hay diferencias significativas en los tiempos de coccion.

# Analizamos supuestos de normalidad y homocedasticidad

# Normalidad
normality_tests3 <- lapply(alimento, shapiro.test)
normality_results3 <- sapply(normality_tests3, function(x) x$p.value)
normality_results3

# Hacer este metodo para Shapiro en ANOVA
shapiro.test(anova3$residuals)

plot(anova3,which=2)

qqPlot(residuals(anova3),ylab = "residuos", col = "coral",pch = 19, col.lines = "cadetblue",id=FALSE)

# Interpretacion: Como todos los p-value son menores a 0.05  hay suf evidencia para rechazar la H0 de normalidad de los grupos. Por lo tanto, esto indica
# que los datos se NO distribuyen normalmente.

# Homocedasticidad

homogeneity_test3 <- car::leveneTest(values ~ ind, grouped_data3)
homogeneity_test3

# Interpretacion: Como el p-value es <= 0.05  se rechaza la H0 de homogeneidad de las varianzas.
# Por lo tanto, podemos asumir que las varianzas de los grupos son significativamente diferentes.


# Cuando no se verifican los supuestos de ANOVA se va a realizar una trasformacion de los datos mediante la transformacion de Box y Cox. 
# Box y Cox se usa para pruebas parametricas como ANOVA.

# transformacion de Box y Cox en el punto d)

# (d) Si respondió afirmativamente en c) concluya en el contexto del problema. Si concluyo negativamente intente una transformación de potencia
# conveniente para normalizar y/o homocedastizar la variable respuesta.

box_1 <- boxcox(values ~ ind, data =grouped_data3)
box_1

# Sacar valor optimo de lambda de Box y Cox
lambda_optimo <- box_1$x[which.max(box_1$y)]
lambda_optimo

grouped_data3

datos_tranformados <-(((grouped_data3$values^lambda_optimo)-1)/lambda_optimo)
datos_tranformados

anova3_tranformado <- aov(values~ind, data = grouped_data3)
summary(anova3_tranformado)

# Interpretacion: El p-value = 0.0255 es menor a 0.05. Por lo tanto, hay evidencia estadistica para rechazar la H0 
# y concluir que existen diferencias significativas aunque se hayan transformado los datos.

# (e) Realice nuevamente la prueba si fuera necesario y el diagnóstico del modelo correspondiente. Concluya en términos del problema

# Analizamos supuestos de normalidad y/o homocedasticidad

# Normalidad
shapiro.test(anova3_tranformado$residuals)

plot(anova3_tranformado,which=2)

qqPlot(residuals(anova3_tranformado),ylab = "residuos", col = "coral",pch = 19, col.lines = "cadetblue",id=FALSE)

# Interpretacion: Como todos los p-value son menores a 0.05  hay suf evidencia para rechazar la H0 de normalidad de los grupos. Por lo tanto, esto indica
# que los datos se NO distribuyen normalmente.

# Conclusion: Como no se verifican los supuestos incluso despues de transformar los datos, se va a optar por utilizar pruebas no parametricas en lugar de ANOVA. 
# Algunas opciones son: Kruskal-Wallis o Friedman (datos repetidos). Estas pruebas evaluan si hay diferencias significativas entre los grupos sin hacer suposiciones sobre
# la distribucion de los datos.

kruskal.test(values ~ ind, data = grouped_data3)

# Interpretacion: El p-value del test es <= 0.05 por lo tanto, se rechaza la hipotesis nula y se 
# concluye que hay diferencias significativas entre los grupos.

kruskalmc(grouped_data3$values ~ grouped_data3$ind)

# Interpretacion: Se visualizan diferencias significativas para el par b-d (TRUE), sin embargo para el resto no se visualizan diferencias.

# (f) Compare los resultados con los del test no paramétrico.

# Conclusion final: Los tiempos de coccion no son los mismos para los distintos grupos de alimentos.

# ANOVA DE DOS VIAS CON Y SIN INTERACCION

# 4.5 

seed <- read.csv("semillas.csv",sep=";")
seed

str(seed)
summary(seed)

# Se visuali3zan nulos por lo tanto se procede a transformar los datos.

quimicos <- na.aggregate(quimicos, FUN = median)
str(quimicos)

# a) Analice la proporción de germinación global.
# b) Estudie si hay asociación entre la humedad y la germinación.
# c) Analice si la germinación depende de la cobertura y si hay interacción entre los dos factores.
# d) Construya un modelo que permita explicar la relación de los dos factores con el porcentaje de germinación.
# e) Utilice los efectos y las comparaciones a posteriori para realizar una recomendación.

# 4.6

# (a) Explorar visualmente las medias por las distintas combinaciones de los factores considerados.
# (b) Valorar visualmente la presencia de interacción.
# (c) Construir un modelo y estimar los coeficientes del mismo. Interpretar los coeficientes y el efecto.



