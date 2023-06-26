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
#library(chatgpt)
#Sys.setenv(OPENAI_API_KEY = "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")
library(aod) #Para test de wald
library(nortest) # Para test de Shapiro y demas 
library(lmtest)
library(car)

setwd("C:/Users/idott/Advanced_Regression/Advanced_Regression/TP 2")



# Capitulo 2. MODELO LINEAL MULTIVARIADO

# MODELO ADITIVO

# 2.1 

# a) Visualizar la asociación entre las variables de a pares.
# b) Ajuste un modelo lineal simple para cada una de las dos predictoras disponibles.
# c) Realice un análisis diagnóstico en cada caso y señale en caso de haberlos
#    puntos influyentes y outliers.
# d) Estime un intervalo de confianza para los coeficientes del modelo lineal estimado en cada caso.
# e) Ajuste un nuevo modelo sin la/s observaciones influyentes.
# f) Construya el intervalo de confianza y el de predicción del 95 % para un
#    árbol cuyo diámetro es 16.1 pulgadas.
# g) Ajuste un modelo utilizando conjuntamente las dos variables predictoras
# y compare este ajuste con el mejor de los modelos anteriores mediante un
# test de modelos anidados. Concluya.

# MODELO CON INTERACCION

# 2.2

# a) Ajustar un modelo de regresión lineal simple para cada una de las variables predictoras por separado. Realizar a continuación el análisis diagnóstico de los modelos.
# b) Ajustar un modelo aditivo con las tres variables y decidir si alguna de
#    ellas no es significativa (test de Wald).
# c) Ajustar los modelos de a pares y quedarse con el que mejor explique la
#    variable respuesta utilizando el criterio de AIC, R2 y Cp_Mallows.
# d) Grafique para el modelo seleccionado el plano de ajuste y evalue si le parece adecuado.
# e) Considere el mejor modelo pero ahora con interacción. Compare los modelos con y sin interacción.

# REGRESORAS CATEGORICAS

# 2.3

# a) Ajustar un modelo lineal para estimar el salario en función del sexo.
# b) Ajustar un modelo lineal para estimar el salario en función de los años de servicio.
# c) Encontrar el modelo lineal que produzca el mejor ajuste con dos variables. Es necesario considerar interacción?
# d) Ajustar el modelo completo.
# e) Proponer un modelo y justificar que es mejor que el modelo completo. Realizar el análisis diagnóstico para este modelo.

# REGRESION POLINOMICA

# 2.4

# a) Utilizar una regresión polinómica de grado 2, otra de grado 5 y otra de
# grado 10 para estimar la variable medv en función de la variable lstat.
# b) Comparar estos dos modelos utilizando el criterio de R2 , son mejores que un modelo lineal simple?
# c) Estudie la incorporación de otra de las variables al modelo seleccionado.

# 2.5

# a) Visualizar la relación entre ambas variables.
# b) Ajustar un modelo lineal simple.
# c) Ajustar un modelo lineal polinómico (seleccionar el grado adecuado).
# d) Definir la métrica RMSE y evalauar sobre un conjunto de validación los modelos ajustados.
# e) Realizar el análisis diagnóstico en cada caso.

# MODELO ROBUSTO

# 2.6

# a) Ajustar un modelos de regresión OLS y realizar analítica y gráficamente
#    un análisis diagnóstico examinando leverage y distancias de Cook.
# b) Identificar las observaciones influyentes (recordar que 4/n es un valor de
#    corte muy utilizado para las distancias de Cook). Ajustar un modelo OLS
#    sin esas observaciones. Comparar los coeficientes estimados en ambos modelos.
# c) Generar una nueva variable con el valor absoluto de los residuos y señalar los diez residuos más altos. Coinciden con los valores influyentes?
# d) Ajustar un modelo de regresión robusta mediante mínimos cuadrados
#    ponderados iterados (IRLS). El comando para ejecutar una regresión robusta está rlme n(library MASS)e. Se pueden utilizar varias funciones de
#    ponderación en IRLS uar en primera instancia los pesos de Huber.
# e) Hacerlo ahora con los pesos de la función bicuadrada ( psi = psi.bisquare).

# REGRESION CUANTILES

# 2.7

# a) Graficar los pesos versus las edades. Qué se puede apreciar en este diagrama de dispersión?
# b) Ajustar un modelo para la mediana y graficar.
# c) Ajustar un modelo para los cuartiles y graficar.
# d) Ajustar un modelo para los deciles y graficar