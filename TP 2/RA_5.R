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



# Capitulo 5. REGRESION LOGISTICA

# MODELO UNIVARIADO: INTERPRETACION

# 5.1 

# a) ¿Se puede afirmarque la temperatura influye en la probabilidad de que
# los propulsores tengan defectos? Qué test utilizó para responder? Justifique.
# b) Interprete el valor del coeficiente estimado para temperatura en el contexto del problema.
# c) Para qué valores de temperatura la probabilidad estimada de que se produzcan defectos supera 0.1? y 0.5?

# 5.2   

# a) Obtenga los box-plots para la variable SSPG para los grupos con y sin
# diabetes. Compare los valores medios de ambos grupos y comente.
# b) Obtenga el diagrama de dispersión de los valores observados de la variable respuesta (en el eje vertical) y la variable SSPG.
# c) Construya una tabla que contenga para cada grupo etáreo la media de la
# edad y la media de DIABET, que corresponde a la proporción de individuos que tienen DIABET en cada grupo. Analice.
# d) Ajuste un modelo para estimar diabet en función de SSPG.
# e) Interprete los coeficientes obtenidos en términos del problema planteado
# f) Para una persona con SSPG igual a 100, ¿qué valores de logit, odds y la
# probabilidad de tener DIABET estima el modelo? Calcúlelos.
# g) Halle un intervalo de confianza del 95 % del Odds Ratio para DIABET. Interprete.
# h) Evalúe la bondad del ajuste obtenido. Para ello construya una matriz de
# confusión con un conjunto de validación del 30 % de los datos utilizando
# como punto de corte pb = 0,5. Hágalo luego con otros puntos de corte.
# i) Realizar el test de Hosmer –Lemeshow y comentar los resultados obtenidos.
# j) Estime el área bajo la curva ROC y explique el resultado.

# 5.3

# a) Estudie la relación entre bajo peso al nacer (LOW =1) y fumar durante el embarazo (SMOKE=1) mediante un modelo logístico.
# b) Escriba la expresión del modelo ajustado e interprete los coeficientes. Es
# significativa la variable smoke? Básese en el test de Wald para responder a esta pregunta.
# c) Construya la matriz de confusión para un conjunto de validación de un tercio de la base.
# d) Testee basándose en la verosimilitud este modelo versus otro que considere también la edad de la madre. Interprete los resultados.

# MODELO MULTIVARIADO

# 5.4

# a) Hay alguna especie que le parece que sobrevivió más que otra? Considera que la supervivencia se asocia con la severidad de la tormente? y con el diámetro del árbol?
# b) Proponga un modelo que sólo utilice como predictora la variable diámetro. Halle la bondad de ajuste y la precisión de la predicción lograda.
# c) Proponga un segundo modelo que considere como predicotras al diámetro y a la severidad de la tormenta. Halle la bondad de ajuste y la precisión de la predicción lograda.
# d) Compare ambos modelos considerando la verosimilitud, la bondad de ajuste y el área bajo la curva ROC de cada uno.
# e) Estimar la probabilidad de que no sobreviva un árbol cuyo diámetro es
# de 30 cm y esté situado en una zona en la que la fuerza de la tormenta viene dada por S=0.8.
# f) Compare la precisión del mejor de los modelos con un análisis discriminante lineal y con un análisis discriminante cuadrático.

# 5.5

# a) Construye un modelo de regresión logística a fin de establecer una relación que permita predecir la presencia de invasión de las vesículas seminales.
# b) Seleccione la/s variable/s adecuadas para informar cual/es de las variables incluidas en el modelo inicial son significativas.
# c) Escriba la expresión del modelo final si solo se incluyeran en el las variables que resultaron significativas.
# d) Pruebe interacciones dobles e incluya alguna en el modelo si resulta significativa.
# e) Considerando como valor de corte 0.5 cómo clasificaría un individuo que tuvo tiene Gleason 4 y penetración capsular.

# 5.6

# a) Ajustar un modelo logístico considerando como variable predictora el
# bmi y como respuesta type.Utilizar el test de razón de verosimilitudes
# para evaluar la significación del modelo, comparándolo con el modelo nulo.
# b) Defina una variable categórica que separe a las mujeres que no han tenido embarazos previos de las que sí. Ajuste un modelo para evaluar si
# esta variable es significativa para predecir type.
# c) Ajuste un modelo utilizando en este caso como predictoras la edad, la variable categórica definida en el item anterior y el bmi.
# d) Ajuste un modelo utilizando en este caso como predictoras la edad, el bmi y el número de embarazos previos.
# e) Seleccione el mejor modelo mediante el test de razón de verosimilitudes.

