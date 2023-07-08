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



# Capitulo 4. ANALISIS DE VARIANZA

# DCA

# 4.1 

# (a) Graficar la variable observada en los grupos y analizar la presencia de
# outliers y la igualdad grafica de las medias y las formas de las distribuciones.
# (b) Calcular la media y el desvio de cada uno de los grupos. Le parece que
# se satisface el supuesto de homogeneidad?
# (c) Establecer las hipótesis estadísticas de interés.
# (d) Contrastar las hipótesis con un nivel α = 0,05.
# (e) Verificar el cumplimiento de los supuestos de normalidad y homocedasticidad.
# (f) Si se verifican concluir en el contexto del problema.

# 4.2

# (a) Realice un análisis gráfico y descriptivo de la eficiencia de conversión
# lograda por los distintos suplementos.
# (b) Establezca las hipótesis de interés del problema y explicite los supuestos
# necesarios.
# (c) Testee las hipótesis con nivel de significación del 5 %.
# (d) Analice el cumplimiento de los supuestos del modelo.
# (e) Concluya en términos del problema y si rechazó H0, indique cuales medias son diferentes. Utilice para ello las comparaciones a posteriori de
# Tuckey

# 4.3

# (a) Identifique la variable dependiente y el factor de interes.
# (b) Escriba el modelo, en general y en términos del problema.
# (c) Analice los resultados de las pruebas de hipótesis para los supuestos del
# modelo.
# (d) Plantee las hipótesis y construya la tabla de Anova sabiendo que (ver formula en PDF)
# (e) Compare los tratamientos y utilizando un test t con nivel global 0.05 es
# decir que como son 3 comparaciones α = 0,05/3 para cada una.
# (f) Adicionalmente se indagó a los pacientes sobre efectos colaterales gástricos como respuesta al tratamiento. Los encuestados respondieron según
# una escala entre 0 y 5 (0 = nunca, 5= siempre). Los resultados obtenidos
# fueron: (ver tabla en PDF)
    # (I) ¿Cree que los investigadores deberían utilizar la misma prueba estadística que la empleada para comparar el tiempo libre de dolor? Justifique.
    # (II) ¿Cuáles son las conclusiones de este estudio?

# ALTERNATIVA NO PARAMETRICA

# 4.4

# (a) Grafique los tiempos de cocción por tratamiento. Calcule las medidas resumen de los mismos.
# (b) Establezca las hipótesis de interés, escriba el modelo detallando los supuestos.
# (c) Realice la prueba y el diagnostico correspondiente. Son válidos los resultados de la prueba?
# (d) Si respondió afirmativamente en c) concluya en el contexto del problema. Si concluyo negativamente intente una transformación de potencia
# conveniente para normalizar y/o homocedastizar la variable respuesta.
# (e) Realice nuevamente la prueba si fuera necesario y el diagnóstico del modelo correspondiente. Concluya en términos del problema
# (f) Compare los resultados con los del test no paramétrico.

# ANOVA DE DOS VIAS CON Y SIN INTERACCION

# 4.5

# a) Analice la proporción de germinación global.
# b) Estudie si hay asociación entre la humedad y la germinación.
# c) Analice si la germinación depende de la cobertura y si hay interacción entre los dos factores.
# d) Construya un modelo que permita explicar la relación de los dos factores con el porcentaje de germinación.
# e) Utilice los efectos y las comparaciones a posteriori para realizar una recomendación.

# 4.6

# (a) Explorar visualmente las medias por las distintas combinaciones de los factores considerados.
# (b) Valorar visualmente la presencia de interacción.
# (c) Construir un modelo y estimar los coeficientes del mismo. Interpretar los coeficientes y el efecto.
