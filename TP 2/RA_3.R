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



# Capitulo 3. MODELOS ALTERNATIVOS

# SELECCION DE VARIABLES

# 3.1 

# (a) Ajustar el modelo saturado (que contiene a todas las varibles dependientes).
# (b) Analizar a través del VIF la presencia de multicolinealidad.
# (c) Realizar una selección de variables foward teniendo en cuenta el criterio de Akaike.
# (d) Escribir la expresión del modelo seleccionado. Realizar un análisis diagnóstico del mismo.
# (e) Realizar una selección backward teniendo en cuenta el criterio de R2
# ajustado. Se selecciona el mismo modelo?
# (f) Utilizando la función ols_step_all_possible de la biblioteca olsrr creada
# por Hebbali (2020) obtener todas las regresiones posibles. Elegir un único modelo visualizando gráficamente los resultados y considerando los
# criterios BIC, AIC, CP y R2 adj.

# 3.2

# a) Hallar el mejor modelo de regresión lineal con variable respuesta brozek
# utilizando entre 1 y 14 variables predictoras. Elegir el mejor considerando el criterio CP de Mallows y R2 adj .
# b) Repetir considerando ahora la minimización del Error Cuadrático Medio del modelo usando validación cruzada leave one out
# c) Inspeccionar gráficamente el MSE y decidir cuál es el mejor modelo. Interpretar los coeficientes del mismo.
# d) Coinciden las variables de los modelos elegidos con los diferentes criterios

# MODELOS DE REGULARIZACION

# 3.3

# 1. Ajustar un modelo de Ridge para la variable respuesta Employed.
# 2. Ajustar un modelo de Lasso para la variable respuesta Employed.
# 3. Ajustar un modelo de Elastic Net para la variable respuesta Employed.
# 4. Comparar los resultados obtenidos en los tres modelos.

# 3.4

# a) Considerando la variable respuesta lpsa, ajustar un modelo lineal utilizando como predictoras a todas las demás. Qué inconveniente tiene este
# modelo?.
# b) Aplicar un método de selección de variables utilizando como criterio BIC. Qué variables quedaron? Coinciden con el OLS?.
# c) Ajustar ahora modelos regularizados y comparar los resultados y coeficientes utilizando CV

# MODELOS BASADOS EN PCA

# 3.5

# a) Realizar un correlograma para el conjunto de variables explicativas. Tiene sentido en este caso un PCA? En caso afirmativo explore las componentes principales.
# b) Partir la base en train-test. Considerando la calidad como variable respuestas, ajustar un modelo de PCR.
# c) Cuál es el número óptimo de componentes principales a considerar? Grafique las puntuaciones originales y las ajustadas por PCR.
# d) Calcular el MSE para este subconjunto de componentes.
# e) Realizar el ajuste en este caso con PLS. Comparar los resultados de ambos
# modelos.
# f) * (para hacer en la unidad de regresión logística) Clasifique a los vinos
# como regulares (calidad< 5) → 0, y buenos o muy buenos (calidad≥ 5) → 1. Ajuste un modelo de regresión logística para estimar la calidad de vino. Evalue la pertinencia del modelo

# 3.6

# a) Realizar un análisis cuidadoso de las variables predictoras y una limpieza de la base.
# b) Aplicar PCR y PLS para predecir Yield (rendimiento) y comparar los resultados de ambos métodos.

