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
library(lsr)
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
library(MASS)
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
library(ResourceSelection)
library(outliers)
library(pgirmess)
library(rstatix)
library(readxl)
library(vcd)
library(pROC)
library(mosaic)
library(ROCR)

setwd("C:/Users/idott/Advanced_Regression/Advanced_Regression/TP 1")

# Capitulo 5. REGRESION LOGISTICA

# MODELO UNIVARIADO: INTERPRETACION

# 5.1 

cha <- read_excel("Challenger.xlsx")
cha

str(cha)
summary(cha)

# a) ¿Se puede afirmarque la temperatura influye en la probabilidad de que
# los propulsores tengan defectos? Qué test utilizó para responder? Justifique.

ggplot(cha, aes(x = Temperature, y = Incident, color=Incident)) +
  geom_point() +
  labs(x = "Temperatura (°F)", y = "Probabilidad de defectos",
       title = "Relación entre temperatura y probabilidad de defectos")

ggplot(cha, aes(x = Temperature, fill = factor(Incident))) +
  geom_bar(position = "fill") +
  labs(x = "Temperatura (°F)", y = "Proporción de defectos",
       title = "Proporción de defectos en función de la temperatura")
# Interpretacon: Graficamente se ve que a medida que la temperatura aumenta, la probabilidad de que los propulsores tengan defectos aumenta.

# Test de significancia estadistica usando Regresion Logistica

modelo <- glm(Incident ~ Temperature, data = cha, family = "binomial")
modelo
summary(modelo)

temp_vals <- seq(min(cha$Temperature), max(cha$Temperature), length.out = 100)

# Predecir las probabilidades utilizando el modelo ajustado
pred_probs <- predict(modelo, newdata = data.frame(Temperature = temp_vals), type = "response")

# Crear un gráfico de curva de probabilidad
ggplot() +
  geom_line(aes(x = temp_vals, y = pred_probs), color = "blue", size = 1) +
  geom_point(data = cha, aes(x = Temperature, y = Incident), color = "red", shape = 19, size = 3) +
  labs(x = "Temperatura (°F)", y = "Probabilidad de defectos",
       title = "Relación entre temperatura y probabilidad de defectos") +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal()

# Interpretacion: El coeficiente (estimate) estimado -0.23 indica que hay una relacion levemente negativa entre
# la temperatura y la probabilidad de que los propulsores tengan defectos. El p-valor del test de Wald es 0.032,
# por lo que se rechaza la hipotesis nula de que el coeficiente estimado sea 0. 
# Por lo tanto, se puede afirmar que la temperatura influye en la probabilidad de
# que los propulsores tengan defectos.
# Las medidas de desviacion (Null y Residual deviance) pueden ser usados para 
# evaluar la bondad de ajuste del modelo. 
# El null deviance mide cuan bien el modelo se ajusta a los datos cuando solo se incluye el intercepto.
# El residual deviance mide cuan bien el modelo se ajusta a los datos cuando se incluye la 
# variable temperatura en el modelo. 
# El AIC mide la calidad del ajuste del modelo, penalizando por la cantidad de parametros 
# estimados. Un valor bajo indica un buen ajuste. En este caso es bajo por lo que el modelo
# se ajusta bien a los datos.

# b) Interprete el valor del coeficiente estimado para temperatura en el contexto del problema.

# Interpretacion: Por cada aumento de 1 unidad de temperatura (°F), la probabilidad de que los 
# propulsores tengan defectos disminuye en 0.23. Es decir, a medida que la temperatura aumenta,
# la probabilidad de que los propulsores tengan defectos disminuye.

coef(modelo)

# c) Para qué valores de temperatura la probabilidad estimada de que se produzcan defectos 
# supera 0.1? y 0.5?

# Calcular la probabilidad estimada para cada valor de temperatura
pred_probs <- predict(modelo, newdata = data.frame(Temperature = cha$Temperature), type = "response")
pred_probs
# Encontrar los valores de temperatura para los cuales la probabilidad estimada supera 0.1 y 0.5
temp_gt_0.1 <- cha$Temperature[pred_probs > 0.1]
temp_gt_0.5 <- cha$Temperature[pred_probs > 0.5]

# Mostrar los resultados
cat("Para una probabilidad estimada superior a 0.1, los valores de temperatura son:", paste(temp_gt_0.1, collapse = ", "), "\n")
cat("Para una probabilidad estimada superior a 0.5, los valores de temperatura son:", paste(temp_gt_0.5, collapse = ", "), "\n")

# Crear el gráfico de curva de probabilidad con líneas adicionales
ggplot() +
  geom_line(data = cha, aes(x = Temperature, y = Probability), color = "blue", size = 1) +
  geom_point(data = cha, aes(x = Temperature, y = Incident), color = "red", shape = 19, size = 3) +
  geom_vline(xintercept = min(temp_gt_0.1), linetype = "dashed", color = "green") +
  geom_vline(xintercept = min(temp_gt_0.5), linetype = "dashed", color = "purple") +
  geom_text(aes(x = min(temp_gt_0.1), y = 0.1, label = round(min(temp_gt_0.1), 2)), vjust = -1, color = "green") +
  geom_text(aes(x = min(temp_gt_0.5), y = 0.5, label = round(min(temp_gt_0.5), 2)), vjust = -1, color = "purple") +
  labs(x = "Temperatura (°F)", y = "Probabilidad de defectos",
       title = "Relación entre temperatura y probabilidad de defectos") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()



# Otra forma 

temperature_0.1 <- cha$Temperature[which.min(abs(predict(modelo, type = "response") - 0.1))]
temperature_0.1
temperature_0.5 <- cha$Temperature[which.min(abs(predict(modelo, type = "response") - 0.5))]
temperature_0.5

# 5.2  

diabetes <- read_excel("diabetes.xls")
diabetes

str(diabetes)
summary(diabetes)

diabetes$DIABET <- factor(diabetes$DIABET)

# a) Obtenga los box-plots para la variable SSPG para los grupos con y sin
# diabetes. Compare los valores medios de ambos grupos y comente.

ggplot(diabetes, aes(x = DIABET, y = SSPG, fill = DIABET)) +
  geom_boxplot() +
  labs(x = "Diabetes", y = "SSPG",
       title = "Boxplots de SSPG para los grupos con y sin diabetes")


ggpairs(diabetes, columns = 1:4, ggplot2::aes(colour = DIABET), lower = list(continuous = wrap("smooth", method = "lm", se = FALSE)))

# Calcular los valores medios de SSPG para los grupos con y sin diabetes
media_con_diabetes <- mean(diabetes$SSPG[diabetes$DIABET == 1])
media_sin_diabetes <- mean(diabetes$SSPG[diabetes$DIABET == 0])
media_con_diabetes
media_sin_diabetes

# Interpretacion: Se ve en los boxplots que claramente las personas con diabetes tienen en promedio un 43%
# mas de SSPG (glucosa plasmatica en estado estacionario) que las personas sin diabetes.

# b) Obtenga el diagrama de dispersión de los valores observados de la variable respuesta (en el eje vertical) y la variable SSPG.

ggplot(diabetes, aes(x = SSPG, y = DIABET,color=DIABET)) +
  geom_point() +
  labs(x = "SSPG", y = "Variable respuesta - DIABET") +
  theme_bw()

# c) Construya una tabla que contenga para cada grupo etáreo la media de la
# edad y la media de DIABET, que corresponde a la proporción de individuos que tienen DIABET en cada grupo.
# Analice.

#diabetes$DIABET <- as.numeric(diabetes$DIABET)

media_diabet_por_grupo <- aggregate(DIABET ~ GROUP, data = diabetes, FUN = mean)
media_diabet_por_grupo

# Otra forma

max_sspg <- max(diabetes$SSPG)

breaks <- seq(0, max_sspg, length.out = 11)

group_sspg <- cut(diabetes$SSPG, breaks = breaks, labels = FALSE, include.lowest = TRUE)

mean_sspg <- tapply(diabetes$SSPG, group_sspg, mean)

proportion_diabet <- tapply(diabetes$DIABET, group_sspg, mean)

output_table <- data.frame(
  Grupos = paste0(breaks[-length(breaks)], "-", breaks[-1]),
  Media_SSPG = round(mean_sspg, 2),
  Propor_DIABET = round(proportion_diabet, 2)
)

print(output_table)

# d) Ajuste un modelo para estimar diabet en función de SSPG.

#diabetes$DIABET <- factor(diabetes$DIABET)
#diabetes$DIABET
modelo2 <- glm(DIABET ~ SSPG, data = diabetes, family = "binomial")
modelo2
summary(modelo2)


rango_sspg <- seq(min(diabetes$SSPG), max(diabetes$SSPG), length.out = 100)

prob_pred <- predict(modelo2, newdata = data.frame(SSPG = rango_sspg), type = "response")


ggplot(diabetes, aes(x = SSPG, y = DIABET)) +
  geom_point() +
  geom_line(data = data.frame(SSPG = rango_sspg, DIABET = prob_pred), aes(x = SSPG, y = DIABET), color = "red") +
  xlab("SSPG") +
  ylab("Probabilidad de ser diabético") +
  ggtitle("Relación entre SSPG y probabilidad de ser diabético")

# e) Interprete los coeficientes obtenidos en términos del problema planteado
summary(modelo2)
coef(modelo2)

# Interpretacion: Por cada aumento de 1 unidad de SSPG (glucosa plasmica en estado estacionario), 
# la probabilidad de tener diabetes es de 0.02 Es decir, a medida que el nivel de glucosa plasmica aumenta,
# la probabilidad de tener diabetes tambien aumenta.

# Además, los valores p asociados a los coeficientes son muy pequeños (p < 0.001), lo que indica que ambos
# coeficientes son estadísticamente significativos y sugieren que existe una relación significativa entre 
# SSPG y la probabilidad de ser diabético.

# f) Para una persona con SSPG igual a 100, ¿qué valores de logit, odds y la
# probabilidad de tener DIABET estima el modelo? Calcúlelos.

SSPG_valor <- 100



logit <- coef(modelo2)[1] + coef(modelo2)[2] * SSPG_valor
logit
# logit estimado = -4,548 + 0,025 * SSPG


odds <- exp(logit)
odds

prob <- odds / (1 + odds)
prob

cat("Logit:", logit, "\n")
cat("Odds:", odds, "\n")
cat("Probabilidad de tener Diabetes: %", round(prob*100,2), "\n")

SSPG_value<-100


# Metodo mas rapido:
probabilidad <- predict(modelo2, data.frame(SSPG=SSPG_value), type = "response")
cat("Probabilidad de tener Diabets: %", round(probabilidad*100,2), "\n")

# g) Halle un intervalo de confianza del 95 % del Odds Ratio para DIABET. Interprete.

intervalo_conf <- confint(modelo2, level = 0.95)
intervalo_conf

# Interpretacion: El intervalo de confianza del 95% para el Odds Ratio esta entre 0.01 y 0.03.
# Esto significa que con un nivel de confianza del 95%, esperamos que el Odds Ratio de la variable DIABET
# aumente entre 0.01 y 0.03 unidades por cada aumento de 1 unidad de SSPG.
# Interpretando el IC podemos decir que existe evidencia estadistica de que el Odds Ratio de DIABET esta 
# influenciado por la variable SSPG. El intervalo no incluye el valor 1, lo que indica que hay una asociación    significativa 
# entre SSPG y la probabilidad de tener diabetes. Además, dado que el intervalo está por debajo de 1, 
# sugiere que a medida que el valor de SSPG aumenta, las probabilidades de tener diabetes disminuyen.

nuevos_puntos <- seq(from = min(diabetes$SSPG), to = max(diabetes$SSPG), by = 0.5) 

predicciones <- predict(modelo2, data.frame(SSPG = nuevos_puntos), se.fit = TRUE) 

predicciones_logit <- exp(predicciones$fit)/(1 + exp(predicciones$fit))

limite_inferior <- predicciones$fit - 1.96 * predicciones$se.fit 
limite_inferior_logit <- exp(limite_inferior)/(1 + exp(limite_inferior))

limite_superior <- predicciones$fit + 1.96 * predicciones$se.fit 
limite_superior_logit <- exp(limite_superior)/(1 + exp(limite_superior))

datos_curva <- data.frame(SSPG = nuevos_puntos, 
                          proba_SSPG = predicciones_logit, 
                          limite_inferior_logit = limite_inferior_logit, 
                          limite_superior_logit = limite_superior_logit)

ggplot(diabetes, aes(x = SSPG, y = DIABET)) + 
  geom_point(aes(color = as.factor(DIABET)),shape = "I", size = 3) + 
  geom_line(data = datos_curva, aes(y = proba_SSPG), color = "brown") + 
  geom_line(data = datos_curva, aes(y = limite_inferior_logit), linetype = "dashed") + 
  geom_line(data = datos_curva, aes(y = limite_superior_logit), linetype = "dashed") + 
  theme_bw() + 
  labs(title = "Modelo regresión logística DIABET ~ SSPG", y = "P(Diabetes | SSPG)", y = "DIABET") + 
  theme(legend.position = "null") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("lightgreen", "lightpink"))

# h) Evalúe la bondad del ajuste obtenido. Para ello construya una matriz de
# confusión con un conjunto de validación del 30 % de los datos utilizando
# como punto de corte pb = 0,5. Hágalo luego con otros puntos de corte.

# Para evaluar la bondad se puede construir una matriz de confusion usando un conjunto de validacion.

set.seed(123)  # Para reproducibilidad
index <- createDataPartition(diabetes$DIABET, p = 0.7, list = FALSE)
datos_entrenamiento <- diabetes[index, ]
datos_validacion <- diabetes[-index, ]

# Ajustar el modelo en los datos de entrenamiento
modelo3 <- glm(DIABET ~ SSPG, data = datos_entrenamiento, family = binomial)

# Realizar predicciones en los datos de validación
prob_pred <- predict(modelo3, newdata = datos_validacion, type = "response")
predicciones <- factor(ifelse(prob_pred >= 0.5, 1, 0), levels = c(0, 1))
datos_validacion$DIABET <- factor(datos_validacion$DIABET, levels = c(0, 1))
# Construir la matriz de confusión
matriz_confusion <- confusionMatrix(data = predicciones, reference = datos_validacion$DIABET)
matriz_confusion

matriz_confusion_visual <- matriz_confusion$table

# Configurar los niveles de las filas y columnas
rownames(matriz_confusion_visual) <- c("Predicción No Diabético", "Predicción Diabético")
colnames(matriz_confusion_visual) <- c("Observación No Diabético", "Observación Diabético")

# Crear el heatmap de la matriz de confusión
heatmap(matriz_confusion_visual, col = heat.colors(256), scale = "none", margins = c(5, 5), 
        main = "Matriz de Confusión",
        xlab = "Observación",
        ylab = "Predicción",
        cex.main = 1.2,
        cex.axis = 0.8,
        cex.lab = 0.9,
        labCol = matriz_confusion_visual,
        labRow = matriz_confusion_visual)


# i) Realizar el test de Hosmer –Lemeshow y comentar los resultados obtenidos.

hoslem.test(diabetes$DIABET, fitted(modelo2))

# Interpretación: El valor p obtenido es 0.8595, que es mayor que el nivel de significancia
# típico de 0.05. Esto indica que no hay suficiente evidencia para rechazar la hipótesis nula de que 
# no hay diferencias significativas entre las frecuencias observadas y esperadas. Por lo tanto, se concluye 
# que el modelo ajustado tiene un buen ajuste a los datos y no hay discrepancias significativas entre las 
# frecuencias observadas y esperadas.
# En resumen, el test de Hosmer-Lemeshow indica que el modelo de regresión logística ajustado tiene
# un buen ajuste a los datos y es capaz de predecir la variable respuesta (DIABET) de manera adecuada.

# j) Estime el área bajo la curva ROC y explique el resultado.
roc_auc <- roc(diabetes$DIABET, fitted(modelo2))
auc <- auc(roc_auc)
auc

plot(roc_auc, main = "Curva ROC", print.thres = "best")

# Con datos de entrenamiento y validacion 

set.seed(123)  # Establecer semilla para reproducibilidad
indices_entrenamiento <- sample(1:nrow(diabetes), nrow(diabetes) * 0.7)
datos_entrenamiento <- diabetes[indices_entrenamiento, ]
datos_validacion <- diabetes[-indices_entrenamiento, ]

# Ajustar el modelo de regresión logística utilizando los datos de entrenamiento
modelo_entrenamiento <- glm(DIABET ~ SSPG, family = "binomial", data = datos_entrenamiento)

# Calcular la curva ROC utilizando los datos de validación
roc_validacion <- roc(datos_validacion$DIABET, predict(modelo_entrenamiento, newdata = datos_validacion, type = "response"))

# Graficar la curva ROC
plot(roc_validacion, main = "Curva ROC - Datos de Validación", print.thres = "best")

# Interpretacion: El valor del AUC oscila entre 0 y 1. Un valor de 0.5 indica un rendimiento similar al azar, 
# mientras que un valor cercano a 1 indica una alta capacidad de discriminación del modelo.
# En este caso, un AUC de 0.9156 indica que el modelo tiene un buen rendimiento en la 
# clasificación de los casos positivos y negativos. Cuanto más cerca esté el valor del AUC de 1, mejor 
# será la capacidad predictiva del modelo.

# 5.3

bajo_peso <- read_excel("bajo_peso.xlsx")
bajo_peso

str(bajo_peso)
summary(bajo_peso)

# a) Estudie la relación entre bajo peso al nacer (LOW =1) y fumar durante el embarazo (SMOKE=1) mediante 
# un modelo logístico.

modelo_peso <- glm(LOW ~ SMOKE, data = bajo_peso, family = "binomial")
summary(modelo_peso)

temp_vals3 <- seq(min(bajo_peso$SMOKE), max(bajo_peso$SMOKE), length.out = 100)

# Predecir las probabilidades utilizando el modelo ajustado
pred_probs3 <- predict(modelo_peso, newdata = data.frame(SMOKE = temp_vals3), type = "response")

# Crear un gráfico de curva de probabilidad
ggplot() +
  geom_line(aes(x = temp_vals3, y = pred_probs3), color = "blue", size = 1) +
  geom_point(data = bajo_peso, aes(x = SMOKE, y = LOW), color = "red", shape = 19, size = 3) +
  labs(x = "SMOKE", y = "LOW",
       title = "Relación entre fumar y peso") +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal()

# b) Escriba la expresión del modelo ajustado e interprete los coeficientes. Es
# significativa la variable smoke? Básese en el test de Wald para responder a esta pregunta.


# logit(p) representa la función logit inversa de la probabilidad p de bajo peso al nacer.

# β₀ es el coeficiente de intersección (intercepto) del modelo.

# β₁ es el coeficiente asociado a la variable SMOKE

# Obtener los coeficientes del modelo
coeficientes <- coef(modelo_peso)
coeficientes
expresion_modelo <- paste("logit(p) = ", coeficientes[1], " + ", coeficientes[2], "*SMOKE")
expresion_modelo
# Interpretación de los coeficientes
intercepto <- coeficientes[1]
coef_smoke <- coeficientes[2]

interpretacion_intercepto <- paste("El valor del intercepto es", round(intercepto, 3), "y representa el logaritmo de la razón de probabilidades de bajo peso al nacer cuando SMOKE es igual a 0 (no fuma).")
interpretacion_coef_smoke <- paste("El coeficiente de SMOKE es", round(coef_smoke, 3), "y representa el cambio en el logaritmo de la razón de probabilidades de bajo peso al nacer por cada unidad de incremento en SMOKE, manteniendo todas las demás variables constantes.")

# Test de Wald para la significancia de la variable SMOKE
test_wald <- summary(modelo_peso)$coef["SMOKE", "Pr(>|z|)"]
if (test_wald < 0.05) {
  significativa <- "sí"
} else {
  significativa <- "no"
}

# Imprimir los resultados
cat("Expresión del modelo ajustado:", expresion_modelo, "\n\n")
cat("Interpretación de los coeficientes:\n")
cat(interpretacion_intercepto, "\n")
cat(interpretacion_coef_smoke, "\n\n")
cat("¿La variable SMOKE es significativa? ", significativa, " (p < 0.05)")

# c) Construya la matriz de confusión para un conjunto de validación de un tercio de la base.

# Paso 1: Dividir los datos en conjunto de entrenamiento y conjunto de validación
set.seed(666)  # Fijar una semilla para reproducibilidad
indices_validacion <- sample(nrow(bajo_peso), round(0.33 * nrow(bajo_peso)))
datos_entrenamiento <- bajo_peso[-indices_validacion, ]
datos_validacion <- bajo_peso[indices_validacion, ]

modelo_peso2 <- glm(LOW ~ SMOKE, data = datos_entrenamiento, family = "binomial")

# Paso 2: Aplicar el modelo logístico ajustado al conjunto de validación
predicciones_validacion <- predict(modelo_peso2, newdata = datos_validacion, type = "response")
umbral <- 0.4  # Umbral de clasificación

# Paso 3: Construir la matriz de confusión
clasificacion2 <- ifelse(predicciones_validacion >= umbral, 1, 0)
matriz_confusion2 <- table(observado=datos_validacion$LOW, predicho=clasificacion2)
# IMPORTANTE: SINO DA LA MATRIZ DE CONFUSION CAMBIAR SEED Y UMBRAL (O CORTE)

# Imprimir la matriz de confusión
cat("Matriz de Confusión:\n")
print(matriz_confusion2)

mosaic(matriz_confusion2, shade = T, colorize = T, gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

# d) Testee basándose en la verosimilitud este modelo versus otro que considere también la edad de la madre. Interprete los resultados.

modelo_peso3 <- glm(LOW ~ SMOKE + AGE, data = datos_entrenamiento, family = "binomial")

probs_validacion3 <- predict(modelo_peso3, newdata = datos_validacion, type = "response")

umbral2 <- 0.5

predicciones_validacion3 <- ifelse(probs_validacion3 >= corte, 1, 0)

matriz_confusion2 <- table(observado = datos_validacion$LOW, predicho = predicciones_validacion3)

print(matriz_confusion2)

mosaic(matriz_confusion2, shade = TRUE, colorize = TRUE,
       xlab = "Predicciones", ylab = "Observado",
       main = "Matriz de Confusión",
        gp = gpar(fill = matrix(c("darkgreen", "darkred", "darkred", "darkgreen"),2,2)))


# Test de Verosimilitud
lrtest<-lrtest(modelo_peso2, modelo_peso3)
lrtest

# Interpretacion: Como el valor p (0.09787) es mayor que el nivel de significancia de 0.05, no hay suficiente evidencia
# para concluir que el modelo 2 (que incluye "edad de la madre") es significativamente mejor que el modelo 1 (que solo 
# considera "fumar durante el embarazo"). Esto significa que no se encontró una mejora significativa en la capacidad de
# explicar la variable objetivo (bajo peso al nacer) al incluir la variable "edad de la madre" en el modelo.

# MODELO MULTIVARIADO

# 5.4

tormenta <- read_excel("tormenta.xlsx")
tormenta

str(tormenta)
summary(tormenta)

ggpairs(tormenta)
ggpairs(tormenta, ggplot2::aes(colour = factor(murio)), lower = list(continuous = wrap("smooth", method = "lm", se = FALSE)))

# a) Hay alguna especie que le parece que sobrevivió más que otra? Considera que la supervivencia se 
# asocia con la severidad de la tormente? y con el diámetro del árbol?

# Especie vs supervivencia

ggplot(tormenta, aes(x = especie, fill = factor(murio))) +
  geom_bar(position = "stack") +
  geom_text(aes(label = stat(count)), stat = "count", position = position_fill(vjust = 0.5)) +
  labs(x = "Especies", y = "Conteo") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Sobrevivio", "Murio")) +
  theme_minimal()

ggplot(tormenta, aes(x = especie, fill = factor(murio))) +
  geom_bar(position = "fill") +
  geom_text(aes(label = stat(count)), stat = "count", position = position_fill(vjust = 0.5)) +
  labs(x = "Especies", y = "Conteo") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Sobrevivio", "Murio")) +
  theme_minimal()


# Interpretacion: En proporcion sobrevivieron mas los arboles de la especie BA, C, PB y RM que la del resto.

# Supervivencia vs severidad
boxplot(tormenta$severidad ~ tormenta$murio, data = tormenta, 
        xlab = "Supervivencia", ylab = "Severidad",
        main = "Supervivencia en relación con la severidad",
        names = c("Sobrevivió", "Murió"),
        col = c("lightblue", "lightpink"))

# Interpreatacion: Claramente los arboles que sobrevivieron fueron los que estuvieron en zonas con menor severidad de tormenta.

# Supervivence vs diametro

boxplot(tormenta$diametro ~ tormenta$murio, data = tormenta, 
        xlab = "Supervivencia", ylab = "Diametro",
        main = "Supervivencia en relación con el diametro",
        names = c("Sobrevivió", "Murió"),
        col = c("lightblue", "lightpink"))

ggplot(data = tormenta, aes(x = especie, y = diametro)) + 
  geom_boxplot(aes(fill = factor(murio)), width = 0.8) + 
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  theme_bw()

# Interpretacion: En promedio, los arboles con menor diametro sobrevivieron mas que los de mayor diametro. 
# Esto tambien se cumple por especie

# b) Proponga un modelo que sólo utilice como predictora la variable diámetro. Halle la bondad de ajuste y la 
# precisión de la predicción lograda.

modelo_torm <- glm(murio ~ diametro, data = tormenta, family = "binomial")
summary(modelo_torm)

temp_vals4 <- seq(min(tormenta$diametro), max(tormenta$diametro), length.out = 100)

# Predecir las probabilidades utilizando el modelo ajustado
pred_probs4 <- predict(modelo_torm, newdata = data.frame(diametro = temp_vals4), type = "response")

# Crear un gráfico de curva de probabilidad
ggplot() +
  geom_line(aes(x = temp_vals4, y = pred_probs4), color = "blue", size = 1) +
  geom_point(data = tormenta, aes(x = diametro, y = murio), color = "red", shape = 20, size = 3) +
  labs(x = "diametro", y = "murio",
       title = "Relación entre diametro y supervivencia") +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal()

# Bondad de ajuste
pred <- predict(modelo_torm,type="response")
hoslem.test(tormenta$murio, pred)

# Interpretacion: El p-value es extremadamente pequeño, por lo tanto, se rechaza la H0 y se concluye 
# que el modelo no se ajusta bien a los datos. 

# Precision prediccion lograda

predicciones_binarias <- ifelse(pred >= 0.5, 1, 0)

matriz_confusion3 <- table(tormenta$murio, predicciones_binarias)
matriz_confusion3

mosaic(matriz_confusion3, shade = T, colorize = T, gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))


precision <- matriz_confusion3[2, 2] / sum(matriz_confusion3[, 2])
recall <- matriz_confusion3[2, 2] / sum(matriz_confusion3[2, ])
precision
recall

medidas_precision <- performance(etiquetas_predichas, "prec", "rec")

plot(medidas_precision, main = "Curva de Precisión y Recuperación", colorize = TRUE)

# c) Proponga un segundo modelo que considere como predicotras al diámetro y a la severidad de la tormenta.
# Halle la bondad de ajuste y la precisión de la predicción lograda.

modelo_torm2 <- glm(murio ~ diametro + severidad, data = tormenta, family = "binomial")
summary(modelo_torm2)

data_pred2 <- data.frame(observed = tormenta$murio,
                        predicted = predict(modelo_torm2, type = "response"))

# Crear el gráfico de regresión logística
ggplot(data_pred2, aes(x = predicted, y = observed)) +
  geom_point(shape = 1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Valores predichos", y = "Valores observados") +
  theme_minimal()

# Bondad de ajuste
pred2 <- predict(modelo_torm2,type="response")
hoslem.test(tormenta$murio, pred2)

# Interpretacion: El p-value es de 0.4 > 0.05 , por lo tanto, no se rechaza la H0 y se concluye que el modelo se ajusta bien a los datos.

# Precision prediccion lograda

predicciones_binarias2 <- ifelse(pred2 >= 0.5, 1, 0)

matriz_confusion4 <- table(tormenta$murio, predicciones_binarias2)
matriz_confusion4

mosaic(matriz_confusion4, shade = T, colorize = T, gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

precision2 <- matriz_confusion4[2, 2] / sum(matriz_confusion4[, 2])
recall2 <- matriz_confusion4[2, 2] / sum(matriz_confusion4[2, ])
precision2
recall2

medidas_precision2 <- performance(etiquetas_predichas2, "prec", "rec")

plot(medidas_precision2, main = "Curva de Precisión y Recuperación", colorize = FALSE)

# Interpretacion: Comparando los dos modelos de regresion logistica ajustados, el modelo 2 
# (que incluye "diametro" y "severidad de la tormenta") tiene un mejor ajuste a los datos que el modelo 1

# d) Compare ambos modelos considerando la verosimilitud, la bondad de ajuste 
# y el área bajo la curva ROC de cada uno.

# Verosimilitud
lrtest<-lrtest(modelo_torm, modelo_torm2)
lrtest

# Interpretacion: La interpretación general es que agregar la variable "severidad" al modelo mejora significativamente su capacidad de ajuste
# y explicación de los datos.

# Curva ROC modelo_torm
etiquetas_predichas <- prediction(pred, tormenta$murio)
medidas <- performance(etiquetas_predichas, "tpr", "fpr")
plot(medidas, main = "Curva ROC", colorize = FALSE)
# Curva ROC modelo_torm2
etiquetas_predichas2 <- prediction(pred2, tormenta$murio)
medidas2 <- performance(etiquetas_predichas2, "tpr", "fpr")
plot(medidas2, col = "blue", lty = 2, add = TRUE)

legend("bottomright", legend = c("Modelo 1", "Modelo 2"), col = c("black", "blue"), lty = c(1, 2))


# e) Estimar la probabilidad de que no sobreviva un árbol cuyo diámetro es
# de 30 cm y esté situado en una zona en la que la fuerza de la tormenta viene dada por S=0.8.

diametro <- 30
severidad <- 0.8

nuevo_datos <- data.frame(Diametro = diametro, Severidad = severidad)

proba.supervivencia <- 1 - predict(modelo_torm2, newdata = nuevo_datos, type = "response")

cat("La probabilidad estimada de que un árbol con diámetro de", diametro, "cm y severidad de", severidad, "no sobreviva es:", proba.supervivencia, "\n")


# f) Compare la precisión del mejor de los modelos con un análisis discriminante lineal y 
# con un análisis discriminante cuadrático.
set.seed(666)
entrenamiento <- sample(1:100,70)
validacion <- c(1:100)[-entrenamiento]

tormenta_train <- tormenta[entrenamiento,]
tormenta_test <- tormenta[validacion,]

modelo_lda <- lda(murio ~ diametro + severidad, data = tormenta_train)
predicciones_lda <- predict(modelo_lda, newdata = tormenta_test)$class

modelo_qda <- qda(murio ~ diametro + severidad, data = tormenta_train)
predicciones_qda <- predict(modelo_qda, newdata = tormenta_test)$class

precision_lda <- mean(predicciones_lda == tormenta_test$murio)
precision_qda <- mean(predicciones_qda == tormenta_test$murio)

cat("Precisión del Modelo 2:", precision, "\n")
cat("Precisión del LDA:", precision_lda, "\n")
cat("Precisión del QDA:", precision_qda, "\n")


# 5.5

pros <- read.csv("prostata.csv",sep=';')
pros

str(pros)

summary(pros)

ggpairs(pros)


# a) Construye un modelo de regresión logística a fin de establecer una relación que permita predecir la presencia de
# invasión de las vesículas seminales.

rl_pros <- glm(invade_vesic_semin ~ ., data = pros, family = "binomial")
summary(rl_pros)



# b) Seleccione la/s variable/s adecuadas para informar cual/es de las variables incluidas en el modelo inicial 
# son significativas.

# Los resultados de la regresion lineal sugieren que la variables adecuadas son penetrac_capsular y log_psa, si usamos 
# como metrica al p-value.

# Otro metodo alternativo ChatGPT

# RL alternativo
# Modelo de regresión logística
model <- glm(invade_vesic_semin ~ volumen_pros + peso_pros + penetrac_capsular + gleason, data = pros, family = binomial)

# Resumen del modelo
summary(model)

# Función para realizar el análisis de deviance
analyze_deviance <- function(model) {
  null_model <- glm(invade_vesic_semin ~ 1, data = pros, family = binomial)
  result <- anova(null_model, model, test = "Chi")
  return(result)
}

# Realizar el análisis de deviance
deviance_analysis <- analyze_deviance(model)

# Visualizar los resultados
print(deviance_analysis)

# Filtrar las variables significativas
significant_vars <- deviance_analysis[deviance_analysis$Pr < 0.05, "term"]

# Construir el modelo final solo con las variables significativas
final_model <- step(model, direction = "backward")

# Resumen del modelo final
summary(final_model)



# c) Escriba la expresión el modelo final si solo se incluyeran en el las variables que resultaron significativas.

# Invade_vesic_semin = β_{0} + β_{1}{penetrac_capsular} + β_{2}{log_psa} 

# d) Pruebe interacciones dobles e incluya alguna en el modelo si resulta significativa.

modelo_interaccion <- glm(invade_vesic_semin ~ penetrac_capsular + log_psa + 
                            penetrac_capsular*log_psa, 
                          data = pros, 
                          family = "binomial")
summary(modelo_interaccion)

modelo_interaccion_todas <- glm(invade_vesic_semin ~ penetrac_capsular + log_psa + 
                            penetrac_capsular*log_psa +
                            volumen_pros*log_psa,
                          data = pros, 
                          family = "binomial")
summary(modelo_interaccion_todas)

# e) Considerando como valor de corte 0.5 cómo clasificaría un individuo que tuvo tiene Gleason 4 y
# penetración capsular.

# Assuming you already have the final_model and the values for the individual
valor_volumen_pros <- -0.5
valor_peso_pros <- 3.2
valor_penetrac_capsular <- 0
gleason <- 4

# Create a new dataframe for the individual
new_data <- data.frame(
  volumen_pros = valor_volumen_pros,
  peso_pros = valor_peso_pros,
  penetrac_capsular = valor_penetrac_capsular,
  gleason = gleason
)

# Predict the probability of invasion of the seminal vesicles
predicted_prob <- predict(final_model, newdata = new_data, type = "response")

# Classify the individual based on the cutoff value of 0.5
classification <- ifelse(predicted_prob >= 0.5, "Present", "Absent")
classification

# 5.6

pima <- Pima.tr
pima

str(pima)
summary(pima)

ggpairs(pima)

ggpairs(pima, ggplot2::aes(colour = type), lower = list(continuous = wrap("smooth", method = "lm", se = FALSE)))

# a) Ajustar un modelo logístico considerando como variable predictora el
# bmi y como respuesta type. Utilizar el test de razón de verosimilitudes
# para evaluar la significación del modelo, comparándolo con el modelo nulo.

modelo_null <- glm(type ~ 1, data = pima, family = binomial)
summary(modelo_null)
pima_model <-glm(type ~ bmi, data = pima, family = "binomial")
summary(pima_model)

lr_test <- anova(modelo_null, pima_model, test = "LRT")
lr_test

# Interpretacion: Basandonos en el p-value < 0.05 de el pima_model podemos decir que el modelo es significativo a diferencia del modelo null-
# La variable bmi tiene un impacto significativo en type.

# b) Defina una variable categórica que separe a las mujeres que no han tenido embarazos previos de las que sí. 
# Ajuste un modelo para evaluar si esta variable es significativa para predecir type.

# Definir variable categórica que indica si ha habido embarazos previos o no
pima$prev_embarazos <- ifelse(pima$npreg > 0, "Ha tenido embarazos", "No ha tenido embarazos")

# Ajustar el modelo con la nueva variable creada y type como respuesta
modelo_prev_embarazos <- glm(type ~ prev_embarazos, data = pima, family = binomial)

# Realizar el test de razón de verosimilitudes
test_lr_prev_embarazos <- anova(modelo_null, modelo_prev_embarazos, test = "LRT")

# Imprimir resultados
print(test_lr_prev_embarazos)

# Interpretacion: Un p-valor mayor que 0.05 (denotado por el código "."), en este caso, indica que el modelo que 
# incluye "prev_embarazos" no es significativamente mejor que el modelo nulo. Por lo tanto, la variable categórica
# "prev_embarazos" no parece tener un impacto significativo en la predicción de "type" en este análisis.

# c) Ajuste un modelo utilizando en este caso como predictoras la edad, la variable categórica definida en el item
# anterior y el bmi.

pima_model2 <- glm(type ~ prev_embarazos + age + bmi, data = pima, family = binomial)
summary(pima_model2)

# Interpretacion: Las variables prev_embarazos no es significativa, pero si lo son age y bmi.

# d) Ajuste un modelo utilizando en este caso como predictoras la edad, el bmi y el número de embarazos previos.

pima_model3 <- glm(type ~ age + bmi + npreg, data = pima, family = "binomial")
summary(pima_model3)

# Interpretacion: Las variables n_prev no es significativa, pero si lo son age y bmi.

# e) Seleccione el mejor modelo mediante el test de razón de verosimilitudes.

lrtest<-lrtest(modelo_null, pima_model, pima_model2, pima_model3,modelo_prev_embarazos)
lrtest

# Interpretacion: Basandonos en el p-value el mejor modelo es el pima_model3 y prima_model2 son mejores que el resto de los modelos.










