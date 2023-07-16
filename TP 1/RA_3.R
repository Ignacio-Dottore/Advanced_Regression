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
#library(chatgpt)
#Sys.setenv(OPENAI_API_KEY = "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")

setwd("C:/Users/idott/Advanced_Regression/Advanced_Regression/TP 1")

# Capitulo 3. MODELOS ALTERNATIVOS

# SELECCION DE VARIABLES

3.1 

b3 <- read_excel("table.b3.xlsx")
b3

colnames(b3)<-c('mpg','displ','pot','torq','comp','ejetr','carb','veltr','lenght','width','weight','trans')
summary(b3)
str(b3)

cor_matrix <- cor(b3)
cor_matrix
corrplot(cor_matrix, method = "color")

# (a) Ajustar el modelo saturado (que contiene a todas las varibles dependientes).
ml_b3 <- lm(mpg~., data = b3)
summary(ml_b3)

# Interpretacion: El estadístico F se utiliza para evaluar la significancia global del modelo. 
# En este caso, el valor F es 8.31 con 11 y 18 grados de libertad, y el valor p asociado es 5.231e-05, 
# lo que sugiere que el modelo en general es estadísticamente significativo.

# (b) Analizar a través del VIF la presencia de multicolinealidad (dos o mas variables tiene alta correlacion)

vif(ml_b3)

# Interpretacion: Los valores del VIF se presentan para cada variable independiente y representan la inflación de 
# la varianza debido a la correlación con otras variables en el modelo.
# Un VIF alto (generalmente superior a 5 o 10) indica una alta correlación entre una variable independiente
# y otras variables del modelo, lo que puede dificultar la interpretación de los coeficientes y la 
# evaluación de la importancia relativa de cada variable.
# En este caso menos la variable compresion, las demas pueden tener problemas de multicolinealidad.

# (c) Realizar una selección de variables foward teniendo en cuenta el criterio de Akaike.

model_forward <- stepAIC(ml_b3, direction = "forward", trace = FALSE)
summary(model_forward)


# Otro metodo:

model_forward_2 <- ols_step_forward_aic(ml_b3)
model_forward_2

summary(model_forward_2$model)


# (d) Escribir la expresión del modelo seleccionado. Realizar un análisis diagnóstico del mismo.

modelo_seleccionado <- lm(mpg ~ pot + torq + ejetr + carb + veltr + weight, data = b3)
print(modelo_seleccionado)
par(mfrow = c(2, 2))
plot(modelo_seleccionado)

shapiro.test(modelo_seleccionado$residuals) # Test de Shapiro-Wilk

bptest(modelo_seleccionado) # Test de Breusch-Pagan

dwt(modelo_seleccionado) # Test de Durbin-Watson

# (e) Realizar una selección backward teniendo en cuenta el criterio de R2
# ajustado. Se selecciona el mismo modelo?

model_backward <- stepAIC(ml_b3, direction = "backward", trace = FALSE)
summary(model_backward)

anova(model_forward, model_backward)

# (f) Utilizando la función ols_step_all_possible de la biblioteca olsrr creada
# por Hebbali (2020) obtener todas las regresiones posibles. Elegir un único modelo visualizando gráficamente
# los resultados y considerando los criterios BIC, AIC, CP y R2 adj.

# Instalar la biblioteca "olsrr" si no está instalada

model_all_possible <- ols_step_all_possible(ml_b3)
model_all_possible
plot(model_all_possible)
summary(model_all_possible)

# Interpretacion: El eje x representa el numero de predictores en cada modelo y el eje y representa el valor de cada criterio.
# Mientras mas bajo el valor de cada criterio, mejor el ajuste del modelo el modelo. Este representa
# el modelo con la mejor combinacion de predictores dependiendo del criterio elegido.

best_model <- ols_step_best_subset(ml_b3)
best_model
summary(best_model)
plot(best_model)

# Interpretacion: Para ver el mejor modelo tener en cuenta lo siguiente:
# - Que AIC, SBIC o SCB sean lo mas bajo posible. Estos criterios penalizan la complejidad del modelo.
# Por lo que modelos con bajo AIC, SBIC o SCB son tienden a tener un mejor balance entre ajuste y simplicidad.
# - Que el Cp se sea minimo. Si priorizas accuracy eleji un modelo con Cp minimo ya que este mide el accuracy predictivo
# con valores bajos indicando un mejro performance del modelo.
# - Que el R2 y el R2 ajustado sea lo mas alto posible. Estas metricas miden la proporcion de la variacion que explica por el modelo.
# Por todo lo analizado el mejor modelo es ejetr lenght weight con tres variables

# 3.2

fat <- faraway::fat
summary(fat)
str(fat)

cor_matrix2 <- cor(fat)
cor_matrix2
corrplot(cor_matrix2, method = "color")

# a) Hallar el mejor modelo de regresión lineal con variable respuesta brozek
# utilizando entre 1 y 14 variables predictoras. Elegir el mejor considerando el criterio CP de Mallows y R2 adj .

best_model_cp <- regsubsets(brozek ~ ., data = fat, method = "exhaustive", nvmax = 14)
cp_stats <- summary(best_model_cp)$cp
best_model <- which.min(cp_stats)
best_predictors <- names(coef(best_model_cp, id = best_model))
print(best_predictors)

best_model_r2adj <- regsubsets(brozek ~ ., data = fat, method = "exhaustive", nvmax = 14, really.big = TRUE)
r2adj_stats <- summary(best_model_r2adj)$adjr2
best_model <- which.max(r2adj_stats)
best_predictors <- names(coef(best_model_r2adj, id = best_model))
print(best_predictors)

# b) Repetir considerando ahora la minimización del Error Cuadrático Medio del modelo usando validación cruzada leave one out

trainControl <- trainControl(method = "LOOCV")

model_loocv <- train(brozek ~ ., data = fat, method = "lm", trControl = trainControl)
print(model_loocv$finalModel)
print(model_loocv)
plot(model_loocv)
# c) Inspeccionar gráficamente el MSE y decidir cuál es el mejor modelo. Interpretar los coeficientes del mismo.
mse_values <- model_loocv$results$RMSE^2
plot(1:length(mse_values), mse_values, type = "b", xlab = "Number of Predictors", ylab = "Mean Square Error")
best_model_index <- which.min(mse_values)
best_model <- model_loocv$finalModel[[best_model_index]]
print(best_model)


# d) Coinciden las variables de los modelos elegidos con los diferentes criterios
selected_variables <- list()

selected_variables$cp <- names(coef(best_model_cp, id = which.min(summary(best_model_cp)$cp)))
selected_variables$r2adj <- names(coef(best_model_r2adj, id = which.max(summary(best_model_r2adj)$adjr2)))
selected_variables$mse <- names(coef(model_loocv$finalModel))

if (identical(selected_variables$cp, selected_variables$r2adj) && identical(selected_variables$r2adj, selected_variables$mse)) {
  print("The selected variables are the same for all criteria.")
} else {
  print("The selected variables differ between the criteria.")
}

# REVISAR

# MODELOS DE REGULARIZACION

# 3.3

longley <- longley
longley

str(longley)
summary(longley)
# 1. Ajustar un modelo de Ridge para la variable respuesta Employed.

x <- model.matrix(Employed ~ ., data = longley)[,-1]
head(x)

y <- longley$Employed

modelos_ridge <- glmnet(x = x, y = y, alpha = 0)

plot(modelos_ridge, xvar = "lambda", label = TRUE)

cv_error_ridge <- cv.glmnet(x = x, y = y, alpha = 0, nfolds = 10, type.measure = "mse") 
plot(cv_error_ridge)

coefpath(modelos_ridge)

# Interpretacion: A coefficient path plot displays the coefficients of the variables in the model as a function of the regularization parameter (often denoted as lambda). The plot shows how the coefficients change as the regularization parameter varies. 
# It helps in understanding the impact of regularization on the model's coefficients.

coefplot(modelos_ridge,lambda=cv_error_ridge$lambda.min,col="blue",sort='magnitude') + theme_bw()


# Valor lambda con el que se consigue el minimo test-error.
cv_error_ridge$lambda.min

# Valor lambda optimo: mayor valor de lambda con el que el test-error no se aleja más de
# 1 sd del mínimo test-error posible.

cv_error_ridge$lambda.1se

modelo_final_ridge <- glmnet(x = x, y = y, alpha = 0, lambda = 0.5844638) 
coef(modelo_final_ridge)


# 2. Ajustar un modelo de Lasso para la variable respuesta Employed.
modelos_lasso <- glmnet(x = x, y = y, alpha = 1) 
plot(modelos_lasso, xvar = "lambda", label = TRUE)

coefpath(modelos_lasso)
coefplot(modelos_lasso,lambda=cv_error_lasso$lambda.min,col="blue",sort='magnitude') + theme_bw()

cv_error_lasso <- cv.glmnet(x = x, y = y, alpha = 1, nfolds = 10) 
plot(cv_error_lasso)

cv_error_lasso$lambda.min

cv_error_lasso$lambda.1se

modelo_final_lasso <- glmnet(x=x, y=y, alpha=1,lambda=cv_error_lasso$lambda.1se) 
coef(modelo_final_lasso)

# 3. Ajustar un modelo de Elastic Net para la variable respuesta Employed.
modelos_elastic_net <- glmnet(x = x, y = y, alpha = 0.5)

plot(modelos_elastic_net, xvar = "lambda", label = TRUE)

cv_error_elastic_net <- cv.glmnet(x = x, y = y, alpha = 0.5, nfolds = 10) 
plot(cv_error_elastic_net)

cv_error_elastic_net$lambda.min

cv_error_elastic_net$lambda.1se

coefpath(modelos_elastic_net)

coefplot(modelos_elastic_net,lambda=cv_error_elastic_net$lambda.min,col="blue",sort='magnitude') + theme_bw()

modelo_final_elastic_net <- glmnet(x=x, y=y, alpha=1,lambda=cv_error_elastic_net$lambda.1se) 
coef(modelo_final_elastic_net)

# 4. Comparar los resultados obtenidos en los tres modelos.

compare_models <- data.frame(Ridge = coef(modelo_final_ridge)[-1],
                             Lasso = coef(modelo_final_lasso)[-1],
                             Elastic_Net = coef(modelo_final_elastic_net)[-1])

print(compare_models)


train_ind <- sample(1:nrow(longley), size = 0.75*(nrow(longley)))
train <- longley[train_ind,]
test <- longley[-train_ind,]

lm_longley <- lm(Employed ~ . ,data=train )
summary(lm_longley)
lm_pred <- predict(lm_longley,new_data=test)
lm_mse <- mean((lm_pred - test$Employed)**2)
lm_mse

tmatrix <- model.matrix(Employed ~ .,test)[,-1]


ridge_cv=cv.glmnet(x, y, alpha=0, type='mse', family='gaussian',
standardize=TRUE, nfolds=10)
ridge_pred = predict(modelos_ridge, S=ridge_cv$lambda.min,newx=tmatrix)
ridge_mse = mean((ridge_pred - test$Employed)**2)

lasso_cv=cv.glmnet(x, y, alpha=1, type='mse', family='gaussian',
standardize=TRUE, nfolds=10)
lasso_pred = predict(modelos_lasso, S=lasso_cv$lambda.min,newx=tmatrix)
lasso_mse = mean((lasso_pred - test$Employed)**2)

elastic_net_cv=cv.glmnet(x, y, alpha=0.5, type='mse', family='gaussian',
standardize=TRUE, nfolds=10)
elastic_pred = predict(modelos_elastic_net, S=elastic_net_cv$lambda.min,newx=tmatrix)
elastic_mse = mean((elastic_pred - test$Employed)**2)

metodo <- c("RIDGE","LASSO","ELASTIC NET")
test_mse <- c(ridge_mse,lasso_mse,elastic_mse)
resultados <- data.frame(metodo,test_mse)
resultados

ggplot(data = resultados, aes(x = reorder(metodo, test_mse), y = sqrt(test_mse))) + 
  geom_bar(stat = "identity") + 
  labs(x = "Método de regresión", y = expression(sqrt("test MSE"))) + theme_bw()


# 3.4

prostata <- read.csv("prostata.csv",sep =";")
prostata
str(prostata)
summary(prostata)


# a) Considerando la variable respuesta lpsa, ajustar un modelo lineal utilizando como predictoras a todas las demás. Qué inconveniente tiene este
# modelo?.

lr <- lm(log_psa ~ ., data=prostata)
summary(lr)

matrix <- cor(prostata)
corrplot(matrix, method = "color")

plot(lr,which=2)

# El inconveniente con este modelo es que hay muchos coeficientes insignificantes con un p-value mayor a 0.05 

# b) Aplicar un método de selección de variables utilizando como criterio BIC. Qué variables quedaron? Coinciden con el OLS?.

criterios <- regsubsets(log_psa~.,data = prostata, nvmax = 8)
summary(criterios)
metricas <- as.data.frame(cbind(seq(1:length(summary(criterios))),
  summary(criterios)$bic,
  summary(criterios)$rsq,
  summary(criterios)$adjr2,
  summary(criterios)$cp)
)
colnames(metricas) <- c('Predictores','BIC','R2','R2Adj','CP')
summary(metricas)
metricas

which.min( summary(criterios )$bic )

variables_bic <- names(coef(modelo_seleccionado))
variables_bic

coeficientes_ols <- coef(lr)[variables_bic]
print(variables_bic)
print(coeficientes_ols)

p <- ggplot(data = data.frame(n_predictores = 1:8, 
            BIC = summary(criterios)$bic), 
            aes( x = n_predictores, y = BIC) ) + 
            geom_line() + 
            geom_point()

p <- p + geom_point( aes( x=n_predictores[which.min(summary(criterios)$bic)], 
                          y=BIC[which.min(summary(criterios)$bic)]), 
                          colour = "red", size = 3 )

p <- p + scale_x_continuous(breaks = c(0:14)) + theme_bw() + 
  labs(title = "BIC vs Número de predictores", x = "Número predictores", y = "BIC")
p

selected_variables <- names(coef(criterios, id = which.min(summary(criterios)$bic)))
selected_variables

# Si coinciden las variables.

# c) Ajustar ahora modelos regularizados y comparar los resultados y coeficientes utilizando CV

train_ind2 <- sample(1:nrow(prostata), size = 0.75*(nrow(prostata)))
train2 <- prostata[train_ind2,]
test2 <- prostata[-train_ind2,]

x2 <- model.matrix(log_psa ~ ., data = prostata)[,-1]

y2<- prostata$log_psa

lm_reg <- lm(log_psa ~ . ,data=train2 )
summary(lm_reg)
lm_pred <- predict(lr,new_data=test2)
lm_mse <- mean((lm_pred - test2$log_psa)**2)
lm_mse

tmatrix2 <- model.matrix(log_psa ~ .,test2)[,-1]


# Modelo Ridge
ridge_cv2=cv.glmnet(x2, y2, alpha=0, type='mse', family='gaussian',
standardize=TRUE, nfolds=10)
ridge_pred2 = predict(ridge_cv2, S=ridge_cv2$lambda.min,newx=tmatrix2)
ridge_mse2 = mean((ridge_pred2 - test2$log_psa)**2)

# Modelo Lasso
lasso_cv2=cv.glmnet(x2, y2, alpha=1, type='mse', family='gaussian',
standardize=TRUE, nfolds=10)
lasso_pred2 = predict(lasso_cv2, S=lasso_cv2$lambda.min,newx=tmatrix2)
lasso_mse2 = mean((lasso_pred2 - test2$log_psa)**2)

# Modelo Elastic Net
elastic_net_cv2=cv.glmnet(x2, y2, alpha=0.5, type='mse', family='gaussian',
standardize=TRUE, nfolds=10)
elastic_pred2 = predict(elastic_net_cv2, S=elastic_net_cv2$lambda.min,newx=tmatrix2)
elastic_mse2 = mean((elastic_pred2 - test2$log_psa)**2)

# Modelo Full
full <- lm(log_psa ~., data = train2)
full_pred <- predict(full, new_data = test2)
full_mse2 <- mean((full_pred - test2$log_psa)^2)



metodo2 <- c("RIDGE","LASSO","ELASTIC NET","FULL")
test_mse2<- c(ridge_mse2,lasso_mse2,elastic_mse2,full_mse2)
resultados2 <- data.frame(metodo2,test_mse2)
resultados2



# MODELOS BASADOS EN PCA

# 3.5


white <- read.csv("winequality-white.csv", sep = ";", fileEncoding = "WINDOWS-1252")
red <- read.csv("winequality-red.csv", sep = ";", fileEncoding = "WINDOWS-1252")
red <- red[,1:12]

summary(white)
summary(red)
str(white)
str(red)

# a) Realizar un correlograma para el conjunto de variables explicativas. Tiene sentido en este caso un PCA? En caso afirmativo explore las componentes principales.

cor_matrix_white <- cor(white)
cor_matrix_white
corrplot(cor_matrix_white, method = "color")

cor_matrix_red <- cor(red)
cor_matrix_red
corrplot(cor_matrix_red, method = "color")

# Interpretacion: En el caso de white y red wine, hay una moderada correlacion entre las varias variables.
# Por lo que tiene sentido realizar un PCA para reducir la dimensionalidad de los datos.

pca_white <- prcomp(white, scale = TRUE)
pca_white
summary(pca_white)
plot(pca_white, type = "l")

pca_red <- prcomp(red, scale = TRUE)
pca_red
summary(pca_red)
plot(pca_red, type = "l")

# Mejores Graficas
fviz_eig(pca_white, addlabels = TRUE)
fviz_eig(pca_red, addlabels = TRUE)

# b) Partir la base en train-test. Considerando la calidad como variable respuestas, ajustar un modelo de PCR.

set.seed(123)  # Para reproducibilidad
#train_indices <- sample(1:nrow(white), nrow(white)*0.7)  # 70% de los datos para entrenamiento
indices_blanco <- createDataPartition(white$calidad, p = 0.7, list = FALSE)

train_data <- white[indices_blanco, ]
test_data <- white[-indices_blanco, ]

#train_indices2 <- sample(1:nrow(red), nrow(red)*0.7)  # 70% de los datos para entrenamiento
indices_rojo <- createDataPartition(red$calidad, p = 0.7, list = FALSE)

train_data2 <- red[train_indices2, ]
test_data2 <- red[-train_indices2, ]

# Ajustar el modelo de PCR
model_white <- pcr(calidad ~ ., data = train_data, scale = TRUE, validation = "CV")
model_white
model_red <- pcr(calidad ~ ., data = train_data2, scale = TRUE, validation = "CV")

# Mostrar un resumen del modelo
summary(model_white)
summary(model_red)

# Realizar predicciones en el conjunto de prueba
predictions <- predict(model_white, newdata = test_data,nvomp = 1)
predictions2 <- predict(model_red, newdata = test_data2,ncomp = 3)

# Calcular el error cuadrático medio (RMSE)
RMSE <- sqrt(mean((test_data$calidad - predictions)^2)) 
RMSE

RMSE2 <- sqrt(mean((test_data2$calidad - predictions2)^2))
RMSE2

# c) Cuál es el número óptimo de componentes principales a considerar? Grafique las puntuaciones originales y las ajustadas por PCR.

validationplot(model_white, val.type = "RMSEP")
which.min(x = model_white$validation$PRESS)

validationplot(model_red, val.type = "RMSEP")
which.min(x = model_red$validation$PRESS)

# d) Calcular el MSE para este subconjunto de componentes.

RMSE <- mean((test_data$calidad - predictions)^2) 
RMSE

RMSE2 <- mean((test_data2$calidad - predictions2)^2)
RMSE2

# e) Realizar el ajuste en este caso con PLS. Comparar los resultados de ambos
# modelos.

set.seed(666)
modelo_white_pls <- plsr(calidad ~ ., data = train_data, scale = TRUE, validation = "CV") 
pred_white_pls <- predict(object = modelo_white_pls, newdata = test_data, ncomp = 3) 
test_blanco_MSE_PLS <- mean((pred_white_pls - test_data$calidad)^2) 
validationplot(modelo_white_pls, val.type = "RMSEP")

set.seed(666)
modelo_red_pls <- plsr(calidad ~ ., data = train_data2, scale = TRUE, validation = "CV") 
pred_red_pls <- predict(object = modelo_red_pls, newdata = test_data2, ncomp = 3) 
test_red_MSE_PLS <- mean((pred_red_pls - test_data2$calidad)^2) 
validationplot(modelo_red_pls, val.type = "RMSEP")

# f) * (para hacer en la unidad de regresión logística) Clasifique a los vinos
# como regulares (calidad< 5) → 0, y buenos o muy buenos (calidad≥ 5) → 1. Ajuste un modelo de regresión logística para estimar la calidad de vino. Evalue la pertinencia del modelo

# 3.6

quimicos <- ChemicalManufacturingProcess
quimicos
str(quimicos)

# Me fijo si existen valores NA
na_rows <- apply(quimicos, 1, function(row) any(is.na(row)))
quimicos[na_rows, ]


# a) Realizar un análisis cuidadoso de las variables predictoras y una limpieza de la base.

# Reemplazamos nulos por mediana
quimicos <- na.aggregate(quimicos, FUN = median)
str(quimicos)

cor_matrix_quimicos <- cor(quimicos)
corrplot(cor_matrix_quimicos, method = "color")

highly_correlated <- findCorrelation(cor_matrix_quimicos, cutoff = 0.8)
quimicos <- quimicos[, -highly_correlated]

# b) Aplicar PCR y PLS para predecir Yield (rendimiento) y comparar los resultados de ambos métodos.

set.seed(666)
indices.q <- createDataPartition(quimicos$Yield, p = 0.8, list = FALSE)

train_q <- quimicos[indices.q, ] 
test_q <- quimicos[-indices.q, ]

#### Modelo PCR

```{r}
quimicos.pcr <- pcr(Yield ~ ., data = train_q, scale = TRUE, validation = "CV")

pred_q_pcr <- predict(object = quimicos.pcr, newdata = test_q)

test_q_MSE_PCR <- mean((pred_q_pcr - test_q$Yield)^2) 

validationplot(quimicos.pcr, val.type = "RMSEP", main = "RMSEP - Modelo PCR Químicos")

cat("")
```

#### Modelo PLS

```{r}
set.seed(666)

quimicos.pls <- plsr(Yield ~ ., data = train_q, scale = TRUE, validation = "CV") 

pred_q_pls <- predict(object = quimicos.pls, newdata = test_q) 

test_q_MSE_PLS <- mean((pred_q_pls - test_q$Yield)^2) 

validationplot(quimicos.pls, val.type = "RMSEP", main = "RMSEP - Modelo PLS Químicos")
```

#### Comparación resultados


metodo <- c("PCR", "PLS") 
test_MSE <- c(test_q_MSE_PCR, test_q_MSE_PLS) 
resultados <- data.frame(metodo, test_MSE) 
resultados

ggplot(data = resultados,
        aes(x = reorder(metodo, test_MSE), 
           y = sqrt(test_MSE))) + 
  geom_bar(stat = "identity") + 
  labs(x = "Método de regresión", y = expression(sqrt("test MSE"))) +
  theme_bw()







