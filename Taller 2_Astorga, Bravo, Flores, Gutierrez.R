#####################################################################
## Taller Evaluado N°2: Herramientas Estadísticas y Forecast (HEF) ## 
#####################################################################

## Integrantes: Charo Astorga, Nathaly Bravo, Camila Flores, Nicolás Gutiérrez.


# Primero, importaremos todas las librerías que utilizaremos en este taller:

pacman::p_load(dplyr, ggplot2, ggthemes, showtext, MASS, forecast, readxl, tidyverse, sjmisc, devtools,SPreg,tseries,
               performanceEstimation,caret,ROCit,brglm2,purrr,tibble,yardstick, lmtest, extrafont,sysfonts)
               
options(scipen=999) # desactivar la notación científica


# Luego, cargaremos las bases de datos:

bikes <- read_excel("bikes.xlsx")

lluvia <- read_excel("lluvia.xlsx")


######################
## Regresión Lineal ##
######################

### Pregunta 1 ###

# 1.1. Transforme a factor las variables que son categóricas y asigne etiqueta.

bikes <- bikes %>%
  mutate(season = factor(season, levels = c(1, 2, 3, 4), 
                         labels = c("Invierno", "Primavera", "Verano", "Otoño")),
         holiday = factor(holiday, levels = c(0, 1), 
                          labels = c("No Festivo", "Festivo")),
         weekday = factor(weekday, levels = c(1, 2, 3, 4, 5, 6, 7), 
                          labels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")),
         workingday = factor(workingday, levels = c(0, 1), 
                             labels = c("No Laboral", "Laboral")),
         weather = factor(weather, levels = c(1, 2, 3, 4), 
                          labels = c("Despejado", "Nublado", "Lluvioso", "Tormentoso")),
         month = factor(month, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                        labels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", 
                                   "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")))

str(bikes)


# 1.2. Transforme las variables temp y atemp a grados celsius, ya que se encuentran en escala normalizada.

# Despejamos la formula para dejar grados celcius.
bikes[["temp"]] <- bikes[["temp"]] * (39 - (-8)) + (-8)
bikes[["atemp"]] <- bikes[["atemp"]] * (50 - (-16)) + (-16)

# 1.3. Transforme la variable humedad a porcentaje.

bikes$humidity <- bikes$humidity * 100

# 1.4. Transforme la velocidad del viento a millas por hora.

bikes$windspeed <- bikes$windspeed * 67

### Pregunta 2 ###

# 2.1. Realice un gráfico de dispersión entre el número de arriendos de bicicletas (target) vs temperatura (temp). Agregue la recta de regresión lineal.

font_add_google(name = "EB Garamond", family = "EB Garamond")
showtext_auto()

ggplot(bikes, aes(x = temp, y = target)) +
  geom_point(color = "#AB82FF", alpha = 0.7) +  
  geom_smooth(method = "lm", se = FALSE, color = "#27408B") +  
  labs(x = "Temperatura (°C)",
       y = "Número de Arriendos",
       title = "Gráfico de Dispersión: Número de Arriendos vs Temperatura") +
  theme_light() + 
  theme(text = element_text(family = "EB Garamond")) 



### Pregunta 3 ###
#¿Es la relación entre la temperatura y el número de bicicletas arrendadas igual en los dos años?, para responder, compare en un
#mismo gráfico la relación entre el número de arriendo vs temperatura para los dos años, añada ambas rectas de regresión lineal.

# Crear un gráfico de dispersión con ggplot2 y añadir ambas rectas de regresión
ggplot(bikes, aes(x = temp, y = target, color = as.factor(year))) +
  geom_point(alpha = 0.7) +  
  geom_smooth(method = "lm", se = FALSE) + 
  scale_color_manual(values = c("2011" = "#68c6c6", "2012" = "#f57e7e")) +
  labs(x = "Temperatura (°C)",
       y = "Número de Arriendos",
       color = "Año",
       title = "Comparación de Número de Arriendos vs Temperatura por Año") +
  theme_minimal() +
  theme(text = element_text(family = "EB Garamond")) 



### Pregunta 4 ###

# 4.1. Por selección forward, construya un modelo sin considerar las variables: date, month, registered.

# Modelo nulo (sin predictores)
modelo_nulo <- lm(target ~ 1, data = bikes)

# Modelo completo (sin las variables date, month y registered)
modelo_completo <- lm(target ~ season + year + holiday + weekday + workingday + 
                        weather + temp + atemp + humidity + windspeed, 
                      data = bikes)

# Selección hacia adelante
modelo_forward <- stepAIC(modelo_nulo, 
                          scope = list(lower = modelo_nulo, upper = modelo_completo),
                          direction = "forward")

# Resumen del modelo final
summary(modelo_forward)


### Pregunta 5 ###

# 5.1 Utilice los residuos del modelo elegido para estudiar la validez de los supuestos: Normalidad y Homocedasticidad.


# Normalidad
#H0: Los datos siguen una distribución Normal.
#H1: Los datos no siguen una distribución Normal.


# Residuos del modelo
residuos <- rstandard(modelo_forward) 
fitted <- fitted(modelo_forward)

# Gráfico Q-Q para normalidad
par(family = "EB Garamond")
qqnorm(residuos)
qqline(residuos, col = "#EEB422", lwd = 1.5)
par(family = "")

# Prueba de Shapiro-Wilk
shapiro.test(residuos)


# Homocedasticidad
#H0 Homocedasticidad
#H1 Heterocedasticidad

# Prueba de Breusch-Pagan
bptest(modelo_forward)



# Gráfico de Residuos vs Valores Ajustados
par(family = "EB Garamond")
plot(modelo_forward$fitted.values, residuos, 
     main = "Residuos vs Valores Ajustados",
     xlab = "Valores Ajustados",
     ylab = "Residuos Estandarizados")
abline(h = 0, col = "red", lwd = 2)
par(family = "")



### Pregunta 6 ###

#Realice una predicción de arriendos de bicicletas para un día con las siguientes cualidades.

# Crear un DataFrame
df1 <- tibble(
  season = factor("Primavera", levels = levels(bikes$season)),
  year = 2011,
  holiday = factor("Festivo", levels = levels(bikes$holiday)), 
  weekday = factor("Sábado", levels = levels(bikes$weekday)),
  workingday = factor("No Laboral", levels = levels(bikes$workingday)), 
  weather = factor("Nublado", levels = levels(bikes$weather)),
  temp = 12,
  atemp = 11,
  humidity = 66.3,
  windspeed = 12.5
)

# Realizar la predicción usando el modelo
prediccion <- predict(modelo_forward, newdata = df1)

# Mostrar el valor
print(prediccion)



######################
## Series de Tiempo ##
######################

### Pregunta 1 ###

# A partir de el ACF verifique que el supuesto de independencia no se cumple y junto al PACF proponga los ordenes p y q 
#de un potencial modelo ARMA para los residuos del modelo de regresión ajustado en la pregunta 5.

# ACF y PACF de los residuos
acf(residuos, main = "ACF de los Residuos", lag.max = 20)
pacf(residuos, main = "PACF de los Residuos", lag.max = 20)


### Pregunta 2 ###

# Utilizando auto.arima() de forecast de R o su equivalente en Python obtenga un modelo ARMA a partir de los ordenes propuestos en el item anterior.


# Ajustar un modelo ARMA a los residuos utilizando auto.arima
modelo_arma <- auto.arima(residuos, max.p = 1, max.q = 3, d = 0, 
                          seasonal = FALSE, stepwise = FALSE, approximation = FALSE)

# Resumen del modelo ajustado
summary(modelo_arma)

# Diagnóstico del modelo
checkresiduals(modelo_arma)


### Pregunta 3 ###

#Realice el test de Box-Ljung y chequee si la hipótesis de blancura se cumple.


# Realizar el test de Box-Ljung sobre los residuos del modelo ARMA ajustado
box_ljung_test <- Box.test(modelo_arma$residuals, lag = 10, type = "Ljung-Box")

# Mostrar los resultados del test de Box-Ljung
print(box_ljung_test)



#########################
## Regresión Logística ##
#########################

### Pregunta 1 ###


# 1.1. Codifique la variable LluviaMan como 0 y 1 para los días sin lluvia y con lluvia.

lluvia$LluviaMan <- ifelse(lluvia$LluviaMan == "Yes", 1, 0)

# 1.2. Realice una separación de la base de datos en un set de entrenamiento y set de validación. Utilice una proporción de 80:20.

lluvia$id<-1:nrow(lluvia) # agregar un id para cada individuo en el dataset

#Train-test split (80 train - 20 test)

set.seed(2024)

train <- lluvia %>%
  dplyr::sample_frac(0.80) # grupo de entrenamiento

table(train$LluviaMan) / nrow(train)

test <- dplyr::anti_join(lluvia, train, by = "id") # grupo de prueba

newdata <- data.frame(x1 = test$LluviaMan)
newdata <- test[,-c(18,21)] # sacar LluviaMan y id


table(test$LluviaMan) / nrow(test)  



### Pregunta 2 ###

# Realice un modelo de regresión logística para predecir si lloverá mañana utilizando la variable Evaporación.


modelo<-glm(LluviaMan~Evaporacion, family = binomial(link = logit), data = train)

summary(modelo)

exp(coef(modelo))

beta<-coef(modelo)

exp(confint(modelo))


### Pregunta 3 ###

# Utilizando un método automatizado, ajuste un modelo de regresión logística, utilizando la metodología de dirección both (forward y backward).


modelo_completo <-glm(LluviaMan ~ MinTemp + MaxTemp + Lluvia + Sol + Evaporacion +
              VelRafaga + Vel9am + Vel3pm + Hum9am + Hum3pm + Pres9am + Pre3pm + Nub9am +Nub3pm + Temp9am+
              Temp3pm + LluviaHoy + Koppen+ Estacion, family = binomial(link = logit), 
            data = train)  # modelo con todas las varibles


modelo_nulo <- glm(LluviaMan ~ 1, family = binomial(link = "logit"), data = train)


modelo_final <- step(modelo_nulo, 
                     scope = list(lower = modelo_nulo, upper = modelo_completo), 
                     direction = "both") # prueba de todos los modelos posibles
summary(modelo_final)
modelo<-modelo_final

exp(coef(modelo))

beta<-coef(modelo)

exp(confint(modelo))

modelo<-glm(formula = LluviaMan ~ Hum3pm + VelRafaga + Sol + Pre3pm + 
              Pres9am + LluviaHoy + Vel3pm + Nub3pm + Koppen + Vel9am + 
              Estacion + Lluvia + MaxTemp + MinTemp + Hum9am + Temp9am, 
            family = binomial(link = "logit"), data = train) # modelo después de aplicar step, este es el "mejor" modelo



### Pregunta 4 ###


# 4.1. Considerando la base de entrenamiento, ajuste la curva ROC y KS asociada al modelo. 

# Predicciones del modelo
pred_modelo <- predict(modelo, newdata = train, type = "response")


# Curva ROC

ROC.modelo <- rocit(score = pred_modelo, class = train$LluviaMan, negref = NULL, method = "empirical", step = FALSE)
summary(ROC.modelo) 
plot.ROC <- plot(ROC.modelo, YIndex = TRUE, values = TRUE)

jpeg("plot_ROC.jpeg", width = 800, height = 600)
plot(ROC.modelo, YIndex = TRUE, values = TRUE)
dev.off()


# Estadístico KS
ks <- max(ROC.modelo$TPR - ROC.modelo$FPR)
ks


# 4.2. Con la información obtenida encuentre un punto de corte que tenga una sensibilidad mínima del 80% y la máxima especificidad.

# Generar la curva ROC
ROC.modelo <- rocit(score = pred_modelo, class = train$LluviaMan, negref = NULL, method = "empirical", step = FALSE)

# Extraer los datos de la curva ROC
roc_data <- data.frame(
  Threshold = ROC.modelo$Cutoff, 
  Sensitivity = ROC.modelo$TPR, 
  Specificity = 1 - ROC.modelo$FPR  # 1 - FPR gives Specificity
)

# Filtrar por una sensibilidad mínima del 80%
roc_filtered <- roc_data[roc_data$Sensitivity >= 0.80, ]

# Encontrar el punto de corte con la máxima especificidad
optimal_threshold <- roc_filtered[which.max(roc_filtered$Specificity), ]

# Mostrar el punto de corte óptimo
optimal_threshold



### Pregunta 5 ###

#Considerando la base de test, obtenga nuevamente la curva ROC y KS asociada al modelo, además, utilizando el punto de corte obtenido, obtenga la precisión

# Predicciones del modelo
pred_modelo_test <- predict(modelo, newdata = test, type = "response")

# Matriz de confusión
prediccion_test <- ifelse(pred_modelo_test > 0.5, 1, 0)
prediccion1_test <- factor(prediccion_test, levels = c(0, 1))
matriz_test <- confusionMatrix(as.factor(test$LluviaMan), prediccion1_test, positive = "1")
print(matriz_test)

# Curva ROC

ROC.modelo_test <- rocit(score = pred_modelo_test, class = test$LluviaMan, negref = NULL, method = "empirical", step = FALSE)
summary(ROC.modelo_test) 
plot.ROC_test <- plot(ROC.modelo_test, YIndex = TRUE, values = TRUE)

jpeg("plot_ROC_test.jpeg", width = 800, height = 600)
plot(ROC.modelo_test, YIndex = TRUE, values = TRUE)
dev.off()

# Estadístico KS
ks <- max(ROC.modelo$TPR - ROC.modelo$FPR)
ks

# Extraer los datos de la curva ROC
roc_data_test <- data.frame(
  Threshold = ROC.modelo_test$Cutoff, 
  Sensitivity = ROC.modelo_test$TPR, 
  Specificity = 1 - ROC.modelo_test$FPR  # 1 - FPR gives Specificity
)

# Encontrar el punto de corte con la máxima especificidad
optimal_threshold <- roc_filtered[which.max(roc_filtered$Specificity), ]
optimal_threshold

# Asignar el punto de corte óptimo
optimal_threshold <- 0.2116886

# Generar las predicciones usando el punto de corte óptimo
pred_optimal <- ifelse(pred_modelo > optimal_threshold, 1, 0)

# Crear una tabla de contingencia (matriz de confusión)
conf_matrix <- table(Predicted = pred_optimal, Actual = train$LluviaMan)

# Calcular la precisión (Valor Predictivo Positivo)
precision <- conf_matrix[2, 2] / (conf_matrix[2, 1] + conf_matrix[2, 2])
precision



### Pregunta 6 ###

# Utilizando el punto de corte encontrado, determine si el día de mañana lloverá.

# Crear el data frame con los datos del día de mañana
new_data <- data.frame(
  MinTemp = 7,
  MaxTemp = 18,
  Lluvia = 0,
  Evaporacion = 7,
  Sol = 12,
  VelRafaga = 72,
  Vel9am = 10,
  Vel3pm = 54,
  Hum9am = 65,
  Hum3pm = 77,
  Pres9am = 1001,
  Pre3pm = 1025,
  Nub9am = 3,
  Nub3pm = 2,
  Temp9am = 11.4,
  Temp3pm = 16.2,
  LluviaHoy = factor("No", levels = c("No", "Yes")),
  Koppen = factor("Subtropical", levels = c("Subtropical", "Temperate")),
  Estacion = factor("Primavera", levels = c("Verano", "Otoño", "Invierno", "Primavera"))
)

# Predecir la probabilidad de lluvia para el día de mañana usando el modelo
pred_mañana <- predict(modelo, newdata = new_data, type = "response")

# Asignar el punto de corte óptimo
optimal_threshold <- 0.2116886

# Determinar si lloverá o no utilizando el punto de corte
llovera_mañana <- ifelse(pred_mañana > optimal_threshold, "Sí, lloverá", "No, no lloverá")

# Mostrar el resultado
llovera_mañana




