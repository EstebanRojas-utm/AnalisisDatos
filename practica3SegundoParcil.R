library(ggplot2)
library(tidyverse)
library(gridExtra)
library(corrplot)
library(dplyr)
library(GGally)

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//kc_house_data.csv"
datahouse <- read.table(namefile, header = TRUE, sep = ",")

summary(datahouse)
View(datahouse)

#Modelo de regresion lineal multiple-variable objetivo es price
modelo <- lm(price ~  bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + sqft_above + sqft_basement + yr_built + yr_renovated + sqft_living15 + sqft_lot15, data = datahouse)
summary(modelo)

## Metricas
residuos <- residuals(modelo)
n <- nrow(datahouse)
#Suma de errores al cuadrado
RSS <- sum(residuos^2)
#Error cuadratico medio
RMSE <- sqrt(RSS/n)
RSE  <- sqrt(RSS / df.residual(modelo))
R2   <- summary(modelo)$r.squared

cat("--- MÉTRICAS DE VALIDACIÓN ---\n")
cat("RSS:  ", RSS, "\n")
cat("RMSE: ", RMSE, "\n")
cat("RSE:  ", RSE, "\n")
cat("R2:   ", R2, "\n")

#identificacion de posibles valores atipicos

datahouse$studentized_residual <- rstudent(modelo)
x11()
ggplot(data = datahouse, aes(x= predict(modelo),
                            y= abs(studentized_residual))) +
  geom_hline(yintercept = 5,  color = "gray", linetype = "dashed") + 
  geom_point(aes(color = ifelse(abs(studentized_residual) > 5, 'red', 'black' ))) + 
  scale_color_identity() + labs(title = "Distribucion de los residuos studentized (modelo)", 
                                x = "prediccion modelo") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))



## Graficas
x11()
#Grafica de residuos vs valores ajustados
ggplot(data = datahouse, aes(modelo$fitted.values, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick', se = FALSE) +
  geom_hline(yintercept = 0) + theme_bw()

x11()
plot(modelo, which=2)

#normalidad de los residuos
qqnorm(modelo$residuals)
qqline(modelo$residuals)


plot1 <- ggplot(data = datahouse, aes(bedrooms, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot2 <- ggplot(data = datahouse, aes(bathrooms, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot3 <- ggplot(data = datahouse, aes(sqft_living, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot4 <- ggplot(data = datahouse, aes(sqft_lot, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot5 <- ggplot(data = datahouse, aes(floors, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot6 <- ggplot(data = datahouse, aes(waterfront, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot7 <- ggplot(data = datahouse, aes(view, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot8 <- ggplot(data = datahouse, aes(condition, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot9 <- ggplot(data = datahouse, aes(grade, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot10 <- ggplot(data = datahouse, aes(sqft_above, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot11 <- ggplot(data = datahouse, aes(sqft_basement, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot12 <- ggplot(data = datahouse, aes(yr_built, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot13 <- ggplot(data = datahouse, aes(yr_renovated, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot14 <- ggplot(data = datahouse, aes(sqft_living15, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot15 <- ggplot(data = datahouse, aes(sqft_lot15, modelo$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()


x11()
grid.arrange(plot1, plot2, plot3, plot4 )

x11()
grid.arrange(plot5, plot6, plot7, plot8 )

x11()
grid.arrange(plot9, plot10, plot11, plot12 )

x11()
grid.arrange(plot13, plot14, plot15)


# MEjor Modelo de regresion lineal multiple-variable objetivo es price###
modeloBest <- lm(price ~   sqft_living + sqft_lot +  sqft_above  + sqft_living15, data = datahouse)
summary(modeloBest)

## Metricas
residuos <- residuals(modeloBest)
n <- nrow(datahouse)
#Suma de errores al cuadrado
RSS <- sum(residuos^2)
#Error cuadratico medio
RMSE <- sqrt(RSS/n)
RSE  <- sqrt(RSS / df.residual(modeloBest))
R2   <- summary(modeloBest)$r.squared

cat("--- MÉTRICAS DE VALIDACIÓN ---\n")
cat("RSS:  ", RSS, "\n")
cat("RMSE: ", RMSE, "\n")
cat("RSE:  ", RSE, "\n")
cat("R2:   ", R2, "\n")

###Graficas

datahouse$studentized_residual <- rstudent(modeloBest)
x11()
ggplot(data = datahouse, aes(x= predict(modeloBest),
                             y= abs(studentized_residual))) +
  geom_hline(yintercept = 5,  color = "gray", linetype = "dashed") + 
  geom_point(aes(color = ifelse(abs(studentized_residual) > 5, 'red', 'black' ))) + 
  scale_color_identity() + labs(title = "Distribucion de los residuos studentized (modeloBest)", 
                                x = "prediccion modelo") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
x11()
ggplot(data = datahouse, aes(x = predict(modeloBest), y = studentized_residual)) +
  geom_hline(yintercept = c(-3, 3), color = "red", linetype = "dashed") + 
  geom_point(alpha = 0.3) +
  coord_cartesian(ylim = c(-6, 6)) + # Limita los ejes sin eliminar datos
  labs(title = "Residuos Studentizados (Escala Limitada)",
       x = "Valores Ajustados", y = "Residuo Studentizado") +
  theme_bw()

x11()
#Grafica de residuos vs valores ajustados
ggplot(data = datahouse, aes(modeloBest$fitted.values, modeloBest$residuals)) + geom_point() + geom_smooth(color='firebrick', se = FALSE) +
  geom_hline(yintercept = 0) + theme_bw()



##### MODELO LIMPIO ############### 

# Distancia de Cook
datahouse$cooks_dist <- cooks.distance(modelo)

# Umbral critico 4/n
criterio_cook <- 4 / nrow(datahouse)

# 1. Crear dataset filtrado eliminando outliers 
data_sin_outliers <- datahouse %>% 
  filter(abs(studentized_residual) <= 3 & cooks_dist <= criterio_cook)

cat("Registros conservados:", nrow(data_sin_outliers), 
    "de", nrow(datahouse), "\n")

# 2. Re-entrenar el modelo con los datos limpios
modelo_limpio <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
                      floors + waterfront + view + condition + grade + 
                      sqft_above + yr_built + yr_renovated + 
                      sqft_living15 + sqft_lot15, 
                    data = data_sin_outliers)

summary(modelo_limpio)

data_sin_outliers$studentized_residual <- rstudent(modelo_limpio)
x11()
ggplot(data = data_sin_outliers, aes(x= predict(modelo_limpio),
                             y= abs(studentized_residual))) +
  geom_hline(yintercept = 5,  color = "gray", linetype = "dashed") + 
  geom_point(aes(color = ifelse(abs(studentized_residual) > 5, 'red', 'black' ))) + 
  scale_color_identity() + labs(title = "Distribucion de los residuos studentized (modelo_limpio)", 
                                x = "prediccion modelo") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

x11()
#Grafica de residuos vs valores ajustados
ggplot(data = data_sin_outliers, aes(modelo_limpio$fitted.values, modelo_limpio$residuals)) + geom_point() + geom_smooth(color='firebrick', se = FALSE) +
  geom_hline(yintercept = 0) + theme_bw()


