library(ggplot2)
library(tidyverse)
library(gridExtra)
library(corrplot)
library(dplyr)

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//state_x77.csv"
datavida <- read.table(namefile, header = TRUE, sep = ",")
summary(datavida)
View(datavida)

#Graficacion de datos, cada preditor y variable objetivo
library(GGally)
X11()
ggpairs(datavida, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

#####################################################################
#Modelo de regresion lineal multiple-variable objetivo es esperanza de vida
modelo <- lm(esp_vida ~ habitantes + ingresos + analfabetismo + asesinatos + universitarios + heladas + area + densidad_pobl, data = datavida)
summary(modelo)

####Modelo mejorado
modeloBest <- lm(esp_vida ~ habitantes + asesinatos + universitarios + heladas, data = datavida)
summary(modeloBest)

x11()
#Grafica de residuos vs valores ajustados
ggplot(data = datavida, aes(modeloBest$fitted.values, modeloBest$residuals)) + geom_point() + geom_smooth(color='firebrick', se = FALSE) +
  geom_hline(yintercept = 0) + theme_bw()


x11()
plot(modeloBest, which=2)


#normalidad de los residuos
qqnorm(modeloBest$residuals)
qqline(modeloBest$residuals)


plot1 <- ggplot(data = datavida, aes(habitantes, modeloBest$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot2 <- ggplot(data = datavida, aes(asesinatos, modeloBest$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot3 <- ggplot(data = datavida, aes(universitarios, modeloBest$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

plot4 <- ggplot(data = datavida, aes(heladas, modeloBest$residuals)) + geom_point() + geom_smooth(color='firebrick') + geom_hline(yintercept = 0) + theme_bw()

x11()
grid.arrange(plot1, plot2, plot3, plot4)

x11()
corrplot(cor(dplyr::select(datavida, habitantes, asesinatos, universitarios, heladas)),
         method= "number", tl.col = "black")

#identificacion de posibles valores atipicos

datavida$studentized_residual <- rstudent(modeloBest)
x11()
ggplot(data = datavida, aes(x= predict(modeloBest),
                            y= abs(studentized_residual))) +
  geom_hline(yintercept = 2,  color = "gray", linetype = "dashed") + 
  geom_point(aes(color = ifelse(abs(studentized_residual) > 2, 'red', 'black' ))) + 
  scale_color_identity() + labs(title = "Distribucion de los residuos studentized", 
                                x = "prediccion modelo") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))





