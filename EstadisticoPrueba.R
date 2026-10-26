library(ggplot2)
library(tidyverse)
library(gridExtra)
library(corrplot)
library(dplyr)
library(GGally)

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//state_x77.csv"
datavida <- read.table(namefile, header = TRUE, sep = ",")
summary(datavida)

#Modelo de regresion lineal multiple-variable objetivo es esperanza de vida
modelo <- lm(esp_vida ~ habitantes + ingresos + analfabetismo + asesinatos + universitarios + heladas + area + densidad_pobl, data = datavida)
summary(modelo)

#Evaluacion de metricas
residuos <- residuals(modelo)
y <- datavida$esp_vida
p <- length(coef(modelo)) - 1
n <- nrow(datavida)
#Suma de errores al cuadrado
RSS <- sum(residuos^2)
  
TSS <- sum((y - mean(y))^2)
numerador_F <- (TSS - RSS) / p
denominador_F <- RSS / (n - p - 1)
F_stat <- numerador_F / denominador_F

cat("--- MÉTRICAS DE VALIDACIÓN ---\n")
cat("RSS:  ", RSS, "\n")
cat("RMSE: ", TSS, "\n")
cat("RSE:  ", F_stat, "\n","N:", n)

vp_colader = pf(F_stat, df1 = numerador_F, df2 = denominador_F, lower.tail = FALSE)
vp_colaizq = pf(F_stat, df1 = numerador_F, df2 = denominador_F, lower.tail = TRUE)
vp = 2*min(vp_colader, vp_colaizq)
