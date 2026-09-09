library(ggplot2)
library(tidyverse)

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//autos.csv"
dataAutos <- read.table(namefile, header = TRUE, sep = ",")

summary(dataAutos)
View(dataAutos)

Media_wheel <- mean(dataAutos$wheel.base)
Media_length <- mean(dataAutos$length)

corre_Wheel_Length <- cor(dataAutos$wheel.base, dataAutos$length)
corre_Wheel_width <- cor(dataAutos$wheel.base, dataAutos$width)
corre_Wheel_height <- cor(dataAutos$wheel.base, dataAutos$height)

corre_length_width <- cor(dataAutos$length, dataAutos$width)
corre_length_height <- cor(dataAutos$length, dataAutos$height)

corre_width_height <- cor(dataAutos$width, dataAutos$height)

# Seleccionamos las columnas continuas de interés
vars_continuas <- dataAutos %>% 
  select(wheel.base, length, width, height)

# Matriz de correlación directa (redondeada a 4 decimales)
matriz_cor <- round(cor(vars_continuas, use = "complete.obs"), 4)
print(matriz_cor)


#Variable categorica
#Funcion de cramer
cramer_phi <- function(x, y) {
  # Eliminamos valores NAs 
  df_clean <- na.omit(data.frame(x = x, y = y))
  
  # p_kl: Frecuencias observadas (tabla de contingencia)
  p_kl <- table(df_clean$x, df_clean$y)
  
  # N: Cantidad total de datos
  N <- sum(p_kl)
  
  # r y k: Número de filas y columnas
  r <- nrow(p_kl)
  k <- ncol(p_kl)
  
  # Si alguna variable tiene solo 1 categoría, la asociación es 0
  if (r <= 1 | k <= 1) return(0)
  
  # f_xk y f_yl: Frecuencias marginales por fila y columna
  f_xk <- rowSums(p_kl)
  f_yl <- colSums(p_kl)
  
  # m_kl: Frecuencias esperadas m_kl = (f_xk * f_yl) / N
  m_kl <- outer(f_xk, f_yl) / N
  
  # Chi-cuadrado (Chi^2)
  chi2 <- sum(((p_kl - m_kl)^2) / m_kl)
  cat("chicuadrada: ", chi2 , "\n")
  
  # Grados de libertad mínimos: min(r - 1, k - 1)
  min_dim <- min(r - 1, k - 1)
  
  # Coeficiente Cramer's Phi
  phi <- sqrt(chi2 / (N * min_dim))
  
  return(phi)
}


cat("\n Cálculo de Cramer's Phi: make vs fuel.type \n")
phi_fuel_asp <- cramer_phi(dataAutos$make , dataAutos$fuel.type)
cat("Cramer's Phi:", round(phi_fuel_asp, 4), "\n")

cat("\n Cálculo de Cramer's Phi: make vs aspiration \n")
phi_fuel_asp <- cramer_phi(dataAutos$make , dataAutos$aspiration)
cat("Cramer's Phi:", round(phi_fuel_asp, 4), "\n")

cat("\n Cálculo de Cramer's Phi: make vs num.of.door \n")
phi_fuel_asp <- cramer_phi(dataAutos$make , dataAutos$num.of.doors)
cat("Cramer's Phi:", round(phi_fuel_asp, 4), "\n")

cat("\n Cálculo de Cramer's Phi: make vs drive.wheels \n")
phi_fuel_asp <- cramer_phi(dataAutos$make , dataAutos$drive.wheels)
cat("Cramer's Phi:", round(phi_fuel_asp, 4), "\n")

cat("\n Cálculo de Cramer's Phi: make vs engine location \n")
phi_fuel_asp <- cramer_phi(dataAutos$make , dataAutos$engine.location)
cat("Cramer's Phi:", round(phi_fuel_asp, 4), "\n")


####  Matriz de cramer  #####
# Seleccionamos las columnas categóricas (factor o character)
vars_categoricas <- dataAutos %>% 
  select(where(~ is.character(.) | is.factor(.)))

num_vars <- ncol(vars_categoricas)
nombres_vars <- colnames(vars_categoricas)

# Creamos una matriz vacía para almacenar los resultados
matriz_cramer <- matrix(1, nrow = num_vars, ncol = num_vars,
                        dimnames = list(nombres_vars, nombres_vars))

# Llenamos la matriz calculando el coeficiente par a par
for (i in 1:num_vars) {
  for (j in 1:num_vars) {
    if (i != j) {
      matriz_cramer[i, j] <- cramer_phi(vars_categoricas[[i]], vars_categoricas[[j]])
    }
  }
}

cat("\n--- MATRIZ DE ASOCIACIÓN (CRAMER'S PHI) ---\n")
print(round(matriz_cramer, 4))
