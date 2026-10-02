library(tidyverse)

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//autosFeatureS.csv"

dataAutos <- read.table(namefile, header = TRUE, sep = ",", check.names = FALSE)
dataAutos$clase <- as.factor(dataAutos$clase)

# 2. DEFINICIÓN DE LAS FÓRMULAS 

# H(X) = - sum_x p(x) * log2(p(x))
calcular_H <- function(x) {
  p_x <- table(x) / length(x)
  p_x <- p_x[p_x > 0]
  return(-sum(p_x * log2(p_x)))
}

# H(X|Y) = - sum_x sum_y p(x, y) * log2( p(x, y) / p(y) )
# Donde X es la característica A_k y Y es la clase C
calcular_H_condicional <- function(x, y) {
  n <- length(x)
  tabla_conjunta <- table(x, y)
  p_xy <- tabla_conjunta / n          # p(x, y)
  p_y  <- table(y) / n                # p(y)
  
  h_cond <- 0
  for (i in 1:nrow(p_xy)) {
    for (j in 1:ncol(p_xy)) {
      if (p_xy[i, j] > 0) {
        pxy_val <- p_xy[i, j]
        py_val  <- p_y[j]
        h_cond  <- h_cond + (pxy_val * log2(pxy_val / py_val))
      }
    }
  }
  return(-h_cond)
}

# 3. CÁLCULO DE ENTROPÍA DE LA CLASE C
h_C <- calcular_H(dataAutos$clase)
cat("Entropía de la Clase H(C):", round(h_C, 6), "bits\n\n")

# 4. APLICACIÓN DE FÓRMULAS PARA CADA CARACTERÍSTICA CATEGÓRICA (A_k)
vars_cat <- c("make", "fuel-type", "aspiration")
tabla_resultados <- data.frame()

for (ak in vars_cat) {
  x <- dataAutos[[ak]]
  
  # a. Entropía de la característica H(A_k)
  h_Ak <- calcular_H(x)
  
  # b. Entropía condicional H(A_k | C)
  h_Ak_C <- calcular_H_condicional(x, dataAutos$clase)
  
  # c. Información Mutua I(A_k, C) = H(A_k) - H(A_k | C)
  i_Ak_C <- h_Ak - h_Ak_C
  
  # d. Información Mutua Modificada MMI(A_k) = 2 * I(A_k, C) / (H(A_k) + H(C))
  mmi_Ak <- (2 * i_Ak_C) / (h_Ak + h_C)
  
  tabla_resultados <- rbind(tabla_resultados, data.frame(
    Caracteristica = ak,
    `H(A_k)`       = round(h_Ak, 6),
    `H(C)`         = round(h_C, 6),
    `H(A_k|C)`     = round(h_Ak_C, 6),
    `I(A_k, C)`    = round(i_Ak_C, 6),
    `MMI(A_k)`     = round(mmi_Ak, 6),
    check.names    = FALSE
  ))
}

cat("=== RESULTADOS DE SELECCIÓN INDIVIDUAL DE CARACTERÍSTICAS ===\n")
print(tabla_resultados)



# ==============================================================================
# TABLAS DE PROBABILIDAD (MARGINALES, CONJUNTAS Y CONDICIONALES)
# ==============================================================================

# Probabilidad Marginal de la Clase: p(y)
p_y <- prop.table(table(dataAutos$clase))
cat("=== PROBABILIDAD MARGINAL DE LA CLASE p(y) ===\n")
print(round(p_y, 4))
cat("\n")

# Tablas para cada característica categórica A_k
for (ak in vars_cat) {
  cat("======================================================================\n")
  cat(" TABLAS DE PROBABILIDAD PARA LA CARACTERÍSTICA:", toupper(ak), "\n")
  cat("======================================================================\n")
  
  # 1. Probabilidad Marginal: p(x)
  p_x <- prop.table(table(dataAutos[[ak]]))
  cat("\n1. Probabilidad Marginal p(x):\n")
  print(round(p_x, 4))
  
  # 2. Probabilidad Conjunta: p(x, y)
  p_xy <- prop.table(table(dataAutos[[ak]], dataAutos$clase))
  cat("\n2. Probabilidad Conjunta p(x, y):\n")
  print(round(p_xy, 4))
  
  # 3. Probabilidad Condicional: p(x | y) = p(x, y) / p(y)
  p_x_dado_y <- prop.table(table(dataAutos[[ak]], dataAutos$clase), margin = 2)
  cat("\n3. Probabilidad Condicional p(x | y):\n")
  print(round(p_x_dado_y, 4))
  cat("\n\n")
}