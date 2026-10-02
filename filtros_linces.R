library(ggplot2)
library(tidyverse)

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//lenses.data"
namescol <- c('instancia','Age', 'Prescription', 'Astigmatic', 'TearProductionRate', 'Class')

datalences <- read.table(namefile, header = FALSE, sep = ',', na.strings = c("?", "NA"), col.names = namescol)


View(datalences)

datalences$Class <- factor(datalences$Class)
summary(datalences)

# Entropía: H(X)
calcular_H <- function(x) {
  p <- table(x) / length(x)
  p <- p[p > 0] # Evitar log2(0)
  return(-sum(p * log2(p)))
}

# Entropía Condicional: H(X|Y) 
calcular_H_cond <- function(x, y) {
  n <- length(x)
  p_xy <- table(x, y) / n
  p_y <- table(y) / n
  
  h_cond <- 0
  for (i in seq_len(nrow(p_xy))) {
    for (j in seq_len(ncol(p_xy))) {
      pxy <- p_xy[i, j]
      py <- p_y[j]
      if (pxy > 0) {
        h_cond <- h_cond - (pxy * log2(pxy / py))
      }
    }
    
  }
  return(as.numeric(h_cond))
}

# Información Mutua: I(Ak, C) 
calcular_I <- function(ak, c) {
  h_ak <- calcular_H(ak)
  h_ak_c <- calcular_H_cond(ak, c)
  return(h_ak - h_ak_c)
}

# Información Mutua Normalizada: MMI(Ak)
calcular_MMI <- function(ak, c) {
  h_ak <- calcular_H(ak)
  h_c <- calcular_H(c)
  i_ak_c <- calcular_I(ak, c)
  
  mmi <- (2 * i_ak_c) / (h_ak + h_c)
  return(mmi)
}

#--------------Evaluación------------------------------------------# 

caracteristicas <- c('Age', 'Prescription', 'Astigmatic', 'TearProductionRate')
clase <- datalences$Class

resultados <- data.frame(
  Caracteristica = character(),
  H_X = numeric(),
  H_X_Y = numeric(),
  I_Ak_C = numeric(),
  MMI = numeric(),
  stringsAsFactors = FALSE
)



h_clase <- calcular_H(clase)

for (col in caracteristicas) {
  ak <- datalences[[col]]
  
  h_ak <- calcular_H(ak)
  h_ak_c <- calcular_H_cond(ak, clase)
  i_ak_c <- h_ak - h_ak_c
  mmi <- (2 * i_ak_c) / (h_ak + h_clase)
  
  resultados <- rbind(resultados, data.frame(
    Caracteristica = col,
    H_Ak = round(h_ak, 4),
    H_Ak_C = round(h_ak_c, 4),
    I_Ak_C = round(i_ak_c, 4),
    MMI = round(mmi, 4)
  ))
}

# Ordenar por relevancia 
resultados <- resultados %>% arrange(desc(MMI))
cat("Entropia de la clase: H(class) = ", h_clase,"\n")
print(resultados)

#-------------Seleccion de la segunda caracteristica ---------------------------#
S1_name <- "TearProductionRate"
candidatas <- setdiff(caracteristicas, S1_name)


# 2. Evaluar
tabla_S2 <- data.frame(
  Caracteristica = character(),
  `MMI(Aj, C)` = numeric(),
  `MMI(Aj, S1)` = numeric(),
  `Score_mRMR` = numeric(),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
print(tabla_S2)
for (col in candidatas) {
  # MMI con la clase C 
  mmi_c <- calcular_MMI(datalences[[col]], datalences$Class)
  
  # MMI con S1 
  mmi_s1 <- calcular_MMI(datalences[[col]], datalences[[S1_name]])
  
  # Criterio mRMR: 
  score <- mmi_c - mmi_s1
  
  tabla_S2 <- rbind(tabla_S2, data.frame(
    Caracteristica = col,
    `MMI(Aj, C)` = round(mmi_c, 4),
    `MMI(Aj, S1)` = round(mmi_s1, 4),
    `Score_mRMR` = round(score, 4),
    check.names = FALSE
  ))
}

# 3. Ordenar resultados descendente
tabla_S2 <- tabla_S2 %>% arrange(desc(Score_mRMR))

# 4. Seleccionar  S2
S2_name <- tabla_S2$Caracteristica[1]

# Impresión
cat("SELECCIÓN DE LA SEGUNDA CARACTERÍSTICA (S2)\n")
cat("Primera característica elegida (S1):", S1_name, "\n")


print(tabla_S2, row.names = FALSE)

cat(" La segunda característica seleccionada (S2) es:", S2_name, "\n")





