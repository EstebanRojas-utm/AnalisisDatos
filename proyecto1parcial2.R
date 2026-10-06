library(ggplot2)
library(tidyverse)

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//mortality.rates.txt"
datamor <- read.table(namefile, header = TRUE, sep = "\t")

summary(datamor)
View(datamor)

# Extraer y ordenar el vector numérico de menor a mayor
x <- sort(datamor$Rate)
n <- length(x)
print(x)
print(n)
# 2. Función para calcular la secuencia 
calcular_midsummaries <- function(vec) {
  vec <- sort(vec)
  n <- length(vec)
  
  etiquetas <- c("M", "F", "E", "D", "C", "B", "A")
  nombres <- c("Median", "Fourth", "Eighth", "Sixteenth", 
               "Thirty-secondth", "Sixty-fourth", "One hundred and twenty-eighth")
  
  resultados <- list()
  
  # Profundidad inicial (Mediana)
  d <- (n + 1) / 2
  
  for (i in seq_along(etiquetas)) {
    # Interpolación para obtener valor inferior (Lower) y superior (Upper)
    d_floor <- floor(d)
    d_frac <- d - d_floor
    
    val_L <- vec[d_floor] + d_frac * (vec[d_floor + 1] - vec[d_floor])
    val_U <- vec[n - d_floor + 1] - d_frac * (vec[n - d_floor + 1] - vec[n - d_floor])
    
    mid <- (val_U + val_L) / 2
    amplitud <- val_U - val_L
    
    resultados[[i]] <- tibble(
      Letras = etiquetas[i],
      Nombre = nombres[i],
      Profundidad = d,
      Lower = val_L,
      Upper = val_U,
      Mids = mid,
      Amplitud = amplitud
    )
    
    # Detener si llegamos al extremo de los datos (profundidad 1)
    if (d <= 1) break
    
    # Nueva profundidad según la fórmula de la tabla: ([depth] + 1) / 2
    d <- (floor(d) + 1) / 2
  }
  
  bind_rows(resultados)
}

# 3. Generar la tabla de resultados
midsummaries_table <- calcular_midsummaries(datamor$Rate)

# Ver el resultado formateado
print(midsummaries_table)

med <- mean(datamor$Rate)
print(med)
mediana <- 24
d <- (med+mediana)/58
