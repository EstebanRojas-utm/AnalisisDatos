library(nycflights13)
library(tidyverse)

data(flights)
summary(flights)

flightsRed <- select(flights, dep_time, sched_dep_time, arr_time, sched_arr_time)
summary(flightsRed)

#eliminar na's
flightsRed <- flightsRed %>%
  filter(!is.na(dep_time), !is.na(sched_dep_time), !is.na(arr_time))
summary(flightsRed)

#Funcion para los Mids
calc_mids <- function(vec, var_name = "Variable") {
  x <- sort(vec)
  n <- length(x)
  
  etiquetas <- c("M", "F", "E", "D", "C", "B", "A")
  nombres <- c("Median", "Fourth", "Eighth", "Sixteenth", 
               "Thirty-secondth", "Sixty-fourth", "twenty-eighth")
  
  resultados <- list()
  d <- (n + 1) / 2
  
  for (i in seq_along(etiquetas)) {
    d_floor <- floor(d)
    d_frac <- d - d_floor
    
    val_L <- x[d_floor] + d_frac * (x[d_floor + 1] - x[d_floor])
    val_U <- x[n - d_floor + 1] - d_frac * (x[n - d_floor + 1] - x[n - d_floor])
    
    mid <- (val_U + val_L) / 2
    
    resultados[[i]] <- tibble(
      Variable = var_name,
      Letra = etiquetas[i],
      Profundidad = d,
      Lower = val_L,
      Upper = val_U,
      Mid = mid,
      Amplitud = val_U - val_L
    )
    
    if (d <= 1) break
    d <- (floor(d) + 1) / 2
  }
  
  bind_rows(resultados)
}

#1 Obtener resúmenes intermedios para las 4 variables originales
mids_dep_time       <- calc_mids(flightsRed$dep_time, "dep_time")
mids_sched_dep_time <- calc_mids(flightsRed$sched_dep_time, "sched_dep_time")
mids_arr_time       <- calc_mids(flightsRed$arr_time, "arr_time")
mids_sched_arr_time <- calc_mids(flightsRed$sched_arr_time, "sched_arr_time")

mids_totales <- bind_rows(mids_dep_time, mids_sched_dep_time, mids_arr_time, mids_sched_arr_time)
print(mids_totales, n=30)


#2. Determinar si los datos son simetricos
# Medida de Asimetría de Hinkley: d = (Mean - Median) / dF
hinkley_index <- function(vec) {
  x <- sort(vec)
  n <- length(x)
  
  # Profundidad de Mediana y Cuartos
  d_M <- (n + 1) / 2
  d_F <- (floor(d_M) + 1) / 2
  
  d_F_floor <- floor(d_F)
  d_F_frac <- d_F - d_F_floor
  
  F_L <- x[d_F_floor] + d_F_frac * (x[d_F_floor + 1] - x[d_F_floor])
  F_U <- x[n - d_F_floor + 1] - d_F_frac * (x[n - d_F_floor + 1] - x[n - d_F_floor])
  
  dF <- F_U - F_L
  media <- mean(vec)
  mediana <- median(vec)
  
  d_hinkley <- (media - mediana) / dF
  return(d_hinkley)
}

hinkley_result <- tibble(
  Variable = c("dep_time", "sched_dep_time", "arr_time", "sched_arr_time"),
  Hinkley_d = c(
    hinkley_index(flightsRed$dep_time),
    hinkley_index(flightsRed$sched_dep_time),
    hinkley_index(flightsRed$arr_time),
    hinkley_index(flightsRed$sched_arr_time)
  )
)
print(hinkley_result)

# Gráfico de simetría
x11()
ggplot(mids_totales, aes(x = factor(Letra, levels = c("M", "F", "E", "D", "C", "B", "A")), y = Mid, group = Variable, color = Variable)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    title = "Verificación de Simetría",
    subtitle = "Tendencia creciente = Sesgo derecha | Tendencia decreciente = Sesgo izquierda | Horizontal = Simetría",
    x = "Letras Descriptivas",
    y = "Mid-Summary"
  ) +
  theme_minimal()

# Función de transformación Tp(X) = (X^p - 1) / p
trans_potencia <- function(x, p) {
  if (p == 0) {
    return(log(x))
  } else {
    return((x^p - 1) / p)
  }
}

#3 Transformación de  los datos: con p = 0, p= 0.5 y p= 1.5
flightsTrans <- flightsRed %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor") %>%
  mutate(
    `p = 1.5` = trans_potencia(Valor, 1.5),
    `p = 1.0 (Sin Cambio)` = trans_potencia(Valor, 1.0),
    `p = 0.5 (Raíz)` = trans_potencia(Valor, 0.5)
  ) %>%
  pivot_longer(cols = starts_with("p ="), names_to = "Potencia", values_to = "Valor_Trans")

#4 Gráficas para evaluar la transformación
x11()
ggplot(flightsTrans, aes(x = Valor_Trans, fill = Variable)) +
  geom_histogram(aes(y = ..density..), bins = 40, alpha = 0.6, color = "black") +
  geom_density(color = "blue", size = 0.8) +
  facet_grid(Variable ~ Potencia, scales = "free_x") +
  labs(
    title = "Transformación mediante Familia de Potencias",
    subtitle = "Evaluación de forma ajustando escala horizontal (scales = 'free_x')",
    x = "Valor Transformado Tp(X)",
    y = "Densidad"
  ) +
  theme_bw() +
  theme(legend.position = "none")
