library(ggplot2)
library(tidyverse)

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//boston.marathon.txt"
dataMarathon <- read.table(namefile, header = TRUE, sep = "\t")

#Descripcion de los datos
View(dataMarathon)
summary(dataMarathon)

#Convertir age en factor
dataMarathon$age <- as.factor(dataMarathon$age)
summary(dataMarathon)

#1.Para la variable time, encontrar el valor de la mediana y  los valores inferior y superior

n <- dim(dataMarathon)[1]
depthm <- (n+1)/2
#Fourth
depthf <- (depthm + 1)/2
#Eighth
depthe <- (depthf + 1)/2
#Sixteenth
depthd <- (depthe + 1)/2

# Ordenado ascendentemente
ordered_timeLu <- sort(dataMarathon$time)
# Ordenado descendentemente
ordered_timeuL <- sort(dataMarathon$time, decreasing = TRUE)
twotime <- data.frame(timeLu = ordered_timeLu, timeuL = ordered_timeuL)

#   Fourth (F) 
indexFL <- floor(depthf)
indexFU <- ceiling(depthf)

# lower fourth 
F_L <- (ordered_timeLu[indexFU] + ordered_timeLu[indexFL]) / 2
# upper fourth
F_U <- (ordered_timeuL[indexFL] + ordered_timeuL[indexFU]) / 2

#  Eighth (E)
indexEL <- floor(depthe)
indexEU <- ceiling(depthe)

# lower eighth 
E_L <- (ordered_timeLu[indexEU] + ordered_timeLu[indexEL]) / 2
# upper eighth
E_U <- (ordered_timeuL[indexEL] + ordered_timeuL[indexEU]) / 2

# Sixteenth (D)
indexDL <- floor(depthd)
indexDU <- ceiling(depthd)

# lower sixteenth 
D_L <- (ordered_timeLu[indexDU] + ordered_timeLu[indexDL]) / 2
# upper sixteenth
D_U <- (ordered_timeuL[indexDL] + ordered_timeuL[indexDU]) / 2

#Mediana-period
med = median(dataMarathon$time)

# Imprimir resultados del Punto 1
cat("Mediana:", med, "\n")
cat("Valores F (Inferior, Superior):", F_L, "-", F_U, "\n")
cat("Valores E (Inferior, Superior):", E_L, "-", E_U, "\n")
cat("Valores D (Inferior, Superior):", D_L, "-", D_U, "\n\n")

#2. descripción gráfica de la variable time: density, boxplot
# Gráfico de densidad 
x11()
g_density <- ggplot(dataMarathon, aes(x = time)) +
  geom_density(fill = "lightblue", alpha = 0.1) +
  labs(title = "Gráfico de Densidad - Time")
print(g_density)
x11()
# Boxplot 
g_boxplot <- ggplot(dataMarathon, aes(y = time)) +
  geom_boxplot(fill = "orange", alpha = 0.3) +
  labs(title = "Boxplot - Time") 
print(g_boxplot)

#3. valores extremos para la variable time de acuerdo a los valores de F

#Valores extremos
#Limites internos
df <- F_U - F_L
Ll <- F_L - 1.5*df
Lu <- F_U + 1.5*df

#Limites externos
LL <- F_L - 3.0*df
LU <- F_U + 3.0*df

time_var <- dataMarathon$time
valExtr_leve_index <- time_var < Ll | time_var >Lu
valExtrexLeve <- dataMarathon[valExtr_leve_index,]
valExtrexLeve

#4.descripción gráfica de la variable time vs age.
x11()
g_boxplot_age <- ggplot(dataMarathon, aes(x = age, y = time, fill = age)) +
  geom_boxplot() +
  labs(title = "Boxplot de Time vs Age") +
  theme_minimal()
print(g_boxplot_age)

#5. valores extremos para cada partición 
valExtremos_PorEdad <- dataMarathon %>%
  # 1. Filtramos para ignorar cualquier fila vacía o con NAs que haya generado el txt
  filter(!is.na(age) & !is.na(time)) %>% 
  group_by(age) %>%
  mutate(
    # 2. Agregamos na.rm = TRUE por precaución
    F_L_grupo = quantile(time, 0.25, na.rm = TRUE),
    F_U_grupo = quantile(time, 0.75, na.rm = TRUE),
    
    # Calculamos el df y los límites para el grupo específico
    df_grupo = F_U_grupo - F_L_grupo,
    Ll_grupo = F_L_grupo - 1.5 * df_grupo,
    Lu_grupo = F_U_grupo + 1.5 * df_grupo,
    
    # Condición de extremo
    es_extremo = time < Ll_grupo | time > Lu_grupo
  ) %>%
  filter(es_extremo == TRUE) %>%
  select(age, time) # Nos quedamos solo con las columnas importantes

cat("\n Valores Extremos Particionados por 'Age'\n")
print(valExtremos_PorEdad)
