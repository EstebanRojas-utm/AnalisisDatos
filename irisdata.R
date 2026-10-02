library(ggplot2)
library(tidyverse)

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//iris.data"
datairis <- read.table(namefile, header = FALSE, sep = ",")

names(datairis) <- c('sepalL', 'sepalW', 'petalL', 'petalW', 'clase')
datairis$clase <- as.factor(datairis$clase)

summary(datairis)


x11()
ggplot(data = datairis, aes(x = petalL, fill = clase, colour = clase)) + 
  geom_density(alpha = 0.1) + theme_minimal() 
  

x11()
ggplot(data = datairis, aes(x = sepalL, fill = clase, colour = clase)) + 
  geom_density(alpha = 0.3) + theme_minimal() 
 

x11()
ggplot(data = datairis, aes(x = petalW, fill = clase, colour = clase)) + 
  geom_density(alpha = 0.1) + theme_minimal() 


x11()
ggplot(data = datairis, aes(x = sepalW, fill = clase, colour = clase)) + 
  geom_density(alpha = 0.3) + theme_minimal() 

##---Estadisticas globales-----------##
global_stats <- datairis %>%
  summarise(across(where(is.numeric), list(media = mean, sd = sd))) %>%
  pivot_longer(everything(), 
               names_to = c("variable", ".value"), 
               names_sep = "_")

cat("=== ESTADÍSTICAS GLOBALES ===\n")
print(global_stats)

##---------Estadisticas por clase ------------------##
stats_por_clase <- datairis %>%
  group_by(clase) %>%
  summarise(across(where(is.numeric), list(media = mean, sd = sd), .names = "{.col}_{.fn}"))

cat("\n=== ESTADÍSTICAS POR CLASE ===\n")
print(stats_por_clase)

#-----------Calculo del fisher-------------#
factor_fisher <- datairis %>%
  pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "valor") %>%
  group_by(variable) %>%
  mutate(media_global = mean(valor)) %>%
  group_by(variable, clase) %>%
  summarise(
    n = n(),
    media_clase = mean(valor),
    ss_within = sum((valor - mean(valor))^2),
    media_global = first(media_global),
    .groups = "drop_last"
  ) %>%
  summarise(
    ss_between = sum(n * (media_clase - media_global)^2),
    ss_within = sum(ss_within),
    factor_fisher = ss_between / ss_within
  ) %>%
  arrange(desc(factor_fisher))

cat("\n=== FACTOR DE FISHER (ORDENADO POR SEPARABILIDAD) ===\n")
print(factor_fisher)