library(ggplot2)
library(tidyverse)

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//gestation.periods.txt"
dataGesAnimal <- read.table(namefile, header = TRUE, sep = "\t")

#Descripcion de datos
summary(dataGesAnimal)
View(dataGesAnimal)


