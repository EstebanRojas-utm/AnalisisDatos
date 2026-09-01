library(ggplot2)
library(tidyverse)

data(diamonds)
View(diamonds)
summary(diamonds)

registros <- dim(diamonds)[1]
variables <- dim(diamonds)[2]

cat("Diamonds tiene ", registros, " registros y ", variables, "variables\n")
str(diamonds)
class(diamonds)

#Obtener la media, desviacion, min, max
stdCarat <- sd(diamonds$carat)
meanCarat <- mean(diamonds$carat)
minCarat <- min(diamonds$carat)
maxCarat <- max(diamonds$carat)

#Cuantiles
Q1 <- quantile(diamonds$carat, c(0.25), type = 6)
Q2 <- quantile(diamonds$carat, c(0.50), type = 6)
Q3 <- quantile(diamonds$carat, c(0.75), type = 6)
IQR <- Q3-Q1
stdTable <- sd(diamonds$table, na.rm = TRUE)
print(stdCarat)
print(meanCarat)

cat("El valor minimo es", minCarat, "Y el valor maximo es ", maxCarat, "\nEl valor promedio es", meanCarat, "y la desviación estandar es ", stdCarat)

#Datos Categoricas: Cut, Color, Clarity
#Variable: Cut
#Frecuencias
freqcut <- table(diamonds$cut)
print(freqcut)
#Moda
modacut <- mode(diamonds$cut)
prop.table(table(diamonds$cut))*100
x11()
pie(freqcut)
x11()
barplot(freqcut, col ="Darkgreen")

#Variable color
freqcolor <- table(diamonds$color)
print(freqcolor)
#Moda
modacolor <- mode(diamonds$color)
prop.table(table(diamonds$color))*100
x11()
pie(freqcolor)
x11()
barplot(freqcolor, col = "Darkred")

#Variable clarity
freqclarity <- table(diamonds$clarity)
print(freqclarity)
#Moda
modaclarity <- mode(diamonds$clarity)
prop.table(table(diamonds$clarity))*100
x11()
pie(freqclarity)
x11()
barplot(freqcolor, col = "Lightblue")















