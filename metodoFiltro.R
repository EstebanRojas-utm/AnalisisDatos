library(ggplot2)
library(tidyverse)

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//autosFeatureS.csv"
dataAutos <- read.table(namefile, header = TRUE, sep = ",")

summary(dataAutos)
View(dataAutos)

namesData <- names(dataAutos)
features <- c(6,7,8,9)
dataAutos$clase <- factor(dataAutos$clase)

x11()
ggplot(data = dataAutos, aes(height, fill=clase, colour=clase)) + geom_density(alpha = 0.1)

x11()
ggplot(data = dataAutos, aes(length, fill= clase, colour = clase)) + geom_density(alpha = 0.1)

x11()
ggplot(data = dataAutos, aes(width, fill = clase, colour = clase)) + geom_density(alpha = 0.1)

x11()
ggplot(data = dataAutos, aes(highway.mpg, fill = clase, colour = clase)) + geom_density(alpha = 0.1)

x11()
ggplot(data = dataAutos, aes(aspiration, fill = clase, colour = clase)) + geom_density(alpha = 0.1)

x11()
ggplot(data = dataAutos, aes(fuel.type, fill = clase, colour = clase)) + geom_density(alpha=0.3)

x11()
ggplot(data = dataAutos, aes(make, fill = clase, colour = clase)) + geom_density(alpha = 0.1)