library(ggplot2)
library(tidyverse)

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//gestation.periods.txt"
dataGesAnimal <- read.table(namefile, header = TRUE, sep = "\t")

#Descripcion de datos
summary(dataGesAnimal)
View(dataGesAnimal)


#Cantidad de registros
n <- dim(dataGesAnimal)[1]
#Mediana-period
M = median(dataGesAnimal$Period)
depthm <- (n+1)/2
#Fourth
depthf <- (depthm + 1)/2
#Eighth
depthe <- (depthf + 1)/2

#ordenado ascendentemente
ordered_periodLu <- sort(dataGesAnimal$Period)

#ordenado descendentemente
ordered_perioduL <- sort(dataGesAnimal$Period, decreasing = TRUE)
twoperiod <- data.frame(periodLu = ordered_periodLu, perioduL = ordered_perioduL)

#Fourth
indexFL <- floor(depthf)
indexFU <- ceiling(depthf)

#lower fourth 
F_L <- (ordered_periodLu[indexFU] + ordered_periodLu[indexFL]) /2
#upper fourth
F_U <- (ordered_perioduL[indexFL] + ordered_perioduL[indexFU]) /2

#Eighth
indexFL1 <- floor(depthe)
indexFU1 <- ceiling(depthe)

#lower eighth 
F_L1 <- (ordered_periodLu[indexFU1] + ordered_periodLu[indexFL1]) /2
#upper fourth
F_U1 <- (ordered_perioduL[indexFL1] + ordered_perioduL[indexFU1]) /2

#Valores extremos
#Limites internos
df <- F_U - F_L
Ll <- F_L - 1.5*df
Lu <- F_U + 1.5*df

#Limites externos
LL <- F_L - 3.0*df
LU <- F_U + 3.0*df

period <- dataGesAnimal$Period
valExtr_leve_index <- period < Ll | period >Lu
valExtrexLeve <- dataGesAnimal[valExtr_leve_index,]
valExtrexLeve










