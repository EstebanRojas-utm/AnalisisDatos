library(ggplot2)
library(tidyverse)

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//boston.marathon.wtimes.txt"
dataBoston <- read.table(namefile, header = TRUE, sep = "\t")

summary(dataBoston)
View(dataBoston)

x11()
ggplot(data = dataBoston, aes(year, minutes)) + geom_point() + xlab("YEAR") + ylab("TIME")

x11()
ggplot(data = dataBoston, aes(year, minutes)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + xlab("YEAR") + ylab("TIME")

#Obtener el modelo de regresion
modelRL <- lm(minutes ~ year , data = dataBoston)
summary(modelRL)

b0 = modelRL$coefficients[1]
b1 = modelRL$coefficients[2]
FIT = b0+b1*dataBoston$year

x11()
#Grafica de residuos vs valores ajustados
dataBoston %>%
  mutate(FIT = b0 + b1 * dataBoston$year,
         Residuos = minutes - FIT) %>%
  ggplot(aes(FIT, Residuos)) + geom_point() + geom_hline(yintercept = 0, color = "red") + xlab("Valores ajustados")

x11()
#Grafica de residuos vs variable predictora
dataBoston %>%
  mutate(FIT = b0 + b1 * dataBoston$year,
         Residuos = minutes - FIT) %>%
  ggplot(aes(year, Residuos)) + geom_point() + geom_hline(yintercept = 0, color = "darkred") + xlab("Predictora = year")


#Evaluacion de metricas
residuos <- residuals(modelRL)
n <- nrow(dataBoston)
#Suma de errores al cuadrado
RSS <- sum(residuos^2)
#Error cuadratico medio
RMSE <- sqrt(RSS/n)
RSE  <- sqrt(RSS / df.residual(modelRL))
R2   <- summary(modelRL)$r.squared

cat("--- MÉTRICAS DE VALIDACIÓN ---\n")
cat("RSS:  ", RSS, "\n")
cat("RMSE: ", RMSE, "\n")
cat("RSE:  ", RSE, "\n")
cat("R2:   ", R2, "\n")



x11()
plot(modelRL, which=2)

x11()
qqnorm(rstandard(modelRL))
qqline(rstandard(modelRL), col="red")

x11()
hist(rstandard(modelRL),  main = "Histograma Residuos estandarizados", xlab = "Residuos estandarizados")
################################################################################################################
#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
################################################################################################################

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namesfile <- ".//datos//threadwear.txt"
dataH <- read.table(namesfile, header = TRUE, sep = "\t")
summary(dataH)
View(dataH)

x11()
ggplot(data = dataH, aes(mileage, groove)) + geom_point() + xlab("YEAR") + ylab("TIME")

x11()
ggplot(data = dataH, aes(mileage, groove)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + xlab("YEAR") + ylab("TIME")

#Obtener el modelo de regresion
modelRL2 <- lm(groove ~ mileage , data = dataH)
summary(modelRL2)

b0 = modelRL2$coefficients[1]
b1 = modelRL2$coefficients[2]
FIT = b0+b1*dataH$groove

x11()
#Grafica de residuos vs valores ajustados
dataH %>%
  mutate(FIT = b0 + b1 * dataH$mileage,
         Residuos = groove - FIT) %>%
  ggplot(aes(FIT, Residuos)) + geom_point() + geom_hline(yintercept = 0, color = "blue") + xlab("Valores ajustados")

x11()
#Grafica de residuos vs variable predictora
dataH %>%
  mutate(FIT = b0 + b1 * dataH$mileage,
         Residuos = groove - FIT) %>%
  ggplot(aes(mileage, Residuos)) + geom_point() + geom_hline(yintercept = 0, color = "darkblue") + xlab("Predictora = year")

x11()
plot(modelRL2, which=2)

x11()
qqnorm(rstandard(modelRL2))
qqline(rstandard(modelRL2), col="red")

x11()
hist(rstandard(modelRL2),  main = "Histograma Residuos estandarizados", xlab = "Residuos estandarizados")

#Evaluacion de metricas
residuos <- residuals(modelRL2)
n <- nrow(dataH)
#Suma de errores al cuadrado
RSS <- sum(residuos^2)
#Error cuadratico medio
RMSE <- sqrt(RSS/n)
RSE  <- sqrt(RSS / df.residual(modelRL2))
R2   <- summary(modelRL2)$r.squared

cat("--- MÉTRICAS DE VALIDACIÓN ---\n")
cat("RSS:  ", RSS, "\n")
cat("RMSE: ", RMSE, "\n")
cat("RSE:  ", RSE, "\n")
cat("R2:   ", R2, "\n")
