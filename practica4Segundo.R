library(ggplot2)
library(tidyverse)

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//boston.marathon.wtimes.txt"
dataBoston <- read.table(namefile, header = TRUE, sep = "\t")

modelo1 <- lm(minutes ~ year , data = dataBoston)
y_hat1 <- predict(modelo1)
summary(modelo1)

y1 <- dataBoston$minutes
x1 <- dataBoston$year
beta1_hat1 <- coef(modelo1)["year"]
n1 <- nrow(dataBoston)

residuos1 <- y1 - y_hat1
s_e1 <- sqrt(sum(residuos1^2) / (n1 - 2))
ss_x1 <- sum((x1 - mean(x1))^2)
SE_beta1 <- s_e1 / sqrt(ss_x1)
t_stat1 <- (beta1_hat1 - 0) / SE_beta1

p_val <- 2 * pt(abs(t_stat1), df = n1 - 2, lower.tail = FALSE)

cat("--- RESULTADOS CALCULADOS PASO A PASO ---\n")
cat("s_e (Error estándar de residuos):", s_e1, "\n")
cat("SE(beta1):                       ", SE_beta1, "\n")
cat("Estadístico t calculado:         ", t_stat1, "\n")
cat("p-valor calculado:               ", p_val, "\n\n")
