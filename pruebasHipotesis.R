#1. Datos origen

horas <- c(2, 4, 5, 7, 8, 10, 11, 13, 14, 16)
calificacion <- c(55, 62, 65, 70, 78, 83, 85, 89, 95, 99)
n <- length(horas)

# 2. Ajustar el modelo para obtener las predicciones (y_hat)
modelo <- lm(calificacion ~ horas)
y_hat <- predict(modelo)
summary(modelo)

#Calculo estadistico t para beta1
y <- calificacion
x <- horas
beta1_hat <- coef(modelo)["horas"]

residuos <- y - y_hat
s_e <- sqrt(sum(residuos^2) / (n - 2))
ss_x <- sum((x - mean(x))^2)
SE_beta1 <- s_e / sqrt(ss_x)
t_stat <- (beta1_hat - 0) / SE_beta1

p_val <- 2 * pt(abs(t_stat), df = n - 2, lower.tail = FALSE)


cat("--- RESULTADOS CALCULADOS PASO A PASO ---\n")
cat("s_e (Error estándar de residuos):", s_e, "\n")
cat("SE(beta1):                       ", SE_beta1, "\n")
cat("Estadístico t calculado:         ", t_stat, "\n")
cat("p-valor calculado:               ", p_val, "\n\n")

cat("--- VERIFICACIÓN CON SUMMARY(MODELO) ---\n")
summary(modelo)

