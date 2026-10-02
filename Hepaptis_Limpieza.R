library(pastecs)
library(summarytools)
library(DataExplorer)
library(ggplot2)
library(dlookr)
library(tidyverse)

# ------------------------------------------------------------------------------
# Carga de los datos
# ------------------------------------------------------------------------------
setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//hepatitis.data"

namescol <- c('CLASS', 'AGE', 'SEX', 'STEROID', 'ANTIVIRALS', 'FATIGUE', 'MALAISE',
              'ANOREXIA', 'LIVERB', 'LIVERF', 'SPLEENP', 'SPIDERS', 'ASCITES', 
              'VARICES', 'BILIRUBIN', 'ALK_PHOSPHATE', 'SGOT', 'ALBUMIN', 'PROTIME', 'HISTOLOGY')

# na.strings convierte '?' a NAs automáticamente al leer el archivo
dataH <- read.table(namefile, header = FALSE, sep = ',', na.strings = c("?", "NA"), col.names = namescol)
summary(dataH)

# Definición de listas
quantitative_vars_names <- c('AGE', 'BILIRUBIN', 'ALK_PHOSPHATE', 'SGOT', 'ALBUMIN', 'PROTIME')

qualitative_vars_names <- c('CLASS', 'SEX', 'STEROID', 'ANTIVIRALS', 'FATIGUE', 'MALAISE', 
                            'ANOREXIA', 'LIVERB', 'LIVERF', 'SPLEENP', 'SPIDERS', 'ASCITES', 
                            'VARICES', 'HISTOLOGY')
x11()
plot_missing(dataH)

# ------------------------------------------------------------------------------
# Tratamiento de datos faltantes (NA's): Imputación por Moda / tercer cuartil
# ------------------------------------------------------------------------------
# Función para obtener la moda
get_mode <- function(v) {
  v_clean <- v[!is.na(v)]
  uniqv <- unique(v_clean)
  uniqv[which.max(tabulate(match(v_clean, uniqv)))]
}

# Imputación
dataH_imputed <- dataH %>%
  # Imputación de categóricas con la MODA
  mutate(across(all_of(qualitative_vars_names), ~ ifelse(is.na(.), get_mode(.), .))) %>%
  # Imputación de cuantitativas con la MEDIANA
  mutate(across(all_of(quantitative_vars_names), ~ ifelse(is.na(.), quantile(., 0.75, na.rm = TRUE), .))) %>%
  # Convertir cualitativas a factor
  mutate(across(all_of(qualitative_vars_names), as.factor))

cat("\n--- Resumen Dataset Imputado (Sin NAs) ---\n")
summary(dataH_imputed)
x11()
plot_missing(dataH_imputed)
x11()
plot_correlation(dataH_imputed[quantitative_vars_names])

# ----------------------------------------------------------------------
# Removiendo na´s (complementario)
#---------------------------------------------------------------------------

dataH_noNA <- na.omit(dataH)


dataH_noNA <- dataH_noNA %>%
  mutate(across(all_of(qualitative_vars_names), as.factor))


cat("Filas iniciales:", nrow(dataH), "\n")
cat("Filas sin NAs:", nrow(dataH_noNA), "\n")
x11()
plot_correlation(dataH_noNA[quantitative_vars_names])


# ------------------------------------------------------------------------------
# Remover registros completos con datos extremos (Outliers)
# ------------------------------------------------------------------------------
# Evaluamos fila por fila si alguna variable cuantitativa sobrepasa el rango 1.5 * IQR
es_outlier_fila <- apply(dataH_imputed[quantitative_vars_names], 1, function(row) {
  any(sapply(quantitative_vars_names, function(col) {
    x <- dataH_imputed[[col]]
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    row[col] < (q1 - 1.5 * iqr) | row[col] > (q3 + 1.5 * iqr)
  }))
})

# Conservamos solo las filas sin outliers
dataH_Limpio <- dataH_imputed[!es_outlier_fila, ]

cat("\n--- Resumen Dataset Final (Sin NAs y sin filas con Outliers) ---\n")
cat("Registros originales:", nrow(dataH), "\n")
cat("Registros conservados:", nrow(dataH_Limpio), "\n")
summary(dataH_Limpio)

# ------------------------------------------------------------------------------
# Visualización comparativa
# ------------------------------------------------------------------------------
x11()
boxplot(dataH_imputed[quantitative_vars_names], main = "Con Outliers (Imputado)")
x11()
boxplot(dataH_Limpio[quantitative_vars_names], main = "Sin Outliers (Registros eliminados)")
x11()
plot_density(dataH_Limpio[quantitative_vars_names])
x11()
plot_boxplot(dataH_Limpio[quantitative_vars_names], by='AGE')
x11()
plot_correlation(dataH_Limpio[quantitative_vars_names])
