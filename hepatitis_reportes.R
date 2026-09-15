library(pastecs)
library(summarytools)
library(DataExplorer)
library(ggplot2)

setwd("C://Users//Alumnos.sc505//Documents//MIA//")
namefile <- ".//datos//hepatitis.data"
dataH <- read.table(namefile, header = FALSE, sep = ',')

#No tiene etiquetas

namescol <- c('CLASS', 'AGE', 'SEX', 'STEROID', 'ANTIVIRALS', 'FATIGUE', 'MALAISE'
              ,'ANOREXIA', 'LIVERB', 'LIVERF', 'SPLEENP', 'SPIDERS', 'ASCITES', 
              'VARICES', 'BILIRUBIN', 'ALK_PHOSPHATE', 'SGOT', 'ALBUMIN', 'PROTIME', 'HISTOLOGY')

names(dataH) <- namescol
summary(dataH)
str(dataH)

x11()
plot_missing(dataH)
dataHN <- lapply(namescol, function(x) as.numeric(dataH[[x]]))
names(dataHN) <- namescol
dataHN <- as.data.frame(dataHN)
summary(dataHN)
x11()
plot_missing(dataHN)

quantitative_vars_names <- c('AGE', 'BILIRUBIN', 'ALK_PHOSPHATE', 'SGOT', 'ALBUMIN', 'PROTIME')
num_quantitative_vars <- length(quantitative_vars_names)
min_quanti_vars <- sapply(quantitative_vars_names, function(x) min(dataHN[[x]]))
max_quanti_vars <- sapply(quantitative_vars_names, function(x) max(dataHN[[x]]))
cat("Numero de variables cuantitativas: ", num_quantitative_vars, "\n")
cat("Nombre de las variables cuantitativas: ", paste(quantitative_vars_names, collapse = ", "))

x11()
plot_density(dataHN[quantitative_vars_names])
x11()
plot_boxplot(dataHN[quantitative_vars_names], by='AGE')
x11()
plot_correlation(dataHN[quantitative_vars_names])

qualitative_vars_names <- c("CLASS", "SEX", "STEROID", "ANTIVIRALS", "FATIGUE", "MALAISE", "ANOREXIA", "LIVERB", "SPLEENP", "SPIDERS", "ASCITES", "VARICES", "HISTOLOGY")
num_qualitative_vars <- length(qualitative_vars_names)
cat("Numero de variables cualitativas: ", num_qualitative_vars, "\n")
cat("Nombre de las variables cualitativas: ", paste(qualitative_vars_names, collapse = ", "))


dataCateg <- lapply(qualitative_vars_names, function(x) factor(dataHN[[x]]))
names(dataCateg) <- qualitative_vars_names
dataCateg <- as.data.frame(dataCateg)
summary(dataCateg)
plot_bar(dataCateg)


library(dlookr)
library(tidyverse)

dataHN%>%
  eda_paged_report(target="CLASS", subtitle= "HEPATITIS", output_dir = "./", output_file = "EDA.pdf", theme="blue" )

#Limpiar datos 


