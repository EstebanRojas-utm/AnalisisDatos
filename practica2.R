library(ggplot2)
library(tidyverse)

data(mpg)
View(mpg)
summary(mpg)

#1. Cuales son las dimensiones del conjunto de datos
registros <- dim(mpg)[1]
variables <- dim(mpg)[2]

cat("MPG tiene ", registros, " registros y ", variables, "variables\n")

#2. Cuantas variables categoricas
str(mpg)
class(mpg)
print("La base cuenta con 6 variables categoricas\n")

#3. Número de variables numericas
print("La base cuenta con 5 variables numericas\n")

#4Describir las variables numericas
#displ
stdDispl <- sd(mpg$displ)
meanDispl <- mean(mpg$displ)
minDispl <- min(mpg$displ)
maxDispl <- max(mpg$displ)
cat("El valor minimo es", minDispl, "Y el valor maximo es ", maxDispl, "\nEl valor promedio es", meanDispl, "y la desviación estandar es ", stdDispl)

#year
stdyear <- sd(mpg$year)
meanyear <- mean(mpg$year)
minyear <- min(mpg$year)
maxyear <- max(mpg$year)
cat("El valor minimo es", minyear, "Y el valor maximo es ", maxyear, "\nEl valor promedio es", meanyear, "y la desviación estandar es ", stdyear)

#hwy
stdhwy <- sd(mpg$hwy)
meanhwy <- mean(mpg$hwy)
minhwy <- min(mpg$hwy)
maxhwy <- max(mpg$hwy)
cat("El valor minimo es", minhwy, "Y el valor maximo es ", maxhwy, "\nEl valor promedio es", meanhwy, "y la desviación estandar es ", stdhwy)

#5. Describir las variables categoricas
#model
freqmodel <- table(mpg$model)
print(freqmodel)
#Moda
modamodel <- mode(mpg$model)
prop.table(table(mpg$model))*100
x11()
pie(freqmodel)
x11()
barplot(freqmodel, col ="lightgreen")


#trans
freqtrans <- table(mpg$trans)
print(freqtrans)
#Moda
modatrans <- mode(mpg$trans)
prop.table(table(mpg$trans))*100
x11()
pie(freqtrans)
x11()
barplot(freqtrans, col ="lightblue")

#Las variables categoricas convertirla a varianle factor las que son de tipo caracter
#Indentificar las variables cualitativas
namesMGP <- names(mpg)
qualitative_vars <- sapply(mpg, is.character)
qualitative_vars_names <- names(mpg)[qualitative_vars]
num_qualitative_vars <- sum(qualitative_vars)
cat("Número de variables cualitativas: ", num_qualitative_vars, "\n")
cat("nombres de las variables cualitativas: ", 
    paste(qualitative_vars_names, collapse = ", "), "\n")
cat("Valores de las variables cualitativas: \n")
lapply(mpg[, qualitative_vars_names], function(x) unique(x))

#Convertir a datos categoricos en factor
dataCateg <- lapply(qualitative_vars_names, function(x) factor(mpg[[x]]))
names(dataCateg) <- qualitative_vars_names
dataCateg <- as.data.frame(dataCateg)
summary(dataCateg)
x11()
ggplot(data=dataCateg) + geom_bar(mapping = aes(x = class))
x11()
ggplot(data=dataCateg) + geom_bar(mapping = aes(y = class))
x11()
ggplot(data=dataCateg) + geom_bar(mapping = aes(x = class, fill = class))
x11()
ggplot(data=dataCateg) + geom_bar(mapping = aes(y = drv, fill = class))
x11()
ggplot(data=dataCateg) + geom_bar(mapping = aes(y = manufacturer, fill = class))
x11()

#conteo de la frecuencia
dataCateg %>% count(class)
x11()
ggplot(data=dataCateg) + geom_bar(mapping = aes(x = class, fill= manufacturer))
x11()
ggplot(data=dataCateg) + geom_bar(mapping = aes(y = class, fill= model))
x11()
ggplot(data=dataCateg) + geom_bar(mapping = aes(y = class, fill= drv))
x11()
ggplot(data=dataCateg) + geom_bar(mapping = aes(x = class, fill= trans))
x11()
ggplot(data=dataCateg) + geom_bar(mapping = aes(y = class, fill= fl))
x11()
ggplot(data=dataCateg) + geom_bar(mapping = aes(x = class, fill= class))

#variables numericas 
quantitative_vars <- sapply(mpg, is.numeric)
quantitative_vars_names <- names(mpg)[quantitative_vars]
num_quantitative_vars <- length(quantitative_vars)
min_quanti_vars <- sapply(quantitative_vars_names, function(x) min(mpg[[x]]))
max_quanti_vars <- sapply(quantitative_vars_names, function(x) min(mpg[[x]]))
cat("Número de variables cuantitativas: ", num_quantitative_vars, "\n")
cat("Nombres de las variables cuantitativas: ", paste(quantitative_vars_names))
cat("Rango de las variables cuantitativas: \n")
for (idvarN in qualitative_vars_names){
  cat(idvarN, ":[",min_quanti_vars[idvarN],"]")
}

#Graficas de variables continuas
x11()
#Variable con distribución ligeramente sesgada a la derecha
ggplot(data = mpg, mapping = aes(x = cty)) + geom_histogram(bins = 10, fill = "grey") + geom_freqpoly(binwidth = 3, color = "red") + theme_light()
                                                            
x11()
ggplot(data = mpg, aes(x=cty)) + geom_density(alpha=0.1, coulor = "lightblue") + theme_dark()

summary(mpg$cty)
x11()
ggplot(data=mpg, mapping = aes(x=cty)) + geom_boxplot()
x11()
ggplot(data=mpg, mapping = aes(sample=cty)) + stat_qq(color = "red") + stat_qq_line(color="blue")

mpg <- mpg %>% mutate(class=factor(class))
summary(mpg)

x11()
ggplot(data=mpg, aes(cty, fill = class, colour = class)) + geom_density(alpha = 0.1)

#Multiples boxplot
x11()
ggplot(data=mpg, mapping = aes(x=cty, y=class)) + geom_boxplot(fill = "plum")

#Variable hwy
x11()
ggplot(data = mpg, aes(x=hwy)) + geom_density(alpha=0.1, coulor = "blue") + theme_dark()
x11()
ggplot(data=mpg, mapping = aes(x=hwy)) + geom_boxplot()
x11()
ggplot(data=mpg, mapping = aes(sample=hwy)) + stat_qq(color = "red") + stat_qq_line(color="blue")
x11()
ggplot(data=mpg, aes(hwy, fill = class, colour = class)) + geom_density(alpha = 0.1)
x11()
ggplot(data=mpg, mapping = aes(x=hwy, y=class)) + geom_boxplot(fill = "purple")



#Variable cyl
x11()
ggplot(data = mpg, aes(x=cyl)) + geom_density(alpha=0.1, coulor = "blue") + theme_dark()
x11()
ggplot(data=mpg, mapping = aes(x=cyl)) + geom_boxplot()
x11()
ggplot(data=mpg, mapping = aes(sample=cyl)) + stat_qq(color = "red") + stat_qq_line(color="blue")
x11()
ggplot(data=mpg, aes(cyl, fill = class, colour = class)) + geom_density(alpha = 0.1)
x11()
ggplot(data=mpg, mapping = aes(x=cyl, y=class)) + geom_boxplot(fill = "purple")



