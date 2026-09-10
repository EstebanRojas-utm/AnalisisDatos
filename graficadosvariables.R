library(ggplot2)
library(dplyr)
library(tidyr)

data(mpg)

x11()
ggplot(data=mpg, mapping = aes(x = cty, y=hwy)) + geom_point()

x11()
ggplot(data = mpg, mapping = aes(x=cty, y = hwy)) + geom_point()+ geom_smooth(method = "lm") + geom_jitter(width=0.5 , size = 1)

x11()
ggplot(data = mpg, mapping = aes(x=displ, y=hwy, color=drv)) + geom_point() + geom_smooth()

x11()
ggplot(data=mpg, mapping = aes(x=cty, y=class)) + geom_boxplot(fill="plum")

x11()
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, color=class)) + facet_wrap(~ class, nrow=2)

x11()
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, color=class)) + facet_grid(drv ~ cyl)

x11()
ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, color=class)) + facet_grid(drv ~.)


#Correlaciones
library(GGally)
varNum <- c('displ', 'cyl', 'cty', 'hwy')
#Grafico de correlacion
x11()
ggcorr(mpg[,varNum])
x11()

#Grafico de correlacion de datos con etiquetas de valores
ggcorr(mpg[,varNum],
       nbreaks = 6,
       label = TRUE,
       label_size = 5,
       color = 'grey50')


print(varNum)
dataAutosNum <- mpg[,varNum]
View(dataAutosNum)

x11()
ggpairs(dataAutosNum, columns = varNum, title = "Análisis de correlacion", 
        upper = list(continuous = wrap("cor", size=3)),
        lower = list(continuous = wrap("smooth", alpha=0.3, size=0.3)))



######## CRAMER´s #########################

cramersS <- function(X,Y) {
  freqIndx <- table(X)
  freqIndy <- table(Y)
  freqObsxy <- table(Y,X)
  pxy_mat <- freqObsxy
  dimPXY <- dim(pxy_mat)
  freqx_mat <- matrix(freqIndx,
                      nrow = dimPXY[1],
                      nol = length(freqIndx),
                      byrow = TRUE)
  freqx_mat
  freqy_mat <- matrix(freqIndy,
                      nrow = length(freqIndx),
                      nol =  dimPXY[2],
                      byrow = FALSE)
  freqy_mat
  freqx_mat
  
  N <- length(X)
  mxy_mat <- (freqx_mat*freqy_mat)/N
  print("Matriz P ")
  print(pxy_mat)
  print("Matriz M")
  print(mxy_mat)
  Xsquare <- ((pxy_mat-mxy_mat) * (pxy_mat - mxy_mat))/(mxy_mat)
  print("XsquareM")
  print(Xsquare)
  Xsquare <- sum(Xsquare)
  print(Xsquare)
  Rx <- length(freqIndx)
  Ky <- length(freqIndy)
  cramersCoef <- sqrt(Xsquare/(N*(min(Rx-1, Ky-1))))
  print(cramersCoef)
  return(cramersCoef)
}

library(reshape2)
namescategoricas <- c('model', 'year', 'trans', 'drv', 'fl')


