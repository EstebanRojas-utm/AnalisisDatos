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

cramersS <- function(X, Y) {
  freqIndx <- table(X)
  freqIndy <- table(Y)
  freqObsxy <- table(Y, X)
  
  pxy_mat <- freqObsxy
  dimPXY <- dim(pxy_mat) # [1] número de filas (Y), [2] número de columnas (X)
  
  # 1. Corrección de ncol y dimensiones
  freqx_mat <- matrix(freqIndx,
                      nrow = dimPXY[1],
                      ncol = dimPXY[2],
                      byrow = TRUE)
  
  freqy_mat <- matrix(freqIndy,
                      nrow = dimPXY[1],
                      ncol = dimPXY[2],
                      byrow = FALSE)
  
  N <- length(X)
  mxy_mat <- (freqx_mat * freqy_mat) / N
  
  # Cálculo de Chi-cuadrado
  XsquareM <- ((pxy_mat - mxy_mat)^2) / mxy_mat
  Xsquare <- sum(XsquareM)
  
  Rx <- length(freqIndx)
  Ky <- length(freqIndy)
  
  min_dim <- min(Rx - 1, Ky - 1)
  
  # Manejo de casos donde min_dim == 0 para evitar división por cero
  if (min_dim == 0) {
    cramersCoef <- 0
  } else {
    cramersCoef <- sqrt(Xsquare / (N * min_dim))
  }
  
  return(cramersCoef)
}
library(reshape2)
namescategoricas <- c('model', 'year', 'trans', 'drv', 'fl', 'class')
cramersMat <- sapply(namescategoricas, function(X)
                  sapply(namescategoricas, function(Y) cramersS(mpg[[X]], mpg[[Y]])))

print(cramersMat)
cramersMat <- as.data.frame(cramersMat)
results <- melt(cramersMat, value.name = "coefCramers")
varsY <- rep(namescategoricas, 6)
print(varsY)
result <- cbind(results, varsY)
print(result)
names(result) <- c("varX", "coefCramers", "varY")
print(result)



#Heatmap vizualizacion con ggplot
results[,1] <- factor(result[,1])
g <- ggplot(results, aes(factor(varX, levels = namescategoricas),
                         factor(varY, levels = namescategoricas))) + 
  geom_tile(aes(fill = coefCramers), colour = "black") + 
  geom_text(aes(label = round(coefCramers,2)))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_gradient(low="white", high = "steelblue") +
  theme_bw() +xlab(NULL) + ylab(NULL) +
  theme(axis.text.x = element_text(angle = -90, hjust = 0))+
  ggtitle("Cramer's V heatmap")
g
x11()
g









print(cramersMat)

# 1. Convertimos la matriz a Data Frame
cramersMat_df <- as.data.frame(cramersMat)

# 2. Aplicamos melt para transformar de formato ancho a formato largo
results <- melt(cramersMat_df, value.name = "coefCramers")

# 3. Creamos la columna varY para emparejar cada combinación
varsY <- rep(namescategoricas, times = length(namescategoricas))

# 4. Construimos el data frame final unificado 'df_heatmap'
df_heatmap <- data.frame(
  varX = results$variable,
  varY = varsY,
  coefCramers = results$coefCramers
)

# 5. Convertimos a factor ordenado para mantener la estructura en el gráfico
df_heatmap$varX <- factor(df_heatmap$varX, levels = namescategoricas)
df_heatmap$varY <- factor(df_heatmap$varY, levels = namescategoricas)

print("--- Data Frame preparado para Heatmap ---")
print(df_heatmap)


g <- ggplot(df_heatmap, aes(x = varX, y = varY)) + 
  geom_tile(aes(fill = coefCramers), colour = "black") + 
  scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, 1)) +
  labs(
    title = "Cramer's V Heatmap",
    x = NULL,
    y = NULL,
    fill = "Cramer's V"
  ) +
  geom_text(aes(label = round(coefCramers,2)))+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid = element_blank()
  )

# Desplegar el gráfico
g
x11()
g


