# Codigo R

# P1

#Cargar librerias
library(tseries)
library(lubridate)
library(dplyr)
library(lmtest) # por revisar
library(quantmod)
library(forecast)
library(ggplot2)
library(aTSA)
library(multimode)

# library(mFilter)
# library(dynlm)
# library(nlme)
# library(knitr)
# library(MASS)
# library(parallel)
# 
# library(fpp2)
# library(stats)

#Tutorial Como importar datos de Yahoo Finance a RStudio

sdate <- "2001-01-01"
fdate <- "2020-06-30"
amazonprices=getSymbols('AMZ', from=sdate, auto.assign = F)
View(amazonprices)

#otro ejercicio
# mdate1="2001-01-04"
# amazonprices1=getSymbols('AMZ', from=mdate, auto.assign = F)[,4]
# print(amazonprices1)

#Tasa de crecimiento
# amazonroc=ROC(amazonprices1, type='discret')
# print(amazonroc)

#Tasa de Rendimiento

# amazonrend = periodReturn(amazonprices, period = 'monthly', type = 'log', # subset = '2018')
# print(amazonrend)

#Obtener datos de S P 500

getSymbols("^GSPC", src = "yahoo", from = "2001-01-01", to = "2020-06-30", periodicity= "monthly")

getSymbols("CX", src = "yahoo", from = "2001-01-01", to = "2020-06-30", periodicity= "monthly")

getSymbols("BIMBOA.MX", src = "yahoo", from = "2001-01-01", to = "2020-06-30", periodicity= "monthly")

getSymbols("AZTECACPO.MX", src = "yahoo", from = "2001-01-01", to = "2020-06-30", periodicity= "monthly")

# Armar base

base <- CX[,6]

names(CX)

# Armar serie de tiempo

ts_base = ts(base, start=2001, frequency = 12)
ts_base
View(ts_base)

# P1

media <- mean(ts_base)

moda <- locmodes(ts_base, mod0 = 1)

cuartiles <- quantile(ts_base,probs = c(0.25,0.5,0.75)) 
max <- max(ts_base)
min <- min(ts_base) 
varianza <- var(ts_base)

boxplot(ts_base)

ggplot(base, aes( x = CX.Adjusted, y = "")) +
  geom_boxplot(fill = "#390099", color = "black",outlier.colour= "#FFBD00" ,outlier.size=2) +
  labs(x="Valor de la acción de CEMEX",y= "",caption = "Para el périodo del 01-01-2001 al 30-06-2020")+
  ggtitle("Dispersón de la acción de CEMEX")+
  coord_flip()+
  theme_bw(base_size = 12)

# P2

summary(ts_base)

boxplot(ts_base ~ cycle(ts_base),
       col = "#390099", # Color de relleno
       border = "#ff0054", #Color de contorno
       main='Variación de la acción de CEMEX por mes',
       xlab= 'Tiempo',
       ylab='Valor de la acción',
       names = month.abb # Agregar nombres puede ser month.abb o month.name o 
       #cualquier vector personalizado
)

ggseasonplot(ts_base,
           polar=FALSE, 
           main="Periodicidad del valor de la acción de CEMEX")+
  labs(x="Meses",
       y= "Valor de la acción",
       caption = "Para el périodo del 01-01-2001 al 30-06-2020",
       color = "Años")+
  scale_color_manual(values = rainbow(20))+
  theme_bw()
# P3

plot(ts_base)
#abline(a = moda$locations, b= 0, add = T)


CX_base = data.frame(ts_base,time = seq(ISOdate(2001,01,01), 
                                     ISOdate(2020,06,30),
                                     by = "month"))

ggplot(CX_base, aes(x = time, y = CX.Adjusted))+ #Se inicializa la gráfica
  geom_line(color = "#390099") + #Se declara una gráfica de línea
  labs(x = 'Tiempo', y = 'Precio de la acción', 
       title = 'Serie de tiempo de la acción de CEMEX')


# P4

# a
p.mov.2 = ma(ts_base, order = 2)
p.mov.10 = ma(ts_base, order = 10)

# b

ts <- c(ts_base, rep(0,6))

ts_matrix <- matrix(ts, ncol = 12, byrow = T)

pesos.b1 <- c(0.5,0.2,0.2,0.1)
p.mov.pond.4 <- stats::filter(ts_base, pesos.b1, sides=1)

suma <- sum(colMeans(ts_matrix))
pesos.b2 <- round(colMeans(ts_matrix)/suma,4)

p.mov.pond.12 <- stats::filter(ts_base, pesos.b2, sides=1)


# c 

p.mov.2.c = ma(ts_base, order = 2, centre = TRUE)
p.mov.10.c = ma(ts_base, order = 10, centre = TRUE)

# d

ts.base.sua_001 = ses(ts_base, alpha=0.01, initial = 'simple')
ts.base.sua_05 = ses(ts_base, alpha=0.5, initial = 'simple')
ts.base.sua_099 = ses(ts_base, alpha=0.99, initial = 'simple')

ts.base.sua_001
ts.base.sua_05
ts.base.sua_099

# e

ts.holt_0502 = Holt(ts_base, alpha = 0.5, beta=0.2)
ts.holt_0508 = Holt(ts_base, alpha = 0.5, beta=0.8)
ts.holt_0502
ts.holt_0508

# f

hw.ts.1 = hw(ts_base, h=12,optim.start = c(0.5,0.2,0.8))
hw.ts.2 = hw(ts_base, h=12, seasonal='additive')

# P5 

# a

plot(ts_base, type='l',col = "#390099", lwd = 2,
     xlab = 'Tiempo', ylab = 'Precio de la acción',
     main = "Serie de tiempo de la acción de CEMEX \n Promedio móviles simples")
lines(p.mov.2, col='#ff0054', lwd = 2)
lines(p.mov.10, col='#ffbd00', lwd = 2)
legend("topright",
       col=c("#390099","#FF0054","#FFBD00"),
       lty=1,lwd=4,cex=0.8,
       legend=c("Precio Acción","PS orden 2","PS orden 10"))  


# b

plot(ts_base, type='l',col = "#390099", lwd = 2,
     xlab = 'Tiempo', ylab = 'Precio de la acción',
     main = "Serie de tiempo de la acción de CEMEX \n Promedio móviles ponderados")
lines(p.mov.pond.4, col='#ff0054', lwd = 2)
lines(p.mov.pond.12, col='#ffbd00', lwd = 2)
legend("topright",
       col=c("#390099","#FF0054","#FFBD00"),
       lty=1,lwd=4,cex=0.8,
       legend=c("Precio Acción","PPM orden 4","PPM orden 12"))

# c

plot(ts_base, type='l',col = "#390099", lwd = 2,
     xlab = 'Tiempo', ylab = 'Precio de la acción',
     main = "Serie de tiempo de la acción de CEMEX \n Promedio móviles simples centrados")
lines(p.mov.2.c, col='#ff0054', lwd = 2)
lines(p.mov.10.c, col='#ffbd00', lwd = 2)
legend("topright",
       col=c("#390099","#FF0054","#FFBD00"),
       lty=1,lwd=4,cex=0.8,
       legend=c("Precio Acción","PSC orden 2","PSC orden 10"))


# d

par(mfrow = c(2,2))
plot(ts_base, type='l',col = "#390099", lwd = 2,
     xlab = 'Tiempo', ylab = 'Precio de la acción',
     main = "Serie de tiempo de la acción de CEMEX")

plot(ts.base.sua_001, type='l',col = "#ff0054", lwd = 2,
     xlab = 'Tiempo', ylab = 'Precio de la acción',
     main = expression(paste("Serie de tiempo de la acción de CEMEX \n Suavización Exponencial Simple con",alpha == 0.01)))

plot(ts.base.sua_05, type='l',col = "#ff5400", lwd = 2,
     xlab = 'Tiempo', ylab = 'Precio de la acción',
     main = expression(paste("Serie de tiempo de la acción de CEMEX \n Suavización Exponencial Simple con",alpha == 0.5)))

plot(ts.base.sua_099, type='l',col = "#ffbd00", lwd = 2,
     xlab = 'Tiempo', ylab = 'Precio de la acción',
     main = expression(paste("Serie de tiempo de la acción de CEMEX \n Suavización Exponencial Simple con",alpha == 0.99)))

# e

par(mfrow = c(1,1))

plot(ts_base, type='l',col = "#390099", lwd = 2,
     xlab = 'Tiempo', ylab = 'Precio de la acción',
     main = expression(paste("Serie de tiempo de la acción de CEMEX \n Suavización Holt con",alpha == 0.5)))
lines(ts.holt_0502$estimate, col='#ff0054', lwd = 2)
lines(ts.holt_0508$estimate, col='#ffbd00', lwd = 2)
legend("topright",
       col=c("#390099","#FF0054","#FFBD00"),
       lty=1,lwd=4,cex=0.8,
       legend=c("Precio Acción",expression(beta == 0.2),
                expression(beta == 0.8)))

# f

plot(ts_base, type='l',col = "#390099", lwd = 2,
     xlab = 'Tiempo', ylab = 'Precio de la acción',
     main = "Serie de tiempo de la acción de CEMEX \n Suavización Holt Winters")
lines(hw.ts.1$fitted, col='#ff0054', lwd = 2)
lines(hw.ts.2$fitted, col='#ffbd00', lwd = 2)
legend("topright",
       col=c("#390099","#FF0054","#FFBD00"),
       lty=1,lwd=4,cex=0.8,
       legend=c("Precio Acción",expression(paste(alpha == 0.5," ",beta == 0.2," ",gamma == 0.8)),"Automática"))

# P6

acf(ts_base,col = "#390099",xlab = "",ylab = "Autocorrelación",main = "Gráfico de Autocorrelación para la Serie \n del valor de la acción de CEMEX", lwd  =2)
