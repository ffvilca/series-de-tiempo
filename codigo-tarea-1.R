# Codigo R

# P1

#Cargar librerias
library(tseries)
library(lubridate)
library(tidyverse)
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

ggplot(base, aes(GSPC.Adjusted)) +
  geom_boxplot()

# P2

summary(ts_base)

# P3

plot(ts_base)
#abline(a = moda$locations, b= 0, add = T)

base %>% 
  ggplot(aes(GSPC.Close, )) +
  geom_line()


# P4

# a
p.mov.2 = ma(ts_base, order = 2)
p.mov.10 = ma(ts_base, order = 10)

# b

ts <- c(ts_base, rep(0,6))

ts_matrix <- matrix(ts, ncol = 12, byrow = T)

pesos_b <- c(0.5,0.2,0.2,0.1)
p.mov.pond.4 <- stats::filter(ts_base, pesos_b, sides=1)

suma <- sum(colMeans(ts_matrix))
pesos.b.2 <- round(colMeans(ts_matrix)/suma,4)

p.mov.pond.12 <- stats::filter(ts_base, pesos.b.2, sides=1)


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

library(aTSA)
coneldo.holt_0502 = Holt(ts_base, alpha = 0.5, beta=0.2)
coneldo.holt_0508 = Holt(ts_base, alpha = 0.5, beta=0.8)


coneldo.holt_0502
coneldo.holt_0508

# f

ts_base_2 <- ts(ts_matrix,start=2001, frequency = 12)

hw.ts.1 = hw(ts_base, h=12,optim.start = c(0.5,0.2,0.8))     # :c
hw.ts.2 = hw(ts_base, h=12, seasonal='additive')

plot(coneldo.hw1)
plot(coneldo.hw2)




