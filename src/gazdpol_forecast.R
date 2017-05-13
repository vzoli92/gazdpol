# Gazdaságpolitika II.
# GDP és HICP előrejelzés
# Farsang Bence és Verba Zoltán


# Projekt kezdete: 2017.05.11.

#------------------------------------------------------
# Packages & Data prep.                     
#------------------------------------------------------

# clear console & environment

cat("\014")    
rm(list=ls(all=TRUE))

# installing  & reading packages

# install.packages("xlsx") 
# install.packages("forecast")
# install.packages("tseries")
# install.packages("rio")
library(xlsx)
library(forecast)
library(tseries)
library(rio)

# original dataset

data0 <- import(file = "data/ger_gdp_hicp.xlsx")

head(data0)

#------------------------------------------------------
# Forecast (GDP - nem görülő minta)
#------------------------------------------------------

# idősorokat egy mátrixba tesszük: minden új oszlopban egy negyedévvel hosszabb az idősor
# az első sor 2005Q4-ig, a második sok 2006Q1-ig tart, és így tovább...
ts_gdp <- matrix(NA,107,43)

# ezt valahogy úgy kellene definiálni, hogy az auto.arima outputok bele tudjanak menni (még nem tudom hogy kell):
# arima_gdp <- NA   # talán <- NULL?


for (i in 1:43)
{ 
  # a mátrix oszlopaiba kerülnek az idősorok
  ts_gdp[1:(64+i),(0+i)] <- ts(data0[1:(64+i),2], frequency = 4)
  
  # auto.arima az idősorokra
  arima_gdp[[i]]    <- auto.arima(ts_gdp[,i])
  
  # hibatag autokorrelációja - ezt majd meg kell nézni, hogy van-e 
  # DL tagra szükség
  # acf_gdp[i]        <- acf(resid(arima_gdp[[i]]))
 
  # forecast
  forecast_gdp[[i]] <- forecast(arima_gdp[[i]], h=4)[4]
}


