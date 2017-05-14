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
# install.packages("mFilter")
# install.packages("quantmod")
library(mFilter)
library(quantmod)
library(xlsx)
library(forecast)
library(tseries)
library(rio)

# original dataset

data0 <- import(file = "data/ger_gdp_hicp.xlsx")

head(data0)

#########################################################
##########            Zoli próbálkozik        ###########
#########################################################

gdp <- ts(data0[1:108,2], frequency = 4)

plot.ts(gdp)

lgdp <- log(gdp)

plot.ts(lgdp)

hpgdp <- hpfilter(lgdp, freq = 1600)
plot.ts(hpgdp$cycle)

#------------------------------------------------------
# Forecast (GDP - rekurzív minta)
#------------------------------------------------------

# idősorokat egy mátrixba tesszük: minden új oszlopban egy negyedévvel hosszabb az idősor
# az első sor 2005Q4-ig, a második sok 2006Q1-ig tart, és így tovább...
ts_gdp <- matrix(NA,107,43)

# memória pre-allokálás 
 arima_gdp    <- c()
 forecast_gdp <- c()

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
  forecast_gdp[[i]] <- forecast(arima_gdp[[i]], h=4)$mean[1:4]
}

<<<<<<< HEAD
# adott időszaki előrejelzés kiíratása:
 forecast_gdp[[43]]$mean[1:4]
 
 
 
 #########################################################
 ##########            Zoli próbálkozik        ###########
 #########################################################
 
gdp <- ts(data0[,2], frequency = 4)
           
plot.ts(gdp)
           
lgdp <- log(gdp)

# install.packages("mFilter")
# install.packages("quantmod")
library(mFilter)
library(quantmod)
           
hpgdp <- hpfilter(lgdp, freq = 1600)
plot.ts(hpgdp$cycle)

# az első sor 2005Q4-ig, a második sok 2006Q1-ig tart, és így tovább...
ts_gdp <- matrix(NA,107,43)

# ezt valahogy úgy kellene definiálni, hogy az auto.arima outputok bele tudjanak menni (még nem tudom hogy kell):
arima_gdp <- c()   # talán <- NULL?
forecast_gdp <- c()

for (i in 1:43)
{ 
  # a mátrix oszlopaiba kerülnek az idősorok
  ts_gdp[1:(64+i),(0+i)] <- ts(hpgdp$cycle[1:(64+i)], frequency = 4)
  
  # auto.arima az idősorokra
  arima_gdp[[i]]    <- auto.arima(ts_gdp[,i])
  
  # hibatag autokorrelációja - ezt majd meg kell nézni, hogy van-e 
  # DL tagra szükség
  # acf_gdp[i]        <- acf(resid(arima_gdp[[i]]))
  
  # forecast
  forecast_gdp[[i]] <- forecast(arima_gdp[[i]], h=4)[4]
}
arima_gdp
forecast_gdp

=======
#------------------------------------------------------
# cross correlation function (CCF)
#------------------------------------------------------
 
 # stacionaritási-teszt
 
 gdp <- ts(na.omit(data0$gdp), frequency = 4)
 adf.test(gdp)          # OK
 
 gdp_chain <- ts(na.omit(data0$gdp_chain), frequency = 4)
 adf.test(gdp_chain)          # OK
  
 industrial <- ts(na.omit(data0$industrial), frequency = 4)
 adf.test(industrial)       # nem OK
 adf.test(diff(industrial)) # ez már OK
 d_industrial <- diff(industrial)
 
 retail <- ts(na.omit(data0$retail ), frequency = 4)
 adf.test(retail )       # nem OK
 adf.test(diff(retail )) # ez már OK
 d_retail  <- diff(retail )
 
 hicp <- ts(na.omit(data0$hicp ), frequency = 4)
 adf.test(hicp)             # nem OK
 adf.test(diff(hicp ))      # nem OK
 adf.test(diff(diff(hicp))) # OK
 dd_hicp  <- diff(diff(hicp ))
 
 ifo_climate <- ts(na.omit(data0$`IFO-CES Business Climate` ), frequency = 4)
 adf.test(ifo_climate)      # OK
 
 ifo_exp <- ts(na.omit(data0$`IFO-CES Business Expectations`), frequency = 4)
 adf.test(ifo_exp)          # OK
 
 # CCF
 
 ccf(diff(gdp),gdp_chain)
 ccf(diff(gdp),d_industrial)# kapcsolat
 ccf(diff(gdp),diff(hicp))  # kapcsolat
 ccf(diff(gdp),retail)      # kapcsolat
 ccf(diff(gdp),ifo_climate) # kapcsolat
 ccf(gdp,ifo_exp)           # kapcsolat
 
 

 

 
>>>>>>> 9a9554ba00eec49a738f283c6961b6bd38513a04
