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

#forecast
test <- ts(data0[1,  2:length(data0[1,])],start=c(1999,2),end = c(2017, 5),frequency = 12 )


#forecast
gazdpolts <- ts(data0[2:31,2])

b=auto.arima(ts(gazdpolts))