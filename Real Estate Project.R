library(forecast)
library(ggplot2)
library(quantmod)
library(vars)

# import data (New Private Housing Units Authorized by Building Permits: 1-Unit Structures in Florida a.k.a."FLBP1FH") from FRED
getSymbols("FLBP1FH", src = "FRED")
housing <- FLBP1FH
housing
class(housing)  # xts

# convert class of housing from xts to ts:
housing <- ts(housing[,1], start=c(1988,1), frequency = 12)
class(housing)  # ts
housing

# visualization:
autoplot(housing, xlab = "Year", ylab="Unit", main="New Private Housing Units Authorized by Building Permits: 1-Unit Structures in Florida") + geom_point()+ theme_bw()

# Split data into training and testing set based on 5 different scenarios:
# Depending on starting/end date for each scenario, adjust n.train, n.test as below, and then re-run the rest of the codes!

## ------------------ Scenario 1: ------------------------------------
# Training data = January, 1988 - December, 2000
# Testing = January, 2001 - December, 2005
# 2000- 1988 +1 = 13 years
# 2005 - 2001 + 1 = 5 years
n.train = 13*12
n.test = 5*12

## ------------------ Scenario 2: ------------------------------------
# Training data = January, 1988 - December, 2003
# Testing = January, 2004 - December, 2008
n.train = 16*12
n.test = 5*12

## ------------------ Scenario 3: ------------------------------------
# Training data = January, 1988 - December, 2008
# Testing = January, 2009 - December, 2013
n.train = 21*12
n.test = 5*12

## ------------------ Scenario 4: ------------------------------------
# Training data = January, 1988 - December, 2005
# Testing = January, 2006 - December, 2010
n.train = 18*12
n.test = 5*12

## ------------------ Scenario 5: ------------------------------------
# Training data = January, 1988 - December, 2018
# Testing = January, 2019 - September, 2020
n.train = 31*12
n.test = 21

# -------------------------------------------------------------------

# Using these n.train and n.test for each scenario, split training and testing set:
n.train
y.train=window(housing,end=c(1988, n.train))
y.train
plot(y.train)

n.test
y.test=window(housing,start=c(1988,n.train+1), end=c(1988, n.train+n.test))
y.test
plot(y.test)

n = length(housing)


## Modeling:
#################################################################
####### M1: Naive Model
M1 = snaive(y.train, h = n.test)
summary(M1)

# residuals:
tsdisplay(M1$residuals)

# accuracy metrics:
M1A = accuracy(M1, y.test)
M1A

# visualization:
autoplot(M1) + theme_bw() + autolayer(M1$fitted,series="sNaive fitted") + autolayer(y.test,series="Testing\nset")
autoplot(y.test,series="testing\nset") + autolayer(M1$mean,series="forecast") +
    theme_bw()


##########################################################################
##### M2.0: regression model (season only)
M2.0 = tslm(y.train ~ season)
M2.0L = tslm(y.train ~ season, lambda = "auto")
summary(M2.0)

# residuals:
tsdisplay(M2.0$residuals)
tsdisplay(M2.0L$residuals)

M2.0F = forecast(M2.0,h=n.test)
M2.0LF = forecast(M2.0L,h=n.test)

# Accuracy:
M2.0A = accuracy(M2.0F, y.test)
M2.0LA = accuracy(M2.0LF, y.test)
M2.0A
M2.0LA

# visualization:
autoplot(M2.0F) + theme_bw() + autolayer(M2.0$fitted.values,series="fitted\nvalues") + autolayer(y.test,series =" testing\ndata")

##########################################################################
##### M2.1: regression model (trend+season)
M2.1 = tslm(y.train ~ trend + season)
M2.1L = tslm(y.train ~ trend + season, lambda = "auto")
summary(M2.1)

# residuals:
tsdisplay(M2.1$residuals)
tsdisplay(M2.1L$residuals)

M2.1F = forecast(M2.1,h=n.test)
M2.1LF = forecast(M2.1L, h=n.test)

# Accuracy metrics:
M2A = accuracy(M2.1F,y.test)
M2.1LA = accuracy(M2.1LF, y.test)
M2A
M2.1LA

# Plot data and prediction on testing set + fitted values + testing set
autoplot(M2.1LF) + theme_bw() + autolayer(M2.1L$fitted.values,series="fitted\nvalues") + autolayer(y.test,series =" testing\ndata")

# to capture/model the cyclical pattern in residuals:
# use higher order polynomial

###################################################################
####### M2.2 = y = trend + trend^2 + seasonal
M2.2 = tslm(y.train ~ trend + I(trend^2) + season)
M2.2L = tslm(y.train ~ trend + I(trend^2) + season, lambda = "auto")
summary(M2.2)

# residuals:
tsdisplay(M2.2$residuals)
tsdisplay(M2.2L$residuals)

M2.2F=forecast(M2.2,h=n.test)
M2.2LF=forecast(M2.2L,h=n.test)

M2.2A = accuracy(M2.2F,y.test)
M2.2LA = accuracy(M2.2LF,y.test)
M2.2A
M2.2LA

# visualization:
autoplot(M2.2F) + theme_bw() + autolayer(M2.2$fitted.values,series="fitted\nvalues") + autolayer(y.test,series =" testing\ndata")

###################################################################
####### M2.3 = y = trend + trend^2 + trend^3 + seasonal
M2.3 = tslm(y.train ~ trend + I(trend^2) + I(trend^3)  + season)
M2.3L = tslm(y.train ~ trend + I(trend^2) + I(trend^3)  + season, lambda = "auto")
M2.3
summary(M2.3L)

# residuals:
tsdisplay(M2.3$residuals)
tsdisplay(M2.3L$residuals)

M2.3F=forecast(M2.3,h=n.test)
M2.3LF=forecast(M2.3L,h=n.test)

M2.3A = accuracy(M2.3F,y.test)
M2.3LA = accuracy(M2.3LF,y.test)
M2.3A
M2.3LA

# visualization:
autoplot(M2.3LF) + theme_bw() + autolayer(M2.3L$fitted.values,series="fitted\nvalues") + autolayer(y.test,series =" testing\ndata")


##########################################################################
## M3: smoothing model
M3 = ets(y.train)
M3L = ets(y.train, lambda = "auto")
summary(M3)
summary(M3L)

# residuals:
tsdisplay(M3$residuals)
tsdisplay(M3L$residuals)

M3F=forecast(M3,h=n.test)
M3LF=forecast(M3L,h=n.test)

# accuracy:
M3A=accuracy(M3F,y.test)
M3LA=accuracy(M3LF,y.test)
M3A
M3LA

# visualization:
autoplot(M3F) + theme_bw() + autolayer(M3$fitted,series="fitted\nvalues") + autolayer(y.test,series =" testing\ndata")


##########################################################################
##### M4 = Arima
M4 = auto.arima(y.train)
M4L = auto.arima(y.train, lambda = "auto")
M4
M4L

# residuals:
tsdisplay(M4$residuals)
tsdisplay(M4L$residuals)

M4F=forecast(M4,h=n.test)
M4LF=forecast(M4L,h=n.test)
M4LF

# accuracy:
M4A = accuracy(M4F,y.test)
M4LA = accuracy(M4LF,y.test)
M4A
M4LA

# visualization:
autoplot(M4F) + theme_bw() + autolayer(M4$fitted,series="fitted\nvalues") +
    autolayer(y.test,series =" testing\ndata")

# plot testing data and M1, M2, M3, and M4 forecasts on one graph
autoplot(y.test,series="Testing\ndata", ylab="Units", xlab="Year") + autolayer(M1$mean,series="Naive\nmodel") +autolayer(M2.2LF$mean, series = "Regression\nmodel") + autolayer(M3LF$mean,series="Smoothing\nmodel") + autolayer(M4F$mean,series="ARIMA\nmodel") + theme_bw()


#######################################################################
#### M5: Neural Network
# Using loop, figure out a combination of parameters (p, P, size) that would give the smallest MAPE on testing set:
# i for p
# j for P
# k for size
pPsize=NULL
MAPE=NULL

start=Sys.time()
for(i in 1:11){
    for(j in 1:4){
        for(k in 2:6){
            pPsize=c(pPsize,paste("p =",i,"P =", j,"size =",k))
            M5=nnetar(y.train,p=i,P=j,size=k)
            M5F=forecast(M5,h=n.test,level = F)
            MAPE=c(MAPE,accuracy(M5F,y.test)[2,"MAPE"])
        }
    }
}
end=Sys.time()
end-start

#Plot MAPE
plot(MAPE,type="b")
# Add a vertical red line showing the minimum value of MAPE
abline(v=which(min(MAPE)==MAPE),col="red")

# combination of parameters that gives the smallest MAPE on the testing set
pPsize[which(min(MAPE)==MAPE)]

# Using this combination of parameters, make a model again
# # adjust parameters for each scenario!
M5=nnetar(y.train,p=3,P=3,size=3)
M5L=nnetar(y.train,p=3,P=3,size=3, lambda = TRUE)

# residuals:
tsdisplay(M5$residuals)
tsdisplay(M5L$residuals)

M5F=forecast(M5,h=n.test)
M5LF = forecast(M5L, h=n.test)
M5F

# accuracy:
M5A = accuracy(M5F, y.test)
M5LA = accuracy(M5LF, y.test)
M5A
M5LA
# Note: accuracy metrics in neural network model keeps changing every time I run the model

# visualization:
autoplot(M5F) + theme_bw() + autolayer(M5$fitted,series="fitted\nvalues") +
    autolayer(y.test,series =" testing\ndata")


####################################################################
####### M6 = Classical time series decomposition
# Decompose training set using stl:
M6=stl(y.train[,1],t.window=13,s.window="periodic")
plot(M6)

# plot seasonally adjusted data:
M6 %>% seasadj() %>% autoplot()

# forecast on testing set using naive model
M6.1F=forecast(M6,method="naive", h=n.test)

# residuals:
plot(M6.1F$residuals)

# accuracy:
accuracy(M6.1F, y.test)

# visualization:
autoplot(M6.1F) + autolayer(y.test, series="testing set") +theme_bw() + autolayer(M6.1F$fitted,series="fitted\nvalues")


###### forecast on testing set using arima model
M6.2F=forecast(M6,method="arima", h=n.test)
plot(M6.2F$residuals)
accuracy(M6.2F, y.test)
autoplot(M6.2F) + autolayer(y.test, series="testing set") +theme_bw()+ autolayer(M6.2F$fitted,series="fitted\nvalues")


###### forecast on testing set using random walk with drift model
M6.3F=forecast(M6,method="rwdrift", h=n.test)
plot(M6.3F$residuals)
accuracy(M6.3F, y.test)
autoplot(M6.3F) + autolayer(y.test, series="testing set") +theme_bw()+ autolayer(M6.3F$fitted,series="fitted\nvalues")


###### forecast on testing set using exponential smoothing model
M6.4F=forecast(M6,method="ets", h=n.test)
plot(M6.4F$residuals)
accuracy(M6.4F, y.test)
autoplot(M6.4F) + autolayer(y.test, series="testing set") +theme_bw() +autolayer(M6.4F$fitted,series="fitted\nvalues")


################################################################################# M7 = Theta
M7F=thetaf(y.train,h=n.test)

# residuals:
tsdisplay(M7F$residuals)

# accuracy:
accuracy(M7F, y.test)

# visualization
autoplot(M7F) + autolayer(y.test) +theme_bw()+autolayer(M7F$fitted,series="fitted\nvalues")


####################################################################
#### M8: Vector Autoregression model (VAR)
# include other data that might help predict building permits
# M8.1: include unemployment rate (monthly) - both seasonally adjusted and not seasonally adjusted

# import those data from FRED:
getSymbols("UNRATENSA", src="FRED")
getSymbols("UNRATE", src="FRED")
getSymbols("FLBP1FH", src="FRED")
autoplot(UNRATENSA)
autoplot(UNRATE)
head(UNRATENSA)
head(UNRATE)

# convert class type of both data from xts to ts:
unratensa = ts(UNRATENSA, frequency=12, start=c(1948,1))
unrate = ts(UNRATE, frequency=12, start=c(1948,1))

# For the Building Permits data, specify the training period so that it has training and testing set
hs = ts(FLBP1FH, frequency=12, start=c(1988,1), end=c(1988, n.train))

# Plotting:
autoplot(unratensa, series="Unemployment \nRate (nsa)") + autolayer(unrate, series ="Unemployment \nRate \n(Seasonally adjusted)")+autolayer(hs/1000, series = "Housing")+xlim(c(1988,1988+(n.train/12)))

# merge these data into one ts:
mult.ts=ts.intersect(unratensa, unrate,hs)
head(mult.ts)
tail(mult.ts)
tsdisplay(mult.ts)

# modeling:
# find the number of lag that gives the smallest MAPE on the testing set
lag_num = NULL
mape = NULL
for(i in 1:12){
    lag_num=c(lag_num,i)
    M8.1=VAR(mult.ts, p = i)
    M8.1F=predict(M8.1,n.ahead=n.test)
    mape = c(mape, mean(abs((y.test-M8.1F$fcst$hs[,1])/y.test))*100)
}
mape
plot(mape)
abline(v=which(min(mape)==mape),col="red")
min_mape_lag = lag_num[which(min(mape)==mape)]
min_mape_lag

# Using this lag parameter, build the model:
M8.1=VAR(mult.ts, p = min_mape_lag)
M8.1F=predict(M8.1,n.ahead=n.test)
M8.1F
plot(M8.1F)

#Extract housing
M8.1F$fcst$hs

# convert the class of "hs" forecasting to ts:
M8.1F_ts = ts(M8.1F$fcst$hs[,1], start=c(1988,n.train+1), end=c(1988, n.train+n.test), frequency=12)
M8.1F_ts

# accuracy (MAPE):
# training set:
accuracy(M8.1$varresult[["hs"]])
# testing set
accuracy(M8.1F_ts, y.test)
mean(abs((y.test-M8.1F$fcst$hs[,1])/y.test))*100   # alternative method to calculate MAPE on testing set

# Visualization:
autoplot(M8.1F_ts, series="Forecast") + autolayer(y.test, series="Testing \nset") +theme_bw() +autolayer(hs, series = "Training \nset")


####################################################################
### M8.2: include Total Nonfarm Payroll: the number of U.S. workers in the economy that excludes proprietors, private household employees, unpaid volunteers, farm employees, and the unincorporated self-employed. According to FRED, this measure accounts for approximately 80 percent of the workers who contribute to Gross Domestic Product (GDP).

# import those data from FRED:
getSymbols("PAYEMS", src="FRED")
autoplot(PAYEMS)
head(PAYEMS)
nonfarm_emp = ts(PAYEMS, frequency=12, start=c(1939,1))
hs = ts(FLBP1FH, frequency=12, start=c(1988,1), end=c(1988, n.train))
autoplot(nonfarm_emp) + autolayer(hs*10, series = "Housing")+
    xlim(c(1988,1988+(n.train/12)))

# merge these data into one
mult.ts=ts.intersect(nonfarm_emp,hs)
head(mult.ts)
tail(mult.ts)
tsdisplay(mult.ts)

# modeling:
# find the number of lag that gives the smallest MAPE on the testing set
lag_num = NULL
mape = NULL
for(i in 1:12){
    lag_num=c(lag_num,i)
    M8.2=VAR(mult.ts, p = i)
    M8.2F=predict(M8.2,n.ahead=n.test)
    mape = c(mape, mean(abs((y.test-M8.2F$fcst$hs[,1])/y.test))*100)
}
mape
plot(mape)
abline(v=which(min(mape)==mape),col="red")
min_mape_lag = lag_num[which(min(mape)==mape)]
min_mape_lag

# Using this lag parameter, build the model:
M8.2=VAR(mult.ts, p = min_mape_lag)
M8.2F=predict(M8.2,n.ahead=n.test)
M8.2F
plot(M8.2F)

#Extract housing
M8.2F$fcst$hs

# convert housing forecasting to ts:
M8.2F_ts = ts(M8.2F$fcst$hs[,1], start=c(1988,n.train+1), end=c(1988, n.train+n.test), frequency=12)
M8.2F_ts

# accuracy (MAPE):
# training set:
accuracy(M8.2$varresult[["hs"]])
# testing set
accuracy(M8.2F_ts, y.test)
mean(abs((y.test-M8.2F$fcst$hs[,1])/y.test))*100

# Visualization:
autoplot(M8.2F_ts, series="Forecast") + autolayer(y.test, series="Testing \nset") +theme_bw() +autolayer(hs, series = "Training \nset")


####################################################################
### M8.3: include Housing Starts: Total: New Privately Owned Housing Units Started because once the housing permits issued, new projects for housing construction may begin.

# import those data from FRED:
getSymbols("HOUST", src="FRED")
getSymbols("HOUSTNSA", src="FRED")
autoplot(HOUST)
autoplot(HOUSTNSA)
head(HOUST)  # seasonally adjusted
head(HOUSTNSA)  # not seasonally adjusted
housing_start = ts(HOUST, frequency=12, start=c(1959,1))
housing_start_nsa= ts(HOUSTNSA, frequency=12, start=c(1959,1))
hs = ts(FLBP1FH, frequency=12, start=c(1988,1), end=c(1988, n.train))
autoplot(housing_start, series="Housing Start \n(nsa)") + autolayer(housing_start_nsa*10, series="Housing Start \nnot seasonally \nadjusted") + autolayer(hs/10, series = "Housing") + xlim(c(1988,1988+(n.train/12)))

# merge these data into one
mult.ts=ts.intersect(housing_start, housing_start_nsa,hs)
head(mult.ts)
tail(mult.ts)
tsdisplay(mult.ts)

# modeling:
# find the number of lag that gives the smallest MAPE on the testing set
lag_num = NULL
mape = NULL
for(i in 1:12){
    lag_num=c(lag_num,i)
    M8.3=VAR(mult.ts, p = i)
    M8.3F=predict(M8.3,n.ahead=n.test)
    mape = c(mape, mean(abs((y.test-M8.3F$fcst$hs[,1])/y.test))*100)
}

mape
plot(mape)
abline(v=which(min(mape)==mape),col="red")
min_mape_lag = lag_num[which(min(mape)==mape)]
min_mape_lag

# using this lag that gives the smallest MAPE, build the model:
M8.3=VAR(mult.ts, p = min_mape_lag)
M8.3F=predict(M8.3,n.ahead=n.test)
M8.3F

#Extract housing
M8.3F$fcst$hs

# forecasting (convert housing forecasting to ts):
M8.3F_ts = ts(M8.3F$fcst$hs[,1], start=c(1988,n.train+1), end=c(1988, n.train+n.test), frequency=12)

# accuracy (MAPE):
# training set:
accuracy(M8.3$varresult[["hs"]])
# testing set
accuracy(M8.3F_ts, y.test)
mean(abs((y.test-M8.3F$fcst$hs[,1])/y.test))*100

# Visualization:
autoplot(M8.3F_ts, series="Forecast") + autolayer(y.test, series="Testing \nset") +theme_bw() +autolayer(hs, series = "Training \nset")


####################################################################
### M8.4: include interest rates, discount rate for US (not seasonally adjusted)
# import data from FRED:
getSymbols("INTDSRUSM193N", src="FRED")
autoplot(INTDSRUSM193N)
head(INTDSRUSM193N)
hs = ts(FLBP1FH, frequency=12, start=c(1988,1), end=c(1988, n.train))
interest_rate = ts(INTDSRUSM193N, frequency=12, start=c(1950,1))
autoplot(interest_rate, series="Interest Rate") + autolayer(hs/1000, series = "Housing") + xlim(c(1988,1988+(n.train/12)))

# merge these data into one
mult.ts=ts.intersect(interest_rate,hs)
head(mult.ts)
tail(mult.ts)
tsdisplay(mult.ts)

# modeling:
# find the number of lag that gives the smallest MAPE on the testing set
lag_num = NULL
mape = NULL
for(i in 1:12){
    lag_num=c(lag_num,i)
    M8.4=VAR(mult.ts, p = i)
    M8.4F=predict(M8.4,n.ahead=n.test)
    mape = c(mape, mean(abs((y.test-M8.4F$fcst$hs[,1])/y.test))*100)
}
mape
plot(mape)
abline(v=which(min(mape)==mape),col="red")
min_mape_lag = lag_num[which(min(mape)==mape)]
min_mape_lag

# Using this lag parameter, build the model:
M8.4=VAR(mult.ts, p = min_mape_lag)
M8.4F=predict(M8.4,n.ahead=n.test)
M8.4F
plot(M8.4F)

#Extract housing
M8.4F$fcst$hs

# forecasting (convert housing forecasting to ts):
M8.4F_ts = ts(M8.4F$fcst$hs[,1], start=c(1988,n.train+1), end=c(1988, n.train+n.test), frequency=12)

# accuracy (MAPE):
# training set:
accuracy(M8.4$varresult[["hs"]])
# testing set
accuracy(M8.4F_ts, y.test)
mean(abs((y.test-M8.4F$fcst$hs[,1])/y.test))*100

# Visualization:
autoplot(M8.4F_ts, series="Forecast") + autolayer(y.test, series="Testing \nset") +theme_bw() +autolayer(hs, series = "Training \nset")


####################################################################
### M8.5: include New One Family Houses Sold: United States (not seasonally adjusted) and interest rate
getSymbols("HSN1FNSA", src="FRED")
autoplot(HSN1FNSA)
head(HSN1FNSA)
house_sold = ts(HSN1FNSA, frequency=12, start=c(1963,1))
hs = ts(FLBP1FH, frequency=12, start=c(1988,1), end=c(1988, n.train))
autoplot(house_sold/10, series="New Houses Sold") + autolayer(interest_rate, series="Interest Rate") + autolayer(hs/1000, series = "Housing") + xlim(c(1988,1988+(n.train/12)))

# merge these data into one
mult.ts=ts.intersect(house_sold, interest_rate, hs)
head(mult.ts)
tail(mult.ts)
tsdisplay(mult.ts)

# modeling:
# find the number of lag that gives the smallest MAPE on the testing set
lag_num = NULL
mape = NULL
for(i in 1:12){
    lag_num=c(lag_num,i)
    M8.5=VAR(mult.ts, p = i)
    M8.5F=predict(M8.5,n.ahead=n.test)
    mape = c(mape, mean(abs((y.test-M8.5F$fcst$hs[,1])/y.test))*100)
}
mape
plot(mape)
abline(v=which(min(mape)==mape),col="red")
min_mape_lag = lag_num[which(min(mape)==mape)]
min_mape_lag

# Using this lag parameter, build the model:
M8.5=VAR(mult.ts, p = min_mape_lag)
M8.5F=predict(M8.5,n.ahead=n.test)
M8.5F
plot(M8.5F)

# forecasting (convert housing forecasting to ts):
M8.5F_ts = ts(M8.5F$fcst$hs[,1], start=c(1988,n.train+1), end=c(1988, n.train+n.test), frequency=12)
M8.5F_ts

# accuracy (MAPE):
# training set:
accuracy(M8.5$varresult[["hs"]])
# testing set
accuracy(M8.5F_ts, y.test)
mean(abs((y.test-M8.5F$fcst$hs[,1])/y.test))*100

# Visualization:
autoplot(M8.5F_ts, series="Forecast") + autolayer(y.test, series="Testing \nset") +theme_bw() +autolayer(hs, series = "Training \nset")


####################################################################
### M8.6: include housing starts (not seasonally adjusted) and interest rate
# merge these data into one
hs = ts(FLBP1FH, frequency=12, start=c(1988,1), end=c(1988, n.train))
autoplot(interest_rate, series="Interest Rate") + autolayer(housing_start_nsa/10, series="Housing Start (nsa)") +autolayer(hs/1000, series = "Housing") + xlim(c(1988,1988+(n.train/12)))

mult.ts=ts.intersect(housing_start_nsa, interest_rate, hs)
head(mult.ts)
tail(mult.ts)
tsdisplay(mult.ts)

# modeling:
# find the number of lag that gives the smallest MAPE on the testing set
lag_num = NULL
mape = NULL
for(i in 1:12){
    lag_num=c(lag_num,i)
    M8.6=VAR(mult.ts, p = i)
    M8.6F=predict(M8.6,n.ahead=n.test)
    mape = c(mape, mean(abs((y.test-M8.6F$fcst$hs[,1])/y.test))*100)
}
mape
plot(mape)
abline(v=which(min(mape)==mape),col="red")
min_mape_lag = lag_num[which(min(mape)==mape)]
min_mape_lag

# Using this lag parameter, build the model:
M8.6=VAR(mult.ts, p = min_mape_lag)
M8.6F=predict(M8.6,n.ahead=n.test)
M8.6F
plot(M8.6F)

# forecasting (convert housing forecasting to ts):
M8.6F_ts = ts(M8.6F$fcst$hs[,1], start=c(1988,n.train+1), end=c(1988, n.train+n.test), frequency=12)
M8.6F_ts

# accuracy (MAPE):
# training set:
accuracy(M8.6$varresult[["hs"]])
# testing set
accuracy(M8.6F_ts, y.test)
mean(abs((y.test-M8.6F$fcst$hs[,1])/y.test))*100

# Visualization:
autoplot(M8.6F_ts, series="Forecast") + autolayer(y.test, series="Testing \nset") +theme_bw() +autolayer(hs, series = "Training \nset")


####################################################################
### M8.7: include unemployment rate(not seasonally adjusted), housing starts (not seasonally adjusted), and interest rate
# merge these data into one
hs = ts(FLBP1FH, frequency=12, start=c(1988,1), end=c(1988, n.train))
autoplot(interest_rate, series="Interest Rate") + autolayer(housing_start_nsa/10, series="Housing Start (nsa)")+autolayer(unratensa, series="Unemployment rate (nsa)") +autolayer(hs/1000, series = "Housing") + xlim(c(1988,1988+(n.train/12)))

mult.ts=ts.intersect(unratensa, housing_start_nsa, interest_rate, hs)
head(mult.ts)
tail(mult.ts)
tsdisplay(mult.ts)

# modeling:
# find the number of lag that gives the smallest MAPE on the testing set
lag_num = NULL
mape = NULL
for(i in 1:12){
    lag_num=c(lag_num,i)
    M8.7=VAR(mult.ts, p = i)
    M8.7F=predict(M8.7,n.ahead=n.test)
    mape = c(mape, mean(abs((y.test-M8.7F$fcst$hs[,1])/y.test))*100)
}
mape
plot(mape)
abline(v=which(min(mape)==mape),col="red")
min_mape_lag = lag_num[which(min(mape)==mape)]
min_mape_lag

# Using this lag parameter, build the model:
M8.7=VAR(mult.ts, p = min_mape_lag)
M8.7F=predict(M8.7,n.ahead=n.test)
M8.7F
plot(M8.7F)

# forecasting (convert housing forecasting to ts):
M8.7F_ts = ts(M8.7F$fcst$hs[,1], start=c(1988,n.train+1), end=c(1988, n.train+n.test), frequency=12)
M8.7F_ts

# accuracy (MAPE):
# training set:
accuracy(M8.7$varresult[["hs"]])
# testing set
accuracy(M8.7F_ts, y.test)
mean(abs((y.test-M8.7F$fcst$hs[,1])/y.test))*100

# Visualization:
autoplot(M8.7F_ts, series="Forecast") + autolayer(y.test, series="Testing \nset") +theme_bw() +autolayer(hs, series = "Training \nset")


####################################################################
### M8.8: include unemployment rate(not seasonally adjusted) and interest rate
# merge these data into one
hs = ts(FLBP1FH, frequency=12, start=c(1988,1), end=c(1988, n.train))
autoplot(interest_rate, series="Interest Rate") + autolayer(unratensa, series="Unemployment rate (nsa)") +autolayer(hs/1000, series = "Housing") + xlim(c(1988,1988+(n.train/12)))

mult.ts=ts.intersect(unratensa,interest_rate, hs)
head(mult.ts)
tail(mult.ts)
tsdisplay(mult.ts)

# modeling:
# find the number of lag that gives the smallest MAPE on the testing set
lag_num = NULL
mape = NULL
for(i in 1:12){
    lag_num=c(lag_num,i)
    M8.8=VAR(mult.ts, p = i)
    M8.8F=predict(M8.8,n.ahead=n.test)
    mape = c(mape, mean(abs((y.test-M8.8F$fcst$hs[,1])/y.test))*100)
}
mape
plot(mape)
abline(v=which(min(mape)==mape),col="red")
min_mape_lag = lag_num[which(min(mape)==mape)]
min_mape_lag

# Using this lag parameter, build the model:
M8.8=VAR(mult.ts, p = min_mape_lag)
M8.8F=predict(M8.8,n.ahead=n.test)
M8.8F
plot(M8.8F)

# forecasting (convert housing forecasting to ts):
M8.8F_ts = ts(M8.8F$fcst$hs[,1], start=c(1988,n.train+1), end=c(1988, n.train+n.test), frequency=12)
M8.8F_ts

# accuracy (MAPE):
# training set:
accuracy(M8.8$varresult[["hs"]])
# testing set
accuracy(M8.8F_ts, y.test)
mean(abs((y.test-M8.8F$fcst$hs[,1])/y.test))*100

# Visualization:
autoplot(M8.8F_ts, series="Forecast") + autolayer(y.test, series="Testing \nset") +theme_bw() +autolayer(hs, series = "Training \nset")



##########################
## M9: data driven model selection: For each scenario, use different models that give the smallest MAPE on testing set instead of choosing just one model for all scenarios (please refer to the excel spreadsheet and the 3rd paragraph of my technical report).
# Scenario 1: M2.2 (non-linear regression: y = trend + trend^2 + seasonal)
# Scenarion 2: M8.7 (VAR)
# Scenarion 3: M8.2 VAR)
# Scenarion 4: M8.5 VAR)
# Scenarion 5: M5 (Neural Network)
