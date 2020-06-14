library(quantmod):
library(timeSeries)
library(tseries)
library(forecast)
library(xts)

#loading data
TME_STOCKPRICE = read.csv('~/Downloads/TME1.csv')
str(TME_STOCKPRICE)
head(TME_STOCKPRICE)
tail(TME_STOCKPRICE)
View(TME_STOCKPRICE)
length(TME_STOCKPRICE$Date)
length(TME_STOCKPRICE$Adj.Close)


## plot the adjusted stock price
library(ggplot2)
qplot(Date, Adj.Close,data = TME_STOCKPRICE)

# Create data frame with date and adj.close price 
TME_adj_prices  <- TME_STOCKPRICE[, "Adj.Close", drop = F]

# Create return and output it
n <- nrow(TME_adj_prices)
#TME_ret <- ((TME_adj_prices[2:n, 1]-TME_adj_prices[1:n-1, 1])
#            /TME_adj_prices[1:n-1, 1]) * 100
TME_ret <- diff(log(TME_STOCKPRICE$Adj.Close))

#check summary statistics
library(moments)
skewness(TME_ret)
kurtosis(TME_ret)

#plot return data and price data
#plot(TME_STOCKPRICE['Date'][2:n,1], TME_ret, type="o", col="blue", lwd=2)
plot(TME_STOCKPRICE$Adj.Close, type='l', ylab="TME_adjusted_close_price",col="blue",lwd=2)
plot(TME_ret, type="l", col="blue", lwd=2)
#rownames(TME_adj_prices) <-TME_adj_prices$Date
qplot(Date, Adj.Close,data = TME_STOCKPRICE)
qplot(Date, log(Adj.Close),data = TME_STOCKPRICE)

#plot ACF and PACF curves
acf(TME_STOCKPRICE$Adj.Close, lag.max = 24)
pacf(TME_STOCKPRICE$Adj.Close, lag.max = 24)
acf(TME_ret_train, lag.max = 24)
pacf(TME_ret_train, lag.max = 24)
#from inspection we should use AR model

### 2.AR Model ###
### test autocorrelation between lag return and return ###
#create 1 day lag return by AR model
TME_ret_train <- TME_ret[0:200]
TME_ret_test <- TME_ret[200:250] 
arimaModel_1 = arima(TME_ret_train, order = c(1, 0, 0))
#plot residuals and evaluate model
plot(arimaModel_1$residuals)
Box.test(arimaModel_1$residuals, lag = 1, type="Ljung-Box")
#predict from the model
prediction <- forecast::forecast(arimaModel_1, h=50)
plot(prediction, type='l', col='blue', lwd=2)
lines(ts(TME_ret_train))


###3 Week of the Day Effect###
TME_model2 <- arima(TME_ret_train, order = c(1, 0, 18), include.mean = FALSE, seasonal=list(order = c(1, 0, 18), period = 2))
prediction2 <- forecast::forecast(TME_model2, h=255-201)
plot(prediction2, type='l', col='blue', lwd=2)
lines(ts(TME_ret))

#check whether data shows seasonal effect
TME_timeseries <- ts(TME_STOCKPRICE$Adj.Close, frequency = 12)
TME_timeseriescomponents <- decompose(TME_timeseries)
plot(TME_timeseriescomponents)

#test model: goodness of fit
Box.test(TME_model2$residuals, lag = 5, type="Ljung-Box")

#test covariance between errors
summary(TME_model2)
arch.test(TME_model2, output=TRUE)

#test if variance of errors is cons
arch.test(TME_model2, output=TRUE)

#test covariance between error and return
Box.test(TME_model2$residuals, lag = 5, type="Ljung-Box")

#draw histogram for errors
hist(TME_model2$residuals)
shapiro.test(TME_model2$residuals) #p-value=0.3722 -> normal

#check multicolinearity 
library(aTSA)
ts.diag(TME_model2)

###For Spotify###
SPOT_STOCKPRICE = read.csv('~/Downloads/SPOT.csv')
#plot the adjusted stock price
plot(SPOT_STOCKPRICE$Adj.Close, type='l', ylab="TME_adjusted_close_price",col="blue",lwd=2)
#generate return 
SPOT_ret <- diff(log(SPOT_STOCKPRICE$Adj.Close))
#split the data
SPOT_ret_train <- SPOT_ret[0:350]
SPOT_ret_test <- SPOT_ret[350:433]
#model and predict
SPOT_fit <- arima(SPOT_ret_train, order = c(5, 1, 8), include.mean = FALSE, seasonal=list(order = c(5, 1, 8), period = 5))
SPOT_prediction <- forecast::forecast(SPOT_fit, h=433-351)
plot(SPOT_prediction, type='l', col='blue', lwd=2)
lines(ts(SPOT_ret))




