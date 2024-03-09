# Ilay Shuel 314713777
#Dov Radoszkowicz 308415009
setwd("C:/Users/ilays/?????????? ????????????/study/year b/production/exercise")
a<-as.numeric(123)
str(a)
a<-as.character(123)
str(a)
a<-as.factor(123)
str(a)

b<-list(1,2,3)
str(b)
b1<-list(1,"2",3)
str(b1)
c<-data.frame(b)
str(c)
str(b1)

DATA<-read.csv(file="data_only.csv",stringsAsFactors= FALSE)
library(forecast)
DATA_TS<-ts(DATA)
plot(DATA)
plot(DATA_TS)
str(DATA_TS)

DATA_TS_End_Quarterly<- ts(DATA_TS,frequency = 4,end = 2022)
plot(DATA_TS_End_Quarterly) 

DATA_TS_End_Annual<- ts(DATA_TS, frequency = 1, end = 2022)
plot(DATA_TS_End_Annual)

Regression_DATA_TS<- tslm(DATA_TS ~ trend)
summary(Regression_DATA_TS)
plot(DATA_TS)
lines(Regression_DATA_TS$fitted)

accuracy(Regression_DATA_TS)

library(ggplot2)

autoplot(DATA_TS, series="Time series")+autolayer(ma(DATA_TS,3),series="3 - months")+autolayer(ma(DATA_TS,6),series="6 - months")+xlab("Time") +ylab("Demand")+ggtitle("Moving Average")+  theme(plot.title = element_text(hjust = 0.5))

Exponential_smoothing_0.1 <- ses(DATA_TS,h=1 , alpha = 0.1)
Exponential_smoothing_0.1$mean
Exponential_smoothing_0.5 <- ses(DATA_TS ,h=1, alpha = 0.5)
Exponential_smoothing_0.5$mean
Exponential_smoothing_0.9 <- ses(DATA_TS ,h=1, alpha = 0.9)
Exponential_smoothing_0.9$mean

Exponential_smoothing_optimal <- ses(DATA_TS,h=1)
Exponential_smoothing_optimal$model

autoplot(DATA_TS, series="Time series")+autolayer(ma(DATA_TS,3),series="3 - months")+autolayer(ma(DATA_TS,6),series="6 - months")+autolayer(ses(DATA_TS ,h=1),series="Exponential smoothing (Optimal)")+xlab("Time") +ylab("Demand")+ggtitle("Moving Average and Exponential Smoothing")+theme(plot.title = element_text(hjust = 0.5))
round(accuracy(Exponential_smoothing_optimal),3)

New_Data <- ts(DATA_TS, frequency = 12)
New_Data <- head(New_Data, -5)
pred_0.1 <- HoltWinters(New_Data, alpha = 0.1, beta = 0.1, gamma = 0.1)
forecast_0.1_result <- forecast(pred_0.1, h = 5)
print(forecast_0.1_result)
accuracy(forecast_0.1_result)

pred_0.9 <- HoltWinters(New_Data, alpha = 0.9, beta = 0.9, gamma = 0.9)
forecast_0.9_result <- forecast(pred_0.9, h = 5)
print(forecast_0.9_result)
accuracy(forecast_0.9_result)

pred_Optimal <- HoltWinters(New_Data)
forecast_Optimal_result <- forecast(pred_Optimal, h = 5)
print(forecast_Optimal_result)
accuracy(forecast_Optimal_result)

Decompose_Data <- ts(DATA_TS, frequency = 12)
decomposedTimeSeries <- decompose(Decompose_Data)
decomposedTimeSeries
plot(decomposedTimeSeries)

max_value <- max(Decompose_Data)
min_value <- min(Decompose_Data)

Noise<-decomposedTimeSeries$random
max_value_Noise <- max(Noise,na.rm = TRUE)
min_value_Noise <- min(Noise, na.rm = TRUE)

max_value_Noise
min_value_Noise


noise1<-c(Noise)
qqnorm(noise1); qqline(noise1, col = 2)
