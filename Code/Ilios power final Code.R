### Install the required libraries
library("forecast")
library("stats")
library("data.table")
library("TTR")
library("lubridate")
library("ggplot2")
library("prophet")
library("gam")
library('quantmod')
library('dygraphs')
library('xts')          # To make the convertion data-frame / xts format
library('tidyverse')
library('DMwR')


# Read the required data from the file.
folder = "E:/ilios powere/"      # path to folder that holds multiple .csv files
file_list = list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder

# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  assign(file_list[i], read.csv(paste(folder, file_list[i], sep=''))
  )}

rm(folder,file_list)

# Convert the date column datatype to date format
DF5.csv$Date=as.Date(DF5.csv$Date,format="%d-%m-%Y")
DF10.csv$Date=as.Date(DF10.csv$Date,format="%d-%m-%Y")
DF15.csv$Date=as.Date(DF15.csv$Date,format="%d-%m-%Y")
DF30.csv$Date=as.Date(DF30.csv$Date,format="%d-%m-%Y")
DF60.csv$Date=as.Date(DF60.csv$Date,format="%d-%m-%Y")

# Index the date and time
DF5.csv$index=as.POSIXct(paste(DF5.csv$Date, DF5.csv$Time), format="%Y-%m-%d %H:%M:%S")
DF10.csv$index=as.POSIXct(paste(DF10.csv$Date, DF10.csv$Time), format="%Y-%m-%d %H:%M:%S")
DF15.csv$index=as.POSIXct(paste(DF15.csv$Date, DF15.csv$Time), format="%Y-%m-%d %H:%M:%S")
DF30.csv$index=as.POSIXct(paste(DF30.csv$Date, DF30.csv$Time), format="%Y-%m-%d %H:%M:%S")
DF60.csv$index=as.POSIXct(paste(DF60.csv$Date, DF60.csv$Time), format="%Y-%m-%d %H:%M:%S")


# Then you can create the xts format, and thus use dygraph for the distribution o data
df5=xts(x = DF5.csv$UnitsConsumed, order.by = DF5.csv$index)
dygraph(df5) %>%
  dyRoller(rollPeriod = 1)

df10=xts(x = DF10.csv$UnitsConsumed, order.by = DF10.csv$index)
dygraph(df10) %>%
  dyRoller(rollPeriod = 1)

df15=xts(x = DF15.csv$UnitsConsumed, order.by = DF15.csv$index)
dygraph(df15) %>%
  dyRoller(rollPeriod = 1)

df30=xts(x = DF30.csv$UnitsConsumed, order.by = DF30.csv$index)
dygraph(df30) %>%
  dyRoller(rollPeriod = 1)

df60=xts(x = DF60.csv$UnitsConsumed, order.by = DF60.csv$index)
dygraph(df60) %>%
  dyRoller(rollPeriod = 1) 


rm(df5,df10,df15,df30,df60)

#### There are few datapoints which are not in the distribution range
DF5.csv$UnitsConsumed=ifelse(DF5.csv$UnitsConsumed>30,0.2,DF5.csv$UnitsConsumed)
DF10.csv$UnitsConsumed=ifelse(DF10.csv$UnitsConsumed>20,0.2,DF10.csv$UnitsConsumed)
DF15.csv$UnitsConsumed=ifelse(DF15.csv$UnitsConsumed>20,0.2,DF15.csv$UnitsConsumed)
DF30.csv$UnitsConsumed=ifelse(DF30.csv$UnitsConsumed>10,0.2,DF30.csv$UnitsConsumed)
DF60.csv$UnitsConsumed=ifelse(DF60.csv$UnitsConsumed>7,0.3,DF60.csv$UnitsConsumed)

##### Bpx plot distribution #########
dev.off()
ggplot(DF5.csv,aes(x = Time,y = UnitsConsumed))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90))
ggplot(DF10.csv,aes(x = Time,y = UnitsConsumed))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90))
ggplot(DF15.csv,aes(x = Time,y = UnitsConsumed))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90))
ggplot(DF30.csv,aes(x = Time,y = UnitsConsumed))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90))
ggplot(DF60.csv,aes(x = Time,y = UnitsConsumed))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90))

### sPLIT THE DATA INTO TRAIN AND TEST DATASETS
train_5=DF5.csv[1:3542,]
test_5=DF5.csv[3543:4620,]

train_10=DF10.csv[1:1771,]
test_10=DF10.csv[1772:2310,]

train_15=DF15.csv[1:1173,]
test_15=DF15.csv[1174:1530,]

train_30=DF30.csv[1:575,]
test_30=DF30.csv[576:750,]

train_60=DF60.csv[1:276,]
test_60=DF60.csv[277:360,]

###################################################
######## stlf #####################
# Holtwinters cant handle the frequency more than 24 frequency

## 5 minutes

## Time serie
time_series_5 <- ts(train_5$UnitsConsumed,frequency = 154)

## Fit the model
fit_5 <- stlf(time_series_5,robust = T,lambda = T,s.window = "periodic",t.window = 5,h =1078)
summary(fit_5)

## predict for time zone
pred_5=forecast(fit_5,h=1078)

plot(pred_5)
lines(fitted(pred_5), col="red", lty=2)
lines(pred_5$mean, type="o", col="red")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data_5","STLF_5"))

pred_5=data.frame(pred_5)
pred_5$Point.Forecast=round(pred_5$Point.Forecast,digits = 1)

## Error
error_stlf_5=data.frame(regr.eval(test_5$UnitsConsumed,pred_5$Point.Forecast))

########## 10 Minutes ######################3
time_series_10 <- ts(train_10$UnitsConsumed,frequency = 77)

fit_10 <- stlf(time_series_10,robust = T,lambda =T,s.window = "periodic",t.window = 10,h =539 )
summary(fit_10)

pred_10=forecast(fit_10,h=539)

plot(pred_10)
lines(fitted(pred_10), col="red", lty=2)
lines(pred_10$mean, type="o", col="red")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data_10","STLF_10"))

pred_10=data.frame(pred_10)
pred_10$Point.Forecast=round(pred_10$Point.Forecast,digits = 1)

error_stlf_10=data.frame(regr.eval(test_10$UnitsConsumed,pred_10$Point.Forecast))

################### 15 Minutes ##########################33
time_series_15 <- ts(train_15$UnitsConsumed,frequency = 51)

fit_15 <- stlf(time_series_15,robust = T,lambda =T,s.window = "periodic",t.window = 10,h = 357)
summary(fit_15)

pred_15=forecast(fit_15,h=357)

plot(pred_15)
lines(fitted(pred_15), col="red", lty=2)
lines(pred_15$mean, type="o", col="red")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data_15","STLF_15"))

pred_15=data.frame(pred_15)
pred_15$Point.Forecast=round(pred_15$Point.Forecast,digits = 1)

error_stlf_15=data.frame(regr.eval(test_15$UnitsConsumed,pred_15$Point.Forecast))

##########################3 30 Minutes #####################
time_series_30 <- ts(train_30$UnitsConsumed,frequency = 25)

fit_30 <- stlf(time_series_30,robust = T,lambda =T,s.window = "periodic",t.window = 10,h =175)
summary(fit_30)

pred_30=forecast(fit_30,h=175)

plot(pred_30)
lines(fitted(pred_30), col="red", lty=2)
lines(pred_30$mean, type="o", col="red")
legend("topleft",lty=1, pch=1, col=1:2, 
       c("data_30","STLF_30"))

pred_30=data.frame(pred_30)
pred_30$Point.Forecast=round(pred_30$Point.Forecast,digits = 1)

error_stlf_30=data.frame(regr.eval(test_30$UnitsConsumed,pred_30$Point.Forecast))

################## 60 Minutes ############################
time_series_60 <- ts(train_60$UnitsConsumed,frequency = 12)
acf(time_series_60)
pacf(time_series_60)

fit_60 <- stlf(time_series_60,robust = T,lambda =T,s.window = "periodic",t.window = 15,h = 84)
summary(fit_60)

pred_60=forecast(fit_60,h=84)

plot(pred_60)
lines(fitted(pred_60), col="red", lty=2)
lines(pred_60$mean, type="o", col="red")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data_60","STLF_60"))

pred_60=data.frame(pred_60)
pred_60$Point.Forecast=round(pred_60$Point.Forecast,digits = 1)

error_stlf_60=data.frame(regr.eval(test_60$UnitsConsumed,pred_60$Point.Forecast))

#############################################################################################
########## Manual Arima and Auto Arima####################

################### 5 minutes #################################3
fit_5_arima=arima(time_series_5,order = c(1,0,1),seasonal = c(0,1,0))
summary(fit_5_arima)


pred_5_arima=forecast(fit_5_arima,h=1078)
plot(pred_5_arima)

lines(fitted(pred_5_arima), col="red", lty=2)
lines(pred_5_arima$mean, type="o", col="red")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data_5","Arima_5"))

preds_5_arima=data.frame(pred_5_arima)
preds_5_arima$Point.Forecast=round(preds_5_arima$Point.Forecast,digits = 1)

regr.eval(test_5$UnitsConsumed,preds_5_arima$Point.Forecast)

################### 10 minutes #################################3
fit_10_arima=arima(time_series_10,order = c(1,0,1),seasonal = c(0,1,0))
summary(fit_10_arima)


pred_10_arima=forecast(fit_10_arima,h=539)
plot(pred_10_arima)

lines(fitted(pred_10_arima), col="red", lty=2)
lines(pred_10_arima$mean, type="o", col="red")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data_10","Arima_10"))

preds_10_arima=data.frame(pred_10_arima)
preds_10_arima$Point.Forecast=round(preds_10_arima$Point.Forecast,digits = 1)

regr.eval(test_10$UnitsConsumed,preds_10_arima$Point.Forecast)
################### 15 minutes #################################3
fit_15_arima=arima(time_series_15,order = c(1,0,1),seasonal = c(0,1,0))
summary(fit_15_arima)


pred_15_arima=forecast(fit_15_arima,h=357)
plot(pred_15_arima)

lines(fitted(pred_15_arima), col="red", lty=2)
lines(pred_15_arima$mean, type="o", col="red")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data_15","Arima_15"))

preds_15_arima=data.frame(pred_15_arima)
preds_15_arima$Point.Forecast=round(preds_15_arima$Point.Forecast,digits = 1)

regr.eval(test_15$UnitsConsumed,preds_15_arima$Point.Forecast)

################### 30 minutes #################################3
fit_30_arima=auto.arima(time_series_30,D = 1)
summary(fit_30_arima)


pred_30_arima=forecast(fit_30_arima,h=175)
plot(pred_30_arima)

lines(fitted(pred_30_arima), col="red", lty=2)
lines(pred_30_arima$mean, type="o", col="red")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data_30","Arima_30"))


preds_30_arima=data.frame(pred_30_arima)
preds_30_arima$Point.Forecast=ifelse(preds_30_arima$Point.Forecast<0,0,preds_30_arima$Point.Forecast)
preds_30_arima$Point.Forecast=round(preds_30_arima$Point.Forecast,digits = 1)

regr.eval(test_30$UnitsConsumed,preds_30_arima$Point.Forecast)
################### 60 minutes #################################3
fit_60_arima=auto.arima(time_series_60,D = 1)
summary(fit_60_arima)


pred_60_arima=forecast(fit_60_arima,h=84)
plot(pred_60_arima)

lines(fitted(pred_60_arima), col="red", lty=2)
lines(pred_60_arima$mean, type="o", col="red")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data_60","Arima_60"))

preds_60_arima=data.frame(pred_60_arima)
preds_60_arima$Point.Forecast=ifelse(preds_60_arima$Point.Forecast<0,0,preds_60_arima$Point.Forecast)
preds_60_arima$Point.Forecast=round(preds_60_arima$Point.Forecast,digits = 1)

regr.eval(test_60$UnitsConsumed,preds_60_arima$Point.Forecast)

#########################################################

#################################### Neural nets ################
set.seed(132)
fit_nnet_60 <- nnetar(time_series_60,p = 13,size = 5,P = 5,repeats = 5)
plot(forecast(fit_nnet_60,h=84))

pred_60_nnet=forecast(fit_nnet_60,h=84)
plot(pred_60_nnet)

pred_60_nnet=data.frame(pred_60_nnet$mean)
pred_60_nnet$pred_60_nnet.mean=round(pred_60_nnet$pred_60_nnet.mean,digits = 1)

regr.eval(test_60$UnitsConsumed,pred_60_nnet$pred_60_nnet.mean)


###### 30
set.seed(124)
fit_nnet_30 <- nnetar(time_series_30,repeats = 5,p = 10,P = 5)
plot(forecast(fit_nnet_30,h=175))

pred_30_nnet=forecast(fit_nnet_30,h=175)
plot(pred_30_nnet)

pred_30_nnet=data.frame(pred_30_nnet$mean)
pred_30_nnet$pred_30_nnet.mean=round(pred_30_nnet$pred_30_nnet.mean,digits = 1)

regr.eval(test_30$UnitsConsumed,pred_30_nnet$pred_30_nnet.mean)

##### 15
set.seed(12345)
fit_nnet_15 <- nnetar(time_series_15,repeats = 5,p = 24,P =5)
plot(forecast(fit_nnet_15,h=357))

pred_15_nnet=forecast(fit_nnet_15,h=357)
plot(pred_15_nnet)

pred_15_nnet=data.frame(pred_15_nnet$mean)
pred_15_nnet$pred_15_nnet.mean=round(pred_15_nnet$pred_15_nnet.mean,digits = 1)

regr.eval(test_15$UnitsConsumed,pred_15_nnet$pred_15_nnet.mean)

#################
set.seed(123456)
fit_nnet_10 <- nnetar(time_series_10,p = 24,P = 10,size = 10)
plot(forecast(fit_nnet_10,h=539))

pred_10_nnet=forecast(fit_nnet_10,h=539)
plot(pred_10_nnet)

pred_10_nnet=data.frame(pred_10_nnet$mean)
pred_10_nnet$pred_10_nnet.mean=round(pred_10_nnet$pred_10_nnet.mean,digits = 1)

regr.eval(test_10$UnitsConsumed,pred_10_nnet$pred_10_nnet.mean)
########################
set.seed(1111)
fit_nnet_5 <- nnetar(time_series_5,p = 24,P = 10,size = 10)
plot(forecast(fit_nnet_5,h=1078))

pred_5_nnet=forecast(fit_nnet_5,h=1078)
plot(pred_5_nnet)

pred_5_nnet=data.frame(pred_5_nnet$mean)
pred_5_nnet$pred_5_nnet.mean=round(pred_5_nnet$pred_5_nnet.mean,digits = 1)

regr.eval(test_5$UnitsConsumed,pred_5_nnet$pred_5_nnet.mean)
