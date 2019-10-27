library(tidyverse)
library(xgboost)
library(caret)
library(magrittr)
library(Matrix)
library(geosphere)

train<-read.csv("C:/Users/nikhi/Desktop/Intermediate/fare.csv",header=TRUE,
                colClasses=c("key"="character","fare_amount"="numeric",
                             "pickup_datetime"="character",
                             "dropoff_longitude"="numeric",
                             "pickup_longitude"="numeric","dropoff_latitude"="numeric",
                             "pickup_latitude"="numeric",
                             "passenger_count"="integer"),nrows=3000000,
                stringsAsFactors = FALSE) %>% select(-key)

train  = train %>%
  mutate(pickup_datetime = as.POSIXct(pickup_datetime)) %>%
  mutate(hour = as.numeric(format(pickup_datetime, "%H"))) %>%
  mutate(min = as.numeric(format(pickup_datetime, "%M"))) %>%   
  mutate(year = as.factor(format(pickup_datetime, "%Y"))) %>%
  mutate(day = as.factor(format(pickup_datetime, "%d"))) %>%
  mutate(month = as.factor(format(pickup_datetime, "%m"))) %>%
  mutate(Wday = as.factor(weekdays(pickup_datetime))) %>%
  mutate(hour_class=as.factor(ifelse(hour<5,"Overnight",ifelse(hour<11,"Morning",ifelse(hour<16,"Noon",ifelse(hour<20,"Evening",ifelse(hour<23,"Night","Overnight"))))))) %>% 
  filter(pickup_longitude>-80 & pickup_longitude < -70) %>% 
  filter(pickup_latitude>35 & pickup_latitude < 45) %>% 
  filter(dropoff_longitude > -80 & dropoff_longitude < -70) %>%
  filter(dropoff_latitude > 35 & dropoff_latitude < 45) %>%
  filter(fare_amount > 2.5 & fare_amount <= 60) %>%
  filter(passenger_count > 0 & passenger_count < 10)


#jfk
jfk_lat<-40.6413
jfk_long<--73.7781
jfk<-c(jfk_long, jfk_lat)
#newark
nwk_lat<-40.6895
nwk_long<--74.1745
nwk<-c(nwk_long, nwk_lat)
#laguardia
lag_lat<-40.779
lag_long<--73.8740
lag<-c(lag_long, lag_lat)
#MSG
msg_lat<-40.7505
msg_long<--73.9934
msg<-c(msg_long, msg_lat)

#times square
ts_lat<-40.7589
ts_long<--73.9851
ts<-c(ts_long, ts_lat)
#freedom tower
freedom_lat<-40.7127
freedom_long<--74.0134
freedom<-c(freedom_long, freedom_lat)
#empire state building
esb_lat<-40.7484
esb_long<--73.9857
esb<-c(esb_long, esb_lat)
#grand central
grand_lat<-40.7527
grand_long<--73.9772
grand<-c(grand_long, grand_lat)

#bronx
bronx_lat <- (40.837048 * pi)/180
bronx_long <- (-73.865433 * pi)/180
bronx<-c(bronx_long, bronx_lat)
nyc<-c(-74.0063889, 40.7141667)

train =train %>% 
  mutate(
    dist = distHaversine(cbind(pickup_longitude, pickup_latitude), cbind(dropoff_longitude, dropoff_latitude), r = 6371),
    to_jfk = distHaversine(cbind(pickup_longitude, pickup_latitude), jfk, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), jfk, r = 6371),
    to_nkw = distHaversine(cbind(pickup_longitude, pickup_latitude), nwk, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), nwk, r = 6371),
    to_lag = distHaversine(cbind(pickup_longitude, pickup_latitude), lag, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), lag, r = 6371),
    to_msg = distHaversine(cbind(pickup_longitude, pickup_latitude), msg, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), msg, r = 6371),
    to_ts = distHaversine(cbind(pickup_longitude, pickup_latitude), ts, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), ts, r = 6371),
    to_freedom = distHaversine(cbind(pickup_longitude, pickup_latitude), freedom, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), freedom, r = 6371),
    #to_esb = distHaversine(cbind(pickup_longitude, pickup_latitude), esb, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), esb, r = 6371),
    to_grand = distHaversine(cbind(pickup_longitude, pickup_latitude), grand, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), grand, r = 6371),
    to_bronx = distHaversine(cbind(pickup_longitude, pickup_latitude), bronx, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), bronx, r = 6371),
    to_nyc = distHaversine(cbind(pickup_longitude, pickup_latitude), nyc, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), nyc, r = 6371)
  )

index=createDataPartition(train$year,p=0.6,list=FALSE)
dtrain=train[index,]
dtest=train[-index,]

index=createDataPartition(dtest$year,p=0.6,list=FALSE)
dtest=dtest[index,]
testacc=dtest[-index,]
label1 <- testacc[,2]-1

to_rad=function(df)
{
  df$pickup_longitude=df$pickup_longitude*pi/180
  df$pickup_latitude=df$pickup_latitude*pi/180
  df$dropoff_longitude=df$dropoff_longitude*pi/180
  df$dropoff_latitude=df$dropoff_latitude*pi/180
  return(df)
}
dtrain=to_rad(dtrain)
dtest=to_rad(dtest)
testacc = to_rad(testacc)

dtrain_matrix=xgb.DMatrix(data=data.matrix(dtrain[,-1]),label=dtrain$fare_amount)
dtest_matrix=xgb.DMatrix(data=data.matrix(dtest[,-1]),label=dtest$fare_amount)
acctest<-xgb.DMatrix(data = data.matrix(testacc[,-1]))

p = list(objective = "reg:linear",eval_metric = "rmse",max_depth = 6 ,eta = .05,subsample=1,colsample_bytree=0.8,num_boost_round=1000,nrounds = 3000)
set.seed(0)
m_xgb <- xgb.train(p,dtrain_matrix,p$nrounds,list(val = dtest_matrix),print_every_n = 1,early_stopping_rounds = 10)

pred <- predict(m_xgb, acctest)
pred=na.omit(pred)
label1 = na.omit(label1)


R<- sqrt(mean((pred - label1)^2))
R
