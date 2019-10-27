library(lubridate)
library(geosphere)
library(ggplot2)
library(caret)
library(magrittr)
library(Matrix)

taxi <- read.csv("C:/Users/nikhi/Desktop/Intermediate/fare.csv")
head(taxi)
attach(taxi)

#Removing na values
taxi=na.omit(taxi)
#data cleaning
taxi <- taxi %>%
  filter(between(pickup_latitude,-90,90),between(dropoff_latitude,-90,90), between(pickup_longitude,-180,180),between(dropoff_longitude,-180,180)) %>%
  filter(fare_amount > 0) %>%
  mutate(total = log(fare_amount))
head(taxi)

taxi <- taxi %>% separate(key,c("key","unique_Id"),sep="\\.")
taxi$key <- NULL


#check for missing value in train data
apply(train, 2, function(x){sum(is.na(x))})
#summary
summary(train)
#drop the coordinates that are ouside a range
train<-train%>%  
  filter(pickup_longitude > -80 & pickup_longitude < -70) %>%
  filter(pickup_latitude > 35 & pickup_latitude < 45) %>%
  filter(dropoff_longitude > -80 & dropoff_longitude < -70) %>%
  filter(dropoff_latitude > 35 & dropoff_latitude < 45)
#date object and spliting
taxi<-taxi%>%
  mutate(
    pickup_datetime = ymd_hms(pickup_datetime),
    year = as.factor(year(pickup_datetime)),
    month = as.factor(month(pickup_datetime)),
    day = as.numeric(day(pickup_datetime)),
    dayOfWeek = as.factor(wday(pickup_datetime)),
    hour = as.numeric(hour(pickup_datetime)),
    timeOfDay = as.factor(ifelse(hour >= 3 & hour < 9,
                                 "Morning", ifelse(hour >= 9 & hour < 14, "Mid-Day",
                                                   ifelse(hour >= 14 & hour < 18, "Evening", "Night"))))
  )%>%
  select(-pickup_datetime)
write.csv(taxi,file="taxi.csv")

#define landmarks and well known areas

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






taxi<-taxi%>%
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

dist_var<-grep("to_", colnames(train))
dist_df<-train[,dist_var]

#divide the data into train and test
pd<- sample(2, nrow(taxi),replace=TRUE, prob=c(0.7,0.3))
train<- taxi[pd==1,]
test<- taxi[pd==2,]
#test$fare_amount <- NULL

#
train<-filter(train, passenger_count!=0, passenger_count<=6)
test1<-taxi[is.na(taxi$fare_amount),]

#dist
m<-geom_point(stat = "summary", fun.y = "mean", col = "blue", size = 5)
med<-geom_point(stat = "summary", fun.y = "median", col = "red", size = 5)
manLegend<-scale_color_manual(name = "Summary Stat", values = c("mean"="blue", "median"="red"))
#visualization - fare
ggplot(train, aes(fare_amount))+
  geom_histogram(fill = "blue", bins = 50)+
  ggtitle("Distribution of Fare Amount")+
  theme(plot.title = element_text(hjust = .5))

#visualization -dist
p<-ggplot(data=train, aes(x=timeOfDay, y= dist )) +
  geom_bar(stat="identity",color="blue", fill="blue")+
  theme(plot.title = element_text(hjust = .5))
p

#price variatiom
a<-ggplot(train, aes(timeOfDay, fill = timeOfDay))+
  geom_bar(stat = "count", aes(y = ..count..))+
  scale_x_discrete(limits=c("Morning", "Mid-Day", "Evening", "Night"))+
  ggtitle("Number of Passengers by timeOfDay")+
  theme(plot.title = element_text(hjust = .5))


b<-ggplot(train, aes(timeOfDay, fare_amount))+
  m+
  med+
  manLegend+
  ggtitle("Fare Amount by Time of Day")+
  ggtitle("Fare Amount by Time of Day")+
  theme(plot.title = element_text(hjust = .5))


gridExtra::grid.arrange(a,b)

#linear Regression
head(train)
taxi_lm <- lm(fare_amount~ pickup_latitude + pickup_longitude + dropoff_latitude + dropoff_longitude +
                passenger_count + year + month + day + dayOfWeek + hour + timeOfDay + dist , data = train)

summary(taxi_lm)
rss <- c(crossprod(taxi_lm$residuals))
mse <- rss / length(taxi_lm$residuals)
rmse <- sqrt(mse)
rmse
#residual plot
par(mfrow=c(2,2))
plot(taxi_lm)
