library(lubridate)
library(mice)
library(ggplot2)
library(dplyr)
library(gam)
library(mltools)
library(corrplot)

      ### Start Data ###

train <- read.csv("/Users/Tervs/Documents/488 Final - Energy/datasets/train.csv")
test <- read.csv("/Users/Tervs/Documents/488 Final - Energy/datasets/test.csv")
building <- read.csv("/Users/Tervs/Documents/488 Final - Energy/datasets/building_metadata.csv")
weather.train <- read.csv("/Users/Tervs/Documents/488 Final - Energy/datasets/weather_train.csv")
weather.test <- read.csv("/Users/Tervs/Documents/488 Final - Energy/datasets/weather_test.csv")
sample <- read.csv("/Users/Tervs/Documents/488 Final - Energy/datasets/sample_submission.csv")



      ### Missing data ###


## Building NA's

#Want to use all but building ID for prediction
init.b <- mice(building, maxit = 1)
meth.b <- init.b$meth
predM.b <- init.b$predictorMatrix
#remove building ID (both for imputation and prediction)
predM.b[ ,c("building_id")] = 0
#create new building dataset
imput.b <- mice(building, method = meth.b, predictorMatrix = predM.b, m=5)
final.b <- complete(imput.b)

## Weather NA's

#Training Set
#We need to separate "timestamp" so we can use it as numeric
as_datetime(weather.train$timestamp)
time.hour <- hour(weather.train$timestamp)
time.day <- day(weather.train$timestamp)
time.month <- month(weather.train$timestamp)
time.year <- year(weather.train$timestamp)
new.time <- cbind.data.frame(time.hour, time.day, time.month, time.year)
#Now we can add to our training data
weather.train <- subset(weather.train, select = -c(timestamp))
weather.train <- cbind.data.frame(new.time, weather.train)
#We want all of them used
init.w <- mice(weather.train, maxit = 0)
meth.w <- init.w$method
predM.w <- init.w$predictorMatrix
#create new weather dataset
imput.w <- mice(weather.train, predictorMatrix = predM.w, m=1)
imput.w <- complete(imput.w)
final.w <- imput.w
final.train <- final.w
#Let's clean up some unwanted datasets
rm(building, weather.train)
rm(imput.w, init.w, predM.w, meth.w)
rm(time.day, time.hour, time.month, time.year, timestamp.n)

#Test set
#We need to separate "tiemestamp" so we can use it as numeric
as_datetime(weather.test$timestamp)
time.hour <- hour(weather.test$timestamp)
time.day <- day(weather.test$timestamp)
time.month <- month(weather.test$timestamp)
time.year <- year(weather.test$timestamp)
new.time <- cbind.data.frame(time.hour, time.day, time.month, time.year)
#Now we can add to our training data
weather.test <- subset(weather.test, select = -c(timestamp))
weather.test <- cbind.data.frame(new.time, weather.test)
#We want all of them used
init.w <- mice(weather.test, maxit = 0)
meth.w <- init.w$method
predM.w <- init.w$predictorMatrix
#create new weather dataset
imput.w <- mice(weather.test, predictorMatrix = predM.w, m=1)
imput.w <- complete(imput.w)
final.test <- imput.w

#Clean up
#Let's clean up some unwanted datasets and rename our created sets
# rm(weather.test)
# rm(imput.w, init.w, predM.w, meth.w)
# rm(time.day, time.hour, time.month, time.year, new.time, timestamp.n)
# rm(final.w)
# final.w.train <- final.train
# final.w.test <- final.test
# rm(final.train, final.test)


      ### Timestamp ###
#Before we can merge, we need to standardize where timestamp appears:
#Timestamp is in train, test, final.train, final.test
#We need to perform deconcatenatino on train, test, and add day to final.train, final.test

#Train
as_datetime(train$timestamp)
train.hour <- hour(train$timestamp)
train.day <- wday(train$timestamp, label = TRUE)
train.date <- day(train$timestamp)
train.month <- month(train$timestamp, label = TRUE)
train.year <- year(train$timestamp)
new.train <- cbind.data.frame(train.hour, train.day, train.date, train.month, train.year)
new.train <- cbind.data.frame(new.train, train)
new.train <- subset(new.train, select = -c(timestamp))
final.train <- new.train
rm(new.train)

#Test
as_datetime(test$timestamp)
test.hour <- hour(test$timestamp)
test.day <- wday(test$timestamp, label = TRUE)
test.date <- day(test$timestamp)
test.month <- month(test$timestamp, label = TRUE)
test.year <- year(test$timestamp)
new.test <- cbind.data.frame(test.hour, test.day, test.date, test.month, test.year)
new.test <- cbind.data.frame(new.test, test)
new.test <- subset(new.test, select = -c(timestamp))
final.test <- new.test
rm(new.test)

#w.train
time.day <- wday(weather.train$timestamp, label = TRUE)
time.date <- day(weather.train$timestamp)
final.w.train <- cbind.data.frame(final.w.train, time.day, time.date)
final.w.train <- subset(final.w.train, select = -c(2, 13))
final.w.train <- final.w.train[ ,c(1, 12, 13, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#w.test
time.day <- wday(weather.test$timestamp, label = TRUE)
time.date <- day(weather.test$timestamp)
final.w.test <- cbind.data.frame(final.w.test, time.day, time.date)
final.w.test <- subset(final.w.test, select = -c(2))
final.w.test <- final.w.test[ ,c(1, 12, 13, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

#Clean up
# rm(merg.dat, test, train, weather.test, weather.train, test.date, test.day, test.hour, test.month,
#    test.year, time.date, time.day, train.date, train.day, train.hour, train.month, train.year)
# 


         ### Data merge ###



#Training set merge - first building meta w/ train
merg.dat <- merge(final.train, final.b, by.x = "building_id", by.y = "building_id", all.x = TRUE)

#Then with weather train data
colnames(merg.dat)[2] <- "time.hour"
colnames(merg.dat)[3] <- "time.day"
colnames(merg.dat)[4] <- "time.date"
colnames(merg.dat)[5] <- "time.month"
colnames(merg.dat)[6] <- "time.year"
final.w.train$time.month <- month(final.w.train$time.month, label = TRUE)
train.full <- merge(merg.dat, final.w.train, by.x = c("time.hour", "time.day", "time.date", "time.month", "time.year", "site_id"), 
                    by.y = c("time.hour", "time.day", "time.date", "time.month", "time.year", "site_id"), all.x = TRUE)

#So, we still have NA's, but way less than before AND the same amount for each weather predictor. This shoud be easy to fix later.

#Test set merge - merge building meta w/ test
merg.dat.test <- merge(final.test, final.b, by.x = "building_id", by.y = "building_id", all.x = TRUE)

#Then with weather test data
colnames(merg.dat.test)[2] <- "time.hour"
colnames(merg.dat.test)[3] <- "time.day"
colnames(merg.dat.test)[4] <- "time.date"
colnames(merg.dat.test)[5] <- "time.month"
colnames(merg.dat.test)[6] <- "time.year"
final.w.test$time.month <- month(final.w.test$time.month, label = TRUE)
test.full <- merge(merg.dat.test, final.w.test, by.x = c("time.hour", "time.day", "time.date", "time.month", "time.year", "site_id"), 
                   by.y = c("time.hour", "time.day", "time.date", "time.month", "time.year", "site_id"), all.x = TRUE)

#Clean up
# rm(merg.dat, merg.dat.test)
# rm(final.b, final.test, final.train, final.w.test, final.w.train)

#Now let's deal with our NA's

#Train
init <- mice(train.full, maxit = 0)
meth <- init$meth
predM <- init$predictorMatrix
#remove building ID (both for imputation and prediction)
meth <- "sample"
predM[ ,c("building_id", "time.hour", "time.day", "time.date", "time.month", "time.year", "meter", "meter_reading", "primary_use", "square_feet", "floor_count", "year_built")]=0
#create new building dataset
imput <- mice(train.full, method = "sample", predictorMatrix = predM, m=1)
final <- complete(imput)
train.final <- complete(imput)
rm(init, meth, predM, imput)

#Test
init <- mice(test.full, maxit = 0)
meth <- init$meth
predM <- init$predictorMatrix
#remove building ID (both for imputation and prediction)
meth <- "sample"
predM[ ,c("building_id", "time.hour", "time.day", "time.date", "time.month", "time.year", "meter", "row_id", "primary_use", "square_feet", "floor_count", "year_built")]=0
#create new building dataset
imput <- mice(test.full, method = "sample", predictorMatrix = predM, m=1)
test.final <- complete(imput)

#Clean up
# rm(train.full, test.full)
# rm(imput, init, predM, meth)

#create a new timestamp variable and move it to the front:
train.final$timestamp <- ymd_h(paste(train.final$time.year, train.final$time.month, train.final$time.date, train.final$time.hour, sep = "-"))
train.final <- train.final %>%
   select(timestamp, everything())
test.final$timestamp <- ymd_h(paste(test.final$time.year, test.final$time.month, test.final$time.date, test.final$time.hour, sep = "-"))
test.final <- test.final %>%
   select(timestamp, everything())


#separate by meter type
train.0 <- train.final[train.final$meter %in% 0,]
train.1 <- train.final[train.final$meter %in% 1,]
train.2 <- train.final[train.final$meter %in% 2,]
train.3 <- train.final[train.final$meter %in% 3,]
test.0 <- test.final[test.final$meter %in% 0,]
test.1 <- test.final[test.final$meter %in% 1,]
test.2 <- test.final[test.final$meter %in% 2,]
test.3 <- test.final[test.final$meter %in% 3,]



      ### EDA ###


#Correlations
cor.0 <- cor(train.0[12:21], train.0$meter_reading) #High cor w/ sq_ft, fl_ct
cor.1 <- cor(train.1[12:21], train.1$meter_reading)
cor.2 <- cor(train.2[12:21], train.2$meter_reading)
cor.3 <- cor(train.3[12:21], train.3$meter_reading) #slight cor w/ sq_ft, air temp
corrplot(cor.0)
corrplot(cor.1)
corrplot(cor.2)
corrplot(cor.3)


#TIME BY METER READING
ggplot(train.0, aes(x = time.day, y = log(meter_reading + 1))) +
   geom_boxplot(alpha = 0.5, colour = "blue") +
   coord_flip() +
   ggtitle("Meter reading by day of the week")
ggplot(train.3, aes(x = time.month, y = log(meter_reading + 1))) +
   geom_boxplot(alpha = 0.5, colour = "blue") +
   coord_flip() +
   ggtitle("Meter reading by month")
hour.avg <- data.frame(0:23)
rownames(hour.avg) <- 0:23
for (i in 1:24){
   hour.avg[i,] <- mean(train.0[train.0$time.hour %in% (i-1),]$meter_reading) 
   print(i-1)
}
hour.avg$hour <- 0:23
ggplot(hour.avg, aes(x = hour, y = hour.avg$X0.23)) +
   geom_point() +
   ggtitle("Average meter reading by hour")

#Weekdays generally higher energy usage, Weekends less w/ sunday the smallest

#WEATHER BY METER READING PLOTS
ggplot(train.final, aes(x = air_temperature, y = log(meter_reading + 1))) +
   geom_point() +
   ggtitle("air temp by energy usage")
ggplot(train.final, aes(x = cloud_coverage, y = log(meter_reading + 1))) +
   geom_point() +
   ggtitle("cloud coverage by energy usage")
ggplot(train.final, aes(x = dew_temperature, y = log(meter_reading + 1))) +
   geom_point() +
   ggtitle("dew temp by energy usage")
ggplot(train.final, aes(x = precip_depth_1_hr, y = log(meter_reading + 1))) +
   geom_point() +
   ggtitle("precipritation depth by energy usage")
ggplot(train.final, aes(x = sea_lvl_pressure, y = log(meter_reading + 1))) +
   geom_point() +
   ggtitle("sea level pressure by energy usage")

#AVG METER READING BY BUILDING
bld.avg <- data.frame(0:1448)
rownames(bld.avg) <- 0:1448
for (i in 1:1449){
   bld.avg[i,] <- mean(train.final[train.final$building_id %in% (i-1),]$meter_reading) 
   print(i-1)
}
bld.avg <- cbind(bld.avg, c(0:1448))
colnames(bld.avg) <- c("avg.meter.read", "building_id")
ggplot(bld.avg, aes(x = building_id, y = avg.meter.read)) +
   geom_point() +
   ggtitle("Average meter reading by building")
ggplot(bld.avg, aes(x = building_id, y = log(avg.meter.read+1))) +
   geom_point() +
   ggtitle("Log of average meter reading by building")

#building 1099 an anomoly - Think about omitting? avg. reading is wayyyy too high

#let's inspect further
bld.1099 <- subset(train.final, building_id == 1099)
plot(bld.1099$timestamp, log(bld.1099$meter_reading+1))

#So that looks weird, how about the other high avg. buildings?

#5 more buildings who's avg. meter read is >10,000
bld.778 <- subset(train.final, building_id == 778)
bld.1197 <- subset(train.final, building_id == 1197)
bld.1168 <- subset(train.final, building_id == 1168)
bld.1159 <- subset(train.final, building_id == 1159)
bld.1148 <- subset(train.final, building_id == 1148)
plot(bld.778$timestamp, log(bld.778$meter_reading+1))
plot(bld.1197$timestamp, log(bld.1197$meter_reading+1))
plot(bld.1168$timestamp, log(bld.1168$meter_reading+1))
plot(bld.1159$timestamp, log(bld.1159$meter_reading+1))
plot(bld.1148$timestamp, log(bld.1148$meter_reading+1))

#Okay, so 1099 and 778 look a little off, let's compare their data to the whole set

ggplot(train.final, aes(x = log(meter_reading + 1))) + 
   geom_histogram(alpha = .5) +
   ggtitle("Meter reading from all buildings")
ggplot(bld.1099, aes(x = log(meter_reading + 1))) + 
   geom_histogram(alpha = .5) +
   ggtitle("Meter reading from bld 1099")
ggplot(bld.778, aes(x = log(meter_reading + 1))) + 
   geom_histogram(alpha = .5) +
   ggtitle("Meter reading from bld 778")
#These are all really pulling the average, with building 1099 the worst. log transformation of response required.
rm(bld.1148, bld.1159, bld.1168, bld.1197)


#METER READING BY METER TYPE
ggplot(train.final, aes(x = log(meter_reading + 1), fill = as.character(meter))) + 
   geom_density(alpha = 0.5, adjust = 2) +
   ggtitle("Average meter reading by energy type")
#So we see that electrcity uses the least amount, steam using the most.
#This is good - maybe 4 different models? 1 and 2 look similar, 0 and 3 quite different
#From here on out, we should be looking at the training set based on meter type, so let's make four new datasets.


         ### MODELING ###



#LASSO to find potentially signif. variables
train.0.matx <- model.matrix(log(meter_reading + 1) ~ primary_use square_feet + year_built + floor_count + air_temperature + cloud_coverage + dew_temperature + sea_level_pressure + wind_direction + wind_speed, data = train.0)
mdl.lasso <-cv.glmnet(train.0.matx, train.0$meter_reading, alpha = 1)
#lambda = .3097798
mdl.lasso.f <- glmnet(train.0.matx, train.0$meter_reading, alpha = 1, lambda = mdl.lasso$lambda.min)
coeff <- predict(mdl.lasso.f, type = "coefficients")[1:11,]
#Didn't really give us anything


#GAM
#Training model on train
#0 - Electricity
mdl.0 <- gam(log(meter_reading+1) ~ ns(square_feet, df = 5) + ns(year_built, df=5) + time.hour + time.month + time.day + s(air_temperature) + 
                 s(cloud_coverage) + s(dew_temperature) + s(sea_level_pressure) + s(wind_direction) + s(wind_speed), data = train.0)
# pred.0 <- predict(mdl.0, newdata = train.0)
# rmsle(pred.0, log(train.0$meter_reading+1))
#1 - Chill
mdl.1 <- gam(log(meter_reading+1) ~ ns(square_feet, df = 5) + ns(year_built, df=5) + time.hour + time.month + time.day + s(air_temperature) + 
                s(cloud_coverage) + s(dew_temperature) + s(sea_level_pressure) + s(wind_direction) + s(wind_speed), data = train.1)
# pred.1 <- predict(mdl.1, newdata = train.1)
# rmsle(pred.1, log(train.1$meter_reading+1))
#2 - Steam
mdl.2 <- gam(log(meter_reading+1) ~ ns(square_feet, df = 5) + time.hour + time.month + time.day + s(air_temperature) + 
                s(cloud_coverage) + s(dew_temperature) + s(sea_level_pressure) + s(wind_direction) + s(wind_speed), data = train.2)
# pred.2 <- predict(mdl.2, newdata = train.2)
# rmsle(pred.2, log(train.2newdata = reading+1))
#3 - Hot
mdl.3 <- gam(log(meter_reading+1) ~ ns(square_feet, df = 5) + time.hour + time.month + time.day + s(air_temperature) + 
                s(cloud_coverage) + s(dew_temperature) + s(sea_level_pressure) + s(wind_direction) + s(wind_speed), data = train.3)
# pred.3 <- predict(mdl.3, newdata = train.3)
# rmsle(pred.3, log(train.3$meter_reading+1))

#Test predictions
#0 - Electricity
newpred.0 <- predict(mdl.0, newdata = test.0)
newpred.0 <- exp(newpred.0) - 1
test.0$yhat <- newpred.0

#1 - Chill
newpred.1 <- predict(mdl.1, newdata = test.1)
newpred.1 <- exp(newpred.1) - 1
test.1$yhat <- newpred.1

#2 - Steam
newpred.2 <- predict(mdl.2, newdata = test.2)
newpred.2 <- exp(newpred.2) - 1
test.2$yhat <- newpred.2

#3 - Hot
newpred.3 <- predict(mdl.3, newdata = test.3)
newpred.3 <- exp(newpred.3) - 1
test.3$yhat <- newpred.3

#Add predictons to test set to prepare for submisison
test.preds <- rbind(test.0, test.1, test.2, test.3)
submission <- subset(test.preds, select = c(9, 22))
colnames(submission) <- c("row_id", "meter_reading")
write.csv(submission,"/Users/Tervs/Desktop/sample_submission.csv", row.names = FALSE)

#Clean-up
rm(test.final, train.final)
rm(test.final)