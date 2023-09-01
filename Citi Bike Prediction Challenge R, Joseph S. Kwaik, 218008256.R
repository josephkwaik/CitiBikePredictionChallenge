# Joseph S. Kwaik

# Loading useful packages
library(rpart)
library(rpart.plot)
library(lubridate)
library(tidyverse)
install.packages("plyr")
library(plyr)
library(RColorBrewer)
install.packages("geosphere")
library(geosphere)

# Errors.R
regr.error <- function(predicted,actual){
  #MAE
  mae <- mean(abs(actual-predicted))
  #MSE
  mse <- mean((actual-predicted)^2)
  #RMSE
  rmse <- sqrt(mean((actual-predicted)^2))
  #MAPE
  mape <- mean(abs((actual-predicted)/actual))
  errors <- c(mae,mse,rmse,mape)
  names(errors) <- c("mae","mse","rmse","mape")
  return(errors)
}

# Frame as Citi, CitiTest, CitiTestA
Citi <- citybike_train
View(Citi)
CitiTest <- citybike_test20000
View(CitiTest)
CitiTestA <- citybike_test10000A 
View(CitiTestA)

# Column for day
Citi$day <- as.numeric(substr(Citi$starttime,9,10))
CitiTest$day <- as.numeric(substr(CitiTest$starttime,9,10))
CitiTestA$day <- as.numeric(substr(CitiTestA$starttime,9,10))

# Columns for generation corresponding to age
generationbyage <- cut(Citi$birth.year, breaks = c(1946, 1965, 1981, 1997, 2012), labels = c("Boomers", "Generation X", "Millennials", "Generation Z"))
Citi$generation <- generationbyage
generationbyageTest <- cut(CitiTest$birth.year, breaks = c(1946, 1965, 1981, 1997, 2012), labels = c("Boomers", "Generation X", "Millennials", "Generation Z"))
CitiTest$generation <- generationbyageTest
generationbyageTestA <- cut(CitiTestA$birth.year, breaks = c(1946, 1965, 1981, 1997, 2012), labels = c("Boomers", "Generation X", "Millennials", "Generation Z"))
CitiTestA$generation <- generationbyageTestA # Just in case I want to test on A, might not do for all

# Column for type of day (week vs weekend)
Citi$day.type <- ifelse(Citi$day %in% c(4, 5, 11, 12, 18, 18, 25, 26), "Weekend", "Weekday")
CitiTest$day.type <- ifelse(CitiTest$day %in% c(4, 5, 11, 12, 18, 18, 25, 26), "Weekend", "Weekday")
CitiTestA$day.type <- ifelse(CitiTestA$day %in% c(4, 5, 11, 12, 18, 18, 25, 26), "Weekend", "Weekday") # Just in case I want to test on A, might not do for all

# Column for hour and time of day
hour = as.numeric(Citi$starttime) %% (24*60*60) / 3600
Citi$hour <- hour
timeofday <- cut(Citi$hour, breaks = c(0, 112, 18, 21, 24), labels = c("Morning", "Afternoon", "Evening", "Night"))
Citi$time.day <- timeofday
hourTest = as.numeric(CitiTest$starttime) %% (24*60*60) / 3600
CitiTest$hour <- hourTest
timeofdayTest <- cut(CitiTest$hour, breaks = c(0, 112, 18, 21, 24), labels = c("Morning", "Afternoon", "Evening", "Night"))
CitiTest$time.day <- timeofdayTest
hourTestA = as.numeric(CitiTestA$starttime) %% (24*60*60) / 3600
CitiTestA$hour <- hourTestA
timeofdayTestA <- cut(CitiTestA$hour, breaks = c(0, 112, 18, 21, 24), labels = c("Morning", "Afternoon", "Evening", "Night"))
CitiTestA$time.day <- timeofdayTestA # Just in case I want to test on A, might not do for all

# Let's move further now and assign columns for the weather variables of that day. Found https://www.kaggle.com/datasets/alejopaullier/new-york-city-weather-data-2019.
NYCTemp <- nyc_temperature
View(NYCTemp)
# Extracting the month from NYC temperature dataset so that we only have May
MayTemp <- NYCTemp[grepl('/5/', NYCTemp$date),]
View(MayTemp)
# Creating a column for day identical to the one we created for Citi
MayTemp$day <- ifelse(nchar(MayTemp$date) == 6, substr(MayTemp$date,0,1), substr(MayTemp$date,1,2))
MayTemp$day = as.numeric(MayTemp$day)

# Adding weather columns based on the matching day between the NYC temp dataset and the Citi dataset
# MERGING CHANGED ALL THE IDs. I NEED A DIFFERENT METHOD. Installed package that allows merge without changing order of IDs. 
CitiJoined <- join(Citi, MayTemp, by = "day")
View(CitiJoined)
Citi <- subset(CitiJoined, select = -c(date, new_snow, snow_depth))
CitiJoinedTest <- join(CitiTest, MayTemp, by = "day")
CitiTest <- subset(CitiJoinedTest, select = -c(date, new_snow, snow_depth))

# Now we have the weather of the day for every single ride. Oppa ! But let's make buckets for rain.
Citi$precipitation[Citi$precipitation == "T"] <- 0 # "T" is trace rain. Just making it 0.
Citi$precipitation <- as.numeric(Citi$precipitation)
CitiTest$precipitation[CitiTest$precipitation == "T"] <- 0 
CitiTest$precipitation <- as.numeric(CitiTest$precipitation)

plot(Citi$precipitation, Citi$tripduration) # A clear relationship, when it rains more, lower durations

# Light rainfall is less than 0.10 inches/hr. Moderate measures 0.10 to 0.30 inches of rain/hr. Heavy rainfall is > 0.30 inches of rain/hr
Citi$rain <- cut(Citi$precipitation, breaks = c(0, 0.001, 0.10, 0.30, 100), include.lowest = TRUE, labels = c("None", "Light", "Moderate", "Heavy"))
CitiTest$rain <- cut(CitiTest$precipitation, breaks = c(0, 0.001, 0.10, 0.30, 100), include.lowest = TRUE, labels = c("None", "Light", "Moderate", "Heavy"))

# Let's keep going. Adding a holiday column. I'm not sure how much this would help given that I feel that a holiday is a better predictor for frequency of rides than it is ride duration. Less people ride on Memorial Day, sure, but does it change the duration of those who do ride anyways?
# Mother's day is the 12th. Memorial day is the 27th. (In May of 2019.)
Citi$holiday <- ifelse(Citi$day %in% c(12, 27), "Yes", "No")
CitiHoliday <- subset(Citi, Citi$holiday == "Yes")
View(CitiHoliday) # Just checking that 12 and 27 are marked as holidays. 
CitiTest$holiday <- ifelse(CitiTest$day %in% c(12, 27), "Yes", "No")
CitiTestA$holiday <- ifelse(CitiTestA$day %in% c(12, 27), "Yes", "No")

# Distance between longitude and latitudes of start/end station
Citi$distance <- distVincentyEllipsoid(Citi[,5:6], Citi[,9:10])
CitiTest$distance <- distVincentyEllipsoid(CitiTest[,4:5], CitiTest[,8:9])

# BEST SUBMISSION BELOW.
PredictModelv2 <- lm(tripduration~usertype + time.day + day.type + hour + holiday + tavg + tmax + rain + precipitation + departure + distance, data = Citi)
PredictionLMv2 <- predict(PredictModelv2, CitiTest)
regr.error(PredictionLMv2, Citi$tripduration)
write.csv(PredictionLMv2, "Ysk32-4-CitiBikeChallengeSubLMv2.csv")







