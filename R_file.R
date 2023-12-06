

library(vitae)

library(dplyr)

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("/Users/couli/Documents/MSBA/Certificates/Google_certificate/Case_data/trip_csv")

jan22 <- read_csv("tripdata_0122.csv")
feb22 <- read_csv("tripdata_0222.csv")
mar22 <- read_csv("tripdata_0322.csv")
apr22 <- read_csv("tripdata_0422.csv")
may22 <- read_csv("tripdata_0522.csv")
jun22 <- read_csv("tripdata_0622.csv")
jul22 <- read_csv("tripdata_0722.csv")
aug22 <- read_csv("tripdata_0822.csv")
sep22 <- read_csv("tripdata_0922.csv")
oct22 <- read_csv("tripdata_1022.csv")
nov22 <- read_csv("tripdata_1122.csv")
dec22 <- read_csv("tripdata_1222.csv")

glimpse(jan22)
glimpse(feb22)
glimpse(mar22)
glimpse(apr22)
glimpse(may22)
glimpse(jun22)
glimpse(jul22)
glimpse(aug22)
glimpse(sep22)
glimpse(oct22)
glimpse(nov22)
glimpse(dec22)


#changing colum type
jan22$started_at <- mdy_hm(jan22$started_at)
jan22$ended_at <- mdy_hm(jan22$ended_at)

feb22$started_at <- mdy_hm(feb22$started_at)
feb22$ended_at <- mdy_hm(feb22$ended_at)

mar22$started_at <- mdy_hm(mar22$started_at)
mar22$ended_at <- mdy_hm(mar22$ended_at)

apr22$started_at <- mdy_hm(apr22$started_at)
apr22$ended_at <- mdy_hm(apr22$ended_at)

may22$started_at <- mdy_hm(may22$started_at)
may22$ended_at <- mdy_hm(may22$ended_at)

jun22$started_at <- mdy_hm(jun22$started_at)
jun22$ended_at <- mdy_hm(jun22$ended_at)

jul22$started_at <- mdy_hm(jul22$started_at)
jul22$ended_at <- mdy_hm(jul22$ended_at)


aug22$started_at <- mdy_hm(aug22$started_at)
aug22$ended_at <- mdy_hm(aug22$ended_at)

oct22$started_at <- mdy_hm(oct22$started_at)
oct22$ended_at <- mdy_hm(oct22$ended_at)

nov22$started_at <- mdy_hm(nov22$started_at)
nov22$ended_at <- mdy_hm(nov22$ended_at)

dec22$started_at <- mdy_hm(dec22$started_at)
dec22$ended_at <- mdy_hm(dec22$ended_at)
  
#combining datasets
year22 <- bind_rows(jan22, feb22, mar22, apr22, may22, jun22, jul22, aug22, sep22, oct22, nov22, dec22)
glimpse(year22)

#removing colums
year22 <- year22 %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))



colnames(year22)  #List of column names
nrow(year22)  #How many rows are in data frame?
dim(year22)  #Dimensions of the data frame?
head(year22)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(year22)  #See list of columns and data types (numeric, character, etc)
summary(year22)  #Statistical summary of data. Mainly for numerics


year22$date <- as.Date(year22$started_at) #The default format is yyyy-mm-dd
year22$month <- format(as.Date(year22$date), "%m")
year22$day <- format(as.Date(year22$date), "%d")
year22$year <- format(as.Date(year22$date), "%Y")
year22$day_of_week <- format(as.Date(year22$date), "%A")

year22$ride_length <- difftime(year22$ended_at,year22$started_at)

str(year22)

year22$ride_length <- as.numeric(as.character(year22$ride_length))
is.numeric(year22$ride_length)

#summary stats

summary(year22)

year22[which.min(year22$ride_length),]

year22[year22$started_at> year22$ended_at,]

year22_v2 <- year22[!(year22$ride_length<0),]

summary(year22_v2$ride_length)

aggregate(year22$ride_length ~ year22$member_casual, FUN = mean)
aggregate(year22$ride_length ~ year22$member_casual, FUN = median)
aggregate(year22$ride_length ~ year22$member_casual, FUN = max)
aggregate(year22$ride_length ~ year22$member_casual, FUN = min)


aggregate(year22_v2$ride_length ~ year22_v2$member_casual + year22_v2$day_of_week, FUN = mean)

year22_v2$day_of_week <- ordered(year22_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(year22_v2$ride_length ~ year22_v2$member_casual + year22_v2$day_of_week, FUN = mean)

year22_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()                                                        #calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>%                 # calculates the average duration
  arrange(member_casual, weekday) 

#create visualizations

year22_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


year22_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")









