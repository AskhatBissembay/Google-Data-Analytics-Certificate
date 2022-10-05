library(tidyverse)
library(skimr)
library(janitor)
library(dplyr)
library(skimr)
library(ggplot2)
library(lubridate)
bike04 <- read.csv("C:/Users/HUAWEI/Desktop/bike project/202004-divvy-tripdata.csv")
bike05 <- read.csv("C:/Users/HUAWEI/Desktop/bike project/202005-divvy-tripdata.csv")
bike06 <- read.csv("C:/Users/HUAWEI/Desktop/bike project/202006-divvy-tripdata.csv")
bike07 <- read.csv("C:/Users/HUAWEI/Desktop/bike project/202007-divvy-tripdata.csv")
bike08 <- read.csv("C:/Users/HUAWEI/Desktop/bike project/202008-divvy-tripdata.csv")
bike09 <- read.csv("C:/Users/HUAWEI/Desktop/bike project/202009-divvy-tripdata.csv")
bike10 <- read.csv("C:/Users/HUAWEI/Desktop/bike project/202010-divvy-tripdata.csv")
bike11 <- read.csv("C:/Users/HUAWEI/Desktop/bike project/202011-divvy-tripdata.csv")
bike12 <- read.csv("C:/Users/HUAWEI/Desktop/bike project/202012-divvy-tripdata.csv")
bike13 <- read.csv("C:/Users/HUAWEI/Desktop/bike project/202101-divvy-tripdata.csv")
bike14 <- read.csv("C:/Users/HUAWEI/Desktop/bike project/202102-divvy-tripdata.csv")
bike15 <- read.csv("C:/Users/HUAWEI/Desktop/bike project/202103-divvy-tripdata.csv")


bike04 <- bike04 %>% mutate(start_station_id=as.character(start_station_id),
                            end_station_id=as.character(end_station_id))
bike05 <- bike05 %>% mutate(start_station_id=as.character(start_station_id),
                            end_station_id=as.character(end_station_id))
bike06 <- bike05 %>% mutate(start_station_id=as.character(start_station_id),
                            end_station_id=as.character(end_station_id))
bike07 <- bike05 %>% mutate(start_station_id=as.character(start_station_id),
                            end_station_id=as.character(end_station_id))
bike08 <- bike05 %>% mutate(start_station_id=as.character(start_station_id),
                            end_station_id=as.character(end_station_id))
bike09 <- bike05 %>% mutate(start_station_id=as.character(start_station_id),
                            end_station_id=as.character(end_station_id))
bike10 <- bike05 %>% mutate(start_station_id=as.character(start_station_id),
                            end_station_id=as.character(end_station_id))
bike11 <- bike05 %>% mutate(start_station_id=as.character(start_station_id),
                            end_station_id=as.character(end_station_id))

bike <- bind_rows(bike04, bike05, bike06, bike07, bike08,
                  bike09, bike10, bike11, bike12, bike13,
                  bike14, bike15)
#CLEANING
str(bike$started_at)
bike$started_at <- ymd_hms(bike$started_at)
bike$ended_at <- ymd_hms(bike$ended_at)

#Remove columns not required or beyond of the project
bike <- bike %>% select(-c(start_lat:end_lng))
glimpse(bike)

#Rename columns for better readability
bike <- bike %>% rename(ride_type=rideable_type,
                        start_time=started_at,
                        end_time=ended_at,
                        customer_type=member_casual)
bike$timedif.min <- round(difftime(bike$end_time,bike$start_time, units = "mins"), digit=2)


nrow(subset(bike,timedif.min < 0))
# 1693 rows time less than 0
# Delete them

bike <- bike %>% filter(timedif.min > 0)

#Group data by customer type
table(bike$customer_type)
#Separate data by casual and member users
casual <- bike %>% filter(customer_type=='casual')
member <- bike %>% filter(customer_type=='member')

summary(as.integer(bike$timedif.min))

#4&5. Analyze and Share the Data
bike %>% group_by(customer_type) %>% 
  summarise(sum = sum(timedif.min),
            mean = mean(timedif.min))

##statistical summary of trip_duration by customer_type
bike %>% group_by(customer_type) %>%
  summarise(min_trip_duration = min(timedif.min), max_trip_duration=max(timedif.min),
            median_trip_duration = median(timedif.min), mean_trip_duration=mean(timedif.min))

#Changing date type
bike$day_of_the_week <- format(as.Date(bike$start_time),'%A')
bike$month <- format(as.Date(bike$start_time),'%B_%Y')
bike$time <- as.POSIXct(bike$start_time, format="%H:%M")
#statistical summary of average trip duration by day of the week and customer type
bike %>% group_by(customer_type,day_of_the_week) %>% 
  summarise(number_of_rides = n(),average_duration_mins=mean(timedif.min)) %>% 
  arrange(customer_type,desc(number_of_rides))
## fix for the day_of_the_week variable so that they show up
bike$day_of_the_week <- factor(bike$day_of_the_week, levels= c("Sunday", "Monday", "Tuesday", 
                                                                     "Wednesday", "Thursday", "Friday", "Saturday"))

#Visualization by week
bike %>% 
  group_by(customer_type,day_of_the_week) %>% 
  summarise(number_of_rides = n()) %>%
  arrange(customer_type,desc(number_of_rides)) %>% 
  ggplot(aes(x = day_of_the_week,y = number_of_rides,fill=customer_type))+geom_bar(stat='identity') %>% 
  labs(title ="Total trips by customer type Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5))

bike$month <- as.factor(bike$month)
#Average number of trips by customer type and month
unique(bike$month)
bike %>% group_by(customer_type,month) %>% 
  summarise(number_of_riders=n(),average_duration_mins=mean(timedif.min)) %>% 
  arrange(customer_type,desc(number_of_riders))

# Visualization by month
bike %>% group_by(customer_type,month) %>% 
  summarise(number_of_rides=n()) %>% 
  arrange(customer_type,month) %>% 
  ggplot(aes(x=month,y=number_of_rides,fill=customer_type))+geom_bar(stat = 'identity') %>% 
  labs(title ="Total trips by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

  
#Visualizaton of average trip duration by customer type on each day of the week

bike %>% group_by(customer_type,day_of_the_week) %>% 
  summarise(average_trip_duration=mean(timedif.min)) %>% 
  ggplot(aes(x=day_of_the_week,y=average_trip_duration,fill=customer_type))+geom_bar(stat='identity') %>% 
  labs(title ="Average trip duration by customer type Vs. Day of the week") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Visualizaton of bike demand over 24 hr period (a day)
str(bike$time)
bike$time <- format(as.POSIXct(strptime(bike$time, "%Y-%m-%d  %H:%M:%S",tz="")) ,format = "%H:%M")
bike$time <- as.POSIXct(bike$time, format = "%H:%M")

bike %>%  
  group_by(customer_type, time) %>% 
  summarise(number_of_trips = n()) %>%
  ggplot(aes(x = time, y = number_of_trips, color = customer_type, group = customer_type)) +
  geom_line(stat='identity') + scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL, date_labels="%H:%M", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Demand over 24 hours of a day", x = "Time of the day")

#Visualizaton of ride type Vs. number of trips by customer type
bike %>%
  group_by(ride_type, customer_type) %>%
  summarise(number_of_trips = n()) %>%  
  ggplot(aes(x= ride_type, y=number_of_trips, fill= customer_type))+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Ride type Vs. Number of trips")

#For TABLEAU
clean_data <- aggregate(bike$timedif.min ~ bike$customer_type + bike$day_of_the_week, FUN = mean)
write.csv(clean_data, "Clean Data.csv", row.names = F)
