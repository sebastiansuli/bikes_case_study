
install.packages("janitor")
library(tidyverse)
library(lubridate)
library(readr)
library(janitor)
library(ggplot2)


### DATA LOADING


d0 <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/google data/gda_case_study/csvs/202111-divvy-tripdata.csv", 
               header = TRUE, sep = ';')
View(d1)

d1 <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/google data/gda_case_study/csvs/202112-divvy-tripdata.csv", 
               header = TRUE, sep = ',')
View(d1)
d2 <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/google data/gda_case_study/csvs/202201-divvy-tripdata.csv", 
               header = TRUE, sep = ',')
d3 <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/google data/gda_case_study/csvs/202202-divvy-tripdata.csv", 
               header = TRUE, sep = ',')
d4 <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/google data/gda_case_study/csvs/202203-divvy-tripdata.csv", 
               header = TRUE, sep = ',')
d5 <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/google data/gda_case_study/csvs/202204-divvy-tripdata.csv", 
               header = TRUE, sep = ',')
d6 <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/google data/gda_case_study/csvs/202205-divvy-tripdata.csv", 
               header = TRUE, sep = ',')
d7 <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/google data/gda_case_study/csvs/202206-divvy-tripdata.csv", 
               header = TRUE, sep = ',')
d8 <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/google data/gda_case_study/csvs/202207-divvy-tripdata.csv", 
               header = TRUE, sep = ',')
d9 <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/google data/gda_case_study/csvs/202208-divvy-tripdata.csv", 
               header = TRUE, sep = ',')
d10 <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/google data/gda_case_study/csvs/202209-divvy-publictripdata.csv", 
                header = TRUE, sep = ',')
d11 <- read.csv(file = "C:/Users/sebas/OneDrive/Pulpit/google data/gda_case_study/csvs/202210-divvy-tripdata.csv", 
                header = TRUE, sep = ',')


### PREPARATION 


#data merging

raw_data <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11)
View(raw_data)

#data cleaning

#removing empty cols and rows (but there any empty; janitor)
raw_data <- remove_empty(raw_data, which = c("cols"))
raw_data <- remove_empty(raw_data, which = c("rows"))
dim(raw_data)

#cleaning names, like # to percent or upper/lower cases (but there any unclean; janitor)
clean_data <- clean_names(raw_data)
View(clean_data)

#retrieves duplicates
get_dupes(clean_data, ride_id)

#NA values
colSums(is.na(clean_data))

clean_data <- clean_data[complete.cases(clean_data), ]
View(clean_data)

colSums(is.na(clean_data))

#remove cols
remove_data <- clean_data %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
View(remove_data)

#convert time type
remove_data$started_at <- as.POSIXct(remove_data$started_at, "%Y-%m-%d %H:%M:%S", tz="Europe/London")
remove_data$ended_at <- as.POSIXct(remove_data$ended_at, "%Y-%m-%d %H:%M:%S", tz="Europe/London")

str(remove_data)

#remove started_at greater than ended_at
fremove_data <- remove_data %>%
  filter(remove_data$started_at < remove_data$ended_at)

#add column ride_length
add_data <- fremove_data %>%
  mutate(ride_length = as.numeric(fremove_data$ended_at - fremove_data$started_at))
summary(add_data$ride_length)

View(add_data)

#add weekday
add_data <- add_data %>%
  mutate(weekday = paste(strftime(add_data$ended_at, "%u")))
unique(add_data$weekday)

#data summary
str(add_data)
summary(add_data)
unique(add_data$member_casual)
unique(add_data$rideable_type)

#aggregation data for each day and month (I made aggregation for weekday, but I also could need for month and days;
# days for holidays and end of and beginning of months and months for seasons and average weather and temperatures)
add_data <- add_data %>%
  mutate(month = paste(strftime(add_data$ended_at, "%m")))
add_data$month <- as.integer(add_data$month)
unique(add_data$month)

add_data <- add_data %>%
  mutate(day = paste(strftime(add_data$ended_at, "%d")))
add_data$day <- as.integer(add_data$day)
unique(add_data$day)


### ANALYZING


# # Guiding questions: 
# - ogranizing and formatting
# - what I discovered
# - trends and relationships

# # So I need to:
# - aggregate data
# - organize and format them
# - perform calculations
# - identify trends and relationships

# Descriptive analysis on ride_length (all figures in seconds)
mean(add_data$ride_length) #straight average (total ride length / rides)
median(add_data$ride_length) #midpoint number in the ascending array of ride lengths
max(add_data$ride_length) #longest ride
min(add_data$ride_length) #shortest ride

#min = 1 second, max = 2057644. I should delete smaller than ~ 60sec and bigger than one week (the biggest one is 23 days,
#it could that long because of steal)
str(add_data)
#add_data$ride_length <- as.integer(add_data$ride_length)
#add_data <- subset(add_data, ride_length > '604 800')
#summary(add_data$ride_length)
#add_data <- subset(add_data, ride_length < '60')

# Compare members and casual users
aggregate(add_data$ride_length ~ add_data$member_casual, FUN = mean)
#casual rides longer, 1346 to 751
aggregate(add_data$ride_length ~ add_data$member_casual, FUN = median)
aggregate(add_data$ride_length ~ add_data$member_casual, FUN = max)
aggregate(add_data$ride_length ~ add_data$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(add_data$ride_length ~ add_data$member_casual + add_data$weekday, FUN = mean)
add_data$weekday <- ordered(add_data$weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# average ride time by each day for members vs casual users
aggregate(add_data$ride_length ~ add_data$member_casual + add_data$weekday, FUN = mean)

# analyze ridership data by type and weekday
add_data %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()							 
            ,average_duration = mean(ride_length)) %>% 		
  arrange(member_casual, weekday)								

# Visualization of the number of rides by rider type
add_data %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Visualization for average duration
add_data %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#duration vs distance
ggplot(filter(add_data, add_data$ride_length < 3600)) +
  geom_histogram(mapping = aes(x = ride_length)) +
  facet_wrap(~member_casual)
#casuals rides longer, but I can't observe many from those charts

#distance traveled in meters
ggplot(filter(add_data, add_data$ride_length < 10000)) +
  geom_density(mapping = aes(x = ride_length)) +
  facet_wrap(~member_casual)
#casuals cover a greater distance, but it's not big diffrence
