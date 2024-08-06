# BTC1855 Midterm Project.
#' The dataset provided is from the second year of Bay Area Bike Share's 
#' operation and has been downloaded from Kaggle 
#' (https://www.kaggle.com/datasets/benhamner/sf-bay-area-bike-share). 

# Install and load relevant packages.
install.packages("dplyr")
library(dplyr)

# Import data sets and create working copies.
station <- read.csv("station.csv")
station_clean <- station
trip <- read.csv("trip.csv")
trip_clean <- trip
weather <- read.csv("weather.csv")
weather_clean <- weather

# Explore data frames.
glimpse(station)
glimpse(trip)
glimpse(weather)
# Check for duplicates in each of the data sets.
TRUE %in% duplicated(station_clean)
TRUE %in% duplicated(trip_clean)
TRUE %in% duplicated(weather_clean)

# Check for NAs and blank spaces in each of the data sets.
which(is.na(station_clean))
which(station_clean == "")
which(is.na(trip_clean))
which(trip_clean == "")
which(is.na(weather_clean))
which(weather_clean == "")
# Station data has no NAs nor blank spaces. No need to change.
# Trip data has blank spaces. Let's change to NAs for consistency.
trip_clean[trip_clean == ""] <- NA
#' Weather data has both NAs and blank spaces. Let's change blank spaces 
#' to NAs for consistency.
weather_clean[weather_clean == ""] <- NA

# Install and load packages for exploratory data analysis.
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")
library(funModeling) 
library(tidyverse) 
library(Hmisc)

#' Set up basic EDA function for reference only. DELETE ONCE DONE. 
basic_eda <- function(data) {
  glimpse(data)
  print(status(data))
  freq(data) 
  print(profiling_num(data))
  plot_num(data)
  describe(data)
}

# Run EDA for trip data.
glimpse(trip_clean)
#' We can see that "id" variables are set to int when they should be 
#' character type.
trip_clean$id <- as.character(trip_clean$id)
trip_clean$start_station_id <- as.character(trip_clean$start_station_id)
trip_clean$end_station_id <- as.character(trip_clean$end_station_id)
trip_clean$bike_id <- as.character(trip_clean$bike_id)
#' We can see that "date" variables are set to character when they should 
#' be date type.
library(lubridate)
trip_clean$start_date <- strptime(trip_clean$start_date, "%m/%d/%Y %H:%M")
trip_clean$end_date <- strptime(trip_clean$end_date, "%m/%d/%Y %H:%M")

# Continue with EDA.
status(trip_clean)
#' There are 74 different station names and 70 different station IDs. 
#' Let's investigate by finding the unique combinations of the above and
#' seeing which IDs are duplicated among the stations.
start_station_name_id_combinations <- unique(trip_clean[,c("start_station_name", "start_station_id")])
start_station_name_id_combinations[duplicated(start_station_name_id_combinations$start_station_id),]
# Found them.
start_station_name_id_combinations[start_station_name_id_combinations$start_station_id == 47 | 
             start_station_name_id_combinations$start_station_id == 46 | 
             start_station_name_id_combinations$start_station_id == 80 |
             start_station_name_id_combinations$start_station_id == 25,]
#' Two duplicate station names are due to a spelling mistake. Let's correct
#' and check that they've been removed.
trip_clean <- trip_clean %>% 
  mutate(start_station_name = recode(start_station_name, 
                                     "Post at Kearny" = "Post at Kearney", 
                                     "Washington at Kearny" = "Washington at Kearney"))
any(trip_clean$start_station_name == "Post at Kearny")
any(trip_clean$start_station_name == "Washington at Kearny")
#' Two duplicate station names just don't have an original ID. Let's check
#' how many observations this entails and then remove them.
nrow(trip_clean[trip_clean$start_station_name == "San Jose Government Center",])
nrow(trip_clean[trip_clean$start_station_name == "Broadway at Main",])
# Record removed observations and then remove them from clean data.
trip_removed <- rbind(trip_clean[trip_clean$start_station_name == "San Jose Government Center",],
                      trip_clean[trip_clean$start_station_name == "Broadway at Main",])
trip_clean <- trip_clean[!trip_clean$start_station_name == "San Jose Government Center",]
trip_clean <- trip_clean[!trip_clean$start_station_name == "Broadway at Main",]
# Repeat the same for end station names and end station IDs (same issue).
end_station_name_id_combinations <- unique(trip_clean[,c("end_station_name", "end_station_id")])
end_station_name_id_combinations[duplicated(end_station_name_id_combinations$end_station_id),]
end_station_name_id_combinations[end_station_name_id_combinations$end_station_id == 47 | 
                                     end_station_name_id_combinations$end_station_id == 46 | 
                                     end_station_name_id_combinations$end_station_id == 80 |
                                     end_station_name_id_combinations$end_station_id == 25,]
trip_clean <- trip_clean %>% 
  mutate(end_station_name = recode(end_station_name, 
                                     "Post at Kearny" = "Post at Kearney", 
                                     "Washington at Kearny" = "Washington at Kearney"))
nrow(trip_clean[trip_clean$end_station_name == "San Jose Government Center",])
nrow(trip_clean[trip_clean$end_station_name == "Broadway at Main",])
trip_removed <- rbind(trip_removed,
                      trip_clean[trip_clean$end_station_name == "San Jose Government Center",],
                      trip_clean[trip_clean$end_station_name == "Broadway at Main",])
trip_clean <- trip_clean[!trip_clean$end_station_name == "San Jose Government Center",]
trip_clean <- trip_clean[!trip_clean$end_station_name == "Broadway at Main",]
# There are zero values for "zip-code", should be changed to NA.
trip_clean$zip_code[which(trip_clean$zip_code == 0)] <- NA

#' freq() for character variables. Note that this is pre-removal of cancelled
#' trips and outliers.
freq(trip_clean)
freq(trip_clean$start_station_name)
#' hist()) and profiling_num() for numerical variable. Note that this 
#' is pre-removal of cancelled trips and outliers.
hist(trip_clean$duration)
profiling_num(trip_clean)

# Run EDA for weather data.
glimpse(weather_clean)
# Let's change date to date variable type.
weather_clean$date <- strptime(weather_clean$date, "%m/%d/%Y")
#' One of the variables has an inconsistent name capitalization. Let's 
#' change this.
names(weather_clean)[names(weather_clean)=="max_wind_Speed_mph"] <- "max_wind_speed_mph"
#' Precipitation amount is of character type due to trace ("T") entries. 
#' Let's replace "T" values with "0.005" inches, which is considered by 
#' meteorologists as "trace". Then convert the vector to numeric.
weather_clean <- weather_clean %>% 
  mutate(precipitation_inches = recode(precipitation_inches, 
                                   "T" = "0.005"))
weather_clean$precipitation_inches <- as.numeric(weather_clean$precipitation_inches)
# Zip code variable is numeric, should be changed to character.
weather_clean$zip_code <- as.character(weather_clean$zip_code)
#' Cloud cover variable is integer type, but better as a character type
#' due to collection of values as part of a scale.
weather_clean$cloud_cover <- as.character(weather_clean$cloud_cover)

# Continue with EDA.
status(weather_clean)
# No issues identified.

# freq() for character variables in weather data.
freq(weather_clean)
#' hist()) and profiling_num() for relevant numerical variables.
glimpse(weather_clean)
par(mfrow = c(1, 3))
hist(weather_clean$max_temperature_f)
hist(weather_clean$mean_temperature_f)
hist(weather_clean$min_temperature_f)
hist(weather_clean$max_visibility_miles)
hist(weather_clean$mean_visibility_miles)
hist(weather_clean$min_visibility_miles)
par(mfrow = c(1, 2))
hist(weather_clean$max_wind_speed_mph)
hist(weather_clean$mean_wind_speed_mph)
par(mfrow = c(1, 1))
hist(weather_clean$max_gust_speed_mph)
hist(weather_clean$precipitation_inches)
profiling_num(weather_clean)