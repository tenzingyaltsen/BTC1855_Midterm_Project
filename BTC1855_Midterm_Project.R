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