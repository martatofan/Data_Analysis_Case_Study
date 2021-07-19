library(tidyverse)
library(readr)

daily_activity <- read.csv("/Users/martatofan/Desktop/Casestudy/dailyActivity_merged.csv")
sleep_day <- read.csv("/Users/martatofan/Desktop/Casestudy/sleepDay_merged.csv")

head(daily_activity)
colnames(daily_activity)

head(sleep_day)
colnames(sleep_day)

# How many unique participants are there in each dataframe?
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)

# How many observations are there in each dataframe?
  
nrow(daily_activity)
nrow(sleep_day)

# Quick summary statistics about each data frame

daily_activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>%
  summary()

sleep_day %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()

# Relationship between Total steps per day and Sedentary minutes

ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes)) + geom_point()

# Relationship between minutes asleep and time in bed

ggplot(data=sleep_day, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point()
ggplot(data=sleep_day, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point()

# Merging these two datasets together

combined_data <- merge(sleep_day, daily_activity, by="Id")

# Number of participants in this dataset
n_distinct(combined_data$Id)
