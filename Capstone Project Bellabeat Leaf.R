install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")
install.packages("here")
install.packages("httpgd")


library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(httpgd)


activity_df <- read_csv("dailyActivity_merged.csv")
##contains data for intensity, calories and steps
sleep_df <- read_csv("sleepDay_merged.csv")
hour_df <- read_csv("hourlySteps.csv")


##Assessing and Cleaning Data 
###Check participants 

n_distinct(sleep_df$Id)
#already can see not everyone uses product for sleep tracking.
n_distinct(activity_df$Id)


##Removing any Duplicate data 

sum(duplicated(activity_df))
sum(duplicated(sleep_df))


sleep_df <- sleep_df %>%
distinct() %>%
drop_na()


##can see that date is character type, need to convert using lubridate package

activity_df <- activity_df %>%
  rename(Date = ActivityDate) %>%
  mutate(Date = as_date(Date, format = "%m/%d/%Y"))

sleep_df <- sleep_df %>%
  rename(Date = SleepDay) %>%
  mutate(Date = as_date(Date,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))


##Column Types
glimpse(activity_df) # check for changes to each dataframe
glimpse(sleep_df)
##plotting

ggplot(data=activity_df) + 
aes(y=TotalSteps, x=Calories) +
geom_point() + 
geom_smooth()

#correlation of totalsteps v correlation 0.59
cor(activity_df$TotalSteps,activity_df$Calories)

#assess impact of activity on sleep quality

merged_sleep <- merge(activity_df,sleep_df,by=c('Id', 'Date'))
head(merged_sleep)

# no correlation between sleep and calories burned
ggplot(data=merged_sleep) + 
aes(x=TotalMinutesAsleep, y=Calories)+
geom_point() +
geom_smooth()

# looks to be correlation
ggplot(data=merged_sleep) + 
aes(x=TotalMinutesAsleep, y=SedentaryMinutes)+
geom_point() +
geom_smooth()

#negative correlation of 0.61.
cor(merged_sleep$SedentaryMinutes,merged_sleep$TotalMinutesAsleep)


#asessing user habits 
