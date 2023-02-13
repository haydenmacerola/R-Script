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
hour_df <- read_csv("hourlySteps_merged.csv")



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

hour_df <- hour_df %>%
  rename(datetime = ActivityHour) %>%
  mutate(datetime = as.POSIXct(datetime, format="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))
##Column Types
glimpse(activity_df) #check for changes to each dataframe
glimpse(sleep_df)
glimpse(hour_df)
##plotting

ggplot(data=activity_df) + 
aes(y=TotalSteps, x=Calories) +
geom_point() + 
geom_smooth() +
labs(title="TotalSteps v Calories Burned") +


#correlation of totalsteps v calories 0.59
cor(activity_df$TotalSteps,activity_df$Calories)


#Assess impact of activity on sleep quality

merged_sleep <- merge(activity_df,sleep_df,by=c('Id', 'Date'))
head(merged_sleep)

#no correlation between sleep and calories burned
ggplot(data=merged_sleep) + 
aes(x=TotalMinutesAsleep, y=Calories)+
geom_point() +
geom_smooth()

#looks to be correlation between sedentary minutes and sleep
ggplot(data=merged_sleep) + 
aes(x=TotalMinutesAsleep, y=SedentaryMinutes)+
geom_point() +
geom_smooth()

#negative correlation of 0.61.
cor(merged_sleep$SedentaryMinutes,merged_sleep$TotalMinutesAsleep)


#asessing user habits 
#split date_time into days and hours
hour_df <- hour_df %>%
  separate(datetime, into =c("date", "time"), sep=" ") %>%
  mutate(date = ymd(date))
head(hour_df)


hour_df %>%
  group_by(time) %>%
  summarize(average_steps = mean(StepTotal)) %>%
  ggplot()+
  geom_col(aes(x=time, y= average_steps, fill = average_steps)) +
  labs(title = "Steps taken each hour of the day", x="", y="")+
  scale_fill_gradient(low = "grey", high = "blue") +
  theme(axis.text.x = element_text(angle = 90))

  #most active after tradional work hours 5-7pm 
  #spike also around lunch time

intensity <- activity_df %>% 
  select(Id,VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  group_by(Id) %>%
  pivot_longer(VeryActiveMinutes:LightlyActiveMinutes, names_to = "Activity_Level", values_to = "minutes") 


ggplot(data=intensity) +
 geom_col(aes(x=Activity_Level, y=minutes, colour=Id)) +
 labs(title="Exercise Intensity Distribution")

#Clear disposition to light exercise over very, or fairly intense exercise
  
#Key Reccomednations

##Marketing Campaign 

##Target the bellabeat leaf to the corporate women. 
##Based on activity time spiking at lunch and after 5pm, coupled with 
##sedentary minutes being very high outside these hours and exercise being traditonally light
##Continue pushing the stylish fitbit narrative, consumers are everyday corporate women
##that do whats necessary to stay "healthy"

##In-app Reccomendations

##Send out notifications for approx 6pm to encourage daily use 
##Highlight the impact of exercise on sleep, encoruage frequent moving breaks from work


