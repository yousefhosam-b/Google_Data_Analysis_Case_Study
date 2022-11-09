# Import necessary libraries
library(tidyverse)
library(lubridate)
library(ggplot2)            
library(dplyr)              
library(skimr) 
library(sqldf)
library(janitor)
library(plotrix) 


# Importing the data
daily_activity <- read_csv("dailyActivity_merged.csv")
sleep_per_day <- read_csv("sleepDay_merged.csv")
weight_log_info <- read_csv("weightLogInfo_merged.csv")


# --------------------- Quick Analysis of Data --------------------- 

# Daily Activity Dataset
str(daily_activity)
skim(daily_activity)
head(daily_activity)
glimpse(daily_activity)

# Sleep Per Day Dataset
str(sleep_per_day)
skim(sleep_per_day)
head(sleep_per_day)
glimpse(sleep_per_day)

# Weight Log Info Dataset
str(weight_log_info)
skim(weight_log_info)
head(weight_log_info)
glimpse(weight_log_info)


# Data Preparation
daily_activity$Rec_Date <- as.Date(daily_activity$ActivityDate,"%m/%d/%y")
daily_activity$month <- format(daily_activity$Rec_Date,"%B")
daily_activity$day_of_week <- format(daily_activity$Rec_Date,"%A")

n_distinct(daily_activity$Id)


# Data Summary 
daily_activity %>%  select(TotalSteps,TotalDistance,SedentaryMinutes,VeryActiveMinutes) %>% summary()
weight_log_info %>%  select(WeightKg,BMI) %>% summary()


# Find the average sleeping time in minutes
Avg_minutes_asleep <- sqldf("SELECT SUM(TotalSleepRecords),SUM(TotalMinutesAsleep)/SUM(TotalSleepRecords) as avg_sleeptime
                            FROM sleep_per_day")
Avg_minutes_asleep

# Find the average bed time in minutes
Avg_TimeInBed <- sqldf("SELECT SUM(TotalTimeInBed)/SUM(TotalSleepRecords) as avg_timeInBed
                       FROM sleep_per_day")
Avg_TimeInBed

n_distinct(sleep_per_day$Id)
n_distinct(weight_log_info$Id)


# --------------------- Data Visualizations --------------------- 
daily_activity$day_of_week <- ordered(daily_activity$day_of_week,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

ggplot(data=daily_activity) + geom_bar(mapping = aes(x=day_of_week),fill="orange") +
  labs(x="Day of week",y="Count",title="No. of times users used tracker across week")


mean_steps <- mean(daily_activity$TotalSteps)
mean_calories <- mean(daily_activity$Calories)
mean_steps
mean_calories


# Calories burned for every step taken
ggplot(data=daily_activity) + geom_point(mapping=aes(x=TotalSteps, y=Calories, color=Calories)) +
  geom_hline(mapping = aes(yintercept=mean_calories),color="yellow",lwd=1.0) +
  geom_vline(mapping = aes(xintercept=mean_steps),color="red",lwd=1.0) +
  geom_text(mapping = aes(x=10000,y=500,label="Average Steps",srt=-90)) +
  geom_text(mapping = aes(x=29000,y=2500,label="Average Calories")) +
  labs(x="Steps Taken",y="Calories Burned",title = "Calories burned for every step taken")


# Total Steps vs Sedentary Minutes
ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes, color = Calories)) + geom_point() +
  geom_smooth(method = "loess",color="green") + 
  labs(x="Total Steps",y="Sedentary Minutes",title="Total Steps vs Sedentary Minutes")


# Sleep Time vs Time in Bed
ggplot(data=sleep_per_day, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point() + stat_smooth(method = lm) +
  labs(x="Total Minutes a sleep", y="Total Time in Bed", title = "Sleep Time vs Time in Bed")


# Very Active Minutes vs Calories Burned
ggplot(data=daily_activity,aes(x = VeryActiveMinutes, y = Calories, color = Calories)) + geom_point() + 
  geom_smooth(method = "loess",color="orange") +
  labs(x="Very Active Minutes",y="Calories",title = "Very Active Minutes vs Calories Burned")


# Calories vs. Sedentary Minutes
ggplot(data=daily_activity,aes(x=SedentaryMinutes,y=Calories,color=Calories)) + geom_point() + 
  geom_smooth(method="loess",color="red") + 
  labs(y="Calories", x="Sedentary Minutes", title="Calories vs. Sedentary Minutes")


# Calculating the sum of individual minute
activity_min <- sqldf("SELECT SUM(VeryActiveMinutes),SUM(FairlyActiveMinutes),
      SUM(LightlyActiveMinutes),SUM(SedentaryMinutes)
      FROM daily_activity")
activity_min


# Percentage of Activity in Minutes
x <- c(19895,12751,181244,931738)
piepercent <- round(100*x / sum(x), 1)
colors = c("red","blue","green","yellow")
pie3D(x,labels = paste0(piepercent,"%"),col=colors,main = "Percentage of Activity in Minutes")
legend("bottomright",c("VeryActiveMinutes","FairlyActiveMinutes","LightlyActiveMinutes","SedentaryMinutes"),cex=0.5,fill = colors)


# Calculating the sum of different distance values
activity_dist <- sqldf("SELECT SUM(ModeratelyActiveDistance),SUM(LightActiveDistance),
      SUM(VeryActiveDistance),SUM(SedentaryActiveDistance)
      FROM daily_activity")
activity_dist


# # Percentage of Activity in Minutes
y <- c(533.49,3140.37,1412.52)
piepercent <- round(100*y / sum(y), 1)
colors = c("orange","green","blue")
pie3D(y,labels = paste0(piepercent,"%"),col=colors,main = "# Percentage of Activity in Minutes
")
legend("bottomright",c("ModeratelyActiveDistance","LightlyActiveDistance","VeryActiveDistance"),cex=0.5,fill = colors)



# calculating the count of people with over weight
count_overweight <- sqldf("SELECT COUNT(DISTINCT(Id))
                          FROM weight_log_info
                          WHERE BMI > 24.9")
count_overweight


# Percentage of people with Over Weight vs Healthy Weight
z <- c(5,3)
piepercent <- round(100*z / sum(z),1)
colors = c("red","green")
pie3D(z,labels=paste0(piepercent,"%"),explode=0.1,col=colors,radius=1,main="Percentage of people with Over Weight 
     vs Healthy Weight")
legend("bottomright",c("OverWeight","HealthyWeight"),cex=0.5,fill=colors)