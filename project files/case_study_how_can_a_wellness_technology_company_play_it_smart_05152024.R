library(tidyverse)
library(readr)
library(ggplot2)
library(here)
library(skimr)
library(janitor)
library(lubridate)
library(dplyr)
library(tidyr)
library(magrittr)
library(labeling)





#variables (x18) - original names
dailyactivity
dailycalories
dailyintensities
dailysteps
heartrate_seconds
hourlycalories
hourlyintensities
hourlysteps
minutecaloriesnarrow
minutecalorieswide
minuteintensitiesnarrow
minuteintensitieswide
minutemetsnarrow
minutesleep
minutestepsnarrow
minutestepswide
sleepday
weightloginfo





## Import all csv files and view each one
dailyactivity <- read.csv(file.choose())
View(dailyactivity)

dailycalories <- read.csv(file.choose("dailyCalories_merged.csv"))
View(dailycalories)
str(dailycalories)

dailyintensities <- read.csv(file.choose("dailyIntensities_merged.csv"))
View(dailyintensities)

dailysteps <- read.csv(file.choose("dailySteps_merged.csv"))
View(dailysteps)

heartrate_seconds <- read.csv(file.choose("heartrate_seconds_merged.csv"))
View(heartrate_seconds)

hourlycalories <- read.csv(file.choose("hourlyCalories_merged.csv"))
View(hourlycalories)
class(hourlycalories$ActivityHour)

hourlyintensities <- read.csv(file.choose("hourlyIntensities_merged.csv"))
View(hourlyintensities)

hourlysteps <- read.csv(file.choose("hourlySteps_merged.csv"))
View(hourlysteps)

minutecaloriesnarrow <- read.csv(file.choose())
View(minutecaloriesnarrow)

minutecalorieswide <- read.csv(file.choose())
View(minutecalorieswide)

minuteintensitiesnarrow <- read.csv(file.choose())
View(minuteintensitiesnarrow)

minuteintensitieswide <- read.csv(file.choose())
View(minuteintensitieswide)

minutemetsnarrow <- read.csv(file.choose())
View(minutemetsnarrow)

minutesleep <- read.csv(file.choose())
View(minutesleep)

minutestepsnarrow <- read.csv(file.choose())
View(minutestepsnarrow)

minutestepswide <- read.csv(file.choose())
View(minutestepswide)

sleepday <- read.csv(file.choose())
View(sleepday)

weightloginfo <- read.csv(file.choose())
View(weightloginfo)





## Check that each data set has the same number of unique Ids
n_unique(dailyactivity$Id)
n_unique(dailycalories$Id)
n_unique(dailyintensities$Id)
n_unique(dailysteps$Id)
n_unique(heartrate_seconds$Id)
n_unique(hourlycalories$Id)
n_unique(hourlyintensities$Id)
n_unique(hourlysteps$Id)
n_unique(minutecaloriesnarrow$Id)
n_unique(minuteintensitieswide$Id)
n_unique(minutemetsnarrow$Id)
n_unique(minuteintensitieswide$Id)
n_unique(minutemetsnarrow$Id)
n_unique(minutesleep$Id)
n_unique(minutestepsnarrow$Id)
n_unique(minutestepswide$Id)
n_unique(sleepday$Id)
n_unique(weightloginfo$Id)





## Check for duplicates
sum(duplicated(dailyactivity))
sum(duplicated(dailycalories))
sum(duplicated(dailyintensities))
sum(duplicated(dailysteps))
sum(duplicated(heartrate_seconds))
sum(duplicated(hourlycalories))
sum(duplicated(hourlyintensities))
sum(duplicated(hourlysteps))
sum(duplicated(minutecaloriesnarrow))
sum(duplicated(minutecalorieswide))
sum(duplicated(minuteintensitiesnarrow))
sum(duplicated(minuteintensitieswide))
sum(duplicated(minutemetsnarrow))
sum(duplicated(minutesleep))
sum(duplicated(minutestepsnarrow))
sum(duplicated(minutestepswide))
sum(duplicated(sleepday))
sum(duplicated(weightloginfo))





## Return records that are duplicates for datasets that returned duplicate rows (optional)
get_dupes(sleepday)
get_dupes(minutesleep)





# Remove duplicate rows from the datasets that returned dupes - create "cleaned", new variable names
sleep_day <- sleepday %>% 
  distinct() %>% 
  drop_na()

minutes_sleep <- minutesleep %>%
  distinct() %>% 
  drop_na()





# Test new variables and check that duplicate function returns "0" to confirm that duplicate row values were removed from
## the dataset
sum(duplicated(sleep_day))
sum(duplicated(minutes_sleep))





# Clean up column names (column names to lower case) and revise variable names (add underscores between words)
clean_names(dailyactivity)
dailyactivity <- rename_with(dailyactivity, tolower)
daily_activity <- dailyactivity
View(daily_activity)


clean_names(dailycalories)
dailycalories <- rename_with(dailycalories,tolower)
daily_calories <- dailycalories
View(daily_calories)

clean_names(dailyintensities)
dailyintensities <- rename_with(dailyintensities, tolower)
daily_intensities <- dailyintensities
View(daily_intensities)

clean_names(dailysteps)
dailysteps <- rename_with(dailysteps, tolower)
daily_steps <- dailysteps
View(daily_steps)

clean_names(heartrate_seconds)
heartrate_seconds <- rename_with(heartrate_seconds, tolower)
heart_rate_seconds <- heartrate_seconds
View(heart_rate_seconds)

clean_names(hourlycalories)
hourlycalories <- rename_with(hourlycalories, tolower)
hourly_calories <- hourlycalories
View(hourly_calories)

class(hourly_calories$activityhour)

clean_names(hourlyintensities)
hourlyintensities <- rename_with(hourlyintensities, tolower)
hourly_intensities <- hourlyintensities
View(hourly_intensities)

clean_names(hourlysteps)
hourlysteps <- rename_with(hourlysteps, tolower)
hourly_steps <- hourlysteps
View(hourly_steps)

clean_names(minutecaloriesnarrow)
minutecaloriesnarrow <- rename_with(minutecaloriesnarrow, tolower)
minute_calories_narrow <- minutecaloriesnarrow
View(minute_calories_narrow)

clean_names(minutecalorieswide)
minutecalorieswide <- rename_with(minutecalorieswide, tolower)
minute_calories_wide <- minutecalorieswide
minute_calories_wide <- minute_calories_wide %>% 
  rename(activityminute = activityhour)
View(minute_calories_wide)

clean_names(minuteintensitiesnarrow)
minuteintensitiesnarrow <- rename_with(minuteintensitiesnarrow, tolower)
minute_intensities_narrow <- minuteintensitiesnarrow
View(minute_intensities_narrow)

clean_names(minuteintensitieswide)
minuteintensitieswide <- rename_with(minuteintensitieswide, tolower)
minute_intensities_wide <- minuteintensitieswide
minute_intensities_wide <- minute_intensities_wide %>% 
  rename(activityminute = activityhour)
View(minute_intensities_wide)

clean_names(minutemetsnarrow)
minutemetsnarrow <- rename_with(minutemetsnarrow, tolower)
minute_mets_narrow <- minutemetsnarrow
View(minute_mets_narrow)

clean_names(minutes_sleep)
minutes_sleep <- rename_with(minutes_sleep, tolower)
minute_sleep <- minutes_sleep
View(minute_sleep)

clean_names(minutestepsnarrow)
minutestepsnarrow <- rename_with(minutestepsnarrow, tolower)
minute_steps_narrow <- minutestepsnarrow
View(minute_steps_narrow)

clean_names(minutestepswide)
minutestepswide <- rename_with(minutestepswide, tolower)
minute_steps_wide <- minutestepswide
minute_steps_wide <- minute_steps_wide %>% 
  rename(activityminute = activityhour)
View(minute_steps_wide)

clean_names(sleep_day)
sleep_day <- rename_with(sleep_day,tolower)
View(sleep_day)

clean_names(weightloginfo)
weightloginfo <- rename_with(weightloginfo, tolower)
weight_log_info <- weightloginfo
View(weight_log_info)





# Convert date columns so they are consistent across time stamps (i.e. "daily" data sets are consistent, "hourly" data sets are consistent, etc.)
daily_activity <- daily_activity %>% 
  rename(date = activitydate) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))
View(daily_activity)

sleep_day <- sleep_day %>% 
  rename(date = sleepday) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))
View(sleep_day)


hourly_calories <- hourly_calories %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))
View(hourly_calories)

class(hourly_calories$date_time)
hourly_intensities <- hourly_intensities %>% 
  rename(date_time = activityhour) %>%
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))
View(hourly_intensities)

hourly_steps <- hourly_steps %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))
View(hourly_steps)





# Join or merge data sets by "daily", "hourly", "minutes"


  #daily data sets
daily_sleep_activity_merged <- merge(daily_activity, sleep_day, by = c("id", "date"))
View(daily_sleep_activity_merged)


  #hourly data sets
hourly_step_intensities_calories <- merge( x= hourly_steps, 
                                           y = hourly_intensities,
                                           by.x = c("id", "date_time"),
                                           by.y = c("id", "date_time"),
                                           all = TRUE)

    #merge third "hourly" data set
hourly_step_intensities_calories <- merge(x = hourly_step_intensities_calories,
                                          y = hourly_calories,
                                          by.x = c("id", "date_time"),
                                          by.y = c("id", "date_time"),
                                          all = TRUE)
View(hourly_step_intensities_calories)


  #minutes narrow data sets
minutes_steps_mets_intensities_calories_narrow <- merge(x = minute_calories_narrow,
                                                        y = minute_intensities_narrow,
                                                        by.x = c("id", "activityminute"),
                                                        by.y = c("id", "activityminute"),
                                                        all = TRUE)

      #merge third "minutes narrow" data set
minutes_steps_mets_intensities_calories_narrow <- merge(x = minutes_steps_mets_intensities_calories_narrow,
                                                        y = minute_mets_narrow,
                                                        by.x = c("id", "activityminute"),
                                                        by.y = c("id", "activityminute"),
                                                        all = TRUE
                                                        )

      #merge fourth "minutes narrow" data set
minutes_steps_mets_intensities_calories_narrow <- merge(x = minutes_steps_mets_intensities_calories_narrow,
                                                        y = minute_steps_narrow,
                                                        by.x = c("id", "activityminute"),
                                                        by.y = c("id", "activityminute"),
                                                        all = TRUE)
View(minutes_steps_mets_intensities_calories_narrow)



  #minutes wide data sets  
minute_steps_intensities_calories_wide <- merge(x = minute_calories_wide,
                                                     y = minute_intensities_wide,
                                                     by.x = c("id", "activityminute"),
                                                     by.y = c("id", "activityminute"),
                                                     all = TRUE)

      #merge third "minutes wide" data set
minute_steps_intensities_calories_wide <- merge(x = minute_steps_intensities_calories_wide,
                                                y = minute_steps_wide,
                                                by.x = c("id", "activityminute"),
                                                by.y = c("id", "activityminute"),
                                                all = TRUE)
View(minute_steps_intensities_calories_wide)





# Summarize data sets (individual & merged)


  # "daily" data set summaries
daily_activity %>%
  summary()

sleep_day %>%
  summary()

daily_calories %>%
  select(calories) %>%
  summary()

daily_intensities %>%
  summary()

daily_steps %>%
  select(steptotal) %>%
  summary()


  # "hourly" data set summaries
hourly_calories %>%
  select(calories) %>%
  summary()

hourly_intensities %>%
  select(totalintensity,
         averageintensity) %>%
  summary()

hourly_steps %>%
  select(steptotal) %>%
  summary()

  # "minutes" data set summaries

      # "minutes narrow" data sets
minute_calories_narrow %>%
  select(calories) %>% 
  summary()

minute_intensities_narrow %>%
  select(intensity) %>% 
  summary()

minute_steps_narrow %>%
  select(steps) %>% 
  summary()

minute_mets_narrow %>%
  select(mets) %>% 
  summary()

    # "minutes wide" data set summaries
minute_calories_wide %>%
  summary()

minute_intensities_wide %>% 
  summary()

minute_steps_wide %>%
  summary()

  # summarize remaining data sets (heart rate seconds, weight log info)
heart_rate_seconds %>% 
  select(value) %>% 
  summary()

weight_log_info %>%
  select(weightkg,
         weightpounds,
         fat,
         bmi) %>% 
  summary()

        # extra summary data sets - groupings
daily_activity_summary_group <- daily_activity %>%
  group_by(id) %>%
  summarise(mean_calories = mean(calories),
            avg_steps = mean(totalsteps),
            avg_distance = mean(totaldistance))
View(daily_activity_summary_group)

           



# Visualize data sets (scatterplot, bar charts, line graphs, etc.)

  # daily steps vs calories - scatterplot
ggplot(data = daily_sleep_activity_merged, aes(x = totalsteps, y = calories)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Total Steps vs Calories per Day",
       x = "Steps per Day",
       y = "Calories per Day")

  # total minutes sleep per day of week - bar/column chart

        # add day of week names to "daily" data sets
weekday_sleep <- daily_sleep_activity_merged %>% 
  mutate(weekday = weekdays(date))
weekday_sleep$weekday <- ordered(weekday_sleep$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday"))
str(daily_sleep_activity_merged) 
View(weekday_sleep)
        # group total minutes of sleep per day by day of week
weekday_sleep_summary_group <- weekday_sleep %>% 
  group_by(weekday) %>% 
  summarise(daily_sleep = mean(totalminutesasleep),
            daily_steps = mean(totalsteps),
            daily_distance = mean(totaldistance),
            daily_bedtime = mean(totaltimeinbed),
            daily_calories= mean(calories)
            )
View(weekday_sleep_summary_group)

ggplot(data = weekday_sleep_summary_group, aes(x = weekday, y = daily_sleep/60)) +
  geom_col(fill = "blue") +
  labs(title = "Sleep per Day",
       x = "",
       y = "Hours")

ggplot(data = weekday_sleep_summary_group, aes(x = weekday, y = daily_steps)) +
  geom_col(fill = "pink") +
  labs(title = "Steps per Day",
       x = "",
       y = "Steps")

ggplot(data = weekday_sleep_summary_group, aes(x = weekday, y = daily_calories)) +
  geom_col(fill = "orange") +
  labs(title = "Calories per Day",
       x = "",
       y = "Calories")

ggplot(data = weekday_sleep_summary_group, aes(x = weekday, y = daily_distance)) +
  geom_col(fill = "green") +
  labs(title = "Distance per Day",
       x = "",
       y = "Distance (mi???)")

ggplot(data = weekday_sleep_summary_group, aes(x = weekday, y = daily_bedtime/60)) +
  geom_col(fill = "red") +
  labs(title = "Time Spent in Bed per Day",
       x = "",
       y = "Hours")

  # hourly steps vs intensities - scatterplot
ggplot(data = hourly_step_intensities_calories, aes(x = totalintensity, y = steptotal)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Intensity vs Steps per Hour",
       x = "Intensity",
       y = "Steps")





# For data sets containing datetime in their "time" referenced columns, split date and time into separate columns
hourly_intensities <- hourly_intensities %>% 
  separate(date_time, into = c("date", "time"), sep = " ")
View(hourly_intensities)

hourly_steps <- hourly_steps %>%
  separate(date_time, into = c("date", "time"), sep= " ") %>%
  mutate(date = ymd(date))
View(hourly_steps)

  # group by new "time"/hour column & drop rows containing missing values (time column will have missing values at the beginning of each day), and summarize avg intensity by time/hour
hourly_intensities <- hourly_intensities %>%
  group_by(time) %>% 
  drop_na() %>% 
  summarise(avg_intensity_per_hr = mean(totalintensity))

hourly_steps <- hourly_steps %>% 
  group_by(time) %>% 
  drop_na() %>% 
  summarise(avg_steps_per_hr = mean(steptotal))

  # make col chart graphing intensity levels by time/hour
ggplot(data = hourly_intensities, aes(x = time, y = avg_intensity_per_hr)) +
  geom_col(fill = "gray") +
  labs(title = "Average Intensity Per Hour",
       x = "Hour",
       y = "Intensity") +
  theme(axis.text.x = element_text(angle = 60))

ggplot(data = hourly_steps, aes(x = time, y = avg_steps_per_hr)) +
  geom_col(fill = "yellow") +
  labs(title = "Average Steps per Hour",
       x = "Hour",
       y = "Steps") +
  theme(axis.text.x = element_text(angle = 60))





# Group by "user type" (amount of usage from each user) using "case_when" function
daily_use <- daily_sleep_activity_merged %>% 
  group_by(id) %>% 
  summarise(days_used = sum(n())) %>% 
  mutate(user_type = case_when(
    days_used >=1 & days_used <= 10 ~ "low user",
    days_used >= 11 & days_used <= 20 ~ "moderate user",
    days_used >= 21 & days_used <= 31 ~ "high user"
  ))

View(daily_use)


  # Create bar chart to bucket the number of high, low, and moderate users
ggplot(data = daily_use, aes(x = user_type, fill = "coral")) +
  geom_bar() +
  scale_x_discrete(labels = c("low user", "moderate user", "high user")) + # "= c(...)" combines values into a vector or list
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14)) +
  labs(title = "User Type by Daily Activity (31 Days)",
       x = "",
       caption = "*Data recorded over a period of 31 days from 4/12/2016 - 5/12/2016")

  # Convert grouped data by user type and set to percentages
daily_use_percent <- daily_use %>% 
  group_by(user_type) %>% 
  summarise(total = n()) %>%  # n() = number of observations in a group
  mutate(totals = sum(total)) %>% 
  group_by(user_type) %>% 
  summarise(total_percent = total/totals) %>% 
  mutate(labels = scales::percent(total_percent))

View(daily_use_percent)

    # Make pie chart from daily_use_percent data
ggplot(data = daily_use_percent, aes(x = " ", y = total_percent, fill = user_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Smart Device Daily Use by User Type",
       caption = "*high user = 21 - 31 days\n moderate user = 11 - 20 days\n low user = 0 - 10 days") +
  theme_minimal() + # gets ride of background coloring
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  geom_text(aes(label = labels), # add label to pie chart (% numbers, variable = "labels")
            position = position_stack(vjust = 0.5)) #adjust positioning to center of polygon
