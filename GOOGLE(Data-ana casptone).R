`{r libraries, echo=TRUE, eval=TRUE}
library(tidyverse)
library(janitor)
library(skimr)
library(here)
library(hablar)
library(readxl)
library(data.table)
library(chron)
library(readr)
library(lubridate)
library(magrittr)
library(DescTools)
library(metR)
```
#### Importing data 
Cyclist data from 01/2021 until 12/2021 is imported and read as csv. files. 

```{r csv data collection, echo=TRUE, eval=FALSE}
data_01 <- read_csv("case study/raw data/2021_01.csv")
data_02 <- read_csv("case study/raw data/2021_02.csv")
data_03 <- read_csv("case study/raw data/2021_03.csv")
data_04 <- read_csv("case study/raw data/2021_04.csv")
data_05 <- read_csv("case study/raw data/2021_05.csv")
data_06 <- read_csv("case study/raw data/2021_06.csv")
data_07 <- read_csv("case study/raw data/2021_07.csv")
data_08 <- read_csv("case study/raw data/2021_08.csv")
data_09 <- read_csv("case study/raw data/2021_09.csv")
data_10 <- read_csv("case study/raw data/2021_10.csv")
data_11 <- read_csv("case study/raw data/2021_11.csv")
data_12 <- read_csv("case study/raw data/2021_12.csv")
```

Compare column names each of the files. While the names don't have to be in the same order, they DO need to match perfectly before they are  joined  into one file.

```{r colname inspection, echo=TRUE, eval=TRUE}
colnames(data_01)
colnames(data_02)
colnames(data_04)
colnames(data_05)
colnames(data_06)
colnames(data_07)
colnames(data_08)
colnames(data_09)
colnames(data_10)
colnames(data_11)
colnames(data_12)
```

Afterwards, the 12 data sets are combined into one big data frame of 5595028 observations.

```{r stacking the datasets , echo=TRUE, eval=TRUE}
all_trips <- bind_rows(data_01, data_02, data_03, data_04, data_05, 
                      data_06, data_07, data_08, data_09, data_10, data_11, data_12)
```

####  Clean up and organize data to prepare for analysis

Inspect the new table that has been created.

```{r all_trips inspection, echo=TRUE, eval=TRUE}
colnames(all_trips)  #List of column names
dim(all_trips)  #Dimensions of the data frame
head(all_trips)  #See the first 6 rows of data frame
str(all_trips)  #See list of columns and data types (numeric, character, etc)
```

Then,  columns that list the date, month, day, day_of_week and year of each ride are added. days of the week are assigned the numbers 1:Monday, 2:Tuesday, etc.
This will allow the aggregation of the data by each day, month or day_of_week.

```{r separting ride date and extracting date data, echo=TRUE, eval=TRUE}
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%u") #"%A" would deliver names of weekdays
```


Add a "ride_length" calculation to all_trips in seconds and in minutes (2 new columns).

```{r calulate the ride_length in secs, echo=TRUE, eval=TRUE}
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
all_trips$ride_length_m <- (difftime(all_trips$ended_at,all_trips$started_at))/60

```


Inspect the structure of the newly added columns.

```{r data inspection, echo=TRUE, eval=TRUE}
str(all_trips)
```


Convert c(ride_length, ride_length_m, day and month) to numeric so that calculation can be executed.

```{r converting variables to numeric, echo=TRUE, eval=TRUE}
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
all_trips$ride_length_m <- as.numeric(as.character(all_trips$ride_length_m))
all_trips$month <- as.numeric(all_trips$month)
all_trips$day <- as.numeric(all_trips$day)
is.numeric(all_trips$ride_length)
is.numeric(all_trips$ride_length_m)
is.numeric(all_trips$month)
is.numeric(all_trips$day)
```


After converting and inspecting data, it was noticed that col:ride_length has some negative values, probably because start_time and end_time were swapped for these rides, or the system simply registered and recorded the rides incorrectly. So, negative-seconds rides must be excluded.

```{r droppig rows, echo=TRUE, eval=TRUE}
all_trips_v1 <- all_trips[!( all_trips$ride_length < 0),]
```


#### First analysis step: descriptive analysis on ride length

First of all, a descriptive analysis on ride length [min] is performed.

```{r data statistical summary, echo=TRUE, eval=TRUE, error=TRUE}
all_trips_v1 %>% 
  summarise(max(ride_length_m),min(ride_length_m),mean(ride_length_m)) 
```

The overall average ride length is 21.9 minutes. 


Second, the mode of weekday is calculated, namely which weekday occurs the most or on which weekday most bike are rented. 

```{r plot1: mode of the week_day, echo=TRUE, eval=TRUE, error=TRUE}
  all_trips_v1 %>% 
  group_by(day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(mapping = aes(x = day_of_week, y = number_of_rides)) + geom_col()
```

The plot shows that most rides were started on Saturday(991047 rides), Saturday(857285 rides) followed by Friday(810508 rides). So most bikes are rented on the weekend. 


Next, a plot of the ride_length or average_duration in minutes for every day of the week for members and casual riders is shown. 

```{r plot2: ride_length per day per rider type, echo=TRUE, eval=TRUE, error=TRUE}
all_trips_v1 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_m)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
```

The plot demonstrates that casual riders rent bikes for longer durations, especially on Sunday, Saturday,  Friday (weekend) and on Monday. Members show a steady riding/using behavior, plus they also tend to ride a little longer on the weekend.  


Here, number of rides per day for every rider type is plotted. 

```{r plot3: number_of_rides per day per rider type, echo=TRUE, eval=TRUE, error=TRUE}
all_trips_v1 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_m)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
```

Surprisingly, and in contrast to the former plot, members begin more rides and thus have higher number of rides on every day of the week except for Saturday and Sunday. 


#### Second analysis step: average ride length depending on rider type and number of each rider type


```{r plot4: mean ride lenght, echo=TRUE, eval=TRUE, error=TRUE}
all_trips_v1 %>%
  group_by(member_casual) %>%
  summarise(max(ride_length_m), min(ride_length_m),avg_ride_length = mean(ride_length_m)) %>% 
  ggplot(aes(x = member_casual, y = avg_ride_length,fill=member_casual)) +
  geom_col()+ scale_y_continuous(breaks = seq(0, 40, by = 5))
```

So, the result shows that casual riders tend to rent bikes for longer mean durations than members (32 min to 13.6 min), in accordance with plot 2. Members probably use bikes to commute, whereas casual riders maybe, among other things, exercising, visiting the city or attending special events.


Here, the overall rider count based on rider type is plotted

```{r plot5: overall rider count by rider type, echo=TRUE, eval=TRUE, error=TRUE}
all_trips_v1 %>%
  group_by(member_casual) %>%
  summarise(rider_count = n()) %>% 
  ggplot(aes(x = member_casual, y = rider_count,fill=member_casual )) +
  geom_col()
```

The plot indicates that more than half of all riders are member riders (3/(3+2.5)=0,55)


#### Third analysis step: Exploring  effect of  seasonality 

Here, the Function "season" of the library "metR" was used to assign season to months:

DJF:winter

MAM:Spring

JJA:Summer

SON:Fall


```{r assigning season to months} 
all_trips_v1$season <- season(all_trips_v1$month)
```

First, let us consider number of rides and ride length by weekday on each season

```{r plot6: number of rides by week_day and rider type and season, echo=TRUE, eval=TRUE, error=TRUE}
all_trips_v1%>%
  group_by(season, day_of_week, member_casual) %>%   
  summarise(number_of_rides = n()						 
  ,avg_ride_length = mean(ride_length_m)) %>% 
ggplot() + geom_col(mapping = aes(x = day_of_week, y = number_of_rides, fill = member_casual), position = "dodge") + facet_wrap(~season) + scale_y_continuous(breaks = seq(0, 400000, by = 50000))
```

```{r plot7: ride_lenght by week_day and rider type and season, echo=TRUE, eval=TRUE, error=TRUE}
all_trips_v1%>%
  group_by(season, day_of_week, member_casual) %>%   
  summarise(number_of_rides = n()						 
  ,avg_ride_length = mean(ride_length_m)) %>% 
ggplot() + geom_col(mapping = aes(x = day_of_week, y = avg_ride_length, fill = member_casual), position = "dodge") + facet_wrap(~season) + scale_y_continuous(breaks = seq(0, 50, by = 10))
```

Plot 6 tells us that number of rides of members is always higher than that of casual riders on every  work day in every season. Weekends are still the time where casual riders bike more than members. The only exception of this trend is in the winter months (Dec, Jan, Feb).
Plot 7 demonstrates that the member group has all year long the average ride length of about 13.6 minutes. Casual riders use bikes  about half an hour long on all days in spring and summer. In winter and fall, the average ride lengths becomes less than 30 minutes. 


Lastly, let us generate a line plot for continuous change of number of rides along the whole year for the two rider types.

```{r plot8: number of rides along the whole year}
all_trips_v1%>%
  group_by(month, member_casual) %>%   
  summarise(number_of_rides = n()						 
  ,avg_ride_length = mean(ride_length_m)) %>% 
ggplot() + geom_line(mapping = aes(x = month, y = number_of_rides, color = member_casual)) + scale_x_continuous(breaks = seq(1, 12, by = 1))
```

##The plot indicates, for casual riders/members, ridership peaked around July/August (Summer months being the turning point) and hit the lowest at February before rebounding up swiftly and continuously. 



