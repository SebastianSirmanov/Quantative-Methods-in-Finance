#####Problem 4#####
#Find the following:
#4.1 For each carrier what is the most common destination?
#4.2 For each carrier what is the biggest delay?
#4.3 Which are the three plane which have flown the most/least miles?
#4.4 What are the first/last flights for each day in February 2013?
#4.5 Which company flew the most miles in March 2013? Which flew the least?
#4.6 Which month had the most delays over 60 minutes?
#4.7 What is the average time between two consecutive flights?
#4.8 Use the SDFunction function from exercise 2 to calculate the standard deviation
#of the flight delays for each month and for each destination.

#####Problem 4 - Solution#####
library(tidyverse)
library(tidyquant)
library(nycflights13)
View(flights)

#4.1
common_dest <- flights %>% 
  group_by(carrier, dest) %>%
  select(carrier, dest) %>%
  summarise(NrOfFlights = n()) %>%
  arrange(desc(NrOfFlights)) %>%
  slice_head()

#4.2
biggest_delay <- flights %>%
  arrange(desc(arr_delay)) %>%
  group_by(carrier) %>%
  slice_head()

#4.3


#4.4
sss <- flights %>%
  filter(year == 2013,
         month == 2,
         !is.na(dep_time)) %>%
  arrange(day, dep_time) %>%
  group_by(day) %>%
  filter(row_number() == 1 |
         row_number() == n()) %>%
  ungroup()

#4.5


#4.6


#4.7


#4.8
