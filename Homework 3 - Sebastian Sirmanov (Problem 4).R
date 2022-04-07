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
library(nycflights13)
View(flights)

#4.1
common_dest <- flights %>% 
  group_by(carrier, dest) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)

#4.2
biggest_dep_delay <- flights %>% 
  group_by(carrier, dep_delay) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)

biggest_arr_delay <- flights %>% 
  group_by(carrier, arr_delay) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)

#4.3


#4.4


#4.5


#4.6


#4.7


#4.8