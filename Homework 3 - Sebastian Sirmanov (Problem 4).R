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
most/least_miles <- not_cancelled %>%
  dplyr::group_by(tailnum) %>%
  dplyr::summarise(TotalDistance = base::sum(distance))  %>%
  dplyr::arrange(dplyr::desc(TotalDistance))%>%
  dplyr::slice(1:3, (n()-2):n())%>%
  dplyr::ungroup()

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
most/least_miles_company <- not_cancelled %>%
  dplyr::filter(month == 3) %>%
  dplyr::group_by(carrier)%>%
  dplyr::mutate(TotalMiles = base::sum(distance))%>%
  dplyr::ungroup()%>%
  dplyr::arrange(dplyr::desc(TotalMiles))%>%
  dplyr::slice(1, n())

#4.6
most_delays_month <-not_cancelled %>%
  dplyr::group_by(month)%>%
  dplyr::summarise(TotalDelay = base::sum(arr_delay>60))%>%
  dplyr::ungroup()%>%
  dplyr::slice_max(TotalDelay)

#4.7
average_time <- not_cancelled %>%
  dplyr::summarise(TimebetweenFlights = base::mean(dep_time - dplyr::lag(dep_time), na.rm = TRUE))%>%
  base::round(digits = 3)

#4.8
SDFunction <- function(inputVector){
  Denominator = sum(inputVector)/length(inputVector)
  Nominator = sum((inputVector - Denominator)^2)
  Result <- sqrt(sum((inputVector - Denominator)^2)/(length(inputVector)-1))
  return(Result)
}  

n <- not_cancelled %>%
  dplyr::group_by(month,dest) %>%
  dplyr::summarise(sdDelay = SDFunction(arr_delay))%>%
  dplyr::ungroup() 