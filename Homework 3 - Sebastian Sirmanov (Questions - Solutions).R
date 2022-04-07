library(tidyverse)
library(nycflights13)

####Exercises 5.6.7####
###Question 1
delay_char <-
  flights %>%
  group_by(flight) %>%
  dplyr::summarise(n = n(),
            fifteen_early = mean(arr_delay == -15, na.rm = TRUE),
            fifteen_late = mean(arr_delay == 15, na.rm = TRUE),
            ten_always = mean(arr_delay == 10, na.rm = TRUE),
            thirty_early = mean(arr_delay == -30, na.rm = TRUE),
            thirty_late = mean(arr_delay == 30, na.rm = TRUE),
            percentage_on_time = mean(arr_delay == 0, na.rm = TRUE),
            twohours = mean(arr_delay > 120, na.rm = TRUE)) %>%
  map_if(is_double, round, 2) %>%
  as_tibble()

#A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time.
delay_char %>%
  dplyr::filter(fifteen_early == 0.5, fifteen_late == 0.5)

#A flight is always 10 minutes late.
delay_char %>%
  filter(ten_always == 1)

#A flight is 30 minutes early 50% of the time, and 30 minutes late 50% of the time.
delay_char %>%
  filter(thirty_early == 0.5 & thirty_late == 0.5)

#99% of the time a flight is on time. 1% of the time it’s 2 hours late.
delay_char %>%
  filter(percentage_on_time == 0.99 & twohours == 0.01)

#Which is more important: arrival delay or departure delay? - It depends.

###Question 2
not_cancelled <-
  flights %>% 
  dplyr::filter(!is.na(dep_delay), !is.na(arr_delay))


not_cancelled %>%
  group_by(dest) %>%
  summarise(n = n())

#and

not_cancelled %>%
  group_by(tailnum) %>%
  tally(wt = distance)

###Question 3
#Because if a flight didn’t leave then it was cancelled. 
#If the condition is.na(dep_delay) is met, then the flight was cancelled.

###Question 4
flights %>%
  group_by(day) %>%
  dplyr::summarise(cancelled = mean(is.na(dep_delay)),
            mean_dep = mean(dep_delay, na.rm = T),
            mean_arr = mean(arr_delay, na.rm = T)) %>%
  ggplot(aes(y = cancelled)) +
  geom_point(aes(x = mean_dep), colour = "red") +
  geom_point(aes(x = mean_arr), colour = "blue") +
  labs(x = "Avg delay per day", y = "Cancelled flights p day")

###Question 5
flights %>%
  summarise(n_car = n_distinct(carrier),
            n_air = n_distinct(dest),
            n_or = n_distinct(origin))

flights %>%
  group_by(carrier) %>%
  mutate(avg_carrier = mean(dep_delay, na.rm = T)) %>%
  group_by(carrier, origin) %>%
  mutate(origin_mean = mean(dep_delay, na.rm = T),
         deviations = origin_mean - avg_carrier) %>%
  summarise(deviations = mean(deviations), mean = mean(avg_carrier)) %>%
  ggplot(aes(origin, deviations)) + geom_col() + facet_wrap(~ carrier)

flights %>%
  group_by(carrier, dest) %>%
  summarise(mean_departure = mean(dep_delay, na.rm = T),
            mean_arrival = mean(arr_delay, na.rm = T))

###Question 6
flights %>%
  count(flight, sort = T)


####Exercises 5.7.1####
###Question 1
#It is applied to groups. 
#For example you can use mutate to create new variables by using group statistics specificly 
#or you can filter out an entire group.

###Question 2
flights %>%
  dplyr::filter(!is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  summarise(prop_time = sum(arr_delay <= 30)/n(),
            mean_arr = mean(arr_delay, na.rm = TRUE),
            fl = n()) %>%
  arrange(desc(prop_time))

###Question 3
flights %>%
  group_by(hour) %>%
  dplyr::filter(!is.na(dep_delay)) %>%
  dplyr::summarise( delay = mean( dep_delay > 0 , na.rm = T)) %>%
  ggplot(aes(hour, delay, fill = delay)) + geom_col() 

###Question 4
flights %>%
  group_by(dest) %>%
  dplyr::filter(!is.na(dep_delay)) %>%
  dplyr::summarise(tot_mins = sum(dep_delay[dep_delay > 0]))

flights %>%
  dplyr::filter(!is.na(dep_delay)) %>%
  group_by(tailnum, dest) %>%
  dplyr::summarise(m = mean(dep_delay > 0), n = n()) %>%
  arrange(desc(m))

###Question 5
flights %>%
  select(year, month, day, hour, dest, dep_delay) %>%
  group_by(dest) %>%
  dplyr::mutate(lag_delay = lag(dep_delay)) %>%
  arrange(dest) %>%
  dplyr::filter(!is.na(lag_delay)) %>%
  dplyr::summarize(cor = cor(dep_delay, lag_delay, use = "complete.obs"),
            n = n()) %>%
  arrange(desc(cor)) %>%
  dplyr::filter(row_number(desc(cor)) %in% 1:10)

###Question 6
# (1)
flights %>%
  group_by(dest) %>%
  arrange(air_time) %>%
  slice(1:5) %>%
  select(tailnum, sched_dep_time, sched_arr_time, air_time) %>%
  arrange(air_time)

# (2)
flights %>%
  group_by(dest) %>%
  dplyr::mutate(shortest = air_time - min(air_time, na.rm = T)) %>%
  top_n(1, air_time) %>%
  arrange(-air_time) %>%
  select(tailnum, sched_dep_time, sched_arr_time, shortest)

###Question 7
flights %>%
  group_by(dest) %>%
  dplyr::filter(n_distinct(carrier) > 2) %>%
  group_by(carrier) %>%
  dplyr::summarise(n = n_distinct(dest)) %>%
  arrange(-n)

###Question 8
not_cancelled %>%
  group_by(origin, tailnum) %>%
  dplyr::summarise(
    count = n(),
    agg_dep_delay = sum(cumsum(dep_delay > 60) < 1)
  )