
install.packages(c("dplyr", "nycflights13"))

library(dplyr)
library(nycflights13)

people <- tibble(
  name = c("Alice", "Bob", "Charlie"),
  age  = c(25, 30, 35)
)

jobs <- tibble(
  name = c("Alice", "Bob", "David"),
  job  = c("Engineer", "Designer", "Analyst")
)

flights_joined <- flights %>%
  left_join(airlines, by = "carrier")

# check 
glimpse(flights_joined)

flights_joined %>%
  group_by(name) %>%
  summarize(mean_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(mean_arr_delay))

flights_dest <- flights %>%
  left_join(airports, by = c("dest" = "faa"))

# check
glimpse(flights_dest)

flights_dest %>%
  count(name, sort = TRUE)


semi_join(flights, airports, by = c("dest" = "faa"))

anti_join(flights, airports, by = c("dest" = "faa"))

a <- tibble(x = 1:4)
b <- tibble(x = 3:6)

union(a, b)     
intersect(a, b)  
setdiff(a, b)    

anti_join(flights, planes, by = "tailnum")

anti_join(planes, flights, by = "tailnum")

flights %>%
  left_join(planes, by = "tailnum") %>%
  group_by(manufacturer) %>%
  summarize(avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_delay))









