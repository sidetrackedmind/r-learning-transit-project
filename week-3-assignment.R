# Load Packages -----------------------------------------------------------

# Load the tidyverse and ggthemes package

library(tidyverse)
library(ggthemes)

# Import Data -------------------------------------------------------------

vehicle_data <- read_csv("stop_event_sample2.csv")

# group by vehicle_number, route_number, direction, and trip_number
# to calculate the total_trip_time in seconds

trip_durations <- vehicle_data |> 
  mutate(DIRECTION_CHAR = as.factor(DIRECTION)) |> 
  group_by(VEHICLE_NUMBER, ROUTE_NUMBER, DIRECTION_CHAR, TRIP_NUMBER) |> 
  summarize(TOTAL_TRIP_TIME_SECONDS=max(ARRIVE_TIME)-min(LEAVE_TIME)) |> 
  ungroup() |> 
  arrange(desc(TOTAL_TRIP_TIME_SECONDS))

# plot a histogram of the various trip times by direction
# use facet rows so it's easy-ish to compare
# it looks like direction 1 is a little slower (longer trip times)
# I was worried about adding too much data to github but I think the 
# histogram would look better with more data :)


ggplot(data=trip_durations,
       mapping=aes(x=TOTAL_TRIP_TIME_SECONDS,fill=DIRECTION_CHAR)) +
  geom_histogram(bins=10) +
  facet_grid(rows = vars(DIRECTION_CHAR)) +
  theme_minimal() + 
  labs(title = "Trip Duration In Seconds By Directions",
       subtitle = "Route 2 - May 18th, 2023",
       x=NULL, y=NULL, fill="Direction")

passenger_boardings_by_trip <- vehicle_data |> 
                                  group_by(ROUTE_NUMBER, TRIP_NUMBER) |> 
                                  summarize(TOTAL_ONS=sum(ONS)) 

ggplot(data=passenger_boardings_by_trip,
       mapping=aes(x=reorder(TRIP_NUMBER,desc(TOTAL_ONS)), y=TOTAL_ONS)) +
  geom_col() +
  labs(title="Total Passenger Boardings By Trip Number",
       subtitle = "Route 2 - May 18th, 2023"
       ,x=NULL,y=NULL) + 
  theme_fivethirtyeight()

