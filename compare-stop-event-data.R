library(tidyverse)
library(ggthemes)


stop_event_data <- read_rds("data/stop_events.rds") 

# I first created an object to store the route description. You can do that in many different ways, I chose to use "distinct" to get all the unique descriptions. If we had more than one, we could access these names using its index.

route_name <- stop_event_data |> 
  distinct(public_route_description)

stop_event_data |> 
  ggplot(mapping=aes(x=total_trip_time_seconds,fill=public_direction_description)) +
  geom_histogram(bins=10, show.legend = FALSE) +
  facet_grid(cols = vars(public_direction_description), rows = vars(service_date)) +
  theme_minimal(base_size=14) +
  labs(title = "Trip Duration In Seconds By Directions By Date",
       subtitle = str_glue("Route {route_name}"), # here's your trick!
       x=NULL, y=NULL, fill=NULL)


passenger_boardings_by_date <- stop_event_data |> 
  group_by(route_number, public_route_description, service_date) |> 
  summarize(total_boardings=sum(total_boardings)) 

ggplot(data=passenger_boardings_by_date,
       mapping=aes(x=reorder(service_date,desc(total_boardings)), y=total_boardings,
                   fill=service_date)) +
  geom_col(show.legend = FALSE) +
  labs(title="Total Passenger Boardings By Date",
       subtitle = "Route [Insert Route Description Here]",x=NULL,y=NULL) +
  theme_fivethirtyeight(base_size=14)
