# import libraries
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(scales)

# get password
route_72_standing_room <- read_rds("route_72_standing_room_20230827_20230930.rds")

route_name <- route_72_standing_room |> 
  distinct(PUBLIC_ROUTE_DESCRIPTION)


# the graph is not sorting by time
# note the TRIP_BEGIN_TIME is in seconds and it includes values > 86300
# transit does a weird thing where late night service is considered the same 
# service date even though it's technically the next day at 1am etc
route_72_standing_room |> 
  group_by(PUBLIC_ROUTE_DESCRIPTION, PUBLIC_DIRECTION_DESCRIPTION,TRIP_BEGIN_TIME, TRIP_BEGIN_TIME_STR) |> 
  summarise(
    STANDING_ROOM_INSTANCES=max(STANDING_ROOM_COUNT),
    NUM_TRIPS=max(NUM_TRIPS)
  ) |> 
  ungroup()  |> 
  mutate(PERCENT_OF_TOTAL_TRIPS=STANDING_ROOM_INSTANCES/NUM_TRIPS,
         TRIP_BEGIN_TIME_NUM=as.double(TRIP_BEGIN_TIME)) |> 
  mutate(TRIP_BEGIN_TIME_STR=fct_reorder(TRIP_BEGIN_TIME_STR, TRIP_BEGIN_TIME_NUM)) |>
  ggplot(aes(x=PERCENT_OF_TOTAL_TRIPS
             , y= TRIP_BEGIN_TIME_STR
  )) + 
  geom_col() +
  labs(title="Percent of Trips with Standing Room Only By Trip Start Time",
       # trick help from Gracielle
       subtitle = str_glue("{route_name}"),x=NULL,y=NULL) + 
  scale_x_continuous(labels= percent)