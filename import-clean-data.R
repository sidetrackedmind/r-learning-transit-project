library(tidyverse)
library(janitor)
library(fs)
library(glue)

# Create Directories ------------------------------------------------------

dir_create("data-raw")
dir_create("data")


# Create function to import and clean raw data ---------------------------------------------------------

test <- read_csv("data-raw/stop_event_sample_20230518.csv") |> 
  clean_names()

load_clean_stop_event <- function(date_str) {
  
  read_csv(glue("data-raw/stop_event_sample_{date_str}.csv")) |> 
      clean_names() |> 
      mutate(direction_char = as.factor(direction)) |> 
      group_by(vehicle_number, route_number, direction_char, trip_number) |> 
      summarize(total_trip_time_seconds=max(arrive_time)-min(leave_time),
                total_boardings=sum(ons)) |> 
      ungroup() |> 
      mutate(service_date=date_str) |> 
      select(-vehicle_number) |> 
      arrange(desc(total_trip_time_seconds))
  
}


# Import clean bind and merge datasets ------------------------------------

# Import data describing route number and directions
route_direction_definitions <- read_csv("data-raw/route_direction_definitions.csv")|> 
  clean_names() |> 
  mutate(direction_char = as.factor(direction)) 

# Import stop event data samples
stop_event_20230518 <- load_clean_stop_event("20230518")
stop_event_20230615 <- load_clean_stop_event("20230615")

stop_events <-
  bind_rows(stop_event_20230518,
            stop_event_20230615) |> 
  left_join(route_direction_definitions,
            join_by(route_number, direction_char))

stop_events |> 
write_rds("data/stop_events.rds")


