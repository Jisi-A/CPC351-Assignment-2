# Using appropriate visuals, show the number of trips
# aggregated for each month for all the 13
# public transport services across the country.

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Read the data
df <- read.csv("Data/ridership_headline.csv")

# Convert date to proper date format
df$date <- as.Date(df$date)

# Create month column with year and month only
df$month <- format(df$date, "%Y-%m")

# Aggregate data by month for all services
# %>% pass the result of one function to the next function
# group_by: group the data by month
# summarise: summarise the data by month
# across: apply the sum function to the columns that start with bus_ or rail_
# pivot_longer: pivot the data to long format
monthly_trips <- df %>%
  group_by(month) %>%
  summarise(across(starts_with(c("bus_", "rail_")), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = -month, 
              names_to = "service", 
              values_to = "trips")

ggplot(monthly_trips, aes(x = month, y = trips/1000000, fill = service)) +  # Convert to millions
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Reduced angle to 45
    legend.position = "right",
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  ) +
  labs(
    title = "Monthly Public Transport Ridership",
    subtitle = "Aggregated trips by service type",
    x = "Month",
    y = "Number of Trips (Millions)",
    fill = "Transport Service"
  ) +
  scale_y_continuous(
    labels = scales::comma_format(scale = 1)
  ) +
  scale_x_discrete(
    breaks = function(x) x[seq(1, length(x), by = 3)]  # Show every 3rd month
  )