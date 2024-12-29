# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Read the data
df <- read.csv("Data/ridership_headline.csv")

# Convert date to proper date format
df$date <- as.Date(df$date)

# Filter for Jan-Feb 2022 and add day type
penang_bus_2022 <- df %>%
  filter(date >= as.Date("2022-01-01") & date <= as.Date("2022-02-28")) %>%
  mutate(
    day_type = ifelse(wday(date) %in% c(1,7), "Weekend", "Weekday"),
    week = floor_date(date, "week"),
    day_name = weekdays(date)
  )

# Calculate daily averages by day type
daily_avg <- penang_bus_2022 %>%
  group_by(day_type) %>%
  summarise(
    avg_ridership = mean(bus_rpn, na.rm = TRUE),
    sd_ridership = sd(bus_rpn, na.rm = TRUE)
  )

# Create boxplot to show distribution
p1 <- ggplot(penang_bus_2022, aes(x = day_type, y = bus_rpn, fill = day_type)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Rapid Bus Penang Ridership",
       subtitle = "January-February 2022",
       x = "Day Type",
       y = "Number of Riders",
       fill = "Day Type") +
  scale_y_continuous(labels = scales::comma)

# Create line plot showing daily trends
p2 <- ggplot(penang_bus_2022, aes(x = date, y = bus_rpn, color = day_type)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Daily Rapid Bus Penang Ridership",
       subtitle = "January-February 2022",
       x = "Date",
       y = "Number of Riders",
       color = "Day Type") +
  scale_y_continuous(labels = scales::comma)

# Create bar plot showing average by day of week
p3 <- penang_bus_2022 %>%
  group_by(day_name) %>%
  summarise(avg_ridership = mean(bus_rpn, na.rm = TRUE)) %>%
  mutate(day_name = factor(day_name, levels = c("Monday", "Tuesday", "Wednesday", 
                                               "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  ggplot(aes(x = day_name, y = avg_ridership, fill = day_name)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Ridership by Day of Week",
       subtitle = "January-February 2022",
       x = "Day",
       y = "Average Number of Riders",
       fill = "Day") +
  scale_y_continuous(labels = scales::comma)

# Print summary statistics
print("Summary Statistics:")
print(daily_avg)

# Display all plots
print(p1)
print(p2)
print(p3)
