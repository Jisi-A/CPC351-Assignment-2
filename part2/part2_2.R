# Load required libraries
library(dplyr)
library(ggplot2)

# Read the data
df <- read.csv("Data/ridership_headline.csv")

# Convert date to proper date format
df$date <- as.Date(df$date)

# Filter for 2023 data and get top/bottom 10 days for bus_rkl
bus_rkl_2023 <- df %>%
  filter(format(date, "%Y") == "2023") %>%
  arrange(desc(bus_rkl)) %>%
  select(date, bus_rkl)

# Get top 10 and bottom 10
top_10 <- head(bus_rkl_2023, 10)
bottom_10 <- tail(arrange(bus_rkl_2023, bus_rkl), 10)

# Combine the results
combined_results <- bind_rows(
  mutate(top_10, category = "Highest Ridership"),
  mutate(bottom_10, category = "Lowest Ridership")
)

# Create a plot
ggplot(combined_results, aes(x = reorder(format(date, "%Y-%m-%d"), bus_rkl), 
                            y = bus_rkl, 
                            fill = category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~category, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Rapid Bus (KL) Ridership Extremes in 2023",
       x = "Date",
       y = "Number of Riders",
       fill = "Category") +
  scale_y_continuous(labels = scales::comma)

# Print the results as a table
print("Top 10 Days with Highest Ridership:")
print(top_10)
print("\nBottom 10 Days with Lowest Ridership:")
print(bottom_10)