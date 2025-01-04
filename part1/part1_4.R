# Analyze  six  years  of  data  on  registered  vehicles  in  Malaysia,
# focusing on  trends  and  patterns related to the fuel types of the
# registered vehicles.

file_path <- "Data/"
library(dplyr)
library(ggplot2)
library(plotly)

# Get list of CSV files using relative path
file_list <- list.files(path = file_path, pattern = "^cars.*\\.csv$", full.names = TRUE)

# Read and combine all CSV files with year information
years <- as.numeric(gsub("cars_|\\.csv", "", basename(file_list)))
combined_data <- do.call(rbind, lapply(seq_along(file_list), function(i) {
  df <- read.csv(file_list[i])
  df$year <- years[i]
  return(df)
})) %>%
  # Remove any rows where fuel is NA or NULL
  filter(!is.na(fuel), fuel != "") 

# Let's check the unique fuel types
print("Unique fuel types:")
print(unique(combined_data$fuel))

# Calculate summary statistics for fuel types
fuel_analysis <- combined_data %>%
  # First get total vehicles per year
  group_by(year) %>%
  mutate(total_vehicles = n()) %>%
  # Then calculate counts and percentages by fuel type
  group_by(year, fuel) %>%
  summarise(
    count = n(),
    percentage = (count / first(total_vehicles)) * 100,
    .groups = 'drop'
  ) %>%
  arrange(year, desc(count))

# Print summary statistics
cat("\nSummary of Vehicle Fuel Types by Year:\n")
print(fuel_analysis)

# Create main trend plot
p1 <- ggplot(fuel_analysis, aes(x = year, y = count, color = fuel, group = fuel)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Trends in Vehicle Fuel Types (2016-2021)",
    subtitle = "Number of registered vehicles by fuel type",
    x = "Year",
    y = "Number of Vehicles",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  scale_color_viridis_d() +
  theme(legend.position = "bottom")

# Create percentage stacked bar plot
p2 <- ggplot(fuel_analysis, aes(x = factor(year), y = percentage, fill = fuel)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Distribution of Fuel Types Over Years",
    subtitle = "Percentage breakdown of fuel types",
    x = "Year",
    y = "Percentage (%)",
    fill = "Fuel Type"
  ) +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(legend.position = "bottom")

# Create individual trend plots
p3 <- ggplot(fuel_analysis, aes(x = year, y = count, color = fuel, group = fuel)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +
  facet_wrap(~fuel, scales = "free_y", ncol = 2) +
  labs(
    title = "Individual Fuel Type Trends",
    subtitle = "Separate trends for each fuel type",
    x = "Year",
    y = "Number of Vehicles"
  ) +
  theme_minimal() +
  scale_color_viridis_d()

# Calculate year-over-year growth rates
growth_analysis <- fuel_analysis %>%
  group_by(fuel) %>%
  arrange(year) %>%
  mutate(
    growth_rate = (count - lag(count)) / lag(count) * 100
  ) %>%
  filter(!is.na(growth_rate))

# Print growth analysis
cat("\nYear-over-Year Growth Rates by Fuel Type:\n")
print(growth_analysis)

# Add new p4 plot for growth rates
p4 <- ggplot(growth_analysis, aes(x = year, y = growth_rate, color = fuel, group = fuel)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Year-over-Year Growth Rates by Fuel Type",
    subtitle = "Percentage change from previous year",
    x = "Year",
    y = "Growth Rate (%)",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  scale_color_viridis_d() +
  theme(legend.position = "bottom")

# Display all plots
print(p1)
print(p2)
print(p3)
print(p4)

# Make the main trend plot interactive
ggplotly(p1)