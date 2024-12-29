# Analyze  six  years  of  data  on  registered  vehicles  in  Malaysia,
# focusing on  trends  and  patterns related to the fuel types of the
# registered vehicles.

file_path <- "Data/"
library(dplyr)
library(ggplot2)
library(plotly)

# Get list of CSV files using relative path
file_list <-
  list.files(path = file_path, pattern = "^cars.*\\.csv$", full.names = TRUE)

print(file_list)

# Read all CSV files into separate dataframes
data_frames <- lapply(file_list, read.csv)

# Assign names to each dataframe based on the file names
names(data_frames) <- basename(file_list)

# Print the names of the dataframes
print(names(data_frames))

# Check unique fuel types in each dataframe
unique_fuel_types <- lapply(data_frames, function(df) unique(df$fuel))

# Print unique fuel types for each dataframe
print((unique_fuel_types))

# Count the number of occurrences of each fuel type in each dataframe
fuel_type_counts <- lapply(data_frames, function(df) table(df$fuel))

# Print the counts of each fuel type for each dataframe
print(fuel_type_counts)

# Extract years from file names
years <- as.numeric(gsub("cars_|\\.csv", "", basename(file_list)))

# Combine data frames into one with an additional column for the year
combined_data <- do.call(rbind, lapply(seq_along(data_frames), function(i) {
  df <- data_frames[[i]]
  df$year <- years[i]
  return(df)
}))

# Aggregate data to get the count of each fuel type per year
fuel_counts_per_year <- combined_data %>%
  group_by(year, fuel) %>%
  summarise(count = n()) %>%
  ungroup()

# Plot the combined graph
ggplot(
  fuel_counts_per_year,
  aes(x = year, y = count, color = fuel, group = fuel)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Number of Registered Vehicles by Fuel Type Over Time",
    x = "Year",
    y = "Number of Vehicles",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Paired")

# Plot the separated graphs
ggplot(
  fuel_counts_per_year,
  aes(x = year, y = count, color = fuel, group = fuel)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Number of Registered Vehicles by Fuel Type Over Time",
    x = "Year",
    y = "Number of Vehicles",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Paired") +
  facet_wrap(~fuel, scales = "free_y", ncol = 4)

# Make the plot interactive and more readable using plotly
p <- ggplot(
  fuel_counts_per_year,
  aes(x = year, y = count, color = fuel, group = fuel)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Number of Registered Vehicles by Fuel Type Over Time",
    x = "Year",
    y = "Number of Vehicles",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Paired") +
  facet_wrap(~fuel, scales = "free_y", ncol = 4)

ggplotly(p) # Makes the plot interactive
