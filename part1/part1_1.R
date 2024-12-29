# Using  appropriate  visuals,  show  the  number  of  vehicles
# registered  at  each  state  JPJ  office (including the Rakan Niaga)
# for the years of 2019, 2020, 2021, 2022, 2023, and 2024.

file_path <- "Data/"
library(ggplot2)
library(dplyr)
library(RColorBrewer)
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

# Check for null values in all dataframes
null_values <- sapply(data_frames, function(df) sum(is.na(df)))

# Print the number of null values in each dataframe
print(null_values)

# Count the number of occurrences of each fuel type in each dataframe
vehicle_counts_state <- lapply(data_frames, function(df) table(df$state))

# Print the counts of each fuel type for each dataframe
print(vehicle_counts_state)

# Extract years from file names
years <- as.numeric(gsub("cars_|\\.csv", "", basename(file_list)))

# Combine data frames into one with an additional column for the year
combined_data <- do.call(rbind, lapply(seq_along(data_frames), function(i) {
  df <- data_frames[[i]]
  df$year <- years[i]
  return(df)
}))

# Aggregate data to get the count of each fuel type per year
vehicle_counts_per_year <- combined_data %>%
  group_by(year, state) %>%
  summarise(count = n()) %>%
  ungroup()

# Plot the combined graph
ggplot(
  vehicle_counts_per_year,
  aes(x = year, y = count, color = state, group = state)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Number of Registered Vehicles by State Over Time",
    x = "Year",
    y = "Number of Vehicles",
    color = "State"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c(brewer.pal(10, "Paired"), brewer.pal(8, "Dark2"))
  )

ggplot(
  vehicle_counts_per_year,
  aes(x = year, y = count, color = state, group = state)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Number of Registered Vehicles by State Over Time",
    x = "Year",
    y = "Number of Vehicles",
    color = "State"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c(brewer.pal(10, "Paired"), brewer.pal(8, "Dark2"))
  ) +
  facet_wrap(~state, scales = "free_y", ncol = 3)

p <- ggplot(
  vehicle_counts_per_year,
  aes(x = state, y = count, fill = as.factor(year))
) +
  geom_bar(stat = "identity", position = "dodge") + # Grouped by year
  labs(
    title = "Number of Registered Vehicles by State (All Years)",
    x = "State",
    y = "Number of Vehicles",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(~state, scales = "free_y", ncol = 3)

ggplotly(p)