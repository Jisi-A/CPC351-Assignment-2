# Create a visual to identify the top five brands of registered vehicles
# in Pulau Pinang for the years 2019, 2020, and 2021.
file_path <- "Data/"
library(ggplot2)
library(dplyr)

# Get list of CSV files using relative path
file_list <-
  list.files(path = file_path, pattern = "^cars.*\\.csv$", full.names = TRUE)

# Read required CSV files into separate dataframes
data_frames <- lapply(file_list[1:3], read.csv)

# Assign names to each dataframe based on the file names
names(data_frames) <- basename(file_list[1:3])

# Print the names of the dataframes
print(names(data_frames))

# Aggregate the number of rows for 'Pulau Pinang' based on 'maker'
maker_counts <- lapply(data_frames, function(df) {
  # Subset rows where state == 'Pulau Pinang'
  df_filtered <- df[df$state == "Pulau Pinang", ]

  # Aggregate to count rows by 'maker'
  aggregate(state ~ maker, data = df_filtered, FUN = length)
})

# Print the aggregated counts
print(maker_counts)

# Sort the aggregated dataframes by the count (state column)
sorted_counts <- lapply(maker_counts, function(df) {
  df[order(-df$state), ] # Sort by descending count
})

# Print the sorted counts
print(sorted_counts)

# Extract Top 5 Rows from Each DataFrame
top_5_counts <- lapply(seq_along(sorted_counts), function(i) {
  df <- sorted_counts[[i]]
  top_5 <- head(df, 5) # Take the top 5 rows

  # Add a column to indicate the dataframe index (or year)
  top_5$dataset <- paste("Year", (2018 + i)) # Customize as needed

  return(top_5)
})

# Combine All DataFrames into One
combined_df <- bind_rows(top_5_counts)

# Plot bar chart
ggplot(
  combined_df,
  aes(x = reorder(maker, -state), y = state, fill = dataset)
) +
  geom_bar(
    stat = "identity", position = "dodge"
  ) + # Dodge for side-by-side bars
  labs(
    title = "Top 5 Vehicle Brands in Pulau Pinang by Year",
    x = "Brand",
    y = "Number of Registrations"
  ) +
  theme_minimal() +
  facet_grid(~dataset, scale = "free") + # Facet by year/dataframe
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) # Rotate x-axis labels
