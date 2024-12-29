# Based on the given datasets (excluding the Rakan Niaga), what is the favourite
# vehicles colour for the following regions:
# a. Northern region of Peninsular Malaysia: Perlis, Kedah, Pulau Pinang, and
# Perak).
# b. Central  region  of  Peninsular  Malaysia:  Selangor,  Kuala  Lumpur,
# Putrajaya,  and  Negeri Sembilan.
# c. East coast region of Peninsular Malaysia: Pahang, Terengganu, and Kelantan.
# d. Southern region of Peninsular Malaysia: Melaka and Johor.
# e. East Malaysia: Sarawak, Sabah, and Labuan.
file_path <- "Data/"
library(dplyr)
library(stringr)
library(ggplot2)

# List of regions and states
regions <- list(
  "Northern" = c("Perlis", "Kedah", "Pulau Pinang", "Perak"),
  "Central" = c("Selangor", "W.P. Kuala Lumpur", "W.P. Putrajaya", "Negeri Sembilan"),
  "East Coast" = c("Pahang", "Terengganu", "Kelantan"),
  "Southern" = c("Melaka", "Johor"),
  "East Malaysia" = c("Sarawak", "Sabah", "W.P. Labuan")
)

# Get list of CSV files using relative path
file_list <-
  list.files(path = file_path, pattern = "^cars.*\\.csv$", full.names = TRUE)

# Read required CSV files into separate dataframes
data_frames <- lapply(file_list, read.csv)

# Assign names to each dataframe based on the file names
names(data_frames) <- basename(file_list)

# Print the names of the dataframes
print(names(data_frames))

# Create a State-to-Region Lookup Table
state_to_region <- stack(regions)
colnames(state_to_region) <- c("state", "region")

# Process Each Dataframe
favorite_colors_by_region <- lapply(data_frames, function(df) {
  # Exclude rows where state == "Rakan Niaga"
  df <- df %>% filter(state != "Rakan Niaga")

  # Join dataframe with region mapping
  df <- df %>%
    left_join(state_to_region, by = c("state" = "state"))

  # Group by region and color, then count
  color_counts <- df %>%
    group_by(region, colour) %>%
    summarise(count = n(), .groups = "drop")

  # Find the most popular color for each region
  top_colors <- color_counts %>%
    group_by(region) %>%
    slice_max(count, n = 1, with_ties = FALSE) # Top color for each region

  return(top_colors)
})

# Step 3: Combine All DataFrames into One
combined_top_colors <- bind_rows(favorite_colors_by_region)

# Display Results
print(combined_top_colors)

# Optional: Plot the Results
ggplot(combined_top_colors, aes(x = region, y = count, fill = colour)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Favorite Vehicle Colors by Region",
    x = "Region",
    y = "Number of Vehicles",
    fill = "Color"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
