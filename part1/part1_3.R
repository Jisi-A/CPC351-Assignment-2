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
file_list <- list.files(path = file_path, pattern = "^cars.*\\.csv$", full.names = TRUE)

# Read required CSV files into separate dataframes
data_frames <- lapply(file_list, read.csv)

# Combine all dataframes into one
combined_df <- bind_rows(data_frames)

# Create a State-to-Region Lookup Table
state_to_region <- stack(regions)
colnames(state_to_region) <- c("state", "region")

# Process the combined dataframe
favorite_colors <- combined_df %>%
  # Exclude rows where state == "Rakan Niaga"
  filter(state != "Rakan Niaga") %>%
  # Join with region mapping
  left_join(state_to_region, by = c("state" = "state")) %>%
  # Group by region and color, then count
  group_by(region, colour) %>%
  summarise(count = n(), .groups = "drop") %>%
  # Calculate total count per region
  group_by(region) %>%
  mutate(total_count = sum(count),
         percentage = (count / total_count) * 100) %>%
  # Find the most popular color for each region
  slice_max(count, n = 1, with_ties = FALSE)

# Create a readable summary
region_summary <- favorite_colors %>%
  arrange(region) %>%
  select(region, colour, percentage) %>%
  mutate(percentage = round(percentage, 2)) %>%
  rename(
    Region = region,
    "Favorite Color" = colour,
    "Percentage (%)" = percentage
  )

# Display formatted results
cat("\nFavorite Vehicle Colors by Region:\n")
print(region_summary, row.names = FALSE)

# Create plot for actual counts
p1 <- ggplot(favorite_colors, aes(x = region, y = count, fill = colour)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), 
            position = position_stack(vjust = 0.5),
            color = "black") +
  labs(
    title = "Favorite Vehicle Colors by Region",
    subtitle = "Actual Number of Vehicles",
    x = "Region",
    y = "Number of Vehicles",
    fill = "Color"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

# Create plot for percentages
p2 <- ggplot(favorite_colors, aes(x = region, y = percentage, fill = colour)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_stack(vjust = 0.5),
            color = "black") +
  labs(
    title = "Favorite Vehicle Colors by Region",
    subtitle = "Percentage Distribution",
    x = "Region",
    y = "Percentage (%)",
    fill = "Color"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

# Display both plots
print(p1)
print(p2)
