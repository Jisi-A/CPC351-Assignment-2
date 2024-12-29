# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Read the data
df <- read.csv("Data/ridership_headline.csv")

# Convert date to proper date format
df$date <- as.Date(df$date)

# Define festival dates (first day) for each year
festivals <- list(
  "Hari Raya" = list(
    "2019" = "2019-06-05",
    "2020" = "2020-05-24",
    "2021" = "2021-05-13",
    "2022" = "2022-05-02",
    "2023" = "2023-04-22"
  ),
  "Chinese New Year" = list(
    "2019" = "2019-02-05",
    "2020" = "2020-01-25",
    "2021" = "2021-02-12",
    "2022" = "2022-02-01",
    "2023" = "2023-01-22"
  ),
  "Deepavali" = list(
    "2019" = "2019-10-27",
    "2020" = "2020-11-14",
    "2021" = "2021-11-04",
    "2022" = "2022-10-24",
    "2023" = "2023-11-12"
  ),
  "Christmas" = list(
    "2019" = "2019-12-25",
    "2020" = "2020-12-25",
    "2021" = "2021-12-25",
    "2022" = "2022-12-25",
    "2023" = "2023-12-25"
  )
)

# Function to get festival period data
get_festival_data <- function(festival_date, df) {
  festival_date <- as.Date(festival_date)
  date_range <- seq(festival_date - 7, festival_date + 7, by = "day")
  
  df %>%
    filter(date %in% date_range) %>%
    select(date, rail_lrt_ampang, rail_mrt_kajang, rail_lrt_kj, rail_monorail)
}

# Initialize empty list to store results
festival_data <- list()

# Get data for each festival and year
for (festival in names(festivals)) {
  for (year in names(festivals[[festival]])) {
    festival_date <- festivals[[festival]][[year]]
    
    # Get data for this festival period
    period_data <- get_festival_data(festival_date, df)
    
    if (nrow(period_data) > 0) {
      # Add festival and year information
      period_data$festival <- festival
      period_data$year <- year
      period_data$days_from_festival <- as.numeric(
        period_data$date - as.Date(festival_date)
      )
      
      festival_data[[paste(festival, year)]] <- period_data
    }
  }
}

# Combine all festival data
all_festival_data <- bind_rows(festival_data)

# Reshape data for plotting
long_data <- all_festival_data %>%
  pivot_longer(
    cols = c(rail_lrt_ampang, rail_mrt_kajang, rail_lrt_kj, rail_monorail),
    names_to = "line",
    values_to = "ridership"
  ) %>%
  mutate(
    line = case_when(
      line == "rail_lrt_ampang" ~ "LRT Ampang",
      line == "rail_mrt_kajang" ~ "MRT Kajang",
      line == "rail_lrt_kj" ~ "LRT Kelana Jaya",
      line == "rail_monorail" ~ "Monorail"
    )
  )

# Create plots for each festival
for (fest in unique(long_data$festival)) {
  p <- long_data %>%
    filter(festival == fest) %>%
    ggplot(aes(x = days_from_festival, y = ridership, color = line)) +
    geom_line() +
    geom_point() +
    facet_wrap(~year, scales = "free_y") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    labs(
      title = paste("Ridership During", fest),
      subtitle = "7 days before and after the festival",
      x = "Days from Festival (0 = Festival Day)",
      y = "Number of Riders",
      color = "Rail Line"
    ) +
    scale_y_continuous(labels = scales::comma) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)
  
  print(p)
}

# Calculate and print average ridership during festival periods
festival_summary <- long_data %>%
  group_by(festival, year, line) %>%
  summarise(
    avg_ridership = mean(ridership, na.rm = TRUE),
    total_ridership = sum(ridership, na.rm = TRUE),
    .groups = "drop"
  )

print("Festival Period Summary:")
print(festival_summary)
