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
  # as.Date() converts the date to format YYYY-MM-DD
  festival_date <- as.Date(festival_date)
  # seq() generates dates of both 7 days before and after the festival
  date_range <- seq(festival_date - 7, festival_date + 7, by = "day")

  df %>%
    # filter() filters the data for the dates in date_range
    filter(date %in% date_range) %>%
    # select() selects the columns to be returned
    select(date, rail_lrt_ampang, rail_mrt_kajang, rail_lrt_kj, rail_monorail)
}

# Initialize empty list to store results
festival_data <- list()

# Get data for each festival and year
# for each festival, for each year, get the festival date
for (festival in names(festivals)) {
  for (year in names(festivals[[festival]])) {
    festival_date <- festivals[[festival]][[year]]

    # Get data for this festival period
    period_data <- get_festival_data(festival_date, df)

    # if there is data for this festival period
    if (nrow(period_data) > 0) {
      # Add festival and year information
      period_data$festival <- festival
      period_data$year <- year
      # as.numeric() converts the date to the number of days from the festival
      period_data$days_from_festival <- as.numeric(
        period_data$date - as.Date(festival_date)
      )
      # add the data to the list
      festival_data[[paste(festival, year)]] <- period_data
    }
  }
}

# Combine all festival data
all_festival_data <- bind_rows(festival_data)

# Reshape data for plotting
# pivot_longer() reshapes the data from wide to long format
    # Original wide format
    # rail_lrt_ampang  rail_mrt_kajang  rail_lrt_kj  rail_monorail
    # 1000             2000             1500         500

    # New long format
    # line             ridership
    # rail_lrt_ampang  1000
    # rail_mrt_kajang   2000
    # rail_lrt_kj       1500
    # rail_monorail     500
long_data <- all_festival_data %>%
  pivot_longer(
    # cols holds the columns to be reshaped
    cols = c(rail_lrt_ampang, rail_mrt_kajang, rail_lrt_kj, rail_monorail),
    # names_to and values_to are the names of the new columns
    names_to = "line",
    values_to = "ridership"
  ) %>%
  # rename the line column 
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
    # filter() filters the data for the festival
    filter(festival == fest) %>%
    # ggplot() creates the plot
    ggplot(aes(x = days_from_festival, y = ridership, color = line)) +
    # geom_line() adds the line layer
    geom_line(linewidth = 0.8) +
    # geom_point() adds the point layer
    geom_point(size = 2) +
    # facet_wrap() creates a separate panel for each year
    facet_wrap(~year, scales = "free_y", ncol = 2) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    ) +
    # labs() adds the labels to the plot
    labs(
      title = paste("Ridership During", fest),
      subtitle = "7 days before and after the festival",
      x = "Days from Festival (0 = Festival Day)",
      y = "Number of Riders",
      color = "Rail Line"
    ) +
    # scale_y_continuous() scales the y-axis
    scale_y_continuous(labels = scales::comma_format(scale = 1/1000, suffix = "K")) +
    # geom_vline() adds a vertical line at x = 0
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
