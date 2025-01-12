# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Read the data
df <- read.csv("Data/ridership_headline.csv")

# Convert date to proper date format
df$date <- as.Date(df$date)

# Define MCO periods
mco_periods <- data.frame(
  start_date = as.Date(c(
    "2020-03-18", # MCO 1.0
    "2020-05-04", # CMCO
    "2020-06-10", # RMCO
    "2021-01-13", # MCO by states
    "2021-06-01", # Total Lock Down
    "2021-06-15" # NRP
  )),
  end_date = as.Date(c(
    "2020-05-03",
    "2020-06-09",
    "2021-03-31",
    "2021-05-31",
    "2021-06-28",
    "2021-12-31"
  )),
  mco_phase = c("MCO 1.0", "CMCO", "RMCO", "MCO by states", "Total Lock Down", "NRP")
)

# Prepare data for analysis
ridership_data <- df %>%
  select(date, rail_lrt_ampang, rail_lrt_kj, rail_monorail) %>%
  mutate(
    year = year(date),
    month = floor_date(date, "month"),
    period = case_when(
      # ~ returns the result of condition
      date < as.Date("2020-03-18") ~ "Pre-MCO",
      date >= as.Date("2022-01-01") ~ "Post-MCO",
      TRUE ~ "During-MCO"
    )
  )

# Calculate monthly averages
monthly_avg <- ridership_data %>%
  group_by(month) %>%
  summarise(
    across(
      c("rail_lrt_ampang", "rail_lrt_kj", "rail_monorail"),
      # ~mean(.) creates an anonymous function where:
      # ~ (tilde) defines the anonymous function
      # . (dot) represents the input column data
      # na.rm ignore NA values
      ~ mean(., na.rm = TRUE)
    )
  )

# Reshape data for plotting
long_data <- monthly_avg %>%
  pivot_longer(
    cols = -month,
    names_to = "line",
    names_pattern = "rail_(.*)",
    values_to = "value"
  )

# Create time series plot with MCO periods highlighted
p1 <- ggplot(long_data, aes(x = month, y = value, color = line)) +
  geom_line() +
  geom_point(size = 1) +
  # Add MCO period rectangles
  geom_rect(
    data = mco_periods,
    aes(
      xmin = start_date, xmax = end_date,
      ymin = -Inf, ymax = Inf, fill = mco_phase
    ),
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    title = "Monthly Average Rail Ridership (2019-2024)",
    subtitle = "Highlighting MCO Periods",
    x = "Date",
    y = "Average Daily Ridership",
    color = "Rail Line",
    fill = "MCO Phase"
  ) +
  scale_y_continuous(labels = scales::comma)

# Calculate year-over-year changes (simplified)
yearly_summary <- ridership_data %>%
  group_by(year) %>%
  summarise(
    across(
      c("rail_lrt_ampang", "rail_lrt_kj", "rail_monorail"),
      ~ mean(., na.rm = TRUE)
    )
  )

# Create bar plot for year-over-year comparison
yearly_long <- yearly_summary %>%
  pivot_longer(
    cols = -year,
    names_to = "line",
    # regex to extract the line name from the column name
    names_pattern = "rail_(.*)",
    values_to = "value"
  )

p2 <- ggplot(yearly_long, aes(x = as.factor(year), y = value, fill = line)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Yearly Average Daily Ridership by Rail Line",
    x = "Year",
    y = "Average Daily Ridership",
    fill = "Rail Line"
  ) +
  scale_y_continuous(labels = scales::comma)

# Calculate percentage changes from pre-COVID baseline (2019)
baseline_comparison <- ridership_data %>%
  group_by(year) %>%
  summarise(
    across(c("rail_lrt_ampang", "rail_lrt_kj", "rail_monorail"), ~ mean(., na.rm = TRUE))
  ) %>%
  mutate(
    across(
      c("rail_lrt_ampang", "rail_lrt_kj", "rail_monorail"),
      ~ (. / first(.)) - 1,
      .names = "{.col}_pct_change"
    )
  )


# Display plots
print(p1)
print(p2)

# Print summary statistics
print("Yearly Summary Statistics:")
print(yearly_summary)

print("\nBaseline Comparison (% change from 2019):")
print(baseline_comparison)
