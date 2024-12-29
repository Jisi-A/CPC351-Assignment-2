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
    "2020-03-18",  # MCO 1.0
    "2020-05-04",  # CMCO
    "2020-06-10",  # RMCO
    "2021-01-13",  # MCO 2.0
    "2021-05-12"   # MCO 3.0
  )),
  end_date = as.Date(c(
    "2020-05-03",
    "2020-06-09",
    "2020-12-31",
    "2021-05-11",
    "2021-12-31"
  )),
  mco_phase = c("MCO 1.0", "CMCO", "RMCO", "MCO 2.0", "MCO 3.0")
)

# Prepare data for analysis
ridership_data <- df %>%
  select(date, rail_lrt_ampang, rail_lrt_kj, rail_monorail) %>%
  mutate(
    year = year(date),
    month = floor_date(date, "month"),
    period = case_when(
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
      starts_with("rail"),
      list(
        avg = ~mean(., na.rm = TRUE),
        total = ~sum(., na.rm = TRUE)
      )
    )
  )

# Reshape data for plotting
long_data <- monthly_avg %>%
  pivot_longer(
    cols = -month,
    names_to = c("line", "metric"),
    names_pattern = "rail_(.*)_(.*)",
    values_to = "value"
  ) %>%
  filter(metric == "avg")  # Use averages for visualization

# Create time series plot with MCO periods highlighted
p1 <- ggplot(long_data, aes(x = month, y = value, color = line)) +
  geom_line() +
  geom_point(size = 1) +
  # Add MCO period rectangles
  geom_rect(data = mco_periods,
            aes(xmin = start_date, xmax = end_date,
                ymin = -Inf, ymax = Inf, fill = mco_phase),
            alpha = 0.2,
            inherit.aes = FALSE) +
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

# Calculate year-over-year changes
yearly_summary <- ridership_data %>%
  group_by(year) %>%
  summarise(
    across(
      starts_with("rail"),
      list(
        total = ~sum(., na.rm = TRUE),
        avg = ~mean(., na.rm = TRUE)
      )
    )
  )

# Create bar plot for year-over-year comparison
yearly_long <- yearly_summary %>%
  pivot_longer(
    cols = -year,
    names_to = c("line", "metric"),
    names_pattern = "rail_(.*)_(.*)",
    values_to = "value"
  ) %>%
  filter(metric == "avg")

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
    across(starts_with("rail"), ~mean(., na.rm = TRUE))
  ) %>%
  mutate(
    across(
      starts_with("rail"),
      ~(. / first(.)) - 1,
      .names = "{.col}_pct_change"
    )
  )

# Create heatmap of recovery patterns
recovery_long <- baseline_comparison %>%
  select(year, ends_with("pct_change")) %>%
  pivot_longer(
    cols = -year,
    names_to = "line",
    values_to = "pct_change"
  )

p3 <- ggplot(recovery_long, aes(x = as.factor(year), y = line, fill = pct_change)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "green",
    midpoint = 0,
    labels = scales::percent
  ) +
  theme_minimal() +
  labs(
    title = "Recovery Pattern Relative to 2019 Baseline",
    x = "Year",
    y = "Rail Line",
    fill = "% Change"
  )

# Display plots
print(p1)
print(p2)
print(p3)

# Print summary statistics
print("Yearly Summary Statistics:")
print(yearly_summary)

print("\nBaseline Comparison (% change from 2019):")
print(baseline_comparison)
