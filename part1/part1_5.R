# Conduct  an  analysis  of  six  years'  worth  of  data  on
# registered  vehicles  in  Malaysia,  highlighting trends  and  patterns
# before  and  after  the  COVID-19  pandemic.  Incorporate  the  effects
# of  the Movement Control Order (MCO) into the discussion, supported by
# relevant visuals.

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Read and combine data
file_path <- "Data/"
file_list <- list.files(path = file_path, pattern = "^cars.*\\.csv$", full.names = TRUE)
years <- as.numeric(gsub("cars_|\\.csv", "", basename(file_list)))

combined_data <- do.call(rbind, lapply(seq_along(file_list), function(i) {
  df <- read.csv(file_list[i])
  df$year <- years[i]
  return(df)
}))

# Define MCO periods
mco_periods <- data.frame(
  start_date = as.Date(c(
    "2020-03-18",  # MCO 1.0
    "2020-05-04",  # CMCO
    "2020-06-10",  # RMCO
    "2021-01-13",  # MCO by states
    "2021-06-01",  # Total Lock Down
    "2021-06-15"   # NRP
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

# Prepare yearly data
yearly_totals <- combined_data %>%
  group_by(year) %>%
  summarise(
    total_vehicles = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    date = as.Date(paste0(year, "-01-01")),
    period = case_when(
      year < 2020 ~ "Pre-COVID",
      year > 2021 ~ "Post-COVID",
      TRUE ~ "During-COVID"
    )
  )

# Create time series plot with MCO periods highlighted
p1 <- ggplot(yearly_totals, aes(x = date, y = total_vehicles)) +
  # Add MCO period rectangles
  geom_rect(data = mco_periods,
            aes(xmin = start_date, xmax = end_date,
                ymin = -Inf, ymax = Inf, fill = mco_phase),
            alpha = 0.2,
            inherit.aes = FALSE) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, aes(color = period)) +
  geom_text(aes(label = scales::comma(total_vehicles)), 
            vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    title = "Total Registered Vehicles (2016-2021)",
    subtitle = "Impact of COVID-19 MCO Periods",
    x = "Year",
    y = "Number of Registered Vehicles",
    color = "Period",
    fill = "MCO Phase"
  ) +
  scale_y_continuous(labels = scales::comma)

# Calculate percentage change from pre-COVID baseline (2019)
baseline_analysis <- yearly_totals %>%
  mutate(
    pct_change = (total_vehicles / total_vehicles[year == 2019] - 1) * 100
  )

# Create bar plot showing percentage changes
p2 <- ggplot(baseline_analysis, aes(x = factor(year), y = pct_change, fill = period)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", pct_change)), 
            vjust = ifelse(baseline_analysis$pct_change >= 0, -0.5, 1.5)) +
  theme_minimal() +
  labs(
    title = "Percentage Change in Vehicle Registration",
    subtitle = "Compared to 2019 (Pre-COVID Baseline)",
    x = "Year",
    y = "Percentage Change (%)",
    fill = "Period"
  ) +
  theme(legend.position = "bottom")

# Analyze by vehicle type
type_analysis <- combined_data %>%
  group_by(year, type) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    date = as.Date(paste0(year, "-01-01")),
    period = case_when(
      year < 2020 ~ "Pre-COVID",
      year > 2021 ~ "Post-COVID",
      TRUE ~ "During-COVID"
    )
  )

# Create vehicle type trends plot
p3 <- ggplot(type_analysis, aes(x = date, y = count, color = type)) +
  geom_rect(data = mco_periods,
            aes(xmin = start_date, xmax = end_date,
                ymin = -Inf, ymax = Inf, fill = mco_phase),
            alpha = 0.2,
            inherit.aes = FALSE) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    title = "Vehicle Registration Trends by Vehicle Type",
    subtitle = "Impact of COVID-19 MCO Periods",
    x = "Year",
    y = "Number of Registrations",
    color = "Vehicle Type",
    fill = "MCO Phase"
  ) +
  scale_y_continuous(labels = scales::comma)

# Prepare monthly data
monthly_totals <- combined_data %>%
  mutate(
    month = format(as.Date(date_reg), "%m"),
    date = as.Date(paste0(year, "-", month, "-01"))
  ) %>%
  group_by(date) %>%
  summarise(
    total_vehicles = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    period = case_when(
      date < as.Date("2020-01-01") ~ "Pre-COVID",
      date > as.Date("2021-12-31") ~ "Post-COVID",
      TRUE ~ "During-COVID"
    )
  )

# Create detailed time series plot with MCO periods highlighted
p4 <- ggplot(monthly_totals, aes(x = date, y = total_vehicles)) +
  # Add MCO period rectangles
  geom_rect(data = mco_periods,
            aes(xmin = start_date, xmax = end_date,
                ymin = -Inf, ymax = Inf, fill = mco_phase),
            alpha = 0.2,
            inherit.aes = FALSE) +
  geom_line(linewidth = 1) +
  geom_point(size = 2, aes(color = period)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    title = "Monthly Registered Vehicles (2019-2021)",
    subtitle = "Impact of COVID-19 MCO Periods",
    x = "Date",
    y = "Number of Registered Vehicles",
    color = "Period",
    fill = "MCO Phase"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")

# Display plots
print(p1)
print(p2)
print(p3)
print(p4)

# Print summary statistics
cat("\nYearly Summary Statistics:\n")
print(yearly_totals)

cat("\nPercentage Changes from 2019 Baseline:\n")
print(baseline_analysis)
