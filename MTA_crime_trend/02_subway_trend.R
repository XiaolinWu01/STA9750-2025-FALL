# ---- 0. Libraries ----
library(tidyverse)
library(lubridate)
library(scales)

# ---- 1. File paths ----
crime_fp <- "data/raw/NYPD_Complaints_2025.csv"
rides_fp <- "data/raw/MTA_Ridership_Daily.csv"

# ---- 2. Load raw data ----
crime_raw <- read_csv(crime_fp, show_col_types = FALSE)
rides_raw <- read_csv(rides_fp, show_col_types = FALSE)

# ---- 3. Filter subway-related crimes ----
crime_subway <- crime_raw %>%
  filter(JURIS_DESC == "N.Y. TRANSIT POLICE") %>%
  mutate(
    date = as.Date(RPT_DT, format = "%m/%d/%Y"),
    year_month = floor_date(date, "month")
  ) %>%
  group_by(year_month) %>%
  summarise(total_crimes = n(), .groups = "drop")

crime_subway <- crime_subway %>%
  mutate(year_month = as.Date(year_month))

rides <- rides %>%
  mutate(year_month = as.Date(year_month))

# ---- 4. Clean and summarize MTA ridership ----
rides <- rides_raw %>%
  mutate(
    date = as.Date(Date, format = "%m/%d/%Y"),        # ✅ 修正日期格式
    year_month = floor_date(date, "month")            # ✅ 不需要再次 as.Date
  ) %>%
  group_by(year_month) %>%
  summarise(
    subway_rides = sum(`Subways: Total Estimated Ridership`, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 5. Combine datasets ----
trend <- left_join(rides, crime_subway, by = "year_month") %>%
  mutate(crimes_per_million = (total_crimes / subway_rides) * 1e6)


# ---- 6. Plot trend ----
library(ggplot2)

p <- ggplot(trend, aes(x = year_month, y = crimes_per_million)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_point(size = 2, color = "#0072B2") +
  
  geom_vline(xintercept = as.Date("2020-03-15"), color = "red", linetype = "dashed") +
  annotate("text",
           x = as.Date("2020-03-15"), y = max(trend$crimes_per_million, na.rm = TRUE) * 0.9,
           label = "COVID-19 Lockdown", color = "red", angle = 90, vjust = -0.5, size = 3.2) +
  
  geom_vline(xintercept = as.Date("2022-02-21"), color = "darkgreen", linetype = "dotted") +
  annotate("text",
           x = as.Date("2022-02-21"), y = max(trend$crimes_per_million, na.rm = TRUE) * 0.8,
           label = "Subway Safety Plan", color = "darkgreen", angle = 90, vjust = -0.5, size = 3.2) +
  
  labs(
    title = "Subway Crime Rate per 1M Rides (2019–2025)",
    subtitle = "Based on NYPD Complaint Data (Transit Bureau) + MTA Daily Ridership",
    x = "Month",
    y = "Crimes per 1,000,000 Rides",
    caption = "Sources: NYC Open Data (NYPD), data.ny.gov (MTA Ridership)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30")
  )

print(p)


# ---- 7. Save outputs ----
if (!dir.exists("figs")) dir.create("figs")
if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

ggsave("figs/subway_crime_trend.png", p, width = 10, height = 5, dpi = 300)
write_csv(trend, "data/processed/subway_crime_trend_monthly.csv")

message("✅ Done! Check 'figs/subway_crime_trend.png' and 'data/processed/subway_crime_trend_monthly.csv'")
