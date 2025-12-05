# ============================================================
#  STA9750 Mid-Semester Project – Xiaolin Wu
#  Has subway crime increased or decreased over time? (2022–2025)
# ============================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(vroom)

# --- 1. Load & Clean NYPD Data (Historic + 2025 YTD) --------------------------
historic <- vroom("data/raw/NYPD_Complaint_Data_Historic_small.csv",
                  col_select = c(CMPLNT_FR_DT, PREM_TYP_DESC)) %>%
  clean_names()

current <- vroom("data/raw/NYPD_Complaints_2025.csv",
                 col_select = c(CMPLNT_FR_DT, PREM_TYP_DESC)) %>%
  clean_names()

# Combine both
nypd_all <- bind_rows(historic, current) %>%
  distinct() %>%
  mutate(date = suppressWarnings(mdy(cmplnt_fr_dt))) %>%
  filter(!is.na(date))

# --- 2. Filter Subway-related complaints -------------------------------------
nypd_subway <- nypd_all %>%
  filter(!is.na(prem_typ_desc)) %>%
  filter(str_detect(toupper(prem_typ_desc), "SUBWAY"))

# --- 3. Aggregate monthly complaint counts -----------------------------------
nypd_monthly <- nypd_subway %>%
  mutate(month = floor_date(date, "month")) %>%
  count(month, name = "crime_count")

# --- 4. Load & aggregate MTA ridership data ----------------------------------
ridership <- read_csv("data/raw/MTA_Ridership_Daily.csv") %>%
  rename_with(~ gsub("[: ]", "_", .x)) %>%
  clean_names() %>%
  mutate(date = suppressWarnings(mdy(date))) %>%
  filter(!is.na(date)) %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    total_riders = sum(subways_total_estimated_ridership, na.rm = TRUE),
    .groups = "drop"
  )

# --- 5. Merge, normalize, and clean incomplete months ------------------------
crime_trend <- left_join(nypd_monthly, ridership, by = "month") %>%
  filter(year(month) >= 2022, !is.na(total_riders), total_riders > 1e7) %>%  # 过滤ridership过低月份
  mutate(rate_per_100k = (crime_count / total_riders) * 100000)

# --- 6. Visualization --------------------------------------------------------
p <- ggplot(crime_trend, aes(x = month, y = rate_per_100k)) +
  geom_line(color = "#0072B2", linewidth = 1.3) +
  geom_point(color = "#0072B2", size = 1.4) +
  geom_smooth(method = "loess", color = "#D55E00", se = FALSE, linewidth = 1.2) +
  
  # Subway Safety Plan line
  geom_vline(xintercept = as.Date("2022-02-18"), linetype = "dashed", color = "gray40") +
  annotate("text", x = as.Date("2022-02-18"),
           y = max(crime_trend$rate_per_100k, na.rm = TRUE) * 0.95,
           label = "Subway Safety Plan", angle = 90, vjust = -0.5, size = 3.3, color = "gray30") +
  
  # Data completeness note
  annotate("text", x = as.Date("2025-07-01"),
           y = max(crime_trend$rate_per_100k, na.rm = TRUE) * 0.92,
           label = "Data incomplete beyond Sep 2025",
           color = "gray50", size = 3.3, hjust = 0) +
  
  # X-axis formatting
  scale_x_date(
    limits = as.Date(c("2022-01-01", "2025-12-31")),
    date_breaks = "3 months",
    date_labels = "%Y-%b"
  ) +
  
  # Labels
  labs(
    title = "Subway Crime Rate Over Time (per 100k Riders)",
    subtitle = "Normalized by monthly MTA ridership (2022–2025)",
    x = NULL,
    y = "Crimes per 100k Riders",
    caption = "Data: NYPD Complaint Data Historic (2022–2024) + NYPD Complaints 2025 YTD + MTA Daily Ridership"
  ) +
  
  # Clean slide-friendly theme
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#F8F8F8", color = NA),
    panel.grid.major.y = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "gray25"),
    axis.text.y = element_text(color = "gray25"),
    plot.title = element_text(face = "bold", size = 16, color = "#333333"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1)
  )

# --- 7. Save figure ----------------------------------------------------------
ggsave("figs/subway_crime_rate_2022_2025_final.png", p, width = 9, height = 5, dpi = 300)
print(p)
