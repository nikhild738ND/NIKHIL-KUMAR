library(tidyverse)
library(lubridate)
library(viridis)
library(plotly)
gtd <- read_csv("gtd.csv", show_col_types = FALSE)
gtd_clean <- gtd |>
mutate(
# Fix month,day if they are 0
month = if_else(imonth %in% 1:12, imonth, 1L),
day = if_else(iday %in% 1:31, iday, 1L),
# creating a proper Date column
date = ymd(paste(iyear, month, day, sep = "-")),
#nameing clearly for country and region
country_name = country_txt,
region_name = region_txt,
# Fatalities
fatalities = replace_na(nkill, 0)
) |>
# Keep records from 1990 onward with valid date + coordinates
filter(
iyear >= 1990,
!is.na(date),
!is.na(latitude),
!is.na(longitude)
)
# Quick check: show first few rows
head(gtd_clean, 5)

# Counting number of attacks per year
attacks_per_year <- gtd_clean |>
count(iyear, name = "n_attacks")
# Time series plot of attacks per year
ggplot(attacks_per_year, aes(x = iyear, y = n_attacks)) +
geom_line(color = "steelblue", linewidth = 1.1) +
geom_smooth(se = FALSE, span = 0.3, color = "darkred") +
labs(title = "Global Terrorist Attacks per Year",
x = "Year",y = "Number of Attacks") +
theme_minimal(base_size = 13)

# Regional Trends Plot ggplot
# Count attacks by region and year
attacks_by_region <- gtd_clean |>
count(region_name, iyear, name = "n_attacks")
# Regional time trend plot
ggplot(attacks_by_region, aes(x = iyear, y = n_attacks, color = region_name)) +
geom_line(linewidth = 1) +
scale_color_viridis_d() +
labs(title = "Terrorist Attacks per Year by Region",
x = "Year",y = "Number of Attacks",color = "Region") +
theme_minimal(base_size = 13)

# Interactive Plot plotly
p_region <- ggplot(attacks_by_region,
aes(x = iyear, y = n_attacks, color = region_name)) +
geom_line(linewidth = 1) +
scale_color_viridis_d() +
labs(title = "Interactive Regional Terrorism Trends",
x = "Year",y = "Number of Attacks",color = "Region"
) +
theme_minimal(base_size = 13)
ggplotly(p_region)
