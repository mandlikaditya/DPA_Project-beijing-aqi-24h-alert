library(dplyr)
library(ggplot2)
library(lubridate)

df <- read.csv("results/intermediate/cleaned_data.csv") %>%
  mutate(
    datetime = as.POSIXct(make_datetime(year, month, day, hour)),
    season = case_when(
      month %in% c(12, 1, 2)  ~ "Winter",
      month %in% c(3, 4, 5)   ~ "Spring",
      month %in% c(6, 7, 8)   ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn"
    )
  )

# H0: mean AQI is the same across stations / seasons
print(summary(aov(AQI ~ station, data = df)))
print(summary(aov(AQI ~ season,  data = df)))

p_season <- ggplot(df, aes(x = season, y = AQI, fill = season)) +
  geom_boxplot(outlier.alpha = 0.1) +
  theme_minimal() +
  labs(title = "AQI Variation by Season", x = "Season", y = "AQI")

ggsave("plots/aqi_by_season_boxplot.png", p_season, width = 8, height = 6)
