library(dplyr)
library(ggplot2)
library(lubridate)
library(corrplot)

df <- read.csv("results/intermediate/cleaned_data.csv") %>%
  mutate(datetime = make_datetime(year, month, day, hour))

# Daily-mean AQI trend
df_daily <- df %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(AQI = mean(AQI, na.rm = TRUE))

p1 <- ggplot(df_daily, aes(x = date, y = AQI)) +
  geom_line(color = "#b5391a", alpha = 0.6) +
  geom_smooth(method = "gam", color = "black") +
  theme_minimal() +
  labs(title = "Beijing Average Daily AQI (2013-2017)",
       x = "Year", y = "Mean AQI")
ggsave("plots/aqi_trend.png", p1, width = 10, height = 6)

# Month x hour heatmap
df_heat <- df %>%
  group_by(month, hour) %>%
  summarise(AQI = mean(AQI, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(df_heat, aes(x = hour, y = factor(month), fill = AQI)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("#e3fde8", "#fdf3e3", "#b5391a")) +
  theme_minimal() +
  labs(title = "AQI Heatmap: Month vs Hour of Day",
       x = "Hour of Day", y = "Month", fill = "Avg AQI")
ggsave("plots/aqi_heatmap.png", p2, width = 10, height = 6)

# Station distribution
p3 <- ggplot(df, aes(x = reorder(station, AQI, FUN = median), y = AQI, fill = station)) +
  geom_boxplot(outlier.alpha = 0.1) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "AQI Distribution by Station", x = "Station", y = "AQI")
ggsave("plots/station_comparison.png", p3, width = 10, height = 8)

# Correlation matrix of pollutants and weather
numeric_cols <- df %>%
  select(PM2.5, PM10, SO2, NO2, CO, O3, TEMP, PRES, DEWP, RAIN, WSPM)

png("plots/correlation_matrix.png", width = 800, height = 800)
corrplot(cor(numeric_cols, use = "complete.obs"),
         method = "color", type = "upper", addCoef.col = "black",
         tl.col = "black", tl.srt = 45,
         title = "Pollutant & Meteorological Correlations",
         mar = c(0, 0, 1, 0))
dev.off()
