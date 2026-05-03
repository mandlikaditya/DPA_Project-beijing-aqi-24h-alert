library(dplyr)
library(lubridate)
library(forecast)

df <- read.csv("results/intermediate/cleaned_data.csv")

# Daily mean AQI for cleaner STL output
df_ts <- df %>%
  mutate(datetime = make_datetime(year, month, day, hour)) %>%
  group_by(date = as.Date(datetime)) %>%
  summarise(AQI = mean(AQI, na.rm = TRUE)) %>%
  arrange(date)

# Beijing data starts 2013-03-01 (day 60); annual seasonality
aqi_ts  <- ts(df_ts$AQI, start = c(2013, 60), frequency = 365.25)
aqi_stl <- stl(aqi_ts, s.window = "periodic")

png("plots/aqi_decomposition.png", width = 1000, height = 800)
plot(aqi_stl, main = "STL Decomposition of Beijing AQI (Trend + Seasonal + Residual)")
dev.off()
