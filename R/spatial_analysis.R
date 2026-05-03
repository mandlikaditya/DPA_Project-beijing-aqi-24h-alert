library(dplyr)
library(ggplot2)

# Hand-curated coordinates for the 12 PRSA stations (~few hundred meters precision)
stations_coords <- data.frame(
  station = c("Aotizhongxin", "Changping", "Dingling", "Dongsi", "Guanyuan",
              "Gucheng", "Huairou", "Nongzhanguan", "Shunyi", "Tiantan",
              "Wanliu", "Wanshouxigong"),
  lat = c(39.982, 40.217, 40.292, 39.929, 39.929,
          39.914, 40.328, 39.937, 40.127, 39.886,
          39.987, 39.866),
  lon = c(116.397, 116.235, 116.220, 116.417, 116.339,
          116.184, 116.628, 116.461, 116.655, 116.407,
          116.287, 116.341)
)

df <- read.csv("results/intermediate/cleaned_data.csv")

station_aqi <- df %>%
  group_by(station) %>%
  summarise(Avg_AQI = mean(AQI, na.rm = TRUE)) %>%
  left_join(stations_coords, by = "station")

p_map <- ggplot(station_aqi, aes(x = lon, y = lat)) +
  geom_point(aes(size = Avg_AQI, color = Avg_AQI), alpha = 0.8) +
  scale_color_gradient(low = "yellow", high = "red") +
  geom_text(aes(label = station), vjust = -1.5, size = 3) +
  theme_minimal() +
  labs(title = "Spatial Distribution of Average AQI in Beijing",
       x = "Longitude", y = "Latitude",
       size = "Mean AQI", color = "Pollution Level")

ggsave("plots/spatial_aqi_map.png", p_map, width = 10, height = 8)
