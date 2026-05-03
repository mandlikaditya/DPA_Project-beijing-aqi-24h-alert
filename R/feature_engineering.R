library(dplyr)
library(tidyr)
library(lubridate)
library(slider)

df <- read.csv("results/intermediate/cleaned_data.csv") %>%
  mutate(
    datetime  = make_datetime(year, month, day, hour),
    hour_sin  = sin(2 * pi * hour  / 24),
    hour_cos  = cos(2 * pi * hour  / 24),
    month_sin = sin(2 * pi * month / 12),
    month_cos = cos(2 * pi * month / 12)
  )

# Multi-horizon alert targets (24h is the headline; others used in extended analysis)
df <- df %>%
  group_by(station) %>%
  arrange(datetime) %>%
  mutate(
    Target_Alert_6h  = as.numeric(lead(AQI, 6)  > 150),
    Target_Alert_24h = as.numeric(lead(AQI, 24) > 150),
    Target_Alert_48h = as.numeric(lead(AQI, 48) > 150),
    Target_Alert_72h = as.numeric(lead(AQI, 72) > 150)
  ) %>%
  ungroup()

# Per-station lags + 24h trailing mean
df <- df %>%
  group_by(station) %>%
  arrange(datetime) %>%
  mutate(
    AQI_lag1   = lag(AQI, 1),
    AQI_lag24  = lag(AQI, 24),
    AQI_lag168 = lag(AQI, 168),
    AQI_roll24 = slide_dbl(AQI, mean, .before = 23, .complete = FALSE)
  ) %>%
  ungroup()

# Decompose wind direction string into U/V components
deg_lookup <- c(
  "N"=0,    "NNE"=22.5,  "NE"=45,   "ENE"=67.5,
  "E"=90,   "ESE"=112.5, "SE"=135,  "SSE"=157.5,
  "S"=180,  "SSW"=202.5, "SW"=225,  "WSW"=247.5,
  "W"=270,  "WNW"=292.5, "NW"=315,  "NNW"=337.5
)
df <- df %>%
  mutate(
    wd_deg = deg_lookup[wd],
    wind_u = WSPM * cos(wd_deg * pi / 180),
    wind_v = WSPM * sin(wd_deg * pi / 180)
  )

# Lagged wind: proxy for pollution transport from a few hours back
df <- df %>%
  group_by(station) %>%
  arrange(datetime) %>%
  mutate(
    wind_u_lag6  = lag(wind_u, 6),
    wind_v_lag6  = lag(wind_v, 6),
    wind_u_lag12 = lag(wind_u, 12),
    wind_v_lag12 = lag(wind_v, 12)
  ) %>%
  ungroup()

# Neighbor-station AQI as wide columns (current AQI of every other station)
spatial_wide <- df %>%
  select(datetime, station, AQI) %>%
  pivot_wider(names_from = station, values_from = AQI, names_prefix = "Neighbor_")
df <- df %>% left_join(spatial_wide, by = "datetime")

# K-means cluster features (requires clustering_analysis.R to have run)
if (file.exists("results/intermediate/station_clusters.csv")) {
  clusters <- read.csv("results/intermediate/station_clusters.csv")
  df <- df %>% left_join(clusters %>% select(station, Zone), by = "station")

  zone_means <- df %>%
    group_by(datetime, Zone) %>%
    summarise(cluster_mean_AQI = mean(AQI, na.rm = TRUE), .groups = "drop")
  df <- df %>% left_join(zone_means, by = c("datetime", "Zone"))

  df <- df %>%
    mutate(
      zone_industrial  = as.numeric(Zone == "Industrial/Heavy"),
      zone_mixed       = as.numeric(Zone == "Mixed/Urban"),
      zone_residential = as.numeric(Zone == "Residential/Green")
    )
} else {
  warning("station_clusters.csv not found - run clustering_analysis.R first")
}

# Drop rows where any feature we need is missing.
# We do this explicitly because silent NA propagation through glm() can hide failures.
df <- df %>%
  drop_na(Target_Alert_24h,
          AQI_lag1, AQI_lag24, AQI_lag168, AQI_roll24,
          wind_u_lag6, wind_v_lag6, wind_u_lag12, wind_v_lag12)

write.csv(df, "results/intermediate/featured_data.csv", row.names = FALSE)
