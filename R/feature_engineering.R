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

# ── ERA5 weather reanalysis merge ──
era5_path <- "data/era5_beijing_hourly.csv"
has_era5 <- file.exists(era5_path)

if (has_era5) {
  message("Merging ERA5 weather reanalysis data...")
  era5 <- read.csv(era5_path) %>%
    mutate(datetime = make_datetime(year, month, day, hour)) %>%
    select(datetime, blh, cape, tp_era5, u10_era5, v10_era5,
           t2m_era5, sp_era5, tcwv)
  df <- df %>% left_join(era5, by = "datetime")
} else {
  message("ERA5 data not found. Continuing without ERA5 features.")
}

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

# Per-station lags + rolling statistics
df <- df %>%
  group_by(station) %>%
  arrange(datetime) %>%
  mutate(
    AQI_lag1   = lag(AQI, 1),
    AQI_lag24  = lag(AQI, 24),
    AQI_lag48  = lag(AQI, 48),
    AQI_lag72  = lag(AQI, 72),
    AQI_lag168 = lag(AQI, 168),
    AQI_roll24      = slide_dbl(AQI, mean, .before = 23, .complete = TRUE),
    AQI_roll24_sd   = slide_dbl(AQI, sd,   .before = 23, .complete = TRUE),
    AQI_roll72_mean = slide_dbl(AQI, mean, .before = 71, .complete = TRUE)
  ) %>%
  ungroup()

# ERA5 lagged features (if available)
if (has_era5) {
  df <- df %>%
    group_by(station) %>%
    arrange(datetime) %>%
    mutate(
      blh_lag24       = lag(blh, 24),
      cape_lag24      = lag(cape, 24),
      tp_era5_lag24   = lag(tp_era5, 24),
      tcwv_lag24      = lag(tcwv, 24),
      blh_roll24_mean = slide_dbl(blh, mean, .before = 23, .complete = TRUE),
      blh_roll24_sd   = slide_dbl(blh, sd,   .before = 23, .complete = TRUE)
    ) %>%
    ungroup()
}

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

# ── Holiday feature ──
# Chinese Spring Festival + National Day dates (2013-2017)
spring_festival <- c(
  # 2013 Spring Festival
  "2013-02-09","2013-02-10","2013-02-11","2013-02-12","2013-02-13","2013-02-14","2013-02-15",
  # 2014
  "2014-01-30","2014-01-31","2014-02-01","2014-02-02","2014-02-03","2014-02-04","2014-02-05",
  # 2015
  "2015-02-18","2015-02-19","2015-02-20","2015-02-21","2015-02-22","2015-02-23","2015-02-24",
  # 2016
  "2016-02-07","2016-02-08","2016-02-09","2016-02-10","2016-02-11","2016-02-12","2016-02-13",
  # 2017
  "2017-01-27","2017-01-28","2017-01-29","2017-01-30","2017-01-31","2017-02-01","2017-02-02"
)

national_day <- c(
  "2013-10-01","2013-10-02","2013-10-03","2013-10-04","2013-10-05","2013-10-06","2013-10-07",
  "2014-10-01","2014-10-02","2014-10-03","2014-10-04","2014-10-05","2014-10-06","2014-10-07",
  "2015-10-01","2015-10-02","2015-10-03","2015-10-04","2015-10-05","2015-10-06","2015-10-07",
  "2016-10-01","2016-10-02","2016-10-03","2016-10-04","2016-10-05","2016-10-06","2016-10-07"
)

all_holidays <- c(spring_festival, national_day)

df <- df %>%
  mutate(
    date_str           = as.character(as.Date(datetime)),
    is_holiday         = as.numeric(date_str %in% all_holidays),
    is_spring_festival = as.numeric(date_str %in% spring_festival)
  )
df <- df %>%
  mutate(
    AQI_x_wind_speed  = AQI_lag1 * WSPM,
    AQI_x_temp        = AQI_lag1 * TEMP,
    roll24_x_neighbor = AQI_roll24 * rowMeans(select(., starts_with("Neighbor_")), na.rm = TRUE),
    high_persist_flag = as.numeric(AQI_lag1 > 100 & AQI_lag24 > 100 & AQI_lag48 > 100)
  )
# Drop rows where any feature we need is missing.
# We do this explicitly because silent NA propagation through glm() can hide failures.
drop_cols <- c("Target_Alert_24h",
               "AQI_lag1", "AQI_lag24", "AQI_lag48", "AQI_lag72", "AQI_lag168",
               "AQI_roll24", "AQI_roll24_sd",
               "wind_u_lag6", "wind_v_lag6", "wind_u_lag12", "wind_v_lag12")

# Add ERA5 lag columns to drop_na list if they exist
if (has_era5) {
  era5_check <- read.csv(era5_path, nrows = 5)
  blh_available <- !all(is.na(era5_check$blh))
  drop_cols_era5 <- c("tp_era5_lag24", "tcwv_lag24")
  if (blh_available) {
    drop_cols_era5 <- c(drop_cols_era5, "blh_lag24", "cape_lag24",
                        "blh_roll24_mean", "blh_roll24_sd")
  }
  drop_cols <- c(drop_cols, drop_cols_era5)
}

# Track rows before and after dropping NAs
rows_before <- nrow(df)
df <- df %>% drop_na(all_of(drop_cols))
rows_after <- nrow(df)
cat(sprintf("Rows before lagging/NA removal: %d\nRows after: %d (%.1f%% retained)\n", 
            rows_before, rows_after, 100 * rows_after / rows_before))

write.csv(df, "results/intermediate/featured_data.csv", row.names = FALSE)
message(sprintf("Feature engineering complete: %d rows, %d columns", nrow(df), ncol(df)))