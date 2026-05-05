library(riem)
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)

# Beijing Capital Airport (ZBAA) — closest major weather station
wx <- riem_measures(
  station = "ZBAA",
  date_start = "2013-03-01",
  date_end = "2017-03-01"
)

era5_sub <- wx %>%
  mutate(
    datetime = floor_date(valid, unit = "hour") + hours(8),
    year  = year(datetime),
    month = month(datetime),
    day   = day(datetime),
    hour  = hour(datetime)
  ) %>%
  group_by(year, month, day, hour) %>%
  summarise(
    t2m_era5  = mean(tmpf, na.rm = TRUE),
    sp_era5   = mean(alti, na.rm = TRUE),
    u10_era5  = mean(sknt * cos(drct * pi/180), na.rm = TRUE),
    v10_era5  = mean(sknt * sin(drct * pi/180), na.rm = TRUE),
    tp_era5   = mean(p01i, na.rm = TRUE),
    tcwv      = mean(dwpf, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    datetime = make_datetime(year, month, day, hour),
    t2m_era5 = (t2m_era5 - 32) * 5/9,
    blh  = NA_real_,
    cape = NA_real_
  )

# Fill missing hours + interpolate NAs
full_hours <- data.frame(
  datetime = seq(min(era5_sub$datetime), max(era5_sub$datetime), by = "hour")
) %>%
  mutate(
    year  = year(datetime),
    month = month(datetime),
    day   = day(datetime),
    hour  = hour(datetime)
  )

era5_sub <- full_hours %>%
  left_join(era5_sub %>% select(-year, -month, -day, -hour), by = "datetime") %>%
  mutate(
    u10_era5 = zoo::na.approx(u10_era5, na.rm = FALSE),
    v10_era5 = zoo::na.approx(v10_era5, na.rm = FALSE),
    t2m_era5 = zoo::na.approx(t2m_era5, na.rm = FALSE),
    sp_era5  = zoo::na.approx(sp_era5,  na.rm = FALSE),
    tp_era5  = replace_na(tp_era5, 0),
    tcwv     = zoo::na.approx(tcwv, na.rm = FALSE),
    blh      = NA_real_,
    cape     = NA_real_
  )

write.csv(era5_sub, "data/era5_beijing_hourly.csv", row.names = FALSE)

message("NA summary for ERA5 substitute:")
print(colSums(is.na(era5_sub)))
message(sprintf("Date range: %s to %s", min(era5_sub$datetime), max(era5_sub$datetime)))
message(sprintf("Total hours expected: %d, got: %d",
                as.integer(difftime(max(era5_sub$datetime), min(era5_sub$datetime), units = "hours")),
                nrow(era5_sub)))
message(sprintf("Saved %d rows to data/era5_beijing_hourly.csv", nrow(era5_sub)))