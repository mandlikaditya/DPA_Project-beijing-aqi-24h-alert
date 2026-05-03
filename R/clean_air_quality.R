library(dplyr)
library(tidyr)
library(purrr)

files <- list.files("data/beijing_csvs/",
                    pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

if (length(files) == 0) {
  stop("No CSV files found in data/beijing_csvs/")
}

df <- map_dfr(files, read.csv)

# Down-up fill: forward, then back. Preserves station-local trend better
# than mean imputation when sensors miss a few hours in a row.
df_clean <- df %>%
  group_by(station) %>%
  arrange(year, month, day, hour) %>%
  fill(PM2.5, PM10, SO2, NO2, CO, O3, TEMP, PRES, DEWP, RAIN, WSPM,
       .direction = "downup") %>%
  ungroup()

# IAQI sub-index per HJ 633-2012 (piecewise linear interpolation between breakpoints)
calc_iaqi_vec <- function(val, breakpoints, iaqi_pts) {
  res <- rep(NA, length(val))
  res[val <= 0] <- 0

  idx_low <- which(val > 0 & val <= breakpoints[1])
  if (length(idx_low) > 0) {
    res[idx_low] <- (iaqi_pts[1] / breakpoints[1]) * val[idx_low]
  }

  for (i in 2:length(breakpoints)) {
    low_v <- breakpoints[i-1]; high_v <- breakpoints[i]
    low_i <- iaqi_pts[i-1];    high_i <- iaqi_pts[i]
    idx <- which(val > low_v & val <= high_v)
    if (length(idx) > 0) {
      res[idx] <- ((high_i - low_i) / (high_v - low_v)) * (val[idx] - low_v) + low_i
    }
  }

  res[val > tail(breakpoints, 1)] <- tail(iaqi_pts, 1)
  ceiling(res)
}

iaqi_pts <- c(50, 100, 150, 200, 300, 400, 500)

df_clean <- df_clean %>%
  mutate(
    IAQI_PM2.5 = calc_iaqi_vec(PM2.5,    c(35, 75, 115, 150, 250, 350, 500),       iaqi_pts),
    IAQI_PM10  = calc_iaqi_vec(PM10,     c(50, 150, 250, 350, 420, 500, 600),      iaqi_pts),
    IAQI_SO2   = calc_iaqi_vec(SO2,      c(150, 500, 800, 1600, 2100, 2620, 3100), iaqi_pts),
    IAQI_NO2   = calc_iaqi_vec(NO2,      c(100, 200, 700, 1200, 2340, 3090, 3840), iaqi_pts),
    IAQI_CO    = calc_iaqi_vec(CO/1000,  c(5, 10, 35, 60, 90, 120, 150),           iaqi_pts),
    IAQI_O3    = calc_iaqi_vec(O3,       c(160, 200, 300, 400, 800, 1000, 1200),   iaqi_pts)
  )

df_clean$AQI <- pmax(df_clean$IAQI_PM2.5, df_clean$IAQI_PM10, df_clean$IAQI_SO2,
                     df_clean$IAQI_NO2,   df_clean$IAQI_CO,   df_clean$IAQI_O3,
                     na.rm = TRUE)

write.csv(df_clean, "results/intermediate/cleaned_data.csv", row.names = FALSE)
