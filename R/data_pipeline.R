# Run from project root: source("R/data_pipeline.R")

# Set seed for reproducibility
set.seed(42)

required_pkgs <- c("dplyr", "tidyr", "lubridate", "purrr",
                   "ggplot2", "corrplot",
                   "slider", "forecast",
                   "xgboost", "pROC")

missing <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

for (d in c("results/intermediate", "results/final", "plots")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Check if ERA5 data exists; if not, print instructions
if (!file.exists("data/era5_beijing_hourly.csv")) {
  message("NOTE: ERA5 data not found at data/era5_beijing_hourly.csv")
  message("Run: python R/download_era5.py   (requires cdsapi + xarray)")
  message("Pipeline will continue WITHOUT ERA5 features.")
}

source("R/clean_air_quality.R")
source("R/statistical_tests.R")
source("R/eda_plots.R")
source("R/time_series_analysis.R")
source("R/spatial_analysis.R")
source("R/clustering_analysis.R")
source("R/feature_engineering.R")
source("R/model_ladder.R")