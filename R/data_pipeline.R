# Run from project root: source("R/data_pipeline.R")

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

source("R/clean_air_quality.R")
source("R/statistical_tests.R")
source("R/eda_plots.R")
source("R/time_series_analysis.R")
source("R/spatial_analysis.R")
source("R/clustering_analysis.R")
source("R/feature_engineering.R")
source("R/model_ladder.R")
