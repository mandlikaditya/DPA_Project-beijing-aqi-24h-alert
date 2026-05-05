"""
Download ERA5 hourly weather reanalysis for Beijing (2013-2017).

Prerequisites:
  1. pip install cdsapi xarray netcdf4 pandas
  2. Create account at https://cds.climate.copernicus.eu/
  3. Create ~/.cdsapirc with your API key:
       url: https://cds.climate.copernicus.eu/api
       key: YOUR_UID:YOUR_API_KEY

Run from project root:  python R/download_era5.py
"""

import cdsapi
import xarray as xr
import pandas as pd
import os

OUT_NC  = "data/era5_beijing_hourly.nc"
OUT_CSV = "data/era5_beijing_hourly.csv"

if os.path.exists(OUT_CSV):
    print(f"{OUT_CSV} already exists. Skipping download.")
    exit(0)

# Beijing bounding box: ~39.5-40.5N, 115.5-117.0E
c = cdsapi.Client()

c.retrieve(
    "reanalysis-era5-single-levels",
    {
        "product_type": "reanalysis",
        "format": "netcdf",
        "variable": [
            "10m_u_component_of_wind",
            "10m_v_component_of_wind",
            "2m_temperature",
            "boundary_layer_height",
            "convective_available_potential_energy",
            "surface_pressure",
            "total_column_water_vapour",
            "total_precipitation",
        ],
        "year":  [str(y) for y in range(2013, 2018)],
        "month": [f"{m:02d}" for m in range(1, 13)],
        "day":   [f"{d:02d}" for d in range(1, 32)],
        "time":  [f"{h:02d}:00" for h in range(24)],
        "area":  [40.5, 115.5, 39.5, 117.0],
    },
    OUT_NC,
)

print("Converting NetCDF to CSV...")
ds = xr.open_dataset(OUT_NC)

# Average across the small spatial grid to get one value per hour
df = ds.mean(dim=["latitude", "longitude"]).to_dataframe().reset_index()

# Rename columns to match R pipeline expectations
rename_map = {
    "time":                                "datetime",
    "u10":                                 "u10_era5",
    "v10":                                 "v10_era5",
    "t2m":                                 "t2m_era5",
    "blh":                                 "blh",
    "cape":                                "cape",
    "sp":                                  "sp_era5",
    "tcwv":                                "tcwv",
    "tp":                                  "tp_era5",
}
df = df.rename(columns=rename_map)

# Add year/month/day/hour columns to match cleaned_data.csv structure
df["year"]  = df["datetime"].dt.year
df["month"] = df["datetime"].dt.month
df["day"]   = df["datetime"].dt.day
df["hour"]  = df["datetime"].dt.hour

df.to_csv(OUT_CSV, index=False)
print(f"Saved {len(df)} rows to {OUT_CSV}")

# Clean up NetCDF
if os.path.exists(OUT_NC):
    os.remove(OUT_NC)
    print(f"Removed {OUT_NC}")
