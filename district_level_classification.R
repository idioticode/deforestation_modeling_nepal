library(terra)
library(tidyverse)
library(sf)


ensemble_final <- read.csv(
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/prediction_total_ensemble.csv"
)

district_nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_2.shp"
) |>
  dplyr::select(
    DISTRICT
  )

# Load a sample raster to check CRS alignment
aspect <- terra::rast("D:/MISC/Deforestation_modeling_rasters/Raster_Nepal/Aspect_SRTM_100m.tif")

# If the CRS is not the same, transform the shapefile to match the raster's CRS
if (st_crs(district_nepal) != crs(aspect)) {
  district_nepal <- st_transform(district_nepal, crs(aspect))
}


district_raster <- terra::rasterize(district_nepal, aspect, field = "DISTRICT") |>
  terra::resample(aspect)

gc()
rm(aspect)

dist_raster_df <- as.data.frame(district_raster, xy = TRUE, na.rm = T)

# Merge the raster data with the ensemble final predictions based on x and y coordinates
merged_data <- merge(ensemble_final, dist_raster_df, by = c("x", "y"), all.x = F)


head(merged_data)


write.csv(
  merged_data,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/prediction_districts.csv"
)

