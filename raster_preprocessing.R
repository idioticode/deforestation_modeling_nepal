#------------------------------------------------------------------------------=
# 1. Masking Population Density ------------------------------------------------
#------------------------------------------------------------------------------=
clear_memory <- function() {
  rm(list = ls())
  gc()
}

# loading the necessary packages. 
library(terra)

# loading the first tile. 
r1 <- terra::rast(
"D:/Drive/Deforestation Modeling/GIS files/Population density/GHS_BUILT_S_E2025_GLOBE_R2023A_4326_3ss_V1_0_R6_C27/GHS_BUILT_S_E2025_GLOBE_R2023A_4326_3ss_V1_0_R6_C27.tif"
)


# loading the second tile.
r2 <- terra::rast(
  "D:/Drive/Deforestation Modeling/GIS files/Population density/GHS_BUILT_S_E2025_GLOBE_R2023A_4326_3ss_V1_0_R7_C27/GHS_BUILT_S_E2025_GLOBE_R2023A_4326_3ss_V1_0_R7_C27.tif"
)


# loading the shape file of Nepal. 
nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_0.shp"
)


# cropping both the raster files using the shape file of Nepal. 

#
r1_crop <- r1 |> crop(nepal)
r1_mask <- r1_crop |> mask(nepal)

#
r2_crop <- r2 |> crop(nepal)
r2_mask <- r2_crop|> mask(nepal)


# renaming the raster column names
names(r1_mask) <- "population_density"
names(r2_mask) <- "population_density"


plot(r1_mask)
plot(r2_mask)


# merging the two raster files
merged_nepal <- merge(r1_mask, r2_mask)

plot(merged_nepal)

# exporting the raster file locally. 
writeRaster(
  merged_nepal,
  "D:/Drive/Deforestation Modeling/GIS files/Population density/raster/population_density_GHS_2025_100m.tif",
  overwrite = T
)



#------------------------------------------------------------------------------=
# 2. Masking Raster Files in Study Regions  ------------------------------------
#------------------------------------------------------------------------------=

# All the raster files have been resampled in ArcMap to match the extent. 
# Masking the raster files in CTML and mountain

# loading necessary libraries
library(terra)

# loading the shape file of physiographic regions of Nepal. 
physiography_nepal <- sf::st_read(
  "D:/MISC/points/Physiography/physiography_nepal_updated.shp"
)|>
  dplyr::select(Physio)

# folder path to original raster files. 
input_dir <- "D:/MISC/Deforestation_modeling_rasters/Raster_Nepal"

# input raster files 
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)


## CTML -----------------------------------------------------------------------

# shape file of chure Terai Madhesh Landscape (Siwalik and Tarai)
siwalik_terai <- physiography_nepal |>
  dplyr::filter(Physio %in% c("Siwalik" ,"Terai"  ))

# output directory. 
dir.create("D:/MISC/Deforestation_modeling_rasters/Rasters_CTML")

output_dir_ctml <- "D:/MISC/Deforestation_modeling_rasters/Rasters_CTML"

# Loop through each raster file
for (tif_file in tif_files) {
  # Load the raster
  raster_data <- rast(tif_file)
  
  # Crop the raster to the extent of siwalik_terai
  cropped_raster <- crop(raster_data, siwalik_terai)
  
  # Mask the cropped raster using siwalik_terai
  masked_raster <- mask(cropped_raster, siwalik_terai)
  
  # Generate the output file name with the _CTML suffix
  file_name <- basename(tif_file)  # Get the base file name (e.g., "raster1.tif")
  output_file <- file.path(output_dir_ctml, sub("\\.tif$", "_CTML.tif", file_name))  # Add suffix
  
  # Save the cropped and masked raster
  writeRaster(masked_raster, filename = output_file, overwrite = TRUE)
  
  # Print a message to confirm
  message("Processed and saved: ", output_file)
}


## MOUNTAIN -----------------------------------------------------------------------

# shape file of Middle Mountain and High Mountain
mountain <- physiography_nepal |>
  dplyr::filter(Physio %in% c("High Mountains", "Middle Mountains"))

# output directory. 
dir.create("D:/MISC/Deforestation_modeling_rasters/Rasters_Mountain")

output_dir_mountain <- "D:/MISC/Deforestation_modeling_rasters/Rasters_Mountain"

# Loop through each raster file
for (tif_file in tif_files) {
  # Load the raster
  raster_data <- rast(tif_file)
  
  # Crop the raster to the extent of mountain
  cropped_raster <- crop(raster_data, mountain)
  
  # Mask the cropped raster using mountain
  masked_raster <- mask(cropped_raster, mountain)
  
  # Generate the output file name with the _CTML suffix
  file_name <- basename(tif_file)  # Get the base file name (e.g., "raster1.tif")
  output_file <- file.path(output_dir_mountain, sub("\\.tif$", "_Mountain.tif", file_name))  # Add suffix
  
  # Save the cropped and masked raster
  writeRaster(masked_raster, filename = output_file, overwrite = TRUE)
  
  # Print a message to confirm
  message("Processed and saved: ", output_file)
}



# ----------------------------------------------------------------------------=
# 3. Raster to CSV -----------------------------------------------------------
# ----------------------------------------------------------------------------=

# Stacking all the raster files, converting into data frame and finally exporting as csv. 

# Loading necessary packages. 
library(terra)
library(tidyverse)

## CTML -----------------------------------------------------------------------

# Define the directory path
raster_dir <- "D:/MISC/Deforestation_modeling_rasters/Rasters_CTML"

# List all .tif files in the directory
tif_files <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE)

# Load all raster files into a list
raster_list <- lapply(tif_files, rast)

# Stack all rasters
raster_stack <- do.call(c, raster_list)

names(raster_stack)

# Define short names for the layers
short_names <- c("aspect", "dist_cropland", "dist_road", "dist_settlement", "dist_stream", 
                 "elevation", "forest_loss", "ndvi_2000", "ndvi_2024", "nighttime_light", 
                 "popn_density", "slope"
                 )

names(raster_stack) <- short_names


df <- as.data.frame(raster_stack, xy = T)

# exporting the data frame of all the rasters as as a csv file. 
write.csv(
  df,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/raster_data_all_CTML.csv",
  row.names = F
)




## Mountain --------------------------------

library(terra)
library(tidyverse)

# Define the directory path
raster_dir <- "D:/MISC/Deforestation_modeling_rasters/Rasters_Mountain"

# List all .tif files in the directory
tif_files <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE)

# Load all raster files into a list
raster_list <- lapply(tif_files, rast)

# Stack all rasters
raster_stack <- do.call(c, raster_list)

names(raster_stack)

# Define short names for the layers
short_names <- c("aspect", "dist_cropland", "dist_road", "dist_settlement", "dist_stream", 
                 "elevation", "forest_loss", "ndvi_2000", "ndvi_2024", "nighttime_light", 
                 "popn_density", "slope"
)

names(raster_stack) <- short_names

df <- as.data.frame(raster_stack, xy = T)

# exporting the data frame of all the rasters as as a csv file. 
write.csv(
  df,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/raster_data_all_Mountain.csv",
  row.names = F
)

## All ------------------------------------------------------------------------

mountain_data <- read.csv(
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/raster_data_all_Mountain.csv",
)


ctml_data <- read.csv(
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/raster_data_all_CTML.csv",
)


total_data <- rbind(df, ctml_data)

write.csv(
  total_data,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/raster_data_all.csv",
  row.names = F
)




# ----------------------------------------------------------------------------=
# 4. Excluding PAs ------------------------------------------------------------
# ----------------------------------------------------------------------------=


# Load necessary libraries
library(sf)
library(terra)
library(dplyr)

# Load the shapefile for protected areas
protected_area <- sf::st_read("D:/MISC/BIODIVERSITY IMPORTANT AREA/PROTECTED AREA/protected_areas_final.shp")

# Load the shapefile of physiographic regions of Nepal
physiography_nepal <- sf::st_read("D:/MISC/points/Physiography/physiography_nepal_updated.shp") %>%
  dplyr::select(Physio) 

# Load a sample raster to check CRS alignment
aspect <- terra::rast("D:/MISC/Deforestation_modeling_rasters/Raster_Nepal/Aspect_SRTM_100m.tif")

# If the CRS is not the same, transform the shapefile to match the raster's CRS
if (st_crs(protected_area) != crs(aspect)) {
  protected_area <- st_transform(protected_area, crs(aspect))
}

# Folder path to original raster files
input_dir <- "D:/MISC/Deforestation_modeling_rasters/Raster_Nepal"

# List all .tif files in the input directory
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

# Convert the protected area shapefile to a SpatVector object (required by terra)
protected_area_vect <- terra::vect(protected_area)

# List of physiographic regions to process
regions <- unique(physiography_nepal$Physio)

# Output directory for saving stacked raster files
output_dir <- "D:/MISC/Deforestation_modeling_rasters/Raster_individual_areas"


# Loop through each physiographic region
for (region_name in regions) {
  # Filter the shapefile for the current region
  region_shape <- physiography_nepal %>%
    dplyr::filter(Physio == region_name)
  
  # Initialize an empty list to store processed rasters
  processed_rasters <- list()
  
  # Loop through each raster file
  for (tif_file in tif_files) {
    # Load the raster
    raster_data <- terra::rast(tif_file)
    
    # Step 1: Exclude protected areas from the raster
    # Create a mask for the protected areas
    protected_mask <- terra::rasterize(protected_area_vect, raster_data, field = 1, background = 0) %>%
      terra::resample(raster_data, method = "near")
    
    # Apply the inverted mask to the raster to exclude protected areas
    raster_excluded <- terra::mask(raster_data, protected_mask, maskvalues = 1)
    
    # Step 2: Crop and mask the raster using the region shapefile
    # Crop the raster to the extent of the region
    cropped_raster <- terra::crop(raster_excluded, region_shape)
    
    # Mask the cropped raster using the region shapefile
    masked_raster <- terra::mask(cropped_raster, region_shape)
    
    # Append the processed raster to the list
    processed_rasters <- append(processed_rasters, list(masked_raster))
  }
  
  # Step 3: Stack all processed rasters together
  stacked_raster <- terra::rast(processed_rasters)
  
  # Step 4: Save the stacked raster as a .tif file
  output_file <- file.path(output_dir, paste0("stacked_raster_", tolower(gsub(" ", "_", region_name)), ".tif"))
  terra::writeRaster(stacked_raster, output_file, filetype = "GTiff", overwrite = TRUE)
  
  # Print a message indicating completion for the current region
  message("Processed and saved stacked raster for region: ", region_name)
}



## Extracting data frame --------------------------------

library(terra)

## high_himalaya -------------
high_himalaya <- rast(
  "D:/MISC/Deforestation_modeling_rasters/Raster_individual_areas/stacked_raster_high_himalaya.tif"
)

high_himalaya_df <- as.data.frame(high_montains, xy = T)
high_himalaya_df$region <- 'high_himalaya'


write.csv(
  high_himalaya_df,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_high_himalaya.csv", 
  row.names = F
)



## high_mountains ----
high_mountains <- rast(
  "D:/MISC/Deforestation_modeling_rasters/Raster_individual_areas/stacked_raster_high_mountains.tif"
)

# Convert the raster to a data frame
high_mountains_df <- as.data.frame(high_mountains, xy = TRUE)
high_mountains_df$region <- 'high_mountains'

names(high_mountains_df)

high_mountains_df <- high_mountains_df %>%
  rename(
    aspect = aspect,
    dist_cropland = Distance_from_Cropland_100m,
    dist_road = Distance_from_Road_100m,
    dist_settlement = Distance_from_Settlement_100m,
    dist_stream = Distance_from_Stream_100m,
    elevation = elevation,
    forest_loss = Forest_Cover_Loss_2000_2023_100m,
    lc2000 = lc2000,
    lc2022 = lc2022,
    ndvi_2000 = NDVI_2000_LANDSAT7_100m,
    ndvi_2024 = NDVI_2024_LANDSAT8_100m,
    night_light = Nighttime_Light_WB_100m,
    popn_density = Population_Density_GHS_2025_100m,
    slope = slope,
    region = region
  )

# Check the new column names
names(high_mountain_df)

head(high_mountains_df, 3)

# Save the data frame as a CSV file
write.csv(
  high_mountains_df,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_high_mountains.csv", 
  row.names = FALSE
)



## middle_mountains ---------
library(terra)
library(dplyr)

# Load tterra# Load the stacked raster for middle_mountains
middle_mountains <- rast(
  "D:/MISC/Deforestation_modeling_rasters/Raster_individual_areas/stacked_raster_middle_mountains.tif"
)

# Convert the raster to a data frame
middle_mountains_df <- as.data.frame(middle_mountains, xy = TRUE)
middle_mountains_df$region <- 'middle_mountains'


middle_mountains_df <- middle_mountains_df %>%
  rename(
    aspect = aspect,
    dist_cropland = Distance_from_Cropland_100m,
    dist_road = Distance_from_Road_100m,
    dist_settlement = Distance_from_Settlement_100m,
    dist_stream = Distance_from_Stream_100m,
    elevation = elevation,
    forest_loss = Forest_Cover_Loss_2000_2023_100m,
    lc2000 = lc2000,
    lc2022 = lc2022,
    ndvi_2000 = NDVI_2000_LANDSAT7_100m,
    ndvi_2024 = NDVI_2024_LANDSAT8_100m,
    night_light = Nighttime_Light_WB_100m,
    popn_density = Population_Density_GHS_2025_100m,
    slope = slope,
    region = region
  )

head(middle_mountains_df, 3)

# Save the data frame as a CSV file
write.csv(
  middle_mountains_df,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_middle_mountains.csv", 
  row.names = FALSE
)


##  siwalik ------------

# Load the stacked raster for siwalik
siwalik <- rast(
  "D:/MISC/Deforestation_modeling_rasters/Raster_individual_areas/stacked_raster_siwalik.tif"
)

# Convert the raster to a data frame
siwalik_df <- as.data.frame(siwalik, xy = TRUE)
siwalik_df$region <- 'siwalik'


siwalik_df <- siwalik_df %>%
  rename(
    aspect = aspect,
    dist_cropland = Distance_from_Cropland_100m,
    dist_road = Distance_from_Road_100m,
    dist_settlement = Distance_from_Settlement_100m,
    dist_stream = Distance_from_Stream_100m,
    elevation = elevation,
    forest_loss = Forest_Cover_Loss_2000_2023_100m,
    lc2000 = lc2000,
    lc2022 = lc2022,
    ndvi_2000 = NDVI_2000_LANDSAT7_100m,
    ndvi_2024 = NDVI_2024_LANDSAT8_100m,
    night_light = Nighttime_Light_WB_100m,
    popn_density = Population_Density_GHS_2025_100m,
    slope = slope,
    region = region
  )

head(siwalik_df, 3)

# Save the data frame as a CSV file
write.csv(
  siwalik_df,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_siwalik.csv", 
  row.names = FALSE
)

## terai-----------
library(terra)
library(dplyr)

# Load the stacked raster for terai
terai <- rast(
  "D:/MISC/Deforestation_modeling_rasters/Raster_individual_areas/stacked_raster_terai.tif"
)

# Convert the raster to a data frame
terai_df <- as.data.frame(terai, xy = TRUE)
terai_df$region <- 'terai'


terai_df <- terai_df %>%
  rename(
    aspect = aspect,
    dist_cropland = Distance_from_Cropland_100m,
    dist_road = Distance_from_Road_100m,
    dist_settlement = Distance_from_Settlement_100m,
    dist_stream = Distance_from_Stream_100m,
    elevation = elevation,
    forest_loss = Forest_Cover_Loss_2000_2023_100m,
    lc2000 = lc2000,
    lc2022 = lc2022,
    ndvi_2000 = NDVI_2000_LANDSAT7_100m,
    ndvi_2024 = NDVI_2024_LANDSAT8_100m,
    night_light = Nighttime_Light_WB_100m,
    popn_density = Population_Density_GHS_2025_100m,
    slope = slope,
    region = region
  )

head(terai_df, 3)

# Save the data frame as a CSV file
write.csv(
  terai_df,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_terai.csv", 
  row.names = FALSE
)


# ----------------------------------------------------------------------------=
# 4. PAs data frame------------------------------------------------------------
# ----------------------------------------------------------------------------=


## Exporting Raster Files of PAs ----------------------------------------------

# Load necessary libraries
library(sf)
library(terra)
library(dplyr)

# Load the shapefile for protected areas
protected_area <- sf::st_read("D:/MISC/BIODIVERSITY IMPORTANT AREA/PROTECTED AREA/protected_areas_final.shp")

# Load the shapefile of physiographic regions of Nepal
physiography_nepal <- sf::st_read("D:/MISC/points/Physiography/physiography_nepal_updated.shp") %>%
  dplyr::select(Physio)  

# Load a sample raster to check CRS alignment
aspect <- terra::rast("D:/MISC/Deforestation_modeling_rasters/Raster_Nepal/Aspect_SRTM_100m.tif")

# If the CRS is not the same, transform the shapefile to match the raster's CRS
if (st_crs(protected_area) != crs(aspect)) {
  protected_area <- st_transform(protected_area, crs(aspect))
}

# Folder path to original raster files
input_dir <- "D:/MISC/Deforestation_modeling_rasters/Raster_Nepal"

# List all .tif files in the input directory
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

# Stack all raster files
stacked_tif_files <- rast(tif_files)

# Define the output directory for saving stacked raster files
output_raster_dir <- "D:/MISC/Deforestation_modeling_rasters/Raster_individual_protected_areas"


# Get unique physiographic regions
physio_regions <- unique(physiography_nepal$Physio)

physio_regions

# Loop through each region to crop, mask, and save the stacked raster
for (region in physio_regions) {
  # Filter the physiographic region
  region_shape <- physiography_nepal |> filter(Physio == region)
  
  # Crop and mask the stacked raster files to the region
  masked_tif_files <- stacked_tif_files |> crop(region_shape) |> mask(region_shape)
  
  # Define the output file name
  output_file <- file.path(output_raster_dir, paste0("stacked_raster_", tolower(gsub(" ", "_", region)), "_PAs.tif"))
  
  # Save the masked raster to a .tif file
  writeRaster(masked_tif_files, output_file, overwrite = TRUE)
  
  # Print a message indicating the region has been processed
  cat("Processed and saved stacked raster for region:", region, "\n")
  
  # Clear memory
  rm(masked_tif_files)
  gc()  # Force garbage collection to free up memory
}


## Saving PAs as DataFrame -----------------------------------------------------

# Define the output directory for CSV files
output_csv_dir <- "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions"

# Ensure the output directory exists
if (!dir.exists(output_csv_dir)) {
  dir.create(output_csv_dir, recursive = TRUE)
}

physio_regions <- c("High Himalaya", "Middle Mountains", "High Mountains", "Siwalik", "Terai")
output_raster_dir <- "D:/MISC/Deforestation_modeling_rasters/Raster_individual_protected_areas"

# Loop through each region to load the saved raster and convert to a data frame
for (region in physio_regions) {
  # Define the input raster file name
  input_file <- file.path(output_raster_dir, paste0("stacked_raster_", tolower(gsub(" ", "_", region)), "_PAs.tif"))
  
  # Load the saved raster file
  region_raster <- rast(input_file)
  
  # Convert the raster to a data frame
  region_df <- as.data.frame(region_raster, xy = TRUE)
  region_df$region <- region
  
  # Rename columns
  region_df <- region_df %>% 
    rename(
      aspect = aspect,
      dist_cropland = Distance_from_Cropland_100m,
      dist_road = Distance_from_Road_100m,
      dist_settlement = Distance_from_Settlement_100m,
      dist_stream = Distance_from_Stream_100m,
      elevation = elevation,
      forest_loss = Forest_Cover_Loss_2000_2023_100m,
      lc2000 = lc2000,
      lc2022 = lc2022,
      ndvi_2000 = NDVI_2000_LANDSAT7_100m,
      ndvi_2024 = NDVI_2024_LANDSAT8_100m,
      night_light = Nighttime_Light_WB_100m,
      popn_density = Population_Density_GHS_2025_100m,
      slope = slope,
      region = region
    )
  
  # Define the output CSV file name
  output_file <- file.path(output_csv_dir, paste0("raster_df_", tolower(gsub(" ", "_", region)), "_PAs.csv"))
  
  # Write the data frame to a CSV file
  write.csv(region_df, output_file, row.names = FALSE)
  
  # Print a message indicating the region has been processed
  cat("Processed and exported data for region:", region, "\n")
  
  # Clear memory
  rm(region_raster, region_df)
  gc()  # Force garbage collection to free up memory
}
