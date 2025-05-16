# Predicting in each provinces ================================================-


library(terra)
library(dplyr)
library(sf)



province_nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_1.shp"
)

# Load a sample raster to check CRS alignment
aspect <- terra::rast("D:/MISC/Deforestation_modeling_rasters/Raster_Nepal/Aspect_SRTM_100m.tif")

# If the CRS is not the same, transform the shapefile to match the raster's CRS
if (st_crs(province_nepal) != crs(aspect)) {
  province_nepal <- st_transform(province_nepal, crs(aspect))
}

province_vect <- terra::vect(province_nepal)


# Rasterize the vector data using the aspect raster as a template
province_raster <- rasterize(province_vect, aspect, field = "PROVINCE") |>
  terra::resample(aspect)


province_df <- province_raster |> as.data.frame(xy = T)

write.csv(
  province_df,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Provincial/province_raster_data.csv",
  row.names = F
)


# -----------------------------------------------------------------------------=
# 1. TERAI -----------------------------------------------------------------------
# -----------------------------------------------------------------------------=

## Dataset 

terai <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_terai.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_terai_PAs.csv"
  )
)


# exporting forest only. 
forest_only <- terai %>% 
  mutate(
    popn_density = replace_na(popn_density, 0),
    night_light = replace_na(popn_density, 0)
  )%>% 
  mutate(
    lc_forest_2000 = ifelse(lc2000 == 4, 1, 0),
    lc_forest_2022 = ifelse(lc2022 == 4, 1, 0)
  ) %>% 
  mutate(
    lc_deforested = case_when(
      lc_forest_2000 ==1 & lc_forest_2022 == 0 ~ 1, 
      TRUE ~ 0
    )
  ) %>% 
  filter(
    lc_forest_2022 == 1
  ) %>% 
  mutate(
    deforested = case_when(
      lc_deforested == 1 | forest_loss == 1 ~ 1, 
      T ~ 0
    )
  ) %>% 
  dplyr::select(
    "x", "y", 
    "aspect", "dist_cropland", "dist_road", "dist_settlement", 
    "dist_stream", "elevation", "night_light", "popn_density", "slope",
    "deforested"
  )


## Merging two based on x and y coordinates

merged_data <- merge(forest_only, province_df, by = c("x", "y"), all.x = TRUE)


# exporting merged data. 
write.csv(
  merged_data,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Provincial/province_data_terai.csv",
  row.names = F
)


rm(list = setdiff(ls(), c("province_df")))
gc()


# -----------------------------------------------------------------------------=
# 2. SIWALIK -----------------------------------------------------------------------
# -----------------------------------------------------------------------------=

## Dataset 

siwalik <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_siwalik.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_siwalik_PAs.csv"
  )
)


# exporting forest only. 
forest_only <- siwalik %>% 
  mutate(
    popn_density = replace_na(popn_density, 0),
    night_light = replace_na(popn_density, 0)
  )%>% 
  mutate(
    lc_forest_2000 = ifelse(lc2000 == 4, 1, 0),
    lc_forest_2022 = ifelse(lc2022 == 4, 1, 0)
  ) %>% 
  mutate(
    lc_deforested = case_when(
      lc_forest_2000 ==1 & lc_forest_2022 == 0 ~ 1, 
      TRUE ~ 0
    )
  ) %>% 
  filter(
    lc_forest_2022 == 1
  ) %>% 
  mutate(
    deforested = case_when(
      lc_deforested == 1 | forest_loss == 1 ~ 1, 
      T ~ 0
    )
  ) %>% 
  dplyr::select(
    "x", "y", 
    "aspect", "dist_cropland", "dist_road", "dist_settlement", 
    "dist_stream", "elevation", "night_light", "popn_density", "slope",
    "deforested"
  )


## Merging two based on x and y coordinates

merged_data <- merge(forest_only, province_df, by = c("x", "y"), all.x = TRUE)


# exporting merged data. 
write.csv(
  merged_data,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Provincial/province_data_siwalik.csv",
  row.names = F
)


rm(list = setdiff(ls(), c("province_df")))
gc()



# -----------------------------------------------------------------------------=
# 3. MIDDLE MOUTNAINS -----------------------------------------------------------------------
# -----------------------------------------------------------------------------=

## Dataset 

middle_mountains <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_middle_mountains.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_middle_mountains_PAs.csv"
  )
)


# exporting forest only. 
forest_only <- middle_mountains %>% 
  mutate(
    popn_density = replace_na(popn_density, 0),
    night_light = replace_na(popn_density, 0)
  )%>% 
  mutate(
    lc_forest_2000 = ifelse(lc2000 == 4, 1, 0),
    lc_forest_2022 = ifelse(lc2022 == 4, 1, 0)
  ) %>% 
  mutate(
    lc_deforested = case_when(
      lc_forest_2000 ==1 & lc_forest_2022 == 0 ~ 1, 
      TRUE ~ 0
    )
  ) %>% 
  filter(
    lc_forest_2022 == 1
  ) %>% 
  mutate(
    deforested = case_when(
      lc_deforested == 1 | forest_loss == 1 ~ 1, 
      T ~ 0
    )
  ) %>% 
  dplyr::select(
    "x", "y", 
    "aspect", "dist_cropland", "dist_road", "dist_settlement", 
    "dist_stream", "elevation", "night_light", "popn_density", "slope",
    "deforested"
  )


## Merging two based on x and y coordinates

merged_data <- merge(forest_only, province_df, by = c("x", "y"), all.x = TRUE)


# exporting merged data. 
write.csv(
  merged_data,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Provincial/province_data_middle_mountains.csv",
  row.names = F
)


rm(list = setdiff(ls(), c("province_df")))
gc()



# -----------------------------------------------------------------------------=
# 4. HIGH MOUTNAINS -----------------------------------------------------------------------
# -----------------------------------------------------------------------------=

## Dataset 

high_mountains <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_high_mountains.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_high_mountains_PAs.csv"
  )
)


# exporting forest only. 
forest_only <- high_mountains %>% 
  mutate(
    popn_density = replace_na(popn_density, 0),
    night_light = replace_na(popn_density, 0)
  )%>% 
  mutate(
    lc_forest_2000 = ifelse(lc2000 == 4, 1, 0),
    lc_forest_2022 = ifelse(lc2022 == 4, 1, 0)
  ) %>% 
  mutate(
    lc_deforested = case_when(
      lc_forest_2000 ==1 & lc_forest_2022 == 0 ~ 1, 
      TRUE ~ 0
    )
  ) %>% 
  filter(
    lc_forest_2022 == 1
  ) %>% 
  mutate(
    deforested = case_when(
      lc_deforested == 1 | forest_loss == 1 ~ 1, 
      T ~ 0
    )
  ) %>% 
  dplyr::select(
    "x", "y", 
    "aspect", "dist_cropland", "dist_road", "dist_settlement", 
    "dist_stream", "elevation", "night_light", "popn_density", "slope",
    "deforested"
  )


## Merging two based on x and y coordinates

merged_data <- merge(forest_only, province_df, by = c("x", "y"), all.x = TRUE)


# exporting merged data. 
write.csv(
  merged_data,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Provincial/province_data_high_mountains.csv",
  row.names = F
)


rm(list = setdiff(ls(), c("province_df")))
gc()







# -----------------------------------------------------------------------------=
# 5. HIGH HIMALAYA -----------------------------------------------------------------------
# -----------------------------------------------------------------------------=

## Dataset 

high_himalaya <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_high_himalaya.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_high_himalaya_PAs.csv"
  )
)


province_df <- read.csv(
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Provincial/province_raster_data.csv"
)

# exporting forest only. 
forest_only <- high_himalaya %>% 
  mutate(
    popn_density = replace_na(popn_density, 0),
    night_light = replace_na(popn_density, 0)
  )%>% 
  mutate(
    lc_forest_2000 = ifelse(lc2000 == 4, 1, 0),
    lc_forest_2022 = ifelse(lc2022 == 4, 1, 0)
  ) %>% 
  mutate(
    lc_deforested = case_when(
      lc_forest_2000 ==1 & lc_forest_2022 == 0 ~ 1, 
      TRUE ~ 0
    )
  ) %>% 
  filter(
    lc_forest_2022 == 1
  ) %>% 
  mutate(
    deforested = case_when(
      lc_deforested == 1 | forest_loss == 1 ~ 1, 
      T ~ 0
    )
  ) %>% 
  dplyr::select(
    "x", "y", 
    "aspect", "dist_cropland", "dist_road", "dist_settlement", 
    "dist_stream", "elevation", "night_light", "popn_density", "slope",
    "deforested"
  )


## Merging two based on x and y coordinates

merged_data <- merge(forest_only, province_df, by = c("x", "y"), all.x = TRUE)


# exporting merged data. 
write.csv(
  merged_data,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Provincial/province_data_high_himalaya.csv",
  row.names = F
)


rm(list = setdiff(ls(), c("province_df")))
gc()







