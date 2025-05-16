# packages, 
library(dplyr)

## TERAI ----------------------------

terai_models <- readRDS(
  "D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_terai.rds"
)



terai_forest_2022 <- terai |>
  select(lc2022)|>
  mutate(
    forest_cover = ifelse(lc2022 == 4, 1, 0)
  )|>
  filter(forest_cover == 1)

cat("Terai Forest Area:", nrow(terai_forest_2022) /100)


## SIWALIK --------------------------------------------------

siwalik <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_siwalik.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_siwalik_PAs.csv"
  )
)


siwalik_forest_2022 <- siwalik |>
  select(lc2022) |>
  mutate(
    forest_cover = ifelse(lc2022 == 4, 1, 0)
  ) |>
  filter(forest_cover == 1)

cat("Siwalik Forest Area:", nrow(siwalik_forest_2022) / 100)


## MIDDLE MOUNTAINS ----------------------------------------
middle_mountains <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_middle_mountains.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_middle_mountains_PAs.csv"
  )
)


middle_mountains_forest_2022 <- middle_mountains |>
  select(lc2022) |>
  mutate(
    forest_cover = ifelse(lc2022 == 4, 1, 0)
  ) |>
  filter(forest_cover == 1)

cat("Middle Mountains Forest Area:", nrow(middle_mountains_forest_2022) / 100)


## HIGH MOUNTAINS ------------------------------------------------------------
high_mountains <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_high_mountains.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_high_mountains_PAs.csv"
  )
)

high_mountains_forest_2022 <- high_mountains |>
  select(lc2022) |>
  mutate(
    forest_cover = ifelse(lc2022 == 4, 1, 0)
  ) |>
  filter(forest_cover == 1)

cat("High Mountains Forest Area:", nrow(high_mountains_forest_2022) / 100)


## HIGH HIMALAYAS ------------------------------------------------
high_himalaya <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_high_himalaya.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_high_himalaya_PAs.csv"
  )
)



high_himalaya_forest_2022 <- high_himalaya |>
  select(lc2022) |>
  mutate(
    forest_cover = ifelse(lc2022 == 4, 1, 0)
  ) |>
  filter(forest_cover == 1)

cat("High Himalaya Forest Area:", nrow(high_himalaya_forest_2022) / 100)
