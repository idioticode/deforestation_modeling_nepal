#-----------------------------------------------------------------------------=
# TERAI REGION ---------------------------------------------------------------
#-----------------------------------------------------------------------------=

## 1. LOADING NECESSARY PACKAGES ------------------------------------------------

library(tidyverse)
library(randomForest)
library(xgboost)


## 2. MODELS AND DATASETS -------------------------------------------------------


terai_models <- readRDS(
"D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_terai.rds"
)



rf_model <- terai_models$rf

xgb_model <- terai_models$xgb

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



nrow(
  forest_only %>% 
    filter(!complete.cases(.))
)

## 3. PREDICTION ---------------------------------------------------------------

rf_predict <- predict(rf_model, forest_only, type = "prob")


predictor_vars <- c("aspect", "dist_cropland", "dist_road", "dist_settlement", 
                    "dist_stream", "elevation", "night_light", "popn_density", "slope")


forest_only_matrix <- xgb.DMatrix(
  data = as.matrix(forest_only[, predictor_vars]),  
  label = forest_only$deforested
)


xgb_predict <- predict(xgb_model, forest_only_matrix)
xgb_predict

max(xgb_predict)
min(xgb_predict)

final_df <- data.frame(
  x = forest_only$x,
  y = forest_only$y,
  rf_predict = rf_predict[ , '1'],
  xgb_predict  = xgb_predict
)



write.csv(
  final_df,
  "D:/Drive/Deforestation Modeling/Working Documents/Prediction CSV/prediction_terai_physiography.csv",
  row.names = F
)

rm(list = ls())
gc()

#-----------------------------------------------------------------------------=
# SIWALIK REGION ---------------------------------------------------------------
#-----------------------------------------------------------------------------=


## 1. LOADING NECESSARY PACKAGES ------------------------------------------------

library(tidyverse)
library(randomForest)
library(xgboost)


## 2. MODELS AND DATASETS -------------------------------------------------------


siwalik_models <- readRDS(
"D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_siwalik.rds"
)



rf_model <- siwalik_models$rf

xgb_model <- siwalik_models$xgb

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



nrow(
  forest_only %>% 
    filter(!complete.cases(.))
)

## 3. PREDICTION ---------------------------------------------------------------

rf_predict <- predict(rf_model, forest_only, type = "prob")


predictor_vars <- c("aspect", "dist_cropland", "dist_road", "dist_settlement", 
                    "dist_stream", "elevation", "night_light", "popn_density", "slope")


forest_only_matrix <- xgb.DMatrix(
  data = as.matrix(forest_only[, predictor_vars]),  
  label = forest_only$deforested
)


xgb_predict <- predict(xgb_model, forest_only_matrix)

final_df <- data.frame(
  x = forest_only$x,
  y = forest_only$y,
  rf_predict = rf_predict[ , '1'],
  xgb_predict  = xgb_predict
)



write.csv(
  final_df,
  "D:/Drive/Deforestation Modeling/Working Documents/Prediction CSV/prediction_siwalik_physiography.csv",
  row.names = F
)



rm(list = ls())
gc()


#-----------------------------------------------------------------------------=
# MIDDLE MOUNTAINS REGION ---------------------------------------------------------------
#-----------------------------------------------------------------------------=


## 1. LOADING NECESSARY PACKAGES ------------------------------------------------

library(tidyverse)
library(randomForest)
library(xgboost)


## 2. MODELS AND DATASETS -------------------------------------------------------

middle_mountains_models <- readRDS(
  "D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_middle_mountains.rds"
  )



rf_model <- middle_mountains_models$rf

xgb_model <- middle_mountains_models$xgb

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



nrow(
  forest_only %>% 
    filter(!complete.cases(.))
)

## 3. PREDICTION ---------------------------------------------------------------

rf_predict <- predict(rf_model, forest_only, type = "prob")


predictor_vars <- c("aspect", "dist_cropland", "dist_road", "dist_settlement", 
                    "dist_stream", "elevation", "night_light", "popn_density", "slope")


forest_only_matrix <- xgb.DMatrix(
  data = as.matrix(forest_only[, predictor_vars]),  
  label = forest_only$deforested
)


xgb_predict <- predict(xgb_model, forest_only_matrix)

final_df <- data.frame(
  x = forest_only$x,
  y = forest_only$y,
  rf_predict = rf_predict[ , '1'],
  xgb_predict  = xgb_predict
)



write.csv(
  final_df,
  "D:/Drive/Deforestation Modeling/Working Documents/Prediction CSV/prediction_middle_mountains_physiography.csv",
  row.names = F
)

rm(list = ls())
gc()


#-----------------------------------------------------------------------------=
# HIGH MOUNTAINS REGION ---------------------------------------------------------------
#-----------------------------------------------------------------------------=


## 1. LOADING NECESSARY PACKAGES ------------------------------------------------

library(tidyverse)
library(randomForest)
library(xgboost)


## 2. MODELS AND DATASETS -------------------------------------------------------

high_mountains_models <- readRDS(
  "D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_high_mountains.rds"
)



rf_model <- high_mountains_models$rf

xgb_model <- high_mountains_models$xgb

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



nrow(
  forest_only %>% 
    filter(!complete.cases(.))
)

## 3. PREDICTION ---------------------------------------------------------------

rf_predict <- predict(rf_model, forest_only, type = "prob")


predictor_vars <- c("aspect", "dist_cropland", "dist_road", "dist_settlement", 
                    "dist_stream", "elevation", "night_light", "popn_density", "slope")


forest_only_matrix <- xgb.DMatrix(
  data = as.matrix(forest_only[, predictor_vars]),  
  label = forest_only$deforested
)


xgb_predict <- predict(xgb_model, forest_only_matrix)

final_df <- data.frame(
  x = forest_only$x,
  y = forest_only$y,
  rf_predict = rf_predict[ , '1'],
  xgb_predict  = xgb_predict
)



write.csv(
  final_df,
  "D:/Drive/Deforestation Modeling/Working Documents/Prediction CSV/prediction_high_mountains_physiography.csv",
  row.names = F
)

rm(list = ls())
gc()






#-----------------------------------------------------------------------------=
# HIGH HIMALAYA REGION ---------------------------------------------------------------
#-----------------------------------------------------------------------------=


## 1. LOADING NECESSARY PACKAGES ------------------------------------------------

library(tidyverse)
library(randomForest)
library(xgboost)


## 2. MODELS AND DATASETS -------------------------------------------------------

high_himalaya_models <- readRDS(
  "D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_high_himalaya.rds"
)



rf_model <- high_himalaya_models$rf


high_himalaya <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_high_himalaya.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_high_himalaya_PAs.csv"
  )
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



nrow(
  forest_only %>% 
    filter(!complete.cases(.))
)

## 3. PREDICTION ---------------------------------------------------------------

rf_predict <- predict(rf_model, forest_only, type = "prob")



final_df <- data.frame(
  x = forest_only$x,
  y = forest_only$y,
  rf_predict = rf_predict[ , '1']
)


write.csv(
  final_df,
  "D:/Drive/Deforestation Modeling/Working Documents/Prediction CSV/prediction_high_himalaya_physiography.csv",
  row.names = F
)

rm(list = ls())
gc()
