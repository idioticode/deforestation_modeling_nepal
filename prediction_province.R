# Risk Prediction in Provinces -------------------------------------------------=



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

terai <- read.csv(
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Provincial/province_data_terai.csv"
) %>% 
  mutate(
    popn_density = replace_na(popn_density, 0),
    night_light = replace_na(popn_density, 0)
  )


forest_only <- terai %>% 
  filter(
    complete.cases(.)
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
  province = forest_only$PROVINCE,
  rf_predict = rf_predict[ , '1'],
  xgb_predict  = xgb_predict
)


write.csv(
  final_df,
  "D:/Drive/Deforestation Modeling/Working Documents/Prediction CSV/prediction_terai_province.csv",
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

siwalik <- read.csv(
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Provincial/province_data_siwalik.csv"
) %>% 
  mutate(
    popn_density = replace_na(popn_density, 0),
    night_light = replace_na(popn_density, 0)
  )


forest_only <- siwalik %>% 
  filter(
    complete.cases(PROVINCE)
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
  province = forest_only$PROVINCE,
  rf_predict = rf_predict[ , '1'],
  xgb_predict  = xgb_predict
)

write.csv(
  final_df,
  "D:/Drive/Deforestation Modeling/Working Documents/Prediction CSV/prediction_siwalik_province.csv",
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

middle_mountains <- read.csv(
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Provincial/province_data_middle_mountains.csv"
) %>% 
  mutate(
    popn_density = replace_na(popn_density, 0),
    night_light = replace_na(popn_density, 0)
  )


forest_only <- middle_mountains %>% 
  filter(
    complete.cases(PROVINCE)
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
  province = forest_only$PROVINCE,
  rf_predict = rf_predict[ , '1'],
  xgb_predict  = xgb_predict
)


write.csv(
  final_df,
  "D:/Drive/Deforestation Modeling/Working Documents/Prediction CSV/prediction_middle_mountains_province.csv",
  row.names = F
)

rm(list = ls())
gc()


#-----------------------------------------------------------------------------=
# HIGH MOUNTAINS REGION --------------------------------------------------------
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

high_mountains <- read.csv(
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Provincial/province_data_high_mountains.csv"
) %>% 
  mutate(
    popn_density = replace_na(popn_density, 0),
    night_light = replace_na(popn_density, 0)
  )


forest_only <- high_mountains %>% 
  filter(
    complete.cases(PROVINCE)
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
  province = forest_only$PROVINCE,
  rf_predict = rf_predict[ , '1'],
  xgb_predict  = xgb_predict
)


write.csv(
  final_df,
  "D:/Drive/Deforestation Modeling/Working Documents/Prediction CSV/prediction_high_mountains_province.csv",
  row.names = F
)

rm(list = ls())
gc()


#-----------------------------------------------------------------------------=
# HIGH HIMALAYA REGION --------------------------------------------------------
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


high_himalaya <- read.csv(
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/Provincial/province_data_high_himalaya.csv"
) %>% 
  mutate(
    popn_density = replace_na(popn_density, 0),
    night_light = replace_na(popn_density, 0)
  )


forest_only <- high_himalaya %>% 
  filter(
    complete.cases(PROVINCE)
  )


## 3. PREDICTION ---------------------------------------------------------------

rf_predict <- predict(rf_model, forest_only, type = "prob")



final_df <- data.frame(
  x = forest_only$x,
  y = forest_only$y,
  province = forest_only$PROVINCE,
  rf_predict = rf_predict[ , '1']
)


write.csv(
  final_df,
  "D:/Drive/Deforestation Modeling/Working Documents/Prediction CSV/prediction_high_himalaya_province.csv",
  row.names = F
)

rm(list = ls())
gc()
