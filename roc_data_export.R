# DEVELOPING ROC ------------------------------------------------------------=

# -----------------------------------------------------------------------------=
# FUNCTIONS -----------------------------------------------------------------------
# -----------------------------------------------------------------------------=

## EXPORTING ROC -----------------------------------------------------------------------
roc_curve <- function(result_data, step, original_column, predicted_probability) {
  # Compute Sensitivity and Specificity for different cutoff values to generate ROC Curve.
  #
  # Parameters:
  # result_data (data.frame): A dataframe containing 'Original Presence' (actual values) and 'Predicted Probability'.
  # step (numeric): The step size for cutoff values (default is 0.05).
  #
  # Returns:
  # data.frame: A dataframe with Cutoff, Sensitivity, and Specificity values.
  
  cutoffs <- seq(0, 1, by = step)  # Generate cutoffs from 0 to 1 with step
  roc_data <- data.frame(Cutoff = numeric(), Sensitivity = numeric(), Specificity = numeric())  # Dataframe to store results
  
  y_true <- result_data[[original_column]]
  y_prob <- result_data[[predicted_probability]]
  
  for (cutoff in cutoffs) {
    y_pred <- as.integer(y_prob >= cutoff)  # Convert probability to binary class based on cutoff
    
    # Calculate TP, FN, FP, TN
    TP <- sum((y_true == 1) & (y_pred == 1))
    FN <- sum((y_true == 1) & (y_pred == 0))
    FP <- sum((y_true == 0) & (y_pred == 1))
    TN <- sum((y_true == 0) & (y_pred == 0))
    
    # Compute Sensitivity (Recall) and Specificity
    sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), 0)
    specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), 0)
    
    # Store results
    roc_data <- rbind(roc_data, data.frame(Cutoff = cutoff, Sensitivity = sensitivity, Specificity = specificity))
  }
  
  # Add True Positive Rate and False Positive Rate columns
  roc_data$True_Positive_Rate <- roc_data$Sensitivity
  roc_data$False_Positive_Rate <- 1 - roc_data$Specificity
  
  return(roc_data)
}


## TEST-TRAIN SPLIT -----------------------------------------------------------
train_test_split <- function(n_data, original_data, test_percentage, dependent_variable, forest_column) {
  
  # Ensure that the dependent variable is a column in the data
  dep_col <- dependent_variable
  
  # Split the data into two subsets based on the dependent variable
  loss_presence <- original_data[original_data[[dep_col]] == 1, ]
  loss_absence <- original_data[original_data[[forest_column]] == 1 & original_data[[dep_col]] == 0, ]
  
  # Check if there are enough samples for both classes
  if (n_data > nrow(loss_presence) | n_data > nrow(loss_absence)) {
    stop("Requested sample size exceeds available data for one of the classes.")
  }
  
  # Sample the specified number of rows from each class
  loss_presence <- loss_presence[sample(nrow(loss_presence), n_data), ]
  loss_absence <- loss_absence[sample(nrow(loss_absence), n_data), ]
  
  # Split into training (1 - test_percentage%) and testing (test_percentage%)
  split_ratio <- 1 - test_percentage / 100
  
  # Create training and testing datasets for both presence and absence of loss
  train_presence <- loss_presence[sample(nrow(loss_presence), size = floor(split_ratio * n_data)), ]
  test_presence <- loss_presence[!rownames(loss_presence) %in% rownames(train_presence), ]
  
  train_absence <- loss_absence[sample(nrow(loss_absence), size = floor(split_ratio * n_data)), ]
  test_absence <- loss_absence[!rownames(loss_absence) %in% rownames(train_absence), ]
  
  # Combine training and testing datasets
  training_data <- rbind(train_presence, train_absence)
  testing_data <- rbind(test_presence, test_absence)
  
  return(list(training_data = training_data, testing_data = testing_data))
}


# -----------------------------------------------------------------------------=
# PACKAGES ---------------------------------------------------------------------
# -----------------------------------------------------------------------------=
library(tidyverse)
library(randomForest)
library(xgboost)




# -----------------------------------------------------------------------------=
# 1. TERAI -----------------------------------------------------------------------
# -----------------------------------------------------------------------------=

## Dataset ==========================================

# Models 

terai_models <- readRDS(
  "D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_terai.rds"
)


rf_model <- terai_models$rf
xgb_model <- terai_models$xgb


# data 
terai <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_terai.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_terai_PAs.csv"
  )
)



modeling_data <- terai %>% 
  dplyr::select(
    -ndvi_2000, -ndvi_2024, -region
  ) %>% 
  filter(
    complete.cases(.)
  ) %>% 
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
  mutate(
    deforested = case_when(
      lc_deforested == 1 | forest_loss == 1 ~ 1, 
      T ~ 0
    )
  ) %>% 
  mutate(
    deforested = as.factor(deforested)
  )


## Test Data --------------------------------------------------------

set.seed(23)

# Assuming `modeling_data` is your data 
test_train_list <- train_test_split(
  n_data =nrow(modeling_data[modeling_data$deforested == 1, ]), 
  original_data = modeling_data,
  test_percentage = 30,
  dependent_variable = 'deforested',
  forest_column = "lc_forest_2022"
)


testing_data <- test_train_list$testing_data%>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope", "deforested"   
  ) 


## Prediction -----------------------------------------------------------------

rf_predict <- predict(rf_model, testing_data, type = "prob")


predictor_vars <- c("aspect", "dist_cropland", "dist_road", "dist_settlement", 
                    "dist_stream", "elevation", "night_light", "popn_density", "slope")


forest_only_matrix <- xgb.DMatrix(
  data = as.matrix(testing_data[, predictor_vars]),  
  label = testing_data$deforested
)


xgb_predict <- predict(xgb_model, forest_only_matrix)



final_df <- data.frame(
  deforested = testing_data$deforested,
  rf_predict = rf_predict[ , '1'],
  xgb_predict  = xgb_predict
)



## ROC-Dataset -------------------------------------------------------

## Randomforest. 

roc_rf <- roc_curve(
  result_data = final_df, 
  step = 0.05, 
  original_column = "deforested", 
  predicted_probability = "rf_predict"
)

roc_rf <- roc_rf |>
  dplyr::mutate(
    model = "rf",
    region = "terai"
  )

roc_xgb <- roc_curve(
  result_data = final_df, 
  step = 0.05, 
  original_column = "deforested", 
  predicted_probability = "xgb_predict"
)


roc_xgb <- roc_rf |>
  dplyr::mutate(
    model = "xgb",
    region = 'terai'
  )


write.csv(
  rbind(
    roc_rf,
    roc_xgb
  ),
    "D:/Drive/Deforestation Modeling/Working Documents/ROC Data/roc_data_terai.csv",
  row.names = F
)

rm(list = ls())
gc()



# -----------------------------------------------------------------------------=
# 2. SIWALIK -----------------------------------------------------------------------
# -----------------------------------------------------------------------------=

## Dataset ==========================================

# Models 

siwalik_models <- readRDS(
  "D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_siwalik.rds"
)


rf_model <- siwalik_models$rf
xgb_model <- siwalik_models$xgb


# data 
siwalik <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_siwalik.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_siwalik_PAs.csv"
  )
)



modeling_data <- siwalik %>% 
  dplyr::select(
    -ndvi_2000, -ndvi_2024, -region
  ) %>% 
  filter(
    complete.cases(.)
  ) %>% 
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
  mutate(
    deforested = case_when(
      lc_deforested == 1 | forest_loss == 1 ~ 1, 
      T ~ 0
    )
  ) %>% 
  mutate(
    deforested = as.factor(deforested)
  )


## Test Data --------------------------------------------------------

set.seed(23)

# Assuming `modeling_data` is your data 
test_train_list <- train_test_split(
  n_data =nrow(modeling_data[modeling_data$deforested == 1, ]), 
  original_data = modeling_data,
  test_percentage = 30,
  dependent_variable = 'deforested',
  forest_column = "lc_forest_2022"
)


testing_data <- test_train_list$testing_data%>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope", "deforested"   
  ) 


## Prediction -----------------------------------------------------------------

rf_predict <- predict(rf_model, testing_data, type = "prob")


predictor_vars <- c("aspect", "dist_cropland", "dist_road", "dist_settlement", 
                    "dist_stream", "elevation", "night_light", "popn_density", "slope")


forest_only_matrix <- xgb.DMatrix(
  data = as.matrix(testing_data[, predictor_vars]),  
  label = testing_data$deforested
)


xgb_predict <- predict(xgb_model, forest_only_matrix)



final_df <- data.frame(
  deforested = testing_data$deforested,
  rf_predict = rf_predict[ , '1'],
  xgb_predict  = xgb_predict
)



## ROC-Dataset -------------------------------------------------------

## Randomforest. 

roc_rf <- roc_curve(
  result_data = final_df, 
  step = 0.05, 
  original_column = "deforested", 
  predicted_probability = "rf_predict"
)

roc_rf <- roc_rf |>
  dplyr::mutate(
    model = "rf",
    region = "siwalik"
  )

roc_xgb <- roc_curve(
  result_data = final_df, 
  step = 0.05, 
  original_column = "deforested", 
  predicted_probability = "xgb_predict"
)


roc_xgb <- roc_rf |>
  dplyr::mutate(
    model = "xgb",
    region = 'siwalik'
  )


write.csv(
  rbind(
    roc_rf,
    roc_xgb
  ),
  "D:/Drive/Deforestation Modeling/Working Documents/ROC Data/roc_data_siwalik.csv",
  row.names = F
)

rm(list = ls())
gc()


# -----------------------------------------------------------------------------=
# 3. MIDDLE MOUNTAINS ----------------------------------------------------------
# -----------------------------------------------------------------------------=

## Dataset ==========================================

# Models 

middle_mountains_models <- readRDS(
  "D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_middle_mountains.rds"
)


rf_model <- middle_mountains_models$rf
xgb_model <- middle_mountains_models$xgb


# data 
middle_mountains <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_middle_mountains.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_middle_mountains_PAs.csv"
  )
)



modeling_data <- middle_mountains %>% 
  dplyr::select(
    -ndvi_2000, -ndvi_2024, -region
  ) %>% 
  filter(
    complete.cases(.)
  ) %>% 
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
  mutate(
    deforested = case_when(
      lc_deforested == 1 | forest_loss == 1 ~ 1, 
      T ~ 0
    )
  ) %>% 
  mutate(
    deforested = as.factor(deforested)
  )


## Test Data --------------------------------------------------------

set.seed(23)

# Assuming `modeling_data` is your data 
test_train_list <- train_test_split(
  n_data =nrow(modeling_data[modeling_data$deforested == 1, ]), 
  original_data = modeling_data,
  test_percentage = 30,
  dependent_variable = 'deforested',
  forest_column = "lc_forest_2022"
)


testing_data <- test_train_list$testing_data%>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope", "deforested"   
  ) 


## Prediction -----------------------------------------------------------------

rf_predict <- predict(rf_model, testing_data, type = "prob")


predictor_vars <- c("aspect", "dist_cropland", "dist_road", "dist_settlement", 
                    "dist_stream", "elevation", "night_light", "popn_density", "slope")


forest_only_matrix <- xgb.DMatrix(
  data = as.matrix(testing_data[, predictor_vars]),  
  label = testing_data$deforested
)


xgb_predict <- predict(xgb_model, forest_only_matrix)



final_df <- data.frame(
  deforested = testing_data$deforested,
  rf_predict = rf_predict[ , '1'],
  xgb_predict  = xgb_predict
)



## ROC-Dataset -------------------------------------------------------

## Randomforest. 

roc_rf <- roc_curve(
  result_data = final_df, 
  step = 0.05, 
  original_column = "deforested", 
  predicted_probability = "rf_predict"
)

roc_rf <- roc_rf |>
  dplyr::mutate(
    model = "rf",
    region = "middle_mountains"
  )

## Xboost Gradient 
roc_xgb <- roc_curve(
  result_data = final_df, 
  step = 0.05, 
  original_column = "deforested", 
  predicted_probability = "xgb_predict"
)


roc_xgb <- roc_rf |>
  dplyr::mutate(
    model = "xgb",
    region = 'middle_mountains'
  )


write.csv(
  rbind(
    roc_rf,
    roc_xgb
  ),
  "D:/Drive/Deforestation Modeling/Working Documents/ROC Data/roc_data_middle_mountains.csv",
  row.names = F
)

rm(list = ls())
gc()



# -----------------------------------------------------------------------------=
# 4. HIGH MOUNTAINS ------------------------------------------------------------
# -----------------------------------------------------------------------------=

## Dataset ==========================================

# Models 

high_mountains_models <- readRDS(
  "D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_high_mountains.rds"
)


rf_model <- high_mountains_models$rf
xgb_model <- high_mountains_models$xgb


# data 
high_mountains <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_high_mountains.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_high_mountains_PAs.csv"
  )
)



modeling_data <- high_mountains %>% 
  dplyr::select(
    -ndvi_2000, -ndvi_2024, -region
  ) %>% 
  filter(
    complete.cases(.)
  ) %>% 
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
  mutate(
    deforested = case_when(
      lc_deforested == 1 | forest_loss == 1 ~ 1, 
      T ~ 0
    )
  ) %>% 
  mutate(
    deforested = as.factor(deforested)
  )


## Test Data --------------------------------------------------------

set.seed(23)

# Assuming `modeling_data` is your data 
test_train_list <- train_test_split(
  n_data = 400000, 
  original_data = modeling_data,
  test_percentage = 30,
  dependent_variable = 'deforested',
  forest_column = "lc_forest_2022"
)


testing_data <- test_train_list$testing_data%>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope", "deforested"   
  ) 


## Prediction -----------------------------------------------------------------

rf_predict <- predict(rf_model, testing_data, type = "prob")


predictor_vars <- c("aspect", "dist_cropland", "dist_road", "dist_settlement", 
                    "dist_stream", "elevation", "night_light", "popn_density", "slope")


forest_only_matrix <- xgb.DMatrix(
  data = as.matrix(testing_data[, predictor_vars]),  
  label = testing_data$deforested
)


xgb_predict <- predict(xgb_model, forest_only_matrix)



final_df <- data.frame(
  deforested = testing_data$deforested,
  rf_predict = rf_predict[ , '1'],
  xgb_predict  = xgb_predict
)



## ROC-Dataset -------------------------------------------------------

## Randomforest. 

roc_rf <- roc_curve(
  result_data = final_df, 
  step = 0.05, 
  original_column = "deforested", 
  predicted_probability = "rf_predict"
)

roc_rf <- roc_rf |>
  dplyr::mutate(
    model = "rf",
    region = "high_mountains"
  )

## Xboost Gradient 
roc_xgb <- roc_curve(
  result_data = final_df, 
  step = 0.05, 
  original_column = "deforested", 
  predicted_probability = "xgb_predict"
)


roc_xgb <- roc_rf |>
  dplyr::mutate(
    model = "xgb",
    region = 'high_mountains'
  )


write.csv(
  rbind(
    roc_rf,
    roc_xgb
  ),
  "D:/Drive/Deforestation Modeling/Working Documents/ROC Data/roc_data_high_mountains.csv",
  row.names = F
)

rm(list = ls())
gc()


# -----------------------------------------------------------------------------=
# 4. HIGH MOUNTAINS ------------------------------------------------------------
# -----------------------------------------------------------------------------=

## Dataset ==========================================
library(randomForest)
# Models 

high_himalaya_models <- readRDS(
  "D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_high_himalaya.rds"
)


rf_model <- high_himalaya_models$rf


# data 
high_himalaya <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_high_himalaya.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_high_himalaya_PAs.csv"
  )
)



modeling_data <- high_himalaya %>% 
  dplyr::select(
    -ndvi_2000, -ndvi_2024, -region
  ) %>% 
  filter(
    complete.cases(.)
  ) %>% 
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
  mutate(
    deforested = case_when(
      lc_deforested == 1 | forest_loss == 1 ~ 1, 
      T ~ 0
    )
  ) %>% 
  mutate(
    deforested = as.factor(deforested)
  )


## Test Data --------------------------------------------------------

set.seed(23)

# Assuming `modeling_data` is your data 
test_train_list <- train_test_split(
  n_data = nrow(modeling_data[modeling_data$deforested==1 , ]), 
  original_data = modeling_data,
  test_percentage = 30,
  dependent_variable = 'deforested',
  forest_column = "lc_forest_2022"
)


testing_data <- test_train_list$testing_data%>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope", "deforested"   
  ) 


## Prediction -----------------------------------------------------------------

rf_predict <- predict(rf_model, testing_data, type = "prob")



final_df <- data.frame(
  deforested = testing_data$deforested,
  rf_predict = rf_predict[ , '1']
)



## ROC-Dataset -------------------------------------------------------

## Randomforest. 

roc_rf <- roc_curve(
  result_data = final_df, 
  step = 0.05, 
  original_column = "deforested", 
  predicted_probability = "rf_predict"
)

roc_rf <- roc_rf |>
  dplyr::mutate(
    model = "rf",
    region = "high_himalaya"
  )

write.csv(
  roc_rf,
  "D:/Drive/Deforestation Modeling/Working Documents/ROC Data/roc_data_high_himalaya.csv",
  row.names = F
)

rm(list = ls())
gc()




