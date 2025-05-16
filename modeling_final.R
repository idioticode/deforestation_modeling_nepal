#------------------------------------------------------------------------------=
# MODEING FINAL ---------------------------------------------------------------=
#------------------------------------------------------------------------------=

#------------------------------------------------------------------------------=
# PACKAGES ------------------------------------------------------------------
#------------------------------------------------------------------------------=
library(tidyverse)
library(pROC)



#------------------------------------------------------------------------------=
# FUNCTIONS -----------------------------------------------------------------
#------------------------------------------------------------------------------=


## Multicollinearity assessment -----

library(dplyr) 
library(car) 

multicollinearity_function <- function(data, th) {
  
  vif_function <- function(data) {
    vif_values <- data.frame(Variable = character(), VIF = numeric())
    for (var in names(data)) {
      formula <- as.formula(paste(var, "~ ."))
      model <- lm(formula, data = data)
      r_squared <- summary(model)$r.squared
      vif_value <- 1 / (1 - r_squared)
      vif_values <- rbind(vif_values, data.frame(Variable = var, VIF = vif_value))
    }
    return(vif_values)
  }
  
  repeat {
    correlation_matrix <- cor(data)
    correlation_df <- as.data.frame(as.table(correlation_matrix))
    colnames(correlation_df) <- c("variable1", "variable2", "correlation")
    
    correlation_df <- correlation_df %>%
      filter(variable1 != variable2) %>%
      mutate(abs_correlation = abs(correlation)) %>%
      arrange(desc(abs_correlation))
    
    high_corr <- correlation_df %>% filter(abs_correlation > th)
    
    if (nrow(high_corr) == 0) {
      break
    }
    
    pair <- high_corr[1, c("variable1", "variable2")]
    
    vif_values <- vif_function(data)
    vif_pair <- vif_values %>% filter(Variable %in% c(pair$variable1, pair$variable2))
    
    variable_to_remove <- vif_pair$Variable[which.max(vif_pair$VIF)]
    
    data <- data %>% select(-all_of(variable_to_remove))
    
    var_names <- names(data)
  }
  
  final_vif_values <- vif_function(data)
  
  return(final_vif_values)
}


## Test-train split ------------------------------------------------------------

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


## Metrices Calculation --------------------------------------------------------

calculate_metrics <- function(data, actual_col, predicted_col, predicted_probs) {
  
  # Confusion Matrix using table function
  conf_matrix <- table(Predicted = data[[predicted_col]], Actual = data[[actual_col]])
  print("Confusion Matrix:")
  print(conf_matrix)
  
  # Extract TP, TN, FP, FN
  TP <- conf_matrix[2, 2]  # True Positives
  TN <- conf_matrix[1, 1]  # True Negatives
  FP <- conf_matrix[2, 1]  # False Positives
  FN <- conf_matrix[1, 2]  # False Negatives
  
  # Calculate the overall accuracy
  overall_accuracy <- (TP + TN) / (TP + TN + FP + FN) * 100
  
  # Calculate Sensitivity (Recall) and Specificity
  sensitivity <- TP / (TP + FN)  # True Positive Rate (Recall)
  specificity <- TN / (TN + FP)  # True Negative Rate
  
  # Calculate TSS (True Skill Statistic)
  TSS <- sensitivity + specificity - 1
  
  # Calculate Precision and F1 Score
  precision <- TP / (TP + FP)
  f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)
  
  # Calculate Kappa Coefficient
  total <- TP + TN + FP + FN
  p_o <- (TP + TN) / total  # Observed agreement
  p_e <- (as.numeric((TP + FP)) * as.numeric((TP + FN)) + as.numeric((FN + TN)) * as.numeric((FP + TN))) / (total^2)
  kappa_value <- (p_o - p_e) / (1 - p_e)
  
  # Load the necessary library for AUC calculation
  library(pROC)
  
  # Calculate AUC using predicted probabilities
  auc_value <- roc(as.numeric(data[[actual_col]]), predicted_probs)$auc
  
  # Return all metrics as a data frame
  metrics <- data.frame(
    Overall_accuracy = overall_accuracy,
    AUC = auc_value,
    TSS = TSS,
    F1_Score = f1_score,
    Kappa = kappa_value,
    Recall = sensitivity,
    Specificity = specificity
  )
  
  print(metrics)
  return(metrics)
}

# -----------------------------------------------------------------------------=
# 1. TERAI ---------------------------------------------------------------------
# -----------------------------------------------------------------------------=

## 1.1. DATASET ---------------------------------------------------------------
terai <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_terai.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_terai_PAs.csv"
  )
)

names(terai)

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


## 1.2. MULTICOLLINEARITY ASSESSMENT -------------------------------------------
names(modeling_data)

vif_values <- multicollinearity_function(
  data = modeling_data[
    modeling_data$deforested ==1 ,
    c(
      "aspect", "dist_cropland", "dist_road", "dist_settlement", "dist_stream", 
      "elevation", "night_light", "popn_density", "slope"
    )
  ],
  
  th = 0.7
)

write.csv(
  vif_values,
  "D:/Drive/Deforestation Modeling/Working Documents/VIF/vif_values_terai.csv",
  row.names = F
)

## 1.3. TEST-TRAIN SPLIT --------------------------------------------------------

nrow(modeling_data[modeling_data$deforested == 1, ])

set.seed(23)

# Assuming `modeling_data` is your data 
test_train_list <- train_test_split(
  n_data =nrow(modeling_data[modeling_data$deforested == 1, ]), 
  original_data = modeling_data,
  test_percentage = 30,
  dependent_variable = 'deforested',
  forest_column = "lc_forest_2022"
)


# Access the training and testing datasets
training_data <- test_train_list$training_data %>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope", "deforested"   
  )

testing_data <- test_train_list$testing_data%>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope", "deforested"   
  ) 

## 1.4. Modeling --------------------------------------
### RF Model ------------------------------------------
library(randomForest)

best_model_rf <- randomForest(
  deforested ~ ., 
  data = training_data, 
  ntree = 700, 
  mtry = 3, 
  nodesize = 1,
  importance = T
)


# Predict on testing data
test_prob <- predict(best_model_rf, testing_data, type = "prob")


metric_data_rf <- data.frame(
  original = testing_data$deforested,
  predicted = ifelse(test_prob[ , '1'] > 0.5, 1, 0)
)

#importance(rf_model)
metric_final_rf <- calculate_metrics(
  data = metric_data_rf, 
  actual_col = "original", 
  predicted_col = "predicted", 
  predicted_probs = test_prob[ , '1']
)

metric_final_rf$model <- 'rf'

gc()

### XGBoost  ---------------------------------------------------------------
library(xgboost)

# Access the training and testing datasets
xgb_training <- training_data |>
  dplyr::mutate(
    deforested = as.numeric(as.character(deforested))
  )


xgb_testing <- testing_data |>
  dplyr::mutate(
    deforested = as.numeric(as.character(deforested))
  )


# Prepare data for XGBoost
train_matrix <- xgb.DMatrix(
  data = as.matrix(xgb_training[, -which(names(xgb_training) == "deforested")]),  # Predictors
  label = xgb_training$deforested  # Response variable
)

test_matrix <- xgb.DMatrix(
  data = as.matrix(xgb_testing[, -which(names(xgb_testing) == "deforested")]),  # Predictors
  label = xgb_testing$deforested  # Response variable
)


best_model_xgb <- xgboost(
  data = train_matrix,
  nrounds = 900,
  max_depth = 13,
  eta = 0.3,
  objective = "binary:logistic",
  verbose = 0
)



# Predict on testing data
xgb_predict <- predict(best_model_xgb, test_matrix)  # Probabilities for class 1

# Create metric data
xgb_metric <- data.frame(
  original = xgb_testing$deforested,
  predicted = ifelse(xgb_predict > 0.5, 1, 0)  # Convert probabilities to binary predictions
)

# Calculate evaluation metrics
metric_final_xgb <- calculate_metrics(
  data = xgb_metric, 
  actual_col = "original", 
  predicted_col = "predicted",
  predicted_probs = xgb_predict
)

metric_final_xgb$model <- 'xgb'

## 1.5. Exporting Data -----------------------------------------------------

# exporting the models as RDS file. 
saveRDS(
  list(
    rf = best_model_rf,
    xgb = best_model_xgb
  ),
  
  file = "D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_terai.rds"
)


# exporting metrices
write.csv(
  rbind(metric_final_rf, metric_final_xgb),
  "D:/Drive/Deforestation Modeling/Working Documents/Metrices/metrices_terai.csv",
  row.names = F
)


# Exporting Variable Importance. 

# Convert Random Forest importance to dataframe
rf_importance <- importance(best_model_rf) |> as.data.frame()
rf_importance$Feature <- rownames(rf_importance)  # Add feature column

# Convert XGBoost importance to dataframe
xgb_importance <- xgb.importance(feature_names = colnames(train_matrix), model = best_model_xgb)

# Merge the two dataframes on Feature column
combined_importance <- merge(rf_importance, xgb_importance, by = "Feature", all = TRUE)
combined_importance


write.csv(
  merge(rf_importance, xgb_importance, by = "Feature", all = TRUE),
  "D:/Drive/Deforestation Modeling/Working Documents/Variable Importance/variable_importance_terai.csv",
  row.names = F
)

rm(list = ls())
gc()

# -----------------------------------------------------------------------------=
# 2. SIWALIK ---------------------------------------------------------------------
# -----------------------------------------------------------------------------=

## 2.1. DATASET ---------------------------------------------------------------
siwalik <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_siwalik.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_siwalik_PAs.csv"
  )
)

names(siwalik)

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


## 2.2. MULTICOLLINEARITY ASSESSMENT -------------------------------------------
names(modeling_data)

vif_values <- multicollinearity_function(
  data = modeling_data[
    modeling_data$deforested ==1 ,
    c(
      "aspect", "dist_cropland", "dist_road", "dist_settlement", "dist_stream", 
      "elevation", "night_light", "popn_density", "slope"
    )
  ],
  
  th = 0.7
)

write.csv(
  vif_values,
  "D:/Drive/Deforestation Modeling/Working Documents/VIF/vif_values_siwalik.csv",
  row.names = F
)

## 2.3. TEST-TRAIN SPLIT --------------------------------------------------------

nrow(modeling_data[modeling_data$deforested == 1, ])

set.seed(23)

# Assuming `modeling_data` is your data 
test_train_list <- train_test_split(
  n_data =nrow(modeling_data[modeling_data$deforested == 1, ]), 
  original_data = modeling_data,
  test_percentage = 30,
  dependent_variable = 'deforested',
  forest_column = "lc_forest_2022"
)


# Access the training and testing datasets
training_data <- test_train_list$training_data %>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope", "deforested"   
  )

testing_data <- test_train_list$testing_data%>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope", "deforested"   
  ) 

  ## 2.4. Modeling --------------------------------------
### RF Model ------------------------------------------
library(randomForest)

best_model_rf <- randomForest(
  deforested ~ ., 
  data = training_data, 
  ntree = 200, 
  mtry = 3, 
  nodesize = 1,
  importance = T
)


# Predict on testing data
test_prob <- predict(best_model_rf, testing_data, type = "prob")


metric_data_rf <- data.frame(
  original = testing_data$deforested,
  predicted = ifelse(test_prob[ , '1'] > 0.5, 1, 0)
)

#importance(rf_model)
metric_final_rf <- calculate_metrics(
  data = metric_data_rf, 
  actual_col = "original", 
  predicted_col = "predicted", 
  predicted_probs = test_prob[ , '1']
)

metric_final_rf$model <- 'rf'

gc()

### XGBoost  ---------------------------------------------------------------
library(xgboost)

# Access the training and testing datasets
xgb_training <- training_data |>
  dplyr::mutate(
    deforested = as.numeric(as.character(deforested))
  )


xgb_testing <- testing_data |>
  dplyr::mutate(
    deforested = as.numeric(as.character(deforested))
  )


# Prepare data for XGBoost
train_matrix <- xgb.DMatrix(
  data = as.matrix(xgb_training[, -which(names(xgb_training) == "deforested")]),  # Predictors
  label = xgb_training$deforested  # Response variable
)

test_matrix <- xgb.DMatrix(
  data = as.matrix(xgb_testing[, -which(names(xgb_testing) == "deforested")]),  # Predictors
  label = xgb_testing$deforested  # Response variable
)


best_model_xgb <- xgboost(
  data = train_matrix,
  nrounds = 900,
  max_depth = 13,
  eta = 0.3,
  objective = "binary:logistic",
  verbose = 0
)



# Predict on testing data
xgb_predict <- predict(best_model_xgb, test_matrix)  # Probabilities for class 1

# Create metric data
xgb_metric <- data.frame(
  original = xgb_testing$deforested,
  predicted = ifelse(xgb_predict > 0.5, 1, 0)  # Convert probabilities to binary predictions
)

# Calculate evaluation metrics
metric_final_xgb <- calculate_metrics(
  data = xgb_metric, 
  actual_col = "original", 
  predicted_col = "predicted",
  predicted_probs = xgb_predict
)

metric_final_xgb$model <- 'xgb'

## 2.5. Exporting Data -----------------------------------------------------

# exporting the models as RDS file. 
saveRDS(
  list(
    rf = best_model_rf,
    xgb = best_model_xgb
  ),
  
  file = "D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_siwalik.rds"
)


# exporting metrices
write.csv(
  rbind(metric_final_rf, metric_final_xgb),
  "D:/Drive/Deforestation Modeling/Working Documents/Metrices/metrices_siwalik.csv",
  row.names = F
)


# Exporting Variable Importance. 

# Convert Random Forest importance to dataframe
rf_importance <- importance(best_model_rf) |> as.data.frame()
rf_importance$Feature <- rownames(rf_importance)  # Add feature column

# Convert XGBoost importance to dataframe
xgb_importance <- xgb.importance(feature_names = colnames(train_matrix), model = best_model_xgb)

# Merge the two dataframes on Feature column
combined_importance <- merge(rf_importance, xgb_importance, by = "Feature", all = TRUE)
combined_importance


write.csv(
  merge(rf_importance, xgb_importance, by = "Feature", all = TRUE),
  "D:/Drive/Deforestation Modeling/Working Documents/Variable Importance/variable_importance_siwalik.csv",
  row.names = F
)

rm(list = ls())
gc()

# -----------------------------------------------------------------------------=
# 3. MIDDLE MOUNTAINS ----------------------------------------------------------
# -----------------------------------------------------------------------------=

## 3.1. DATASET ----------------------------------------------------------------

middle_mountains <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_middle_mountains.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_middle_mountains_PAs.csv"
  )
)

names(middle_mountains)

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


## 3.2. MULTICOLLINEARITY ASSESSMENT -------------------------------------------
names(modeling_data)

vif_values <- multicollinearity_function(
  data = modeling_data[
    modeling_data$deforested ==1 ,
    c(
      "aspect", "dist_cropland", "dist_road", "dist_settlement", "dist_stream", 
      "elevation", "night_light", "popn_density", "slope"
    )
  ],
  
  th = 0.7
)

write.csv(
  vif_values,
  "D:/Drive/Deforestation Modeling/Working Documents/VIF/vif_values_middle_mountains.csv",
  row.names = F
)

## 3.3. TEST-TRAIN SPLIT --------------------------------------------------------

nrow(modeling_data[modeling_data$deforested == 1, ])


set.seed(23)

# Assuming `modeling_data` is your data 
test_train_list <- train_test_split(
  n_data =nrow(modeling_data[modeling_data$deforested == 1, ]), 
  original_data = modeling_data,
  test_percentage = 30,
  dependent_variable = 'deforested',
  forest_column = "lc_forest_2022"
)


# Access the training and testing datasets
training_data <- test_train_list$training_data %>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope", "deforested"   
  )

testing_data <- test_train_list$testing_data%>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope", "deforested"   
  ) 

## 3.4. Modeling --------------------------------------
### RF Model ------------------------------------------
library(randomForest)

gc()

best_model_rf <- randomForest(
  deforested ~ ., 
  data = training_data, 
  ntree = 200, 
  mtry = 3, 
  nodesize = 1,
  importance = T
)


# Predict on testing data
test_prob <- predict(best_model_rf, testing_data, type = "prob")


metric_data_rf <- data.frame(
  original = testing_data$deforested,
  predicted = ifelse(test_prob[ , '1'] > 0.5, 1, 0)
)

#importance(rf_model)
metric_final_rf <- calculate_metrics(
  data = metric_data_rf, 
  actual_col = "original", 
  predicted_col = "predicted", 
  predicted_probs = test_prob[ , '1']
)

metric_final_rf$model <- 'rf'

gc()

### XGBoost  ---------------------------------------------------------------
library(xgboost)

# Access the training and testing datasets
xgb_training <- training_data |>
  dplyr::mutate(
    deforested = as.numeric(as.character(deforested))
  )


xgb_testing <- testing_data |>
  dplyr::mutate(
    deforested = as.numeric(as.character(deforested))
  )


# Prepare data for XGBoost
train_matrix <- xgb.DMatrix(
  data = as.matrix(xgb_training[, -which(names(xgb_training) == "deforested")]),  # Predictors
  label = xgb_training$deforested  # Response variable
)

test_matrix <- xgb.DMatrix(
  data = as.matrix(xgb_testing[, -which(names(xgb_testing) == "deforested")]),  # Predictors
  label = xgb_testing$deforested  # Response variable
)


best_model_xgb <- xgboost(
  data = train_matrix,
  nrounds = 900,
  max_depth = 15,
  eta = 0.4,
  objective = "binary:logistic",
  verbose = 0
)



# Predict on testing data
xgb_predict <- predict(best_model_xgb, test_matrix)  # Probabilities for class 1

# Create metric data
xgb_metric <- data.frame(
  original = xgb_testing$deforested,
  predicted = ifelse(xgb_predict > 0.5, 1, 0)  # Convert probabilities to binary predictions
)

# Calculate evaluation metrics
metric_final_xgb <- calculate_metrics(
  data = xgb_metric, 
  actual_col = "original", 
  predicted_col = "predicted",
  predicted_probs = xgb_predict
)

metric_final_xgb$model <- 'xgb'

## 3.5. Exporting Data -----------------------------------------------------

# exporting the models as RDS file. 
saveRDS(
  list(
    rf = best_model_rf,
    xgb = best_model_xgb
  ),
  
  file = "D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_middle_mountains.rds"
)


# exporting metrices
write.csv(
  rbind(metric_final_rf, metric_final_xgb),
  "D:/Drive/Deforestation Modeling/Working Documents/Metrices/metrices_middle_mountains.csv",
  row.names = F
)


# Exporting Variable Importance. 

# Convert Random Forest importance to dataframe
rf_importance <- importance(best_model_rf) |> as.data.frame()
rf_importance$Feature <- rownames(rf_importance)  # Add feature column

# Convert XGBoost importance to dataframe
xgb_importance <- xgb.importance(feature_names = colnames(train_matrix), model = best_model_xgb)

# Merge the two dataframes on Feature column
combined_importance <- merge(rf_importance, xgb_importance, by = "Feature", all = TRUE)
combined_importance


write.csv(
  merge(rf_importance, xgb_importance, by = "Feature", all = TRUE),
  "D:/Drive/Deforestation Modeling/Working Documents/Variable Importance/variable_importance_middle_mountains.csv",
  row.names = F
)

rm(list = ls())
gc()

# -----------------------------------------------------------------------------=
# 4. HIGH MOUNTAINS ------------------------------------------------------------
# -----------------------------------------------------------------------------=

## 4.1. DATASET ----------------------------------------------------------------

high_mountains <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_high_mountains.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_high_mountains_PAs.csv"
  )
)

names(high_mountains)

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


## 4.2. MULTICOLLINEARITY ASSESSMENT -------------------------------------------
names(modeling_data)

vif_values <- multicollinearity_function(
  data = modeling_data[
    modeling_data$deforested ==1 ,
    c(
      "aspect", "dist_cropland", "dist_road", "dist_settlement", "dist_stream", 
      "elevation", "night_light", "popn_density", "slope"
    )
  ],
  
  th = 0.7
)

vif_values

write.csv(
  vif_values,
  "D:/Drive/Deforestation Modeling/Working Documents/VIF/vif_values_high_mountains.csv",
  row.names = F
)

## 4.3. TEST-TRAIN SPLIT --------------------------------------------------------

nrow(modeling_data[modeling_data$deforested == 1, ])


set.seed(23)

# Assuming `modeling_data` is your data 
test_train_list <- train_test_split(
  n_data =400000, 
  original_data = modeling_data,
  test_percentage = 30,
  dependent_variable = 'deforested',
  forest_column = "lc_forest_2022"
)


# Access the training and testing datasets
training_data <- test_train_list$training_data %>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope", "deforested"   
  )

testing_data <- test_train_list$testing_data%>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope", "deforested"   
  ) 

## 4.4. Modeling --------------------------------------
### RF Model ------------------------------------------
library(randomForest)

gc()

best_model_rf <- randomForest(
  deforested ~ ., 
  data = training_data, 
  ntree = 200, 
  mtry = 3, 
  nodesize = 1,
  importance = T
)


# Predict on testing data
test_prob <- predict(best_model_rf, testing_data, type = "prob")


metric_data_rf <- data.frame(
  original = testing_data$deforested,
  predicted = ifelse(test_prob[ , '1'] > 0.5, 1, 0)
)

#importance(rf_model)
metric_final_rf <- calculate_metrics(
  data = metric_data_rf, 
  actual_col = "original", 
  predicted_col = "predicted", 
  predicted_probs = test_prob[ , '1']
)

metric_final_rf$model <- 'rf'

gc()

### XGBoost  ---------------------------------------------------------------
library(xgboost)

# Access the training and testing datasets
xgb_training <- training_data |>
  dplyr::mutate(
    deforested = as.numeric(as.character(deforested))
  )


xgb_testing <- testing_data |>
  dplyr::mutate(
    deforested = as.numeric(as.character(deforested))
  )


# Prepare data for XGBoost
train_matrix <- xgb.DMatrix(
  data = as.matrix(xgb_training[, -which(names(xgb_training) == "deforested")]),  # Predictors
  label = xgb_training$deforested  # Response variable
)

test_matrix <- xgb.DMatrix(
  data = as.matrix(xgb_testing[, -which(names(xgb_testing) == "deforested")]),  # Predictors
  label = xgb_testing$deforested  # Response variable
)

gc()

best_model_xgb <- xgboost(
  data = train_matrix,
  nrounds = 700,
  max_depth = 15,
  eta = 0.5,
  objective = "binary:logistic",
  verbose = 0
)



# Predict on testing data
xgb_predict <- predict(best_model_xgb, test_matrix)  # Probabilities for class 1

# Create metric data
xgb_metric <- data.frame(
  original = xgb_testing$deforested,
  predicted = ifelse(xgb_predict > 0.5, 1, 0)  # Convert probabilities to binary predictions
)

# Calculate evaluation metrics
metric_final_xgb <- calculate_metrics(
  data = xgb_metric, 
  actual_col = "original", 
  predicted_col = "predicted",
  predicted_probs = xgb_predict
)

metric_final_xgb$model <- 'xgb'

## 4.5. Exporting Data -----------------------------------------------------

# exporting the models as RDS file. 
saveRDS(
  list(
    rf = best_model_rf,
    xgb = best_model_xgb
  ),
  
  file = "D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_high_mountains.rds"
)


# exporting metrices
write.csv(
  rbind(metric_final_rf, metric_final_xgb),
  "D:/Drive/Deforestation Modeling/Working Documents/Metrices/metrices_high_mountains.csv",
  row.names = F
)


# Exporting Variable Importance. 

# Convert Random Forest importance to dataframe
rf_importance <- importance(best_model_rf) |> as.data.frame()
rf_importance$Feature <- rownames(rf_importance)  # Add feature column

# Convert XGBoost importance to dataframe
xgb_importance <- xgb.importance(feature_names = colnames(train_matrix), model = best_model_xgb)

# Merge the two dataframes on Feature column
combined_importance <- merge(rf_importance, xgb_importance, by = "Feature", all = TRUE)
combined_importance


write.csv(
  merge(rf_importance, xgb_importance, by = "Feature", all = TRUE),
  "D:/Drive/Deforestation Modeling/Working Documents/Variable Importance/variable_importance_high_mountains.csv",
  row.names = F
)

gc()


# -----------------------------------------------------------------------------=
# 5. HIGH HIMALAYA ------------------------------------------------------------
# -----------------------------------------------------------------------------=

## 5.1. DATASET ----------------------------------------------------------------

high_himalaya <- rbind(
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_high_himalaya.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_high_himalaya_PAs.csv"
  )
)

names(high_himalaya)

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


## 4.2. MULTICOLLINEARITY ASSESSMENT -------------------------------------------
names(modeling_data)

vif_values <- multicollinearity_function(
  data = modeling_data[
    modeling_data$deforested ==1 ,
    c(
      "aspect", "dist_cropland", "dist_road", "dist_settlement", "dist_stream", 
      "elevation", "night_light", "popn_density", "slope"
    )
  ],
  
  th = 0.7
)

vif_values

write.csv(
  vif_values,
  "D:/Drive/Deforestation Modeling/Working Documents/VIF/vif_values_high_himalaya.csv",
  row.names = F
)

## 4.3. TEST-TRAIN SPLIT --------------------------------------------------------

nrow(modeling_data[modeling_data$deforested == 1, ])


set.seed(23)

# Assuming `modeling_data` is your data 
test_train_list <- train_test_split(
  n_data =nrow(modeling_data[modeling_data$deforested == 1, ]), 
  original_data = modeling_data,
  test_percentage = 30,
  dependent_variable = 'deforested',
  forest_column = "lc_forest_2022"
)


# Access the training and testing datasets
training_data <- test_train_list$training_data %>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope", "deforested"   
  )

testing_data <- test_train_list$testing_data%>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope", "deforested"   
  ) 

## 4.4. Modeling --------------------------------------
### RF Model ------------------------------------------
library(randomForest)

gc()
gc()
gc()


best_model_rf <- randomForest(
  deforested ~ ., 
  data = training_data, 
  ntree = 1500, 
  mtry = 3, 
  nodesize = 1,
  importance = T
)


# Predict on testing data
test_prob <- predict(best_model_rf, testing_data, type = "prob")


metric_data_rf <- data.frame(
  original = testing_data$deforested,
  predicted = ifelse(test_prob[ , '1'] > 0.5, 1, 0)
)

#importance(rf_model)
metric_final_rf <- calculate_metrics(
  data = metric_data_rf, 
  actual_col = "original", 
  predicted_col = "predicted", 
  predicted_probs = test_prob[ , '1']
)

metric_final_rf$model <- 'rf'

gc()



## 4.5. Exporting Data -----------------------------------------------------

# exporting the models as RDS file. 
saveRDS(
  list(
    rf = best_model_rf
  ),
  
  file = "D:/Drive/Deforestation Modeling/Working Documents/Final Models/final_models_high_himalaya.rds"
)


# exporting metrices
write.csv(
 metric_final_rf,
  "D:/Drive/Deforestation Modeling/Working Documents/Metrices/metrices_high_himalaya.csv",
  row.names = F
)


# Exporting Variable Importance. 

# Convert Random Forest importance to dataframe
rf_importance <- importance(best_model_rf) |> as.data.frame()

write.csv(
  rf_importance,
  "D:/Drive/Deforestation Modeling/Working Documents/Variable Importance/variable_importance_high_himalaya.csv",
  row.names = F
)

gc()
rm(list = ls())


