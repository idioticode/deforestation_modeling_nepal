# PACKAGES ---------------------------------------------------------------------

# loading necessary packages 
library(tidyverse)
library(pROC)


# -----------------------------------------------------------------------------=
# FUNCTIONS --------------------------------------------------------------------
# -----------------------------------------------------------------------------=

# multicollinearity assessment --

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


## Training and Testing dataset --

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


## Metric Calculation --

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



# DATASET ----------------------------------------------------------------------
high_mountain <- rbind(
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Individual_Regions/raster_df_high_mountains.csv"
  ),
  
  read.csv(
    "D:/MISC/Deforestation_modeling_rasters/CSV_files/Protected Areas Individual Regions/raster_df_high_mountains_PAs.csv"
  )
  
)

modeling_data <- high_mountain %>% 
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


# MULTICOLLINEARITY ASSESSMENT ------------------------------------------------
names(modeling_data)

multicollinearity_function(
  data = modeling_data[
    modeling_data$deforested ==1 ,
    c(
      "aspect", "dist_cropland", "dist_road", "dist_settlement", "dist_stream", 
      "elevation", "night_light", "popn_density", "slope"
    )
  ],
  
  th = 0.7
)

# TEST-TRAIN SPLIT -------------------------------------------------------------

nrow(modeling_data[modeling_data$deforested == 1, ])

# Assuming `modeling_data` is your data 
test_train_list <- train_test_split(
  n_data = 400000, 
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


# RF --------------------------------------
library(randomForest)
library(pROC)

# Initialize variables to store the best model, AUC, and parameters
best_model_rf <- NULL
best_auc_rf <- 0
best_params_rf <- list()

## Hyperparameter Tuning

gc()

# Perform grid search
for (mtry_val in c(3, 5, 7)) {
  for (nodesize_val in c(1, 3, 5, 7)) {
    for (ntree_val in c(200, 300, 500, 700)) {
      cat("Training model with mtry =", mtry_val, 
          ", nodesize =", nodesize_val, ", ntree =", ntree_val, "\n")
      
      gc()
      
      # Train Random Forest model
      rf_model <- randomForest(
        deforested ~ ., 
        data = training_data, 
        ntree = ntree_val, 
        mtry = mtry_val, 
        nodesize = nodesize_val
      )
      
      # Predict probabilities on testing data
      test_prob <- predict(rf_model, testing_data, type = "prob")[, '1']
      
      # Compute AUC
      
      auc_value <- roc(testing_data$deforested, test_prob, levels = c(0,1), direction = "<")$auc
      
      
      cat("AUC:", auc_value, "\n \n")
      
      # Store best model
      if (auc_value > best_auc_rf) {  # Compare with best_auc_rf
        best_auc_rf <- auc_value
        best_model_rf <- rf_model
        best_params_rf <- list(mtry = mtry_val, nodesize = nodesize_val, ntree = ntree_val)
        
        gc()
        
      }
    }
  }
}

# Print best parameters and AUC
print(best_params_rf)
cat("Best AUC:", best_auc_rf, "\n")

# Train Random Forest model
best_model_rf <- randomForest(
  deforested ~ ., 
  data = training_data, 
  ntree = 200, 
  mtry = 3, 
  nodesize = 1
)

# Update the best model's nodesize (optional, for reference)
best_model_rf$nodesize <- 1

##
best_models <- list(
  rf = best_model_rf
)

# Save the trained Random Forest model
saveRDS(best_models, file = "D:/MISC/Deforestation_modeling_rasters/Final_Models/best_models_high_mountain.rds")



importance(best_model_rf)



# Predict on testing data
test_prob <- predict(rf_model, testing_data, type = "prob")


metric_data <- data.frame(
  original = testing_data$deforested,
  predicted = ifelse(test_prob[ , '1'] > 0.5, 1, 0)
)

#importance(rf_model)
calculate_metrics(
  data = metric_data, 
  actual_col = "original", 
  predicted_col = "predicted", 
  predicted_probs = test_prob[ , '1']
)


# Predicting in forest only
forest_only <- high_mountain %>% 
  dplyr::select(
    -ndvi_2000, -ndvi_2024, -region
  ) %>%  
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
  )



final_test_prob <- predict(rf_model, forest_only, type = "prob")

final_df <- data.frame(
  x = forest_only$x,
  y =forest_only$y,
  predicted = final_test_prob[ , '1']
)

names(final_df)

final_df$predicted_bin <- cut(final_df$predicted, 
                              breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                              labels = c("Very Low", "Low", "Medium", "High", "Very High"), 
                              include.lowest = TRUE)

table(final_df$predicted_bin) |> as.data.frame()


high_mountain_sf <- sf::st_read(
  "D:/MISC/points/Physiography/physiography_nepal_updated.shp"
) %>%
  dplyr::select(Physio)  %>% 
  dplyr::filter(Physio == "high_mountain")


image <- ggplot()+
  geom_sf(
    data = high_mountain_sf,
    color = "black",
    fill = "grey"
  )+
  geom_raster(
    data = final_df,
    aes(x = x,
        y = y,
        fill = predicted_bin)
  )+
  scale_fill_manual(
    values = c(
      "Very Low" = "green", 
      "Low" = "forestgreen",
      "Medium" = "yellow", 
      "High" = "red", 
      "Very High" = "maroon"
    )
  )+
  theme_classic()




ggsave(
  'D:/MISC/Deforestation_modeling_rasters/Final_Images/SVG/high_mountain_rf.svg',
  image,
  width = 12,
  height = 5
)



# XGBoost  ---------------------------------------------------------------
library(xgboost)


# Access the training and testing datasets
xgb_training <- training_data |>
  dplyr::mutate(
    deforested = as.numeric(as.character(deforested))
  )

table(xgb_training$deforested)

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


xgb_new <- xgboost(
  data = train_matrix,
  nrounds = 300,
  max_depth = 9,
  eta = 0.01,
  objective = "binary:logistic",
  verbose = 0
)

## Hyperparameter Tuning -------------------------------------------------=

best_model_xgb <- NULL
best_auc_xgb <- 0
best_params_xgb <- list()

# eta_values  = learning rate
# depth values = max depth of trees
# nround_values = number of boosting rounds

# Perform grid search
for (eta_val in c(0.5)) { # o.01 and 0.001 are removed for the second tuning. 
  for (depth_val in c(15)) {
    for (nrounds_val in c(200, 300, 500, 700, 900)) {
      cat("Training model with eta =", eta_val, ", max_depth =", depth_val, ", nrounds =", nrounds_val, "\n")
      
      gc()
      
      # Train XGBoost model
      xgb_model <- xgboost(
        data = train_matrix,
        nrounds = nrounds_val,
        max_depth = depth_val,
        eta = eta_val,
        objective = "binary:logistic",
        verbose = 0
      )
      
      # Predict probabilities on testing data
      xgb_predict <- predict(xgb_model, test_matrix)
      
      # Compute AUC
      auc_value <- roc(xgb_testing$deforested, xgb_predict, levels = c(0,1), direction = "<")$auc
      
      cat("AUC:", auc_value, "\n")
      
      # Store best model
      if (auc_value > best_auc_xgb) {  # Corrected this line
        best_auc_xgb <- auc_value
        best_model_xgb <- xgb_model
        best_params_xgb <- list(eta = eta_val, max_depth = depth_val, nrounds = nrounds_val)
        
        gc()
      }
    }
  }
}





# Print best parameters and AUC
print(best_params_xgb)
cat("Best AUC:", best_auc_xgb, "\n")

best_model_xgb$eta <- best_params_xgb$eta #0.5
best_model_xgb$nrounds <- best_params_xgb$nrounds # 700
best_model_xgb$max_depth <- best_params_xgb$max_depth # 15

best_model_xgb



best_models <- list(
  rf = best_model_rf,
  xgb = best_model_xgb
)

best_models


# Save the trained Random Forest model
saveRDS(best_models, file = "D:/MISC/Deforestation_modeling_rasters/Final_Models/best_models_high_mountain.rds")

# Predict on testing data
xgb_predict <- predict(best_model_xgb, test_matrix)  # Probabilities for class 1

# Create metric data
xgb_metric <- data.frame(
  original = xgb_testing$deforested,
  predicted = ifelse(xgb_predict > 0.5, 1, 0)  # Convert probabilities to binary predictions
)

# Calculate evaluation metrics
calculate_metrics(
  data = xgb_metric, 
  actual_col = "original", 
  predicted_col = "predicted",
  predicted_probs = xgb_predict
)


# Naive Bayes --------------------------------------
library(e1071)


# Initialize variables to store the best model, AUC, and parameters
best_model_nb <- NULL
best_auc_nb <- 0
best_laplace_nb <- 0

# Define a range of Laplace smoothing values to try

# Perform grid search over Laplace values
for (laplace_val in c(0, 1, 2, 5, 10)) {
  cat("Training Naive Bayes model with Laplace =", laplace_val, "\n")
  
  # Train Naive Bayes model
  nb_model <- naiveBayes(
    deforested ~ .,  # Predict deforested using all other columns
    data = training_data,  # Training data
    laplace = laplace_val  # Laplace smoothing
  )
  
  # Predict probabilities on testing data
  test_prob <- predict(nb_model, testing_data, type = "raw")[, 2]
  
  # Compute AUC
  auc_value <- roc(testing_data$deforested, test_prob, levels = c(0, 1), direction = "<")$auc
  
  cat("AUC:", auc_value, "\n")
  
  # Store best model
  if (auc_value > best_auc_nb) {
    best_auc_nb <- auc_value
    best_model_nb <- nb_model
    best_laplace_nb <- laplace_val
  }
}

# Print best Laplace value and AUC
cat("Best Laplace:", best_laplace_nb, "\n")
cat("Best AUC:", best_auc_nb, "\n")

# Return the best model
best_model_nb

gc()

# CART --------------------------------------
library(rpart)
library(pROC)

# Initialize variables to store the best model, AUC, and parameters
best_model_cart <- NULL
best_auc_cart <- 0
best_params_cart <- list()

# Define ranges for hyperparameters & Perform grid search
for (minsplit_val in c(5, 10, 20, 30)) {
  for (minbucket_val in c(2, 5, 10, 15)) {
    for (cp_val in c(0.001, 0.01, 0.1, 0.2)) {
      for (maxdepth_val in c(5, 10, 15, 20)) {
        cat("Training CART model with minsplit =", minsplit_val, 
            ", minbucket =", minbucket_val, 
            ", cp =", cp_val, 
            ", maxdepth =", maxdepth_val, "\n")
        
        # Train CART model
        cart_model <- rpart(
          deforested ~ .,  # Predict deforested using all other columns
          data = training_data,  # Training data
          method = "class",  # For classification
          control = rpart.control(
            minsplit = minsplit_val,
            minbucket = minbucket_val,
            cp = cp_val,
            maxdepth = maxdepth_val
          )
        )
        
        # Predict probabilities on testing data
        test_prob <- predict(cart_model, testing_data, type = "prob")[, 2]
        
        # Compute AUC
        auc_value <- roc(testing_data$deforested, test_prob, levels = c(0, 1), direction = "<")$auc
        
        cat("AUC:", auc_value, "\n")
        
        # Store best model
        if (auc_value > best_auc_cart) {
          best_auc_cart <- auc_value
          best_model_cart <- cart_model
          best_params_cart <- list(
            minsplit = minsplit_val,
            minbucket = minbucket_val,
            cp = cp_val,
            maxdepth = maxdepth_val
          )
        }
      }
    }
  }
}

# Print best parameters and AUC
print(best_params_cart)
cat("Best AUC:", best_auc_cart, "\n")

# Return the best model
best_model_cart |> summary()


best_models <- list(
  rf = best_model_rf,
  xgb = best_model_xgb,
  cart = best_model_cart
)

best_models


# Save the trained Random Forest model
saveRDS(best_models, file = "D:/MISC/Deforestation_modeling_rasters/Final_Models/best_models_high_mountain.rds")

gc()

# MaxEnt --------------------------------------
library(dismo)
library(pROC)

# Initialize variables to store the best model, AUC, and parameters
best_model_maxent <- NULL
best_auc_maxent <- 0
best_betamultiplier_maxent <- NULL

# Perform grid search over betamultiplier values
for (betamultiplier_val in c(0.5, 1, 2, 5)) {  # Regularization strength
  cat("Training MaxEnt model with betamultiplier =", betamultiplier_val, "\n")
  
  # Train MaxEnt model
  maxent_model <- maxent(
    x = training_data[, -which(names(training_data) == "deforested")],  # Predictors
    p = training_data$deforested,  # Response variable
    args = c(
      paste0("betamultiplier=", betamultiplier_val)
    )
  )
  
  # Predict probabilities on testing data
  test_prob <- predict(maxent_model, testing_data[, -which(names(testing_data) == "deforested")])
  
  # Compute AUC
  auc_value <- roc(testing_data$deforested, test_prob, levels = c(0, 1), direction = "<")$auc
  
  cat("AUC:", auc_value, "\n")
  
  # Store best model
  if (auc_value > best_auc_maxent) {
    best_auc_maxent <- auc_value
    best_model_maxent <- maxent_model
    best_betamultiplier_maxent <- betamultiplier_val
  }
}

# Print best betamultiplier and AUC
cat("Best betamultiplier:", best_betamultiplier_maxent, "\n")
cat("Best AUC:", best_auc_maxent, "\n")

# Return the best model
best_model_maxent


## ANN --------------------------------------
library(nnet)
library(pROC)

# Initialize variables to store the best model, AUC, and parameters
best_model_ann <- NULL
best_auc_ann <- 0
best_params_ann <- list()

# Perform grid search
for (size_val in c(3, 5, 10, 15)) {  # Number of units in the hidden layer
  for (decay_val in c(0.001, 0.01, 0.1, 0.5)) {  # Weight decay
    for (maxit_val in c(500, 1000, 1500)) {  # Maximum number of iterations
      cat("Training ANN model with size =", size_val, 
          ", decay =", decay_val, 
          ", maxit =", maxit_val, "\n")
      
      # Train ANN model
      ann_model <- nnet(
        deforested ~ .,  # Predict deforested using all other columns
        data = training_data,  # Training data
        size = size_val,  # Number of units in the hidden layer
        decay = decay_val,  # Weight decay
        maxit = maxit_val,  # Maximum number of iterations
        trace = FALSE  # Suppress training output
      )
      
      # Predict probabilities on testing data
      test_prob <- predict(ann_model, testing_data, type = "raw")
      
      # Compute AUC
      auc_value <- roc(testing_data$deforested, test_prob, levels = c(0, 1), direction = "<")$auc
      
      cat("AUC:", auc_value, "\n")
      
      # Store best model
      if (auc_value > best_auc_ann) {
        best_auc_ann <- auc_value
        best_model_ann <- ann_model
        best_params_ann <- list(size = size_val, decay = decay_val, maxit = maxit_val)
      }
    }
  }
}

gc()
# Print best parameters and AUC
print(best_params_ann)
cat("Best AUC:", best_auc_ann, "\n")

# Return the best model
best_model_ann

## SVM --------------------------------------
library(e1071)
library(pROC)

# Initialize variables to store the best model, AUC, and parameters
best_model_svm <- NULL
best_auc_svm <- 0
best_params_svm <- list()

# Perform grid search
for (kernel_val in c("linear", "radial", "polynomial", "sigmoid")) {  # Kernel types
  for (cost_val in c(0.1, 1, 10, 100)) {  # Cost values
    if (kernel_val == "linear") {
      # For linear kernel, gamma is not applicable
      cat("Training SVM model with kernel =", kernel_val, 
          ", cost =", cost_val, "\n")
      
      # Train SVM model
      svm_model <- svm(
        deforested ~ .,  # Predict deforested using all other columns
        data = training_data,  # Training data
        kernel = kernel_val,  # Kernel type
        cost = cost_val,  # Cost parameter
        probability = TRUE  # Enable probability estimates
      )
    } else {
      # For non-linear kernels, include gamma in the grid search
      for (gamma_val in c(0.01, 0.1, 1, 10)) {  # Gamma values
        if (kernel_val == "polynomial") {
          # For polynomial kernel, include degree in the grid search
          for (degree_val in c(2, 3, 4)) {  # Degree values
            cat("Training SVM model with kernel =", kernel_val, 
                ", cost =", cost_val, 
                ", gamma =", gamma_val, 
                ", degree =", degree_val, "\n")
            
            # Train SVM model
            svm_model <- svm(
              deforested ~ .,  # Predict deforested using all other columns
              data = training_data,  # Training data
              kernel = kernel_val,  # Kernel type
              cost = cost_val,  # Cost parameter
              gamma = gamma_val,  # Gamma parameter
              degree = degree_val,  # Degree parameter (for polynomial kernel)
              probability = TRUE  # Enable probability estimates
            )
            
            # Predict probabilities on testing data
            test_prob <- attr(predict(svm_model, testing_data, probability = TRUE), "probabilities")[, 2]
            
            # Compute AUC
            auc_value <- roc(testing_data$deforested, test_prob, levels = c(0, 1), direction = "<")$auc
            
            cat("AUC:", auc_value, "\n")
            
            # Store best model
            if (auc_value > best_auc_svm) {
              best_auc_svm <- auc_value
              best_model_svm <- svm_model
              best_params_svm <- list(
                kernel = kernel_val,
                cost = cost_val,
                gamma = gamma_val,
                degree = degree_val
              )
            }
          }
        } else {
          # For radial and sigmoid kernels, degree is not applicable
          cat("Training SVM model with kernel =", kernel_val, 
              ", cost =", cost_val, 
              ", gamma =", gamma_val, "\n")
          
          # Train SVM model
          svm_model <- svm(
            deforested ~ .,  # Predict deforested using all other columns
            data = training_data,  # Training data
            kernel = kernel_val,  # Kernel type
            cost = cost_val,  # Cost parameter
            gamma = gamma_val,  # Gamma parameter
            probability = TRUE  # Enable probability estimates
          )
          
          # Predict probabilities on testing data
          test_prob <- attr(predict(svm_model, testing_data, probability = TRUE), "probabilities")[, 2]
          
          # Compute AUC
          auc_value <- roc(testing_data$deforested, test_prob, levels = c(0, 1), direction = "<")$auc
          
          cat("AUC:", auc_value, "\n")
          
          # Store best model
          if (auc_value > best_auc_svm) {
            best_auc_svm <- auc_value
            best_model_svm <- svm_model
            best_params_svm <- list(
              kernel = kernel_val,
              cost = cost_val,
              gamma = gamma_val,
              degree = NA  # Degree is not applicable for non-polynomial kernels
            )
          }
        }
      }
    }
  }
}


# Print best parameters and AUC
print(best_params_svm)
cat("Best AUC:", best_auc_svm, "\n")

# Return the best model
best_model_svm


gc()


## BRT --------------------------------------
library(gbm)

# Initialize variables to store the best model, AUC, and parameters
best_model_brt <- NULL
best_auc_brt <- 0
best_params_brt <- list()

# Perform grid search
for (n_trees_val in c(100, 500, 1000)) {  # Number of trees
  for (depth_val in c(1, 3, 5, 7)) {  # Interaction depth
    for (shrinkage_val in c(0.001, 0.01, 0.1)) {  # Learning rate
      for (bag_frac_val in c(0.5, 0.75, 1)) {  # Bagging fraction
        cat("Training BRT model with n.trees =", n_trees_val, 
            ", interaction.depth =", depth_val, 
            ", shrinkage =", shrinkage_val, 
            ", bag.fraction =", bag_frac_val, "\n")
        
        # Train BRT model
        brt_model <- gbm(
          deforested ~ .,  # Predict deforested using all other columns
          data = training_data,  # Training data
          distribution = "bernoulli",  # For binary classification
          n.trees = n_trees_val,  # Number of trees
          interaction.depth = depth_val,  # Depth of each tree
          shrinkage = shrinkage_val,  # Learning rate
          bag.fraction = bag_frac_val,  # Bagging fraction
          cv.folds = 5,  # Cross-validation folds
          verbose = FALSE  # Suppress training output
        )
        
        # Predict probabilities on testing data
        test_prob <- predict(brt_model, testing_data, n.trees = n_trees_val, type = "response")
        
        # Compute AUC
        auc_value <- roc(testing_data$deforested, test_prob, levels = c(0, 1), direction = "<")$auc
        
        cat("AUC:", auc_value, "\n")
        
        # Store best model
        if (auc_value > best_auc_brt) {
          best_auc_brt <- auc_value
          best_model_brt <- brt_model
          best_params_brt <- list(
            n.trees = n_trees_val,
            interaction.depth = depth_val,
            shrinkage = shrinkage_val,
            bag.fraction = bag_frac_val
          )
        }
      }
    }
  }
}

# Print best parameters and AUC
print(best_params_brt)
cat("Best AUC:", best_auc_brt, "\n")

# Return the best model
best_model_brt


## FDA --------------------------------------
library(mda)
library(earth)  # For mars method
library(polyreg)  # For polyreg method
library(pROC)

# Initialize variables to store the best model, AUC, and parameters
best_model_fda <- NULL
best_auc_fda <- 0
best_params_fda <- list()

# Perform grid search
for (method_val in c(polyreg, mars)) {  # Use function objects directly
  if (identical(method_val, polyreg)) {
    # For polynomial regression, tune the degree
    for (degree_val in c(1, 2, 3)) {  # Polynomial degrees
      cat("Training FDA model with method = polyreg", 
          ", degree =", degree_val, "\n")
      
      # Train FDA model
      fda_model <- fda(
        deforested ~ .,  # Predict deforested using all other columns
        data = training_data,  # Training data
        method = polyreg,  # Use polyreg function
        degree = degree_val  # Degree of polynomial
      )
      
      # Predict probabilities on testing data
      test_prob <- predict(fda_model, testing_data, type = "posterior")[, 2]
      
      # Compute AUC
      auc_value <- roc(testing_data$deforested, test_prob, levels = c(0, 1), direction = "<")$auc
      
      cat("AUC:", auc_value, "\n")
      
      # Store best model
      if (auc_value > best_auc_fda) {
        best_auc_fda <- auc_value
        best_model_fda <- fda_model
        best_params_fda <- list(
          method = "polyreg",
          degree = degree_val,
          nprune = NA  # Not applicable for polyreg
        )
      }
    }
  } else {
    # For MARS, tune the number of terms
    for (nprune_val in c(5, 10, 15)) {  # Maximum number of terms
      cat("Training FDA model with method = mars", 
          ", nprune =", nprune_val, "\n")
      
      # Train FDA model
      fda_model <- fda(
        deforested ~ .,  # Predict deforested using all other columns
        data = training_data,  # Training data
        method = mars,  # Use mars function
        nprune = nprune_val  # Maximum number of terms
      )
      
      # Predict probabilities on testing data
      test_prob <- predict(fda_model, testing_data, type = "posterior")[, 2]
      
      # Compute AUC
      auc_value <- roc(testing_data$deforested, test_prob, levels = c(0, 1), direction = "<")$auc
      
      cat("AUC:", auc_value, "\n")
      
      # Store best model
      if (auc_value > best_auc_fda) {
        best_auc_fda <- auc_value
        best_model_fda <- fda_model
        best_params_fda <- list(
          method = "mars",
          degree = NA,  # Not applicable for mars
          nprune = nprune_val
        )
      }
    }
  }
}

# Print best parameters and AUC
print(best_params_fda)
cat("Best AUC:", best_auc_fda, "\n")

# Return the best model
best_model_fda

## MARS --------------------------------------
library(earth)
library(pROC)

# Initialize variables to store the best model, AUC, and parameters
best_model_mars <- NULL
best_auc_mars <- 0
best_params_mars <- list()

# Perform grid search
for (degree_val in c(1, 2, 3)) {  # Degree of interaction
  for (nprune_val in c(5, 10, 15, 20)) {  # Maximum number of terms
    cat("Training MARS model with degree =", degree_val, 
        ", nprune =", nprune_val, "\n")
    
    # Train MARS model
    mars_model <- earth(
      deforested ~ .,  # Predict deforested using all other columns
      data = training_data,  # Training data
      degree = degree_val,  # Degree of interaction
      nprune = nprune_val,  # Maximum number of terms
      glm = list(family = binomial)  # For binary classification
    )
    
    # Predict probabilities on testing data
    test_prob <- predict(mars_model, testing_data, type = "response")
    
    # Compute AUC
    auc_value <- roc(testing_data$deforested, test_prob, levels = c(0, 1), direction = "<")$auc
    
    cat("AUC:", auc_value, "\n")
    
    # Store best model
    if (auc_value > best_auc_mars) {
      best_auc_mars <- auc_value
      best_model_mars <- mars_model
      best_params_mars <- list(
        degree = degree_val,
        nprune = nprune_val
      )
    }
  }
}

# Print best parameters and AUC
print(best_params_mars)
cat("Best AUC:", best_auc_mars, "\n")

# Return the best model
best_model_mars


## GLM with Regularization (glmnet) --------------------------------------
library(glmnet)
library(pROC)

# Prepare data for glmnet
x <- model.matrix(deforested ~ ., data = training_data)[, -1]  # Predictors (remove intercept)
y <- training_data$deforested  # Response variable

# Initialize variables to store the best model, AUC, and parameters
best_model_glmnet <- NULL
best_auc_glmnet <- 0
best_alpha_glmnet <- NULL
best_lambda_glmnet <- NULL

# Perform grid search over alpha and lambda
for (alpha_val in c(0, 0.5, 1)) {  # Alpha values (0 = ridge, 1 = lasso)
  cat("Training GLMnet model with alpha =", alpha_val, "\n")
  
  # Train GLMnet model with cross-validation
  cv_glmnet <- cv.glmnet(
    x = x,  # Predictors
    y = y,  # Response variable
    family = "binomial",  # For binary classification
    alpha = alpha_val,  # L1/L2 mixing parameter
    type.measure = "auc"  # Use AUC for cross-validation
  )
  
  # Get the best lambda from cross-validation
  best_lambda <- cv_glmnet$lambda.min
  
  # Predict probabilities on testing data
  test_matrix <- model.matrix(deforested ~ ., data = testing_data)[, -1]  # Prepare testing data
  test_prob <- predict(cv_glmnet, newx = test_matrix, s = best_lambda, type = "response")
  
  # Compute AUC
  auc_value <- roc(testing_data$deforested, test_prob, levels = c(0, 1), direction = "<")$auc
  
  cat("AUC:", auc_value, "\n")
  
  # Store best model
  if (auc_value > best_auc_glmnet) {
    best_auc_glmnet <- auc_value
    best_model_glmnet <- cv_glmnet
    best_alpha_glmnet <- alpha_val
    best_lambda_glmnet <- best_lambda
  }
}

# Print best parameters and AUC
cat("Best alpha:", best_alpha_glmnet, "\n")
cat("Best lambda:", best_lambda_glmnet, "\n")
cat("Best AUC:", best_auc_glmnet, "\n")

# Return the best model
best_model_glmnet

## GAM Hyperparameter Tuning --------------------------------------
library(mgcv)
library(pROC)

# Initialize variables to store the best model, AUC, and parameters
best_model_gam <- NULL
best_auc_gam <- 0
best_params_gam <- list()

# Define the range of k values (number of basis functions)
k_values <- c(5, 10, 15)

# Perform grid search over k values
for (k_val in k_values) {
  cat("Training GAM model with k =", k_val, "\n")
  
  # Train GAM model
  gam_model <- gam(
    deforested ~ s(aspect, k = k_val) + 
      s(dist_cropland, k = k_val) + 
      s(dist_road, k = k_val) + 
      s(dist_settlement, k = k_val) + 
      s(dist_stream, k = k_val) + 
      s(elevation, k = k_val) + 
      s(night_light, k = k_val) + 
      s(popn_density, k = k_val) + 
      s(slope, k = k_val),  # Smooth terms for all predictors
    data = training_data,  # Training data
    family = binomial(),  # For binary classification
    method = "REML"  # Smoothing parameter estimation method
  )
  
  # Predict probabilities on testing data
  test_prob <- predict(gam_model, testing_data, type = "response")
  
  # Compute AUC
  auc_value <- roc(testing_data$deforested, test_prob, levels = c(0, 1), direction = "<")$auc
  
  cat("AUC:", auc_value, "\n")
  
  # Store best model
  if (auc_value > best_auc_gam) {
    best_auc_gam <- auc_value
    best_model_gam <- gam_model
    best_params_gam <- list(
      k = k_val
    )
  }
}

# Print best parameters and AUC
print(best_params_gam)
cat("Best AUC:", best_auc_gam, "\n")

# Return the best model