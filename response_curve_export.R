# EXPORTING RESPONSE CURVE DATASET ------------------------------------------=


# -----------------------------------------------------------------------------=
# FUNCTIONS -----------------------------------------------------------------------
# -----------------------------------------------------------------------------=

## RESPONSE CURVE --------------------------------------------------------------
generate_response_curves <- function(input_data, region_name, output_dir) {
  # Extract predictor variables (exclude dependent variable if present)
  predictor_vars <- setdiff(names(input_data), "deforested")  # FIXED: Use input_data
  
  # Initialize empty list to store results
  response_curve_list <- list()
  
  # Loop through each predictor variable
  for (var in predictor_vars) {
    
    # Ensure the variable exists in input_data
    if (!(var %in% names(input_data))) {
      cat("Skipping", var, "- Not found in input_data\n")
      next
    }
    
    # Generate response curve for RandomForest model
    rf_pdp <- partial(
      rf_model, 
      pred.var = as.character(var),  # FIXED: Ensure var is treated as a string
      train = input_data, 
      type = "classification", 
      prob = TRUE
    ) |> 
      as.data.frame() |> 
      rename(
        rf_response = yhat,
        Value = !!sym(var)
      )
    
    # Generate response curve for XGBoost model
    xgb_pdp <- partial(
      xgb_model, 
      pred.var = as.character(var),  # FIXED: Ensure var is treated as a string
      train = input_data, 
      type = "classification", 
      prob = TRUE
    ) |> 
      as.data.frame() |> 
      rename(
        xgb_response = yhat,
        Value = !!sym(var)
      )
    
    # Interpolate XGBoost values to match RF values
    xgb_pdp_interp <- approx(
      x = xgb_pdp$Value, y = xgb_pdp$xgb_response, xout = rf_pdp$Value
    )$y
    xgb_pdp$Value <- rf_pdp$Value  
    xgb_pdp$xgb_response <- xgb_pdp_interp  
    
    # Merge response curves
    merged_pdp <- merge(rf_pdp, xgb_pdp, by = "Value", all = TRUE) |>
      mutate(
        variable = var,
        region = region_name
      )
    
    # Append to list
    response_curve_list[[var]] <- merged_pdp
  }
  
  # Combine all response curves into a single dataframe
  final_response_curve <- bind_rows(response_curve_list)
  
  # Define output file path
  output_file <- file.path(output_dir, paste0("response_curve_", region_name, ".csv"))
  
  # Save as CSV
  write_csv(final_response_curve, output_file)
  
  cat("Response curves saved to:", output_file, "\n")
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
library(pdp)
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


training_data <- test_train_list$training_data%>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope"  
  ) %>% 
  sample_n(50000)



## Export response curve===============================================================

generate_response_curves(
  input_data = training_data, 
  region_name = "terai", 
  output_dir = "D:/Drive/Deforestation Modeling/Working Documents/Response Curve Data"
)


rm(list = setdiff(ls(), c("generate_response_curves", "train_test_split")))
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


training_data <- test_train_list$training_data%>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope"  
  ) %>% 
  sample_n(50000)



## Export response curve===============================================================

generate_response_curves(
  input_data = training_data, 
  region_name = "siwalik", 
  output_dir = "D:/Drive/Deforestation Modeling/Working Documents/Response Curve Data"
)


rm(list = setdiff(ls(), c("generate_response_curves", "train_test_split")))
gc()



# -----------------------------------------------------------------------------=
# 2. MIDDLE MOUNTAINS -----------------------------------------------------------------------
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


training_data <- test_train_list$training_data%>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope"  
  ) %>% 
  sample_n(50000)



## Export response curve===============================================================

generate_response_curves(
  input_data = training_data, 
  region_name = "middle_mountains", 
  output_dir = "D:/Drive/Deforestation Modeling/Working Documents/Response Curve Data"
)


rm(list = setdiff(ls(), c("generate_response_curves", "train_test_split")))
gc()



# -----------------------------------------------------------------------------=
# 3. HIGH MOUNTAINS -----------------------------------------------------------------------
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
  n_data =nrow(modeling_data[modeling_data$deforested == 1, ]), 
  original_data = modeling_data,
  test_percentage = 30,
  dependent_variable = 'deforested',
  forest_column = "lc_forest_2022"
)


training_data <- test_train_list$training_data%>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope"  
  ) %>% 
  sample_n(50000)



## Export response curve===============================================================

generate_response_curves(
  input_data = training_data, 
  region_name = "high_mountains", 
  output_dir = "D:/Drive/Deforestation Modeling/Working Documents/Response Curve Data"
)


rm(list = setdiff(ls(), c("generate_response_curves", "train_test_split")))
gc()



# -----------------------------------------------------------------------------=
# 3. HIGH MOUNTAINS -----------------------------------------------------------------------
# -----------------------------------------------------------------------------=


generate_response_curves_rf <- function(input_data, region_name, output_dir) {
  # Extract predictor variables (exclude dependent variable if present)
  predictor_vars <- setdiff(names(input_data), "deforested")
  
  # Initialize empty list to store results
  response_curve_list <- list()
  
  # Loop through each predictor variable
  for (var in predictor_vars) {
    
    # Ensure the variable exists in input_data
    if (!(var %in% names(input_data))) {
      cat("Skipping", var, "- Not found in input_data\n")
      next
    }
    
    # Generate response curve for RandomForest model
    rf_pdp <- partial(
      rf_model, 
      pred.var = var,  # Variable name as string
      train = input_data, 
      type = "classification", 
      prob = TRUE
    ) |> 
      as.data.frame() |> 
      rename(
        rf_response = yhat,
        Value = !!sym(var)
      )
    
    # Store response curve
    rf_pdp <- rf_pdp |> 
      mutate(
        variable = var,
        region = region_name
      )
    
    # Append to list
    response_curve_list[[var]] <- rf_pdp
  }
  
  # Combine all response curves into a single dataframe
  final_response_curve <- bind_rows(response_curve_list)
  
  # Define output file path
  output_file <- file.path(output_dir, paste0("response_curve_", region_name, ".csv"))
  
  # Save as CSV
  write_csv(final_response_curve, output_file)
  
  cat("Response curves saved to:", output_file, "\n")
}


## Dataset ==========================================

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
  n_data =nrow(modeling_data[modeling_data$deforested == 1, ]), 
  original_data = modeling_data,
  test_percentage = 30,
  dependent_variable = 'deforested',
  forest_column = "lc_forest_2022"
)


training_data <- test_train_list$training_data%>% 
  dplyr::select(
    "aspect", "dist_cropland",  "dist_road", "dist_settlement",  "dist_stream",
    "elevation", "night_light", "popn_density", "slope"  
  ) %>% 
  sample_n(50000)



## Export response curve===============================================================

generate_response_curves_rf(
  input_data = training_data, 
  region_name = "high_himalaya", 
  output_dir = "D:/Drive/Deforestation Modeling/Working Documents/Response Curve Data"
)


rm(list = setdiff(ls(), c("generate_response_curves", "train_test_split")))
gc()

