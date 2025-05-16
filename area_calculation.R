
# Area Calculation ------------------------------------------------------------=

# EXPORTING WEIGHT VALUE -------------------------------------------------------
library(dplyr)

# Define regions
regions <- c("terai", "siwalik", "middle_mountains", "high_mountains", "high_himalaya")

# Define path
path <- "D:/Drive/Deforestation Modeling/Working Documents/Metrices/"

# Load AUC values into a named list
auc_list <- lapply(regions, function(region) {
  df <- read.csv(file.path(path, paste0("metrices_", region, ".csv")))
  list(
    rf = df |> filter(model == "rf") |> pull(AUC),
    xgb = df |> filter(model == "xgb") |> pull(AUC)
  )
})

# Name the list elements properly
names(auc_list) <- regions

auc_list

# Flatten the structure to have names like "rf_terai", "xgb_terai", etc.
auc_named_list <- setNames(unlist(auc_list, recursive = FALSE), 
                           paste0(rep(c("rf_", "xgb_"), each = length(regions)), regions))

# Save the list as an RDS file
saveRDS(auc_named_list, file = "")


hey <- readRDS(
  "D:/Drive/Deforestation Modeling/Working Documents/Metrices/auc_values.rds"
)

hey


# Remove empty elements
hey <- hey[lengths(hey) > 0]

# Convert list to data frame
auc_df <- stack(hey)
colnames(auc_df) <- c("AUC", "Model")

# Extract Region from Model names
auc_df$Region <- sub("^(rf_|xgb_)", "", auc_df$Model)
auc_df$Model <- sub("_.*", "", auc_df$Model)  # Extract rf or xgb

# Print the data frame
print(auc_df)


# Compute weights using group_by and mutate
auc_df <- auc_df %>%
  group_by(Region) %>%
  mutate(Weight = AUC / sum(AUC)) %>%
  ungroup()

# Print the final data frame
print(auc_df)

write.csv(
  auc_df,
  "D:/Drive/Deforestation Modeling/Working Documents/Metrices/weight_values.csv",
  row.names = F
)




#------------------------------------------------------------------------------=
# PHYSIOGRAPHY -----------------------------------------------------------------
#------------------------------------------------------------------------------=

library(tidyverse)

csv_files <- list.files(
  path = "D:/Drive/Deforestation Modeling/Working Documents/Prediction CSV/",
  pattern = "\\physiography.csv$", 
  full.names = T
)


# Read CSV files and assign them as separate variables
lapply(csv_files, 
       function(f) assign(tools::file_path_sans_ext(basename(f)), 
                          read.csv(f), 
                          envir = .GlobalEnv)
      ) |> invisible()


# IMPORTING RDS FILE OF AUC values. 

weight_values <- read.csv(
  "D:/Drive/Deforestation Modeling/Working Documents/Metrices/weight_values.csv"
)




# List of regions
regions <- c("terai", "siwalik", "middle_mountains", "high_mountains")

# Initialize an empty data frame to store all regions' predictions
final_ensemble_predictions <- data.frame()

# Loop through each region
for (region in regions) {
  
  # Dynamically get the dataset
  pred_data <- get(paste0("prediction_", region, "_physiography"))
  
  # Extract the weights for this region
  region_weights <- weight_values[weight_values$Region == region, ]
  
  # Ensure the weights are correctly assigned
  w_rf <- region_weights$Weight[region_weights$Model == "rf"]
  w_xgb <- region_weights$Weight[region_weights$Model == "xgb"]
  
  # Compute weighted ensemble prediction
  pred_data$ensemble_predict <- (w_rf * pred_data$rf_predict + w_xgb * pred_data$xgb_predict) / (w_rf + w_xgb)
  
  # Add the Region column
  pred_data$Region <- region
  
  # Select only required columns
  pred_data <- pred_data[, c("x", "y", "Region", "ensemble_predict")]
  
  # Append to final data frame
  final_ensemble_predictions <- rbind(final_ensemble_predictions, pred_data)
}

# Print first few rows to verify
head(final_ensemble_predictions)


# working on high himalaya
ensemble_high_himalaya <- prediction_high_himalaya_physiography |>
  rename(
    ensemble_predict = rf_predict
  ) |>
  mutate(
    Region = "high_himalaya"
  )



final_ensemble_predictions <- rbind(
  final_ensemble_predictions,
  ensemble_high_himalaya
)


final_ensemble_predictions$predicted_bin <- cut(final_ensemble_predictions$ensemble_predict, 
                                                            breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                                                            labels = c("Very Low", "Low", "Medium", "High", "Very High"), 
                                                            include.lowest = TRUE)



write.csv(
  final_ensemble_predictions,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/prediction_total_ensemble.csv",
  row.names = F
)


# Compute frequency of predicted bins grouped by Region
bin_frequencies <- as.data.frame(table(final_ensemble_predictions$Region, final_ensemble_predictions$predicted_bin))

# Rename columns for clarity
colnames(bin_frequencies) <- c("region", "predicted_bin", "frequency_counts")

# Print the first few rows
head(bin_frequencies)


bin_frequencies <- bin_frequencies |>
  dplyr::mutate(
    total_area_sq_m = frequency_counts * 10000,
    total_area_sq_km = frequency_counts * 0.01
  )|>
  dplyr::group_by(region) |>
  dplyr::mutate(percentage_physiography = (frequency_counts / sum(frequency_counts)) * 100)|>
  dplyr::ungroup()






head(bin_frequencies, 12)

# exporting the csv file. 
write.csv(
  bin_frequencies,
  "D:/Drive/Deforestation Modeling/Working Documents/Area/area_covered_physiography.csv",
  row.names = F
)


rm(list = ls())
gc()




#------------------------------------------------------------------------------=
# PROVINCES --------------------------------------------------------------------
#------------------------------------------------------------------------------=

library(tidyverse)

csv_files <- list.files(
  path = "D:/Drive/Deforestation Modeling/Working Documents/Prediction CSV/",
  pattern = "\\province.csv$", 
  full.names = T
)


# Read CSV files and assign them as separate variables
lapply(csv_files, 
       function(f) assign(tools::file_path_sans_ext(basename(f)), 
                          read.csv(f), 
                          envir = .GlobalEnv)
) |> invisible()


prediction_high_mountains_province |> head()


# IMPORTING RDS FILE OF AUC values. 

weight_values <- read.csv(
  "D:/Drive/Deforestation Modeling/Working Documents/Metrices/weight_values.csv"
)


# List of regions, except high_himalaya, because, ensemble modeling is not implemented there. 
regions <- c("terai", "siwalik", "middle_mountains", "high_mountains")

# Initialize an empty data frame to store all regions' predictions
final_ensemble_predictions <- data.frame()

# Loop through each region
for (region in regions) {
  
  # Dynamically get the dataset
  pred_data <- get(paste0("prediction_", region, "_province"))
  
  # Extract the weights for this region
  region_weights <- weight_values[weight_values$Region == region, ]
  
  # Ensure the weights are correctly assigned
  w_rf <- region_weights$Weight[region_weights$Model == "rf"]
  w_xgb <- region_weights$Weight[region_weights$Model == "xgb"]
  
  # Compute weighted ensemble prediction
  pred_data$ensemble_predict <- (w_rf * pred_data$rf_predict + w_xgb * pred_data$xgb_predict) / (w_rf + w_xgb)
  
  # Add the Region column
  pred_data$Region <- region
  
  # Select only required columns
  pred_data <- pred_data[, c("x", "y", "Region", "province", "ensemble_predict")]
  
  # Append to final data frame
  final_ensemble_predictions <- rbind(final_ensemble_predictions, pred_data)
}

# Print first few rows to verify
head(final_ensemble_predictions)


# working on high himalaya
ensemble_high_himalaya <- prediction_high_himalaya_province |>
  rename(
    ensemble_predict = rf_predict
  ) |>
  mutate(
    Region = "high_himalaya"
  )



final_ensemble_predictions <- rbind(
  final_ensemble_predictions,
  ensemble_high_himalaya
)


final_ensemble_predictions$predicted_bin <- cut(final_ensemble_predictions$ensemble_predict, 
                                                breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                                                labels = c("Very Low", "Low", "Medium", "High", "Very High"), 
                                                include.lowest = TRUE)



# Compute frequency of predicted bins grouped by Region
bin_frequencies <- as.data.frame(table(final_ensemble_predictions$province, final_ensemble_predictions$predicted_bin))

# Rename columns for clarity
colnames(bin_frequencies) <- c("province", "predicted_bin", "frequency_counts")

# Print the first few rows
head(bin_frequencies)


bin_frequencies <- bin_frequencies |>
  dplyr::mutate(
    total_area_sq_m = frequency_counts * 10000,
    total_area_sq_km = frequency_counts * 0.01
  )|>
  dplyr::group_by(province) |>
  dplyr::mutate(percentage_province = (frequency_counts / sum(frequency_counts)) * 100)|>
  dplyr::ungroup()






head(bin_frequencies, 12)

# exporting the csv file. 
write.csv(
  bin_frequencies,
  "D:/Drive/Deforestation Modeling/Working Documents/Area/area_covered_province.csv",
  row.names = F
)


rm(list = ls())
gc()




















# OVERALL ---------------------------------------------------------------------

library(dplyr)

ensemble_final <- read.csv(
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/prediction_total_ensemble.csv"
)

bin_frequency <- table(ensemble_final$predicted_bin) |> as.data.frame()

bin_frequency <- bin_frequency |>
  rename(
    Probability = Var1,
    total_frequency = Freq
  ) |>
  mutate(
    total_area_sq_m = total_frequency * 10000,
    total_area_sq_km = total_frequency * 0.01,
    percentage_risk = total_frequency/sum(total_frequency)*100,
  )




write.csv(
  bin_frequency, 
  "D:/Drive/Deforestation Modeling/Working Documents/Area/area_covered_total.csv",
  row.names = F
  )


rm(list = ls())
gc()


## DISTRICTs--------------------------------------------------------------------


district_data <- read.csv(
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/prediction_districts.csv"
  
)

frequency_count <- table(district_data$DISTRICT, district_data$predicted_bin)|> as.data.frame()

library(tidyverse)

frequency_count <- frequency_count |>
  rename(
    districts = Var1,
    risk_classes = Var2,
    total_frequency = Freq
  )


frequency_count <- frequency_count |>
  mutate(
    total_area_sq_m = total_frequency * 10000,
    total_area_sq_km = total_frequency * 0.01
  )|>
  group_by(districts)|>
  mutate(
    percentage_risk = total_frequency/sum(total_frequency)*100,
  )



write.csv(
  frequency_count,
  "D:/Drive/Deforestation Modeling/Working Documents/Area/area_covered_districts.csv",
  row.names = F
)


