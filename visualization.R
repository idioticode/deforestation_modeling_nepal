# visualization --------------------------------------------------------------=

# FINAL MAP ------------------------------------------------------------------

library(ggplot2)
extrafont::loadfonts(quiet = T)

# Datasets --

## physiographic shape file
physiography_nepal <- sf::st_read(
  "D:/MISC/points/Physiography/physiography_nepal_updated.shp"
)|>
  dplyr::select(Physio)

# final dataset 
final_ensemble <- read.csv(
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/prediction_total_ensemble.csv"
)

# deforestation risk dataset
percent_prob <- read.csv("D:/Drive/Deforestation Modeling/Working Documents/Area/area_covered_total.csv")

#
percent_prob$Probability <- factor(
  percent_prob$Probability,
  levels = c("Very Low", "Low", "Medium", "High", "Very High")
)


# plotting risk percentage from the latter dataset 
prob_bar <- ggplot()+
  geom_bar(
    data = percent_prob,
    aes(x = percentage_risk,
        y = Probability,
        fill = Probability),
    stat = "identity",
    position = 'dodge',
    show.legend = F
  )+
  scale_fill_manual(
    values = c(
      "Very High" = "#A30000",
      "High" = "#FF3333",
      "Medium" = "#FFFF00",
      "Low" = "#47FF47",
      "Very Low" = "#00B800"
    )
  )+
  labs(x = "Risk (%)",
       title = "Deforestation Probability")+
  theme(
    axis.text = element_text(color = "black",
                             size = 10,
                             family = "Segoe UI",
                             face = "bold"),
    axis.title = element_text(color = "black",
                              size = 11,
                              family = "Segoe UI",
                              face = "bold"),
    plot.title = element_text(color = "black",
                              size = 11,
                              family = "Segoe UI",
                              face = "bold"),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    
  )


prob_bar

bar_grob <- ggplotGrob(prob_bar)
 

image <- ggplot() +
  geom_raster(
    data = final_ensemble, 
    aes(x = x,
        y = y, 
        fill = predicted_bin),
    show.legend = F
  ) +
  geom_sf(
    data = physiography_nepal,
    fill = "transparent",
    color = "black",
    linewidth = 0.3
  ) +
  scale_fill_manual(
    values = c(
      "Very High" = "#A30000",
      "High" = "#FF3333",
      "Medium" = "#FFFF00",
      "Low" = "#47FF47",
      "Very Low" = "#00B800"
    ),
    breaks = c("Very High", "High", "Medium", "Low", "Very Low"),
    name = "Deforestation Probability"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(color = "black", 
                             family = "Segoe UI", 
                             size = 11),
    legend.text = element_text(color = "black", 
                               family = "Segoe UI", 
                               size = 11),
    legend.title = element_text(color = "black", 
                                family = "Segoe UI", 
                                size = 11, 
                                face = "bold")
  )+
  annotation_custom(
    grob = bar_grob,
    xmin = 85.5, xmax = 88.5, 
    ymin = 28.5, ymax = 30
  )


ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/JPEG/final.jpg",
  image,
  width = 11,
  height = 6.5,
  dpi = 800
)  

gc()



ggsave(
 "D:/Drive/Deforestation Modeling/Final_Images/SVG/final.svg",
 image,
 width = 11,
 height = 6.5
)  
  


rm(list = ls())
gc()

# RISK BAR - PHYSIOGRAPHY ---------------------------------------------------

area_physiography <- read.csv(
  "D:/Drive/Deforestation Modeling/Working Documents/Area/area_covered_physiography.csv"
)

head(area_physiography, 3)

library(tidyverse)
extrafont::loadfonts(quiet = T)
# Ensure factor levels are ordered for correct bar positioning
area_physiography$predicted_bin <- factor(area_physiography$predicted_bin, 
                                 levels = c("Very High", "High", "Medium", "Low", "Very Low"))

area_physiography$region <- factor(area_physiography$region, 
                                   levels = c("terai", "siwalik", "middle_mountains", "high_mountains", "high_himalaya"),
                                   labels = c("Terai", "Siwalik", "Middle Mountains", "High Mountains", "High Himalaya"))

risk_physiography <- ggplot(data = area_physiography,
       aes(x = region,
           y = percentage_physiography,
           fill = predicted_bin,
           color = predicted_bin,)
  ) +
  geom_bar(
    stat = "identity", 
    linewidth = 1,
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      "Very High" = "#A30000",
      "High" = "#FF3333",
      "Medium" = "#FFFF00",
      "Low" = "#47FF47",
      "Very Low" = "#00B800"
    ),
    breaks = c("Very High", "High", "Medium", "Low", "Very Low")
  ) +
  scale_color_manual(
    values = c(
      "Very High" = "#A30000",
      "High" = "#FF3333",
      "Medium" = "#FFFF00",
      "Low" = "#47FF47",
      "Very Low" = "#00B800"
    )
  )+
  theme_bw() +
  labs(x = "Physiographic Regions",
       y = "Risk Percentage") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black",
                               size = 12,
                               family = "Segoe UI"),
  #  axis.text.x = element_text(face = "bold"),
    legend.text = element_text(size = 12,
                               family = "Segoe UI",
                               face = "bold"),
    legend.title = element_blank(),
    axis.title = element_text(size = 12, 
                              face = "bold",
                              family = "Segoe UI"),
    legend.position = c(hjust = 0.9, 
                        vjust = 0.81),
    legend.background = element_rect(color = "black",
                                     fill = "transparent",
                                     linewidth = 0.2),
    panel.background = element_rect(color = 'transparent',
                                   fill = 'transparent'),
  axis.ticks.x = element_blank()
  )+
  guides(fill = guide_legend(keywidth = 1.5, keyheight = 1)) 


ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/SVG/risk_physiography.svg",
  risk_physiography,
  width = 8, 
  height = 4
)  

  ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/JPEG/risk_physiography.jpg",
  risk_physiography,
  width = 8,
  height = 4,
  dpi = 600
)  



# RISK BAR - PROVINCES --------------------------------------------------------

library(tidyverse)
extrafont::loadfonts(quiet = T)

province_risk <- read.csv(
  "D:/Drive/Deforestation Modeling/Working Documents/Area/area_covered_province.csv"
)

head(province_risk)

province_risk$predicted_bin <- factor(province_risk$predicted_bin,
                                      levels = c("Very High", "High", "Medium", "Low", "Very Low"))


# Factor the province column with custom levels and labels
province_risk$province <- factor(
  province_risk$province,
  levels = c(7, 6, 5, 4, 3, 2, 1),  # Levels ordered from 7 to 1
  labels = c("Sudurpaschim", "Karnali", "Lumbini", "Gandaki", "Bagmati", "Madhesh", "Koshi")
)


# visualization


risk_province <- ggplot(data = province_risk,
       aes(x = province,
           y = percentage_province,
           fill = predicted_bin,
           color = predicted_bin)
) +
  geom_bar(
    stat = "identity", 
    linewidth = 1,
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      "Very High" = "#A30000",
      "High" = "#FF3333",
      "Medium" = "#FFFF00",
      "Low" = "#47FF47",
      "Very Low" = "#00B800"
    ),
    breaks = c("Very High", "High", "Medium", "Low", "Very Low")
  ) +
  scale_color_manual(
    values = c(
      "Very High" = "#A30000",
      "High" = "#FF3333",
      "Medium" = "#FFFF00",
      "Low" = "#47FF47",
      "Very Low" = "#00B800"
    )
  )+
  theme_bw() +
  labs(x = "Provinces",
       y = "Risk Percentage") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black",
                             size = 12,
                             family = "Segoe UI"),
    #  axis.text.x = element_text(face = "bold"),
    legend.text = element_text(size = 12,
                               family = "Segoe UI",
                               face = "bold"),
    legend.title = element_blank(),
    axis.title = element_text(size = 12, 
                              face = "bold",
                              family = "Segoe UI"),
    legend.position = c(hjust = 0.93, 
                        vjust = 0.81),
    legend.background = element_rect(color = "black",
                                     fill = "transparent",
                                     linewidth = 0.2),
    panel.background = element_rect(color = 'transparent',
                                    fill = 'transparent'),
    axis.ticks.x = element_blank()
  )+
  guides(fill = guide_legend(keywidth = 1.5, keyheight = 1)) 

ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/SVG/risk_province.svg",
  risk_province,
  width = 11, 
  height = 4
)  

ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/JPEG/risk_province.jpg",
  risk_province,
  width = 11, 
  height = 4,
  dpi = 600
)  

# ROC --------------------------------------------------------------------------

roc_data <- do.call(
  rbind, 
  lapply(
    list.files(
      "D:/Drive/Deforestation Modeling/Working Documents/ROC Data", 
      full.names = TRUE, 
      pattern = "\\.csv$"
    ), 
    read.csv
  )
)



library(tidyverse)
extrafont::loadfonts(quiet = T)


rf_model <- roc_data[roc_data$model == "rf", ]

head(roc_data)


ggplot(rf_model, aes(x = False_Positive_Rate, y = True_Positive_Rate, color = region)) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1, color = "Random Guess"), 
               linetype = 2, linewidth = 0.4) +  # Random guess line
  geom_line(linewidth = 0.8) +
  scale_color_manual(
    values = c(
      "terai"  = "#F500F5",
      "siwalik" = "#F57600",
      "middle_mountains" = "#00E000",
      "high_mountains" = "#FF0022",
      "high_himalaya" = "#120AFF",
      "Random Guess" = "black"  
    ),
    name = "Region",
    labels = c(
      "terai"  = "Terai",
      "siwalik" = "Siwalik",
      "middle_mountains" = "Middle Mountains",
      "high_mountains" = "High Mountains",
      "high_himalaya" = "High Himalaya",
      "Random Guess" = "Random Guess"
    ),
    breaks = c(
      "high_himalaya",
      "high_mountains",
      "middle_mountains",
      "siwalik",
      "terai",
      "Random Guess"
    )
  ) +
  labs(
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = c(hjust = 0.8, vjust = 0.23),
    axis.text = element_text(size = 10, color = "black", family = "Segoe UI"),
    axis.title = element_text(size = 10, face = "bold", family = "Segoe UI"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10, color = "black", family = "Segoe UI")
  )+
  guides(color = guide_legend(keywidth = 2.5, keyheight = 1)) 



ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/SVG/roc.svg",
  width = 6, 
  height = 3.5
)  

rm(list = ls())
gc()

# VARIABLE_IMPORTANCE --------------------------------------------------------------------------
library(tidyverse)
extrafont::loadfonts(quiet = T)

weight_values <- read.csv(
  "D:/Drive/Deforestation Modeling/Working Documents/Metrices/weight_values.csv"
)

weight_values

# Load variable importance files
var_imp_files <- list.files(
  "D:/Drive/Deforestation Modeling/Working Documents/Variable Importance", 
  full.names = TRUE, 
  pattern = "\\.csv$"
)

# Exclude the specific file (we'll handle high_himalaya separately)
var_imp_files <- var_imp_files[!grepl("variable_importance_high_himalaya.csv", var_imp_files)]

# Load the remaining files
var_imp <- do.call(rbind, lapply(var_imp_files, read.csv))

# Prepare weight data
weights_wide <- weight_values %>%
  select(Model, Region, Weight) %>%
  pivot_wider(
    names_from = Model,
    values_from = Weight,
    names_glue = "{Model}_weight"
  ) %>%
  rename(region = Region)

# Merge weights with variable importance data
var_imp <- var_imp %>%
  left_join(weights_wide, by = "region")

# Normalize Importance by Region and Compute Final Weighted Importance
var_imp <- var_imp %>%
  group_by(region) %>%
  mutate(
    RF_Importance = MeanDecreaseGini / sum(MeanDecreaseGini),  # Normalize RF
    XGB_Importance = Gain / sum(Gain)  # Normalize XGB
  ) %>%
  ungroup()

# Calculate weighted importance using the weights from weight_values
var_imp <- var_imp %>% 
  mutate(
    weighted_imp = (rf_weight * RF_Importance + xgb_weight * XGB_Importance) /
      (rf_weight + xgb_weight)
  )

# Handle high_himalaya separately (only RF)
high_himal_var_imp <- read.csv(
  "D:/Drive/Deforestation Modeling/Working Documents/Variable Importance/variable_importance_high_himalaya.csv"
)

high_himal_var_imp <- high_himal_var_imp |>
  mutate(
    weighted_imp = MeanDecreaseGini / sum(MeanDecreaseGini),  # Normalized importance
    region = "high_himalaya"
  )

# Combine all regions
final_var_imp <- rbind(
  var_imp[ , c("Feature", "weighted_imp", "region")],
  high_himal_var_imp[ , c("Feature", "weighted_imp", "region")]
)

# Rename Features for better understanding
final_var_imp$Feature <- dplyr::recode(final_var_imp$Feature,
                                       "aspect" = "Aspect",
                                       "dist_cropland" = "Distance to Cropland",
                                       "dist_road" = "Distance to Road",
                                       "dist_settlement" = "Distance to Settlement",
                                       "dist_stream" = "Distance to Stream",
                                       "elevation" = "Elevation",
                                       "night_light" = "Night Light",
                                       "popn_density" = "Population Density",
                                       "slope" = "Slope")

# Rename regions for better understanding
final_var_imp$region <- dplyr::recode(final_var_imp$region,
                                      "high_himalaya" = "High Himalaya",
                                      "high_mountains" = "High Mountains",
                                      "middle_mountains" = "Middle Mountains",
                                      "siwalik" = "Siwalik",
                                      "terai" = "Terai")

# Create variable importance plot
var_imp_plot <- ggplot(data = final_var_imp, aes(x = Feature, y = weighted_imp)) +
  geom_bar(stat = "identity", fill = "#199AF0") +
  facet_wrap(~region) +
  coord_flip() +
  labs(x = "Predictor Variables",
       y = "Relative Variable Importance") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(color = "black",
                             family = "Segoe UI",
                             size = 10),
    strip.text = element_text(color = "black",
                              size = 10,
                              face = "bold", 
                              family = "Segoe UI"),
    axis.title = element_text(color = "black",
                              size = 10,
                              face = "bold", 
                              family = "Segoe UI"),
    axis.ticks.y = element_blank()
  )

# Save plots
ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/SVG/variable_importance.svg",
  plot = var_imp_plot,
  width = 8, 
  height = 6
)  

ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/JPEG/variable_importance.jpg",
  plot = var_imp_plot,
  width = 8, 
  height = 6,
  dpi = 1000
)




# RESPONSE CURVES --------------------------------------------------------------------------

library(tidyverse)
extrafont::loadfonts(quiet = T)


response_files <- list.files(
  "D:/Drive/Deforestation Modeling/Working Documents/Response Curve Data",
  full.names = TRUE, 
  pattern = "\\.csv$"
)

# Exclude the specific file
response_files <- response_files[!grepl("response_curve_high_himalaya.csv", response_files)]

# Load the remaining files
response_curves <- do.call(rbind, lapply(response_files, read.csv))


weight_values <- read.csv(
  "D:/Drive/Deforestation Modeling/Working Documents/Metrices/weight_values.csv"
)


# First, let's prepare the weight data
weights_wide <- weight_values %>%
  select(Model, Region, Weight) %>%
  pivot_wider(
    names_from = Model,
    values_from = Weight,
    names_glue = "{Model}_weight"
  ) %>%
  rename(region = Region)

# Merge with response curves
response_curves <- response_curves %>%
  left_join(weights_wide, by = "region")


# Calculate weighted average response
response_curves <- response_curves %>%
  mutate(
    weighted_response = (rf_weight*rf_response + xgb_weight*xgb_response)/(rf_weight + xgb_weight)
    )

# Recode variable names
response_curves$variable <- recode(
  response_curves$variable,
  "aspect" = "Aspect",
  "dist_cropland" = "Distance to Cropland",
  "dist_road" = "Distance to Road",
  "dist_settlement" = "Distance to Settlement",
  "dist_stream" = "Distance to Stream",
  "elevation" = "Elevation",
  "night_light" = "Nighttime Light",
  "popn_density" = "Population Density",
  "slope" = "Slope"
)

# converting distance lat-lon in meters 
response_curves <- response_curves |>
  mutate(
    final_value = case_when(
      variable == "Distance to Cropland" ~ Value * 100.95009028379,
      variable == "Distance to Road" ~ Value * 100.95009028379,
      variable == "Distance to Settlement" ~ Value * 100.95009028379,
      variable == "Distance to Stream" ~ Value * 100.95009028379,
      TRUE ~ Value
    )
  )


# Get unique regions
unique_regions <- unique(response_curves$region)

# Generate plots for each region
for (regions in unique_regions){
  
  data_set <- response_curves[response_curves$region == regions, ]
  
  image <- ggplot(data = data_set,
                  aes(x = final_value,
                      y = weighted_response)
  ) +
    geom_smooth(
      color = "#282828",
      se = FALSE,
      method = "loess",
      linetype = 2
    ) +
    geom_line(
      color = "#F500F5"
    ) +
    facet_wrap(
      ~ variable,
      scale = "free_x"
    ) +
    theme_bw() +
    labs(
      x = "Variable Values",
      y = "Probability",
      title = paste("Response Curves for", str_to_title(gsub("_", " ", regions)))
    ) +
    theme(
      panel.grid = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 10,
                                color = "black",
                                face = "bold",
                                family = "Segoe UI"),
      axis.text = element_text(color = "black",
                               family = "Segoe UI",
                               size = 10),
      axis.title = element_text(color = "black",
                                face = "bold",
                                family = "Segoe UI",
                                size = 10),
      legend.text = element_text(color = "black",
                                 family = "Segoe UI",
                                 size = 10),
      legend.title = element_blank(),
      legend.position = "top",
      plot.title = element_text(hjust = 0.5, face = "bold", family = "Segoe UI")
    )+
    coord_cartesian(ylim = c(0.3, 0.65))
  
  ggsave(
    plot = image,
    file = paste0("D:/Drive/Deforestation Modeling/Final_Images/JPEG/Response Curves/response_curve_", regions, ".jpg"),
    width = 8,
    height = 6,
    dpi = 600
  )  
  
  
  ggsave(
    plot = image,
    file = paste0("D:/Drive/Deforestation Modeling/Final_Images/SVG/Response Curves/response_curve_", regions, ".svg"),
    width = 8,
    height = 6
  )  
  
  
}

# for high himalaya region


high_himalaya_response <- read.csv(
  "D:/Drive/Deforestation Modeling/Working Documents/Response Curve Data/response_curve_high_himalaya.csv"
)

names(high_himalaya_response)

high_himalaya_response$variable <- high_himalaya_response$variable |>
  recode(
    "aspect" = "Aspect",
    "dist_cropland" = "Distance to Cropland",
    "dist_road" = "Distance to Road",
    "dist_settlement" = "Distance to Settlement",
    "dist_stream" = "Distance to Stream",
    "elevation" = "Elevation",
    "night_light" = "Nighttime Light",
    "popn_density" = "Population Density",
    "slope" = "Slope"
  )

# converting distance lat-lon in meters 
high_himalaya_response <- high_himalaya_response |>
   mutate(
    final_value = case_when(
      variable == "Distance to Cropland" ~ Value * 100.95009028379,
      variable == "Distance to Road" ~ Value * 100.95009028379,
      variable == "Distance to Settlement" ~ Value * 100.95009028379,
      variable == "Distance to Stream" ~ Value * 100.95009028379,
      TRUE ~ Value
    )
  )


high_himal_res <- ggplot(data = high_himalaya_response,
       aes(x= final_value,
           y= rf_response)
   )+
  geom_smooth(
     color = "#282828",
     se = F,
     method = "loess",
     linetype = 2
   )+
  geom_line(
    color = "#F500F5"
  )+
  facet_wrap(
    ~ variable,
    scale = "free_x"
  )+
  theme_bw()+
  labs(
    x = "Variable Values",
    y = "Probability",
    title = "Response Curves for High Himalayas"
    
  )+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 10,
                              color = "black",
                              face = "bold",
                              family = "Segoe UI"),
    axis.text = element_text(color = "black",
                             family = "Segoe UI",
                             size = 10),
    axis.title = element_text(color = "black",
                              face = "bold",
                              family = "Segoe UI",
                              size = 10),
    legend.text = element_text(color = "black",
                               family = "Segoe UI",
                               size = 10),
    legend.title = element_blank(),
    legend.position =  "top",
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Segoe UI")
  )+
  coord_cartesian(ylim = c(0.3, 0.65))
  

ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/SVG/Response Curves/response_curve_high_himalaya.svg",
  high_himal_res,
  width = 8 ,
  height = 6
)  


ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/JPEG/Response Curves/response_curve_high_himalaya.jpg",
  high_himal_res,
  width = 8 ,
  height = 6,
  dpi = 600
)  


# STUDY AREA ------------------------------------------------------------------

library(tidyverse)
library(sf)
extrafont::loadfonts(quiet = T)

physiography_nepal <- sf::st_read(
  "D:/MISC/points/Physiography/physiography_nepal_updated.shp"
)|>
  dplyr::select(Physio)




province_nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_1.shp"
)


# Add province names to the province_nepal data frame
province_nepal <- province_nepal %>%
  mutate(
    PROVINCE_NAME = case_when(
      PROVINCE == 1 ~ "Koshi",
      PROVINCE == 2 ~ "Madhesh",
      PROVINCE == 3 ~ "Bagmati",
      PROVINCE == 4 ~ "Gandaki",
      PROVINCE == 5 ~ "Lumbini",
      PROVINCE == 6 ~ "Karnali",
      PROVINCE == 7 ~ "Sudurpaschim"
    )
  )

  # Update the ggplot code to use PROVINCE_NAME for labels
  ggplot() +
    geom_sf(
      data = physiography_nepal,
      color = "white",
      aes(fill = Physio),
      alpha = 1,
      linewidth = 0.3,
      size = 0.1
    )+
    scale_fill_viridis_d(
      option = "viridis", 
      direction = 1, 
      begin = 0.3, 
      end = 1,
      name = "Physiographic Regions"
    ) +
    geom_sf(
      data = province_nepal,
      color = "black",
      fill = "transparent",
      linewidth = 0.5
    ) +
    geom_sf_text(
      data = province_nepal,
      aes(label = PROVINCE_NAME),  # Use PROVINCE_NAME for labels
      color = "black",
      size = 3.5,
      fontface = "bold",
      family = "Segoe UI",
      fun.geometry = st_centroid
    )+
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "false",
      pad_x = unit(0, "cm"),
      pad_y = unit(0.3, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering(
        fill = c("black", "black"),
        line_col = "white",
        text_family = "Britannic Bold",  # Changed to a commonly available font
        text_col = "Black",
        text_size = 11
      )
    )+
    ggspatial::annotation_scale(
      location = "bl",
      style = "ticks",
      line_col = "black",
      pad_x = unit(2, "cm"),
      pad_y = unit(1.5, "cm"),
      text_col = "black",
      text_family = "Segoe UI",
      text_cex = 0.7
    ) +
    theme_bw()+
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(color = "black",
                               size = 10, 
                               family = "Segoe UI"),
      legend.text = element_text(color = "black",
                                 size = 10, 
                                 family = "Segoe UI"),
      legend.title = element_text(color = "black",
                                  size = 10, 
                                  face = "bold",
                                  family = "Segoe UI"),
      legend.position = c(hjust = 0.9,
                          vjust = 0.65),
      axis.ticks.length.y = unit(-2, "mm"),
      axis.ticks.length.x = unit(-2, "mm"),
      axis.text.x = element_text(hjust = 0.5,
                                 vjust = 2,
                                 margin = margin(t = -14)
                                 ),
      axis.text.y = element_text(
        hjust = 1,  # Align text to the right (inside the plot)
        margin = margin(r = -30)  # Move text closer to the axis line
      )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.07, 0.01)),
                       breaks = c(27, 28, 29, 30))+
    scale_x_continuous(expand = expansion(mult = c(0.3, 0.03)))

ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/jpeg/study_area.jpg",
  study_area,
  width = 9,
  height = 5,
  dpi = 1000
)



ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/SVG/study_area.svg",
  study_area,
  width = 9,
  height = 5
)  




# LAND COVER MAP --------------------------------------------------------------

library(terra)
extrafont::loadfonts(quiet = T)

physiography_nepal <- sf::st_read(
  "D:/MISC/points/Physiography/physiography_nepal_updated.shp"
)|>
  dplyr::select(Physio) 


land_cover <- terra::rast(
  "D:/MISC/Deforestation_modeling_rasters/Raster_Nepal/Land_Cover_2022_100m.tif"
)


land_cover_df <- land_cover |> 
  as.data.frame(xy = T, na.rm = T)


unique(land_cover_df$Land_Cover_2022_100m)


# Define mapping of grid values to land cover class names
land_cover_labels <- c(
  "1"  = "Waterbody",
  "2"  = "Glacier",
  "3"  = "Snow",
  "4"  = "Forest",
  "5"  = "Riverbed",
  "6"  = "Built-up area",
  "7"  = "Cropland",
  "8"  = "Bare soil",
  "9"  = "Bare rock",
  "10" = "Grassland",
  "11" = "Other wooded land"
)

# Define hex colors for each land cover class
land_cover_colors <- c(
  "Waterbody" = "#005BE7",
  "Glacier" = "#B9B9B9",
  "Snow" = "#C8D79E",
  "Forest" = "#0D6E23",
  "Riverbed" = "#01C5FF",
  "Built-up area" = "#EE0000",
  "Cropland" = "#FFFF00",
  "Bare soil" = "#FF73DE",
  "Bare rock" = "#C500FF",
  "Grassland" = "#94E400",
  "Other wooded land" = "#5CFF5C"
)

# Convert numeric land cover values to factor with labels
land_cover_df <- land_cover_df |>
  dplyr::mutate(Land_Cover_2022_100m = factor(Land_Cover_2022_100m, 
                                       levels = names(land_cover_labels), 
                                       labels = land_cover_labels))

# Plot using ggplot2
lc_plot <-ggplot() +
  geom_raster(
    data = land_cover_df, 
    aes(x = x, 
        y = y, 
        fill = Land_Cover_2022_100m)
  ) + 
  scale_fill_manual(values = land_cover_colors) +
  geom_sf(
    data = physiography_nepal,
    color = "black",
    fill = "transparent",
    linewidth = 0.3
  )+
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid = element_blank(),
        axis.title  = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, 
                                   color = "black",
                                   face = "bold",
                                   family = "Segoe UI"),
        legend.position = c(hjust = 0.8,
                            vjust = 0.75)
        )+
  guides(
    fill = guide_legend(
      ncol = 2, 
      bycol = T,
      keywidth = 1.5
    )
  )+
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01)))+
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01)))

ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/SVG/lc_map.svg",
  lc_plot,
  width = 8.6,
  height = 5
)  

ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/JPEG/lc_map.jpg",
  lc_plot,
  width = 8.6,
  height = 5,
  dpi = 600
)  


# FOREST COVER MAP -------------------------------------------------------------

library(tidyverse)

# THIS PART IS FOR EXPORTING THE DATASET ---
# VISUALIZATION IS IN THE LATTER PART OF THIS SUBHEADING ---


# Define regions and file paths
regions <- c("terai", "siwalik", "middle_mountains", "high_mountains", "high_himalaya")
base_path <- "D:/MISC/Deforestation_modeling_rasters/CSV_files"


# Load and process all datasets
data_list <- lapply(regions, function(region) {
  files <- c(
    file.path(base_path, "Individual_Regions", paste0("raster_df_", region, ".csv")),
    file.path(base_path, "Protected Areas Individual Regions", paste0("raster_df_", region, "_PAs.csv"))
  )
  
  bind_rows(lapply(files, read.csv)) %>%
    select(x, y, forest_loss, lc2000, lc2022) %>%
    mutate(
      lc_forest_2000 = ifelse(lc2000 == 4, 1, 0),
      lc_forest_2022 = ifelse(lc2022 == 4, 1, 0)
    ) %>% 
    filter(lc_forest_2000 == 1 | lc_forest_2022 == 1 | forest_loss == 1) %>%  
    mutate(
      forest_cover = case_when(
        lc_forest_2000 == 1 & lc_forest_2022 == 0 ~ "Loss",
        lc_forest_2000 == 0 & lc_forest_2022 == 1 ~ "Gain",
        forest_loss == 1 ~ "Loss",
        TRUE ~ "Unchanged"
      )
    )
})



# Assign each dataset to its respective region name
names(data_list) <- regions
list2env(data_list, envir = .GlobalEnv)



new <- rbind(terai, siwalik, middle_mountains, high_mountains, high_himalaya)

names(new)

new_new <- new[ , c("x", "y", "forest_loss", "lc_forest_2000",
                "lc_forest_2022", "forest_cover"  )]


write.csv(
  new_new,
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/forest_only.csv"
)


# VISUALIZATION ---#######################################---------------------
# dataset 
library(tidyverse)
extrafont::loadfonts(quiet = T)


forest_change <- read.csv(
  "D:/MISC/Deforestation_modeling_rasters/CSV_files/forest_only.csv"
)


physiography_nepal <- sf::st_read(
  "D:/MISC/points/Physiography/physiography_nepal_updated.shp"
)|>
  dplyr::select(Physio)

leftmost_points <- physiography_nepal %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POINT") %>%
  cbind(st_coordinates(.)) %>%
  st_drop_geometry()

# Get leftmost X and its corresponding Y for each region
label_coords <- leftmost_points %>%
  group_by(Physio) %>%
  slice_min(order_by = X, n = 1, with_ties = FALSE) %>%
  ungroup()

# Step 2: Extract values for each region
get_label_coord <- function(region) {
  label_coords[label_coords$Physio == region, c("X", "Y")]
}

# Get coordinates
coord_himalaya <- get_label_coord("High Himalaya")
coord_mountains <- get_label_coord("High Mountains")
coord_middle <- get_label_coord("Middle Mountains")
coord_siwalik <- get_label_coord("Siwalik")
coord_terai <- get_label_coord("Terai")

# visualization 
forest_cover <- ggplot() +
  geom_raster(
    data = forest_change,
    aes(x = x, y = y, fill = forest_cover)
  ) +
  geom_sf(
    data = physiography_nepal,
    color = "black",
    fill = "transparent",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "Loss" = "red",
      "Gain" = "blue",
      "Unchanged" = "green"
    ),
    name = "Forest Cover Change\n(2000 - 2022)"
  ) +
  geom_text(
    aes(x = coord_himalaya$X - 0.1,
        y = coord_himalaya$Y),
    label = "High Himalaya",
    family = "Segoe UI",
    size = 3.5,
    fontface = "bold",
    hjust = 1,
    color = "black"
  ) +
  geom_text(
    aes(x = coord_mountains$X - 0.1,
        y = coord_mountains$Y),
    label = "High Mountains",
    family = "Segoe UI",
    size = 3.5,
    fontface = "bold",
    hjust = 1,
    vjust = -1,
    color = "black"
  ) +
  geom_text(
    aes(x = coord_middle$X - 0.05,
        y = coord_middle$Y),
    label = "Middle Mountains",
    family = "Segoe UI",
    size = 3.5,
    fontface = "bold",
    hjust = 1,
    vjust = -0.5,
    color = "black"
  ) +
  geom_text(
    aes(x = coord_siwalik$X - 0.1,
        y = coord_siwalik$Y),
    label = "Siwalik",
    family = "Segoe UI",
    size = 3.5,
    fontface = "bold",
    hjust = 1,
    vjust = -0.2,
    color = "black"
  ) +
  geom_text(
    aes(x = coord_terai$X - 0.1,
        y = coord_terai$Y),
    label = "Terai",
    family = "Segoe UI",
    size = 3.5,
    fontface = "bold",
    hjust = 1,
    vjust = -0.2,
    color = "black"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "false",
    pad_x = unit(0, "cm"),
    pad_y = unit(0.3, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "black"),
      line_col = "white",
      text_family = "Britannic Bold",
      text_col = "Black",
      text_size = 11
    )
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    style = "ticks",
    line_col = "black",
    pad_x = unit(5, "cm"),
    pad_y = unit(3, "cm"),
    text_col = "black",
    text_family = "Segoe UI",
    text_cex = 0.7
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(color = "black", size = 10, family = "Segoe UI"),
    legend.text = element_text(color = "black", size = 10, family = "Segoe UI"),
    legend.title = element_text(color = "black", size = 10, face = "bold", family = "Segoe UI"),
    legend.position = c(hjust = 0.88, vjust = 0.6),
    axis.ticks.length.y = unit(-2, "mm"),
    axis.ticks.length.x = unit(-2, "mm"),
    axis.text.x = element_text(hjust = 0.5, vjust = 2, margin = margin(t = -14)),
    axis.text.y = element_text(hjust = 1, margin = margin(r = -30))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.09, 0.1)), breaks = c(27, 28, 29, 30)) +
  scale_x_continuous(expand = expansion(mult = c(0.25, 0.03)),
                     breaks = seq(80, 88, by = 2))


ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/SVG/forest_cover_map.svg",
  forest_cover,
  width = 8.6,
  height = 5
)  

ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/JPEG/forest_cover_map.jpg",
  forest_cover,
  width = 9.5,
  height = 5,
  dpi = 600
)  


rm(list = ls())
gc()


# DISTRICT RISK ----------------------------------
library(tidyverse)
extrafont::loadfonts(quiet =  T)



district_area <- read.csv(
  "D:/Drive/Deforestation Modeling/Working Documents/Area/area_covered_districts.csv",
)




high_very_high <- district_area |>
  filter(
    risk_classes %in% c("High", "Very High")
  ) |>
  select(
    districts, percentage_risk, total_area_sq_km, risk_classes
  ) |>
  group_by(districts) |>
  mutate(
    high_very = sum(percentage_risk)
  ) |>
  ungroup() |>  # IMPORTANT: ungroup before fct_reorder
  filter(high_very > 20) |>
  mutate(
    districts = fct_reorder(districts, high_very, .desc = TRUE)
  )



# Plot
district_bar <- ggplot() +
  geom_col(
    data = high_very_high,
    aes(y = districts,
        x = percentage_risk,
        fill = risk_classes),
    color = "white",
    linewidth = 0.5
  ) +
  theme_bw() +
  scale_fill_manual(
    values = c(
      "Very High" = "#A30000",
      "High" = "#FF3333"
    ),
    name = "Deforestation\nProbability",
    breaks = c("Very High", "High")
  ) +
  labs(
    x = "Risk Percentage",
    y = "Districts"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black",
                             size = 10,
                             family = "Segoe UI"),
    axis.title = element_text(color = "black",
                              size = 10,
                              face = "bold",
                              family = "Segoe UI"),
    legend.text = element_text(color = "black",
                               size = 10,
                               family = "Segoe UI"),
    legend.title = element_text(color = "black",
                                size = 10,
                                face = "bold",
                                family = "Segoe UI"),
    legend.position = c(hjust = 0.835,
                        vjust = 0.1),
    legend.background = element_rect(color = "black",
                                     fill = "transparent",
                                     linewidth = 0.3)
  )+ scale_y_discrete(limits = rev)


#district_bar


ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/SVG/district_percentage.svg",
  district_bar,
  width = 5,
  height = 6
)  


ggsave(
  "D:/Drive/Deforestation Modeling/Final_Images/JPEG/district_percentage.jpg",
  district_bar,
  width = 5,
  height = 6,
  dpi = 600
)  



