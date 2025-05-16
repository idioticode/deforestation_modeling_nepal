library(terra)
library(dplyr)

# Load rasters
lc_2000 <- rast("D:/Drive/LULC_Protected_Area/Raster Data/nlcms_2000/data/lc2000.tif")
lc_2022 <- rast("D:/Drive/LULC_Protected_Area/Raster Data/nlcms_2022/data/lc2022.tif")

# Create binary rasters (1 for forest, 0 for non-forest)
forest_2000 <- lc_2000 == 4
forest_2022 <- lc_2022 == 4





# Create a raster for change detection
forest_change <- classify(c(forest_2000, forest_2022), 
                          matrix(c(
                            0,0, 0, # no forest both times
                            0,1, 3, # gain (0 to 1)
                            1,0, 2, # loss (1 to 0)
                            1,1, 1  # no change (1 to 1)
                          ), ncol=3, byrow=TRUE))

# Count pixel values
change_table <- freq(forest_change)

print(change_table)








code_raster <- forest_2000 * 10 + forest_2022

# Now classify based on this code
rcl <- matrix(c(
  0, 0,       # 0 = no forest
  1, 3,       # 1 = gain
  10, 2,      # 10 = loss
  11, 1       # 11 = no change
), ncol=2, byrow=TRUE)

forest_change <- classify(code_raster, rcl)

# Now forest_change raster has:
# 1 = no change
# 2 = loss
# 3 = gain

# Check summary
counts <- freq(forest_change) |> as.data.frame()

counts <- counts |>
  mutate(
    change = case_when(
      value == 0 ~ "No Forest",
      value == 1 ~ "No change",
      value == 2 ~ "Loss",
      value == 3 ~ "Gain"
    )
  )|> 
  mutate(
    area_sq_km = count * 0.0009
  )


counts |>
  summarise(
    dat = sum(area_sq_km)
  )


write.csv(
  counts,
  "D:/Drive/Deforestation Modeling/Working Documents/Area/forest_cover_change.csv",
  row.names = F
)


