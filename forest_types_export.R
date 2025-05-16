library(sf)
library(tidyverse)

population <- sf::st_read(
  "D:/MISC/points/Population Local level/TotalPopulation_GIS.shp"
)

physio_data <- st_read("https://raw.githubusercontent.com/idioticode/physiographic_zones_of_nepal/main/geojson_files/physiography_nepal_updated.geojson")

physio_data <- st_transform(physio_data, crs = st_crs(population))



library(dplyr)
library(sf)

regions <- unique(physio_data$Physio)

# Create a list of filtered physio shapes
physio_list <- setNames(
  lapply(regions, function(r) filter(physio_data, Physio == r)),
  paste0("physio_", tolower(gsub(" ", "_", regions)))
)

# Create a list of intersected population data
popn_list <- setNames(
  lapply(regions, function(r) st_intersection(population, physio_data[physio_data$Physio == r, ])),
  paste0("popn_", tolower(gsub(" ", "_", regions)))
)

# Combine all population intersections into one
popn_binded <- do.call(
  rbind, popn_list
)|>
  dplyr::select(
    GaPa_NaPa, Physio, Total_2021
  ) |> as.data.frame()

class(popn_binded)



summary_data <- popn_binded|>
  group_by(Physio)|>
  summarise(
    total = sum(Total_2021)
  )|>
  mutate(
    percent = total/sum(total)*100
  )





###################

physio_data <- st_read(
  "https://raw.githubusercontent.com/idioticode/physiographic_zones_of_nepal/main/geojson_files/physiography_nepal_updated.geojson"
)

ecoregion_nepal <- st_read(
  "D:/MISC/points/Ecoregion/wwf_ecoregions_nepal.shp"
)

plot(ecoregion_nepal[, "ECO_NAME"])

names(ecoregion_nepal)



# High Himalaya
eco_high_himalaya <- st_intersection(
  ecoregion_nepal,
  physio_data[physio_data$Physio == "High Himalaya", ]
)

# High Mountains
eco_high_mountains <-  st_intersection(
  ecoregion_nepal,
  physio_data[physio_data$Physio == "High Mountains", ]
)

# Middle Mountains
eco_middle_mountains <-  st_intersection(
  ecoregion_nepal,
  physio_data[physio_data$Physio == "Middle Mountains", ]
)

# Siwalik
eco_siwalik <- st_intersection(
  ecoregion_nepal,
  physio_data[physio_data$Physio == "Siwalik", ]
)

# Terai
eco_terai <- st_intersection(
  ecoregion_nepal,
  physio_data[physio_data$Physio == "Terai", ]
)



unique(eco_high_himalaya$ECO_NAME)
unique(eco_high_mountains$ECO_NAME)
unique(eco_middle_mountains$ECO_NAME)
unique(eco_siwalik$ECO_NAME)
unique(eco_terai$ECO_NAME)






