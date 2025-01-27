library(sf)
library(leaflet)
library(shiny)
library(dplyr)

# Read and process data
districts_sf <- jsonlite::fromJSON("data/Cartografía/Jrz_Map.geojson",simplifyVector = F)
districts_sf <- st_read("data/Cartografía/SHAPE_NUEVA_DISTRITACION.shp")
districts_sf <- st_zm(districts_sf)
aej_data <- read.csv("data/processed/AEJ_2019_2023_Vivienda.csv")
district_metrics <- aej_data %>%
  group_by(Distrito,Año) %>%
  summarise(
    avg_satisfaction = mean(Satisfaccion, na.rm = TRUE),
    avg_quality = mean(Calidad, na.rm = TRUE),
    avg_size = mean(Tamaño,na.rm = T),
    avg_location = mean(Ubicacion,na.rm=T)  )
  
district_metrics<- district_metrics[-10,]
district_metrics2023<-district_metrics[district_metrics$Año==2023,]

district_metrics <- districts_sf %>%
  left_join(district_metrics, by = c("No_Distrit" = "Distrito"))
saveRDS(district_metrics,'data/processed/Map_Survey.rds')
write.csv(district_metrics,'data/processed/Map_Survey.csv',row.names=F)


