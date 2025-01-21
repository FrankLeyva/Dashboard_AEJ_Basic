source('R/utils/extraction/SPSS_Data_Extraction_Functions.R')
source('R/utils/extraction/CSV_Data_Extraction_Functions.R')
spss_data2023 <- read_sav("data/raw/Encuesta Percepción Ciudadana AEJ 2023.sav")
spss_data2021 <- read_sav("data/RAW/PERCEPCION 2011-2023/Base de datos AEJ Percepción 2021 DISTRITOS.sav")
spss_data2020 <- read_sav("data/RAW/PERCEPCION 2011-2023/Base de datos AEJ Percepción 2020 DISTRITOS.sav")
spss_data2022 <- read_sav("data/RAW/PERCEPCION 2011-2023/_Encuesta de Percepción Ciudadana 2022.sav")
spss_data2019 <- read_sav("data/RAW/PERCEPCION 2011-2023/Base de datos AEJ Percepción 2019 DISTRITOS.sav")

create_manual <- function(spss_data){
metadata <- extract_spss_metadata(spss_data)
metadata$label <- gsub("CQ[0-9.]+\\s*", "", metadata$label)
metadata$label <- gsub("C Q[0-9.]+\\s*", "", metadata$label)
metadata$label <- gsub("Q[0-9.]+\\s*", "", metadata$label)
metadata <- fix_latin_encoding(metadata, 'label')
metadata$label<-make_clean_names(metadata$label)
return(metadata)
}
manual2019 <- create_manual(spss_data2019)
manual2020 <- create_manual(spss_data2020)
manual2021 <- create_manual(spss_data2021)
manual2022 <- create_manual(spss_data2022)
manual2023 <- create_manual(spss_data2023)
vivienda2023 <- spss_data2023 %>% 
  select(Q2,Q25,Q26,Q27,Q28,Q101,Q102)
vivienda2023$año <- 2023
vivienda2022 <- spss_data2022 %>% 
  select(Q2,Q49,Q50,Q51,Q52,Q92.6,Q92.9)
vivienda2022$año <- 2022
vivienda2021 <- spss_data2021 %>% 
  select(Q2,Q53,Q54,Q55,Q56,Q123,Q124)
vivienda2021$año <- 2021
vivienda2020 <- spss_data2020 %>% 
  select(Distrito,SatisfechoCasa,CalidadCasa,TamañoCasa,UbicacionCasa,Sexo,Edad)
vivienda2020$año <- 2020
vivienda2019 <- spss_data2019 %>% 
  select(DistritoElec,SatisfechoCasa,CalidadCasa,TamañoCasa,Sexo,Edad) %>% 
    add_column(UbicacionCasa = NA, .after = 4)
vivienda2019$año <- 2019
names(vivienda2023) <- c("Distrito", "Satisfaccion","Calidad","Tamaño","Ubicacion","Sexo","Edad",'Año')
names(vivienda2022) <- c("Distrito", "Satisfaccion","Calidad","Tamaño","Ubicacion","Sexo","Edad",'Año')
names(vivienda2021) <- c("Distrito", "Satisfaccion","Calidad","Tamaño","Ubicacion","Sexo","Edad",'Año')
names(vivienda2020) <- c("Distrito", "Satisfaccion","Calidad","Tamaño","Ubicacion","Sexo","Edad",'Año')
names(vivienda2019) <- c("Distrito", "Satisfaccion","Calidad","Tamaño","Ubicacion","Sexo","Edad",'Año')
vivienda <- rbind(vivienda2023,vivienda2022,vivienda2021,vivienda2020,vivienda2019)
write.csv(vivienda, 'data/processed/AEJ_2019-2023_Vivienda.csv',row.names=F)
