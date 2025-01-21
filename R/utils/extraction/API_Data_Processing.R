library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)


# SNIVV
# URL: https://sniiv.sedatu.gob.mx/api/CuboAPI/GetFinanciamiento/{anios}/{clave_estado}/{clave_municipio}/{dimensiones}
base_url <- 'https://sniiv.sedatu.gob.mx/api/CuboAPI/GetFinanciamiento/'
# Rango de años deseado a cuatro dígitos, separados por una coma, sin espacios (año inicio,año fin)
años <- '2023/'

clave_estado <- '08/'

clave_municipio <- '037/'

dimensiones <- 'anio,valor_vivienda'
request <- paste0(base_url,años,clave_estado,clave_municipio,dimensiones)
req <- GET(request)
SNIIV_tipo_vivienda <- fromJSON(rawToChar(req$content), flatten = TRUE)


dimensiones <- 'anio,modalidad'
request <- paste0(base_url,años,clave_estado,clave_municipio,dimensiones)
req <- GET(request)
SNIIV_modalidad <- fromJSON(rawToChar(req$content), flatten = TRUE)


dimensiones <- 'anio,organismo'
request <- paste0(base_url,años,clave_estado,clave_municipio,dimensiones)
req <- GET(request)
SNIIV_organismo <- fromJSON(rawToChar(req$content), flatten = TRUE)

write.csv(SNIIV_tipo_vivienda,'data/processed/SNIIV_Vivienda.csv', row.names=F)
write.csv(SNIIV_modalidad,'data/processed/SNIIV_modalidad.csv', row.names=F)

write.csv(SNIIV_organismo,'data/processed/SNIIV_organismo.csv', row.names=F)

# INEGI
# Example URL: https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/1003000011/es/070000080037/true/BISE/2.0/a6c929db-138d-b76e-c413-9b16352c5e81?type=json

# 1003000001 Total de vivienda habitada
# 1003000015 Promedio de ocupantes por vivienda

#base_url <- "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/"
#idioma <- 'es'
#region <- 070000080037
#API_key <- 'a6c929db-138d-b76e-c413-9b16352c5e81'
request <- 'https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/1003000001/es/070000080037/false/BISE/2.0/a6c929db-138d-b76e-c413-9b16352c5e81?type=json'
req <- GET(request)
data <- fromJSON(rawToChar(req$content), flatten = TRUE)
Viv_part_habitada<- data$Series$OBSERVATIONS[[1]][,1:2]
names(Viv_part_habitada) <- c('Año', 'viviendas_habitadas')

request <- 'https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/1003000015/es/070000080037/false/BISE/2.0/a6c929db-138d-b76e-c413-9b16352c5e81?type=json'
req <- GET(request)
data <- fromJSON(rawToChar(req$content), flatten = TRUE)
Ocup_vivienda<- data$Series$OBSERVATIONS[[1]][,1:2]
names(Ocup_vivienda) <- c('Año', 'Promedio_Ocupantes_Por_Vivienda')

viviendas_deshabitadas <- tibble(
    Año = c('2005', '2010', '2020'),
  viviendas_deshabitadas = c(70434, 111103, 56379)
)
write.csv(Viv_part_habitada, 'data/processed/INEGI_Viv_part_habitada.csv', row.names = F)
write.csv(Ocup_vivienda, 'data/processed/INEGI_Ocup_vivienda.csv', row.names  = F)
write.csv(viviendas_deshabitadas, 'data/processed/INEGI_viviendas_deshabitadas.csv', row.names = F)

viviendas_completo <- Ocup_vivienda %>%
  # Join with habitadas_df
  left_join(Viv_part_habitada, by = "Año") %>%
  # Join with deshabitadas_df
  left_join(viviendas_deshabitadas, by = "Año") 
write.csv(viviendas_completo,'data/processed/viviendas_completo.csv',row.names  = F)
