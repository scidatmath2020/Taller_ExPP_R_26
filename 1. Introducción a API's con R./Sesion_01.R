#=====================================================#
# Taller: Introducción a las API's                    #
# Tema: Principios de la consulta HTTPS sin encriptado#
# Asesor: Alexis Adonai Morales Alberto               #
# Fecha: 28/03/2026                                   #
# Código de R                                         #
#=====================================================#

# Remover memoria del programa (interfaz) ----

rm(list = ls())

# Verificación de la paquetería de pacman -----

if(require("pacman", quietly = T)){
  cat("La paquetería pacman se encuentrada instalada")
} else {
  install.packages("pacman", dependencies = T)
}

# Carga de paquetes a utilizar ----

pacman::p_load(
  "tidyverse",
  "httr",
  "curl",
  "jsonlite",
  "data.table"
)

# Principio: Accesos a la API WEB (simulación de resupuesta) ----

respuesta <- GET(
  "https://api.github.com/search/repositories?q=d3&sort=forks"
)


## Revisar estatus de conexión o parámetros -----

parametros <- list(q = "d3", sort = "forks")

respuesta_info <- GET(
  "https://api.github.com/search/repositories?q=d3&sort=forks",
  query = parametros
)

respuesta_info

# Conversión de la consulta en text para transformación ----

cuerpo <- content(respuesta, "text")

# Conversión a JSON ----

DatosJson <- fromJSON(cuerpo)

## Evaluar si la consulta nos genera un dataframe ----

is.data.frame(DatosJson)
is.data.frame(DatosJson[["items"]])

#====================================================#
# Ejemplo con BCRPData                           ----
#====================================================#

## URL de BCRPData -----

respuesta_BCRP <- GET(
  "https://estadisticas.bcrp.gob.pe/estadisticas/series/api/PN01288PM/json/2013-1/2016-9"
)

respuesta_BCRP

## Conversión de la consulta en text para transformación ----

cuerpo_BCRP <- content(respuesta_BCRP, "text")

## Conversión a JSON ----

DatosJson_BCRP <- fromJSON(cuerpo_BCRP)

### Evaluar si la consulta nos genera un dataframe ----

is.data.frame(DatosJson_BCRP)
is.data.frame(DatosJson_BCRP[["periods"]])

### Sustracción de la serie ----

DatosJson_BCRP[["periods"]] %>% 
  mutate(values = as.numeric(values)) %>% 
  view()





