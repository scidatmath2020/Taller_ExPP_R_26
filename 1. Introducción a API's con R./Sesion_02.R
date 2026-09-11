#=====================================================#
# Taller: Introducción a las API's                    #
# Tema: Principios de la consulta HTTPS con encriptado#
# Subtitulo: Ejemplo con BIE INEGI y SIE BANXICO      #
# Asesor: Alexis Adonai Morales Alberto               #
# Fecha: 29/03/2026                                   #
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
  "data.table",
  "lubridate",
  "zoo"
)

# Construcción del HTTPS para BIE INEGI (1 serie) ----

## 1. Definir el token ----

token_id <- "af847734-746b-8eb8-f0e6-4070cc851e47"

## 2. Sustraer URL de la consulta del indicador seleccionado y sustituir el token-----

# https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/742211/es/00/false/BIE-BISE/2.0/[Aquí va tu Token]?type=json

URL_BIE <- paste0(
  "https://www.inegi.org.mx/app/api/indicadores/",
  "desarrolladores/jsonxml/INDICATOR/742211/",
  "es/00/false/BIE-BISE/2.0/",
  token_id,
  "?type=json"
) 

## 3. Enviar la respuesta -----

respuesta <- GET(
  URL_BIE
)

respuesta

## 4. Conversión a texto de la respuesta -----

cuerpo <- content(respuesta, "text")

## 5. Realizar lectura del JSON generado de la consula ------

DatosJson <- fromJSON(cuerpo)

## 6. Visualizar información buscandola en el elemento -----

Series <- DatosJson[["Series"]]$OBSERVATIONS[[1]]

## 7. Limpieza de datos -----

### 7.1 Selección de columnas de interés -----

Series <- Series %>% 
  select(TIME_PERIOD, OBS_VALUE) %>% 
  set_names(nm = c("Fecha", "Serie"))

### 7.2 Darle formato correcto según la información ----

Series <- Series %>% 
  mutate(
    Año = substr(Fecha, 1,4),
    Mes = substr(Fecha, 6,7),
    Mes = case_when(
      Mes == "01" ~ "01",
      Mes == "02" ~ "04",
      Mes == "03" ~ "07",
      Mes == "04" ~ "10"
    ),
    Fecha = as.Date(
      paste0(Año, "-", Mes, "-01"),
      format = "%Y-%m-%d"
    ),
    Serie = as.numeric(Serie),
    Trimestre = as.yearqtr(Fecha)
  ) %>% 
  select(-Mes, -Año) %>% 
  arrange(Fecha)

Series <- Series %>% 
  select(all_of(c("Fecha", "Trimestre", "Serie")))

# Construcción del HTTPS para BIE INEGI (2 series) ----

## 1. Definir el token ----

token_id <- "af847734-746b-8eb8-f0e6-4070cc851e47"

## 2. Sustraer URL de la consulta del indicador seleccionado y sustituir el token-----

# https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/742211/es/00/false/BIE-BISE/2.0/[Aquí va tu Token]?type=json

URL_BIE <- paste0(
  "https://www.inegi.org.mx/app/api/indicadores/",
  "desarrolladores/jsonxml/INDICATOR/742211,735879/",
  "es/00/false/BIE-BISE/2.0/",
  token_id,
  "?type=json"
) 

## 3. Enviar la respuesta -----

respuesta <- GET(
  URL_BIE
)

respuesta

## 4. Conversión a texto de la respuesta -----

cuerpo <- content(respuesta, "text")

## 5. Realizar lectura del JSON generado de la consula ------

DatosJson <- fromJSON(cuerpo)

## 6. Visualizar información buscandola en el elemento -----

Series1 <- DatosJson[["Series"]]$OBSERVATIONS[[1]]
Series2 <- DatosJson[["Series"]]$OBSERVATIONS[[2]]


## 7. Limpieza de datos -----

### 7.1 Selección de columnas de interés -----

Series1 <- Series1 %>% 
  select(TIME_PERIOD, OBS_VALUE) %>% 
  set_names(nm = c("Fecha", "Serie"))


Series2 <- Series2 %>% 
  select(TIME_PERIOD, OBS_VALUE) %>% 
  set_names(nm = c("Fecha", "Serie"))

### 7.2 Darle formato correcto según la información ----

Series1 <- Series1 %>% 
  mutate(
    Año = substr(Fecha, 1,4),
    Mes = substr(Fecha, 6,7),
    Mes = case_when(
      Mes == "01" ~ "01",
      Mes == "02" ~ "04",
      Mes == "03" ~ "07",
      Mes == "04" ~ "10"
    ),
    Fecha = as.Date(
      paste0(Año, "-", Mes, "-01"),
      format = "%Y-%m-%d"
    ),
    Serie = as.numeric(Serie),
    Trimestre = as.yearqtr(Fecha)
  ) %>% 
  select(-Mes, -Año) %>% 
  arrange(Fecha)

Series1 <- Series1 %>% 
  select(all_of(c("Fecha", "Trimestre", "Serie")))


Series2 <- Series2 %>% 
  mutate(
    Año = substr(Fecha, 1,4),
    Mes = substr(Fecha, 6,7),
    Mes = case_when(
      Mes == "01" ~ "01",
      Mes == "02" ~ "04",
      Mes == "03" ~ "07",
      Mes == "04" ~ "10"
    ),
    Fecha = as.Date(
      paste0(Año, "-", Mes, "-01"),
      format = "%Y-%m-%d"
    ),
    Serie = as.numeric(Serie),
    Trimestre = as.yearqtr(Fecha)
  ) %>% 
  select(-Mes, -Año) %>% 
  arrange(Fecha)

Series2 <- Series2 %>% 
  select(all_of(c("Fecha", "Trimestre", "Serie")))


# SIE API BANXICO ----

## 1. Definir el token ----

token_id <- "4596344496d33ae26b869fa815fb6d92dd442e9c22e3145ac44445796734685a"

## 2. Sustraer URL de la consulta del indicador seleccionado y sustituir el token-----

# https://www.banxico.org.mx/SieAPIRest/service/v1/series/SF17906/datos?token=4596344496d33ae26b869fa815fb6d92dd442e9c22e3145ac44445796734685a


URL_SIE <- paste0(
  "https://www.banxico.org.mx/SieAPIRest/service/v1/series/SF17906/",
  "datos?token=",
  token_id
) 

## 3. Enviar la respuesta -----

respuesta <- GET(
  URL_SIE
)

respuesta

## 4. Conversión a texto de la respuesta -----

cuerpo <- content(respuesta, "text")

## 5. Realizar lectura del JSON generado de la consula ------

DatosJson <- fromJSON(cuerpo)

## 6. Visualizar información buscandola en el elemento -----

Series_SIE <- DatosJson[["bmx"]][["series"]][["datos"]][[1]]

## 7. Limpieza de datos -----

### 7.1 Selección de columnas de interés -----

Series_SIE <- Series_SIE %>% 
  select(fecha, dato) %>% 
  set_names(nm = c("Fecha", "Serie"))

### 7.2 Darle formato correcto según la información ----

Series_SIE <- Series_SIE %>% 
  mutate(
    Fecha = as.Date(
      Fecha,
      format = "%d/%m/%Y"
    ),
    Serie = as.numeric(Serie)
  ) %>% 
  arrange(Fecha)






