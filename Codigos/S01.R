#//////////////////////////////////////////////////////# 
# Taller: Excel y Power Point con R                    #
# Tema 1: Instalación del paquete openxlsx             #
# Sesión: 01                                           #
# Instructor: Alexis Adonai Morales Alberto            #
# Fecha: 31/08/26                                      #
# SciData                                              #
#//////////////////////////////////////////////////////# 

# ¿Cómo voy a insatalar openxlsx? -----

## Instalación clásica -----

install.packages(
  "openxlsx",
  dependencies = T
)

library(openxlsx)

## Instalación y llamado con pacman ----

pacman::p_load(
  "openxlsx"
)

# Primeros pasos en openxlsx -----

## Carga de datos a exportar (previo a un procesamiento) ------

Titanic <- read.csv(
  "https://raw.githubusercontent.com/datasciencedojo/datasets/refs/heads/master/titanic.csv"
)

## Exportación sin formato -----

write.xlsx(Titanic, "Titanic.xlsx")


## Exportación usando un formato básico -----

### 1) Crear el libro de trabajo (workbook) -----

wb <- createWorkbook()

### 2) Crear la hoja donde se almacenará los datos dentro del WB ----

addWorksheet(wb, "Titanic")

### 3) Crear estilos básicos -----

#### 3.1) Encabezado ----

Encabezado <- createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#4472C4",
  halign = "center",
  valign = "center",
  textDecoration = "bold",
  border = "TopBottomLeftRight",
  wrapText = TRUE
)

#### 3.2) Bordes -----

Borde <- createStyle(
  border = "TopBottomLeftRight",
  borderColour = "#000000"
)

### 4) Escritura de los datos -----

#### 4.1 ) Añadir datos en la hoja titanic y especificar estilo de encabezado -----

writeData(wb, "Titanic", Titanic, startRow = 1, headerStyle = Encabezado)

#### 4.2) Columnas, filas y estilos de borde -----

n_filas <- nrow(Titanic)
n_cols <- ncol(Titanic)

addStyle(wb, "Titanic",
         style = Borde,
         rows = 2:(n_filas+1), cols = 1:n_cols, gridExpand = TRUE,
         stack = TRUE)

### 5) Guardar libro -----

saveWorkbook(wb, "Titanic_formatoS.xlsx", overwrite = TRUE)


# Tarea: Reeplicar lo visto en clase pero usando los datos de IRIS 

iris
