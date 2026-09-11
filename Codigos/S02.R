#//////////////////////////////////////////////////////# 
# Taller: Excel y Power Point con R                    #
# Tema 2: Formatos numéricos, uso de funciones y otros #
#         elementos                                    #
# Sesión: 02                                           #
# Instructor: Alexis Adonai Morales Alberto            #
# Fecha: 01/09/26                                      #
# SciData                                              #
#//////////////////////////////////////////////////////# 

# Llamado o carga de paquetería o paqueterías -----

pacman::p_load(
  "tidyverse",
  "openxlsx"
)

# Data frame ficticio de ventas -----

datos <- data.frame(
  Producto = c("Laptop X1", "Mouse óptico", "Teclado Meca",
               "Monitor 27''", "Impresora L", "Silla Ergo",
               "Cámara Web", "Audifonos BT"),
  Categoría = c("Computo", "Accesorios", "Accesorios",
                "Computo", "Oficina", "Oficina",
                "Accesorios", "Accesorios"),
  Unidades = c(120, 480, 210, 95, 60, 40, 300, 275),
  PrecioUnit = c(15800, 250, 890, 4200, 3100, 2650, 480, 690),
  MetaUnidad = c(150, 400, 200, 100, 80, 50, 250, 300),
  stringsAsFactors = FALSE
)

# 1) Crear libro y hoja -----

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Reporte_Ventas", gridLines = FALSE)
hoja <- "Reporte_Ventas"

# 2) Creación de elementos de dimensión de tabla -----

n <- nrow(datos)
fila_inicio_titulo <- 1
fila_encabezado <- 3
fila_datos_ini <- fila_encabezado+1
fila_datos_fin <- fila_datos_ini+n-1
fila_totales <- fila_datos_fin+1
fila_pie <- fila_totales+2

# 3) Título de la tabla (fuente, tamaño y combinación de celdas) -----

writeData(wb, hoja, "REPORTE DE VENTAS - CIERRE MES",
          startCol = 1, startRow = fila_inicio_titulo)
mergeCells(wb, hoja, cols = 1:8, rows = fila_inicio_titulo)

estilo_titulo <- createStyle(
  fontName = "Arial",
  fontSize = 18,
  fontColour = "#FFFFFF",
  fgFill = "#1F4E78",
  halign = "center",
  valign = "center",
  textDecoration = "bold"
)

addStyle(wb, hoja, estilo_titulo, rows = fila_inicio_titulo,
         cols = 1:8)

setRowHeights(wb, hoja, rows = fila_inicio_titulo, heights = 30)

# 4) Subtitulos de la tabla (fecha, fuente y tamaño menor)-----

writeData(wb, hoja, paste("Generado el", format(Sys.Date(), "%d/%m/%Y")),
          startCol = 1, startRow = 2)
mergeCells(wb, hoja, cols = 1:8, rows = 2)
estilo_subtitulo  <- createStyle(
  fontName = "Arial", fontSize = 10, fontColour = "#555555",
  halign = "center", textDecoration = "italic"
)
addStyle(wb, hoja, estilo_subtitulo, rows = 2, cols = 1:8)

# 5) Encabezado de tablas -----

encabezados <- c(
  "Producto",
  "Categoría",
  "Unidades",
  "Precio Unitario",
  "Meta Unidades",
  "Total Venta",
  "% del Total",
  "Cumplimiento Meta"
)

writeData(wb, hoja, t(encabezados), startCol = 1,
          startRow = fila_encabezado,
          colNames = FALSE)

estilo_encabezados <- createStyle(
  fontName = "Arial", fontSize = 11, fontColour = "#FFFFFF",
  fgFill = "#2E75B6", textDecoration = "bold", 
  halign = "center", valign = "center", border = "TopBottomLeftRight",
  wrapText = TRUE
)

addStyle(wb, hoja, estilo_encabezados, rows = fila_encabezado,
         cols = 1:8)
setRowHeights(wb, hoja, rows = fila_encabezado, heights = 26)

# 6) Escribir datos dentro del excel ------

writeData(
  wb, hoja, datos, startCol = 1, startRow = fila_datos_ini,
  colNames = FALSE
)

# 7) Cálculos internos de Excel (formulas reales, no valore fijos) ----

f_total <- paste0(
  "C", fila_datos_ini:fila_datos_fin, "*D",
  fila_datos_ini:fila_datos_fin
)

writeFormula(wb, hoja, x = f_total, startCol = 6, 
             startRow = fila_datos_ini)

f_ptc <- paste0(
  "F", fila_datos_ini:fila_datos_fin, "/SUM(F$",
  fila_datos_ini, ":F$", fila_datos_fin, ")"
)

writeFormula(wb, hoja, x = f_ptc, startCol = 7,
             startRow = fila_datos_ini)

f_cumpl <- paste0(
  "C", fila_datos_ini:fila_datos_fin, "/E",
  fila_datos_ini:fila_datos_fin
)

writeFormula(wb, hoja, x = f_cumpl, startCol = 8,
             startRow = fila_datos_ini)


# 8) Filtas de totales / pie de tabla con formulas SUM y PROMEDIO ----

writeData(wb, hoja, "TOTALES", startCol = 1,
          startRow = fila_totales)

writeFormula(
  wb, hoja,
  x = paste0(
    "SUM(C", fila_datos_ini, ":C", fila_datos_fin, ")"
  ),
  startCol = 3, startRow = fila_totales
)

writeFormula(
  wb, hoja,
  x = paste0(
    "SUM(F", fila_datos_ini, ":F", fila_datos_fin, ")"
  ),
  startCol = 6, startRow = fila_totales
)

writeFormula(
  wb, hoja,
  x = paste0(
    "SUM(G", fila_datos_ini, ":G", fila_datos_fin, ")"
  ),
  startCol = 7, startRow = fila_totales
)

writeFormula(
  wb, hoja,
  x = paste0(
    "AVERAGE(H", fila_datos_ini, ":H", fila_datos_fin, ")"
  ),
  startCol = 8, startRow = fila_totales
)


estilo_totales <- createStyle(
  fontName = "Arial", fontSize = 11, textDecoration = "bold",
  fgFill = "#D9E1F2", border = "TopBottomLeftRight"
)

addStyle(wb, hoja, estilo_totales, rows = fila_totales,
         cols = 1:8, stack = TRUE)


# 9) Formatos de número (miles, moneda y porcentaje) -----

estilo_miles <- createStyle(numFmt = "#,##0")
estilo_moneda <- createStyle(numFmt = "$#,##0.0")
estilo_ptc <- createStyle(numFmt = "0.0%")

addStyle(wb, hoja, estilo_miles, rows = fila_datos_ini:fila_totales,
         cols =c(3,5), gridExpand = TRUE, stack = TRUE)

addStyle(wb, hoja, estilo_moneda, rows = fila_datos_ini:fila_totales,
         cols =c(4,6), gridExpand = TRUE, stack = TRUE)

addStyle(wb, hoja, estilo_ptc, rows = fila_datos_ini:fila_totales,
         cols =c(7,8), gridExpand = TRUE, stack = TRUE)

# 10) Bordes -----

estilo_borde <- createStyle(
  border = "TopBottomLeftRight",
  borderColour = "#B7B7B7"
)

addStyle(wb, hoja, estilo_borde, rows = fila_datos_ini:fila_datos_fin,
         cols = 1:8, gridExpand = TRUE, stack = TRUE)


# 11) Formato condicional -----

conditionalFormatting(wb, hoja,
                      cols = 7, rows = fila_datos_ini:fila_datos_fin,
                      type = "colourScale",
                      style = c("#F8696B", "#FFEB84", "#63BE7B")
)

rojo    <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
amarillo<- createStyle(fontColour = "#9C6500", bgFill = "#FFEB9C")
verde   <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")

conditionalFormatting(wb, hoja, cols = 8, rows = fila_datos_ini:fila_datos_fin,
                      rule = "<0.9", style = rojo)
conditionalFormatting(wb, hoja, cols = 8, rows = fila_datos_ini:fila_datos_fin,
                      rule = ">=0.9", style = amarillo)
conditionalFormatting(wb, hoja, cols = 8, rows = fila_datos_ini:fila_datos_fin,
                      rule = ">=1", style = verde)



# 12) Pie de Tabla (Notas / Fuente) ------

writeData(
  wb,
  hoja,
  "Nota: Cumplimiento de Meta = Unidades vendidas / Meta de unidades. Fuente: sistema interno de ventas.",
  startCol = 1, startRow = fila_pie
)

mergeCells(wb, hoja, cols = 1:8, 
           rows = fila_pie)

estilo_pie <- createStyle(
  fontName = "Arial",
  fontSize = 9,
  fontColour = "#000000",
  textDecoration = "italic"
)

addStyle(wb, hoja, estilo_pie, rows = fila_pie, cols = 1:8)

# 13) Ajustes finales: Anchos de columna, panel inmovilizado, filtro ----

setColWidths(wb, hoja, cols = 1:8,
             widths = c(18, 14, 11, 15, 13, 14, 12, 16))

freezePane(wb, hoja, firstActiveRow = fila_datos_ini, firstActiveCol = 2)
addFilter(wb, hoja, row = fila_encabezado, cols = 1:8)


# Adiconal: Área de impresión -----

pageSetup(
  wb, hoja,
  orientation = "landscape",
  fitToWidth = TRUE,
  fitToHeight = FALSE,
  paperSize = 9,
  left = 0.5, right = 0.5, top = 0.75, bottom = 0.75,
  header = 0.3, footer = 0.3,
  printTitleRows = fila_encabezado
)

createNamedRegion(wb, sheet = hoja,
                  cols = 1:8, rows = fila_inicio_titulo:fila_pie,
                  name = "_xlnm.Print_Area",
                  overwrite = TRUE)

setHeaderFooter(wb, hoja,
                header = c(NA, "Reporte de Ventas", NA),
                footer = c("Confidencial",NA, "Pagina &[Page] de &[Pages]"))
# 14) Guardar el documento -----

salida <- "reporte_ventas_openxlsx_V1.xlsx"
saveWorkbook(wb, salida, overwrite = TRUE)
