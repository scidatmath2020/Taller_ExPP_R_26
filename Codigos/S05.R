#//////////////////////////////////////////////////////# 
# Taller: Excel y Power Point con R                    #
# Tema 3: Configuración de patron de diapositivas con  #
#         python y R                                   #
# Sesión: 05                                           #
# Instructor: Alexis Adonai Morales Alberto            #
# Fecha: 04/09/26                                      #
# SciData                                              #
#//////////////////////////////////////////////////////#

# Instalación de paquetes forzadas -----

## Contar con la librería pak -----

# if(require("pak", quietly = T)){
#   cat("pak se encuentra instalado en R")
# } else{
#   install.packages("pak", dependencies = T)
# }

## Usar comando pkg_install de pak ----

# pak::pkg_install(
#   c(
#     "officer",
#     "flextable",
#     "scales"
#   )
# )

# Llamado o carga de paquetería o paqueterías -----

pacman::p_load(
  "tidyverse",
  "officer",
  "flextable",
  "scales"
)

# Crear pptx blanca o vacia -----

doc <- read_pptx()

# Exportar con nombre base_vacia.pptx ----

print(doc, target = "base_vacia.pptx")

# 1) Datos ficticios ------

trimestres <- c("Q1", "Q2", "Q3", "Q4")
ventas_trimestre <- c(120, 145, 138, 168)
regiones <- c("Norte", "Centro", "Sur", "Occiente")
detalle <- data.frame(
  Region = regiones,
  Q1 = c(38, 34, 30, 18),
  Q2 = c(44, 40, 36, 25),
  Q3 = c(46, 42, 34, 16),
  Q4 = c(52, 44, 40, 32)
)
detalle$Total <- rowSums(detalle[,-1])
productos <- c("Software", "Hardware", "Servicios")
ventas_producto <- c(250, 190, 131)

kpis <- list(
  total = sum(ventas_trimestre),
  crecimiento = "+18%",
  ticket_prom = "$2,850",
  clientes = 342
)

# 2) Definción de paleta de colores ----

navy <- "#1E2761"
ice <- "#CADCFC"
white <- "#FFFFFF"
gray_tx <- "#3B3B3B"

# 3) Gráficos para presentación ----

theme_reporte <- theme_minimal(
  base_size = 15
)+
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", color = navy, size = 17),
    axis.text = element_text(color = gray_tx)
  )

## Gráfico 1: Tendencia trimestral -----

g1 <- ggplot(
  data.frame(trimestres = factor(trimestres, levels = trimestres),
             ventas = ventas_trimestre),
  aes(x = trimestres, y = ventas)
)+
  geom_col(fill = navy, width = 0.55)+
  geom_text(aes(label = paste0("$", ventas,  "K")), vjust = -0.6,
            color = navy, fontface = "bold", size = 5)+
  scale_y_continuous(limits = c(0, max(ventas_trimestre)*1.2))+
  labs(title = "Ventas trimestrales 2026 (miles USD)")+
  theme_reporte

ggsave("img/chart_trimestre.png", g1, width = 8.5,
       height = 4.3, dpi = 300, bg = "white")

## Grafico 2: ventas por region (barras horizontales) ----

g2 <- ggplot(detalle, aes(x = reorder(Region, Total), y = Total)) +
  geom_col(fill = navy, width = 0.6) +
  geom_text(aes(label = paste0("$", Total, "K")), hjust = -0.15,
            color = navy, fontface = "bold", size = 4.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(detalle$Total) * 1.25)) +
  labs(title = "Ventas por region (miles USD)") +
  theme_reporte
ggsave("img/chart_region.png", g2, width = 8.5, height = 4.3, dpi = 300, bg = "white")

## Grafico 3: distribucion por linea de producto (dona) ----

df_prod <- data.frame(producto = productos, ventas = ventas_producto)
df_prod$pct <- percent(df_prod$ventas / sum(df_prod$ventas), accuracy = 1)
paleta_prod <- setNames(c(navy, "#5B72C1", ice), productos)
color_txt_prod <- setNames(c("white", "white", navy), productos)  # texto oscuro sobre el segmento claro (ice)
df_prod$producto <- factor(df_prod$producto, levels = productos)  # fija el orden Software/Hardware/Servicios

g3 <- ggplot(df_prod, aes(x = 2, y = ventas, fill = producto)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  geom_text(aes(label = paste0(producto, "\n", pct), color = producto),
            position = position_stack(vjust = 0.5),
            fontface = "bold", size = 4.2, show.legend = FALSE) +
  scale_color_manual(values = color_txt_prod) +
  scale_fill_manual(values = paleta_prod) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", color = navy, size = 17, hjust = 0.5)) +
  labs(title = "Distribucion de ventas por linea de producto")
ggsave("img/chart_producto.png", g3, width = 8.5, height = 4.3, dpi = 300, bg = "white")


# Crear pptx blanca o vacia -----

doc <- read_pptx("plantilla_corporativa.pptx")

## 4.3) Slide 1: Portada (layout "Title Slide") ----

doc <- add_slide(
  doc, layout = "Title Slide", master = "Office Theme"
)

doc <- ph_with(
  doc, value = "Reporte de Ventas Anual 2026",
  location = ph_location_type(type = "ctrTitle")
)

doc <- ph_with(
  doc,
  value = fpar(ftext("Análsis de desempeño comercial | Datos simulados con fines pedagógicos",
                     )),
  location = ph_location_type(type = "subTitle")
)

## 4.4) Slide 2: Separador de sección -----

doc <- add_slide(doc, layout = "Section Header", master = "Office Theme")
doc <- ph_with(doc, value = "Resumen Ejecutivo", 
               location = ph_location_type(type = "title"))
doc <- ph_with(doc, value = "Principales indicadores del año fiscal 2026",
               location = ph_location_type(type = "body"))

## 4.5) Slide 3: Kpi's -----

doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, value = "Indicadores clave (KPI's)", 
               location = ph_location_type(type = "title"))
kpi_labels <- c("Ventas totales 2026", "Crecimiento YoY",
                "Ticket promedio", "Clientes nuevos")
kpi_values <- c(paste0("$", kpis$total, "K"), 
                kpis$crecimiento, kpis$ticket_prom, kpis$clientes)
kpi_x <- c(1.5, 6.5, 1.5, 6.5)
kpi_y <- c(2.0, 2.0, 4.3, 4.3)

for (i in seq_along(kpi_labels)) {
  doc <- ph_with(doc,
                 value = fpar(ftext(kpi_values[i], 
                                    fp_text(color = navy,
                                            bold = TRUE,
                                            font.size = 40))),
                 location = ph_location(left = kpi_x[i],
                                        top = kpi_y[i], width = 4.3,
                                        height = 0.9))
  doc <- ph_with(doc,
                 value = fpar(ftext(kpi_labels[i], 
                                    fp_text(color = gray_tx, font.size = 15))),
                 location = ph_location(left = kpi_x[i], 
                                        top = kpi_y[i] + 0.85,
                                        width = 4.3, height = 0.4))
}


## 4.6 ) Slide 4: Tendencia trimestral (gráfico como imagen) ----

doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, value = "Tendencia de ventas trimestrales", 
               location = ph_location_type(type = "title"))
doc <- ph_with(doc, external_img("img/chart_trimestre.png",
                                 width = 8.5, height = 4.3),
               location = ph_location(left = 0.7, top = 1.9,
                                      width = 8.5, height = 4.3))

## 4.7 ) Slide 5: Venntas por región (gráfico como imagen) ----

doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, value = "Ventas por región", 
               location = ph_location_type(type = "title"))
doc <- ph_with(doc, external_img("img/chart_region.png",
                                 width = 8.5, height = 4.3),
               location = ph_location(left = 0.7, top = 1.9,
                                      width = 8.5, height = 4.3))

# plot_layout_properties(
#   doc,
#   layout = "Two Content"
# )
# 
# layout_summary(doc)

## 4.8 ) Slide 6: Distribución por producto (gráfico como imagen) ----

doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, value = "Distribución por línea de producto", 
               location = ph_location_type(type = "title"))
doc <- ph_with(doc, external_img("img/chart_producto.png",
                                 width = 6.6, height = 3.2),
               location = ph_location(top = 2.0,
                                      left = 0.1,
                                      width = 6.5, height = 3.2))

ft_region <- flextable(
  detalle[,c("Region", "Total")]
) |> 
  set_header_labels(
    Region = "Región",
    Total = "Total (miles USD)"
  ) |> 
  bg(part = "header", bg = navy) |> 
  color(part = "header", color = "white") |> 
  bold(part = "header") |> 
  bg(i = seq(1,4,2), bg = ice, part = "body") |> 
  autofit()

doc <- ph_with(doc,
               ft_region,
               location = ph_location(top = 2.5,
                                      left = 6,
                                      width = 4.3, height = 2.4)
)


## 4.9) Slide 7: Tabla detallada ----

doc <- add_slide(doc, layout = "Title Only", master = "Office Theme")
doc <- ph_with(
  doc,
  value = "Detalle de ventas por región y trimestre",
  location = ph_location_type(type = "title")
)

ft_detalle <- flextable(detalle) |> 
  set_header_labels(
    Total = "Total (miles UDS)",
    Region = "Región"
  ) |> 
  bg(part = "header", bg = navy) |> 
  color(part = "header", color = "white") |> 
  bold(part = "header") |> 
  bold(j = "Total") |> 
  bg(i = seq(1,4,2), bg = ice, part = "body") |> 
  align(align = "center", part = "all") |> 
  align(j = "Region", align = "left", part = "all") |> 
  fontsize(size = 20, part = "all") |> 
  padding(padding.top = 12, padding.bottom = 12, part = "all") |> 
  width(j = "Region", width = 1.5) |> 
  width(j = c("Q1", "Q2", "Q3", "Q4"), width = 1.1) |> 
  width(j = "Total", width = 3)

doc <- ph_with(doc,
               ft_detalle,
               location = ph_location(left = 0.7, 
                                      top = 2,
                                      width = 8.6,
                                      height = 3.2))

## 4.10) Slide 8: Conclusiones ------

doc <- add_slide(doc,
                 layout = "Title and Content",
                 master = "Office Theme")

doc <- ph_with(doc,
               value = "Hallazgos y recomendaciones",
               location = ph_location_type(type = "title"))

bullets1 <- fpar(
  ftext("Crecimiento sostenido: ", fp_text(bold = TRUE, color = navy,
                                           font.size = 16)),
  ftext("las ventas subieron cada trimestre salvo un ligero ajuste en Q3",
        txt_gray()),
  fp_p = fp_par(padding.bottom = 12)
)

bullets2 <- fpar(
  ftext("Norte lidera la región: ", fp_text(bold = TRUE, color = navy,
                                            font.size = 16)),
  ftext("concentra el 32% de las ventas totales; Occiente tiene el mayor margen de mejora.",
        txt_gray()),
  fp_p = fp_par(padding.bottom = 12)
)


bullets3 <- fpar(
  ftext("Software es el producto estrella: ", fp_text(bold = TRUE, color = navy,
                                                      font.size = 16)),
  ftext("representa el 44% del total; se recomienda reforzar su equipo comercial",
        txt_gray()),
  fp_p = fp_par(padding.bottom = 12)
)


doc <- ph_with(
  doc,
  value = block_list(bullets1, bullets2, bullets3),
  location = ph_location_type(type = "body")
)

## 4.11) Cierre de la presentación -----

doc <- add_slide(doc,
                 layout = "Section Header",
                 master = "Office Theme")

doc <- ph_with(doc, value = "Gracias", location = ph_location_type(type = "title"))
doc <- ph_with(doc, value = "Preguntas y discusión",
               location = ph_location_type(type = "body"))

# 5) Exportar pptx -----

print(doc, target = "Reporte_Ventas_2026_xml_modificado.pptx")

