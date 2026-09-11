#======================================================# 
# Taller: Excel y Power Point con R                    #
# Tema 3: Configuración de patron de diapositivas con  #
#         python y R                                   #
# Sesión: 05                                           #
# Instructor: Alexis Adonai Morales Alberto            #
# Fecha: 04/09/26                                      #
# SciData                                              #
#======================================================# 

"""
Código de Python que permite procesar la plantilla de pptx descomprimida
mediante los archivos xml que se encuentran en la carpeta ppt de extracted_clean
"""

# Modulos

import re, os 

# Directorio de los xml de la presentación

BASE = "extracted_clean/ppt"

# Colores o paletas de colores 

NAVY = "1E2761"
NAVY2 = "141B47"
MIDBLUE = "5B72C1"
ICE = "CADCFC"
GOLD = "C9A24B"
GRAY = "5A5A5A"

# Tema: Paleta de colores + tipografía 

theme_path = f"{BASE}/theme/theme1.xml"
x = open(theme_path, encoding = "utf-8").read()

## Cambio en paleta de colores 

def set_scheme_color(xml, tag, hexval):
    return re.sub(
        rf"<a:{tag}>.*?</a:{tag}>",
        f"<a:{tag}><a:srgbClr val=\"{hexval}\"/></a:{tag}>",
        xml, count=1, flags=re.S
    )
    
x = set_scheme_color(x, "dk2", NAVY)
x = set_scheme_color(x, "lt2", ICE)
x = set_scheme_color(x, "accent1", NAVY)
x = set_scheme_color(x, "accent2", MIDBLUE)
x = set_scheme_color(x, "accent3", ICE)
x = set_scheme_color(x, "accent4", GOLD)
x = set_scheme_color(x, "accent5", GRAY)
x = set_scheme_color(x, "accent6", NAVY2)

## Cambio en tipografía 

master_path = f"{BASE}/slideMasters/slideMaster1.xml"
m = open(master_path, encoding = "utf-8").read()

### Estilo por defecto del titulo: navy, Georgia, negrita 

m = re.sub(
    r'(<p:titleStyle><a:lvl1pPr[^>]*>.*?<a:defRPr sz=")\d+(")([^>]*)>(<a:solidFill>).*?(</a:solidFill>)',
    r'\g<1>3600\g<2>\g<3> b="1">\g<4><a:schemeClr val="tx2"/>\g<5>',
    m, count=1, flags=re.S
)

### Barra de acento superior + linea dorada bajo el título 

accent_shapes = f'''
<p:sp>
  <p:nvSpPr><p:cNvPr id="900" name="Accent Bar Top"/><p:cNvSpPr/><p:nvPr/></p:nvSpPr>
  <p:spPr>
    <a:xfrm><a:off x="0" y="0"/><a:ext cx="12192000" cy="46000"/></a:xfrm>
    <a:prstGeom prst="rect"><a:avLst/></a:prstGeom>
    <a:solidFill><a:srgbClr val="{NAVY}"/></a:solidFill>
    <a:ln><a:noFill/></a:ln>
  </p:spPr>
  <p:txBody><a:bodyPr/><a:lstStyle/><a:p/></p:txBody>
</p:sp>
<p:sp>
  <p:nvSpPr><p:cNvPr id="901" name="Accent Bar Bottom"/><p:cNvSpPr/><p:nvPr/></p:nvSpPr>
  <p:spPr>
    <a:xfrm><a:off x="0" y="6804000"/><a:ext cx="12192000" cy="19000"/></a:xfrm>
    <a:prstGeom prst="rect"><a:avLst/></a:prstGeom>
    <a:solidFill><a:srgbClr val="{GOLD}"/></a:solidFill>
    <a:ln><a:noFill/></a:ln>
  </p:spPr>
  <p:txBody><a:bodyPr/><a:lstStyle/><a:p/></p:txBody>
</p:sp>
'''
m = m.replace("</p:spTree>", accent_shapes + "</p:spTree>")
 
open(master_path, "w", encoding="utf-8").write(m)

# Modificar layout "Title Slide" 

title_slide_path = f"{BASE}/slideLayouts/slideLayout1.xml"

t = open(title_slide_path, encoding = "utf-8").read()

bg_block = f'<p:bg><p:bgPr><a:solidFill><a:srgbClr val="{NAVY}"/></a:solidFill><a:effectLst/></p:bgPr></p:bg>'

t = re.sub(r'(<p:cSld[^>]*>)', r'\1' + bg_block, t, count=1)

## Definición para forzar texto blanco en el titulo y subtitulo 

def force_white_runs(xml):
  return xml.replace('<a:t>', '<a:t>')

def style_placeholder_text(xml, ph_type, hexval):
  marker =  f'type="{ph_type}"'
  i = xml.find(marker)
  if i == -1:
    return xlm
  sp_start = xml.rfind("<p:sp>", 0, i)
  sp_end = xml.find("</p:sp>", i) + len("</p:sp>")
  block = xml[sp_start:sp_end]
  
  if "<a:lstStyle/>" in block:
    new_block = block.replace(
            "<a:lstStyle/>",
            f'<a:lstStyle><a:lvl1pPr><a:defRPr><a:solidFill>'
            f'<a:srgbClr val="{hexval}"/></a:solidFill></a:defRPr></a:lvl1pPr></a:lstStyle>',
            1
        )
  elif "<a:solidFill>" in block:
    new_block = re.sub(
            r"<a:solidFill>.*?</a:solidFill>",
            f'<a:solidFill><a:srgbClr val="{hexval}"/></a:solidFill>',
            block, count=1, flags=re.S
        )
  elif re.search(r"<a:defRPr[^>]*/>", block):
    new_block = re.sub(
            r"<a:defRPr([^>]*)/>",
            rf'<a:defRPr\1><a:solidFill><a:srgbClr val="{hexval}"/></a:solidFill></a:defRPr>',
            block, count=1
        )
  else:
    new_bloc = block
    
  return xml[:sp_start] + new_block + xml[sp_end:]

t = style_placeholder_text(t, "ctrTitle", "FFFFFF")
t = style_placeholder_text(t, "subTitle", ICE)

open(title_slide_path, "w", encoding="utf-8").write(t)

# Modificar layout: Section Header - Navy + Texto blanco 

section_path = f"{BASE}/slideLayouts/slideLayout3.xml"
s = open(section_path, encoding="utf-8").read()
 
s = re.sub(r'(<p:cSld[^>]*>)', r'\1' + bg_block, s, count=1)
s = style_placeholder_text(s, "title", "FFFFFF")
s = style_placeholder_text(s, "body", ICE)
 
open(section_path, "w", encoding="utf-8").write(s)
