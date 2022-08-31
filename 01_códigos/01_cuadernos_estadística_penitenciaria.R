#------------------------------------------------------------------------------#
# Proyecto:              AMICUS para la SCJN sobre prisión preventiva oficiosa  
# Objetivo:              Procesar 
#
# Encargadas:            Coordinación de datos de Intersecta
# 
# Fecha de creación:     06 de diciembre de 2021 (códigos originales)
# Última actualización:  31 de agosto de 2021 (código amicus)
#------------------------------------------------------------------------------#

# Fuente: https://www.inegi.org.mx/programas/enpol/2021/#Microdatos

# 0. Configuración inicial -----------------------------------------------------


# ---- Opciones generales 
options(dplyr.summarise.inform = FALSE) # Silenciar mensajes de .group en dplyr
options(scipen=999)                     # Silenciar notación científica

# ---- Librerías 
pacman::p_load(
    googledrive, googlesheets4, readr, readxl, tidyverse, janitor, ggrepel)

# ---- Limpiar espacio de trabajo 
rm(list=ls())

# ---- Funciones con direcciones de las carpetas
paste_code  <- function(x){paste0("01_códigos/"                                  , x)}
paste_inp   <- function(x){paste0("02_datos_crudos/01_cuadernos_estadística_penitenciaria/" , x)}
paste_out   <- function(x){paste0("03_datos_limpios/01_cuadernos_estadística_penitenciaria/", x)}
paste_fig   <- function(x){paste0("04_figuras/"                                  , x)}



# 1. Cargar datos --------------------------------------------------------------

load(paste_out("df_incidentes_sitjurid.RData"))
load(paste_out("df_incidentes.RData"))

# 2. Procesar datos ------------------------------------------------------------

df_sitjurid <- df_incidentes_sitjurid %>% 
    mutate(
        estatus = case_when(
            str_detect(situacion, "Sentenciada") ~ "Con sentencia", 
            str_detect(situacion, "Procesada"  ) ~ "En prisión preventiva", 
            T ~ "No especificado")
    )

unique(df_sitjurid$estatus)

# 3. Visualizar datos ----------------------------------------------------------

# ---- Tema
tema <-  theme_void() +
    theme(
        text             = element_text(family = "Fira Sans", color = "black"),
        plot.title       = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(10,5,5,5), family="Fira Sans", color = "black"),
        plot.subtitle    = element_text(size = 10, color = "#666666", hjust = 0.5, margin = margin(5, 5, 5, 5), family="Fira Sans"),
        plot.caption     = element_text(hjust = .5, size = 6, family = "Fira Sans", color = "black"),
        panel.grid       = element_line(linetype = 2),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position  = "top",
        legend.title     = element_text(size = 8, face = "bold", family="Fira Sans"),
        legend.text      = element_text(size = 10, family="Fira Sans"),
        axis.title.x     = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Fira Sans"),
        axis.title.y     = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Fira Sans", angle = 90),
        axis.text.y      = element_text(size = 8, family="Fira Sans", angle=0, hjust=.5),
        axis.text.x      = element_text(size = 8, family="Fira Sans", angle=0, hjust=.5),
        strip.background = element_rect(fill="white", colour = NA),
        strip.text.x     = element_text(size=9, family = "Fira Sans", face = "bold", color = "black"),
        strip.text.y     = element_text(size=9, family = "Fira Sans", face = "bold", color = "black"))



# ---- Colores 

v_rojos     <- c("#E23E57", "#88304E", "#522546", "#311D3F")
v_azules    <- c("#E9D5DA", "#827397", "#4D4C7D", "#363062")


## 3.1. Muertes por año total --------------------------------------------------------------

df_year <- df_incidentes %>% 
    filter(evento == "Decesos")     %>% 
    group_by(year)    %>% 
    summarise(total = sum(total)) %>% 
    mutate(total = if_else(year == 2021, 918+71, total))

ggplot(
    df_year, 
    aes(x = year, y = total)) +
    geom_col(fill = v_azules[4]) +
    geom_label(aes(label = total), family = "Fira Sans", size = 2) +
    labs(
        title = "Personas privadas de la libertad que\nfallecieron en centros penitenciarios de México", 
        subtitle = "Por año\n", 
        x = "", 
        y = "Total de decesos\n", 
        caption = "\nFuente: Cuadernos Mensuales de Información Estadística Penitenciaria Nacional. 
        Datos procesados por Intersecta (intersecta.org)\n") +
    scale_x_continuous(breaks = 2008:2021) +
    tema +
    theme(axis.title.x = element_text(angle = 90))

ggsave(paste_fig("08_decesos_ppl.png"), 
       device = "png", type = "cairo", 
       width = 6, height = 4)


## 3.2. Muertes por estatus mensual --------------------------------------------

# ---- Totales 

df_data <- df_sitjurid                                          %>% 
    filter(evento == "Decesos")                                 %>% 
    group_by(year, mes, estatus)                                %>% 
    summarise(total = sum(total))                               %>% 
    ungroup()                                                   %>% 
    mutate(fecha = as.Date(paste(year, mes, "01", sep = "-")))  %>% 
    filter(estatus != "No especificado")

ggplot(
    # Datos
    df_data, 
    # Coordenadas
    aes(x = fecha, y = total, color = estatus, group = estatus)) +
    # Geoms
    geom_line() +
    geom_point(size = 1) +
    # Etiquetas
    labs(
        title = "Personas privadas de la libertad que\nfallecieron en centros penitenciarios de México", 
        subtitle = "Por mes y por situación jurídica\n", 
        x = "\nMes",
        y = "\nTotal de decesos\n",
        color = "", 
        caption = "\nFuente: Cuadernos Mensuales de Información Estadística Penitenciaria Nacional. 
        Datos procesados por Intersecta (intersecta.org)\n") +
    # Escalas
    # scale_x_date() +
    scale_color_manual(values = v_rojos) +
    # Tema
    tema +
    theme(axis.title.x = element_text(angle = 0))

# ggsave(paste_fig("00_muertes_estatus.png"), 
#        device = "png", type = "cairo", 
#        width = 6, height = 4)

# ---- Porcentajes 
df_data <- df_sitjurid                                          %>% 
    filter(evento == "Decesos")                                 %>% 
    group_by(year, mes, estatus)                                %>% 
    summarise(total = sum(total))                               %>% 
    ungroup()                                                   %>% 
    group_by(year, mes)                                         %>% 
    mutate(total_total = sum(total),        
           porcentaje = total/total_total)                      %>% 
    ungroup()                                                   %>% 
    mutate(fecha = as.Date(paste(year, mes, "01", sep = "-")))  %>% 
    filter(estatus != "No especificado")

ggplot(
    # Datos
    df_data, 
    # Coordenadas
    aes(x = fecha, y = porcentaje, color = estatus, group = estatus)) +
    # Geoms
    geom_line() +
    geom_point(size = 1) +
    # Etiquetas
    labs(
        title = "¿Cuál era la situación jurídica de las personas privadas de la libertad 
        que fallecieron en centros penitenciarios de México?", 
        subtitle = "Por mes y por situación jurídica\n", 
        x = "\nMes",
        y = "",
        color = "", 
        caption = "\nFuente: Cuadernos Mensuales de Información Estadística Penitenciaria Nacional. 
        Datos procesados por Intersecta (intersecta.org)\n") +
    # Escalas
    # scale_x_date() +
    scale_color_manual(values = v_rojos) +
    scale_y_continuous(labels = scales::percent_format()) +
    # Tema
    tema +
    theme(axis.title.x = element_text(angle = 0), 
          plot.title = element_text(size = 13)) 

# ggsave(paste_fig("00_muertes_estatus_porcentajes.png"), 
#        device = "png", type = "cairo", 
#        width = 6, height = 4)


## 3.2. Muertes por estatus anual --------------------------------------------------------------

df_data <- df_sitjurid                      %>% 
    filter(evento == "Decesos")             %>% 
    group_by(year, estatus)                 %>% 
    summarise(total = sum(total))           %>% 
    ungroup()                               %>% 
    group_by(year)                          %>% 
    mutate(total_total = sum(total), 
           porcentaje = total/total_total)  %>% 
    ungroup()

ggplot(
    # Datos
    df_data, 
    # Coordenadas
    aes(x = year, y = porcentaje, group = estatus)) +
    # Geoms
    geom_col(aes(fill = estatus)) +
    geom_label(aes(label = scales::percent(porcentaje, accuracy = 0.1))) +
    # Etiquetas
    labs(
        title = "¿Cuál era la situación jurídica de las personas privadas de la libertad 
        que fallecieron en centros penitenciarios de México?", 
        subtitle = "Por mes y por situación jurídica\n", 
        x = "\n Año",
        y = "",
        fill = "", 
        caption = "\nFuente: Cuadernos Mensuales de Información Estadística Penitenciaria Nacional. 
        Datos procesados por Intersecta (intersecta.org)\n") +
    # Escalas
    # scale_x_date() +
    scale_fill_manual(values = v_rojos) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(breaks = 2015:2021) +
    # Tema
    tema +
    theme(axis.title.x = element_text(angle = 0), 
          plot.title = element_text(size = 13)) 

# ggsave(paste_fig("00_muertes_estatus_porcentajes_año.png"), 
#        device = "png", type = "cairo", 
#        width = 6, height = 4)

# FIN. -------------------------------------------------------------------------
