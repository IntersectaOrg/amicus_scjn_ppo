#------------------------------------------------------------------------------#
# Proyecto:              AMICUS para la SCJN sobre prisión preventiva oficiosa  
# Objetivo:              Procesar datos del Censo de Población y Vivienda (2020) 
#
# Encargadas:            Coordinación de datos de Intersecta
# 
# Fecha de creación:     06 de diciembre de 2021
# Última actualización:  31 de agosto de 2021
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Cargar librerías 
require(pacman)
p_load(srvyr, tidyverse, dplyr, lubridate, scales, beepr, data.table, extrafont)

# Limpiar espacio de trabajo 
rm(list=ls())

# Método para el caso de 1 UPM en los estratos
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

# 1. Cargar datos ---------------------------------------------------------

# Cuestionario ampliado 
# Datos disponibles en https://www.inegi.org.mx/programas/ccpv/2020/#Microdatos

# 2. Funciones ------------------------------------------------------------

## 2.1 Sexo ---------------------------------------------------------------
codificar_sexo <- function(var = x){
    
    v_sexo   <- c("Hombres", "Mujeres")
    
    case_when(
        var == "1" ~ v_sexo[1], 
        var == "3" ~ v_sexo[2])
}

## 2.2. Situación conyugal ------------------------------------------------

codificar_situacion_c <- function(var = x){
    
    v_situacion   <- c("Unión libre", "Separadas", "Divorciadas", "Viudas", 
                       "Casadas por el civil", "Casadas religiosamente", 
                       "Casadas por el civil y religiosamente", "Solteras",
                       "No especificado", "Otros")
    
    case_when(
        var == "1" ~ v_situacion[1], 
        var == "2" ~ v_situacion[2],
        var == "3" ~ v_situacion[3], 
        var == "4" ~ v_situacion[4], 
        var == "5" ~ v_situacion[5], 
        var == "6" ~ v_situacion[6], 
        var == "7" ~ v_situacion[7], 
        var == "8" ~ v_situacion[8], 
        var == "9" ~ v_situacion[9], 
        TRUE ~ v_situacion[10])
}

## 2.3. Sí/no -------------------------------------------------------------
codificar_sino <- function(var = x){
    
    v_sino   <- c("Sí", "No", "No especificado")
    
    case_when(
        var == "1" ~ v_sino[1], 
        var == "3" ~ v_sino[2],
        TRUE ~ v_sino[3])
}

## 2.4. Entidad -----------------------------------------------------------
codificar_entidad <- function(var = x){
    
    v_entidad <- c("Aguascalientes", "Baja California", "Baja California Sur", 
                   "Campeche", "Coahuila de Zaragoza", "Colima", "Chiapas", 
                   "Chihuahua", "Ciudad de México", "Durango", "Guanajuato", 
                   "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacán de Ocampo", 
                   "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", 
                   "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", 
                   "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
                   "Veracruz de Ignacio de la Llave", "Yucatán", "Zacatecas", 
                   "No especificado"
    )
    
    case_when(
        var == "1" ~ v_entidad[1], 
        var == "2" ~ v_entidad[2], 
        var == "3" ~ v_entidad[3], 
        var == "4" ~ v_entidad[4], 
        var == "5" ~ v_entidad[5], 
        var == "6" ~ v_entidad[6], 
        var == "7" ~ v_entidad[7], 
        var == "8" ~ v_entidad[8], 
        var == "9" ~ v_entidad[9], 
        var == "10" ~ v_entidad[10], 
        var == "11" ~ v_entidad[11], 
        var == "12" ~ v_entidad[12], 
        var == "13" ~ v_entidad[13], 
        var == "14" ~ v_entidad[14], 
        var == "15" ~ v_entidad[15], 
        var == "16" ~ v_entidad[16], 
        var == "17" ~ v_entidad[17], 
        var == "18" ~ v_entidad[18], 
        var == "19" ~ v_entidad[19], 
        var == "20" ~ v_entidad[20], 
        var == "21" ~ v_entidad[21], 
        var == "22" ~ v_entidad[22], 
        var == "23" ~ v_entidad[23], 
        var == "24" ~ v_entidad[24], 
        var == "25" ~ v_entidad[25], 
        var == "26" ~ v_entidad[26], 
        var == "27" ~ v_entidad[27], 
        var == "28" ~ v_entidad[28], 
        var == "29" ~ v_entidad[29], 
        var == "30" ~ v_entidad[30], 
        var == "31" ~ v_entidad[31], 
        var == "32" ~ v_entidad[32],
        TRUE ~ v_entidad[33])
}

## 2.5. Ocupación ---------------------------------------------------------
codificar_ocupacion <- function(var = x){
    
    v_sino   <- c("Funcionarios, directores y jefes",
                  "Profesionistas y técnicos",
                  "Trabajadores auxiliares en actividades administrativas",
                  "Comerciantes, empleados en ventas y agentes de ventas",
                  "Trabajadores en servicios personales y de vigilancia",
                  "Trabajadores en actividades agrícolas, ganaderas, forestales, caza y pesca",
                  "Trabajadores artesanales, en la construcción y otros oficios",
                  "Operadores de maquinaria industrial, ensambladores, choferes y conductores de transporte",
                  "Trabajadores en actividades elementales y de apoyo",
                  "No especificado")
    
    case_when(
        startsWith(var, "1") ~ v_sino[1],
        startsWith(var, "2") ~ v_sino[2],
        startsWith(var, "3") ~ v_sino[3],
        startsWith(var, "4") ~ v_sino[4],
        startsWith(var, "5") ~ v_sino[5],
        startsWith(var, "6") ~ v_sino[6],
        startsWith(var, "7") ~ v_sino[7],
        startsWith(var, "8") ~ v_sino[8],
        startsWith(var, "9") ~ v_sino[9],
        var == "999" ~ v_sino[10]
    )
}

## 2.6. Grupos edad -------------------------------------------------------
codificar_edad <- function(var = x){
    
    v_edad   <- c("18-24", "25-29", "30-34", "35-39", "40-44", 
                  "45-49", "Mayores de 50", "Otros")
    
    case_when(
        var >= 18 & var <= 24 ~ v_edad[1],
        var >= 25 & var <= 29 ~ v_edad[2],
        var >= 30 & var <= 34 ~ v_edad[3], 
        var >= 35 & var <= 39 ~ v_edad[4],
        var >= 40 & var <= 44 ~ v_edad[5],
        var >= 45 & var <= 49 ~ v_edad[6],
        var >= 50 & var <= 97 ~ v_edad[7],
        (var == 98 | var == 99) ~ v_edad[8],
        TRUE ~ v_edad[8])
}

## 2.7. Escolaridad -------------------------------------------------------
codificar_escolaridad <- function(var = x){
    
    v_grado   <- c("Ninguno", 
                   "Preescolar", 
                   "Primaria", 
                   "Secundaria",
                   "Preparatoria o bachillerato general",
                   "Bachillerato tecnológico", 
                   "Estudios técnicos o comerciales con primaria terminada",
                   "Estudios técnicos o comerciales con secundaria terminada",
                   "Estudios técnicos o comerciales con preparatoria terminada",
                   "Normal con primaria o secundaria terminada",
                   "Normal de licenciatura",
                   "Licenciatura", 
                   "Especialidad", 
                   "Maestría", 
                   "Doctorado",
                   "Otros")
    
    case_when(
        var == "0" ~ v_grado[1], 
        var == "1" ~ v_grado[2], 
        var == "2" ~ v_grado[3], 
        var == "3" ~ v_grado[4], 
        var == "4" ~ v_grado[5], 
        var == "5" ~ v_grado[6], 
        var == "6" ~ v_grado[7], 
        var == "7" ~ v_grado[8], 
        var == "8" ~ v_grado[9], 
        var == "9" ~ v_grado[10], 
        var == "10" ~ v_grado[11],
        var == "11" ~ v_grado[12],
        var == "12" ~ v_grado[13],
        var == "13" ~ v_grado[14],
        var == "14" ~ v_grado[15],
        var == "99" ~ v_grado[16])
}

## 2.8. Escolaridad reagrupada --------------------------------------------
codificar_escolaridad_r <- function(var = x){
    
    v_grado   <- c("Ninguno", 
                   "Preescolar", 
                   "Primaria", 
                   "Secundaria",
                   "Preparatoria o bachillerato general",
                   "Bachillerato tecnológico", 
                   "Estudios técnicos o comerciales con primaria terminada",
                   "Estudios técnicos o comerciales con secundaria terminada",
                   "Estudios técnicos o comerciales con preparatoria terminada",
                   "Normal con primaria o secundaria terminada",
                   "Normal de licenciatura",
                   "Licenciatura", 
                   "Especialidad", 
                   "Maestría", 
                   "Doctorado",
                   "Otros")
    
    v_grado_reagrupado   <- c("Ninguno", 
                              "Preescolar", 
                              "Primaria", 
                              "Secundaria",
                              "Preparatoria o bachillerato",
                              "Estudios técnicos o comerciales",
                              "Normal básica",
                              "Licenciatura o profesional", 
                              "Posgrado",
                              "Otros")
    
    case_when(
        var == v_grado[1] ~ v_grado_reagrupado[1], 
        var == v_grado[2] ~ v_grado_reagrupado[2], 
        var == v_grado[3] ~ v_grado_reagrupado[3], 
        var == v_grado[4] ~ v_grado_reagrupado[4], 
        var == v_grado[5] ~ v_grado_reagrupado[5], 
        var == v_grado[6] ~ v_grado_reagrupado[5], 
        var == v_grado[7] ~ v_grado_reagrupado[6], 
        var == v_grado[8] ~ v_grado_reagrupado[6], 
        var == v_grado[9] ~ v_grado_reagrupado[6], 
        var == v_grado[10] ~ v_grado_reagrupado[7], 
        var == v_grado[11] ~ v_grado_reagrupado[8],
        var == v_grado[12] ~ v_grado_reagrupado[8],
        var == v_grado[13] ~ v_grado_reagrupado[9],
        var == v_grado[14] ~ v_grado_reagrupado[9],
        var == v_grado[15] ~ v_grado_reagrupado[9],
        var == v_grado[16] ~ v_grado_reagrupado[10])
}

# 3. Procesar los datos ---------------------------------------------------

# Filtrar personas mayores de 18 años
df_personas_raw <- df_censo_2020[df_censo_2020$EDAD >= 18]

## 3.1. Renombrar variables -----------------------------------------------
df_limpio <- df_personas_raw %>% 
    rename(sexo               = SEXO,
           edad               = EDAD,
           `estado civil`     = SITUA_CONYUGAL,
           indigena           = PERTE_INDIGENA,
           `lengua indígena`  = HLENGUA,
           afrodescendiente   = AFRODES,
           alfabeta           = ALFABET,
           escolaridad        = ESCOACUM,
           `nivel escolar`    = NIVACAD,
           ocupacion          = OCUPACION_C,
           ingreso            = INGTRMEN
    )

## 3.2. Usar las funciones para recodificar las variables -----------------
df_limpio[, `:=` (
    sexo = codificar_sexo(sexo),
    g_edad = codificar_edad(edad),
    `estado civil` = codificar_situacion_c(`estado civil`),
    `lengua indígena` = codificar_sino(`lengua indígena`),
    indigena = codificar_sino(indigena),
    afrodescendiente = codificar_sino(afrodescendiente),
    alfabeta = codificar_sino(alfabeta),
    `nivel escolar` = codificar_escolaridad(`nivel escolar`),
    entidad = codificar_entidad(ENT),
    ocupacion = codificar_ocupacion(as.character(ocupacion))
)]

## 3.3. Aplicar formato de encuesta ---------------------------------------
df_encuesta <- df_limpio %>% 
    # Eliminar "-" de ESTRATO
    mutate(ESTRATO = gsub("-", "", ESTRATO, fixed = T)) %>% 
    # Convertir a numéricos
    mutate(ESTRATO = as.numeric(ESTRATO),
           UPM     = as.numeric(UPM),
           FACTOR  = as.numeric(FACTOR)) %>% 
    # Ordenar 
    arrange(ESTRATO, UPM) %>% 
    # Aplicar diseño de encuesta
    as_survey_design_(
        ids = ~UPM, 
        strata = ~ESTRATO,
        weights = ~FACTOR,
        nest = TRUE) 

# Guardar objeto
# save(df_encuesta, file = paste_data("df_encuesta.RData"))
load(paste_data("df_encuesta.RData"))

# 4. Tema para gráficas ---------------------------------------------------

# Tema para gráficas 
tema <-  theme_linedraw() +
    theme(
        text             = element_text(family = "Avenir Next Condensed", color = "black"),
        plot.title       = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(10,5,5,5), family="Avenir Next Condensed", color = "black"),
        plot.subtitle    = element_text(size = 13, color = "#666666", hjust = 0.5, margin = margin(5, 5, 5, 5), family="Avenir Next Condensed"),
        plot.caption     = element_text(hjust = .5, size = 9, family = "Avenir Next Condensed", color = "black"),
        panel.grid       = element_line(linetype = 2), 
        legend.position  = "top",
        panel.grid.minor = element_blank(),
        legend.title     = element_text(size = 10, face = "bold", family="Avenir Next Condensed"),
        legend.text      = element_text(size = 10, family="Avenir Next Condensed"),
        axis.title       = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Avenir Next Condensed"),
        axis.text.y      = element_text(size = 8, family="Avenir Next Condensed", angle=0, hjust=.5),
        axis.text.x      = element_text(size = 8, family="Avenir Next Condensed", angle=0, hjust=.5, margin = margin(l = 3, unit = "cm")),
        strip.background = element_rect(fill="white", colour = NA),
        strip.text.x     = element_text(size=9, family = "Avenir Next Condensed", face = "bold", color = "black"),
        strip.text.y     = element_text(size=9, family = "Avenir Next Condensed", face = "bold", color = "black"))

v_colors <- c("#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#2ec4b6", "#cbf3f0", "#ffbf69", 
              "#ff006e", "#8338ec", "#3a86ff", "gray")

v_colshort <- c("#ef476f", "#ffd166", "#06d6a0", "#118ab2")

# Etiquetas
v_caption       <- "Fuente: Muetra del Cuestionario Ampliado Censo 2020. 
Datos procesados por Intersecta.org"
v_percent       <- "\nPorcentaje"
v_empty         <- ""
v_sexo          <- "Por sexo"
v_formato       <- ".png"

# 5. Figuras --------------------------------------------------------------

## 5.1. Situación conyugal ------------------------------------------------

# Proporciones para entidad 1
df_data <- df_encuesta[df_encuesta$variables$ENT == 1]

df_tabla <- df_data                        %>% 
    srvyr::group_by(sexo, `estado civil`)  %>%
    srvyr::survey_count()                  %>% 
    ungroup()

# Proporciones para el resto de las entidades
for(i in 2:32){
    print(paste("Vuelta", i, "de", 32))
    
    # Filtrar por entidad
    df_data <- df_encuesta[df_encuesta$variables$ENT == i]
    
    # Estimación de totales y proporciones
    df_tabla_loop <- df_data             %>% 
        srvyr::group_by(sexo, `estado civil`)  %>%
        srvyr::survey_count()            %>% 
        ungroup()
    
    df_tabla <- df_tabla %>% bind_rows(df_tabla_loop)
}

# Renombrar base
df_situacion_conyugal <- df_tabla

# Guardar base
save(df_situacion_conyugal, file = paste_data("df_situacion_conyugal.RData"))

## ---- General
# Proporciones a nivel nacional
df_tabla_nacional <- df_situacion_conyugal  %>% 
    group_by(`estado civil`)                %>% 
    summarise(total = sum(n))               %>% 
    ungroup()                               %>% 
    mutate(porcentaje = total/sum(total))

# Gráfica 
v_title     <- "En México, ¿cuál es el estado civil de las personas de 18 años o más?"

ggplot(df_tabla_nacional,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(`estado civil`, porcentaje))) +
    geom_col(position = position_stack(reverse = TRUE), fill = "#9381FF") +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = `estado civil`),
               position = position_stack(vjust = 1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_empty, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema
    tema +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(15))

ggsave(
    file = paste0(paste_figs("01_estado_civil_2020_porct"), v_formato), 
    width = 8, height = 8) 

## ---- Por sexo
# Proporciones a nivel nacional
df_tabla_nacional <- df_situacion_conyugal   %>% 
    group_by(sexo, `estado civil`)           %>% 
    summarise(total = sum(n))                %>% 
    ungroup()                                %>% 
    group_by(sexo)                           %>% 
    mutate(porcentaje = total/sum(total))

# Gráfica 
v_title     <- "En México, ¿cuál es el estado civil de las personas de 18 años o más?"

ggplot(df_tabla_nacional,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(`estado civil`, porcentaje))) +
    geom_col(position = position_stack(reverse = TRUE), fill = "#9381FF") +
    geom_label(aes(label=paste0(round(porcentaje,4)*100, "%"), group = `estado civil`),
               position = position_stack(vjust = 1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_sexo, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema
    tema +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(15)) +
    facet_wrap(~sexo)

ggsave(
    file = paste0(paste_figs("01_estado_civil_sexo_2020_porct"), v_formato), 
    width = 8, height = 8) 

## 5.2. Lengua indígena ---------------------------------------------------

# Proporciones para entidad 1
df_data <- df_encuesta[df_encuesta$variables$ENT == 1]

df_tabla <- df_data                           %>% 
    srvyr::group_by(sexo, `lengua indígena`)  %>%
    srvyr::survey_count()                     %>% 
    ungroup()

# Proporciones para el resto de las entidades
for(i in 2:32){
    print(paste("Vuelta", i, "de", 32))
    
    # Filtrar por entidad
    df_data <- df_encuesta[df_encuesta$variables$ENT == i]
    
    # Estimación de totales y proporciones
    df_tabla_loop <- df_data                      %>% 
        srvyr::group_by(sexo, `lengua indígena`)  %>%
        srvyr::survey_count()                     %>% 
        ungroup()
    
    df_tabla <- df_tabla %>% bind_rows(df_tabla_loop)
}

# Renombrar base
df_lengua_indígena <- df_tabla

# Guardar base
save(df_lengua_indígena, file = paste_data("df_lengua_indígena.RData"))

## ---- General
# Proporciones a nivel nacional
df_tabla_nacional <- df_lengua_indígena  %>% 
    group_by(`lengua indígena`)          %>% 
    summarise(total = sum(n))            %>% 
    ungroup()                            %>% 
    mutate(porcentaje = total/sum(total))

# Gráfica 
v_title     <- "En México, ¿las personas de 18 años o más hablan alguna lengua indígena?"
# hay que cambiar "lengua"

ggplot(df_tabla_nacional,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(`lengua indígena`, porcentaje))) +
    geom_col(position = position_stack(reverse = TRUE), fill = "#9381FF") +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = `lengua indígena`),
               position = position_stack(vjust = 1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_empty, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema
    tema +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(15))

ggsave(
    file = paste0(paste_figs("01_lengua_indígena_2020_porct"), v_formato), 
    width = 8, height = 8) 

## ---- Por sexo
# Proporciones a nivel nacional
df_tabla_nacional <- df_lengua_indígena      %>% 
    group_by(sexo, `lengua indígena`)        %>% 
    summarise(total = sum(n))                %>% 
    ungroup()                                %>% 
    group_by(sexo)                           %>% 
    mutate(porcentaje = total/sum(total))

# Gráfica 
v_title     <- "En México, ¿las personas de 18 años o más hablan alguna lengua indígena?"

ggplot(df_tabla_nacional,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(`lengua indígena`, porcentaje))) +
    geom_col(position = position_stack(reverse = TRUE), fill = "#9381FF") +
    geom_label(aes(label=paste0(round(porcentaje,4)*100, "%"), group = `lengua indígena`),
               position = position_stack(vjust = 1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_sexo, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema
    tema +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(15)) +
    facet_wrap(~sexo)

ggsave(
    file = paste0(paste_figs("01_lengua_indígena_sexo_2020_porct"), v_formato), 
    width = 8, height = 8) 


## 5.3. Autoadscripción afrodescendiente ----------------------------------

# Proporciones para entidad 1
df_data <- df_encuesta[df_encuesta$variables$ENT == 1]

df_tabla <- df_data                           %>% 
    srvyr::group_by(sexo, afrodescendiente)   %>%
    srvyr::survey_count()                     %>% 
    ungroup()

# Proporciones para el resto de las entidades
for(i in 2:32){
    print(paste("Vuelta", i, "de", 32))
    
    # Filtrar por entidad
    df_data <- df_encuesta[df_encuesta$variables$ENT == i]
    
    # Estimación de totales y proporciones
    df_tabla_loop <- df_data                      %>% 
        srvyr::group_by(sexo, afrodescendiente)   %>%
        srvyr::survey_count()                     %>% 
        ungroup()
    
    df_tabla <- df_tabla %>% bind_rows(df_tabla_loop)
}

# Renombrar base
df_afrodescendiente <- df_tabla

# Guardar base
save(df_afrodescendiente, file = paste_data("df_afrodescendiente.RData"))

## ---- General
# Proporciones a nivel nacional
df_tabla_nacional <- df_afrodescendiente  %>% 
    group_by(afrodescendiente)            %>% 
    summarise(total = sum(n))             %>% 
    ungroup()                             %>% 
    mutate(porcentaje = total/sum(total))

# Gráfica 
v_title     <- "En México, ¿las personas de 18 años o más 
se autoadscriben como personas afrodescendientes?"

ggplot(df_tabla_nacional,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(afrodescendiente, porcentaje))) +
    geom_col(position = position_stack(reverse = TRUE), fill = "#9381FF") +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = afrodescendiente),
               position = position_stack(vjust = 1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_empty, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema
    tema +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(15))

ggsave(
    file = paste0(paste_figs("01_afrodescendiente_2020_porct"), v_formato), 
    width = 8, height = 8) 

## ---- Por sexo
# Proporciones a nivel nacional
df_tabla_nacional <- df_afrodescendiente     %>% 
    group_by(sexo, afrodescendiente)         %>% 
    summarise(total = sum(n))                %>% 
    ungroup()                                %>% 
    group_by(sexo)                           %>% 
    mutate(porcentaje = total/sum(total))

# Gráfica 
v_title     <- "En México, ¿las personas de 18 años o más
se autoadscriben como personas afrodescendientes?"

ggplot(df_tabla_nacional,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(afrodescendiente, porcentaje))) +
    geom_col(position = position_stack(reverse = TRUE), fill = "#9381FF") +
    geom_label(aes(label=paste0(round(porcentaje,4)*100, "%"), group = afrodescendiente),
               position = position_stack(vjust = 1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_sexo, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema
    tema +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(15)) +
    facet_wrap(~sexo)

ggsave(
    file = paste0(paste_figs("01_afrodescendiente_sexo_2020_porct"), v_formato), 
    width = 8, height = 8) 

## 5.4. Alfabetismo -------------------------------------------------------

# Proporciones para entidad 1
df_data <- df_encuesta[df_encuesta$variables$ENT == 1]

df_tabla <- df_data                   %>% 
    srvyr::group_by(sexo, alfabeta)   %>%
    srvyr::survey_count()             %>% 
    ungroup()

# Proporciones para el resto de las entidades
for(i in 2:32){
    print(paste("Vuelta", i, "de", 32))
    
    # Filtrar por entidad
    df_data <- df_encuesta[df_encuesta$variables$ENT == i]
    
    # Estimación de totales y proporciones
    df_tabla_loop <- df_data              %>% 
        srvyr::group_by(sexo, alfabeta)   %>%
        srvyr::survey_count()             %>% 
        ungroup()
    
    df_tabla <- df_tabla %>% bind_rows(df_tabla_loop)
}

# Renombrar base
df_alfabetismo <- df_tabla

# Guardar base
save(df_alfabetismo, file = paste_data("df_alfabetismo.RData"))

## ---- General
# Proporciones a nivel nacional
df_tabla_nacional <- df_alfabetismo  %>% 
    group_by(alfabeta)               %>% 
    summarise(total = sum(n))        %>% 
    ungroup()                        %>% 
    mutate(porcentaje = total/sum(total))

# Gráfica 
v_title     <- "En México, ¿las personas de 18 años o más saben leer y escribir un recado?"

ggplot(df_tabla_nacional,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(alfabeta, porcentaje))) +
    geom_col(position = position_stack(reverse = TRUE), fill = "#9381FF") +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = alfabeta),
               position = position_stack(vjust = 1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_empty, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema
    tema +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(15))

ggsave(
    file = paste0(paste_figs("01_alfabetismo_2020_porct"), v_formato), 
    width = 8, height = 8) 

## ---- Por sexo
# Proporciones a nivel nacional
df_tabla_nacional <- df_alfabetismo      %>% 
    group_by(sexo, alfabeta)             %>% 
    summarise(total = sum(n))            %>% 
    ungroup()                            %>% 
    group_by(sexo)                       %>% 
    mutate(porcentaje = total/sum(total))

# Gráfica  
v_title     <- "En México, ¿las personas de 18 años o más saben leer y escribir un recado?"

ggplot(df_tabla_nacional,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(alfabeta, porcentaje))) +
    geom_col(position = position_stack(reverse = TRUE), fill = "#9381FF") +
    geom_label(aes(label=paste0(round(porcentaje,4)*100, "%"), group = alfabeta),
               position = position_stack(vjust = 1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_sexo, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema
    tema +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(15)) +
    facet_wrap(~sexo)

ggsave(
    file = paste0(paste_figs("01_alfabetismo_sexo_2020_porct"), v_formato), 
    width = 8, height = 8) 

## 5.5. Escolaridad -------------------------------------------------------

# Proporciones para entidad 1
df_data <- df_encuesta[df_encuesta$variables$ENT == 1]

df_tabla <- df_data                      %>% 
    srvyr::group_by(sexo, escolaridad)   %>%
    srvyr::survey_count()                %>% 
    ungroup()

# Proporciones para el resto de las entidades

for(i in 2:32){
    print(paste("Vuelta", i, "de", 32))
    
    # Filtrar por entidad
    df_data <- df_encuesta[df_encuesta$variables$ENT == i]
    
    # Estimación de totales y proporciones
    df_tabla_loop <- df_data                %>% 
        srvyr::group_by(sexo, escolaridad)  %>%
        srvyr::survey_count()               %>% 
        ungroup()
    
    df_tabla <- df_tabla %>% bind_rows(df_tabla_loop)
}

# Renombrar base
df_escolaridad <- df_tabla

# Guardar base
save(df_escolaridad, file = paste_data("df_escolaridad.RData"))

## ---- General
# Proporciones a nivel nacional
df_tabla_nacional <- df_escolaridad  %>% 
    group_by(escolaridad)            %>% 
    summarise(total = sum(n))        %>% 
    ungroup()                        %>% 
    mutate(porcentaje = total/sum(total))

# Gráfica 
v_title     <- "En México, ¿cuántos grados escolares aprobaron las personas de 18 años o más?"

ggplot(df_tabla_nacional %>% filter(escolaridad != 99),
       # Coordenadas y geoms
       aes(x = escolaridad, y = porcentaje)) +
    geom_col(position = position_stack(reverse = TRUE), fill = "#9381FF") +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = escolaridad),
               position = position_stack(vjust = 1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_empty, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema
    tema +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = seq(0, 24, by = 1))

ggsave(
    file = paste0(paste_figs("01_escolaridad_acumulada_2020_porct"), v_formato), 
    width = 8, height = 6) 

## ---- Por sexo
# Proporciones a nivel nacional
df_tabla_nacional <- df_escolaridad      %>% 
    group_by(sexo, escolaridad)          %>% 
    summarise(total = sum(n))            %>% 
    ungroup()                            %>% 
    group_by(sexo)                       %>% 
    mutate(porcentaje = total/sum(total))

# Gráfica  
v_title     <- "En México, ¿cuál es el último grado escolar que las personas de 18 años o más alcanzaron?"

ggplot(df_tabla_nacional %>% filter(escolaridad != 99),
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(escolaridad, -escolaridad))) +
    geom_col(position = position_stack(reverse = TRUE), fill = "#9381FF") +
    geom_label(aes(label=paste0(round(porcentaje,4)*100, "%"), group = escolaridad),
               position = position_stack(vjust = 1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_sexo, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema
    tema +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(15)) +
    facet_wrap(~sexo)

ggsave(
    file = paste0(paste_figs("01_escolaridad_acumulada_sexo_2020_porct"), v_formato), 
    width = 8, height = 6) 

## 5.6. Nivel escolar -----------------------------------------------------

# Proporciones para entidad 1
df_data <- df_encuesta[df_encuesta$variables$ENT == 1]

df_tabla <- df_data                          %>% 
    srvyr::group_by(sexo, `nivel escolar`)   %>%
    srvyr::survey_count()                    %>% 
    ungroup()

# Proporciones para el resto de las entidades

for(i in 13:32){
    print(paste("Vuelta", i, "de", 32))
    
    # Filtrar por entidad
    df_data <- df_encuesta[df_encuesta$variables$ENT == i]
    
    # Estimación de totales y proporciones
    df_tabla_loop <- df_data                    %>% 
        srvyr::group_by(sexo, `nivel escolar`)  %>%
        srvyr::survey_count()                   %>% 
        ungroup()
    
    df_tabla <- df_tabla %>% bind_rows(df_tabla_loop)
}

# Renombrar base
df_nivel_escolar <- df_tabla

# Guardar base
save(df_nivel_escolar, file = paste_data("df_nivel_escolar.RData"))

## ---- General
# Proporciones a nivel nacional
df_tabla_nacional <- df_nivel_escolar  %>% 
    group_by(`nivel escolar`)          %>% 
    summarise(total = sum(n))          %>% 
    ungroup()                          %>% 
    mutate(porcentaje = total/sum(total))

# Gráfica 
v_title     <- "En México, ¿cuál es el último grado escolar que 
las personas de 18 años o más alcanzaron?"

ggplot(df_tabla_nacional,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(`nivel escolar`, porcentaje))) +
    geom_col(width = 0.9, position = position_dodge(10), fill = "#9381FF") +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = `nivel escolar`),
               position = position_stack(vjust = 1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_empty, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema
    tema +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(35))

ggsave(
    file = paste0(paste_figs("01_escolaridad_2020_porct"), v_formato), 
    width = 6, height = 8) 

## ---- Por sexo
# Proporciones a nivel nacional
df_tabla_nacional <- df_nivel_escolar    %>% 
    group_by(sexo, `nivel escolar`)      %>% 
    summarise(total = sum(n))            %>% 
    ungroup()                            %>% 
    group_by(sexo)                       %>% 
    mutate(porcentaje = total/sum(total))

# Gráfica  
v_title     <- "En México, ¿cuál es el último grado escolar que
las personas de 18 años o más alcanzaron?"

ggplot(df_tabla_nacional,
       # Coordenadas y geoms
       aes(y = reorder(`nivel escolar`, porcentaje), x = porcentaje)) +
    geom_col(width = 0.9, position = position_stack(reverse = TRUE), fill = "#9381FF") +
    geom_label(aes(label=paste0(round(porcentaje,4)*100, "%"), group = `nivel escolar`),
               position = position_stack(vjust = 1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_sexo, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema
    tema +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(35)) +
    facet_wrap(~sexo)

ggsave(
    file = paste0(paste_figs("01_escolaridad_sexo_2020_porct"), v_formato), 
    width = 8, height = 8) 

## 5.7. Grupos de edad ---------------------------------------------------

# Tema
tema <- theme_void() + 
    theme(
        text             = element_text(family = "Fira Sans", color = "black"),
        plot.title       = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(10,5,5,5), family="Fira Sans", color = "black"),
        plot.subtitle    = element_text(size = 13, color = "#666666", hjust = 0.5, margin = margin(5, 5, 5, 5), family="Fira Sans"),
        plot.caption     = element_text(hjust = .5, size = 8, family = "Fira Sans", color = "black"),
        panel.grid       = element_line(linetype = 2),
        panel.grid.major = element_blank(), 
        legend.position  = "top",
        panel.grid.minor = element_blank(),
        legend.title     = element_text(size = 10, face = "bold", family="Fira Sans"),
        legend.text      = element_text(size = 10, family="Fira Sans"),
        axis.title       = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Fira Sans", angle = 90),
        axis.text.y      = element_text(size = 8, family="Fira Sans", angle=0, hjust=.5),
        axis.text.x      = element_text(size = 8, family="Fira Sans", angle=0, hjust=.5),
        strip.background = element_rect(fill="white", colour = NA),
        strip.text.x     = element_text(size=9, family = "Fira Sans", face = "bold", color = "black"),
        strip.text.y     = element_text(size=9, family = "Fira Sans", face = "bold", color = "black", angle = 0))

v_colors_todos <- c("#e23e57", "#88304e", "#522546", "#311d3f",
                    "#e9d5da", "#827397", "#4d4c7d", "#363062")

v_colors_blue <- c("#e9d5da", "#827397", "#4d4c7d", "#363062")

v_colors_red <- c("#e23e57", "#88304e", "#522546", "#311d3f")


# Proporciones para entidad 1
df_data <- df_encuesta[df_encuesta$variables$ENT == 1]

df_tabla <- df_data                       %>% 
    srvyr::group_by(sexo, g_edad)         %>%
    srvyr::survey_count()                 %>% 
    ungroup()

# Proporciones para el resto de las entidades

for(i in 2:32){
    print(paste("Vuelta", i, "de", 32))
    
    # Filtrar por entidad
    df_data <- df_encuesta[df_encuesta$variables$ENT == i]
    
    # Estimación de totales y proporciones
    df_tabla_loop <- df_data                    %>% 
        srvyr::group_by(sexo, g_edad)           %>%
        srvyr::survey_count()                   %>% 
        ungroup()
    
    df_tabla <- df_tabla %>% bind_rows(df_tabla_loop)
}

# Renombrar base
df_edad <- df_tabla

# Guardar base
save(df_edad, file = paste_data("df_edad.RData"))

# Gráficas 
v_title     <- "En México, ¿cuál es el la edad de 
las personas?"

# ---- Proporciones generales
df_data <- df_edad                     %>% 
    group_by(g_edad)                   %>% 
    summarise(total = sum(n))          %>% 
    ungroup()                          %>% 
    mutate(porcentaje = total/sum(total))

ggplot(df_data,
       # Coordenadas y geoms
       aes(x = g_edad, y = porcentaje)) +
    geom_col(fill = "#4d4c7d") +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = g_edad),
               position = position_stack(1), size=2, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_empty, 
        x        = "Grupo quinquenal\n", 
        y        = v_empty,
        caption  = v_caption) +
    # Tema 
    guides(fill = "none") +
    tema +
    scale_x_discrete(labels = scales::wrap_format(15)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(axis.text.x = element_text(angle = 0),
          axis.title.x = element_text(angle = 0))

ggsave(
    file = paste0(paste_figs("01_edad_2020_porct"), v_formato), 
    width = 6, height = 4) 

# ---- Por sexo

df_data <- df_edad                       %>% 
    group_by(sexo, g_edad)               %>% 
    summarise(total = sum(n))            %>% 
    ungroup()                            %>% 
    group_by(sexo)                       %>% 
    mutate(porcentaje = total/sum(total))

ggplot(df_data,
       # Coordenadas y geoms
       aes(x = factor(g_edad,
                      levels = c("Otros",
                                 "Mayores de 50",
                                 "45-49",
                                 "40-44",
                                 "35-39",
                                 "30-34",
                                 "25-29",
                                 "18-24")),
           y = porcentaje)) +
    geom_col(aes(fill = sexo)) +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = g_edad),
               position = position_stack(1), size=2, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_sexo, 
        x        = "Grupo quinquenal\n", 
        y        = v_percent,
        caption  = v_caption) +
    # Tema 
    guides(fill = "none") +
    tema +
    scale_fill_manual(values = v_colors_blue) +
    scale_x_discrete(labels = scales::wrap_format(15)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(axis.text.x = element_text(angle = 0),
          axis.title.x = element_text(angle = 0)) +
    facet_grid(~sexo) +
    coord_flip()

ggsave(
    file = paste0(paste_figs("01_edad_sexo_2020_porct"), v_formato), 
    width = 8, height = 8) 

## 5.8. Autoadscripción indígena ----------------------------------------

# Proporciones para entidad 1
df_data <- df_encuesta[df_encuesta$variables$ENT == 1]

df_tabla <- df_data                           %>% 
    srvyr::group_by(sexo, indigena)           %>%
    srvyr::survey_count()                     %>% 
    ungroup()

# Proporciones para el resto de las entidades
for(i in 2:32){
    print(paste("Vuelta", i, "de", 32))
    
    # Filtrar por entidad
    df_data <- df_encuesta[df_encuesta$variables$ENT == i]
    
    # Estimación de totales y proporciones
    df_tabla_loop <- df_data                      %>% 
        srvyr::group_by(sexo, indigena)           %>%
        srvyr::survey_count()                     %>% 
        ungroup()
    
    df_tabla <- df_tabla %>% bind_rows(df_tabla_loop)
}

# Renombrar base
df_indigena <- df_tabla

# Guardar base
save(df_indigena, file = paste_data("df_indigena.RData"))

## ---- General
# Proporciones a nivel nacional
df_data <- df_indigena                    %>% 
    group_by(indigena)                    %>% 
    summarise(total = sum(n))             %>% 
    ungroup()                             %>% 
    mutate(porcentaje = total/sum(total))

# Gráfica 
v_title     <- "En México, ¿las personas de 18 años o más 
se autoadscriben como personas indígenas?"

ggplot(df_data,
       # Coordenadas y geoms
       aes(x = reorder(indigena, -porcentaje), y = porcentaje)) +
    geom_col(fill = "#4d4c7d") +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = indigena),
               position = position_stack(1), size=2, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_empty, 
        x        = v_empty, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema 
    guides(fill = "none") +
    tema +
    scale_x_discrete(labels = scales::wrap_format(15)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(axis.text.x = element_text(angle = 0),
          axis.title.x = element_text(angle = 0))

ggsave(
    file = paste0(paste_figs("01_a_indigena_2020_porct"), v_formato), 
    width = 8, height = 8) 

## ---- Por sexo
# Proporciones a nivel nacional
df_data <- df_indigena                       %>% 
    group_by(sexo, indigena)                 %>% 
    summarise(total = sum(n))                %>% 
    ungroup()                                %>% 
    group_by(sexo)                           %>% 
    mutate(porcentaje = total/sum(total))

# Gráfica 
v_title     <- "En México, ¿las personas de 18 años o más
se autoadscriben como personas indígenas?"

ggplot(df_data,
       # Coordenadas y geoms
       aes(x = reorder(indigena, porcentaje),
           y = porcentaje)) +
    geom_col(aes(fill = sexo)) +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = indigena),
               position = position_stack(1), size=2, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_sexo, 
        x        = v_empty, 
        y        = v_percent,
        caption  = v_caption) +
    # Tema 
    guides(fill = "none") +
    tema +
    scale_fill_manual(values = v_colors_blue) +
    scale_x_discrete(labels = scales::wrap_format(15)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(axis.text.x = element_text(angle = 0),
          axis.title.x = element_text(angle = 0)) +
    facet_grid(~sexo) +
    coord_flip()

ggsave(
    file = paste0(paste_figs("01_a_indigena_sexo_2020_porct"), v_formato), 
    width = 8, height = 8) 
