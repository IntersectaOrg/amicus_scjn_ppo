#------------------------------------------------------------------------------#
# Proyecto:              AMICUS para la SCJN sobre prisión preventiva oficiosa  
# Objetivo:              Procesar datos de la Encuesta Nacional de Población 
#
# Encargadas:            Coordinación de datos de Intersecta
# 
# Fecha de creación:     06 de diciembre de 2021
# Última actualización:  31 de agosto de 2021
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# ---- Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# ---- Silenciar warnings 
options(warn=-1)

# ---- Cargar librerías 
require(pacman)
p_load(srvyr, tidyverse, dplyr, lubridate, scales)

# ---- Limpiar espacio de trabajo 
rm(list=ls())

# ---- Eliminar notación científica 
options(scipen=999)

# ---- Método para el caso de 1 UPM en los estratos
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

# ---- Funciones de directorios
paste_code  <- function(x){paste0("01_códigos/04_enpol/"      , x)}
paste_inp   <- function(x){paste0("02_datos_crudos/04_enpol/" , x)}
paste_data  <- function(x){paste0("03_datos_limpios/04_enpol/", x)}
paste_figs  <- function(x){paste0("04_figuras/"               , x)}


# 1. Cargar datos --------------------------------------------------------------

# ---- ENPOL 2021
# Bases de datos originales disponible en
# https://www.inegi.org.mx/programas/enpol/2021/#Microdatos

load(paste_data("df_ENPOL_2021.RData"))

# 2. Funciones -----------------------------------------------------------------

## 2.1. Grupos edad ----------------------------------------------------
codificar_edad <- function(var = x){
    
    v_edad   <- c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                  "Mayores de 50", "Otros")
    
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

## 2.2. Escolaridad ------------------------------------------------------------
codificar_escolaridad <- function(var = x){
    
    v_escolaridad <- c("Ninguno", "Preescolar", "Primaria", "Secundaria", 
                       "Carrera técnica con secundaria",
                       "Normal básica", "Preparatoria o bachillerato", 
                       "Carrera técnica con preparatoria",
                       "Licenciatura", "Maestría o doctorado", "Otros")
    
    case_when( 
        var == "00" ~ v_escolaridad[1],
        var == "01" ~ v_escolaridad[2], 
        var == "02" ~ v_escolaridad[3], 
        var == "03" ~ v_escolaridad[4], 
        var == "04" ~ v_escolaridad[5], 
        var == "05" ~ v_escolaridad[6], 
        var == "06" ~ v_escolaridad[7], 
        var == "07" ~ v_escolaridad[8], 
        var == "08" ~ v_escolaridad[9], 
        var == "09" ~ v_escolaridad[10], 
        var == "98" ~ v_escolaridad[11],
        var == "99" ~ v_escolaridad[11],
        TRUE ~ v_escolaridad[11])
}

## 2.3. Ingresos el mes antes de la dentención ---------------------------------
codificar_ingresos <- function(var = x){
    
    v_ingresos  <- c("Menos de $3,000",
                     "De $3,000 a $5,500",
                     "De $5,501 a $7,500",
                     "De $7,501 a $9,000",
                     "De $9,001 a $11,000",
                     "Más de $11,000",
                     "No recibía ingresos",
                     "Otros")
    
    case_when(
        var == 1 ~ v_ingresos[1],
        var == 2 ~ v_ingresos[2],
        var == 3 ~ v_ingresos[3],
        var == 4 ~ v_ingresos[4],
        var == 5 ~ v_ingresos[5],
        var == 6 ~ v_ingresos[6],
        var == 7 ~ v_ingresos[7],
        var == 8 ~ v_ingresos[8],
        var == 9 ~ v_ingresos[8],
        TRUE ~ v_ingresos[8])
}

## 2.4. Servicios solicitados al Centro ----------------------------------------

codificar_servicios <- function(var = x){
    v_respuestas <- c("Sí", "No", "No lo he solicitado", "No sabe", "No responde")
    
    case_when(
        var == "1" ~ v_respuestas[1],
        var == "2" ~ v_respuestas[2],
        var == "3" ~ v_respuestas[3],
        var == "8" ~ v_respuestas[4],
        var == "9" ~ v_respuestas[5],
    )
}

## 2.5. Función para codificar lugar donde vive la visita ----------------------

codificar_lugar <- function(var =x){
    v_lugar <- c("Misma ciudad o localidad", "Mismo estado pero localidad diferente", 
                 "Otro estado", "Otro país", "Otros")
    case_when(
        var == "1" ~ v_lugar[1],
        var == "2" ~ v_lugar[2],
        var == "3" ~ v_lugar[3],
        var == "4" ~ v_lugar[4],
        var == "8" ~ v_lugar[5],
        var == "9" ~ v_lugar[5],
    )
}

## 2.6. Sí/No 2 opciones -------------------------------------------------------
codificar_siono2 <- function(var = x){
    
    v_respuestas <- c("No", "Sí", "Otros")
    
    case_when(
        var == "0" ~ v_respuestas[1],
        var == "1" ~ v_respuestas[2], 
        TRUE ~ v_respuestas[3])
}

## 2.7. Sí/No 4 opciones (otros) -----------------------------------------------

codificar_siono4o <- function(var = x){
    
    v_respuestas <- c("Sí", "No", "Otros")
    
    case_when(
        var == "1" ~ v_respuestas[1],
        var == "2" ~ v_respuestas[2],
        var == "8" ~ v_respuestas[3],
        var == "9" ~ v_respuestas[3],
        TRUE ~ v_respuestas[3]
    )
}

# 3. Procesar los datos --------------------------------------------------------

## 3.1. Usar las funciones para recodificar las variables ----------------------
df_limpio <- df_ENPOL_2021                       %>% 
    # Convertir edad en valor numérico
    mutate(
        edad = as.numeric(as.character(P1_3))
    ) %>% 
    # Recodificar variables
    mutate(
        g_edad      = codificar_edad(edad),
        escolaridad = codificar_escolaridad(P1_18_N),
        ingresos    = codificar_ingresos(P2_15),
        lugar       = codificar_lugar(P7_29),
        gasto = as.numeric(as.character(P7_30))
        )  %>%
    # Recodificar sexo
    mutate(
        sexo = case_when(
            SEXO == 1 ~ "Hombres", 
            SEXO == 2 ~ "Mujeres")) %>% 
    # Crear variable de disrciminación
    mutate(
        discriminacion = 
            case_when(
                (P7_46_01 == "1" | P7_46_02 == "1" | P7_46_03 == "1" |
                     P7_46_04 == "1" | P7_46_05 == "1" | P7_46_06 == "1" |
                     P7_46_07 == "1" | P7_46_08 == "1" | P7_46_09 == "1" | 
                     P7_46_10 == "1" | P7_46_11 == "1" | P7_46_12 == "1" |
                     P7_46_13 == "1" | P7_46_14 == "1") ~ "Sí",
                TRUE ~ "No",
                (P7_46_01 %in% c("8", "9") & P7_46_02 %in% c("8", "9") & 
                     P7_46_03 %in% c("8", "9") & P7_46_04 %in% c("8", "9") & 
                     P7_46_05 %in% c("8", "9") & P7_46_06 %in% c("8", "9") &
                     P7_46_07 %in% c("8", "9") & P7_46_08 %in% c("8", "9") & 
                     P7_46_09 %in% c("8", "9") & P7_46_10 %in% c("8", "9") & 
                     P7_46_11 %in% c("8", "9") & P7_46_12 %in% c("8", "9") &
                     P7_46_13 %in% c("8", "9") & P7_46_14 %in% c("8", "9")) ~ NA_character_
            )
    ) %>% 
    # Filtrar a personas en prisión preventiva
    filter(P5_3 == 1)

# 4. Formato encuesta ----------------------------------------------------------

df_encuesta <- df_limpio                        %>%
    # Convertir a formato numérico para implementar diseño de encuesta
    # Nota: Como se trata de variables tipo factor, primero se pasa a caracter.
    mutate_at(
        .vars = c("ID_PER", "EST_DIS", "FPC", "FAC_PER"), 
        ~as.numeric(as.character(.)))           %>% 
    # Diseño de encuesta
    as_survey_design(
        ids = ID_PER, strata = EST_DIS, weights = FAC_PER, fpc = FPC)  


# 5. Gráficas ------------------------------------------------------------------

## 5.00. Tema ------------------------------------------------------------------

# ---- Tema para gráficas 
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
v_colors_blue       <- c("#e9d5da", "#827397", "#4d4c7d", "#363062")
v_colors_red        <- c("#e23e57", "#88304e", "#522546", "#311d3f")
v_colors            <- c("#e23e57", "#88304e", "#522546", "#311d3f", 
                         "#e9d5da", "#827397", "#4d4c7d", "#363062")

# ----Formato para gráficas
v_formato <- ".png"

# ---- Etiquetas
v_caption       <- "\nFuente: Encuesta Nacional de Población Privada de la Libertad 2021 (ENPOL). 
Datos procesados por Intersecta (intersecta.org)\n"

v_percent       <- "\nPorcentaje"
v_empty         <- ""
v_sexo          <- "Por sexo\n"


## 5.01. Edad ------------------------------------------------------------------

v_title     <- "En México, a las personas en prisión preventiva,\n¿qué edad tienen?\n"

# Con paquete srvyr (estimar porcentaje)
df_data <- df_encuesta                      %>%
    srvyr::group_by(g_edad)                 %>%
    # Estimar el porcentaje de personas con intervalos de confianza
    srvyr::summarise(
        porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95))

# Gráfica 
ggplot(df_data,
       # Coordenadas y geoms
       aes(x = g_edad, y = porcentaje)) +
    geom_col(fill = "#4d4c7d") +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = g_edad),
               position = position_stack(1), size=2, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = "¿Qué edad tienen las personas que están\nen prisión preventiva en México?", 
        subtitle = v_empty, 
        x        = "\nGrupo quinquenal", 
        y        = v_percent,
        caption  = v_caption) +
    # Tema 
    guides(fill = "none") +
    tema +
    scale_x_discrete(labels = scales::wrap_format(15)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(axis.text.x = element_text(angle = 0),
          axis.title.x = element_text(angle = 0))

ggsave(
    file = paste_figs("04_edad_persona_pp.png"), 
    device = "png", type = "cairo", 
    width = 6, height = 4)

## 5.02. Escolaridad -----------------------------------------------------------

v_title     <- "¿Cuál es la escolaridad de las personas\nen prisión preventiva en México?"

# Con paquete srvyr (estimar porcentaje)
df_data <- df_encuesta                      %>%
    srvyr::group_by(escolaridad)            %>%
    # Estimar el porcentaje de personas con intervalos de confianza
    srvyr::summarise(
        porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95))

# Gráfica 
ggplot(df_data,
       # Coordenadas y geoms
       aes(x = porcentaje, y = factor(escolaridad, 
                                      levels = c("Otros",
                                                 "Maestría o doctorado",
                                                 "Licenciatura",
                                                 "Normal básica",
                                                 "Carrera técnica con preparatoria",
                                                 "Carrera técnica con secundaria",
                                                 "Preparatoria o bachillerato",
                                                 "Secundaria",
                                                 "Primaria",
                                                 "Preescolar",
                                                 "Ninguno")))) +
    geom_col(fill = "#4d4c7d") +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = escolaridad),
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
    tema +
    guides(fill = "none") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(25)) +
    theme(axis.text.x = element_text(angle = 0),
          axis.text.y = element_text(size = 5.5),
          axis.title.x = element_text(angle = 0))

ggsave(
    file = paste_figs("05_escolaridad_personas_pp.png"), 
    device = "png", type = "cairo", 
    width = 6, height = 4)

## 5.03. Ingresos --------------------------------------------------------------

v_title     <- "¿Cuánto ganaron en el mes previo a su detención las\npersonas en prisión preventiva en México?"

# Con paquete srvyr (estimar porcentaje)
df_data <- df_encuesta                      %>%
    srvyr::group_by(sexo, ingresos)         %>%
    # Estimar el porcentaje de personas con intervalos de confianza
    srvyr::summarise(
        prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
    # mutate(total = round(n)) %>%
    rename(porcentaje = prop) %>% 
    mutate(ingresos = factor(ingresos, 
                             levels = c("Otros",
                                        "Más de $11,000",
                                        "De $9,001 a $11,000",
                                        "De $7,501 a $9,000",
                                        "De $5,501 a $7,500",
                                        "De $3,000 a $5,500",
                                        "Menos de $3,000",
                                        "No recibía ingresos")))

# Gráfica 
ggplot(df_data,
       # Coordenadas y geoms
       aes(x = porcentaje, y = ingresos)) +
    geom_col(aes(fill = sexo)) +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = ingresos),
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
    tema +
    guides(fill = "none") +
    scale_fill_manual(values = v_colors_blue) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.3)) +
    scale_y_discrete(labels = scales::wrap_format(25)) +
    theme(axis.text.x = element_text(angle = 0),
          axis.title.x = element_text(angle = 0),
          axis.text.y = element_text(size = 5.5)) +
    facet_grid(~sexo)

ggsave(
    file = paste_figs("06_ingresos_personas_pp.png"), 
    width = 6, height = 4)

## 5.04. Acceso a servicios (P6_10) --------------------------------------------

v_title <- "En México, ¿qué les proporciona el centro penitenciario\na las personas en prisión preventiva?"

v_codes <- c("P6_10_01", "P6_10_02", "P6_10_03", "P6_10_04", "P6_10_05", "P6_10_06", 
             "P6_10_07", "P6_10_08", "P6_10_08A", "P6_10_09")
v_tipos <- c("Servicios médicos", "Servicios dentales", "Servicios psicológicos", 
             "Medicamentos", "Ropa", "Calzado", "Cobijas", "Artículos de limpieza personal",
             "Toallas sanitarias", "Artículos de aseo personal")

# Con paquete srvyr (estimar porcentaje)
df_data <- df_encuesta                                                      %>%
    rename(respuesta = v_codes[1])                                          %>%
    select(respuesta)                                                       %>%
    mutate(respuesta = codificar_servicios(as.character(respuesta)))        %>%
    srvyr::group_by(respuesta)                                              %>%
    srvyr::summarise(
        total = survey_total(),
        porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95))  %>% 
    mutate(tipo = v_tipos[1])

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>%
        rename(respuesta = v_codes[i])                                      %>%
        select(respuesta)                                                   %>%
        mutate(respuesta = codificar_servicios(as.character(respuesta)))    %>%
        srvyr::group_by(respuesta)                                          %>%
        srvyr::summarise(
            total = survey_total(),
            porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
        mutate(tipo = v_tipos[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

df_data <- df_data %>% 
    filter(respuesta == "Sí")

# Gráfica
ggplot(df_data, aes(x = porcentaje, y = reorder(tipo, porcentaje), fill = "#118ab2")) +
    geom_col(fill = "#4d4c7d")+ 
    geom_label(aes(
        label=paste0(round(porcentaje,3)*100, "%"), group = tipo),
        position = position_stack(1), size=2.5, hjust=.5, vjust=.5, 
        angle = 0, fill = "white",
        color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_empty, 
        x        = v_empty, 
        y        = v_empty,
        fill     = v_empty,
        caption  = paste0("\n", v_caption)) +
    # Tema 
    tema +
    guides(fill = "none") +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_discrete(labels = scales::wrap_format(20)) +
    theme(axis.text.x = element_text(angle = 0))

ggsave(
    file = paste_figs("07_servicios_personas_pp.png"), 
    width = 6, height = 4, device = "png", type = "cairo")

## 5.05. Tipo de discriminación (P7_46) ----------------------------------------

v_title <- "Motivos por los que las personas en prisión preventiva 
han tenido problemas de trato diferente, rechazo o maltrato"


v_codes <- c("P7_46_01", "P7_46_02", "P7_46_03", "P7_46_04", "P7_46_05", "P7_46_06", 
             "P7_46_07", "P7_46_08", "P7_46_09", "P7_46_10", "P7_46_11", "P7_46_12",
             "P7_46_13")
v_tipod <- c("Edad", "Color de piel", "Otros rasgos físicos", "Enfermedad", "Discapacidad",
             "Lengua o idioma", "Ser indígena o afrodescendiente", "Orientación sexual",
             "Identidad de género", "Religión", "Situación económica", 
             "Delito cometido", "Por haber pertenecido a las fuerzas armadas")

# ---- Por sexo

df_data <- df_encuesta                                                      %>%
    filter(discriminacion == "Sí")                                          %>%
    rename(respuesta = v_codes[1])                                          %>%
    select(sexo, respuesta)                                                 %>%
    mutate(respuesta = codificar_siono4o(as.character(respuesta)))          %>%
    srvyr::group_by(sexo, respuesta)                                        %>%
    srvyr::summarise(
        total = survey_total(),
        porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95))  %>%
    mutate(tipo = v_tipod[1])

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                                 %>%
        filter(discriminacion == "Sí")                                          %>%
        rename(respuesta = v_codes[i])                                          %>%
        select(sexo, respuesta)                                                 %>%
        mutate(respuesta = codificar_siono4o(as.character(respuesta)))          %>%
        srvyr::group_by(sexo, respuesta)                                        %>%
        srvyr::summarise(
            total = survey_total(),
            porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95))  %>% 
        mutate(tipo = v_tipod[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

df_data <- df_data %>% 
    filter(respuesta == "Sí")

ggplot(df_data, aes(x = porcentaje, y = reorder(tipo, porcentaje))) +
    geom_col(aes(fill = sexo))+ 
    geom_label(aes(
        label=paste0(round(porcentaje,3)*100, "%"), group = tipo),
        position = position_stack(1), size=1.5, hjust=.5, vjust=.5, 
        angle = 0, fill = "white",
        color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = "Por sexo", 
        x        = v_empty, 
        y        = v_empty,
        fill     = v_empty,
        caption  = paste0("\n", v_caption)) +
    # Tema 
    tema +
    guides(fill = "none") +
    scale_fill_manual(values = v_colors_blue) +
    scale_x_continuous(labels = scales::percent_format(), limits = c(0, 0.42)) +
    scale_y_discrete(labels = scales::wrap_format(30)) +
    theme(axis.text.x = element_text(angle = 0), 
          plot.title = element_text(size = 12)) +
    theme(axis.text.y = element_text(size = 5), 
    ) +
    facet_grid(~sexo)


ggsave(
    file = paste_figs("09_motivos_discriminación_personas_pp.png"), 
    width = 6, height = 4, device = "png", type = "cairo")


## 5.06. Violencia (P7_40) -----------------------------------------------------

v_title <- "Situaciones de violencia experimentadas por\nlas personas en prisión preventiva en México"

v_codes <- c("P7_40_1", "P7_40_2", "P7_40_3", "P7_40_4", "P7_40_5", "P7_40_6")

v_tipos <- c("Robado objetos personales", 
             "Amenazado o presionado para exigirle dinero o bienes", 
             "Amenazado o presionado para que hiciera algo o dejara de hacerlo", 
             "Provocado alguna lesión física", 
             "Agredido mediante hostigamiento sexual, manoseo, exhibicionismo o intento de violación",
             "Obligado mediante violencia física o amenaza a tener una actividad sexual")

# Con paquete srvyr (estimar porcentaje)
df_data <- df_encuesta                                                      %>%
    rename(respuesta = v_codes[1])                                          %>%
    select(sexo, respuesta)                                                 %>%
    mutate(respuesta = codificar_siono4o(as.character(respuesta)))          %>%
    srvyr::group_by(sexo, respuesta)                                        %>%
    srvyr::summarise(
        total = survey_total(),
        porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95))  %>%                                 
    mutate(tipo = v_tipos[1])

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>%
        rename(respuesta = v_codes[i])                                      %>%
        select(sexo, respuesta)                                             %>%
        mutate(respuesta = codificar_siono4o(as.character(respuesta)))      %>%
        srvyr::group_by(sexo, respuesta)                                    %>%
        srvyr::summarise(
            total = survey_total(),
            porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
        mutate(tipo = v_tipos[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

df_data <- df_data %>% 
    filter(respuesta == "Sí")

# Gráfica 
ggplot(df_data, aes(x = porcentaje, y = reorder(tipo, porcentaje))) +
    geom_col(aes(fill = sexo))+ 
    geom_label(aes(
        label=paste0(round(porcentaje,3)*100, "%"), group = tipo),
        position = position_stack(1), size=2, hjust=.5, vjust=.5, 
        angle = 0, fill = "white",
        color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = "Por sexo", 
        x        = v_empty, 
        y        = v_empty,
        fill     = v_empty,
        caption  = paste0("\n", v_caption)) +
    # Tema 
    tema +
    guides(fill = "none") +
    # Escalas
    scale_fill_manual(values = v_colors_blue) +
    scale_x_continuous(labels = scales::percent_format(), limits = c(0, 0.42)) +
    scale_y_discrete(labels = scales::wrap_format(28)) +
    # Más temas
    theme(axis.text.x = element_text(angle = 0)) +
    facet_grid(~sexo)

ggsave(
    file = paste_figs("10_violencia_personas_pp.png"), 
    width = 6, height = 4, device = "png", type = "cairo")

## 5.07. Lugar donde reside la visita (P7_29) ----------------------------------

v_title <- "Residencia de las personas que constantemente visitan a 
las personas en prisión preventiva en México"

# Con paquete srvyr (estimar porcentaje)
df_data <- df_encuesta                                                  %>% 
    filter(P7_25 == 1)                                                  %>% 
    srvyr::group_by(lugar)                                              %>% 
    srvyr::summarise(
        porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95))

# Gráfica
ggplot(df_data, 
       aes(x = reorder(lugar, porcentaje),
           y = porcentaje)) +
    geom_col(fill = "#4d4c7d") +
    geom_label(aes(group = lugar,
                   label = paste0(round(porcentaje,3)*100, "%"), 
                   family = "Fira Sans"),
               size = 2) +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_empty, 
        y        = v_empty, 
        x        = v_empty,
        fill     = v_empty,
        caption  = paste0("\n", v_caption)) +
    # Tema 
    tema +
    guides(fill = "none") +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_discrete(labels = scales::wrap_format(15)) +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 0))

ggsave(
    file = paste_figs("11_residencia_personas_pp.png"), 
    width = 6, height = 4, device = "png", type = "cairo")

## 5.08. Gasto por visita ------------------------------------------------------

v_title <-  "Monto que gastan las personas por cada visita que realizan 
a personas en prisión preventiva en México"

# Con paquete srvyr (estimar porcentaje)
df_data <- df_encuesta                          %>% 
    filter(P7_25 == 1)                          %>% 
    filter(!gasto %in% c(99999999, 99999998))   %>% 
    mutate(gasto = as.integer(gasto))           %>% 
    mutate(gasto_clas = case_when(
        (gasto <= 100) ~ "Menos de $100",
        (gasto > 100   & gasto <= 500) ~ "De $101 a $500",
        (gasto > 500   & gasto <= 1000) ~ "De $501 a $1,000",
        (gasto > 1000  & gasto <= 5000) ~ "De $1,001 a $5,000",
        (gasto > 5000  & gasto <= 10000) ~ "Más de $5,000",
        (gasto > 10000 & gasto <= 20000) ~ "Más de $5,000",
        (gasto > 20000) ~ "Más de $5,000"
    )) %>% 
    srvyr::group_by(gasto_clas)                                          %>% 
    srvyr::summarise(
        porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
    mutate(gasto_clas = factor(gasto_clas, 
                               levels = c(
                                   "Menos de $100",
                                   "De $101 a $500",
                                   "De $501 a $1,000",
                                   "De $1,001 a $5,000",
                                   "Más de $5,000"
                               )))

# Gráfica
ggplot(df_data, aes(x = gasto_clas, y = porcentaje)) +
    geom_col(fill = "#4d4c7d") +
    geom_label(aes(
        label=paste0(round(porcentaje,3)*100, "%"), group = gasto_clas),
        position = position_stack(1), size=2, hjust=.5, vjust=.5, 
        angle = 0, fill = "white",
        color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_empty, 
        y        = v_empty, 
        x        = "\nPesos mexicanos",
        fill     = v_empty,
        caption  = v_caption) +
    # Tema 
    tema +
    guides(fill = "none") +
    scale_fill_manual(values = v_colors_blue) +
    theme(axis.text.x = element_text(angle = 0), 
          axis.title.x = element_text(angle = 0)) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_discrete(labels = scales::wrap_format(15))

ggsave(
    file = paste_figs("12_gasto_visitas.png"), 
    width = 6, height = 4, device = "png", type = "cairo")

## 5.09. Ayuda proporcionada por la visita  (P7_28) ----------------------------

v_title <- "Artículos proporcionados por visitas a 
las personas en prisión preventiva en México"

v_codes <- c("P7_28_01", "P7_28_02", "P7_28_03", "P7_28_04", "P7_28_05", 
             "P7_28_06", "P7_28_07", "P7_28_08", "P7_28_09")

v_tipos <- c("Comida", "Ropa", "Zapatos", "Dinero", "Medicinas", 
             "Vender productos que elabora", "Material para su trabajo",
             "Artículos de higiene personal", "Nada")

# Con paquete srvyr (estimar porcentaje)
df_data <- df_encuesta                                                      %>%
    filter(P7_25 == 1)                                                      %>% 
    rename(respuesta = v_codes[1])                                          %>%
    select(respuesta)                                                       %>%
    mutate(respuesta = codificar_siono2(as.character(respuesta)))           %>%
    srvyr::group_by(respuesta)                                              %>%
    srvyr::summarise(
        total = survey_total(),
        porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95))  %>%                                 
    mutate(tipo = v_tipos[1])

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>%
        filter(P7_25 == 1)                                                  %>% 
        rename(respuesta = v_codes[i])                                      %>%
        select(respuesta)                                                   %>%
        mutate(respuesta = codificar_siono2(as.character(respuesta)))       %>%
        srvyr::group_by(respuesta)                                          %>%
        srvyr::summarise(
            total = survey_total(),
            porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
        mutate(tipo = v_tipos[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

df_data <- df_data %>% 
    filter(respuesta == "Sí")

# Gráfica
ggplot(df_data, aes(x = porcentaje, y = reorder(tipo, porcentaje))) +
    geom_col(fill = "#4d4c7d")+ 
    geom_label(aes(
        label = paste0(round(porcentaje,3)*100, "%"), group = tipo),
        position = position_stack(1), size=2, hjust=.5, vjust=.5, 
        angle = 0, fill = "white",
        color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_empty, 
        x        = v_empty, 
        y        = v_empty,
        fill     = v_empty,
        caption  = paste0("\n", v_caption)) +
    # Tema 
    tema +
    guides(fill = "none") +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_discrete(labels = scales::wrap_format(20)) +
    theme(axis.text.x = element_text(angle = 0))

ggsave(
    file = paste_figs("13_articulos_visitas.png"), 
    width = 6, height = 4, device = "png", type = "cairo")
 
## 5.10. Tiempos de espera sin sentencia ---------------------------------------

df_tiempo <- df_encuesta                                                %>%
    # Filtrar y dejar solo personas sin sentencias
    srvyr::filter(!(P5_3 %in% c(3)))                                    %>% 
    # Quitar personas que desconocen el año de la detención
    srvyr::filter(
        !(P3_5_A %in% c(0000, 9998, 9999)))                             %>% 
    # Quitar personas que desconocen el mes de la detención
    srvyr::filter(
        !(P3_5_M %in% c(00, 98, 99)))                                   %>% 
    # Estimar tiempo entre detención y sentencia 
    srvyr::mutate(
        # Fecha detención
        fecha_detencion = paste(P3_5_A, P3_5_M, "01", sep = "-"),
        fecha_detencion = as.Date(fecha_detencion, format = "%Y-%m-%d"), 
        # Fecha sentencia
        fecha_encuesta = as.Date("2021-07-01", format = "%Y-%m-%d"), 
        # Tiempo de espera 
        days  = as.numeric(
            difftime(fecha_encuesta, fecha_detencion, units = "days")),
        weeks = round(
            as.numeric(difftime(fecha_encuesta, fecha_detencion, units = "weeks")))
    ) %>% 
    # Quitar casos donde la diferencia sea negativa 
    srvyr::filter(days >= 0)

# Guardar copia con nombre completo para procesamiento posterior
df_tiempo_sin_sentencia <- df_tiempo

# Guardar códigos de las variables de delitos por los que fueron detenidas (no sentenciadas)
v_codes <- names(df_encuesta$variables %>% select(starts_with("P5_31_")))

# Crear vectores con el nombre del delito 
v_delitos <- c(
    "Robo de vehículo",
    "Robo a casa habitación",
    "Robo a negocios",
    "Robo en transporte público",
    "Robo a transeúnte",
    "Robo de autopartes",
    "Otros robos",
    "Posesión ilegal de drogas", 
    "Comercio ilegal de drogas",
    "Lesiones",
    "Homicidio culposo",
    "Homicidio doloso",
    "Portación ilegal de armas",
    "Incumplimiento de obl. de asistencia familiar",
    "Violencia familiar",
    "Daño a la propiedad",
    "Secuestro",
    "Violación",
    "Fraude",
    "Delincuencia organizada",
    "Otros delitos sexuales",
    "Extorsión",
    "Privación de la libertad",
    "Abuso de confianza",
    "Amenazas",
    "Otros delitos")

# Ensayo 
df_data <- df_tiempo                                                        %>% 
    rename(code_delito = v_codes[1])                                        %>% 
    mutate(code_delito = as.numeric(as.character(code_delito)))             %>% 
    filter(code_delito == 1)                                                %>%
    srvyr::summarise(   
        mean_days = survey_mean(days))                                      %>% 
    mutate(delito = v_delitos[1])    


# Bucle
for(i in 2:26){
    print(paste("Vuelta", i, "de", 26))
    
    df_loop <- df_tiempo                                                    %>% 
        rename(code_delito = v_codes[i])                                    %>% 
        filter(code_delito == 1)                                            %>%
        srvyr::summarise(
            mean_days = survey_mean(days))                                  %>% 
        mutate(delito = v_delitos[i])    
    
    df_data <- df_data %>% bind_rows(df_loop)
}

# -Robo (si se puede integrar una con todos los robos)
# -Posesión ilegal de drogas
# -Comercio ilegal de drogas
# -Homicidio doloso
# -Portación ilegal de armas
# -Secuestro y secuestro exprés
# -Delincuencia organizada

# Limpieza final para la gráfica 
v_delitos_interes <- c(
    "Robo", "Posesión ilegal de drogas", "Comercio ilegal de drogas", 
    "Homicidio doloso", "Portación ilegal de armas", "Secuestro", 
    "Delincuencia organizada")

df_plot <- df_data                                                          %>% 
    mutate(delito = case_when(
        str_detect(delito, "robo") | str_detect(delito, "Robo") ~ "Robo", 
        T ~ delito))                                                        %>% 
    filter(delito %in% v_delitos_interes) %>% 
    group_by(delito) %>% 
    summarise(mean_days = mean(mean_days)) %>% 
    filter(!(delito %in% c("Posesión ilegal de drogas", "Robo")))

# ---- Texto
v_title <- "Las personas en prisión preventiva en México, 
¿cuánto tiempo llevan esperando sentencia?"
v_days  <- "\nDías de espera promedio, acumulados hasta el levantamiento de la ENPOL\n"

# 

# ---- Gráfica de barras
ggplot(
    # Datos
    df_plot, 
    aes(x = mean_days, y = reorder(delito, mean_days))) +
    #Geoms
    geom_col(fill = v_colors_blue[4]) +
    geom_vline(xintercept = (365*1),   linetype = "dashed", color = "black", alpha = 0.2) +
    geom_vline(xintercept = (365*2),   linetype = "dashed", color = "black", alpha = 0.2) +
    geom_vline(xintercept = (365*3),   linetype = "dashed", color = "black", alpha = 0.7) +
    geom_vline(xintercept = (365*4)+1, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_vline(xintercept = (365*5)+1, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_vline(xintercept = (365*6)+1, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_vline(xintercept = (365*7)+1, linetype = "dashed", color = "black", alpha = 0.2) +
    geom_vline(xintercept = (365*8)+2, linetype = "dashed", color = "black", alpha = 0.2) +
    annotate("text", x = -120+(365*1), y = 0.25, label = "1 año" , size = 2.5, family = "Fira Sans", color = "#666666") +
    annotate("text", x = -120+(365*2), y = 0.25, label = "2 años", size = 2.5, family = "Fira Sans", color = "#666666") +
    annotate("text", x = -120+(365*3), y = 0.25, label = "3 años", size = 2.5, family = "Fira Sans", color = "#666666") +
    annotate("text", x = -50 +(365*4), y = 5.8 , label = "Tiempo promedio", size = 2.5, family = "Fira Sans", color = "black", alpha = 0.7) +
    annotate("text", x = -120+(365*4), y = 0.25, label = "4 años", size = 2.5, family = "Fira Sans", color = "#666666") +
    annotate("text", x = -120+(365*5), y = 0.25, label = "5 años", size = 2.5, family = "Fira Sans", color = "#666666") +
    annotate("text", x = -120+(365*6), y = 0.25, label = "6 años", size = 2.5, family = "Fira Sans", color = "#666666") +
    annotate("text", x = -120+(365*7), y = 0.25, label = "7 años", size = 2.5, family = "Fira Sans", color = "#666666") +
    annotate("text", x = -120+(365*8), y = 0.25, label = "8 años", size = 2.5, family = "Fira Sans", color = "#666666") +
    geom_label(aes(label = scales::comma(round(mean_days))), size = 2, family = "Fira Sans") +
    # Títulos 
    labs(
        title    = v_title, 
        subtitle = "Por delito\n", 
        x        = v_days, 
        y        = v_empty,
        fill     = v_empty,
        caption  = v_caption
    ) +
    # Escalas
    scale_x_continuous(
        breaks = seq(0, 3000, 200), 
        label = scales::comma_format()) +
    scale_y_discrete(labels = scales::wrap_format(20))+
    expand_limits(y = c(0, 6)) +
    # Tema
    tema +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# ---- Guardar
ggsave(
    file = paste_figs("18_tiempo_sinsentencia.png"), 
    width = 6, height = 4, device = "png", type = "cairo")

# FIN. -------------------------------------------------------------------------
♠