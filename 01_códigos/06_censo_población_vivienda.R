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
p_load(srvyr, tidyverse, data.table)

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

## 2.2. Sí/no -------------------------------------------------------------
codificar_sino <- function(var = x){
    
    v_sino   <- c("Sí", "No", "No especificado")
    
    case_when(
        var == "1" ~ v_sino[1], 
        var == "3" ~ v_sino[2],
        TRUE ~ v_sino[3])
}

## 2.3. Grupos edad -------------------------------------------------------
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

## 2.4. Escolaridad reagrupada --------------------------------------------
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
           indigena           = PERTE_INDIGENA,
           afrodescendiente   = AFRODES,
           `nivel escolar`    = NIVACAD
    )

## 3.2. Usar las funciones para recodificar las variables -----------------
df_limpio[, `:=` (
    sexo = codificar_sexo(sexo),
    g_edad = codificar_edad(edad),
    indigena = codificar_sino(indigena),
    afrodescendiente = codificar_sino(afrodescendiente),
    `nivel escolar` = codificar_escolaridad(`nivel escolar`),
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

# 4. Estadísticas ---------------------------------------------------------

## 4.1. Grupos de edad ----------------------------------------------------

df_edad <- df_encuesta       %>% 
    srvyr::group_by(g_edad)  %>% 
    srvyr::summarise(
        porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95)
    )

## 4.2. Escolaridad -------------------------------------------------------

df_escolaridad <- df_encuesta         %>% 
    srvyr::group_by(`nivel escolar`)  %>% 
    srvyr::summarise(
        porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95)
    )

## 4.3. Autoadscripción a pueblos originarios -----------------------------

df_a_indigena <- df_encuesta   %>% 
    srvyr::group_by(indigena)  %>% 
    srvyr::summarise(
        porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95)
    )

## 4.4. Autoadscripción afrodescendiente ----------------------------------

df_a_afrodescendiente <- df_encuesta    %>% 
    srvyr::group_by(afrodescendiente)   %>% 
    srvyr::summarise(
        porcentaje = survey_mean(na.rm = T, vartype = "ci", level = 0.95)
    )