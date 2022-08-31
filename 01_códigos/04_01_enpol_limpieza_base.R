#------------------------------------------------------------------------------#
# Proyecto:              AMICUS para la SCJN sobre prisión preventiva oficiosa  
# Objetivo:              Juntar bases la Encuesta Nacional de Población 
#                        Privada de la Libertad (ENPOL) 2021
#
# Encargadas:            Coordinación de datos de Intersecta
# 
# Fecha de creación:     12 de enero  de 2021 (códigos originales)
# Última actualización:  31 de agosto de 2022 (código amicus)
#------------------------------------------------------------------------------#

# Fuente: https://www.inegi.org.mx/programas/enpol/2021/#Microdatos

# 0. Configuración inicial -----------------------------------------------------

# ---- Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# ---- Cargar librerías 
require(pacman)
p_load(
    foreign, readxl, tidyverse, dplyr, lubridate, zoo, beepr)

# ---- Limpiar espacio de trabajo 
rm(list=ls())

# ---- Método para el caso de 1 UPM en los estratos
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

# ---- Funciones de directorios
paste_code  <- function(x){paste0("01_códigos/04_enpol/"      , x)}
paste_inp   <- function(x){paste0("02_datos_crudos/04_enpol/" , x)}
paste_data  <- function(x){paste0("03_datos_limpios/04_enpol/", x)}
paste_figs  <- function(x){paste0("04_figuras/"               , x)}


# 1. Cargar datos --------------------------------------------------------------

## 1.1. ENPOL 2021 -------------------------------------------------------------

# ---- Cargar todos los dbs
df_raw2021_1 <- read.dbf(paste_inp("ENPOL2021_2_3.dbf"))
df_raw2021_2 <- read.dbf(paste_inp("ENPOL2021_4.dbf"))
df_raw2021_3 <- read.dbf(paste_inp("ENPOL2021_5.dbf"))
df_raw2021_4 <- read.dbf(paste_inp("ENPOL2021_6.dbf"))
df_raw2021_5 <- read.dbf(paste_inp("ENPOL2021_7.dbf"))
df_raw2021_6 <- read.dbf(paste_inp("ENPOL2021_8_9_10_11.dbf"))
df_raw2021_7 <- read.dbf(paste_inp("ENPOL2021_SOC.dbf"))


# 2. Procesar datos ------------------------------------------------------------

## 2.1. Unificar bases 2021 ----------------------------------------------------

# ---- Variables que conforman la llave personal 
v_llave         <- c("ID_PER" , "CVE_ENT", "NOM_ENT", "CEN_INT", 
                     "NOM_INT", "SEXO"   , "FUERO")

# ---- Varibales muestrales 
v_expansion     <- c("EST_DIS", "FPC", "FAC_PER")

# ---- Unir bases
df_unida2021    <- df_raw2021_1                             %>% 
    full_join(df_raw2021_2, by = c(v_llave, v_expansion))   %>% 
    full_join(df_raw2021_3, by = c(v_llave, v_expansion))   %>% 
    full_join(df_raw2021_4, by = c(v_llave, v_expansion))   %>% 
    full_join(df_raw2021_5, by = c(v_llave, v_expansion))   %>% 
    full_join(df_raw2021_6, by = c(v_llave, v_expansion))   %>% 
    full_join(df_raw2021_7, by = c(v_llave, v_expansion))   %>%
    mutate(year = 2021)

dim(df_unida2021) # Las mismas 61,449 observaciones

## 2.2. Codificar valores ------------------------------------------------------


# ---- Codificar indicador de "delitos graves" que ameritan PPO 
df_codificada <- df_unida2021                                           |> 
    # Cambiar formato de 
    mutate(across(starts_with("P5_11_"), ~as.numeric(as.character(.)))) |>
    mutate(across(starts_with("P5_12_"), ~as.numeric(as.character(.)))) |>
    mutate(across(starts_with("P5_13_"), ~as.numeric(as.character(.)))) |>
    mutate(across(starts_with("P5_31_"), ~as.numeric(as.character(.)))) |>
    mutate(across(starts_with("P5_32_"), ~as.numeric(as.character(.)))) |>
    mutate(across(starts_with("P5_33_"), ~as.numeric(as.character(.)))) |>
    mutate(
        year_arresto = as.numeric(as.character(P3_5_A)), 
        sentencia = if_else(P5_3  == "3", 1, 0),
        ppo = case_when(
            # ---- Delitos que ameritan PPO desde 2011 (con sentencia)
            (year_arresto >= 2011) & (sentencia == 1) & (
                # Delitos contra la salud (fuero federal)
                (P5_11_09 == 1 & P5_13_1 == 1) |
                    # Delincuencia organizada 
                    (P5_11_20 == 1) |
                    # Homicidio 
                    (P5_11_11 == 1 | P5_11_12 == 1) |
                    # Secuestro/privación de la libertad
                    (P5_11_17 == 1 | P5_11_23 == 1) |
                    # Trata de personas 
                    (P5_11_20 & P5_12_05 == 1) |
                    # Violación
                    (P5_11_18 == 1) 
            ) ~ 1, 
            # ---- Delitos que ameritan PPO desde 2011 (sin sentencia)
            (year_arresto >= 2011) & (sentencia == 0) & (
                # Delitos contra la salud (fuero federal)
                (P5_31_09 == 1 & P5_33_1 == 1) |
                    # Delincuencia organizada 
                    (P5_31_20 == 1) |
                    # Homicidio 
                    (P5_31_11 == 1 | P5_31_12 == 1) |
                    # Secuestro/privación de la libertad
                    (P5_31_17 == 1 | P5_31_23 == 1) |
                    # Trata de personas 
                    (P5_31_20 & P5_32_05 == 1) |
                    # Violación
                    (P5_31_18 == 1) 
            ) ~ 1, 
            # ---- Delitos que ameritan PPO desde 2016 (con sentencia)
            (year_arresto >= 2016) & (sentencia == 1) & (
                # Comercio de narcóticos FC | Comercio de narcóticos FF
                (P5_11_09 == 1 ) |
                    # Posesión con fines de comercio o suministro de naróticos
                    (P5_11_08 == 1) 
                # EN LA ENPOL solo hay dos delitos relacionados con drogas
                # Posesión de narcóticos
                # () |
                # Producción de narcóticos
                # () |
                # Suministro de narcóticos FC
                # () |
                # Suministro de narcóticos FF
                # () |
                # Tráfico de narcóticos
                # () |
                # Transporte de narcóticos
                # () |
            ) ~ 1, 
            # ---- Delitos que ameritan PPO desde 2016 (sin sentencia)
            (year_arresto >= 2016) & (sentencia == 0) & (
                # Comercio de narcóticos FC | Comercio de narcóticos FF
                (P5_31_09 == 1 ) |
                    # Posesión con fines de comercio o suministro de naróticos
                    (P5_31_08 == 1) 
                # EN LA ENPOL solo hay dos delitos relacionados con drogas
                # Posesión de narcóticos
                # () |
                # Producción de narcóticos
                # () |
                # Suministro de narcóticos FC
                # () |
                # Suministro de narcóticos FF
                # () |
                # Tráfico de narcóticos
                # () |
                # Transporte de narcóticos
                # () |
            ) ~ 1, 
            # ---- Delitos que ameritan PPO desde 2019 (con sentencia)
            (year_arresto >= 2019) & (sentencia == 1) & (
                # Feminicidio (no hay feminicidio en la ENPOL)
                # () |
                # Robo a casa habitación 
                (P5_11_02 == 1) |
                    # Delitos electorales
                    # () |
                    # Enriquecimiento ilícito 
                    # () |
                    # Ejercicio abusivo de funciones 
                    # () |
                    # Robo a transportistas 
                    # () |
                    # Hidrocarburos y sus derivados 
                    # () |
                    # Desaparición forzada de de personas 
                # () |
                # Armas de fuego
                (P5_11_13 == 1)     
            ) ~ 1, 
            # ---- Delitos que ameritan PPO desde 2016 (sin sentencia)
            (year_arresto >= 2019) & sentencia == 0 & (
                # Feminicidio (no hay feminicidio en la ENPOL)
                # () |
                # Robo a casa habitación 
                (P5_31_02 == 1) |
                    # Delitos electorales
                    # () |
                    # Enriquecimiento ilícito 
                    # () |
                    # Ejercicio abusivo de funciones 
                    # () |
                    # Robo a transportistas 
                    # () |
                    # Hidrocarburos y sus derivados 
                    # () |
                    # Desaparición forzada de de personas 
                # () |
                # Armas de fuego
                (P5_31_13 == 1)     
            ) ~ 1, 
            
            # ---- DEMÁS CASOS (no ameritan PPO)
            T ~ 0))

# Controles de calidad
# dim(df_codificada)
# table(df_codificada$year_arresto)
# table(df_codificada$ppo)
# table(df_codificada$P5_11_01)

# 3. Guardar datos -------------------------------------------------------------

# ---- Guardar base unida 

df_ENPOL_2021 <- df_codificada 

save(df_ENPOL_2021, file = paste_data("df_ENPOL_2021.RData"))

# write_csv(df_ENPOL_2021, file = paste0(out_data, "df_ENPOL_2021.csv"))

# FIN. -------------------------------------------------------------------------