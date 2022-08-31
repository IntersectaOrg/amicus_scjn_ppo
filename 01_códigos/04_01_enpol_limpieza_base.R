#------------------------------------------------------------------------------#
# Proyecto:                   ENPOL 2021
# Objetivo:                   Importar y unificar datos crudos de la ENPOL
#
# Encargadas:                 Regina Isabel Medina
# Correo:                     rmedina@intersecta.org
# Fecha de creación:          12 de enero de 2022
# Última actualización:       13 de junio de 2022
#------------------------------------------------------------------------------#

# Fuente: https://www.inegi.org.mx/programas/enpol/2016/

# URL Drive de los datos de la ENPOL2016 en el drive de  INTR:
# - https://drive.google.com/drive/u/1/folders/1heccHThnKXTZkbVHNvxXFBREox-sZlUx


# URL Drive de los datos de la ENPOL2016 en el drive de  INTR:
# - https://drive.google.com/drive/u/1/folders/1SUjq9_9wAudhNbHaKi1AdGum95D8prAZ

# Revisar unificación de base 2016

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Cargar librerías 
require(pacman)
p_load(
    foreign, readxl, googledrive, googlesheets4, tidyverse, dplyr, lubridate, 
    zoo, beepr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
inp         <- "02_datos_crudos/"
out_data    <- "03_datos_limpios/"
out_figs    <- "04_figuras/"


# Activar las credenciales de google
googledrive::drive_auth("rmedina@intersecta.org")
googlesheets4::gs4_auth("rmedina@intersecta.org")

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# Función para importar de manera más corta desde drive
# imp_dv <- function(x){
#     googlesheets4::read_sheet(
#         paste0("https://docs.google.com/spreadsheets/d/", x))}
# 

# 1. Cargar datos --------------------------------------------------------------

## 1.1. ENPOL 2016 -------------------------------------------------------------

#--- Importar ids de archivos de la carpeta de drive (última parte del URL)
df_files <- drive_ls(as_id("1heccHThnKXTZkbVHNvxXFBREox-sZlUx")) 

#df_files # Las bases .dbf corresponden a los elementos 2 al 7

#--- Importar primera base de ENPOL2021 (prueba)
# Crear archivo temporal 
temp    <- tempfile(fileext = ".dbf") 

# Descargar la base, desde el drive, al archivo temporal
dl      <- drive_download(as_id(df_files$id[2]), path = temp, overwrite = TRUE)

# Importar la base del archivo temporal a RStudio 
df_raw  <- foreign::read.dbf(dl$local_path)


#--- Importar bases en bucle 
for(i in 2:7){
    temp    <- tempfile(fileext = ".dbf")
    dl      <- drive_download(as_id(df_files$id[i]), path = temp, overwrite = TRUE)
    df_raw  <- foreign::read.dbf(dl$local_path)
    assign(paste0("df_raw2016_", i), df_raw)
}


## 1.2. ENPOL 2021 -------------------------------------------------------------

#--- Importar ids de archivos de la carpeta de drive (última parte del URL)
df_files    <- drive_ls(as_id("1SUjq9_9wAudhNbHaKi1AdGum95D8prAZ")) 

#--- Importar bases en bucle 
for(i in 1:7){
    temp    <- tempfile(fileext = ".dbf")
    dl      <- drive_download(as_id(df_files$id[i]), path = temp, overwrite = TRUE)
    df_raw  <- foreign::read.dbf(dl$local_path)
    assign(paste0("df_raw2021_", i), df_raw)
}


# 2. Procesar datos ------------------------------------------------------------

## 2.1. Unificación de bases 2016 ----------------------------------------------

# Variables que conforman la llave personal 
v_llave <- c("ID_PER", "CVE_ENT", "NOM_ENT", "CEN_INT", "NOM_INT", "RESUL_VIV", 
             "SEXO", "FUERO")

# Varibales muestrales 
v_expansion     <- c("EST_DIS", "FPC", "FAC_PER")

# Unir bases
df_unida2016    <- df_raw2016_2                             %>% 
    full_join(df_raw2016_3, by = c(v_llave, v_expansion))   %>% 
    full_join(df_raw2016_4, by = c(v_llave, v_expansion))   %>% 
    full_join(df_raw2016_5, by = c(v_llave, v_expansion))   %>% 
    full_join(df_raw2016_6, by = c(v_llave, v_expansion))   %>% 
    full_join(df_raw2016_7, by = c(v_llave, v_expansion))   %>%
    mutate(year = 2016)

dim(df_unida2016) # Las mismas 58,127 observaciones

# Guardar gráfica unida 
df_ENPOL_2016 <- df_unida2016 
save(df_ENPOL_2016, file = paste0(out_data, "df_ENPOL_2016.RData"))
# write_csv(df_ENPOL_2016, file = paste0(out_data, "df_ENPOL_2016.csv"))


## 2.2. Unificación de bases 2021 ----------------------------------------------

# Variables que conforman la llave personal 
v_llave         <- c("ID_PER" , "CVE_ENT", "NOM_ENT", "CEN_INT", 
                     "NOM_INT", "SEXO"   , "FUERO")

# Varibales muestrales 
v_expansion     <- c("EST_DIS", "FPC", "FAC_PER")

# Unir bases
df_unida2021    <- df_raw2021_1                             %>% 
    full_join(df_raw2021_2, by = c(v_llave, v_expansion))   %>% 
    full_join(df_raw2021_3, by = c(v_llave, v_expansion))   %>% 
    full_join(df_raw2021_4, by = c(v_llave, v_expansion))   %>% 
    full_join(df_raw2021_5, by = c(v_llave, v_expansion))   %>% 
    full_join(df_raw2021_6, by = c(v_llave, v_expansion))   %>% 
    full_join(df_raw2021_7, by = c(v_llave, v_expansion))   %>%
    mutate(year = 2021)

dim(df_unida2021) # Las mismas 61,449 observaciones

## 2.3. Codificar valores ------------------------------------------------------

# load(paste0(out_data, "df_ENPOL_2021.RData"))



# df_unida2021 <- df_ENPOL_2021

# Codificar indicador de "delitos graves" que ameritan PPO 
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

# Guardar gráfica unida 

df_ENPOL_2021 <- df_codificada 

save(df_ENPOL_2021, file = paste0(out_data, "df_ENPOL_2021.RData"))

# write_csv(df_ENPOL_2021, file = paste0(out_data, "df_ENPOL_2021.csv"))

# FIN. -------------------------------------------------------------------------