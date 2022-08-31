#------------------------------------------------------------------------------#
# Proyecto:              AMICUS para la SCJN sobre prisión preventiva oficiosa  
# Objetivo:              Procesar datos y generar figuras de Cuaderno Mensual 
#                        de Información Estadística Penitenciaria Nacional 
#
# Encargadas:            Coordinación de datos de Intersecta
# 
# Fecha de creación:     22 de abril  de 2021 (códigos originales)
# Última actualización:  31 de agosto de 2022 
#------------------------------------------------------------------------------#

# Fuente: 
# https://www.gob.mx/prevencionyreadaptacion/documentos/cuaderno-mensual-de-informacion-estadistica-penitenciaria-nacional?idiom=es

# 0. Configuración inicial -----------------------------------------------------

# ---- Opciones generales 
options(dplyr.summarise.inform = FALSE) # Silenciar mensajes de .group en dplyr
options(scipen=999)                     # Silenciar notación científica

# ---- Librerías 
pacman::p_load(readr, readxl, tidyverse, stringr, janitor, ggrepel, beepr)

# ---- Limpiar espacio de trabajo 
rm(list=ls())

# ---- Funciones con direcciones de las carpetas
paste_code  <- function(x){paste0("01_códigos/"                                  , x)}
paste_inp   <- function(x){paste0("02_datos_crudos/01_cuadernos_estadística_penitenciaria/" , x)}
paste_out   <- function(x){paste0("03_datos_limpios/01_cuadernos_estadística_penitenciaria/", x)}
paste_fig   <- function(x){paste0("04_figuras/"                                  , x)}


# 1. Procesamiento incidentes por estatus jurídico -----------------------------
## 1.1. Limpieza de ensayo -----------------------------------------------------

# Vectores para importar base 
v_years         <- c(2020)
v_month         <- c("01")
v_sheet         <- c("Pág-55-Tab-NII") # Estatal
v_forma         <- c(".xls")

# Importar base
df_crudo        <- read_excel(paste_inp(paste0(v_years, "/", "CE_", v_years, "_", 
                                     v_month, v_forma)), sheet = v_sheet)

v_names         <- names(df_crudo)  # Nombres de las variables de la base cruda

# Renombrar primera variable para facilitar el corte 
df_seleccionado <- df_crudo                         %>%
    rename(situacion = v_names[1])


# Primera observación 
v_head <- which(grepl("Situación Jurídica", df_seleccionado$situacion, ignore.case=TRUE))[2]
v_tail <- which(grepl("total"             , df_seleccionado$situacion, ignore.case=TRUE))

df_obs <- df_seleccionado                           %>% 
    slice(v_head:v_tail)                            %>% 
    remove_empty(which = c("rows", "cols"))         %>%
    row_to_names(row_number = 1)                    

# Nombre de las variables numéricas 
v_numeric   <- names(df_obs)[2:dim(df_obs)[2]]
v_names     <- names(df_obs) 

# Formato largo 
df_long <- df_obs %>% 
    mutate_at(v_numeric, as.numeric)                %>% 
    pivot_longer(cols = v_numeric, 
                 names_to = "evento", 
                 values_to = "total")               %>% 
    mutate(total = coalesce(as.numeric(total), 0))  %>% 
    mutate(year = v_years, mes = v_month, 
           grupo = "Población penitenciaria", nivel = "Federal")

v_names <- names(df_long)
df_long <- df_long %>% rename(situacion = v_names[1])


## 1.2. Limpieza en bucle ------------------------------------------------------
### 1.2.1. Establecer parámetros -----------------------------------------------

# Vectores de conteo para años y meses
v_years     <- c(2000:2021)
v_months    <- c("01", "02", "03", "04", "05", "06", 
                 "07", "08", "09", "10", "11", "12")

# Nombres de las pestañas 

# 2013/02-2019/10: "Pág-44-Tab-NII" || "Pág-43-Tab-IP(2)"
# 2019/11        : "Pág-56-Tab-NII" || "Pág-55-Tab-IP(2)"
# 2019/12        : "Pág-54-Tab-NII" || "Pág-53-Tab-IP(2)"
# 2020/01-2020/03: "Pág-55-Tab-NII" || "Pág-54-Tab-IP(2)"
# 2020/04        : "Pág-56-Tab-NII" || "Pág-0-IP(2)"
# 2020/10-2020/11: "Pág-56-Tab-NII" || "Pág-55-Tab-IPCF"
# 2020/12-2021/02: "Pág-51-Tab-NII" || "Pág-49-Tab-IP(2)"
# 2021/05-2021/09: "Pág-50-Tab-NII"
# 2021/10-2021/11-: "Pág-55-Tab-NII"

v_names_sheet <- c(
    "Pág-44-Tab-NII", "Pág-56-Tab-NII", "Pág-54-Tab-NII", "Pág-55-Tab-NII", 
    "Pág-56-Tab-NII", "Pág-56-Tab-NII", "Pág-51-Tab-NII", "Pág-50-Tab-NII", 
    "Pág-52-Tab-NII", "Pág-53-Tab-NII")

# Periodo de 2010 a 2013
v_sheetG <- c("201006", "201007", "201008", "201009", "201010", 
              "201011", "201012", "201101", "201102", "201103")
v_sheetA <- c("201101", "201102", "201103")
v_sheetB <- c("201104", "201105")
v_sheetC <- c("201201")
v_sheetD <- c("201202")
v_sheetE <- c("201301")

# Periodo de 2012 a 2021
v_sheet1 <- c("201201", "202107")
v_sheet2 <- c("201202", "201203")
v_sheet3 <- c("201911", "202004")
v_sheet4 <- c("201912", "202109")
v_sheet5 <- c("202001", "202002", "202003", "202110", "202111")
v_sheet6 <- c("202010", "202011")
v_sheet7 <- c("202012", "202101", "202106")
v_sheet8 <- c("202102", "202103", "202105")
v_sheet9 <- c("202104")
v_sheetW <- c("202108")


# Archivos con formato (.xlsx): 2005/01-2010/12
v_xlsx <- c("201105", "201106", "201107", "201108", "201110", "201112",
            "201202", "201203", "201204", "201208", "201209", "201210", 
            "201211", "201212", "201301", "201304", "201411", "202111")


### 1.2.2. Ejecutar bucle  -----------------------------------------------------

# Base en blanco para el pegado
df_acumulada <- data.frame()

v_years <- 2015:2021
# v_years <- 2021

for (i in v_years){
    for(x in v_months){
        # Imprimir y guardar el identificador de la base
        # print("Paso 1")
        print(paste(i, x)) 
        id          <- paste0(i, x)
        
        if(id %in% c("201708", "201710", "201804")) next
        
        # Formato del archivo 
        v_format    <- ifelse((id %in% v_xlsx | i %in% 2005:2010), ".xlsx", ".xls")
        v_siglas    <- ifelse(i != 2005, "CE_", "CEP_")
        
        # Establecer la pestaña
        v_sheet <- case_when(
            id %in% v_sheet1 ~ v_names_sheet[1], 
            !(id %in% c(v_sheet3, v_sheet4, v_sheet5, v_sheet6, v_sheet7)) & 
                i %in% 2012:2020 ~ v_names_sheet[1], 
            id %in% v_sheet3 ~ v_names_sheet[2],
            id %in% v_sheet4 ~ v_names_sheet[3],
            id %in% v_sheet5 ~ v_names_sheet[4],
            id %in% v_sheet6 ~ v_names_sheet[5],
            id %in% v_sheet7 ~ v_names_sheet[7], 
            id %in% v_sheet8 ~ v_names_sheet[8],
            id %in% v_sheet9 ~ v_names_sheet[9],
            id %in% v_sheetW ~ v_names_sheet[10]) 
        
        
        # Importar datos
        df_crudo            <- read_excel(
            paste_inp(paste0(i, "/", v_siglas, i, "_", x, v_format)), sheet = v_sheet)
        
        # LIMPIEZA DE LA BASE
        v_names             <- names(df_crudo)  # Nombres de las variables de la base cruda
        
        # Renombrar primera variable para facilitar el corte
        df_seleccionado     <- df_crudo                     %>%
            rename(situacion = v_names[1])
        
        # Primera observación
        if(id %in% c("201501", "201503")){j <- 1} else{j <- 2}
        
        v_head <- which(grepl("Situación Jurídica", df_seleccionado$situacion, ignore.case=TRUE))[j]
        v_tail <- which(grepl("total",              df_seleccionado$situacion, ignore.case=TRUE))
        
        df_obs <- df_seleccionado                           %>%
            slice(v_head:v_tail)                            %>%
            remove_empty(which = c("rows", "cols"))         %>%
            row_to_names(row_number = 1)
        
        # Nombre de las variables numéricas
        v_numeric   <- names(df_obs)[2:dim(df_obs)[2]]
        v_names     <- names(df_obs)
        
        # Formato largo
        df_long <- df_obs                                   %>%
            mutate_at(v_numeric, as.numeric)                %>%
            pivot_longer(cols = v_numeric,          
                         names_to = "evento",           
                         values_to = "total")               %>%
            mutate(total = coalesce(as.numeric(total), 0))  %>%
            mutate(year = i, mes = x,
                   grupo = "Población penitenciaria", nivel = "Federal")
        
        v_names <- names(df_long)
        df_long <- df_long %>% rename(situacion = v_names[1])
        
        df_acumulada <- df_acumulada                        %>%
            bind_rows(df_long)                              %>%
            distinct()
    }
}

## 1.3. Pulir base -------------------------------------------------------------

names(df_acumulada)

table(df_acumulada$year)
table(df_acumulada$mes)
table(df_acumulada$situacion)
table(df_acumulada$evento)
table(df_acumulada$grupo)

table(df_acumulada$year, df_acumulada$mes)


df_incidentes_sitjurid <- df_acumulada                                  %>% 
    filter(!is.na(situacion), situacion != "Total", evento != "Total")  %>% 
    select(year, mes, grupo, nivel, situacion, evento, total)


## 1.4. Guardar base final -----------------------------------------------------

save(df_incidentes_sitjurid, file = paste_out("df_incidentes_sitjurid.RData"))

# 2. Procesamiento incidentes  -------------------------------------------------

## 2.1. Limpieza de ensayo -----------------------------------------------------

### 2.1.1. Limpiar para formato wide ---------------------------------------------

# Vectores para importar base 
v_years             <- c(2020)
v_month             <- c("12")
v_sheet             <- c("Pág-49-Tab-IP(1)") # Estatal
# v_sheet             <- c("Pág-50-Tab-IP(2)") # Federal
v_forma             <- c(".xls")

# Importar base
df_crudo            <- read_excel(paste_inp(paste0(v_years, "/", "CE_", v_years, 
                                                   "_", v_month, v_forma)), 
                                  sheet = v_sheet)

v_names             <- names(df_crudo)  # Nombres de las variables de la base cruda

df_seleccionado     <- df_crudo                     %>%
    rename(evento = v_names[1])

# Primera observación 
v_head <- which(grepl("Intentos de Fuga", df_seleccionado$evento, ignore.case=TRUE)) - 2
v_tail <- which(grepl("total",            df_seleccionado$evento, ignore.case=TRUE)) - 1

df_obs <- df_seleccionado                           %>% 
    slice(v_head:v_tail)                            %>% 
    remove_empty(which = c("rows", "cols")) 

v_names <- names(df_obs)

df_selected <- df_obs                               %>% 
    rename(evento = evento, unidad = v_names[2])    %>% 
    select(-c(v_names[length(v_names)])) 

df_selected$evento[1] <- "evento"
df_selected$unidad[1] <- "unidad"

df_limpio <- df_selected                            %>%
    row_to_names(row_number = 1)                    %>% 
    filter(unidad == "Incidencias")

# Nombre de las variables numéricas 
v_numeric <- names(df_limpio)[3:dim(df_limpio)[2]]

# Formato largo 
df_long <- df_limpio                                %>% 
    mutate_at(v_numeric, as.numeric)                %>% 
    pivot_longer(cols = v_numeric, 
                 names_to = ("entidad"), 
                 values_to = "total")               %>% 
    mutate(total = coalesce(as.numeric(total), 0))  %>% 
    mutate(year = v_years, mes = v_month, 
           grupo = "Población penitenciaria", nivel = "Federal")


### 2.1.2. Limpiar para formato long -------------------------------------------

# Vectores para importar base 
v_years         <- c(2013)
v_month         <- c("01")
v_sheet         <- c("Pág-43-Tab-IP") # Estatal
v_forma         <- c(".xlsx")

# Importar base
df_crudo        <- read_excel(paste_inp(v_years, "/", "CE_", v_years, "_", 
                                     v_month, v_forma), sheet = v_sheet)

df_obs          <- df_crudo                              %>% 
    remove_empty(which = c("rows", "cols")) 

v_names         <- names(df_obs)  # Nombres de las variables de la base cruda

df_renombrado   <- df_obs %>% 
    rename(entidad = v_names[1], 
           intentofuga_incidencia           = v_names[2], 
           intentofuga_involucradas         = v_names[3], 
           intentofuga_heridas              = v_names[4], 
           intentofuga_homicidio            = v_names[5], 
           
           fugas_incidencia                 = v_names[6], 
           fugas_involucradas               = v_names[7], 
           fugas_heridas                    = v_names[8], 
           fugas_homicidio                  = v_names[9], 
           
           motines_incidencia               = v_names[10], 
           motines_involucradas             = v_names[11], 
           motines_heridas                  = v_names[12], 
           motines_homicidio                = v_names[13],
           
           riñas_incidencia                 = v_names[14], 
           riñas_involucradas               = v_names[15], 
           riñas_heridas                    = v_names[16], 
           riñas_homicidio                  = v_names[17], 
           
           intentohomicidio_incidencia      = v_names[18], 
           intentohomicidio_involucradas    = v_names[19], 
           intentohomicidio_heridas         = v_names[20], 
           
           homicidios_incidencia            = v_names[21], 
           homicidios_involucradas          = v_names[22], 
           homicidios_heridas               = v_names[23], 
           
           intentosuicidio_incidencia       = v_names[24], 
           intentosuicidio_involucradas     = v_names[25], 
           
           suicidios_incidencia             = v_names[26], 
           suicidios_involucradas           = v_names[27], 
           
           huelgashambre_incidencia         = v_names[28], 
           huelgashambre_involucradas       = v_names[29],
           
           decesos_incidencia               = v_names[30], 
           decesos_involucradas             = v_names[31], 
           
           intentoviolaciones_incidencia    = v_names[32], 
           intentoviolaciones_involucradas  = v_names[33],
           
           violaciones_incidencia           = v_names[34], 
           violaciones_involucradas         = v_names[35], 
           
           agresioenesterceros_incidencia   = v_names[36], 
           agresioenesterceros_involucradas = v_names[37], 
           
           autoagresiones_incidencia        = v_names[38], 
           autoagresiones_involucradas      = v_names[39], 
           
           total_incidencia                 = v_names[40], 
           total_personas                   = v_names[41]) %>% 
    select(-c(total_incidencia, total_personas))

# Primera observación 
v_head <- which(grepl("Aguascalientes", df_renombrado$entidad, ignore.case=TRUE)) 
v_tail <- which(grepl("ceferepsi",      df_renombrado$entidad, ignore.case=TRUE)) 

v_numeric <- names(df_renombrado)[2:39]

# Formato largo
df_long <- df_renombrado %>%
    slice(v_head:v_tail) %>% 
    mutate_at(v_numeric, as.numeric) %>%
    pivot_longer(cols = v_numeric,
                 names_to = c("evento", "unidad"),
                 names_sep = "_", 
                 values_to = "total") %>%
    mutate(total = coalesce(as.numeric(total), 0)) %>%
    mutate(year = i, mes = x,
           grupo = "Población penitenciaria", nivel = NA)



## 2.2. Limpieza en bucle ------------------------------------------------------
### 2.2.1. Establecer parámetros -----------------------------------------------

# Base en blanco para el pegado
df_acumulada <- data.frame()

# Vectores de conteo para años y meses
v_years     <- c(2013:2021)
v_months    <- c("01", "02", "03", "04", "05", "06", 
                 "07", "08", "09", "10", "11", "12")

# Formato wide: ????-2013/01
# Formato long: 2013/02-present

# Hasta el 2008/03 no hay datos desagregados por centros penitenciarios. 
# Antes de 2008/04 no hay datos de incidentes desagregados por centros penitenciarios


# Nombre de las pestañas donde centros estatatles y federales están juntos 
# 2005/01-2011/03: "pág 38"
# 2009/07        : "pág 36"
# 2010/06:2011/01: "pág 38 "
# 2011/04-2011/05: "pág 39 "
# 2011/06-2011/12: "pág 40 "
# 2012/01        : "pág 41"
# 2012/02: "pág 42"
# 2012/03: "pág 42 "
# 2012/04-2013/01: "Pág-43-Tab-IP"

# Nombres de las pestañas de incidencias para entidades federativas
# 2013/02-2019/10: "Pág-43-Tab-IP(1)"
# 2019/11        : "Pág-54-Tab-IP(1)"
# 2019/12        : "Pág-52-Tab-IP(1)"
# 2020/01-2020/03: "Pág-53-Tab-IP(1)"
# 2020/04        : "Pág-54-Tab-IP(1)"
# 2020/05-2020/09: "Pág-43-Tab-IP(1)"
# 2020/10-2020/11: "Pág-54-Tab-IPEF"
# 2020/12        : "Pág-49-Tab-IP(1)"
# 2021/02-2021/03: "Pág-48-Tab-IP(1)"
# 2021/04: "Pág-50-Tab-IP(1)"
# 2021/05: "Pág-48-Tab-IP(1)"
# 2021/10-2021/11: "Pág-53-Tab-IP(1)"

# Nombres de las pestañas de incidencias para centros penitenciales federales
# 2013/02-2019/10: "Pág-43-Tab-IP(2)"
# 2019/11        : "Pág-55-Tab-IP(2)"
# 2019/12        : "Pág-53-Tab-IP(2)"
# 2020/01-2020/03: "Pág-54-Tab-IP(2)"
# 2020/04        : "Pág-55-Tab-IP(2)"
# 2020/05-2020/09: "Pág-43-Tab-IP(2)"
# 2020/10-2020/11: "Pág-55-Tab-IPCF"
# 2020/12-2021/03: "Pág-49-Tab-IP(2)"
# 2021/04: "Pág-51-Tab-IP(2)"
# 2021/05: "Pág-49-Tab-IP(2)"


# Periodo de 2008 a 

# Periodo de 2010 a 2013
v_sheetG <- c("201006", "201007", "201008", "201009", "201010", 
              "201011", "201012", "201101", "201102", "201103")
v_sheetA <- c("201101", "201102", "201103")
v_sheetB <- c("201104", "201105")
v_sheetC <- c("201201")
v_sheetD <- c("201202")
v_sheetE <- c("201301")

# Periodo de 2012 a 2021
v_sheet1 <- c("201201")
v_sheet2 <- c("201202", "201203")
v_sheet3 <- c("201911", "202004")
v_sheet4 <- c("201912", "202109")
v_sheet5 <- c("202001", "202002", "202003", "202110", "202111")
v_sheet6 <- c("202010", "202011")
v_sheet7 <- c("202012", "202101", "202106")
v_sheet8 <- c("202102", "202103", "202105")
v_sheet9 <- c("202104")
v_sheetX <- c("202108")

# Nombres de las pestañas cuando entidades y centros estaban en la misma pestaña  
v_nj_names_sheet <- c(
    "pág 38", "pág 39 ", "pág 40 ", "pág 41 ", "pág 42", "Pág-43-Tab-IP")

# Nombres de las pestañas para los centros estatales
v_n1_names_sheet <- c(
    "Pág-43-Tab-IP(1)", "Pág-54-Tab-IP(1)", "Pág-52-Tab-IP(1)", 
    "Pág-53-Tab-IP(1)", "Pág-54-Tab-IPEF" , "Pág-49-Tab-IP(1)", 
    "Pág-48-Tab-IP(1)", "Pág-50-Tab-IP(1)", "Pág-51-Tab-IP(1)")

# Nombres de las pestañas para los centros federales 
v_n2_names_sheet <- c(
    "Pág-43-Tab-IP(2)", "Pág-55-Tab-IP(2)", "Pág-53-Tab-IP(2)", 
    "Pág-54-Tab-IP(2)", "Pág-55-Tab-IPCF" , "Pág-50-Tab-IP(2)", 
    "Pág-49-Tab-IP(2)", "Pág-51-Tab-IP(2)", "Pág-52-Tab-IP(2)")


# Vector con distintos grupos 
v_nivel <- c("Estal", "Federal")


# Fechas con formato .xlsx
# Fechas con formato .xlsx
# Archivos con formato (.xls) : 
# Archivos con formato (.xlsx): 2005/01-2010/12
v_xlsx <- c("201105", "201106", "201107", "201108", "201110", "201112",
            "201202", "201203", "201204", "201208", "201209", "201210", 
            "201211", "201212", "201301", "201304", "201411", "202111")


### 2.2.2. Ejecutar bucle  -----------------------------------------------------

v_years <- 2005:2021
# v_years <- 2021

for (i in v_years) {
    for (x in v_months) {
        
        # Imprimir y guardar el identificador de la base
        # print("Paso 1")
        print(paste(i, x)) 
        id          <- paste0(i, x)
        
        # Formato del archivo 
        v_format    <- ifelse((id %in% v_xlsx | i %in% 2005:2010), ".xlsx", ".xls")
        v_siglas    <- ifelse(i != 2005, "CE_", "CEP_")
        
        # Bases en donde todos los centros (estatales y federales) están en una pestaña
        if(i %in% 2005:2012 | id == "201301") {
            
            print("Base con centros estatales y federales en una misma pestaña")
            
            # Importar base
            v_sheet <- case_when(
                id %in% "201203"    ~ "pág 42 ",
                id %in% "200907"    ~ "pág 36",
                id %in% c(v_sheetG) ~ "pág 38 ",
                !(id %in% c("200907", v_sheetG)) & (id %in% v_sheetA | i %in% 2005:2010) ~ v_nj_names_sheet[1],
                id %in% v_sheetB ~ v_nj_names_sheet[2],
                !(id %in% c(v_sheetA, v_sheetB)) & i == 2011 ~ v_nj_names_sheet[3],
                id %in% v_sheetC ~ v_nj_names_sheet[4],
                id %in% v_sheetD ~ v_nj_names_sheet[5],
                !(id %in% c(v_sheetC, v_sheetD)  & (id %in% v_sheetE | i %in% 2012)) ~ v_nj_names_sheet[6])
            
            df_crudo    <- read_excel(paste_inp(paste0(i, "/", v_siglas, i, "_", x, v_format)), sheet = v_sheet)
            
            # Limpiar bases en formato long
            if(FALSE){
                print("Bases con formato long")
            }
            
            # Limpiar bases en formato wide (Entre 2008/04 y 2013/01)
            if((i %in% 2008:2012 | id == "201301") & !(id %in% c("200801", "200802", "200803"))){
                
                print("Base con formato wide")
                df_obs <- df_crudo                              %>% 
                    remove_empty(which = c("rows", "cols")) 
                
                v_names             <- names(df_obs)  # Nombres de las variables de la base cruda
                
                df_renombrado <- df_obs %>% 
                    rename(entidad = v_names[1], 
                           intentofuga_incidencia           = v_names[2], 
                           intentofuga_involucradas         = v_names[3], 
                           intentofuga_heridas              = v_names[4], 
                           intentofuga_homicidio            = v_names[5], 
                           fugas_incidencia                 = v_names[6], 
                           fugas_involucradas               = v_names[7], 
                           fugas_heridas                    = v_names[8], 
                           fugas_homicidio                  = v_names[9], 
                           motines_incidencia               = v_names[10], 
                           motines_involucradas             = v_names[11], 
                           motines_heridas                  = v_names[12], 
                           motines_homicidio                = v_names[13],
                           riñas_incidencia                 = v_names[14], 
                           riñas_involucradas               = v_names[15], 
                           riñas_heridas                    = v_names[16], 
                           riñas_homicidio                  = v_names[17], 
                           intentohomicidio_incidencia      = v_names[18], 
                           intentohomicidio_involucradas    = v_names[19], 
                           intentohomicidio_heridas         = v_names[20], 
                           homicidios_incidencia            = v_names[21], 
                           homicidios_involucradas          = v_names[22], 
                           homicidios_heridas               = v_names[23], 
                           intentosuicidio_incidencia       = v_names[24], 
                           intentosuicidio_involucradas     = v_names[25], 
                           suicidios_incidencia             = v_names[26], 
                           suicidios_involucradas           = v_names[27], 
                           huelgashambre_incidencia         = v_names[28], 
                           huelgashambre_involucradas       = v_names[29],
                           decesos_incidencia               = v_names[30], 
                           decesos_involucradas             = v_names[31], 
                           intentoviolaciones_incidencia    = v_names[32], 
                           intentoviolaciones_involucradas  = v_names[33],
                           violaciones_incidencia           = v_names[34], 
                           violaciones_involucradas         = v_names[35], 
                           agresioenesterceros_incidencia   = v_names[36], 
                           agresioenesterceros_involucradas = v_names[37], 
                           autoagresiones_incidencia        = v_names[38], 
                           autoagresiones_involucradas      = v_names[39], 
                           total_incidencia                 = v_names[40], 
                           total_personas                   = v_names[41]) %>% 
                    select(-c(total_incidencia, total_personas))
                
                # Primera observación 
                v_head <- which(grepl("Aguascalientes", df_renombrado$entidad, ignore.case=TRUE)) 
                v_tail <- which(grepl("ceferepsi", df_renombrado$entidad, ignore.case=TRUE)) 
                
                v_numeric <- names(df_renombrado)[2:39]
                
                # Formato largo
                df_long <- df_renombrado %>%
                    slice(v_head:v_tail) %>% 
                    mutate_at(v_numeric, as.numeric) %>%
                    pivot_longer(cols = v_numeric,
                                 names_to = c("evento", "unidad"),
                                 names_sep = "_", 
                                 values_to = "total") %>%
                    mutate(total = coalesce(as.numeric(total), 0)) %>%
                    mutate(year = i, mes = x,
                           grupo = "Población penitenciaria", nivel = NA) %>% 
                    select(evento, entidad, total, year, mes, nivel, grupo)
                
                # Pegar en una sola base
                # print("Paso 12")
                df_acumulada <- df_acumulada                        %>%
                    bind_rows(df_long)                              %>%
                    distinct()
                
            }
        }
        
        # Antes de 2008/04 no hay datos de incidentes desagregados por centros penitenciarios
        
        # Bases en donde dividen en dos pestañas los centros estatales de los federales
        if(i %in% 2013:2021 & id != "201301") {
            print("Bases con dos petañas separadas para centros federales y estatales")
            # Realizar proceso de importar y limpiar para centros estatales y luego federales 
            for(n in v_nivel) {
                # Seleccionar el nombre de la pestaña pertinente
                # Para la base de centros estatales
                
                if(n == v_nivel[1]){
                    
                    v_sheet <- case_when(
                        !(id %in% c(v_sheet3, v_sheet4, v_sheet5, v_sheet6, 
                                    v_sheet7, v_sheet8, v_sheet9, v_sheetX)) & 
                            i %in% 2012:2021   ~ v_n1_names_sheet[1], 
                        id %in% v_sheet3    ~ v_n1_names_sheet[2],
                        id %in% v_sheet4    ~ v_n1_names_sheet[3],
                        id %in% v_sheet5    ~ v_n1_names_sheet[4],
                        id %in% v_sheet6    ~ v_n1_names_sheet[5],
                        id %in% v_sheet7    ~ v_n1_names_sheet[6],
                        id %in% v_sheet8    ~ v_n1_names_sheet[7],
                        id %in% v_sheet9    ~ v_n1_names_sheet[8], 
                        id %in% v_sheetX    ~ v_n1_names_sheet[9], ) 
                } 
                
                # Para la base de centros federales
                if(n == v_nivel[2]){
                    
                    v_sheet <- case_when(
                        !(id %in% c(v_sheet3, v_sheet4,  v_sheet5, v_sheet6, 
                                    v_sheet7, v_sheet8, v_sheet9, v_sheetX)) & 
                            i %in% 2012:2021   ~ v_n2_names_sheet[1], 
                        id %in% v_sheet3    ~ v_n2_names_sheet[2],
                        id %in% v_sheet4    ~ v_n2_names_sheet[3],
                        id %in% v_sheet5    ~ v_n2_names_sheet[4],
                        id %in% v_sheet6    ~ v_n2_names_sheet[5],
                        id %in% v_sheet7    ~ v_n2_names_sheet[6],
                        id %in% v_sheet8    ~ v_n2_names_sheet[7],
                        id %in% v_sheet9    ~ v_n2_names_sheet[8],
                        id %in% v_sheetX    ~ v_n2_names_sheet[9]) 
                }
                
                
                df_crudo    <- read_excel(paste_inp(paste0(i, "/", v_siglas, i, 
                                                           "_", x, v_format)), 
                                          sheet = v_sheet)
                
                # Limpieza
                v_names             <- names(df_crudo)  # Nombres de las variables de la base cruda
                
                df_seleccionado     <- df_crudo                         %>%
                    rename(evento = v_names[1])
                
                # Primera observación
                v_head <- which(grepl("Intentos de Fuga", df_seleccionado$evento, ignore.case=TRUE)) -2
                v_tail <- which(grepl("total", df_seleccionado$evento, ignore.case=TRUE)) - 1
                
                df_obs <- df_seleccionado %>%
                    slice(v_head:v_tail) %>%
                    remove_empty(which = c("rows", "cols"))
                
                v_names <- names(df_obs)
                
                df_selected <- df_obs %>%
                    rename(evento = evento, unidad = v_names[2]) %>%
                    select(-c(v_names[length(v_names)]))
                
                df_selected$evento[1] <- "evento"
                df_selected$unidad[1] <- "unidad"
                
                df_limpio <- df_selected %>%
                    row_to_names(row_number = 1) %>%
                    filter(unidad == "Incidencias")
                
                # Nombre de las variables numéricas
                v_numeric <- names(df_limpio)[3:dim(df_limpio)[2]]
                
                # Formato largo
                df_long <- df_limpio %>%
                    mutate_at(v_numeric, as.numeric) %>%
                    pivot_longer(cols = v_numeric,
                                 names_to = ("entidad"),
                                 values_to = "total") %>%
                    mutate(total = coalesce(as.numeric(total), 0)) %>%
                    mutate(year = i, mes = x,
                           grupo = "Población penitenciaria", nivel = n)
                
                # Pegar en una sola base
                # print("Paso 12")
                df_acumulada <- df_acumulada                        %>%
                    bind_rows(df_long)                              %>%
                    distinct()                                      %>% 
                    select(evento, entidad, total, year, mes, nivel, grupo)
            }
        }
    }
}

beep(1)


## 2.3. Pulir bases procesadas y unificadas ------------------------------------

# View(df_acumulada)

# Controles de calidad 
table(df_acumulada$entidad)
table(df_acumulada$evento)
table(df_acumulada$nivel)
table(df_acumulada$grupo)

# Limpieza
df_incidentes <- df_acumulada                                       %>% 
    filter(is.na(evento) == F, str_detect(evento, "Fuente") == F)   %>% 
    mutate(entidad  = case_when(
        entidad == "Cefereso 7 Nor-Noroeste"           ~ "Cefereso 7", 
        entidad == "CEFERESO No. 7 Nor-Noroeste"       ~ "Cefereso 7", 
        entidad == "Cefereso No. 7 Nor-Noroeste"       ~ "Cefereso 7", 
        str_detect(entidad, "Femenil")                 ~ "Centro Federal Femenil",
        str_detect(entidad, "Altiplano")               ~ "Cefereso 1", 
        str_detect(entidad, "Almoloya")                ~ "Cefereso 1",
        str_detect(entidad, "Palma")                   ~ "Cefereso 1",
        str_detect(entidad, "Occidente")               ~ "Cefereso 2", 
        str_detect(entidad, "Puente")                  ~ "Cefereso 2", 
        str_detect(entidad, "3 Noreste")               ~ "Cefereso 3", 
        str_detect(entidad, "Matamoros")               ~ "Cefereso 3", 
        str_detect(entidad, "CEFERESO No. 4 Noroeste") ~ "Cefereso 4", 
        str_detect(entidad, "Cefereso No. 4 Noroeste") ~ "Cefereso 4", 
        str_detect(entidad, "CEFERESO 4 Noroeste")     ~ "Cefereso 4", 
        str_detect(entidad, "Cefereso 4 Noroeste")     ~ "Cefereso 4", 
        str_detect(entidad, "Rincón")                  ~ "Cefereso 4", 
        str_detect(entidad, "Oriente")                 ~ "Cefereso 5", 
        str_detect(entidad, "Sureste")                 ~ "Cefereso 6", 
        str_detect(entidad, "Nor-Poniente")            ~ "Cefereso 8", 
        str_detect(entidad, "9")                       ~ "Cefereso 9", 
        str_detect(entidad, "10")                      ~ "Cefereso 10", 
        str_detect(entidad, "11")                      ~ "Cefereso 11", 
        str_detect(entidad, "12")                      ~ "Cefereso 12", 
        str_detect(entidad, "13")                      ~ "Cefereso 13", 
        str_detect(entidad, "14")                      ~ "Cefereso 14", 
        str_detect(entidad, "15")                      ~ "Cefereso 15", 
        str_detect(entidad, "16")                      ~ "Cefereso 16", 
        str_detect(entidad, "17")                      ~ "Cefereso 17",
        str_detect(entidad, "18")                      ~ "Cefereso 18",
        str_detect(entidad, "Distrito")                ~ "Ciudad de México",
        str_detect(entidad, "Cd")                      ~ "Ciudad de México",
        str_detect(entidad, "Edo")                     ~ "Estado de México",
        str_detect(entidad, "Islas")                   ~ "Complejo Penitenciario Islas Marías",
        entidad == "C P F I M"                         ~ "Complejo Penitenciario Islas Marías",
        entidad == "C P I M"                           ~ "Complejo Penitenciario Islas Marías",
        entidad == "Centro Federal Femenil"            ~ "Complejo Penitenciario Islas Marías",
        entidad == "Ags."                              ~ "Aguascalientes",
        entidad == "B. C."                             ~ "Baja California",
        entidad == "B. C. S."                          ~ "Baja California Sur",
        entidad == "Camp."                             ~ "Campeche",
        entidad == "Chis."                             ~ "Chiapas",
        entidad == "Chih."                             ~ "Chihuahua",
        entidad == "Coah."                             ~ "Coahuila",
        entidad == "Col."                              ~ "Colima",
        entidad == "D. F."                             ~ "Ciudad de México",
        entidad == "CDMX"                              ~ "Ciudad de México",
        entidad == "Dgo."                              ~ "Durango",
        entidad == "Gto."                              ~ "Guanajuato",
        entidad == "Gro."                              ~ "Guerrero",
        entidad == "Hgo"                               ~ "Hidalgo",
        entidad == "Hgo."                              ~ "Hidalgo",
        entidad == "Jal."                              ~ "Jalisco",
        entidad == "México"                            ~ "Estado de México",
        entidad == "Méx."                              ~ "Estado de México",
        entidad == "Mich"                              ~ "Michoacán",
        entidad == "Mich."                             ~ "Michoacán",
        entidad == "Mor."                              ~ "Morelos",
        entidad == "Nay."                              ~ "Nayarit",
        entidad == "N. L."                             ~ "Nuevo León",
        entidad == "Oax."                              ~ "Oaxaca",
        entidad == "Pue."                              ~ "Puebla",
        entidad == "Qro."                              ~ "Querétaro",
        entidad == "Q. Roo."                           ~ "Quintana Roo",
        entidad == "SLP."                              ~ "San Luis Potosí",
        entidad == "*SLP."                             ~ "San Luis Potosí",
        entidad == "Sin."                              ~ "Sinaloa",
        entidad == "Son."                              ~ "Sonora",
        entidad == "Tab."                              ~ "Tabasco",
        entidad == "Tamps."                            ~ "Tamaulipas",
        entidad == "Tlaxc."                            ~ "Tlaxcala",
        entidad == "Ver."                              ~ "Veracruz",
        entidad == "Yuc."                              ~ "Yucatán",
        entidad == "Zac."                              ~ "Zacatecas",
        entidad == entidad ~ entidad)) %>% 
    mutate(evento = case_when(
        evento == "agresioenesterceros"                ~ "Agresiones a Terceros", 
        evento == "autoagresiones"                     ~ "Autoagresión", 
        evento == "Autoagresiones"                     ~ "Autoagresión", 
        evento == "decesos"                            ~ "Decesos", 
        evento == "fugas"                              ~ "Fugas", 
        evento == "homicidios"                         ~ "Homicidios", 
        evento == "huelgashambre"                      ~ "Huelgas de Hambre", 
        evento == "intentofuga"                        ~ "Intentos de Fuga", 
        evento == "intentohomicidio"                   ~ "Intento de Homicidio", 
        evento == "Intentos de Homicidio"              ~ "Intento de Homicidio", 
        evento == "intentosuicidio"                    ~ "Intentos de Suicidio", 
        evento == "motines"                            ~ "Motines", 
        evento == "riñas"                              ~ "Riñas",
        evento == "suicidios"                          ~ "Suicidios",
        evento == "intentoviolaciones"                 ~ "Intentos de Violación", 
        evento == "Intento de Violaciones"             ~ "Intentos de Violación", 
        evento == "violaciones"                        ~ "Violaciones", 
        T ~ evento)) %>% 
    # Agregar el nivel 
    mutate(nivel = case_when(str_detect(entidad, "Cefe") | str_detect(entidad, "Complejo") | str_detect(entidad, "Federal")  ~ "Federal", 
                             !(str_detect(entidad, "Cefe") | str_detect(entidad, "Complejo") | str_detect(entidad, "Federal")) ~ "Estatal")) %>%  
    select(nivel, entidad, year, mes, grupo, evento, total)

unique(df_incidentes$entidad)
unique(df_incidentes$evento)

sum(is.na(df_incidentes$entidad))
sum(is.na(df_incidentes$evento))
sum(is.na(df_incidentes$total))

# Cambios 
table(df_incidentes$entidad)
table(df_incidentes$evento)
table(df_incidentes$nivel)
table(df_incidentes$grupo)
table(df_incidentes$year)
table(df_incidentes$mes)

table(df_incidentes$year, df_incidentes$mes)

length(unique(df_incidentes$entidad))


# df_faltantes <- df_incidentes %>% 
#     filter(evento == "Huelgas de Hambre", mes == "06")
# 
# table(df_faltantes$entidad)

## 2.4. Guardar base final -----------------------------------------------------

save(df_incidentes, file = paste_out("df_incidentes.Rdata"))

# FIN --------------------------------------------------------------------------
