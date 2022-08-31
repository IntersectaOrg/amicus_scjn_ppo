
###########################################################
# Personas privadas de la libertad (CNGSPSE 2011-2022)    #
# *Delitos atribuidos a ppl / estatus y sexo*             #
#                                                         #
# Adriana E. Ortega Arriaga                               #
# Fecha de creación: Marzo 2021                           #
# Fecha de actualización: Junio 2022                      #
###########################################################

rm(list=ls())
setwd("~")

require(pacman)
p_load(scales, tidyverse, stringi, dplyr, plyr, foreign, readxl, janitor,extrafont,
       beepr, extrafont, treemapify, ggmosaic, srvyr, ggrepel, lubridate, tidyr)

Sys.setlocale("LC_ALL", "es_ES.UTF-8")

inp <- "/Volumes/GoogleDrive/My Drive/PPO/datos_crudos/Censo_delitos_ppl"
clean <- "/Volumes/GoogleDrive/My Drive/PPO/datos_limpios/Censos_delitos_ppl/"

#### Datos 2010 ####
ppl_10 <- read_csv(paste(inp, "POB_RECL_10.csv", sep = "/"))
ppl_10 <- filter(ppl_10, SIT_PREC > 30 & SIT_PREC < 35, CTL_POB == 7)
ppl_10 <- mutate(ppl_10, EST_POB = replace_na(EST_POB,0))
ppl_10 <- filter(ppl_10, EST_POB == 0)
ppl_10$SEXO <- factor(ppl_10$SEXO, 
                          levels = c(1, 2),
                          labels = c("Hombres", "Mujeres"))

ppl_10$SIT_PREC <- factor(ppl_10$SIT_PREC, 
                      levels = c(31, 32, 33, 34),
                      labels = c("Sin sentencia", "Con sentencia", "Con sentencia", "Con sentencia"))


ppl_10$UBIC_GEO <- factor(ppl_10$UBIC_GEO, 
                             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                        11, 12, 13, 14, 15, 16, 17, 
                                        18, 19, 20, 21 , 22, 23, 24, 
                                        25, 26, 27, 28, 29, 30, 31, 32),
                             labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                        "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                        "Ciudad de México", "Durango", "Guanajuato", 
                                        "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                        "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                        "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                        "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                        "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

ppl_10 <- select(ppl_10, UBIC_GEO, DELITO, SEXO, SIT_PREC, TOT_POB)
ppl_10 <- mutate(ppl_10, fuero = case_when(DELITO < 134 ~ "Fuero común"))
ppl_10 <- mutate(ppl_10, fuero = case_when(DELITO > 200 ~ "Fuero federal"))

ppl_10 <- select(ppl_10, UBIC_GEO, DELITO, SEXO, SIT_PREC, TOT_POB)%>%
  dplyr::rename(entidad = UBIC_GEO,
         delito = DELITO,
         sexo = SEXO,
         estatus = SIT_PREC,
         total = TOT_POB)%>%
  mutate(fuero = case_when(delito < 134 ~ "Fuero común",
                           delito > 200 ~ "Fuero federal"))%>%
  mutate(year = "2010")%>%
  select(entidad, sexo, delito, estatus, fuero, total, year)

ppl_10$fuero <- as.factor(ppl_10$fuero)

ppl_10$delito <- factor(ppl_10$delito, 
                          levels = c(101, 102, 103, 104, 105, 106, 107, 108, 109,
                                     110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
                                     120, 121, 122, 123, 124, 125, 126, 127, 128, 129,
                                     130, 131, 132, 133, 201, 202, 203, 204, 205, 206,
                                     207, 208, 209, 210, 211, 212, 213, 214, 215, 216,
                                     217, 218, 219, 220, 221, 222, 223, 224, 225),
                          labels = c("Homicidio doloso", "Homicidio culposo", "Lesiones dolosas",
                                     "Lesiones culposas", "Violencia familiar", "Otros integridad corporal",
                                     "Violación", "Violación", "Otros delitos sexuales", "Secuestro",
                                     "Secuestro express", "Otros contra la libertad personal", "Robo a casa habitación",
                                     "Robo de vehículo", "Robo de autopartes", "Robo a transeúnte", "Robo en carretera",
                                     "Robo a instituciones bancarias", "Robo a negocio", "Robo de ganado", "Fraude",
                                     "Abuso de confianza", "Extorsión", "Daño en la propiedad", "Despojo", "Otros contra el patrimonio",
                                     "Amenazas", "Allanamiento de morada", "Evasión de presos", "Trata de personas", "Narcomenudeo",
                                     "Otros del fuero común", "No identificado", "Delitos contra la Seguridad de la Nación",
                                     "Evasión de presos", "Asociación delictuosa", "Otros contra la Seguridad Pública",
                                     "Operaciones con recursos de procedencia ilícita", "Ataque a las vías de comunicación",
                                     "Delitos cometidos por servidores públicos", "Falsedad de declaraciones", "Falsificación de documentos",
                                     "Otros de falsedad", "Delitos contra la autoridad", "Contra la vida y la integridad corporal",
                                     "Secuestro", "Contra de las personas en su patrimonio", "Delitos previstos en el Código Fiscal de la Federación",
                                     "Delitos ambientales", "Delitos electorales", "Delitos contra la salud", "Delincuencia organizada",
                                     "Trata de personas", "Armas de Fuego y Explosivos", "Derecho de Autor", "Ley General de Población",
                                     "Otros delitos del Fuero Federal", "No identificado"))

##Revisión NAs
sum(is.na(ppl_10$entidad))
sum(is.na(ppl_10$estatus))
sum(is.na(ppl_10$fuero))
sum(is.na(ppl_10$delito))
sum(is.na(ppl_10$sexo))
sum(is.na(ppl_10$total))
sum(is.na(ppl_10$year))

#Chequeo de totales
sum(ppl_10$total) #183,247

#### Datos 2011 ####
ppl_11 <- read_csv(paste(inp, "SP_CENTR_11.csv", sep = "/"))
ppl_11 <- filter(ppl_11, CLA_POB == 3)
ppl_11 <- filter(ppl_11, ESTRUCTU == 310 | ESTRUCTU == 312)
ppl_11 <- mutate(ppl_11, EST_PRCP = replace_na(EST_PRCP,0))
ppl_11 <- filter(ppl_11, EST_PRCP == 0)

ppl_11$SEXO <- factor(ppl_11$SEXO, 
                      levels = c(1, 2, 9),
                      labels = c("Hombres", "Mujeres", "No especificado"))


ppl_11$TIPO_FUERO <- factor(ppl_11$TIPO_FUERO, 
                      levels = c(1, 2),
                      labels = c("Fuero común", "Fuero federal"))

ppl_11$EST_JURI <- factor(ppl_11$EST_JURI, 
                            levels = c(1, 2, 3, 4, 9),
                            labels = c("Sin sentencia", "Con sentencia", "Con sentencia",
                                       "Con sentencia", "No especificado"))
ppl_11$ENTIDAD <- factor(ppl_11$ENTIDAD, 
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                     11, 12, 13, 14, 15, 16, 17, 
                                     18, 19, 20, 21 , 22, 23, 24, 
                                     25, 26, 27, 28, 29, 30, 31, 32),
                          labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                     "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                     "Ciudad de México", "Durango", "Guanajuato", 
                                     "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                     "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                     "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                     "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                     "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

ppl_11 <- select(ppl_11, ENTIDAD, SEXO, TIPO_FUERO, DELITOS, EST_JURI, POB_CEPE)%>%
  dplyr::rename(entidad = ENTIDAD,
                sexo = SEXO,
                fuero = TIPO_FUERO,
                delito = DELITOS,
                estatus = EST_JURI,
                total = POB_CEPE)%>%
  mutate(year = "2011")%>%
  select(entidad, sexo, delito, estatus, fuero, total, year)

ppl_11$delito <- factor(ppl_11$delito, 
                        levels = c(1101, 1102, 1103, 1201, 1202, 1203, 1204, 1205, 1301,
                                   1302, 1303, 1304, 1401, 1402, 1403, 1404, 1405, 1406,
                                   1407, 1408, 1409, 1410, 1411, 1412, 1413, 1414, 1415,
                                   1416, 1417, 1418, 1419, 1501, 1502, 1601, 1602, 1603,
                                   1701, 1702, 1703, 1704, 1705, 1999, 201, 202, 203, 204, 205, 206,
                                   207, 208, 209, 210, 211, 212, 213, 214, 215, 216,
                                   217, 218, 219, 220, 221, 222, 223, 224, 299),
                        labels = c("Homicidio", "Lesiones", "Otros contra la vida y la integridad corporal",
                                   "Privación de la libertad", "Secuestro", "Secuestro express", "Tráfico de menores",
                                   "Otros contra la libertad personal", "Abuso sexual", "Violación", "Violación",
                                   "Otros contra la libertad y seguridad sexual", "Robo a casa habitación", "Robo de vehículo",
                                   "Robo a transeúnte en vía pública", "Robo a transeúnte en espacio abierto al público",
                                   "Robo a transportista", "Robo en transporte público individual", "Robo en transporte público colectivo",
                                   "Robo en transporte individual", "Robo a institución bancaria", "Robo a negocio", "Robo de ganado",
                                   "Robo de maquinaria", "Otros robos", "Fraude", "Abuso de confianza", "Extorsión", "Daño a la propiedad",
                                   "Despojo", "Otros contra el patrimonio", "Violencia familiar", "Otros contra la familia",
                                   "Corrupción de menores", "Trata de personas", "Otros delitos contra la sociedad", "Narcomenudeo", "Amenazas",
                                   "Allanamiento de morada", "Evasión de presos", "Otros delitos del Fuero Común", "No especificado delito del fuero común",
                                  "Delitos contra la Seguridad de la Nación",
                                   "Evasión de presos", "Asociación delictuosa", "Otros contra la Seguridad Pública",
                                   "Operaciones con recursos de procedencia ilícita", "Ataque a las vías de comunicación",
                                   "Delitos cometidos por servidores públicos", "Falsedad de declaraciones", "Falsificación de documentos",
                                   "Otros de falsedad", "Delitos contra la autoridad", "Contra la vida y la integridad corporal",
                                   "Secuestro", "Contra de las personas en su patrimonio", "Delitos previstos en el Código Fiscal de la Federación",
                                   "Delitos ambientales", "Delitos electorales", "Delitos contra la salud", "Delincuencia organizada",
                                   "Trata de personas", "Armas de Fuego y Explosivos", "Derecho de Autor", "Ley General de Población",
                                   "Otros delitos del Fuero Federal", "No especificado del fuero federal"))

##Revisión NAs
sum(is.na(ppl_11$entidad))
sum(is.na(ppl_11$estatus))
sum(is.na(ppl_11$fuero))
sum(is.na(ppl_11$delito))
sum(is.na(ppl_11$sexo))
sum(is.na(ppl_11$total))
sum(is.na(ppl_11$year))

#Chequeo de totales
sum(ppl_11$total) #208,172

#### Datos 2012 ####
ppl_12 <- read_csv(paste(inp, "SP_CENTR_12.csv", sep = "/"))
ppl_12 <- filter(ppl_12, CLA_POB == 3)
ppl_12 <- filter(ppl_12, ESTRUCTU == 3100 | ESTRUCTU == 3120)
ppl_12 <- mutate(ppl_12, EST_PRCP = replace_na(EST_PRCP,0))
ppl_12 <- filter(ppl_12, EST_PRCP == 0)
ppl_12 <- mutate(ppl_12, POB_CEPE = replace_na(POB_CEPE,0))

ppl_12$SEXO <- factor(ppl_12$SEXO, 
                      levels = c(1, 2, 9),
                      labels = c("Hombres", "Mujeres", "No especificado"))

ppl_12$TIPO_FUE <- factor(ppl_12$TIPO_FUE, 
                            levels = c(1, 2),
                            labels = c("Fuero común", "Fuero federal"))

ppl_12$EST_JURI <- factor(ppl_12$EST_JURI, 
                          levels = c(1, 2, 3, 4, 9),
                          labels = c("Sin sentencia", "Con sentencia", "Con sentencia",
                                     "Con sentencia", "No especificado"))

ppl_12$ENTIDAD <- factor(ppl_12$ENTIDAD, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                    11, 12, 13, 14, 15, 16, 17, 
                                    18, 19, 20, 21 , 22, 23, 24, 
                                    25, 26, 27, 28, 29, 30, 31, 32),
                         labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                    "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                    "Ciudad de México", "Durango", "Guanajuato", 
                                    "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                    "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                    "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                    "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                    "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

ppl_12 <- select(ppl_12, ENTIDAD, SEXO, TIPO_FUE, DELITOS, EST_JURI, POB_CEPE)%>%
  dplyr::rename(entidad = ENTIDAD,
                sexo = SEXO,
                fuero = TIPO_FUE,
                delito = DELITOS,
                estatus = EST_JURI,
                total = POB_CEPE)%>%
  mutate(year = "2012")%>%
  select(entidad, sexo, delito, estatus, fuero, total, year)

ppl_12$delito <- factor(ppl_12$delito, 
                        levels = c(1101, 1102, 1103, 1201, 1202, 1203, 1204, 1205, 1301,
                                   1302, 1303, 1304, 1401, 1402, 1403, 1404, 1405, 1406,
                                   1407, 1408, 1409, 1410, 1411, 1412, 1413, 1414, 1415,
                                   1416, 1417, 1418, 1419, 1501, 1502, 1601, 1602, 1603,
                                   1701, 1702, 1703, 1704, 1705, 1999, 201, 202, 203, 204, 205, 206,
                                   207, 208, 209, 210, 211, 212, 213, 214, 215, 216,
                                   217, 218, 219, 220, 221, 222, 223, 224, 299),
                        labels = c("Homicidio", "Lesiones", "Otros contra la vida y la integridad corporal",
                                   "Privación de la libertad", "Secuestro", "Secuestro express", "Tráfico de menores",
                                   "Otros contra la libertad personal", "Abuso sexual", "Violación", "Violación",
                                   "Otros contra la libertad y seguridad sexual", "Robo a casa habitación", "Robo de vehículo",
                                   "Robo a transeúnte en vía pública", "Robo a transeúnte en espacio abierto al público",
                                   "Robo a transportista", "Robo en transporte público individual", "Robo en transporte público colectivo",
                                   "Robo en transporte individual", "Robo a institución bancaria", "Robo a negocio", "Robo de ganado",
                                   "Robo de maquinaria", "Otros robos", "Fraude", "Abuso de confianza", "Extorsión", "Daño a la propiedad",
                                   "Despojo", "Otros contra el patrimonio", "Violencia familiar", "Otros contra la familia",
                                   "Corrupción de menores", "Trata de personas", "Otros delitos contra la sociedad", "Narcomenudeo", "Amenazas",
                                   "Allanamiento de morada", "Evasión de presos", "Otros delitos del Fuero Común", "No especificado del fuero común",
                                   "Delitos contra la Seguridad de la Nación",
                                   "Evasión de presos", "Asociación delictuosa", "Otros contra la Seguridad Pública",
                                   "Operaciones con recursos de procedencia ilícita", "Ataque a las vías de comunicación",
                                   "Delitos cometidos por servidores públicos", "Falsedad de declaraciones", "Falsificación de documentos",
                                   "Otros de falsedad", "Delitos contra la autoridad", "Contra la vida y la integridad corporal",
                                   "Secuestro", "Contra de las personas en su patrimonio", "Delitos previstos en el Código Fiscal de la Federación",
                                   "Delitos ambientales", "Delitos electorales", "Delitos contra la salud", "Delincuencia organizada",
                                   "Trata de personas", "Armas de Fuego y Explosivos", "Derecho de Autor", "Ley General de Población",
                                   "Otros delitos del Fuero Federal", "No especificado del fuero federal"))

##Revisión NAs
sum(is.na(ppl_12$entidad))
sum(is.na(ppl_12$estatus))
sum(is.na(ppl_12$fuero))
sum(is.na(ppl_12$delito))
sum(is.na(ppl_12$sexo))
sum(is.na(ppl_12$total))
sum(is.na(ppl_12$year))

#which(is.na(ppl_12$delito))

#Chequeo de totales
sum(ppl_12$total) #202,319

#### Datos 2013 ####
ppl_13 <- read_csv(paste(inp, "SP_CENTR_13.csv", sep = "/"))
ppl_13 <- filter(ppl_13, ESTRUCTU == 3110)
ppl_13 <- mutate(ppl_13, EST_PRCP = replace_na(EST_PRCP,0))
ppl_13 <- filter(ppl_13, EST_PRCP == 0)

ppl_13$SEXO <- factor(ppl_13$SEXO, 
                      levels = c(1, 2, 9),
                      labels = c("Hombres", "Mujeres", "No especificado"))

ppl_13$EST_JURI <- factor(ppl_13$EST_JURI, 
                          levels = c(1, 2, 3, 4, 9),
                          labels = c("Sin sentencia", "Con sentencia", "Con sentencia",
                                     "Con sentencia", "No especificado"))

ppl_13$ENTIDAD <- factor(ppl_13$ENTIDAD, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                    11, 12, 13, 14, 15, 16, 17, 
                                    18, 19, 20, 21 , 22, 23, 24, 
                                    25, 26, 27, 28, 29, 30, 31, 32),
                         labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                    "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                    "Ciudad de México", "Durango", "Guanajuato", 
                                    "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                    "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                    "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                    "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                    "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

ppl_13 <- mutate(ppl_13, DELIT_FC = replace_na(DELIT_FC,0))
ppl_13 <- mutate(ppl_13, DELIT_FF = replace_na(DELIT_FF,0))
ppl_13 <- mutate(ppl_13, delito = rowSums(ppl_13[,6:7], na.rm=T)) 

ppl_13 <- select(ppl_13, ENTIDAD, SEXO, EST_JURI, POB_CEPE, delito)%>%
  dplyr::rename(entidad = ENTIDAD,
                sexo = SEXO,
                estatus = EST_JURI,
                total = POB_CEPE,
                delito = delito)%>%
  mutate(fuero = case_when(delito <= 99 ~ "Fuero federal",
                           delito >= 101 ~ "Fuero común"))%>%
  mutate(year = "2013")%>%
  select(entidad, sexo, delito, estatus, fuero, total, year)

ppl_13$fuero <- as.factor(ppl_13$fuero)

ppl_13$delito <- factor(ppl_13$delito, 
                        levels = c(101, 102, 103, 104, 105, 201, 202, 203, 204, 205, 206, 301, 302, 
                                   303, 304, 305, 306, 307, 401, 402, 403, 404, 405, 406, 407, 408, 409,
                                   410, 411, 412, 413, 414, 415, 416, 417, 418, 419, 420, 501, 502, 503,
                                   504, 601, 602, 603, 701, 702, 703, 704, 705, 706, 707, 708, 709, 710, 999,
                                   1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                                   21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37,
                                   38, 39, 40, 99),
                        labels = c("Homicidio", "Feminicidio", "Aborto", "Lesiones", "Otros contra la vida y la integridad corporal",
                                   "Privación de la libertad", "Secuestro", "Secuestro exprés", "Tráfico de menores", "Rapto",
                                   "Otros contra la libertad personal", "Abuso sexual", "Acoso sexual", "Hostigamiento sexual",
                                   "Violación", "Violación", "Incesto", "Otros contra la libertad y seguridad sexual",
                                   "Robo a casa habitación", "Robo de vehículo", "Robo a transeúnte en vía pública", "Robo a transeúnte en espacio abierto al público",
                                   "Robo a transportista", "Robo en transporte público individual", "Robo en transporte público colectivo",
                                   "Robo en transporte individual", "Robo a institución bancaria", "Robo a negocio", "Robo de ganado", "Robo de maquinaria",
                                   "Robo de autopartes", "Otros robos", "Fraude", "Abuso de confianza", "Extorsión", "Daño a la propiedad", "Despojo", "Otros contra el patrimonio",
                                   "Violencia familiar", "Violencia de género distinta a la violencia familiar", "Incumplimiento de obligaciones familiares",
                                   "Otros contra la familia", "Corrupción de menores", "Trata de personas", "Otros contra la sociedad", 
                                   "Narcomenudeo", "Amenazas", "Allanamiento de morada", "Evasión de presos", "Falsedad", "Falsificación",
                                   "Contra el medio ambiente", "Responsabilidad de servidores públicos", "Electorales", "Otros del Fuero Común",
                                   "No especificado",
                                   "Contra la Seguridad de la Nación", "Contra el Derecho Internacional", "Contra la Humanidad", "Contra la Seguridad Pública",
                                   "Materia de Vías de Comunicación y Correspondencia", "Contra la Autoridad", "Contra la Salud", "Contra el libre desarrollo de la personalidad",
                                   "Acceso ilícito a sistemas y equipos de informática", "Cometidos por Servidores Públicos", "Cometidos contra la administración de justicia",
                                   "Responsabilidad Profesional", "Falsedad", "Contra la Economía Pública", "Contra la Libertad y el Normal Desarrollo Psicosexual",
                                   "Contra el Estado Civil y Bigamia", "Materia de Inhumaciones y Exhumaciones", "Contra la Paz y Seguridad de las Personas","Contra la Vida y la Integridad Corporal",
                                   "Contra el Honor", "Privación Ilegal de la Libertad", "Contra de las Personas en su Patrimonio", "Encubrimiento", "Delitos Electorales",
                                   "Contra el Ambiente y la Gestión Ambiental", "Materia de Derechos de Autor", "Código de Justicia Militar", "Código Fiscal de la Federación", 
                                   "Ley General de Población", "Ley de Migración", "Armas de Fuego y Explosivos", "Ley de Concursos Mercantiles", "Delincuencia Organizada", "Delitos bancarios y financieros",
                                   "En materia de invenciones y marcas", "Propiedad intelectual", "Monumentos y Zonas Arqueológicos", "Delitos de Imprenta", "Juegos y Sorteos",
                                   "Otro", "No especificado"))

##Revisión NAs
sum(is.na(ppl_13$entidad))
sum(is.na(ppl_13$estatus))
sum(is.na(ppl_13$fuero))
sum(is.na(ppl_13$delito))
sum(is.na(ppl_13$sexo))
sum(is.na(ppl_13$total))
sum(is.na(ppl_13$year))

#which(is.na(ppl_13$delito))

#Chequeo de totales
sum(ppl_13$total) #214,694

#### Datos 2014 ####
ppl_14 <- read.csv(paste(inp, "SP_CENTR_14.csv", sep = "/"))
ppl_14 <- filter(ppl_14, ESTRUCTU == 3202 | ESTRUCTU == 3203) 
ppl_14 <- mutate(ppl_14, EST_PRCP = replace_na(EST_PRCP,0))
ppl_14 <- filter(ppl_14, EST_PRCP == 0)

ppl_14$SEXO <- factor(ppl_14$SEXO, 
                      levels = c(1, 2, 9),
                      labels = c("Hombres", "Mujeres", "No especificado"))

ppl_14$EST_JURI <- factor(ppl_14$EST_JURI, 
                          levels = c(1, 2, 3, 4, 9),
                          labels = c("Sin sentencia", "Con sentencia", "Con sentencia",
                                     "Con sentencia", "No especificado"))

ppl_14$ENTIDAD <- factor(ppl_14$ENTIDAD, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                    11, 12, 13, 14, 15, 16, 17, 
                                    18, 19, 20, 21 , 22, 23, 24, 
                                    25, 26, 27, 28, 29, 30, 31, 32),
                         labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                    "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                    "Ciudad de México", "Durango", "Guanajuato", 
                                    "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                    "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                    "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                    "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                    "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

ppl_14 <- mutate(ppl_14, DELIT_FC = replace_na(DELIT_FC,0))
ppl_14 <- mutate(ppl_14, DELIT_FF = replace_na(DELIT_FF,0))
ppl_14 <- mutate(ppl_14, delito = rowSums(ppl_14[,4:5], na.rm=T)) 

ppl_14 <- select(ppl_14, ENTIDAD, SEXO, EST_JURI, POB_CEPE, delito)%>%
  dplyr::rename(entidad = ENTIDAD,
                sexo = SEXO,
                estatus = EST_JURI,
                total = POB_CEPE,
                delito = delito)%>%
  mutate(fuero = case_when(delito <= 99 ~ "Fuero federal",
                           delito >= 101 ~ "Fuero común"))%>%
  mutate(year = "2014")%>%
  select(entidad, sexo, delito, estatus, fuero, total, year)

ppl_14$fuero <- as.factor(ppl_14$fuero)

ppl_14$delito <- factor(ppl_14$delito, 
                        levels = c(101, 102, 103, 104, 105, 106, 107, 201, 202, 203, 204, 205, 206, 207,
                                   208, 209, 210, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310,
                                   401, 402, 403, 404, 405, 406, 407, 408, 409,
                                   410, 411, 412, 413, 414, 415, 416, 417, 418, 419, 420, 421, 422, 501, 502, 503,
                                   509, 601, 602, 603, 604, 605, 701, 702, 703, 704, 705, 706, 707, 708, 709, 710, 999,
                                   1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                                   21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37,
                                   38, 39, 40, 99),
                        labels = c("Homicidio", "Feminicidio", "Aborto", "Otros delitos que privan de la vida", "Lesiones", "Otros que atentan contra la integridad corporal",
                                   "Otros que atentan contra la vida y la integridad corporal", "Privación de la libertad", "Tráfico de menores", "Robo de menor", "Rapto",
                                   "Desaparición forzada", "Otros que atentan contra la libertad personal s/l", "Secuestro", "Secuestro exprés", "Otros que atentan contra la libertad personal",
                                   "Otros que atentan contra la libertad personal", "Abuso sexual", "Acoso sexual", "Hostigamiento sexual", "Otros delitos sexuales sin realización de cópula",
                                   "Violación", "Violación", "Estupro", "Incesto", "Otros delitos sexuales con realización de cópula", "Otros delitos que atentan contra la libertad y seguridad sexual",
                                   "Robo", "Robo a casa habitación", "Robo de vehículo", "Robo de autopartes", "Robo a transeúnte en vía pública", "Robo a transeúnte en espacio abierto al público",
                                   "Robo a persona en un lugar privado", "Robo a transportista", "Robo en transporte público individual", "Robo en transporte público colectivo", "Robo en transporte individual",
                                   "Robo a institución bancaria", "Robo a negocio", "Robo de ganado", "Robo de maquinaria", "Otros robos", "Fraude", "Abuso de confianza",
                                   "Extorsión", "Daño a la propiedad", "Despojo", "Otros delitos que atentan contra el patrimonio", "Violencia familiar", "Incumplimiento de obligaciones de asistencia familiar",
                                   "Otros delitos contra la familia", "No especificado contra familia", "Corrupción de menores incapaces", "Trata de personas", "Violencia de genero en todas sus modalidades",
                                   "Discriminación", "Otros delitos contra la sociedad", "Narcomenudeo", "Amenazas", "Allanamiento de morada", "Evasión de presos", "Falsedad",
                                   "Falsificación", "Contra el medio ambiente", "Responsabilidad de servidores públicos", "Electorales", "Otros delitos del Fuero Común", "No especificado",
                                   "Contra la Seguridad de la Nación", "Contra el Derecho Internacional", "Contra la Humanidad", "Contra la Seguridad Pública",
                                   "Materia de Vías de Comunicación y Correspondencia", "Contra la Autoridad", "Contra la Salud", "Contra el libre desarrollo de la personalidad",
                                   "Acceso ilícito a sistemas y equipos de informática", "Cometidos por Servidores Públicos", "Cometidos contra la administración de justicia",
                                   "Responsabilidad Profesional", "Falsedad", "Contra la Economía Pública", "Contra la Libertad y el Normal Desarrollo Psicosexual",
                                   "Contra el Estado Civil y Bigamia", "Materia de Inhumaciones y Exhumaciones", "Contra la Paz y Seguridad de las Personas","Contra la Vida y la Integridad Corporal",
                                   "Contra el Honor", "Privación Ilegal de la Libertad", "Contra de las Personas en su Patrimonio", "Encubrimiento", "Delitos Electorales",
                                   "Contra el Ambiente y la Gestión Ambiental", "Materia de Derechos de Autor", "Código de Justicia Militar", "Código Fiscal de la Federación", 
                                   "Ley General de Población", "Ley de Migración", "Armas de Fuego y Explosivos", "Ley de Concursos Mercantiles", "Delincuencia Organizada", "Delitos bancarios y financieros",
                                   "En materia de invenciones y marcas", "Propiedad intelectual", "Monumentos y Zonas Arqueológicos", "Delitos de Imprenta", "Juegos y Sorteos",
                                   "Otro", "No especificado"))

##Revisión NAs
sum(is.na(ppl_14$entidad))
sum(is.na(ppl_14$estatus))
sum(is.na(ppl_14$fuero))
sum(is.na(ppl_14$delito))
sum(is.na(ppl_14$sexo))
sum(is.na(ppl_14$total))
sum(is.na(ppl_14$year))

#which(is.na(ppl_14$delito))

#Chequeo de totales
ppl_14$total <- as.numeric(ppl_14$total)
sum(ppl_14$total) #287,705

#### Datos 2015 (supuestamente cambió a delitos) ####
ppl_15 <- read.csv(paste(inp, "SP_CENTR_15.csv", sep = "/"))
ppl_15 <- filter(ppl_15, ESTRUCTU == 30203800 | ESTRUCTU == 30203900) 
ppl_15 <- mutate(ppl_15, EST_PRCP = replace_na(EST_PRCP,0))
ppl_15 <- filter(ppl_15, EST_PRCP == 0)

ppl_15$SEXO <- factor(ppl_15$SEXO, 
                      levels = c(1, 2, 9),
                      labels = c("Hombres", "Mujeres", "No especificado"))

ppl_15$EST_JURI <- factor(ppl_15$EST_JURI, 
                          levels = c(1, 2, 3, 4, 9),
                          labels = c("Sin sentencia", "Con sentencia", "Con sentencia",
                                     "Con sentencia", "No especificado"))

ppl_15$ENTIDAD <- factor(ppl_15$ENTIDAD, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                    11, 12, 13, 14, 15, 16, 17, 
                                    18, 19, 20, 21 , 22, 23, 24, 
                                    25, 26, 27, 28, 29, 30, 31, 32),
                         labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                    "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                    "Ciudad de México", "Durango", "Guanajuato", 
                                    "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                    "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                    "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                    "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                    "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

ppl_15$DELI_FF <- formatC(ppl_15$DELI_FF, flag = 0, width = 5)
ppl_15$DELI_FF <- as.numeric(ppl_15$DELI_FF)
ppl_15 <- mutate(ppl_15, DELI_FF = replace_na(DELI_FF,0))
ppl_15 <- mutate(ppl_15, DELI_FC = replace_na(DELI_FC,0))
ppl_15 <- mutate(ppl_15, fuero = case_when(DELI_FC >= 1010 ~ "Fuero común",
                                           DELI_FC == 0 ~ "Fuero federal"))

ppl_15 <- mutate(ppl_15, delito = rowSums(ppl_15[,7:8], na.rm=T)) 

ppl_15$DELI_FF <- formatC(ppl_15$DELI_FF, flag = 0, width = 5)

ppl_15 <- select(ppl_15, ENTIDAD, SEXO, EST_JURI, DEL_POCP, fuero, delito)%>%
  dplyr::rename(entidad = ENTIDAD,
                sexo = SEXO,
                estatus = EST_JURI,
                total = DEL_POCP,
                delito = delito)%>%
  mutate(year = "2015")%>%
  select(entidad, sexo, delito, estatus, fuero, total, year)

out <- "/Users/adriana/Google Drive/PPO/datos_crudos/Censo_delitos_ppl"
write.csv(ppl_15,paste(out, "ppl_15_sd.csv",sep="/"), fileEncoding = "UTF-8", row.names=F)
ppl_15 <- read_csv(paste(out, "ppl_15.csv", sep = "/")) ##Hice trampa y etiqueté los delitos en excel :(
ppl_15 <- mutate(ppl_15, total = replace_na(total,0))

ppl_15$entidad <- as.factor(ppl_15$entidad)
ppl_15$sexo <- as.factor(ppl_15$sexo)
ppl_15$delito <- as.factor(ppl_15$delito)
ppl_15$fuero <- as.factor(ppl_15$fuero)
ppl_15$estatus <- as.factor(ppl_15$estatus)
ppl_15$year <- as.character(ppl_15$year)

##Revisión NAs
sum(is.na(ppl_15$entidad))
sum(is.na(ppl_15$estatus))
sum(is.na(ppl_15$fuero))
sum(is.na(ppl_15$delito))
sum(is.na(ppl_15$sexo))
sum(is.na(ppl_15$total))
sum(is.na(ppl_15$year))

#which(is.na(ppl_15$total))

#Chequeo de totales
sum(ppl_15$total) #299,277

#### Datos 2016 ####
# fuero común #
ppl_16_fc <- read_csv(paste(inp, "RECLTIFC_16.csv", sep = "/"))%>%
  mutate_if(is.character, as.numeric)

ppl_16_fc <- select(ppl_16_fc, ENTIDAD, DELITOS, ESTJUR1, ESTJUR2, ESTJUR3, ESTJUR4, ESTJUR5, ESTJUR6, ESTJUR7, ESTJUR8)%>%
  dplyr::rename(entidad = ENTIDAD,
                delito = DELITOS,
                sin_sentencia_hombres = ESTJUR1,
                sin_sentencia_mujeres = ESTJUR2,
                con_sentencia_hombres_1 = ESTJUR3,
                con_sentencia_mujeres_1 = ESTJUR4,
                con_sentencia_hombres_2 = ESTJUR5,
                con_sentencia_mujeres_2 = ESTJUR6,
                con_sentencia_hombres_3 = ESTJUR7,
                con_sentencia_mujeres_3 = ESTJUR8)%>%
  select(entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres,
         con_sentencia_hombres_1, con_sentencia_hombres_2, con_sentencia_hombres_3,
         con_sentencia_mujeres_1, con_sentencia_mujeres_2, con_sentencia_mujeres_3)

ppl_16_fc <- mutate(ppl_16_fc, con_sentencia_hombres = rowSums(ppl_16_fc[,5:7], na.rm=T))
ppl_16_fc <- mutate(ppl_16_fc, con_sentencia_mujeres = rowSums(ppl_16_fc[,8:10], na.rm=T))
ppl_16_fc <- select(ppl_16_fc, entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres, con_sentencia_hombres, con_sentencia_mujeres)

ppl_16_fc <- pivot_longer(ppl_16_fc, cols = c(sin_sentencia_hombres:con_sentencia_mujeres),
                          names_to = "estatus", values_to = "total")

ppl_16_fc <- mutate(ppl_16_fc, sexo = case_when(estatus == "con_sentencia_hombres" ~ "Hombres",
                                                estatus == "sin_sentencia_hombres" ~ "Hombres",
                                                estatus == "con_sentencia_mujeres" ~ "Mujeres",
                                                estatus == "sin_sentencia_mujeres" ~ "Mujeres"))

ppl_16_fc <- mutate(ppl_16_fc, estatus = case_when(estatus == "con_sentencia_hombres" ~ "Con sentencia",
                                                estatus == "sin_sentencia_hombres" ~ "Sin sentencia",
                                                estatus == "con_sentencia_mujeres" ~ "Con sentencia",
                                                estatus == "sin_sentencia_mujeres" ~ "Sin sentencia"))

ppl_16_fc <- mutate(ppl_16_fc, year = "2016", fuero = "Fuero común")
ppl_16_fc <- select(ppl_16_fc, entidad, sexo, delito, estatus, fuero, total, year)
ppl_16_fc <- mutate(ppl_16_fc, total = replace_na(total,0))

ppl_16_fc$entidad <- factor(ppl_16_fc$entidad, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                    11, 12, 13, 14, 15, 16, 17, 
                                    18, 19, 20, 21 , 22, 23, 24, 
                                    25, 26, 27, 28, 29, 30, 31, 32),
                         labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                    "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                    "Ciudad de México", "Durango", "Guanajuato", 
                                    "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                    "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                    "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                    "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                    "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

#Filtro a delitos sin desagregación
ppl_16_fc <- filter(ppl_16_fc, delito >= 10100 &  delito <= 20600 | delito >= 20900 &  delito <= 40100 |
                          delito >= 40200 &  delito <= 40500 | delito >= 40600 &  delito <= 60100 | delito == 60200 |
                          delito >= 60300 &  delito <= 60900 | delito >= 70101 &  delito <= 70199 | delito >= 70201 & delito <= 80100 |
                          delito == 80200 | delito >= 80300 &  delito <= 90500 | delito == 90600 | delito == 90700 |
                          delito >= 90800 &  delito <= 999999)

ppl_16_fc$delito <- factor(ppl_16_fc$delito, 
                               levels = c(10100, 10200, 10300, 10400, 10900, 20100, 20200, 20300, 20400, 20500, 20600,
                                          20900, 30100, 30200, 30300, 30400, 30500, 30600, 30700, 30900, 40100, 40200, 40300,
                                          40400, 40500, 40600, 40700, 40900, 50100, 50200, 50900, 60100, 60200, 60300, 60400,
                                          60900, 70101, 70102, 70103, 70104, 70109, 70199, 70201, 70202, 70203, 70204, 70205,
                                          70206, 70209, 70299, 70300, 70400, 70500, 70600, 70900, 80100, 80200, 80300, 80400,
                                          80900, 90100, 90200, 90300, 90400, 90500, 90600, 90700, 90800, 90900, 91000, 91100,
                                          91200, 91300, 91400, 91500, 91600, 91900, 999999),
                               labels = c("Homicidio", "Feminicidio", "Aborto", "Lesiones", "Otros contra la vida y la integridad corporal",
                                          "Privación de la libertad", "Tráfico de menores", "Retención o sustracción de menores", "Rapto",
                                          "Desaparición forzada", "Secuestro", "Otros contra la libertad personal", "Abuso sexual", "Acoso sexual",
                                          "Hostigamiento sexual", "Violación", "Violación equiparada", "Estupro", "Incesto", "Otros contra la libertad y seguridad sexual",
                                          "Robo", "Sustracción de hidrocarburos", "Fraude", "Abuso de confianza", "Extorsión", "Daño a la propiedad", "Despojo",
                                          "Otros contra el patrimonio", "Violencia familiar", "Incumplimiento de obligaciones familiares", "Otros contra la familia",
                                          "Contra el libre desarrollo de la personalidad", "Trata de personas", "Violencia de género distinta a violencia familiar",
                                          "Discriminación", "Otros contra la sociedad", "Posesión simple de narcóticos", "Posesión con fines de comercio o suministro de narcóticos",
                                          "Comercio de narcóticos FC", "Suministro de narcóticos FC", "Otros narcomenudeo", "Narcomenudeo NE",
                                          "Producción de narcóticos", "Transporte de narcóticos", "Tráfico de narcóticos", "Comercio de narcóticos FF","Suministro de narcóticos FF",
                                          "Posesión de narcóticos", "Otros federales contra la salud", "Narcotráfico NE", "Evasión de presos", "Armas y objetos prohibidos",
                                          "Delincuencia organizada", "Armas, explosivos y otros materiales", "Otros contra la seguridad pública", "Corrupción",
                                          "Contra la administración de justicia", "En materia fiscal", "Delitos electorales", "Otros contra la administración del Estado",
                                          "Amenazas", "Allanamiento de morada", "Falsedad", "Falsificación", "Contra el medio ambiente", "En materia de comunicaciones",
                                          "En materia de migración", "En materia de derechos de autor", "En materia de instituciones de crédito",  "En materia de propiedad industrial",
                                          "Contra la salud no relacionados con narcóticos", "Encubrimiento","Operaciones con recursos de procedencia ilícita",
                                          "Tortura", "Suplantación y usurpación de identidad", "Contra la seguridad de los datos", "Otros delitos", "No especificado"))

##Revisión NAs
sum(is.na(ppl_16_fc$entidad))
sum(is.na(ppl_16_fc$estatus))
sum(is.na(ppl_16_fc$fuero))
sum(is.na(ppl_16_fc$delito))
sum(is.na(ppl_16_fc$sexo))
sum(is.na(ppl_16_fc$total))
sum(is.na(ppl_16_fc$year))

#which(is.na(ppl_16$total))

#Chequeo de totales
sum(ppl_16_fc$total) #341,496 (antes de filtro de delitos)

# fuero federal #
ppl_16_ff <- read_csv(paste(inp, "RECLTIFF_16.csv", sep = "/"))%>%
  mutate_if(is.character, as.numeric)

ppl_16_ff <- select(ppl_16_ff, ENTIDAD, DELITOS, ESTJUR1, ESTJUR2, ESTJUR3, ESTJUR4, ESTJUR5, ESTJUR6, ESTJUR7, ESTJUR8)%>%
  dplyr::rename(entidad = ENTIDAD,
                delito = DELITOS,
                sin_sentencia_hombres = ESTJUR1,
                sin_sentencia_mujeres = ESTJUR2,
                con_sentencia_hombres_1 = ESTJUR3,
                con_sentencia_mujeres_1 = ESTJUR4,
                con_sentencia_hombres_2 = ESTJUR5,
                con_sentencia_mujeres_2 = ESTJUR6,
                con_sentencia_hombres_3 = ESTJUR7,
                con_sentencia_mujeres_3 = ESTJUR8)%>%
  select(entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres,
         con_sentencia_hombres_1, con_sentencia_hombres_2, con_sentencia_hombres_3,
         con_sentencia_mujeres_1, con_sentencia_mujeres_2, con_sentencia_mujeres_3)

ppl_16_ff <- mutate(ppl_16_ff, con_sentencia_hombres = rowSums(ppl_16_ff[,5:7], na.rm=T))
ppl_16_ff <- mutate(ppl_16_ff, con_sentencia_mujeres = rowSums(ppl_16_ff[,8:10], na.rm=T))
ppl_16_ff <- select(ppl_16_ff, entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres, con_sentencia_hombres, con_sentencia_mujeres)

ppl_16_ff <- pivot_longer(ppl_16_ff, cols = c(sin_sentencia_hombres:con_sentencia_mujeres),
                          names_to = "estatus", values_to = "total")

ppl_16_ff <- mutate(ppl_16_ff, sexo = case_when(estatus == "con_sentencia_hombres" ~ "Hombres",
                                                estatus == "sin_sentencia_hombres" ~ "Hombres",
                                                estatus == "con_sentencia_mujeres" ~ "Mujeres",
                                                estatus == "sin_sentencia_mujeres" ~ "Mujeres"))

ppl_16_ff <- mutate(ppl_16_ff, estatus = case_when(estatus == "con_sentencia_hombres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_hombres" ~ "Sin sentencia",
                                                   estatus == "con_sentencia_mujeres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_mujeres" ~ "Sin sentencia"))

ppl_16_ff <- mutate(ppl_16_ff, year = "2016", fuero = "Fuero federal")
ppl_16_ff <- select(ppl_16_ff, entidad, sexo, delito, estatus, fuero, total, year)
ppl_16_ff <- mutate(ppl_16_ff, total = replace_na(total,0))

ppl_16_ff$entidad <- factor(ppl_16_ff$entidad, 
                            levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                       11, 12, 13, 14, 15, 16, 17, 
                                       18, 19, 20, 21 , 22, 23, 24, 
                                       25, 26, 27, 28, 29, 30, 31, 32),
                            labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                       "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                       "Ciudad de México", "Durango", "Guanajuato", 
                                       "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                       "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                       "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                       "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                       "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

#Filtro a delitos sin desagregación
ppl_16_ff <- filter(ppl_16_ff, delito >= 10100 &  delito <= 20600 | delito >= 20900 &  delito <= 40100 |
                      delito >= 40200 &  delito <= 40500 | delito >= 40600 &  delito <= 60100 | delito == 60200 |
                      delito >= 60300 &  delito <= 60900 | delito >= 70101 &  delito <= 70199 | delito >= 70201 & delito <= 80100 |
                      delito == 80200 | delito >= 80300 &  delito <= 90500 | delito == 90600 | delito == 90700 |
                      delito >= 90800 &  delito <= 999999)

ppl_16_ff$delito <- factor(ppl_16_ff$delito, 
                           levels = c(10100, 10200, 10300, 10400, 10900, 20100, 20200, 20300, 20400, 20500, 20600,
                                      20900, 30100, 30200, 30300, 30400, 30500, 30600, 30700, 30900, 40100, 40200, 40300,
                                      40400, 40500, 40600, 40700, 40900, 50100, 50200, 50900, 60100, 60200, 60300, 60400,
                                      60900, 70101, 70102, 70103, 70104, 70109, 70199, 70201, 70202, 70203, 70204, 70205,
                                      70206, 70209, 70299, 70300, 70400, 70500, 70600, 70900, 80100, 80200, 80300, 80400,
                                      80900, 90100, 90200, 90300, 90400, 90500, 90600, 90700, 90800, 90900, 91000, 91100,
                                      91200, 91300, 91400, 91500, 91600, 91900, 999999),
                           labels = c("Homicidio", "Feminicidio", "Aborto", "Lesiones", "Otros contra la vida y la integridad corporal",
                                      "Privación de la libertad", "Tráfico de menores", "Retención o sustracción de menores", "Rapto",
                                      "Desaparición forzada", "Secuestro", "Otros contra la libertad personal", "Abuso sexual", "Acoso sexual",
                                      "Hostigamiento sexual", "Violación", "Violación equiparada", "Estupro", "Incesto", "Otros contra la libertad y seguridad sexual",
                                      "Robo", "Sustracción de hidrocarburos", "Fraude", "Abuso de confianza", "Extorsión", "Daño a la propiedad", "Despojo",
                                      "Otros contra el patrimonio", "Violencia familiar", "Incumplimiento de obligaciones familiares", "Otros contra la familia",
                                      "Contra el libre desarrollo de la personalidad", "Trata de personas", "Violencia de género distinta a violencia familiar",
                                      "Discriminación", "Otros contra la sociedad", "Posesión simple de narcóticos", "Posesión con fines de comercio o suministro de narcóticos",
                                      "Comercio de narcóticos FC", "Suministro de narcóticos FC", "Otros narcomenudeo", "Narcomenudeo NE",
                                      "Producción de narcóticos", "Transporte de narcóticos", "Tráfico de narcóticos", "Comercio de narcóticos FF","Suministro de narcóticos FF",
                                      "Posesión de narcóticos", "Otros federales contra la salud", "Narcotráfico NE", "Evasión de presos", "Armas y objetos prohibidos",
                                      "Delincuencia organizada", "Armas, explosivos y otros materiales", "Otros contra la seguridad pública", "Corrupción",
                                      "Contra la administración de justicia", "En materia fiscal", "Delitos electorales", "Otros contra la administración del Estado",
                                      "Amenazas", "Allanamiento de morada", "Falsedad", "Falsificación", "Contra el medio ambiente", "En materia de comunicaciones",
                                      "En materia de migración", "En materia de derechos de autor", "En materia de instituciones de crédito",  "En materia de propiedad industrial",
                                      "Contra la salud no relacionados con narcóticos", "Encubrimiento","Operaciones con recursos de procedencia ilícita",
                                      "Tortura", "Suplantación y usurpación de identidad", "Contra la seguridad de los datos", "Otros delitos", "No especificado"))

##Revisión NAs
sum(is.na(ppl_16_ff$entidad))
sum(is.na(ppl_16_ff$estatus))
sum(is.na(ppl_16_ff$fuero))
sum(is.na(ppl_16_ff$delito))
sum(is.na(ppl_16_ff$sexo))
sum(is.na(ppl_16_ff$total))
sum(is.na(ppl_16_ff$year))

#which(is.na(ppl_16$total))

#Chequeo de totales
sum(ppl_16_ff$total) #43392 (antes de filtro de delitos)

#pegando ambos fueros#
ppl_16 <- bind_rows(ppl_16_fc, ppl_16_ff)
ppl_16$sexo <- as.factor(ppl_16$sexo)
ppl_16$estatus <- as.factor(ppl_16$estatus)
ppl_16$fuero <- as.factor(ppl_16$fuero)

sum(is.na(ppl_16$entidad))
sum(is.na(ppl_16$estatus))
sum(is.na(ppl_16$fuero))
sum(is.na(ppl_16$delito))
sum(is.na(ppl_16$sexo))
sum(is.na(ppl_16$total))
sum(is.na(ppl_16$year))

#### Datos 2017 ####
# fuero común #
ppl_17_fc <- read_csv(paste(inp, "RECLTIFC_17.csv", sep = "/"))%>%
  mutate_if(is.character, as.numeric)

ppl_17_fc <- select(ppl_17_fc, ENTIDAD, TIPDELIT, ESJURD1, ESJURD2, ESJURD4, ESJURD5, ESJURD7, ESJURD8, ESJURD10, ESJURD11)%>%
  dplyr::rename(entidad = ENTIDAD,
                delito = TIPDELIT,
                sin_sentencia_hombres = ESJURD1,
                sin_sentencia_mujeres = ESJURD2,
                con_sentencia_hombres_1 = ESJURD4,
                con_sentencia_mujeres_1 = ESJURD5,
                con_sentencia_hombres_2 = ESJURD7,
                con_sentencia_mujeres_2 = ESJURD8,
                con_sentencia_hombres_3 = ESJURD10,
                con_sentencia_mujeres_3 = ESJURD11)%>%
  select(entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres,
         con_sentencia_hombres_1, con_sentencia_hombres_2, con_sentencia_hombres_3,
         con_sentencia_mujeres_1, con_sentencia_mujeres_2, con_sentencia_mujeres_3)

ppl_17_fc <- mutate(ppl_17_fc, con_sentencia_hombres = rowSums(ppl_17_fc[,5:7], na.rm=T))
ppl_17_fc <- mutate(ppl_17_fc, con_sentencia_mujeres = rowSums(ppl_17_fc[,8:10], na.rm=T))
ppl_17_fc <- select(ppl_17_fc, entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres, con_sentencia_hombres, con_sentencia_mujeres)

ppl_17_fc <- pivot_longer(ppl_17_fc, cols = c(sin_sentencia_hombres:con_sentencia_mujeres),
                          names_to = "estatus", values_to = "total")

ppl_17_fc <- mutate(ppl_17_fc, sexo = case_when(estatus == "con_sentencia_hombres" ~ "Hombres",
                                                estatus == "sin_sentencia_hombres" ~ "Hombres",
                                                estatus == "con_sentencia_mujeres" ~ "Mujeres",
                                                estatus == "sin_sentencia_mujeres" ~ "Mujeres"))

ppl_17_fc <- mutate(ppl_17_fc, estatus = case_when(estatus == "con_sentencia_hombres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_hombres" ~ "Sin sentencia",
                                                   estatus == "con_sentencia_mujeres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_mujeres" ~ "Sin sentencia"))

ppl_17_fc <- mutate(ppl_17_fc, year = "2017", fuero = "Fuero común")
ppl_17_fc <- select(ppl_17_fc, entidad, sexo, delito, estatus, fuero, total, year)
ppl_17_fc <- mutate(ppl_17_fc, total = replace_na(total,0))

ppl_17_fc$entidad <- factor(ppl_17_fc$entidad, 
                            levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                       11, 12, 13, 14, 15, 16, 17, 
                                       18, 19, 20, 21 , 22, 23, 24, 
                                       25, 26, 27, 28, 29, 30, 31, 32),
                            labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                       "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                       "Ciudad de México", "Durango", "Guanajuato", 
                                       "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                       "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                       "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                       "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                       "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

#Filtro a delitos sin desagregación
ppl_17_fc <- filter(ppl_17_fc, delito >= 10100 &  delito <= 20600 | delito >= 20700 &  delito <= 30400 |
                          delito >= 30500 &  delito <= 39999 | delito >= 40101 &  delito <= 40199 | delito == 40200 | delito >= 40300 &  delito <= 40500 |
                          delito >= 40600 &  delito <= 60100 | delito == 60200 | delito >= 60300 &  delito <= 69999 | delito >= 70101 &  delito <= 70199 |
                          delito >= 70201 &  delito <= 70299 | delito >= 70300 &  delito <= 70600 | delito >= 70700 &  delito <= 79999 |
                          delito >= 80101 &  delito <= 80199 | delito == 80200 | delito >= 80300 &  delito <= 90500 | delito == 90600 | delito == 90700 |
                          delito >= 90800 &  delito <= 91600 | delito >= 91700 &  delito <= 999999)

ppl_17_fc$delito <- factor(ppl_17_fc$delito, 
                               levels = c(10100, 10200, 10300, 10400, 10500, 19999, 20100, 20200, 20300, 20400, 20500, 20600,
                                          20700, 29999, 30100, 30200, 30300, 30400, 30500, 30600, 30700, 39999, 40101, 40102, 40103,
                                          40104, 40105, 40106, 40107, 40108, 40109, 40110, 40111, 40112, 40113, 40114, 40115, 40116,
                                          40117, 40118, 40199, 40200, 40300, 40400, 40500, 40600, 40700, 40800, 49999, 50100, 50200, 50300, 59999, 60100, 60200,
                                          60300, 60400, 60500, 60600, 69999, 70101, 70102, 70103, 70104, 70105, 70106, 70199, 
                                          70201, 70202, 70203, 70204, 70205, 70206, 70207, 70208, 70299, 70300, 70400, 70500, 70600, 70700, 70800,
                                          70900, 79999, 80101, 80102, 80103, 80104, 80105, 80106, 80107, 80108, 80109, 80199, 80200, 80300, 80400, 80500,
                                          89999, 90100, 90200, 90300, 90400, 90500, 90600, 90700, 90800, 90900,
                                          91000, 91100, 91200, 91300, 91400, 91500, 91600, 91700, 91800, 99999, 100100, 999999),
                               labels = c("Homicidio", "Feminicidio", "Aborto", "Lesiones", "Otros contra la vida y la integridad corporal",
                                          "La vida y la integridad corporal NE", "Privación de la libertad", "Tráfico de menores", "Retención o sustracción de menores e incapaces",
                                          "Rapto", "Desaparición forzada de personas", "Secuestro", "Otros que atentan contra la libertad personal",
                                          "La libertad personal NE", "Abuso sexual", "Acoso sexual", "Hostigamiento sexual", "Violación", "Estupro",
                                          "Incesto", "Otros contra la libertad y la seguridad sexual", "La libertad y la seguridad sexual NE",
                                          "Robo simple", "Robo a casa habitación", "Robo de vehículo", "Robo de autopartes", "Robo a transeúnte en vía pública", 
                                          "Robo a transeúnte en espacio abierto al público", "Robo a persona en un lugar privado", "Robo a transportista",
                                          "Robo en transporte público individual", "Robo en transporte público colectivo", "Robo en transporte individual",
                                          "Robo a institución bancaria", "Robo a negocio", "Robo de ganado", "Robo de maquinaria", "Robo de energía eléctrica",
                                          "Otros robos", "Robo NI", "Robo NE", 
                                          "Hidrocarburos y sus derivados", "Fraude", "Abuso de confianza", "Extorsión", "Daño a la propiedad",
                                          "Despojo", "Otros contra el patrimonio", "El patrimonio NE", "Violencia familiar", "Incumplimiento de obligaciones familiares",
                                          "La familia - Otros", "La familia NE", "Contra el libre desarrollo de la personalidad", "Trata de personas", "Violencia de género distinta a la violencia familiar",
                                          "Discriminación", "Lenocinio", "Otros contra la sociedad", "La sociedad NE", 
                                          "Posesión simple de narcóticos", "Posesión con fines de comercio o suministro de narcóticos", "Comercio de narcóticos FC",
                                          "Suministro de narcóticos FC", "Otros narcomenudeo", "Narcomenudeo NI", "Narcomenudeo NE", "Producción de narcóticos",
                                          "Transporte de narcóticos", "Tráfico de narcóticos", "Comercio de narcóticos FF", "Suministro de narcóticos FF", "Posesión de narcóticos",
                                          "Otros narcóticos", "Narcotráfico NI", "Narcotráfico NE", "Evasión de presos", "En materia de armas y objetos prohibidos", "Delincuencia organizada", "Armas, explosivos y otros materiales",
                                          "Asociación delictuosa", "Terrorismo", "Otros contra la seguridad pública", "La seguridad pública NE",
                                          "Ejercicio indebido del servicio público", "Abuso de autoridad", "Cohecho", "Peculado", "Enriquecimiento ilícito", "Ejercicio abusivo de funciones",
                                          "Tráfico de influencias", "Otros corrupción", "Corrupción NI", "Corrupción NE","Contra la administración de justicia", "Delitos en materia fiscal", "Delitos electorales", "Otros delitos contra la administración del Estado",
                                          "La administración del Estado NE", "Amenazas", "Allanamiento de morada", "Falsedad",
                                          "Falsificación", "Delitos contra el medio ambiente", "En materia de vías de comunicación", "En materia de migración",
                                          "En materia de derechos de autor", "En materia de instituciones de crédito", "En materia de propiedad industrial",
                                          "Contra la salud no relacionados con narcóticos", "Encubrimiento", "Operaciones con recursos de procedencia ilícita",
                                          "Tortura", "Suplantación y usurpación de identidad", "Contra la seguridad de los datos", "Tratos o penas crueles",
                                          "Otros delitos", "Otros NE", "No identificado", "No especificado"))


##Revisión NAs
sum(is.na(ppl_17_fc$entidad))
sum(is.na(ppl_17_fc$estatus))
sum(is.na(ppl_17_fc$fuero))
sum(is.na(ppl_17_fc$delito))
sum(is.na(ppl_17_fc$sexo))
sum(is.na(ppl_17_fc$total))
sum(is.na(ppl_17_fc$year))

#which(is.na(ppl_17$total))

#Chequeo de totales
sum(ppl_17_fc$total) #457,682 (antes de filtro de delitos)

# fuero federal #
ppl_17_ff <- read_csv(paste(inp, "RECLTIFF_17.csv", sep = "/"))%>%
  mutate_if(is.character, as.numeric)

ppl_17_ff <- select(ppl_17_ff, ENTIDAD, TIPDELIT, ESJURD1, ESJURD2, ESJURD4, ESJURD5, ESJURD7, ESJURD8, ESJURD10, ESJURD11)%>%
  dplyr::rename(entidad = ENTIDAD,
                delito = TIPDELIT,
                sin_sentencia_hombres = ESJURD1,
                sin_sentencia_mujeres = ESJURD2,
                con_sentencia_hombres_1 = ESJURD4,
                con_sentencia_mujeres_1 = ESJURD5,
                con_sentencia_hombres_2 = ESJURD7,
                con_sentencia_mujeres_2 = ESJURD8,
                con_sentencia_hombres_3 = ESJURD10,
                con_sentencia_mujeres_3 = ESJURD11)%>%
  select(entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres,
         con_sentencia_hombres_1, con_sentencia_hombres_2, con_sentencia_hombres_3,
         con_sentencia_mujeres_1, con_sentencia_mujeres_2, con_sentencia_mujeres_3)

ppl_17_ff <- mutate(ppl_17_ff, con_sentencia_hombres = rowSums(ppl_17_ff[,5:7], na.rm=T))
ppl_17_ff <- mutate(ppl_17_ff, con_sentencia_mujeres = rowSums(ppl_17_ff[,8:10], na.rm=T))
ppl_17_ff <- select(ppl_17_ff, entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres, con_sentencia_hombres, con_sentencia_mujeres)

ppl_17_ff <- pivot_longer(ppl_17_ff, cols = c(sin_sentencia_hombres:con_sentencia_mujeres),
                          names_to = "estatus", values_to = "total")

ppl_17_ff <- mutate(ppl_17_ff, sexo = case_when(estatus == "con_sentencia_hombres" ~ "Hombres",
                                                estatus == "sin_sentencia_hombres" ~ "Hombres",
                                                estatus == "con_sentencia_mujeres" ~ "Mujeres",
                                                estatus == "sin_sentencia_mujeres" ~ "Mujeres"))

ppl_17_ff <- mutate(ppl_17_ff, estatus = case_when(estatus == "con_sentencia_hombres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_hombres" ~ "Sin sentencia",
                                                   estatus == "con_sentencia_mujeres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_mujeres" ~ "Sin sentencia"))

ppl_17_ff <- mutate(ppl_17_ff, year = "2017", fuero = "Fuero federal")
ppl_17_ff <- select(ppl_17_ff, entidad, sexo, delito, estatus, fuero, total, year)
ppl_17_ff <- mutate(ppl_17_ff, total = replace_na(total,0))

ppl_17_ff$entidad <- factor(ppl_17_ff$entidad, 
                            levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                       11, 12, 13, 14, 15, 16, 17, 
                                       18, 19, 20, 21 , 22, 23, 24, 
                                       25, 26, 27, 28, 29, 30, 31, 32),
                            labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                       "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                       "Ciudad de México", "Durango", "Guanajuato", 
                                       "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                       "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                       "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                       "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                       "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

#Filtro a delitos sin desagregación
ppl_17_ff <- filter(ppl_17_ff, delito >= 10100 &  delito <= 20600 | delito >= 20700 &  delito <= 30400 |
                      delito >= 30500 &  delito <= 39999 | delito >= 40101 &  delito <= 40199 | delito == 40200 | delito >= 40300 &  delito <= 40500 |
                      delito >= 40600 &  delito <= 60100 | delito == 60200 | delito >= 60300 &  delito <= 69999 | delito >= 70101 &  delito <= 70199 |
                      delito >= 70201 &  delito <= 70299 | delito >= 70300 &  delito <= 70600 | delito >= 70700 &  delito <= 79999 |
                      delito >= 80101 &  delito <= 80199 | delito == 80200 | delito >= 80300 &  delito <= 90500 | delito == 90600 | delito == 90700 |
                      delito >= 90800 &  delito <= 91600 | delito >= 91700 &  delito <= 999999)

ppl_17_ff$delito <- factor(ppl_17_ff$delito, 
                           levels = c(10100, 10200, 10300, 10400, 10500, 19999, 20100, 20200, 20300, 20400, 20500, 20600,
                                      20700, 29999, 30100, 30200, 30300, 30400, 30500, 30600, 30700, 39999, 40101, 40102, 40103,
                                      40104, 40105, 40106, 40107, 40108, 40109, 40110, 40111, 40112, 40113, 40114, 40115, 40116,
                                      40117, 40118, 40199, 40200, 40300, 40400, 40500, 40600, 40700, 40800, 49999, 50100, 50200, 50300, 59999, 60100, 60200,
                                      60300, 60400, 60500, 60600, 69999, 70101, 70102, 70103, 70104, 70105, 70106, 70199, 
                                      70201, 70202, 70203, 70204, 70205, 70206, 70207, 70208, 70299, 70300, 70400, 70500, 70600, 70700, 70800,
                                      70900, 79999, 80101, 80102, 80103, 80104, 80105, 80106, 80107, 80108, 80109, 80199, 80200, 80300, 80400, 80500,
                                      89999, 90100, 90200, 90300, 90400, 90500, 90600, 90700, 90800, 90900,
                                      91000, 91100, 91200, 91300, 91400, 91500, 91600, 91700, 91800, 99999, 100100, 999999),
                           labels = c("Homicidio", "Feminicidio", "Aborto", "Lesiones", "Otros contra la vida y la integridad corporal",
                                      "La vida y la integridad corporal NE", "Privación de la libertad", "Tráfico de menores", "Retención o sustracción de menores e incapaces",
                                      "Rapto", "Desaparición forzada de personas", "Secuestro", "Otros que atentan contra la libertad personal",
                                      "La libertad personal NE", "Abuso sexual", "Acoso sexual", "Hostigamiento sexual", "Violación", "Estupro",
                                      "Incesto", "Otros contra la libertad y la seguridad sexual", "La libertad y la seguridad sexual NE",
                                      "Robo simple", "Robo a casa habitación", "Robo de vehículo", "Robo de autopartes", "Robo a transeúnte en vía pública", 
                                      "Robo a transeúnte en espacio abierto al público", "Robo a persona en un lugar privado", "Robo a transportista",
                                      "Robo en transporte público individual", "Robo en transporte público colectivo", "Robo en transporte individual",
                                      "Robo a institución bancaria", "Robo a negocio", "Robo de ganado", "Robo de maquinaria", "Robo de energía eléctrica",
                                      "Otros robos", "Robo NI", "Robo NE", 
                                      "Hidrocarburos y sus derivados", "Fraude", "Abuso de confianza", "Extorsión", "Daño a la propiedad",
                                      "Despojo", "Otros contra el patrimonio", "El patrimonio NE", "Violencia familiar", "Incumplimiento de obligaciones familiares",
                                      "La familia - Otros", "La familia NE", "Contra el libre desarrollo de la personalidad", "Trata de personas", "Violencia de género distinta a la violencia familiar",
                                      "Discriminación", "Lenocinio", "Otros contra la sociedad", "La sociedad NE", 
                                      "Posesión simple de narcóticos", "Posesión con fines de comercio o suministro de narcóticos", "Comercio de narcóticos FC",
                                      "Suministro de narcóticos FC", "Otros narcomenudeo", "Narcomenudeo NI", "Narcomenudeo NE", "Producción de narcóticos",
                                      "Transporte de narcóticos", "Tráfico de narcóticos", "Comercio de narcóticos FF", "Suministro de narcóticos FF", "Posesión de narcóticos",
                                      "Otros narcóticos", "Narcotráfico NI", "Narcotráfico NE", "Evasión de presos", "En materia de armas y objetos prohibidos", "Delincuencia organizada", "Armas, explosivos y otros materiales",
                                      "Asociación delictuosa", "Terrorismo", "Otros contra la seguridad pública", "La seguridad pública NE",
                                      "Ejercicio indebido del servicio público", "Abuso de autoridad", "Cohecho", "Peculado", "Enriquecimiento ilícito", "Ejercicio abusivo de funciones",
                                      "Tráfico de influencias", "Otros corrupción", "Corrupción NI", "Corrupción NE","Contra la administración de justicia", "Delitos en materia fiscal", "Delitos electorales", "Otros delitos contra la administración del Estado",
                                      "La administración del Estado NE", "Amenazas", "Allanamiento de morada", "Falsedad",
                                      "Falsificación", "Delitos contra el medio ambiente", "En materia de vías de comunicación", "En materia de migración",
                                      "En materia de derechos de autor", "En materia de instituciones de crédito", "En materia de propiedad industrial",
                                      "Contra la salud no relacionados con narcóticos", "Encubrimiento", "Operaciones con recursos de procedencia ilícita",
                                      "Tortura", "Suplantación y usurpación de identidad", "Contra la seguridad de los datos", "Tratos o penas crueles",
                                      "Otros delitos", "Otros NE", "No identificado", "No especificado"))

##Revisión NAs
sum(is.na(ppl_17_ff$entidad))
sum(is.na(ppl_17_ff$estatus))
sum(is.na(ppl_17_ff$fuero))
sum(is.na(ppl_17_ff$delito))
sum(is.na(ppl_17_ff$sexo))
sum(is.na(ppl_17_ff$total))
sum(is.na(ppl_17_ff$year))

#which(is.na(ppl_17$total))

#Chequeo de totales
sum(ppl_17_ff$total) #65,229 (antes de filtro de delitos)

#pegando ambos fueros#
ppl_17 <- bind_rows(ppl_17_fc, ppl_17_ff)
sum(is.na(ppl_17$entidad))
sum(is.na(ppl_17$estatus))
sum(is.na(ppl_17$fuero))
sum(is.na(ppl_17$delito))
sum(is.na(ppl_17$sexo))
sum(is.na(ppl_17$total))
sum(is.na(ppl_17$year))

ppl_17$sexo <- as.factor(ppl_17$sexo)
ppl_17$estatus <- as.factor(ppl_17$estatus)
ppl_17$fuero <- as.factor(ppl_17$fuero)


#### Datos 2018 ####
# fuero común #
ppl_18_fc <- read_csv(paste(inp, "RECLTFCO_18.csv", sep = "/"))%>%
  mutate_if(is.character, as.numeric)

ppl_18_fc <- select(ppl_18_fc, ENTIDAD, TIPODELI, ESJURD1, ESJURD2, ESJURD4, ESJURD5, ESJURD7, ESJURD8, ESJURD10, ESJURD11)%>%
  dplyr::rename(entidad = ENTIDAD,
                delito = TIPODELI,
                sin_sentencia_hombres = ESJURD1,
                sin_sentencia_mujeres = ESJURD2,
                con_sentencia_hombres_1 = ESJURD4,
                con_sentencia_mujeres_1 = ESJURD5,
                con_sentencia_hombres_2 = ESJURD7,
                con_sentencia_mujeres_2 = ESJURD8,
                con_sentencia_hombres_3 = ESJURD10,
                con_sentencia_mujeres_3 = ESJURD11)%>%
  select(entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres,
         con_sentencia_hombres_1, con_sentencia_hombres_2, con_sentencia_hombres_3,
         con_sentencia_mujeres_1, con_sentencia_mujeres_2, con_sentencia_mujeres_3)

ppl_18_fc <- mutate(ppl_18_fc, con_sentencia_hombres = rowSums(ppl_18_fc[,5:7], na.rm=T))
ppl_18_fc <- mutate(ppl_18_fc, con_sentencia_mujeres = rowSums(ppl_18_fc[,8:10], na.rm=T))
ppl_18_fc <- select(ppl_18_fc, entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres, con_sentencia_hombres, con_sentencia_mujeres)

ppl_18_fc <- pivot_longer(ppl_18_fc, cols = c(sin_sentencia_hombres:con_sentencia_mujeres),
                          names_to = "estatus", values_to = "total")

ppl_18_fc <- mutate(ppl_18_fc, sexo = case_when(estatus == "con_sentencia_hombres" ~ "Hombres",
                                                estatus == "sin_sentencia_hombres" ~ "Hombres",
                                                estatus == "con_sentencia_mujeres" ~ "Mujeres",
                                                estatus == "sin_sentencia_mujeres" ~ "Mujeres"))

ppl_18_fc <- mutate(ppl_18_fc, estatus = case_when(estatus == "con_sentencia_hombres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_hombres" ~ "Sin sentencia",
                                                   estatus == "con_sentencia_mujeres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_mujeres" ~ "Sin sentencia"))

ppl_18_fc <- mutate(ppl_18_fc, year = "2018", fuero = "Fuero común")
ppl_18_fc <- select(ppl_18_fc, entidad, sexo, delito, estatus, fuero, total, year)
ppl_18_fc <- mutate(ppl_18_fc, total = replace_na(total,0))

ppl_18_fc$entidad <- factor(ppl_18_fc$entidad, 
                            levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                       11, 12, 13, 14, 15, 16, 17, 
                                       18, 19, 20, 21 , 22, 23, 24, 
                                       25, 26, 27, 28, 29, 30, 31, 32),
                            labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                       "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                       "Ciudad de México", "Durango", "Guanajuato", 
                                       "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                       "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                       "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                       "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                       "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

#Filtro a delitos sin desagregación
ppl_18_fc <- filter(ppl_18_fc, delito >= 10100 &  delito <= 20600 | delito >= 20700 &  delito <= 30400 |
                          delito >= 30500 &  delito <= 39999 | delito >= 40101 &  delito <= 40199 | delito == 40200 | delito >= 40300 &  delito <= 40500 |
                          delito >= 40600 &  delito <= 60100 | delito == 60200 | delito >= 60300 &  delito <= 69999 | delito >= 70101 &  delito <= 70199 |
                          delito >= 70201 &  delito <= 70299 | delito >= 70300 &  delito <= 70600 | delito >= 70700 &  delito <= 79999 |
                          delito >= 80101 &  delito <= 80199 | delito == 80200 | delito >= 80300 &  delito <= 90500 | delito == 90600 | delito == 90700 |
                          delito >= 90800 &  delito <= 91600 | delito >= 91700 &  delito <= 999999)

ppl_18_fc$delito <- factor(ppl_18_fc$delito, 
                               levels = c(10100, 10200, 10300, 10400, 10500, 19999, 20100, 20200, 20300, 20400, 20500, 20600,
                                          20700, 29999, 30100, 30200, 30300, 30400, 30500, 30600, 30700, 39999, 40101, 40102, 40103,
                                          40104, 40105, 40106, 40107, 40108, 40109, 40110, 40111, 40112, 40113, 40114, 40115, 40116,
                                          40117, 40118, 40199, 40200, 40300, 40400, 40500, 40600, 40700, 40800, 49999, 50100, 50200, 50300, 59999, 60100, 60200,
                                          60300, 60400, 60500, 60600, 69999, 70101, 70102, 70103, 70104, 70105, 70106, 70199, 
                                          70201, 70202, 70203, 70204, 70205, 70206, 70207, 70208, 70299, 70300, 70400, 70500, 70600, 70700, 70800,
                                          70900, 79999, 80101, 80102, 80103, 80104, 80105, 80106, 80107, 80108, 80109, 80199, 80200, 80300, 80400, 80500,
                                          89999, 90100, 90200, 90300, 90400, 90500, 90600, 90700, 90800, 90900,
                                          91000, 91100, 91200, 91300, 91400, 91500, 91600, 91700, 91800, 99999, 100100, 999999),
                               labels = c("Homicidio", "Feminicidio", "Aborto", "Lesiones", "Otros contra la vida y la integridad corporal",
                                          "La vida y la integridad corporal NE", "Privación de la libertad", "Tráfico de menores", "Retención o sustracción de menores e incapaces",
                                          "Rapto", "Desaparición forzada de personas", "Secuestro", "Otros que atentan contra la libertad personal",
                                          "La libertad personal NE", "Abuso sexual", "Acoso sexual", "Hostigamiento sexual", "Violación", "Estupro",
                                          "Incesto", "Otros contra la libertad y la seguridad sexual", "La libertad y la seguridad sexual NE",
                                          "Robo simple", "Robo a casa habitación", "Robo de vehículo", "Robo de autopartes", "Robo a transeúnte en vía pública", 
                                          "Robo a transeúnte en espacio abierto al público", "Robo a persona en un lugar privado", "Robo a transportista",
                                          "Robo en transporte público individual", "Robo en transporte público colectivo", "Robo en transporte individual",
                                          "Robo a institución bancaria", "Robo a negocio", "Robo de ganado", "Robo de maquinaria", "Robo de energía eléctrica",
                                          "Otros robos", "Robo NI", "Robo NE", 
                                          "Hidrocarburos y sus derivados", "Fraude", "Abuso de confianza", "Extorsión", "Daño a la propiedad",
                                          "Despojo", "Otros contra el patrimonio", "El patrimonio NE", "Violencia familiar", "Incumplimiento de obligaciones familiares",
                                          "La familia - Otros", "La familia NE", "Contra el libre desarrollo de la personalidad", "Trata de personas", "Violencia de género distinta a la violencia familiar",
                                          "Discriminación", "Lenocinio", "Otros contra la sociedad", "La sociedad NE", 
                                          "Posesión simple de narcóticos", "Posesión con fines de comercio o suministro de narcóticos", "Comercio de narcóticos FC",
                                          "Suministro de narcóticos FC", "Otros narcomenudeo", "Narcomenudeo NI", "Narcomenudeo NE", "Producción de narcóticos",
                                          "Transporte de narcóticos", "Tráfico de narcóticos", "Comercio de narcóticos FF", "Suministro de narcóticos FF", "Posesión de narcóticos",
                                          "Otros narcóticos", "Narcotráfico NI", "Narcotráfico NE", "Evasión de presos", "En materia de armas y objetos prohibidos", "Delincuencia organizada", "Armas, explosivos y otros materiales",
                                          "Asociación delictuosa", "Terrorismo", "Otros contra la seguridad pública", "La seguridad pública NE",
                                          "Ejercicio indebido del servicio público", "Abuso de autoridad", "Cohecho", "Peculado", "Enriquecimiento ilícito", "Ejercicio abusivo de funciones",
                                          "Tráfico de influencias", "Otros corrupción", "Corrupción NI", "Corrupción NE","Contra la administración de justicia", "Delitos en materia fiscal", "Delitos electorales", "Otros delitos contra la administración del Estado",
                                          "La administración del Estado NE", "Amenazas", "Allanamiento de morada", "Falsedad",
                                          "Falsificación", "Delitos contra el medio ambiente", "En materia de vías de comunicación", "En materia de migración",
                                          "En materia de derechos de autor", "En materia de instituciones de crédito", "En materia de propiedad industrial",
                                          "Contra la salud no relacionados con narcóticos", "Encubrimiento", "Operaciones con recursos de procedencia ilícita",
                                          "Tortura", "Suplantación y usurpación de identidad", "Contra la seguridad de los datos", "Tratos o penas crueles",
                                          "Otros delitos", "Otros NE", "No identificado", "No especificado"))

##Revisión NAs
sum(is.na(ppl_18_fc$entidad))
sum(is.na(ppl_18_fc$estatus))
sum(is.na(ppl_18_fc$fuero))
sum(is.na(ppl_18_fc$delito))
sum(is.na(ppl_18_fc$sexo))
sum(is.na(ppl_18_fc$total))
sum(is.na(ppl_18_fc$year))

#which(is.na(ppl_17$total))

#Chequeo de totales
sum(ppl_18_fc$total) #388,882 (antes de filtro de delitos)

# fuero federal #
ppl_18_ff <- read_csv(paste(inp, "RECLTIFU_18.csv", sep = "/"))%>%
  mutate_if(is.character, as.numeric)

ppl_18_ff <- select(ppl_18_ff, ENTIDAD, TIPODELI, ESJURD1, ESJURD2, ESJURD4, ESJURD5, ESJURD7, ESJURD8, ESJURD10, ESJURD11)%>%
  dplyr::rename(entidad = ENTIDAD,
                delito = TIPODELI,
                sin_sentencia_hombres = ESJURD1,
                sin_sentencia_mujeres = ESJURD2,
                con_sentencia_hombres_1 = ESJURD4,
                con_sentencia_mujeres_1 = ESJURD5,
                con_sentencia_hombres_2 = ESJURD7,
                con_sentencia_mujeres_2 = ESJURD8,
                con_sentencia_hombres_3 = ESJURD10,
                con_sentencia_mujeres_3 = ESJURD11)%>%
  select(entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres,
         con_sentencia_hombres_1, con_sentencia_hombres_2, con_sentencia_hombres_3,
         con_sentencia_mujeres_1, con_sentencia_mujeres_2, con_sentencia_mujeres_3)

ppl_18_ff <- mutate(ppl_18_ff, con_sentencia_hombres = rowSums(ppl_18_ff[,5:7], na.rm=T))
ppl_18_ff <- mutate(ppl_18_ff, con_sentencia_mujeres = rowSums(ppl_18_ff[,8:10], na.rm=T))
ppl_18_ff <- select(ppl_18_ff, entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres, con_sentencia_hombres, con_sentencia_mujeres)

ppl_18_ff <- pivot_longer(ppl_18_ff, cols = c(sin_sentencia_hombres:con_sentencia_mujeres),
                          names_to = "estatus", values_to = "total")

ppl_18_ff <- mutate(ppl_18_ff, sexo = case_when(estatus == "con_sentencia_hombres" ~ "Hombres",
                                                estatus == "sin_sentencia_hombres" ~ "Hombres",
                                                estatus == "con_sentencia_mujeres" ~ "Mujeres",
                                                estatus == "sin_sentencia_mujeres" ~ "Mujeres"))

ppl_18_ff <- mutate(ppl_18_ff, estatus = case_when(estatus == "con_sentencia_hombres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_hombres" ~ "Sin sentencia",
                                                   estatus == "con_sentencia_mujeres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_mujeres" ~ "Sin sentencia"))

ppl_18_ff <- mutate(ppl_18_ff, year = "2018", fuero = "Fuero federal")
ppl_18_ff <- select(ppl_18_ff, entidad, sexo, delito, estatus, fuero, total, year)
ppl_18_ff <- mutate(ppl_18_ff, total = replace_na(total,0))

ppl_18_ff$entidad <- factor(ppl_18_ff$entidad, 
                            levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                       11, 12, 13, 14, 15, 16, 17, 
                                       18, 19, 20, 21 , 22, 23, 24, 
                                       25, 26, 27, 28, 29, 30, 31, 32),
                            labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                       "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                       "Ciudad de México", "Durango", "Guanajuato", 
                                       "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                       "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                       "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                       "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                       "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

#Filtro a delitos sin desagregación
ppl_18_ff <- filter(ppl_18_ff, delito >= 10100 &  delito <= 20600 | delito >= 20700 &  delito <= 30400 |
                      delito >= 30500 &  delito <= 39999 | delito >= 40101 &  delito <= 40199 | delito == 40200 | delito >= 40300 &  delito <= 40500 |
                      delito >= 40600 &  delito <= 60100 | delito == 60200 | delito >= 60300 &  delito <= 69999 | delito >= 70101 &  delito <= 70199 |
                      delito >= 70201 &  delito <= 70299 | delito >= 70300 &  delito <= 70600 | delito >= 70700 &  delito <= 79999 |
                      delito >= 80101 &  delito <= 80199 | delito == 80200 | delito >= 80300 &  delito <= 90500 | delito == 90600 | delito == 90700 |
                      delito >= 90800 &  delito <= 91600 | delito >= 91700 &  delito <= 999999)

ppl_18_ff$delito <- factor(ppl_18_ff$delito, 
                           levels = c(10100, 10200, 10300, 10400, 10500, 19999, 20100, 20200, 20300, 20400, 20500, 20600,
                                      20700, 29999, 30100, 30200, 30300, 30400, 30500, 30600, 30700, 39999, 40101, 40102, 40103,
                                      40104, 40105, 40106, 40107, 40108, 40109, 40110, 40111, 40112, 40113, 40114, 40115, 40116,
                                      40117, 40118, 40199, 40200, 40300, 40400, 40500, 40600, 40700, 40800, 49999, 50100, 50200, 50300, 59999, 60100, 60200,
                                      60300, 60400, 60500, 60600, 69999, 70101, 70102, 70103, 70104, 70105, 70106, 70199, 
                                      70201, 70202, 70203, 70204, 70205, 70206, 70207, 70208, 70299, 70300, 70400, 70500, 70600, 70700, 70800,
                                      70900, 79999, 80101, 80102, 80103, 80104, 80105, 80106, 80107, 80108, 80109, 80199, 80200, 80300, 80400, 80500,
                                      89999, 90100, 90200, 90300, 90400, 90500, 90600, 90700, 90800, 90900,
                                      91000, 91100, 91200, 91300, 91400, 91500, 91600, 91700, 91800, 99999, 100100, 999999),
                           labels = c("Homicidio", "Feminicidio", "Aborto", "Lesiones", "Otros contra la vida y la integridad corporal",
                                      "La vida y la integridad corporal NE", "Privación de la libertad", "Tráfico de menores", "Retención o sustracción de menores e incapaces",
                                      "Rapto", "Desaparición forzada de personas", "Secuestro", "Otros que atentan contra la libertad personal",
                                      "La libertad personal NE", "Abuso sexual", "Acoso sexual", "Hostigamiento sexual", "Violación", "Estupro",
                                      "Incesto", "Otros contra la libertad y la seguridad sexual", "La libertad y la seguridad sexual NE",
                                      "Robo simple", "Robo a casa habitación", "Robo de vehículo", "Robo de autopartes", "Robo a transeúnte en vía pública", 
                                      "Robo a transeúnte en espacio abierto al público", "Robo a persona en un lugar privado", "Robo a transportista",
                                      "Robo en transporte público individual", "Robo en transporte público colectivo", "Robo en transporte individual",
                                      "Robo a institución bancaria", "Robo a negocio", "Robo de ganado", "Robo de maquinaria", "Robo de energía eléctrica",
                                      "Otros robos", "Robo NI", "Robo NE", 
                                      "Hidrocarburos y sus derivados", "Fraude", "Abuso de confianza", "Extorsión", "Daño a la propiedad",
                                      "Despojo", "Otros contra el patrimonio", "El patrimonio NE", "Violencia familiar", "Incumplimiento de obligaciones familiares",
                                      "La familia - Otros", "La familia NE", "Contra el libre desarrollo de la personalidad", "Trata de personas", "Violencia de género distinta a la violencia familiar",
                                      "Discriminación", "Lenocinio", "Otros contra la sociedad", "La sociedad NE", 
                                      "Posesión simple de narcóticos", "Posesión con fines de comercio o suministro de narcóticos", "Comercio de narcóticos FC",
                                      "Suministro de narcóticos FC", "Otros narcomenudeo", "Narcomenudeo NI", "Narcomenudeo NE", "Producción de narcóticos",
                                      "Transporte de narcóticos", "Tráfico de narcóticos", "Comercio de narcóticos FF", "Suministro de narcóticos FF", "Posesión de narcóticos",
                                      "Otros narcóticos", "Narcotráfico NI", "Narcotráfico NE", "Evasión de presos", "En materia de armas y objetos prohibidos", "Delincuencia organizada", "Armas, explosivos y otros materiales",
                                      "Asociación delictuosa", "Terrorismo", "Otros contra la seguridad pública", "La seguridad pública NE",
                                      "Ejercicio indebido del servicio público", "Abuso de autoridad", "Cohecho", "Peculado", "Enriquecimiento ilícito", "Ejercicio abusivo de funciones",
                                      "Tráfico de influencias", "Otros corrupción", "Corrupción NI", "Corrupción NE","Contra la administración de justicia", "Delitos en materia fiscal", "Delitos electorales", "Otros delitos contra la administración del Estado",
                                      "La administración del Estado NE", "Amenazas", "Allanamiento de morada", "Falsedad",
                                      "Falsificación", "Delitos contra el medio ambiente", "En materia de vías de comunicación", "En materia de migración",
                                      "En materia de derechos de autor", "En materia de instituciones de crédito", "En materia de propiedad industrial",
                                      "Contra la salud no relacionados con narcóticos", "Encubrimiento", "Operaciones con recursos de procedencia ilícita",
                                      "Tortura", "Suplantación y usurpación de identidad", "Contra la seguridad de los datos", "Tratos o penas crueles",
                                      "Otros delitos", "Otros NE", "No identificado", "No especificado"))

##Revisión NAs
sum(is.na(ppl_18_ff$entidad))
sum(is.na(ppl_18_ff$estatus))
sum(is.na(ppl_18_ff$fuero))
sum(is.na(ppl_18_ff$delito))
sum(is.na(ppl_18_ff$sexo))
sum(is.na(ppl_18_ff$total))
sum(is.na(ppl_18_ff$year))

#which(is.na(ppl_18$total))

#Chequeo de totales
sum(ppl_18_ff$total) #66,261 (antes de filtro de delitos)

#pegando ambos fueros#
ppl_18 <- bind_rows(ppl_18_fc, ppl_18_ff)
sum(is.na(ppl_18$entidad))
sum(is.na(ppl_18$estatus))
sum(is.na(ppl_18$fuero))
sum(is.na(ppl_18$delito))
sum(is.na(ppl_18$sexo))
sum(is.na(ppl_18$total))
sum(is.na(ppl_18$year))

ppl_18$entidad <- as.factor(ppl_18$entidad)
ppl_18$sexo <- as.factor(ppl_18$sexo)
ppl_18$estatus <- as.factor(ppl_18$estatus)
ppl_18$fuero <- as.factor(ppl_18$fuero)

#### Datos 2019 ####
# fuero común #
ppl_19_fc <- read_csv(paste(inp, "m3s1p86_cngspspe2020.csv", sep = "/"))%>%
  mutate_if(is.character, as.numeric)

ppl_19_fc <- select(ppl_19_fc, entidad_a, tipdelit_b, esjucs3, esjucs4,
                    esjucs6, esjucs7, esjucs9, esjucs10)%>%
  dplyr::rename(entidad = entidad_a,
                delito = tipdelit_b,
                sin_sentencia_hombres = esjucs3,
                sin_sentencia_mujeres = esjucs4,
                con_sentencia_hombres_1 = esjucs6,
                con_sentencia_mujeres_1 = esjucs7,
                con_sentencia_hombres_2 = esjucs9,
                con_sentencia_mujeres_2 = esjucs10)%>%
  select(entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres,
         con_sentencia_hombres_1, con_sentencia_hombres_2, 
         con_sentencia_mujeres_1, con_sentencia_mujeres_2)

ppl_19_fc <- mutate(ppl_19_fc, con_sentencia_hombres = rowSums(ppl_19_fc[,5:6], na.rm=T))
ppl_19_fc <- mutate(ppl_19_fc, con_sentencia_mujeres = rowSums(ppl_19_fc[,7:8], na.rm=T))
ppl_19_fc <- select(ppl_19_fc, entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres, con_sentencia_hombres, con_sentencia_mujeres)

ppl_19_fc <- pivot_longer(ppl_19_fc, cols = c(sin_sentencia_hombres:con_sentencia_mujeres),
                          names_to = "estatus", values_to = "total")

ppl_19_fc <- mutate(ppl_19_fc, sexo = case_when(estatus == "con_sentencia_hombres" ~ "Hombres",
                                                estatus == "sin_sentencia_hombres" ~ "Hombres",
                                                estatus == "con_sentencia_mujeres" ~ "Mujeres",
                                                estatus == "sin_sentencia_mujeres" ~ "Mujeres"))

ppl_19_fc <- mutate(ppl_19_fc, estatus = case_when(estatus == "con_sentencia_hombres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_hombres" ~ "Sin sentencia",
                                                   estatus == "con_sentencia_mujeres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_mujeres" ~ "Sin sentencia"))

ppl_19_fc <- mutate(ppl_19_fc, year = "2019", fuero = "Fuero común")
ppl_19_fc <- select(ppl_19_fc, entidad, sexo, delito, estatus, fuero, total, year)
ppl_19_fc <- mutate(ppl_19_fc, total = replace_na(total,0))

ppl_19_fc$entidad <- factor(ppl_19_fc$entidad, 
                            levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                       11, 12, 13, 14, 15, 16, 17, 
                                       18, 19, 20, 21 , 22, 23, 24, 
                                       25, 26, 27, 28, 29, 30, 31, 32),
                            labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                       "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                       "Ciudad de México", "Durango", "Guanajuato", 
                                       "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                       "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                       "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                       "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                       "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

#Filtro a delitos sin desagregación
ppl_19_fc <- filter(ppl_19_fc, delito >= 10100 &  delito <= 20600 | delito >= 20700 &  delito <= 30400 |
                          delito >= 30500 &  delito <= 39999 | delito >= 40101 &  delito <= 40199 | delito == 40200 | delito >= 40300 &  delito <= 40500 |
                          delito >= 40600 &  delito <= 60100 | delito == 60200 | delito >= 60300 &  delito <= 69999 | delito >= 70101 &  delito <= 70199 |
                          delito >= 70201 &  delito <= 70299 | delito >= 70300 &  delito <= 70600 | delito >= 70700 &  delito <= 79999 |
                          delito >= 80101 &  delito <= 80199 | delito == 80200 | delito >= 80300 &  delito <= 90500 | delito == 90600 | delito == 90700 |
                          delito >= 90800 &  delito <= 91600 | delito >= 91700 &  delito <= 999999)

ppl_19_fc$delito <- factor(ppl_19_fc$delito, 
                               levels = c(10100, 10200, 10300, 10400, 10500, 19999, 20100, 20200, 20300, 20400, 20500, 20600,
                                          20700, 29999, 30100, 30200, 30300, 30400, 30500, 30600, 30700, 39999, 40101, 40102, 40103,
                                          40104, 40105, 40106, 40107, 40108, 40109, 40110, 40111, 40112, 40113, 40114, 40115, 40116,
                                          40117, 40118, 40199, 40200, 40300, 40400, 40500, 40600, 40700, 40800, 49999, 50100, 50200, 50300, 59999, 60100, 60200,
                                          60300, 60400, 60500, 60600, 69999, 70101, 70102, 70103, 70104, 70105, 70106, 70199, 
                                          70201, 70202, 70203, 70204, 70205, 70206, 70207, 70208, 70299, 70300, 70400, 70500, 70600, 70700, 70800,
                                          70900, 79999, 80101, 80102, 80103, 80104, 80105, 80106, 80107, 80108, 80109, 80199, 80200, 80300, 80400, 80500,
                                          89999, 90100, 90200, 90300, 90400, 90500, 90600, 90700, 90800, 90900,
                                          91000, 91100, 91200, 91300, 91400, 91500, 91600, 91700, 91800, 99999, 100100, 999999),
                               labels = c("Homicidio", "Feminicidio", "Aborto", "Lesiones", "Otros contra la vida y la integridad corporal",
                                          "La vida y la integridad corporal NE", "Privación de la libertad", "Tráfico de menores", "Retención o sustracción de menores e incapaces",
                                          "Rapto", "Desaparición forzada de personas", "Secuestro", "Otros que atentan contra la libertad personal",
                                          "La libertad personal NE", "Abuso sexual", "Acoso sexual", "Hostigamiento sexual", "Violación", "Estupro",
                                          "Incesto", "Otros contra la libertad y la seguridad sexual", "La libertad y la seguridad sexual NE",
                                          "Robo simple", "Robo a casa habitación", "Robo de vehículo", "Robo de autopartes", "Robo a transeúnte en vía pública", 
                                          "Robo a transeúnte en espacio abierto al público", "Robo a persona en un lugar privado", "Robo a transportista",
                                          "Robo en transporte público individual", "Robo en transporte público colectivo", "Robo en transporte individual",
                                          "Robo a institución bancaria", "Robo a negocio", "Robo de ganado", "Robo de maquinaria", "Robo de energía eléctrica",
                                          "Otros robos", "Robo NI", "Robo NE", 
                                          "Hidrocarburos y sus derivados", "Fraude", "Abuso de confianza", "Extorsión", "Daño a la propiedad",
                                          "Despojo", "Otros contra el patrimonio", "El patrimonio NE", "Violencia familiar", "Incumplimiento de obligaciones familiares",
                                          "La familia - Otros", "La familia NE", "Contra el libre desarrollo de la personalidad", "Trata de personas", "Violencia de género distinta a la violencia familiar",
                                          "Discriminación", "Lenocinio", "Otros contra la sociedad", "La sociedad NE", 
                                          "Posesión simple de narcóticos", "Posesión con fines de comercio o suministro de narcóticos", "Comercio de narcóticos FC",
                                          "Suministro de narcóticos FC", "Otros narcomenudeo", "Narcomenudeo NI", "Narcomenudeo NE", "Producción de narcóticos",
                                          "Transporte de narcóticos", "Tráfico de narcóticos", "Comercio de narcóticos FF", "Suministro de narcóticos FF", "Posesión de narcóticos",
                                          "Otros narcóticos", "Narcotráfico NI", "Narcotráfico NE", "Evasión de presos", "En materia de armas y objetos prohibidos", "Delincuencia organizada", "Armas, explosivos y otros materiales",
                                          "Asociación delictuosa", "Terrorismo", "Otros contra la seguridad pública", "La seguridad pública NE",
                                          "Ejercicio indebido del servicio público", "Abuso de autoridad", "Cohecho", "Peculado", "Enriquecimiento ilícito", "Ejercicio abusivo de funciones",
                                          "Tráfico de influencias", "Otros corrupción", "Corrupción NI", "Corrupción NE","Contra la administración de justicia", "Delitos en materia fiscal", "Delitos electorales", "Otros delitos contra la administración del Estado",
                                          "La administración del Estado NE", "Amenazas", "Allanamiento de morada", "Falsedad",
                                          "Falsificación", "Delitos contra el medio ambiente", "En materia de vías de comunicación", "En materia de migración",
                                          "En materia de derechos de autor", "En materia de instituciones de crédito", "En materia de propiedad industrial",
                                          "Contra la salud no relacionados con narcóticos", "Encubrimiento", "Operaciones con recursos de procedencia ilícita",
                                          "Tortura", "Suplantación y usurpación de identidad", "Contra la seguridad de los datos", "Tratos o penas crueles",
                                          "Otros delitos", "Otros NE", "No identificado", "No especificado"))


##Revisión NAs
sum(is.na(ppl_19_fc$entidad))
sum(is.na(ppl_19_fc$delito))
sum(is.na(ppl_19_fc$sexo))
sum(is.na(ppl_19_fc$estatus))
sum(is.na(ppl_19_fc$total))
sum(is.na(ppl_19_fc$year))
sum(is.na(ppl_19_fc$fuero))

#which(is.na(ppl_19_fc$entidad))

#Chequeo de totales
ppl_19_fc$total <- as.numeric(ppl_19_fc$total)
ppl_19_fc <- mutate(ppl_19_fc, total = replace_na(total,0))
sum(ppl_19_fc$total) #263,690

#fuero federal
ppl_19_ff <- read_csv(paste(inp, "m3s1p87_cngspspe2020.csv", sep = "/"))%>%
  mutate_if(is.character, as.numeric)

ppl_19_ff <- select(ppl_19_ff, entidad_a, tipdelit_b, esjucs3, esjucs4,
                    esjucs6, esjucs7, esjucs9, esjucs10)%>%
  dplyr::rename(entidad = entidad_a,
                delito = tipdelit_b,
                sin_sentencia_hombres = esjucs3,
                sin_sentencia_mujeres = esjucs4,
                con_sentencia_hombres_1 = esjucs6,
                con_sentencia_mujeres_1 = esjucs7,
                con_sentencia_hombres_2 = esjucs9,
                con_sentencia_mujeres_2 = esjucs10)%>%
  select(entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres,
         con_sentencia_hombres_1, con_sentencia_hombres_2, 
         con_sentencia_mujeres_1, con_sentencia_mujeres_2)

ppl_19_ff <- mutate(ppl_19_ff, con_sentencia_hombres = rowSums(ppl_19_ff[,5:6], na.rm=T))
ppl_19_ff <- mutate(ppl_19_ff, con_sentencia_mujeres = rowSums(ppl_19_ff[,7:8], na.rm=T))
ppl_19_ff <- select(ppl_19_ff, entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres, con_sentencia_hombres, con_sentencia_mujeres)

ppl_19_ff <- pivot_longer(ppl_19_ff, cols = c(sin_sentencia_hombres:con_sentencia_mujeres),
                          names_to = "estatus", values_to = "total")

ppl_19_ff <- mutate(ppl_19_ff, sexo = case_when(estatus == "con_sentencia_hombres" ~ "Hombres",
                                                estatus == "sin_sentencia_hombres" ~ "Hombres",
                                                estatus == "con_sentencia_mujeres" ~ "Mujeres",
                                                estatus == "sin_sentencia_mujeres" ~ "Mujeres"))

ppl_19_ff <- mutate(ppl_19_ff, estatus = case_when(estatus == "con_sentencia_hombres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_hombres" ~ "Sin sentencia",
                                                   estatus == "con_sentencia_mujeres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_mujeres" ~ "Sin sentencia"))

ppl_19_ff <- mutate(ppl_19_ff, year = "2019", fuero = "Fuero federal")
ppl_19_ff <- select(ppl_19_ff, entidad, sexo, delito, estatus, fuero, total, year)
ppl_19_ff <- mutate(ppl_19_ff, total = replace_na(total,0))

ppl_19_ff$entidad <- factor(ppl_19_ff$entidad, 
                            levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                       11, 12, 13, 14, 15, 16, 17, 
                                       18, 19, 20, 21 , 22, 23, 24, 
                                       25, 26, 27, 28, 29, 30, 31, 32),
                            labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                       "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                       "Ciudad de México", "Durango", "Guanajuato", 
                                       "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                       "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                       "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                       "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                       "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

#Filtro a delitos sin desagregación
ppl_19_ff <- filter(ppl_19_ff, delito >= 10100 &  delito <= 20600 | delito >= 20700 &  delito <= 30400 |
                      delito >= 30500 &  delito <= 39999 | delito >= 40101 &  delito <= 40199 | delito == 40200 | delito >= 40300 &  delito <= 40500 |
                      delito >= 40600 &  delito <= 60100 | delito == 60200 | delito >= 60300 &  delito <= 69999 | delito >= 70101 &  delito <= 70199 |
                      delito >= 70201 &  delito <= 70299 | delito >= 70300 &  delito <= 70600 | delito >= 70700 &  delito <= 79999 |
                      delito >= 80101 &  delito <= 80199 | delito == 80200 | delito >= 80300 &  delito <= 90500 | delito == 90600 | delito == 90700 |
                      delito >= 90800 &  delito <= 91600 | delito >= 91700 &  delito <= 999999)

ppl_19_ff$delito <- factor(ppl_19_ff$delito, 
                           levels = c(10100, 10200, 10300, 10400, 10500, 19999, 20100, 20200, 20300, 20400, 20500, 20600,
                                      20700, 29999, 30100, 30200, 30300, 30400, 30500, 30600, 30700, 39999, 40101, 40102, 40103,
                                      40104, 40105, 40106, 40107, 40108, 40109, 40110, 40111, 40112, 40113, 40114, 40115, 40116,
                                      40117, 40118, 40199, 40200, 40300, 40400, 40500, 40600, 40700, 40800, 49999, 50100, 50200, 50300, 59999, 60100, 60200,
                                      60300, 60400, 60500, 60600, 69999, 70101, 70102, 70103, 70104, 70105, 70106, 70199, 
                                      70201, 70202, 70203, 70204, 70205, 70206, 70207, 70208, 70299, 70300, 70400, 70500, 70600, 70700, 70800,
                                      70900, 79999, 80101, 80102, 80103, 80104, 80105, 80106, 80107, 80108, 80109, 80199, 80200, 80300, 80400, 80500,
                                      89999, 90100, 90200, 90300, 90400, 90500, 90600, 90700, 90800, 90900,
                                      91000, 91100, 91200, 91300, 91400, 91500, 91600, 91700, 91800, 99999, 100100, 999999),
                           labels = c("Homicidio", "Feminicidio", "Aborto", "Lesiones", "Otros contra la vida y la integridad corporal",
                                      "La vida y la integridad corporal NE", "Privación de la libertad", "Tráfico de menores", "Retención o sustracción de menores e incapaces",
                                      "Rapto", "Desaparición forzada de personas", "Secuestro", "Otros que atentan contra la libertad personal",
                                      "La libertad personal NE", "Abuso sexual", "Acoso sexual", "Hostigamiento sexual", "Violación", "Estupro",
                                      "Incesto", "Otros contra la libertad y la seguridad sexual", "La libertad y la seguridad sexual NE",
                                      "Robo simple", "Robo a casa habitación", "Robo de vehículo", "Robo de autopartes", "Robo a transeúnte en vía pública", 
                                      "Robo a transeúnte en espacio abierto al público", "Robo a persona en un lugar privado", "Robo a transportista",
                                      "Robo en transporte público individual", "Robo en transporte público colectivo", "Robo en transporte individual",
                                      "Robo a institución bancaria", "Robo a negocio", "Robo de ganado", "Robo de maquinaria", "Robo de energía eléctrica",
                                      "Otros robos", "Robo NI", "Robo NE", 
                                      "Hidrocarburos y sus derivados", "Fraude", "Abuso de confianza", "Extorsión", "Daño a la propiedad",
                                      "Despojo", "Otros contra el patrimonio", "El patrimonio NE", "Violencia familiar", "Incumplimiento de obligaciones familiares",
                                      "La familia - Otros", "La familia NE", "Contra el libre desarrollo de la personalidad", "Trata de personas", "Violencia de género distinta a la violencia familiar",
                                      "Discriminación", "Lenocinio", "Otros contra la sociedad", "La sociedad NE", 
                                      "Posesión simple de narcóticos", "Posesión con fines de comercio o suministro de narcóticos", "Comercio de narcóticos ff",
                                      "Suministro de narcóticos ff", "Otros narcomenudeo", "Narcomenudeo NI", "Narcomenudeo NE", "Producción de narcóticos",
                                      "Transporte de narcóticos", "Tráfico de narcóticos", "Comercio de narcóticos FF", "Suministro de narcóticos FF", "Posesión de narcóticos",
                                      "Otros narcóticos", "Narcotráfico NI", "Narcotráfico NE", "Evasión de presos", "En materia de armas y objetos prohibidos", "Delincuencia organizada", "Armas, explosivos y otros materiales",
                                      "Asociación delictuosa", "Terrorismo", "Otros contra la seguridad pública", "La seguridad pública NE",
                                      "Ejercicio indebido del servicio público", "Abuso de autoridad", "Cohecho", "Peculado", "Enriquecimiento ilícito", "Ejercicio abusivo de funciones",
                                      "Tráfico de influencias", "Otros corrupción", "Corrupción NI", "Corrupción NE","Contra la administración de justicia", "Delitos en materia fiscal", "Delitos electorales", "Otros delitos contra la administración del Estado",
                                      "La administración del Estado NE", "Amenazas", "Allanamiento de morada", "Falsedad",
                                      "Falsificación", "Delitos contra el medio ambiente", "En materia de vías de comunicación", "En materia de migración",
                                      "En materia de derechos de autor", "En materia de instituciones de crédito", "En materia de propiedad industrial",
                                      "Contra la salud no relacionados con narcóticos", "Encubrimiento", "Operaciones con recursos de procedencia ilícita",
                                      "Tortura", "Suplantación y usurpación de identidad", "Contra la seguridad de los datos", "Tratos o penas crueles",
                                      "Otros delitos", "Otros NE", "No identificado", "No especificado"))


##Revisión NAs
sum(is.na(ppl_19_ff$entidad))
sum(is.na(ppl_19_ff$delito))
sum(is.na(ppl_19_ff$sexo))
sum(is.na(ppl_19_ff$estatus))
sum(is.na(ppl_19_ff$total))
sum(is.na(ppl_19_ff$year))
sum(is.na(ppl_19_ff$fuero))

#which(is.na(ppl_19_fc$entidad))

#Chequeo de totales
ppl_19_ff$total <- as.numeric(ppl_19_ff$total)
ppl_19_ff <- mutate(ppl_19_ff, total = replace_na(total,0))
sum(ppl_19_ff$total) #27,210

#pegando ambos fueros#
ppl_19 <- bind_rows(ppl_19_fc, ppl_19_ff)
sum(is.na(ppl_19$entidad))
sum(is.na(ppl_19$estatus))
sum(is.na(ppl_19$fuero))
sum(is.na(ppl_19$delito))
sum(is.na(ppl_19$sexo))
sum(is.na(ppl_19$total))
sum(is.na(ppl_19$year))

#### Datos 2020 ####
# fuero común #
ppl_20_fc <- read_csv(paste(inp, "m1s2p59_cnsipee2021.csv", sep = "/"))%>%
  mutate_if(is.character, as.numeric)

ppl_20_fc <- select(ppl_20_fc, entidad_a, tipdelit_c, esjucs3, esjucs4,
                    esjucs6, esjucs7, esjucs9, esjucs10)%>%
  dplyr::rename(entidad = entidad_a,
                delito = tipdelit_c,
                sin_sentencia_hombres = esjucs3,
                sin_sentencia_mujeres = esjucs4,
                con_sentencia_hombres_1 = esjucs6,
                con_sentencia_mujeres_1 = esjucs7,
                con_sentencia_hombres_2 = esjucs9,
                con_sentencia_mujeres_2 = esjucs10)%>%
  select(entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres,
         con_sentencia_hombres_1, con_sentencia_hombres_2, 
         con_sentencia_mujeres_1, con_sentencia_mujeres_2)

ppl_20_fc <- mutate(ppl_20_fc, con_sentencia_hombres = rowSums(ppl_20_fc[,5:6], na.rm=T))
ppl_20_fc <- mutate(ppl_20_fc, con_sentencia_mujeres = rowSums(ppl_20_fc[,7:8], na.rm=T))
ppl_20_fc <- select(ppl_20_fc, entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres, con_sentencia_hombres, con_sentencia_mujeres)

ppl_20_fc <- pivot_longer(ppl_20_fc, cols = c(sin_sentencia_hombres:con_sentencia_mujeres),
                          names_to = "estatus", values_to = "total")

ppl_20_fc <- mutate(ppl_20_fc, sexo = case_when(estatus == "con_sentencia_hombres" ~ "Hombres",
                                                estatus == "sin_sentencia_hombres" ~ "Hombres",
                                                estatus == "con_sentencia_mujeres" ~ "Mujeres",
                                                estatus == "sin_sentencia_mujeres" ~ "Mujeres"))

ppl_20_fc <- mutate(ppl_20_fc, estatus = case_when(estatus == "con_sentencia_hombres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_hombres" ~ "Sin sentencia",
                                                   estatus == "con_sentencia_mujeres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_mujeres" ~ "Sin sentencia"))

ppl_20_fc <- mutate(ppl_20_fc, year = "2020", fuero = "Fuero común")
ppl_20_fc <- select(ppl_20_fc, entidad, sexo, delito, estatus, fuero, total, year)
ppl_20_fc <- mutate(ppl_20_fc, total = replace_na(total,0))

ppl_20_fc$entidad <- factor(ppl_20_fc$entidad, 
                            levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                       11, 12, 13, 14, 15, 16, 17, 
                                       18, 19, 20, 21 , 22, 23, 24, 
                                       25, 26, 27, 28, 29, 30, 31, 32),
                            labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                       "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                       "Ciudad de México", "Durango", "Guanajuato", 
                                       "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                       "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                       "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                       "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                       "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

#Filtro a delitos sin desagregación
ppl_20_fc <- filter(ppl_20_fc, delito >= 10100 &  delito <= 20600 | delito >= 20700 &  delito <= 30400 |
                          delito >= 30500 &  delito <= 39999 | delito >= 40101 &  delito <= 40199 | delito == 40200 | delito >= 40300 &  delito <= 40500 |
                          delito >= 40600 &  delito <= 60100 | delito == 60200 | delito >= 60300 &  delito <= 69999 | delito >= 70101 &  delito <= 70199 |
                          delito >= 70201 &  delito <= 70299 | delito >= 70300 &  delito <= 70600 | delito >= 70700 &  delito <= 79999 |
                          delito >= 80101 &  delito <= 80199 | delito == 80200 | delito >= 80300 &  delito <= 90500 | delito == 90600 | delito == 90700 |
                          delito >= 90800 &  delito <= 91600 | delito >= 91700 &  delito <= 999999)

ppl_20_fc$delito <- factor(ppl_20_fc$delito, 
                               levels = c(10100, 10200, 10300, 10400, 10500, 19999, 20100, 20200, 20300, 20400, 20500, 20600,
                                          20700, 29999, 30100, 30200, 30300, 30400, 30500, 30600, 30700, 39999, 40101, 40102, 40103,
                                          40104, 40105, 40106, 40107, 40108, 40109, 40110, 40111, 40112, 40113, 40114, 40115, 40116,
                                          40117, 40118, 40199, 40200, 40300, 40400, 40500, 40600, 40700, 40800, 49999, 50100, 50200, 50300, 59999, 60100, 60200,
                                          60300, 60400, 60500, 60600, 69999, 70101, 70102, 70103, 70104, 70105, 70106, 70199, 
                                          70201, 70202, 70203, 70204, 70205, 70206, 70207, 70208, 70299, 70300, 70400, 70500, 70600, 70700, 70800,
                                          70900, 79999, 80101, 80102, 80103, 80104, 80105, 80106, 80107, 80108, 80109, 80199, 80200, 80300, 80400, 80500,
                                          89999, 90100, 90200, 90300, 90400, 90500, 90600, 90700, 90800, 90900,
                                          91000, 91100, 91200, 91300, 91400, 91500, 91600, 91700, 91800, 99999, 100100, 999999),
                               labels = c("Homicidio", "Feminicidio", "Aborto", "Lesiones", "Otros contra la vida y la integridad corporal",
                                          "La vida y la integridad corporal NE", "Privación de la libertad", "Tráfico de menores", "Retención o sustracción de menores e incapaces",
                                          "Rapto", "Desaparición forzada de personas", "Secuestro", "Otros que atentan contra la libertad personal",
                                          "La libertad personal NE", "Abuso sexual", "Acoso sexual", "Hostigamiento sexual", "Violación", "Estupro",
                                          "Incesto", "Otros contra la libertad y la seguridad sexual", "La libertad y la seguridad sexual NE",
                                          "Robo simple", "Robo a casa habitación", "Robo de vehículo", "Robo de autopartes", "Robo a transeúnte en vía pública", 
                                          "Robo a transeúnte en espacio abierto al público", "Robo a persona en un lugar privado", "Robo a transportista",
                                          "Robo en transporte público individual", "Robo en transporte público colectivo", "Robo en transporte individual",
                                          "Robo a institución bancaria", "Robo a negocio", "Robo de ganado", "Robo de maquinaria", "Robo de energía eléctrica",
                                          "Otros robos", "Robo NI", "Robo NE", 
                                          "Hidrocarburos y sus derivados", "Fraude", "Abuso de confianza", "Extorsión", "Daño a la propiedad",
                                          "Despojo", "Otros contra el patrimonio", "El patrimonio NE", "Violencia familiar", "Incumplimiento de obligaciones familiares",
                                          "La familia - Otros", "La familia NE", "Contra el libre desarrollo de la personalidad", "Trata de personas", "Violencia de género distinta a la violencia familiar",
                                          "Discriminación", "Lenocinio", "Otros contra la sociedad", "La sociedad NE", 
                                          "Posesión simple de narcóticos", "Posesión con fines de comercio o suministro de narcóticos", "Comercio de narcóticos FC",
                                          "Suministro de narcóticos FC", "Otros narcomenudeo", "Narcomenudeo NI", "Narcomenudeo NE", "Producción de narcóticos",
                                          "Transporte de narcóticos", "Tráfico de narcóticos", "Comercio de narcóticos FF", "Suministro de narcóticos FF", "Posesión de narcóticos",
                                          "Otros narcóticos", "Narcotráfico NI", "Narcotráfico NE", "Evasión de presos", "En materia de armas y objetos prohibidos", "Delincuencia organizada", "Armas, explosivos y otros materiales",
                                          "Asociación delictuosa", "Terrorismo", "Otros contra la seguridad pública", "La seguridad pública NE",
                                          "Ejercicio indebido del servicio público", "Abuso de autoridad", "Cohecho", "Peculado", "Enriquecimiento ilícito", "Ejercicio abusivo de funciones",
                                          "Tráfico de influencias", "Otros corrupción", "Corrupción NI", "Corrupción NE","Contra la administración de justicia", "Delitos en materia fiscal", "Delitos electorales", "Otros delitos contra la administración del Estado",
                                          "La administración del Estado NE", "Amenazas", "Allanamiento de morada", "Falsedad",
                                          "Falsificación", "Delitos contra el medio ambiente", "En materia de vías de comunicación", "En materia de migración",
                                          "En materia de derechos de autor", "En materia de instituciones de crédito", "En materia de propiedad industrial",
                                          "Contra la salud no relacionados con narcóticos", "Encubrimiento", "Operaciones con recursos de procedencia ilícita",
                                          "Tortura", "Suplantación y usurpación de identidad", "Contra la seguridad de los datos", "Tratos o penas crueles",
                                          "Otros delitos", "Otros NE", "No identificado", "No especificado"))


##Revisión NAs
sum(is.na(ppl_20_fc$entidad))
sum(is.na(ppl_20_fc$delito))
sum(is.na(ppl_20_fc$sexo))
sum(is.na(ppl_20_fc$estatus))
sum(is.na(ppl_20_fc$total))
sum(is.na(ppl_20_fc$year))
sum(is.na(ppl_20_fc$fuero))

#which(is.na(ppl_20_fc$entidad))

#Chequeo de totales
ppl_20_fc$total <- as.numeric(ppl_20_fc$total)
ppl_20_fc <- mutate(ppl_20_fc, total = replace_na(total,0))
sum(ppl_20_fc$total) #242,888

#fuero federal
ppl_20_ff <- read_csv(paste(inp, "m1s2p60_cnsipee2021.csv", sep = "/"))%>%
  mutate_if(is.character, as.numeric)

ppl_20_ff <- select(ppl_20_ff, entidad_a, tipdelit_c, esjucs3, esjucs4,
                    esjucs6, esjucs7, esjucs9, esjucs10)%>%
  dplyr::rename(entidad = entidad_a,
                delito = tipdelit_c,
                sin_sentencia_hombres = esjucs3,
                sin_sentencia_mujeres = esjucs4,
                con_sentencia_hombres_1 = esjucs6,
                con_sentencia_mujeres_1 = esjucs7,
                con_sentencia_hombres_2 = esjucs9,
                con_sentencia_mujeres_2 = esjucs10)%>%
  select(entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres,
         con_sentencia_hombres_1, con_sentencia_hombres_2, 
         con_sentencia_mujeres_1, con_sentencia_mujeres_2)

ppl_20_ff <- mutate(ppl_20_ff, con_sentencia_hombres = rowSums(ppl_20_ff[,5:6], na.rm=T))
ppl_20_ff <- mutate(ppl_20_ff, con_sentencia_mujeres = rowSums(ppl_20_ff[,7:8], na.rm=T))
ppl_20_ff <- select(ppl_20_ff, entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres, con_sentencia_hombres, con_sentencia_mujeres)

ppl_20_ff <- pivot_longer(ppl_20_ff, cols = c(sin_sentencia_hombres:con_sentencia_mujeres),
                          names_to = "estatus", values_to = "total")

ppl_20_ff <- mutate(ppl_20_ff, sexo = case_when(estatus == "con_sentencia_hombres" ~ "Hombres",
                                                estatus == "sin_sentencia_hombres" ~ "Hombres",
                                                estatus == "con_sentencia_mujeres" ~ "Mujeres",
                                                estatus == "sin_sentencia_mujeres" ~ "Mujeres"))

ppl_20_ff <- mutate(ppl_20_ff, estatus = case_when(estatus == "con_sentencia_hombres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_hombres" ~ "Sin sentencia",
                                                   estatus == "con_sentencia_mujeres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_mujeres" ~ "Sin sentencia"))

ppl_20_ff <- mutate(ppl_20_ff, year = "2020", fuero = "Fuero federal")
ppl_20_ff <- select(ppl_20_ff, entidad, sexo, delito, estatus, fuero, total, year)
ppl_20_ff <- mutate(ppl_20_ff, total = replace_na(total,0))

ppl_20_ff$entidad <- factor(ppl_20_ff$entidad, 
                            levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                       11, 12, 13, 14, 15, 16, 17, 
                                       18, 19, 20, 21 , 22, 23, 24, 
                                       25, 26, 27, 28, 29, 30, 31, 32),
                            labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                       "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                       "Ciudad de México", "Durango", "Guanajuato", 
                                       "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                       "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                       "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                       "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                       "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

#Filtro a delitos sin desagregación
ppl_20_ff <- filter(ppl_20_ff, delito >= 10100 &  delito <= 20600 | delito >= 20700 &  delito <= 30400 |
                      delito >= 30500 &  delito <= 39999 | delito >= 40101 &  delito <= 40199 | delito == 40200 | delito >= 40300 &  delito <= 40500 |
                      delito >= 40600 &  delito <= 60100 | delito == 60200 | delito >= 60300 &  delito <= 69999 | delito >= 70101 &  delito <= 70199 |
                      delito >= 70201 &  delito <= 70299 | delito >= 70300 &  delito <= 70600 | delito >= 70700 &  delito <= 79999 |
                      delito >= 80101 &  delito <= 80199 | delito == 80200 | delito >= 80300 &  delito <= 90500 | delito == 90600 | delito == 90700 |
                      delito >= 90800 &  delito <= 91600 | delito >= 91700 &  delito <= 999999)

ppl_20_ff$delito <- factor(ppl_20_ff$delito, 
                           levels = c(10100, 10200, 10300, 10400, 10500, 19999, 20100, 20200, 20300, 20400, 20500, 20600,
                                      20700, 29999, 30100, 30200, 30300, 30400, 30500, 30600, 30700, 39999, 40101, 40102, 40103,
                                      40104, 40105, 40106, 40107, 40108, 40109, 40110, 40111, 40112, 40113, 40114, 40115, 40116,
                                      40117, 40118, 40199, 40200, 40300, 40400, 40500, 40600, 40700, 40800, 49999, 50100, 50200, 50300, 59999, 60100, 60200,
                                      60300, 60400, 60500, 60600, 69999, 70101, 70102, 70103, 70104, 70105, 70106, 70199, 
                                      70201, 70202, 70203, 70204, 70205, 70206, 70207, 70208, 70299, 70300, 70400, 70500, 70600, 70700, 70800,
                                      70900, 79999, 80101, 80102, 80103, 80104, 80105, 80106, 80107, 80108, 80109, 80199, 80200, 80300, 80400, 80500,
                                      89999, 90100, 90200, 90300, 90400, 90500, 90600, 90700, 90800, 90900,
                                      91000, 91100, 91200, 91300, 91400, 91500, 91600, 91700, 91800, 99999, 100100, 999999),
                           labels = c("Homicidio", "Feminicidio", "Aborto", "Lesiones", "Otros contra la vida y la integridad corporal",
                                      "La vida y la integridad corporal NE", "Privación de la libertad", "Tráfico de menores", "Retención o sustracción de menores e incapaces",
                                      "Rapto", "Desaparición forzada de personas", "Secuestro", "Otros que atentan contra la libertad personal",
                                      "La libertad personal NE", "Abuso sexual", "Acoso sexual", "Hostigamiento sexual", "Violación", "Estupro",
                                      "Incesto", "Otros contra la libertad y la seguridad sexual", "La libertad y la seguridad sexual NE",
                                      "Robo simple", "Robo a casa habitación", "Robo de vehículo", "Robo de autopartes", "Robo a transeúnte en vía pública", 
                                      "Robo a transeúnte en espacio abierto al público", "Robo a persona en un lugar privado", "Robo a transportista",
                                      "Robo en transporte público individual", "Robo en transporte público colectivo", "Robo en transporte individual",
                                      "Robo a institución bancaria", "Robo a negocio", "Robo de ganado", "Robo de maquinaria", "Robo de energía eléctrica",
                                      "Otros robos", "Robo NI", "Robo NE", 
                                      "Hidrocarburos y sus derivados", "Fraude", "Abuso de confianza", "Extorsión", "Daño a la propiedad",
                                      "Despojo", "Otros contra el patrimonio", "El patrimonio NE", "Violencia familiar", "Incumplimiento de obligaciones familiares",
                                      "La familia - Otros", "La familia NE", "Contra el libre desarrollo de la personalidad", "Trata de personas", "Violencia de género distinta a la violencia familiar",
                                      "Discriminación", "Lenocinio", "Otros contra la sociedad", "La sociedad NE", 
                                      "Posesión simple de narcóticos", "Posesión con fines de comercio o suministro de narcóticos", "Comercio de narcóticos FC",
                                      "Suministro de narcóticos FC", "Otros narcomenudeo", "Narcomenudeo NI", "Narcomenudeo NE", "Producción de narcóticos",
                                      "Transporte de narcóticos", "Tráfico de narcóticos", "Comercio de narcóticos FF", "Suministro de narcóticos FF", "Posesión de narcóticos",
                                      "Otros narcóticos", "Narcotráfico NI", "Narcotráfico NE", "Evasión de presos", "En materia de armas y objetos prohibidos", "Delincuencia organizada", "Armas, explosivos y otros materiales",
                                      "Asociación delictuosa", "Terrorismo", "Otros contra la seguridad pública", "La seguridad pública NE",
                                      "Ejercicio indebido del servicio público", "Abuso de autoridad", "Cohecho", "Peculado", "Enriquecimiento ilícito", "Ejercicio abusivo de funciones",
                                      "Tráfico de influencias", "Otros corrupción", "Corrupción NI", "Corrupción NE","Contra la administración de justicia", "Delitos en materia fiscal", "Delitos electorales", "Otros delitos contra la administración del Estado",
                                      "La administración del Estado NE", "Amenazas", "Allanamiento de morada", "Falsedad",
                                      "Falsificación", "Delitos contra el medio ambiente", "En materia de vías de comunicación", "En materia de migración",
                                      "En materia de derechos de autor", "En materia de instituciones de crédito", "En materia de propiedad industrial",
                                      "Contra la salud no relacionados con narcóticos", "Encubrimiento", "Operaciones con recursos de procedencia ilícita",
                                      "Tortura", "Suplantación y usurpación de identidad", "Contra la seguridad de los datos", "Tratos o penas crueles",
                                      "Otros delitos", "Otros NE", "No identificado", "No especificado"))


##Revisión NAs
sum(is.na(ppl_20_ff$entidad))
sum(is.na(ppl_20_ff$delito))
sum(is.na(ppl_20_ff$sexo))
sum(is.na(ppl_20_ff$estatus))
sum(is.na(ppl_20_ff$total))
sum(is.na(ppl_20_ff$year))
sum(is.na(ppl_20_ff$fuero))

#which(is.na(ppl_20_fc$entidad))

#Chequeo de totales
ppl_20_ff$total <- as.numeric(ppl_20_ff$total)
ppl_20_ff <- mutate(ppl_20_ff, total = replace_na(total,0))
sum(ppl_20_ff$total) #27,560

#pegando ambos fueros#
ppl_20 <- bind_rows(ppl_20_fc, ppl_20_ff)
sum(is.na(ppl_20$entidad))
sum(is.na(ppl_20$estatus))
sum(is.na(ppl_20$fuero))
sum(is.na(ppl_20$delito))
sum(is.na(ppl_20$sexo))
sum(is.na(ppl_20$total))
sum(is.na(ppl_20$year))

#### Datos 2021 ####
# fuero común #
ppl_21_fc <- read_csv(paste(inp, "m1s2p63_cnsipee2022.csv", sep = "/"))%>%
  mutate_if(is.character, as.numeric)

ppl_21_fc <- select(ppl_21_fc, entidad_a, tipdelit_e, esjucs3, esjucs4,
                    esjucs6, esjucs7, esjucs9, esjucs10, esjucs13, esjucs14)%>%
  dplyr::rename(entidad = entidad_a,
                delito = tipdelit_e,
                sin_sentencia_hombres = esjucs3,
                sin_sentencia_mujeres = esjucs4,
                con_sentencia_hombres_1 = esjucs6,
                con_sentencia_mujeres_1 = esjucs7,
                con_sentencia_hombres_2 = esjucs9,
                con_sentencia_mujeres_2 = esjucs10,
                con_sentencia_hombres_3 = esjucs13,
                con_sentencia_mujeres_3 = esjucs14)%>%
  select(entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres,
         con_sentencia_hombres_1, con_sentencia_hombres_2, con_sentencia_hombres_3,
         con_sentencia_mujeres_1, con_sentencia_mujeres_2, con_sentencia_mujeres_3)

ppl_21_fc <- mutate(ppl_21_fc, con_sentencia_hombres = rowSums(ppl_21_fc[,5:7], na.rm=T))
ppl_21_fc <- mutate(ppl_21_fc, con_sentencia_mujeres = rowSums(ppl_21_fc[,8:10], na.rm=T))
ppl_21_fc <- select(ppl_21_fc, entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres, con_sentencia_hombres, con_sentencia_mujeres)

ppl_21_fc <- pivot_longer(ppl_21_fc, cols = c(sin_sentencia_hombres:con_sentencia_mujeres),
                          names_to = "estatus", values_to = "total")

ppl_21_fc <- mutate(ppl_21_fc, sexo = case_when(estatus == "con_sentencia_hombres" ~ "Hombres",
                                                estatus == "sin_sentencia_hombres" ~ "Hombres",
                                                estatus == "con_sentencia_mujeres" ~ "Mujeres",
                                                estatus == "sin_sentencia_mujeres" ~ "Mujeres"))

ppl_21_fc <- mutate(ppl_21_fc, estatus = case_when(estatus == "con_sentencia_hombres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_hombres" ~ "Sin sentencia",
                                                   estatus == "con_sentencia_mujeres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_mujeres" ~ "Sin sentencia"))

ppl_21_fc <- mutate(ppl_21_fc, year = "2021", fuero = "Fuero común")
ppl_21_fc <- select(ppl_21_fc, entidad, sexo, delito, estatus, fuero, total, year)
ppl_21_fc <- mutate(ppl_21_fc, total = replace_na(total,0))

ppl_21_fc$entidad <- factor(ppl_21_fc$entidad, 
                            levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                       11, 12, 13, 14, 15, 16, 17, 
                                       18, 19, 20, 21 , 22, 23, 24, 
                                       25, 26, 27, 28, 29, 30, 31, 32),
                            labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                       "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                       "Ciudad de México", "Durango", "Guanajuato", 
                                       "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                       "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                       "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                       "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                       "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

#Filtro a delitos sin desagregación
ppl_21_fc <- filter(ppl_21_fc, delito >= 10100 &  delito <= 20600 | delito >= 20700 &  delito <= 30400 |
                      delito >= 30500 &  delito <= 39999 | delito >= 40101 &  delito <= 40199 | delito == 40210 | delito >= 40300 &  delito <= 40500 |
                      delito >= 40600 &  delito <= 60100 | delito == 60200 | delito >= 60300 &  delito <= 69999 | delito >= 70101 &  delito <= 70199 |
                      delito >= 70201 &  delito <= 70299 | delito >= 70300 &  delito <= 70600 | delito >= 70700 &  delito <= 79999 |
                      delito >= 80101 &  delito <= 80199 | delito == 80200 | delito >= 80300 & delito <= 90500 | delito == 90600 | delito == 90700 |
                      delito >= 90800 &  delito <= 91600 | delito >= 91700 & delito <= 999999)

ppl_21_fc$delito <- factor(ppl_21_fc$delito, 
                           levels = c(10100, 10200, 10300, 10400, 10500, 19999, 20100, 20200, 20300, 20400, 20500, 20600,
                                      20700, 29999, 30100, 30200, 30300, 30400, 30500, 30600, 30700, 39999, 40101, 40102, 40103,
                                      40104, 40105, 40106, 40107, 40108, 40109, 40110, 40111, 40112, 40113, 40114, 40115, 40116,
                                      40117, 40118, 40199, 40200, 40300, 40400, 40500, 40600, 40700, 40800, 49999, 50100, 50200, 50300, 59999, 60100, 60200,
                                      60300, 60400, 60500, 60600, 69999, 70101, 70102, 70103, 70104, 70105, 70106, 70199, 
                                      70201, 70202, 70203, 70204, 70205, 70206, 70207, 70208, 70299, 70300, 70400, 70500, 70600, 70700, 70800,
                                      70900, 79999, 80101, 80102, 80103, 80104, 80105, 80106, 80107, 80108, 80109, 80199, 80200, 80300, 80400, 80500,
                                      89999, 90100, 90200, 90300, 90400, 90500, 90600, 90700, 90800, 90900,
                                      91000, 91100, 91200, 91300, 91400, 91500, 91600, 91700, 91800, 99999, 100100, 999999),
                           labels = c("Homicidio", "Feminicidio", "Aborto", "Lesiones", "Otros contra la vida y la integridad corporal",
                                      "La vida y la integridad corporal NE", "Privación de la libertad", "Tráfico de menores", "Retención o sustracción de menores e incapaces",
                                      "Rapto", "Desaparición forzada de personas", "Secuestro", "Otros que atentan contra la libertad personal",
                                      "La libertad personal NE", "Abuso sexual", "Acoso sexual", "Hostigamiento sexual", "Violación", "Estupro",
                                      "Incesto", "Otros contra la libertad y la seguridad sexual", "La libertad y la seguridad sexual NE",
                                      "Robo simple", "Robo a casa habitación", "Robo de vehículo", "Robo de autopartes", "Robo a transeúnte en vía pública", 
                                      "Robo a transeúnte en espacio abierto al público", "Robo a persona en un lugar privado", "Robo a transportista",
                                      "Robo en transporte público individual", "Robo en transporte público colectivo", "Robo en transporte individual",
                                      "Robo a institución bancaria", "Robo a negocio", "Robo de ganado", "Robo de maquinaria", "Robo de energía eléctrica",
                                      "Otros robos", "Robo NI", "Robo NE", 
                                      "Hidrocarburos y sus derivados", "Fraude", "Abuso de confianza", "Extorsión", "Daño a la propiedad",
                                      "Despojo", "Otros contra el patrimonio", "El patrimonio NE", "Violencia familiar", "Incumplimiento de obligaciones familiares",
                                      "La familia - Otros", "La familia NE", "Contra el libre desarrollo de la personalidad", "Trata de personas", "Violencia de género distinta a la violencia familiar",
                                      "Discriminación", "Lenocinio", "Otros contra la sociedad", "La sociedad NE", 
                                      "Posesión simple de narcóticos", "Posesión con fines de comercio o suministro de narcóticos", "Comercio de narcóticos FC",
                                      "Suministro de narcóticos FC", "Otros narcomenudeo", "Narcomenudeo NI", "Narcomenudeo NE", "Producción de narcóticos",
                                      "Transporte de narcóticos", "Tráfico de narcóticos", "Comercio de narcóticos FF", "Suministro de narcóticos FF", "Posesión de narcóticos",
                                      "Otros narcóticos", "Narcotráfico NI", "Narcotráfico NE", "Evasión de presos", "En materia de armas y objetos prohibidos", "Delincuencia organizada", "Armas, explosivos y otros materiales",
                                      "Asociación delictuosa", "Terrorismo", "Otros contra la seguridad pública", "La seguridad pública NE",
                                      "Ejercicio indebido del servicio público", "Abuso de autoridad", "Cohecho", "Peculado", "Enriquecimiento ilícito", "Ejercicio abusivo de funciones",
                                      "Tráfico de influencias", "Otros corrupción", "Corrupción NI", "Corrupción NE","Contra la administración de justicia", "Delitos en materia fiscal", "Delitos electorales", "Otros delitos contra la administración del Estado",
                                      "La administración del Estado NE", "Amenazas", "Allanamiento de morada", "Falsedad",
                                      "Falsificación", "Delitos contra el medio ambiente", "En materia de vías de comunicación", "En materia de migración",
                                      "En materia de derechos de autor", "En materia de instituciones de crédito", "En materia de propiedad industrial",
                                      "Contra la salud no relacionados con narcóticos", "Encubrimiento", "Operaciones con recursos de procedencia ilícita",
                                      "Tortura", "Suplantación y usurpación de identidad", "Contra la seguridad de los datos", "Tratos o penas crueles",
                                      "Otros delitos", "Otros NE", "No identificado", "No especificado"))


##Revisión NAs
sum(is.na(ppl_21_fc$entidad))
sum(is.na(ppl_21_fc$delito))
sum(is.na(ppl_21_fc$sexo))
sum(is.na(ppl_21_fc$estatus))
sum(is.na(ppl_21_fc$total))
sum(is.na(ppl_21_fc$year))
sum(is.na(ppl_21_fc$fuero))

#which(is.na(ppl_21_fc$delito))

#perdidos <- ppl_21_fc %>% 
  #filter_all(any_vars(is.na(.))) 

#Chequeo de totales
ppl_21_fc$total <- as.numeric(ppl_21_fc$total)
ppl_21_fc <- mutate(ppl_21_fc, total = replace_na(total,0))
sum(ppl_21_fc$total) #285,133

#fuero federal
ppl_21_ff <- read_csv(paste(inp, "m1s2p64_cnsipee2022.csv", sep = "/"))%>%
  mutate_if(is.character, as.numeric)

ppl_21_ff <- select(ppl_21_ff, entidad_a, tipdelit_e, esjucs3, esjucs4,
                    esjucs6, esjucs7, esjucs9, esjucs10, esjucs13, esjucs14)%>%
  dplyr::rename(entidad = entidad_a,
                delito = tipdelit_e,
                sin_sentencia_hombres = esjucs3,
                sin_sentencia_mujeres = esjucs4,
                con_sentencia_hombres_1 = esjucs6,
                con_sentencia_mujeres_1 = esjucs7,
                con_sentencia_hombres_2 = esjucs9,
                con_sentencia_mujeres_2 = esjucs10,
                con_sentencia_hombres_3 = esjucs13,
                con_sentencia_mujeres_3 = esjucs14)%>%
  select(entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres,
         con_sentencia_hombres_1, con_sentencia_hombres_2, con_sentencia_hombres_3,
         con_sentencia_mujeres_1, con_sentencia_mujeres_2, con_sentencia_mujeres_3)

ppl_21_ff <- mutate(ppl_21_ff, con_sentencia_hombres = rowSums(ppl_21_ff[,5:6], na.rm=T))
ppl_21_ff <- mutate(ppl_21_ff, con_sentencia_mujeres = rowSums(ppl_21_ff[,7:8], na.rm=T))
ppl_21_ff <- select(ppl_21_ff, entidad, delito, sin_sentencia_hombres, sin_sentencia_mujeres, con_sentencia_hombres, con_sentencia_mujeres)

ppl_21_ff <- pivot_longer(ppl_21_ff, cols = c(sin_sentencia_hombres:con_sentencia_mujeres),
                          names_to = "estatus", values_to = "total")

ppl_21_ff <- mutate(ppl_21_ff, sexo = case_when(estatus == "con_sentencia_hombres" ~ "Hombres",
                                                estatus == "sin_sentencia_hombres" ~ "Hombres",
                                                estatus == "con_sentencia_mujeres" ~ "Mujeres",
                                                estatus == "sin_sentencia_mujeres" ~ "Mujeres"))

ppl_21_ff <- mutate(ppl_21_ff, estatus = case_when(estatus == "con_sentencia_hombres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_hombres" ~ "Sin sentencia",
                                                   estatus == "con_sentencia_mujeres" ~ "Con sentencia",
                                                   estatus == "sin_sentencia_mujeres" ~ "Sin sentencia"))

ppl_21_ff <- mutate(ppl_21_ff, year = "2021", fuero = "Fuero federal")
ppl_21_ff <- select(ppl_21_ff, entidad, sexo, delito, estatus, fuero, total, year)
ppl_21_ff <- mutate(ppl_21_ff, total = replace_na(total,0))

ppl_21_ff$entidad <- factor(ppl_21_ff$entidad, 
                            levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                       11, 12, 13, 14, 15, 16, 17, 
                                       18, 19, 20, 21 , 22, 23, 24, 
                                       25, 26, 27, 28, 29, 30, 31, 32),
                            labels = c("Aguascalientes", "Baja California", "Baja California Sur",
                                       "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                       "Ciudad de México", "Durango", "Guanajuato", 
                                       "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                                       "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
                                       "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí",
                                       "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                       "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

#Filtro a delitos sin desagregación
ppl_21_ff <- filter(ppl_21_ff, delito >= 10100 &  delito <= 20600 | delito >= 20700 &  delito <= 30400 |
                      delito >= 30500 &  delito <= 39999 | delito >= 40101 &  delito <= 40199 | delito == 40210 | delito >= 40300 &  delito <= 40500 |
                      delito >= 40600 &  delito <= 60100 | delito == 60200 | delito >= 60300 &  delito <= 69999 | delito >= 70101 &  delito <= 70199 |
                      delito >= 70201 &  delito <= 70299 | delito >= 70300 &  delito <= 70600 | delito >= 70700 &  delito <= 79999 |
                      delito >= 80101 &  delito <= 80199 | delito == 80200 | delito >= 80300 & delito <= 90500 | delito == 90600 | delito == 90700 |
                      delito >= 90800 &  delito <= 91600 | delito >= 91700 & delito <= 999999)

ppl_21_ff$delito <- factor(ppl_21_ff$delito, 
                           levels = c(10100, 10200, 10300, 10400, 10500, 19999, 20100, 20200, 20300, 20400, 20500, 20600,
                                      20700, 29999, 30100, 30200, 30300, 30400, 30500, 30600, 30700, 39999, 40101, 40102, 40103,
                                      40104, 40105, 40106, 40107, 40108, 40109, 40110, 40111, 40112, 40113, 40114, 40115, 40116,
                                      40117, 40118, 40199, 40200, 40300, 40400, 40500, 40600, 40700, 40800, 49999, 50100, 50200, 50300, 59999, 60100, 60200,
                                      60300, 60400, 60500, 60600, 69999, 70101, 70102, 70103, 70104, 70105, 70106, 70199, 
                                      70201, 70202, 70203, 70204, 70205, 70206, 70207, 70208, 70299, 70300, 70400, 70500, 70600, 70700, 70800,
                                      70900, 79999, 80101, 80102, 80103, 80104, 80105, 80106, 80107, 80108, 80109, 80199, 80200, 80300, 80400, 80500,
                                      89999, 90100, 90200, 90300, 90400, 90500, 90600, 90700, 90800, 90900,
                                      91000, 91100, 91200, 91300, 91400, 91500, 91600, 91700, 91800, 99999, 100100, 999999),
                           labels = c("Homicidio", "Feminicidio", "Aborto", "Lesiones", "Otros contra la vida y la integridad corporal",
                                      "La vida y la integridad corporal NE", "Privación de la libertad", "Tráfico de menores", "Retención o sustracción de menores e incapaces",
                                      "Rapto", "Desaparición forzada de personas", "Secuestro", "Otros que atentan contra la libertad personal",
                                      "La libertad personal NE", "Abuso sexual", "Acoso sexual", "Hostigamiento sexual", "Violación", "Estupro",
                                      "Incesto", "Otros contra la libertad y la seguridad sexual", "La libertad y la seguridad sexual NE",
                                      "Robo simple", "Robo a casa habitación", "Robo de vehículo", "Robo de autopartes", "Robo a transeúnte en vía pública", 
                                      "Robo a transeúnte en espacio abierto al público", "Robo a persona en un lugar privado", "Robo a transportista",
                                      "Robo en transporte público individual", "Robo en transporte público colectivo", "Robo en transporte individual",
                                      "Robo a institución bancaria", "Robo a negocio", "Robo de ganado", "Robo de maquinaria", "Robo de energía eléctrica",
                                      "Otros robos", "Robo NI", "Robo NE", 
                                      "Hidrocarburos y sus derivados", "Fraude", "Abuso de confianza", "Extorsión", "Daño a la propiedad",
                                      "Despojo", "Otros contra el patrimonio", "El patrimonio NE", "Violencia familiar", "Incumplimiento de obligaciones familiares",
                                      "La familia - Otros", "La familia NE", "Contra el libre desarrollo de la personalidad", "Trata de personas", "Violencia de género distinta a la violencia familiar",
                                      "Discriminación", "Lenocinio", "Otros contra la sociedad", "La sociedad NE", 
                                      "Posesión simple de narcóticos", "Posesión con fines de comercio o suministro de narcóticos", "Comercio de narcóticos FC",
                                      "Suministro de narcóticos FC", "Otros narcomenudeo", "Narcomenudeo NI", "Narcomenudeo NE", "Producción de narcóticos",
                                      "Transporte de narcóticos", "Tráfico de narcóticos", "Comercio de narcóticos FF", "Suministro de narcóticos FF", "Posesión de narcóticos",
                                      "Otros narcóticos", "Narcotráfico NI", "Narcotráfico NE", "Evasión de presos", "En materia de armas y objetos prohibidos", "Delincuencia organizada", "Armas, explosivos y otros materiales",
                                      "Asociación delictuosa", "Terrorismo", "Otros contra la seguridad pública", "La seguridad pública NE",
                                      "Ejercicio indebido del servicio público", "Abuso de autoridad", "Cohecho", "Peculado", "Enriquecimiento ilícito", "Ejercicio abusivo de funciones",
                                      "Tráfico de influencias", "Otros corrupción", "Corrupción NI", "Corrupción NE","Contra la administración de justicia", "Delitos en materia fiscal", "Delitos electorales", "Otros delitos contra la administración del Estado",
                                      "La administración del Estado NE", "Amenazas", "Allanamiento de morada", "Falsedad",
                                      "Falsificación", "Delitos contra el medio ambiente", "En materia de vías de comunicación", "En materia de migración",
                                      "En materia de derechos de autor", "En materia de instituciones de crédito", "En materia de propiedad industrial",
                                      "Contra la salud no relacionados con narcóticos", "Encubrimiento", "Operaciones con recursos de procedencia ilícita",
                                      "Tortura", "Suplantación y usurpación de identidad", "Contra la seguridad de los datos", "Tratos o penas crueles",
                                      "Otros delitos", "Otros NE", "No identificado", "No especificado"))


##Revisión NAs
sum(is.na(ppl_21_ff$entidad))
sum(is.na(ppl_21_ff$delito))
sum(is.na(ppl_21_ff$sexo))
sum(is.na(ppl_21_ff$estatus))
sum(is.na(ppl_21_ff$total))
sum(is.na(ppl_21_ff$year))
sum(is.na(ppl_21_ff$fuero))

#which(is.na(ppl_21_fc$entidad))

#Chequeo de totales
ppl_21_ff$total <- as.numeric(ppl_21_ff$total)
ppl_21_ff <- mutate(ppl_21_ff, total = replace_na(total,0))
sum(ppl_21_ff$total) #27,276

#pegando ambos fueros#
ppl_21 <- bind_rows(ppl_21_fc, ppl_21_ff)
sum(is.na(ppl_21$entidad))
sum(is.na(ppl_21$estatus))
sum(is.na(ppl_21$fuero))
sum(is.na(ppl_21$delito))
sum(is.na(ppl_21$sexo))
sum(is.na(ppl_21$total))
sum(is.na(ppl_21$year))

#### Uniendo todas las bases ####
ppl_delitos <- bind_rows(ppl_10, ppl_11, ppl_12, ppl_13, ppl_14, ppl_15,
                         ppl_16, ppl_16, ppl_17, ppl_18, ppl_19, ppl_20, ppl_21)

sum(is.na(ppl_delitos$entidad))
sum(is.na(ppl_delitos$estatus))
sum(is.na(ppl_delitos$fuero))
sum(is.na(ppl_delitos$delito))
sum(is.na(ppl_delitos$sexo))
sum(is.na(ppl_delitos$total))

##### delitos clean #####
ppl_delitos <- mutate(ppl_delitos, delito_corto = case_when(delito == "No especificado" ~ "Otros",
                                                          delito == "Aborto" ~ "Otros",
                                                          delito == "Abuso de autoridad" ~ "Otros",
                                                          delito == "Abuso de confianza" ~ "Otros",
                                                          delito == "Abuso sexual" ~ "Otros delitos sexuales",
                                                          delito == "Acoso sexual" ~ "Otros delitos sexuales",
                                                          delito == "Acceso ilícito a sistemas y equipos de informática" ~ "Otros",
                                                          delito == "Allanamiento de morada" ~ "Otros",
                                                          delito == "Amenazas" ~ "Otros",
                                                          delito == "Armas de Fuego y Explosivos" ~ "Armas de fuego",
                                                          delito == "Armas de fuego, explosivos y otros materiales" ~ "Armas de fuego",
                                                          delito == "Armas y objetos prohibidos" ~ "Armas de fuego",
                                                          delito == "Armas, explosivos y otros materiales" ~ "Armas de fuego",
                                                          delito == "Asociación delictuosa" ~ "Otros",
                                                          delito == "Ataque a las vías de comunicación" ~ "Otros",
                                                          delito == "Código de Justicia Militar" ~ "Otros",
                                                          delito == "Código Fiscal de la Federación" ~ "Otros",
                                                          delito == "Cohecho" ~ "Otros",
                                                          delito == "Comercio de Narcóticos" ~ "Contra la salud",
                                                          delito == "Comercio de narcóticos ff" ~ "Contra la salud",
                                                          delito == "Cometidos contra la administración de justicia" ~ "Otros",
                                                          delito == "Cometidos contra la Administración de Justicia" ~ "Otros",
                                                          delito == "Cometidos contra servidores públicos" ~ "Otros",
                                                          delito == "Cometidos por miembros de la delincuencia organizada" ~ "Delincuencia organizada",
                                                          delito == "Cometidos por servidores públicos" ~ "Otros",
                                                          delito == "Cometidos por Servidores Públicos" ~ "Otros",
                                                          delito == "Contra de las personas en su patrimonio" ~ "Otros",
                                                          delito == "Contra de las Personas en su Patrimonio" ~ "Otros",
                                                          delito == "Contra el Ambiente y la Gestión Ambiental" ~ "Otros",
                                                          delito == "Contra el Derecho Internacional" ~ "Otros",
                                                          delito == "Contra el Estado Civil y Bigamia" ~ "Otros",
                                                          delito == "Contra el Honor" ~ "Otros",
                                                          delito == "Contra el libre desarrollo de la personalidad" ~ "Otros",
                                                          delito == "Contra el Libre Desarrollo de la Personalidad" ~ "Otros",
                                                          delito == "Contra el libre desarrollo de la personalidad - No especificado" ~ "Otros",
                                                          delito == "Contra el medio ambiente" ~ "Otros",
                                                          delito == "Contra el medio ambiente y equilibrio ecológico" ~ "Otros",
                                                          delito == "Contra la administración de justicia" ~ "Otros",
                                                          delito == "Contra la administración del Estado" ~ "Otros",
                                                          delito == "Contra la Autoridad" ~ "Otros",
                                                          delito == "Contra la Economía Pública" ~ "Otros",
                                                          delito == "Contra la Humanidad" ~ "Otros",
                                                          delito == "Contra la Libertad y el Normal Desarrollo Psicosexual" ~ "Otros delitos sexuales",
                                                          delito == "Contra la Paz y Seguridad de las Personas" ~ "Otros",
                                                          delito == "Contra la Salud" ~ "Contra la salud",
                                                          delito == "Contra la Salud - No especificado" ~ "Contra la salud",
                                                          delito == "Contra la Salud en su modalidad de Narcomenudeo" ~ "Narcomenudeo",
                                                          delito == "Contra la salud no relacionados con narcóticos" ~ "Otros",
                                                          delito == "Contra la Seguridad de la Nación" ~ "Otros",
                                                          delito == "Contra la seguridad de los datos" ~ "Otros",
                                                          delito == "Contra la seguridad pública" ~ "Otros",
                                                          delito == "Contra la Seguridad Pública" ~ "Otros",
                                                          delito == "Contra la vida y la integridad corporal" ~ "Otros",
                                                          delito == "Contra la Vida y la Integridad Corporal" ~ "Otros",
                                                          delito == "Corrupción" ~ "Otros",
                                                          delito == "Corrupción NI" ~ "Otros",
                                                          delito == "Corrupción NE" ~ "Otros",
                                                          delito == "Corrupción de menores" ~ "Otros",
                                                          delito == "Corrupción de menores e incapaces" ~ "Otros",
                                                          delito == "Corrupción de menores incapaces" ~ "Otros",
                                                          delito == "Daño a la propiedad" ~ "Otros",
                                                          delito == "Daño en la propiedad" ~ "Otros",
                                                          delito == "Delincuencia organizada" ~ "Delincuencia organizada",
                                                          delito == "Delincuencia Organizada" ~ "Delincuencia organizada",
                                                          delito == "Delitos ambientales" ~ "Otros",
                                                          delito == "Delitos bancarios y financieros" ~ "Otros",
                                                          delito == "Delitos cometidos por servidores públicos" ~ "Otros",
                                                          delito == "Delitos contra el medio ambiente" ~ "Otros",
                                                          delito == "Delitos contra la autoridad" ~ "Otros",
                                                          delito == "Delitos Contra la Dignidad de las Personas (Capítulo Θnico, Discriminación)" ~ "Otros",
                                                          delito == "Delitos contra la salud" ~ "Contra la salud",
                                                          delito == "Delitos contra la Seguridad de la Nación" ~ "Otros",
                                                          delito == "Delitos de Imprenta" ~ "Otros",
                                                          delito == "Delitos electorales" ~ "Delitos electorales",
                                                          delito == "Delitos Electorales" ~ "Delitos electorales",
                                                          delito == "Delitos en materia fiscal" ~ "Otros",
                                                          delito == "Delitos federales contra la salud" ~ "Contra la salud",
                                                          delito == "Delitos previstos en el Código Fiscal de la Federación" ~ "Otros",
                                                          delito == "Derecho de Autor" ~ "Otros",
                                                          delito == "Desaparición forzada" ~ "Desaparición forzada",
                                                          delito == "Desaparición forzada de personas" ~ "Desaparición forzada",
                                                          delito == "Despojo" ~ "Otros",
                                                          delito == "Discriminación" ~ "Otros",
                                                          delito == "Ejercicio abusivo de funciones" ~ "Ejercicio abusivo de funciones",
                                                          delito == "Ejercicio indebido del servicio público" ~ "Otros",
                                                          delito == "Electorales" ~ "Otros",
                                                          delito == "Electorales y en Materia de Registro Nacional de Ciudadanos" ~ "Otros",
                                                          delito == "En materia de armas y objetos prohibidos" ~ "Armas de fuego",
                                                          delito == "En materia de comunicaciones" ~ "Otros",
                                                          delito == "En materia de derechos de autor" ~ "Otros",
                                                          delito == "En Materia de Derechos de Autor" ~ "Otros",
                                                          delito == "En Materia de Inhumaciones y Exhumaciones" ~ "Otros",
                                                          delito == "En materia de instituciones de crédito" ~ "Otros",
                                                          delito == "en materia de Instituciones de crédito, inversión, fianzas y seguros" ~ "Otros",
                                                          delito == "En materia de invenciones y marcas" ~ "Otros",
                                                          delito == "En materia de migración" ~ "Otros",
                                                          delito == "En materia de propiedad industrial" ~ "Otros",
                                                          delito == "En materia de vías de comunicación" ~ "Otros",
                                                          delito == "En Materia de Vías de Comunicación y Correspondencia" ~ "Otros",
                                                          delito == "En materia fiscal" ~ "Otros",
                                                          delito == "Encubrimiento" ~ "Encubrimiento",
                                                          delito == "Encubrimiento y Operaciones con Recursos de Procedencia Ilícita" ~ "Encubrimiento",
                                                          delito == "Enriquecimiento ilícito" ~ "Enriquecimiento ilícito",
                                                          delito == "Estupro" ~ "Otros delitos sexuales",
                                                          delito == "Evasión de presos" ~ "Otros",
                                                          delito == "Extorsión" ~ "Extorsión",
                                                          delito == "Falsedad" ~ "Otros",
                                                          delito == "Falsedad de declaraciones" ~ "Otros",
                                                          delito == "Falsificación" ~ "Otros",
                                                          delito == "Falsificación de documentos" ~ "Otros",
                                                          delito == "Feminicidio" ~ "Feminicidio",
                                                          delito == "Fraude" ~ "Otros",
                                                          delito == "Hidrocarburos y sus derivados" ~ "Hidrocarburos y sus derivados",
                                                          delito == "Homicidio" ~ "Homicidio",
                                                          delito == "Homicidio culposo" ~ "Homicidio",
                                                          delito == "Homicidio doloso" ~ "Homicidio",
                                                          delito == "Hostigamiento sexual" ~ "Otros delitos sexuales",
                                                          delito == "Incesto" ~ "Otros",
                                                          delito == "Incumplimiento de obligaciones de asistencia familiar" ~ "Otros",
                                                          delito == "Incumplimiento de obligaciones familiares" ~ "Otros",
                                                          delito == "Juegos y Sorteos" ~ "Otros",
                                                          delito == "La familia - Otros" ~ "Otros",
                                                          delito == "Legislación de Instituciones de Crédito, Inversión, Fianzas y Seguros" ~ "Otros",
                                                          delito == "Lenocinio" ~ "Otros",
                                                          delito == "Lesiones" ~ "Lesiones",
                                                          delito == "Lesiones culposas" ~ "Lesiones",
                                                          delito == "Lesiones dolosas" ~ "Lesiones",
                                                          delito == "Ley de Concursos Mercantiles" ~ "Otros",
                                                          delito == "Ley de Migración" ~ "Otros",
                                                          delito == "Ley de Vías Generales de Comunicación" ~ "Otros",
                                                          delito == "Ley Federal contra la Delincuencia Organizada" ~ "Delincuencia organizada",
                                                          delito == "Ley Federal de Armas de Fuego y Explosivos" ~ "Armas de fuego",
                                                          delito == "Ley Federal de Juegos y Sorteos" ~ "Otros",
                                                          delito == "Ley Federal del Derecho de Autor" ~ "Otros",
                                                          delito == "Ley Federal sobre Monumentos y Zonas Arqueológicos, Artísticos e Históricos" ~ "Otros",
                                                          delito == "Ley General de Población" ~ "Otros",
                                                          delito == "Ley sobre Delitos de Imprenta" ~ "Otros",
                                                          delito == "Materia de Derechos de Autor" ~ "Otros",
                                                          delito == "Materia de Inhumaciones y Exhumaciones" ~ "Otros",
                                                          delito == "Materia de Vías de Comunicación y Correspondencia" ~ "Otros",
                                                          delito == "Monumentos y Zonas Arqueológicos" ~ "Otros",
                                                          delito == "Narcotráfico NI" ~ "Contra la salud",
                                                          delito == "Narcotráfico NE" ~ "Contra la salud",
                                                          delito == "Narcomenudeo NE" ~ "Narcomenudeo",
                                                          delito == "Narcomenudeo NI" ~ "Narcomenudeo",
                                                          delito == "Narcomenudeo" ~ "Narcomenudeo",
                                                          delito == "Narcomenudeo - No especificado" ~ "Narcomenudeo",
                                                          delito == "Narcomenudeo en modalidad de posesión con fines de venta y suministro" ~ "Posesión con fines de venta y suministro",
                                                          delito == "Narcomenudeo en modalidad de posesión simple" ~ "Narcomenudeo",
                                                          delito == "Narcomenudeo en modalidad de venta o suministro" ~ "Venta o suministro",
                                                          delito == "Narcomenudeo en modalidad distinta a las anteriores" ~ "Narcomenudeo", 
                                                          delito == "No especificado" ~ "Otros",
                                                          delito == "No especificado del fuero común" ~ "No especificado",
                                                          delito == "No especificado del fuero federal" ~ "No especificado",
                                                          delito == "No especificado delito del fuero común" ~ "No especificado",
                                                          delito == "No identificado" ~ "No especificado",
                                                          delito == "Operaciones con recursos de procedencia ilícita" ~ "Otros",
                                                          delito == "Otras Modalidades distintas a las anteriores relacionadas con Narcóticos" ~ "Otros",
                                                          delito == "Otro" ~ "Otros",
                                                          delito == "Otro tipo de secuestros" ~ "Secuestro",
                                                          delito == "Otros contra el patrimonio" ~ "Otros",
                                                          delito == "Otros contra la administración del Estado" ~ "Otros",
                                                          delito == "Otros contra la familia" ~ "Otros",
                                                          delito == "Otros contra la libertad personal" ~ "Otros",
                                                          delito == "Otros contra la libertad y la seguridad sexual" ~ "Otros delitos sexuales",
                                                          delito == "Otros contra la libertad y seguridad sexual" ~ "Otros delitos sexuales",
                                                          delito == "Otros contra la seguridad pública" ~ "Otros",
                                                          delito == "Otros contra la Seguridad Pública" ~ "Otros",
                                                          delito == "Otros contra la sociedad" ~ "Otros",
                                                          delito == "Otros contra la vida y la integridad corporal" ~ "Otros",
                                                          delito == "Otros delitos contra la administración del Estado" ~ "Otros",
                                                          delito == "Otros de falsedad" ~ "Otros",
                                                          delito == "Otros del fuero común" ~ "Otros",
                                                          delito == "Otros del Fuero Común" ~ "Otros",
                                                          delito == "Otros delitos" ~ "Otros",
                                                          delito == "Otros delitos cometidos contra el servicio público" ~ "Otros",
                                                          delito == "Otros Delitos contenidos en el Código Penal Federal" ~ "Otros",
                                                          delito == "Otros delitos contenidos en Leyes Federales" ~ "Otros",
                                                          delito == "Otros delitos contra el libre desarrollo de la personalidad" ~ "Otros",
                                                          delito == "Otros delitos contra la familia" ~ "Otros",
                                                          delito == "Otros delitos contra la salud" ~ "Otros",
                                                          delito == "Otros delitos contra la seguridad pública" ~ "Otros",
                                                          delito == "Otros delitos contra la sociedad" ~ "Otros",
                                                          delito == "Otros delitos del fuero común" ~ "Otros",
                                                          delito == "Otros delitos del Fuero Común" ~ "Otros",
                                                          delito == "Otros delitos del Fuero Federal" ~ "Otros",
                                                          delito == "Otros delitos previstos en la Ley General de Salud" ~ "Otros",
                                                          delito == "Otros delitos que atentan contra el patrimonio" ~ "Otros",
                                                          delito == "Otros delitos que atentan contra la libertad personal" ~ "Otros",
                                                          delito == "Otros delitos que atentan contra la libertad y seguridad sexual" ~ "Otros delitos sexuales",
                                                          delito == "Otros delitos que atentan contra la vida y la integridad corporal" ~ "Otros",
                                                          delito == "Otros delitos que privan de la vida" ~ "Otros",
                                                          delito == "Otros delitos sexuales" ~ "Otros delitos sexuales",
                                                          delito == "Otros delitos sexuales con realización de cópula" ~ "Otros delitos sexuales",
                                                          delito == "Otros delitos sexuales sin realización de cópula" ~ "Otros delitos sexuales",
                                                          delito == "Otros corrupción" ~ "Otros",
                                                          delito == "Otros narcomenudeo" ~ "Narcomenudeo",
                                                          delito == "Otros narcóticos" ~ "Contra la salud",
                                                          delito == "Otros integridad corporal" ~ "Otros",
                                                          delito == "Otros que atentan contra la integridad corporal" ~ "Otros",
                                                          delito == "Otros que atentan contra la libertad personal" ~ "Otros",
                                                          delito == "Otros que atentan contra la libertad personal s/l" ~ "Otros",
                                                          delito == "Otros que atentan contra la vida y la integridad corporal" ~ "Otros",
                                                          delito == "Otros robos" ~ "Robo",
                                                          delito == "Otros federales contra la salud" ~ "Otros",
                                                          delito == "Peculado" ~ "Otros",
                                                          delito == "Pornografía infantil" ~ "Otros delitos sexuales",
                                                          delito == "Posesión de Narcóticos" ~ "Contra la salud",
                                                          delito == "Posesión de narcóticos" ~ "Contra la salud",
                                                          delito == "Posesión de narcóticos" ~ "Contra la salud",
                                                          delito == "Posesión simple de narcóticos" ~ "Posesión simple",
                                                          delito == "Posesión con fines de comercio o suministro de narcóticos" ~ "Posesión con fines de comercio o suministro",
                                                          delito == "Comercio de narcóticos FC" ~ "Comercio de narcóticos",
                                                          delito == "Comercio de narcóticos FF" ~ "Contra la salud",
                                                          delito == "Suministro de narcóticos FC" ~ "Suministro de narcóticos",
                                                          delito == "Privación de la libertad" ~ "Privación de la libertad",
                                                          delito == "Privación Ilegal de la Libertad" ~ "Privación de la libertad",
                                                          delito == "Privación Ilegal de la Libertad y de otras Garantías" ~ "Privación de la libertad",
                                                          delito == "Producción de narcóticos" ~ "Contra la salud",
                                                          delito == "Producción de Narcóticos" ~ "Contra la salud",
                                                          delito == "Propiedad intelectual" ~ "Otros",
                                                          delito == "Prostitución de menores e incapaces" ~ "Otros delitos sexuales",
                                                          delito == "Rapto" ~ "Otros",
                                                          delito == "Responsabilidad de servidores públicos" ~ "Otros",
                                                          delito == "Responsabilidad Profesional" ~ "Otros",
                                                          delito == "Retención de menores e incapaces" ~ "Otros",
                                                          delito == "Retención o sustracción de menores e incapaces" ~ "Otros",
                                                          delito == "Retención o sustracción de menores" ~ "Otros",
                                                          delito == "Revelación de secretos y acceso ilícito a sistemas y equipos de informática" ~ "Otros",
                                                          delito == "Robo" ~ "Robo",
                                                          delito == "Robo simple" ~ "Robo",
                                                          delito == "Robo de energía eléctrica" ~ "Robo",
                                                          delito == "Robo a casa habitación" ~ "Robo a casa habitación",
                                                          delito == "Robo a institución bancaria" ~ "Robo",
                                                          delito == "Robo a instituciones bancarias" ~ "Robo",
                                                          delito == "Robo a negocio" ~ "Robo",
                                                          delito == "Robo a persona en un lugar privado" ~ "Robo",
                                                          delito == "Robo a transeúnte" ~ "Robo",
                                                          delito == "Robo a transeúnte en espacio abierto al público" ~ "Robo",
                                                          delito == "Robo a transeúnte en vía pública" ~ "Robo",
                                                          delito == "Robo a transportista" ~ "Robo a transportista",
                                                          delito == "Robo de autopartes" ~ "Robo",
                                                          delito == "Robo de ganado" ~ "Robo",
                                                          delito == "Robo de maquinaria" ~ "Robo",
                                                          delito == "Robo de menor" ~ "Otros",
                                                          delito == "Robo de vehículo" ~ "Robo",
                                                          delito == "Robo en carretera" ~ "Robo",
                                                          delito == "Robo en transporte individual" ~ "Robo",
                                                          delito == "Robo en transporte público colectivo" ~ "Robo",
                                                          delito == "Robo en transporte público individual" ~ "Robo",
                                                          delito == "Robo NI" ~ "Robo",
                                                          delito == "Robo NE" ~ "Robo",
                                                          delito == "Secuestro" ~ "Secuestro",
                                                          delito == "Secuestro - No especificado" ~ "Secuestro",
                                                          delito == "Secuestro con calidad de rehén" ~ "Secuestro",
                                                          delito == "Secuestro exprés" ~ "Secuestro",
                                                          delito == "Secuestro express" ~ "Secuestro",
                                                          delito == "Secuestro extorsivo" ~ "Secuestro",
                                                          delito == "Secuestro para causar daños" ~ "Secuestro",
                                                          delito == "Suministro de Narcóticos" ~ "Contra la salud",
                                                          delito == "Suministro de narcóticos" ~ "Contra la salud",
                                                          delito == "Suministro de narcóticos FF" ~ "Contra la salud",
                                                          delito == "Suministro de narcóticos ff" ~ "Contra la salud",
                                                          delito == "Suplantación y usurpación de identidad" ~ "Otros",
                                                          delito == "Sustracción de hidrocarburos" ~ "Otros",
                                                          delito == "Terrorismo" ~ "Otros",
                                                          delito == "Tortura" ~ "Otros",
                                                          delito == "Tráfico de influencias" ~ "Otros",
                                                          delito == "Tráfico de menores" ~ "Otros",
                                                          delito == "Tráfico de Menores" ~ "Otros",
                                                          delito == "Tráfico de narcóticos" ~ "Contra la salud",
                                                          delito == "Tráfico de Narcóticos" ~ "Contra la salud",
                                                          delito == "Transporte de narcóticos" ~ "Contra la salud",
                                                          delito == "Transporte de Narcóticos" ~ "Contra la salud",
                                                          delito == "Trata de personas" ~ "Trata de personas",
                                                          delito == "Trata de personas - No especificado" ~ "Trata de personas",
                                                          delito == "Trata de personas con fines de explotación sexual" ~ "Trata de personas",
                                                          delito == "Trata de personas con fines de trabajo o servicios forzados" ~ "Trata de personas",
                                                          delito == "Trata de personas con fines de tráfico de órganos" ~ "Trata de personas",
                                                          delito == "Trata de personas con otros fines de explotación" ~ "Trata de personas",
                                                          delito == "Tratos o penas crueles" ~ "Otros",
                                                          delito == "Turismo sexual" ~ "Otros",
                                                          delito == "Violación" ~ "Violación",
                                                          delito == "Violación equiparada" ~ "Violación",
                                                          delito == "Violencia de género distinta a la violencia familiar" ~ "Otros",
                                                          delito == "Violencia de genero en todas sus modalidades" ~ "Otros",
                                                          delito == "Violencia de género en todas sus modalidades distinta a la violencia familiar" ~ "Otros",
                                                          delito == "Violencia de género distinta a violencia familiar" ~ "Otros",
                                                          delito == "Violencia familiar" ~ "Violencia familiar"))

ppl_delitos <- select(ppl_delitos, entidad, sexo, estatus, delito, delito_corto, fuero, total, year)

sum(is.na(ppl_delitos$delito_corto))

ppl_delitos$entidad <- gsub("Ciudad de MÃ©xico", "Ciudad de México", ppl_delitos$entidad, fixed = TRUE)
ppl_delitos$entidad <- gsub("Estado de MÃ©xico", "Estado de México", ppl_delitos$entidad, fixed = TRUE)
ppl_delitos$entidad <- gsub("MichoacÃ¡n", "Michoacán", ppl_delitos$entidad, fixed = TRUE)
ppl_delitos$entidad <- gsub("Nuevo LeÃ³n", "Nuevo León", ppl_delitos$entidad, fixed = TRUE)
ppl_delitos$entidad <- gsub("QuerÃ©taro", "Querétaro", ppl_delitos$entidad, fixed = TRUE)
ppl_delitos$entidad <- gsub("San Luis PotosÃ", "San Luis Potosí", ppl_delitos$entidad, fixed = TRUE)
ppl_delitos$entidad <- gsub("SanÂ LuisÂ PotosÃ", "San Luis Potosí", ppl_delitos$entidad, fixed = TRUE)
ppl_delitos$entidad <- gsub("San Luis Potosí­", "San Luis Potosí", ppl_delitos$entidad, fixed = TRUE)
ppl_delitos$entidad <- gsub("YucatÃ¡n", "Yucatán", ppl_delitos$entidad, fixed = TRUE)

entidades <- ppl_delitos %>% 
  group_by(entidad) %>% 
  summarise()

saveRDS(ppl_delitos,paste(clean, "ppl_delitos_10-21.rds", sep = "/")) 



















