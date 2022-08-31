###########################################################
# Estatus ppl (CNGSPSE 2010-2021)                         #
# *análisis delitos ppo*                                  #
#                                                         #
# Adriana E. Ortega Arriaga                               #
# Fecha de creación: Junio 2022                           #
# Última actualización: Junio 2022                        #
###########################################################

rm(list=ls())
setwd("~")

require(pacman)
p_load(scales, tidyverse, stringi, dplyr, plyr, foreign, readxl, janitor,extrafont,
       beepr, extrafont, treemapify, ggmosaic, srvyr, ggrepel, lubridate, tidyr, showtext)

Sys.setlocale("LC_ALL", "es_ES.UTF-8")

inp <- "/Volumes/GoogleDrive/Mi unidad/PPO/datos_limpios/Censos_delitos_ppl/ppl_delitos_10-21.rds"
out2 <-"/Volumes/GoogleDrive/Mi unidad/PPO/paraanimal/indicador ppo/"

ppl <- read_rds("/Volumes/GoogleDrive/Mi unidad/PPO/datos_limpios/Censos_delitos_ppl/ppl_delitos_10-21.rds")
ppl <- filter(ppl, sexo != "No especificado")

# Tema para gráficas 
   
tema <-  theme_linedraw() +
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
        axis.title       = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Fira Sans"),
        axis.text.y      = element_text(size = 8, family="Fira Sans", angle=0, hjust=.5),
        axis.text.x      = element_text(size = 8, family="Fira Sans", angle=0, hjust=.5),
        strip.background = element_rect(fill="white", colour = NA),
        strip.text.x     = element_text(size=9, family = "Fira Sans", face = "bold", color = "black"),
        strip.text.y     = element_text(size=9, family = "Fira Sans", face = "bold", color = "black"))

v_rojos <- c("#E23E57", "#88304E", "#522546", "#311D3F")
v_azules <- c("#E9D5DA", "#827397", "#4D4C7D", "#363062")
todos <-c("#E9D5DA", "#827397", "#4D4C7D", "#363062","#E23E57", "#88304E", "#522546", "#311D3F", "#95888B", "#88958B",
          "#8B8895", "#524625", "#51756B", "#CBBBBD")
    
#Indicador ppo
### construcción de delitos con ppo vs no ppo
ppo_10 <- ppl %>%
  filter(year == 2010) %>%
  mutate(pponel = case_when(  delito_corto == "Delincuencia organizada" |
                              delito_corto == "Homicidio" |
                              delito_corto == "Secuestro" |
                              delito_corto == "Violación" |
                              delito_corto == "Contra la salud" ~ "PPO",
                              delito_corto != "Delincuencia organizada" |
                              delito_corto != "Homicidio" |
                              delito_corto != "Secuestro" |
                              delito_corto != "Violación" |
                              delito_corto != "Contra la salud" ~ "No PPO",
                              delito_corto == delito_corto ~ delito_corto))

ppo_11 <- ppl %>%
  filter(year == 2011) %>%
  mutate(pponel = case_when(delito_corto == "Delincuencia organizada" |
                            delito_corto == "Homicidio" |
                            delito_corto == "Secuestro" |
                            delito_corto == "Violación" |
                            delito_corto == "Trata de personas" |
                            delito_corto == "Contra la salud" ~ "PPO",
                            delito_corto != "Delincuencia organizada" |
                            delito_corto != "Homicidio" |
                            delito_corto != "Secuestro" |
                            delito_corto != "Violación" |
                            delito_corto != "Trata de personas" |
                            delito_corto != "Contra la salud" ~ "No PPO",
                            delito_corto == delito_corto ~ delito_corto))

ppo_12 <- ppl %>%
  filter(year == 2012) %>%
  mutate(pponel = case_when(delito_corto == "Delincuencia organizada" |
                            delito_corto == "Homicidio" |
                            delito_corto == "Secuestro" |
                            delito_corto == "Violación" |
                            delito_corto == "Trata de personas" |
                            delito_corto == "Privación de la libertad" |
                            delito_corto == "Contra la salud" ~ "PPO",
                            delito_corto != "Delincuencia organizada" |
                            delito_corto != "Homicidio" |
                            delito_corto != "Secuestro" |
                            delito_corto != "Violación" |
                            delito_corto != "Trata de personas" |
                            delito_corto != "Privación de la libertad" |
                            delito_corto != "Contra la salud" ~ "No PPO",
                            delito_corto == delito_corto ~ delito_corto))

ppo_13 <- ppl %>%
  filter(year == 2013) %>%
  mutate(pponel = case_when(delito_corto == "Delincuencia organizada" |
                            delito_corto == "Homicidio" |
                            delito_corto == "Secuestro" |
                            delito_corto == "Violación" |
                            delito_corto == "Trata de personas" |
                            delito_corto == "Privación de la libertad" |
                            delito_corto == "Contra la salud" ~ "PPO",
                            delito_corto != "Delincuencia organizada" |
                            delito_corto != "Homicidio" |
                            delito_corto != "Secuestro" |
                            delito_corto != "Violación" |
                            delito_corto != "Trata de personas" |
                            delito_corto != "Privación de la libertad" |
                            delito_corto != "Contra la salud" ~ "No PPO",
                            delito_corto == delito_corto ~ delito_corto))

ppo_14 <- ppl %>%
  filter(year == 2014) %>%
  mutate(pponel = case_when(delito_corto == "Delincuencia organizada" |
                            delito_corto == "Homicidio" |
                            delito_corto == "Secuestro" |
                            delito_corto == "Violación" |
                            delito_corto == "Trata de personas" |
                            delito_corto == "Privación de la libertad" |
                            delito_corto == "Contra la salud" ~ "PPO",
                            delito_corto != "Delincuencia organizada" |
                            delito_corto != "Homicidio" |
                            delito_corto != "Secuestro" |
                            delito_corto != "Violación" |
                            delito_corto != "Trata de personas" |
                            delito_corto != "Privación de la libertad" |
                            delito_corto != "Contra la salud" ~ "No PPO",
                            delito_corto == delito_corto ~ delito_corto))

ppo_15 <- ppl %>%
  filter(year == 2015) %>%
  mutate(pponel = case_when(delito_corto == "Delincuencia organizada" |
                            delito_corto == "Homicidio" |
                            delito_corto == "Secuestro" |
                            delito_corto == "Violación" |
                            delito_corto == "Trata de personas" |
                            delito_corto == "Privación de la libertad" |
                            delito_corto == "Posesión con fines de venta y suministro"|
                            delito_corto == "Venta o suministro"|
                            delito_corto == "Contra la salud" ~ "PPO",
                            delito_corto != "Delincuencia organizada" |
                            delito_corto != "Homicidio" |
                            delito_corto != "Secuestro" |
                            delito_corto != "Violación" |
                            delito_corto != "Trata de personas" |
                            delito_corto != "Privación de la libertad" |
                            delito_corto != "Posesión con fines de venta y suministro"|
                            delito_corto != "Venta o suministro"|
                            delito_corto != "Contra la salud" ~ "No PPO",
                            delito_corto == delito_corto ~ delito_corto))

ppo_16 <- ppl %>%
  filter(year == 2016) %>%
  mutate(pponel = case_when(delito_corto == "Delincuencia organizada" |
                            delito_corto == "Homicidio" |
                            delito_corto == "Secuestro" |
                            delito_corto == "Violación" |
                            delito_corto == "Trata de personas" |
                            delito_corto == "Privación de la libertad" |
                            delito_corto == "Posesión con fines de venta y suministro"|
                            delito_corto == "Venta o suministro"|
                            delito_corto == "Comercio de narcóticos"|
                            delito_corto == "Contra la salud" ~ "PPO",
                            delito_corto != "Delincuencia organizada" |
                            delito_corto != "Homicidio" |
                            delito_corto != "Secuestro" |
                            delito_corto != "Violación" |
                            delito_corto != "Trata de personas" |
                            delito_corto != "Privación de la libertad" |
                            delito_corto != "Posesión con fines de venta y suministro"|
                            delito_corto != "Venta o suministro"|
                            delito_corto != "Comercio de narcóticos"|
                            delito_corto != "Contra la salud" ~ "No PPO",
                            delito_corto == delito_corto ~ delito_corto))

ppo_17 <- ppl %>%
  filter(year == 2017) %>%
  mutate(pponel = case_when(delito_corto == "Delincuencia organizada" |
                            delito_corto == "Homicidio" |
                            delito_corto == "Secuestro" |
                            delito_corto == "Violación" |
                            delito_corto == "Trata de personas" |
                            delito_corto == "Privación de la libertad" |
                            delito_corto == "Posesión con fines de comercio o suministro"|
                            delito_corto == "Suministro de narcóticos"|
                            delito_corto == "Comercio de narcóticos"|
                            delito_corto == "Contra la salud" ~ "PPO",
                            delito_corto != "Delincuencia organizada" |
                            delito_corto != "Homicidio" |
                            delito_corto != "Secuestro" |
                            delito_corto != "Violación" |
                            delito_corto != "Trata de personas" |
                            delito_corto != "Privación de la libertad" |
                            delito_corto != "Posesión con fines de comercio o suministro"|
                            delito_corto != "Suministro de narcóticos"|
                            delito_corto != "Comercio de narcóticos"|
                            delito_corto != "Contra la salud" ~ "No PPO",
                            delito_corto == delito_corto ~ delito_corto))

ppo_18 <- ppl %>%
  filter(year == 2018) %>%
  mutate(pponel = case_when(delito_corto == "Delincuencia organizada" |
                            delito_corto == "Homicidio" |
                            delito_corto == "Secuestro" |
                            delito_corto == "Violación" |
                            delito_corto == "Trata de personas" |
                            delito_corto == "Privación de la libertad" |
                            delito_corto == "Posesión con fines de comercio o suministro"|
                            delito_corto == "Suministro de narcóticos"|
                            delito_corto == "Comercio de narcóticos"|
                            delito_corto == "Contra la salud" ~ "PPO",
                            delito_corto != "Delincuencia organizada" |
                            delito_corto != "Homicidio" |
                            delito_corto != "Secuestro" |
                            delito_corto != "Violación" |
                            delito_corto != "Trata de personas" |
                            delito_corto != "Privación de la libertad" |
                            delito_corto != "Posesión con fines de comercio o suministro"|
                            delito_corto != "Suministro de narcóticos"|
                            delito_corto != "Comercio de narcóticos"|
                            delito_corto != "Contra la salud" ~ "No PPO",
                            delito_corto == delito_corto ~ delito_corto))

ppo_19 <- ppl %>%
  filter(year == 2019) %>%
  mutate(pponel = case_when(delito_corto == "Delincuencia organizada" |
                            delito_corto == "Robo a transportista" |
                            delito_corto == "Robo a casa habitación" |
                            delito_corto == "Hidrocarburos y sus derivados"|
                            delito_corto == "Feminicidio" |
                            delito_corto == "Enriquecimiento ilícito"|
                            delito_corto == "Ejercicio abusivo de funciones" |
                            delito_corto == "Desaparición forzada de personas" |
                            delito_corto == "Armas de fuego" |
                            delito_corto == "Delitos electorales" |
                            delito_corto == "Homicidio" |
                            delito_corto == "Secuestro" |
                            delito_corto == "Violación" |
                            delito_corto == "Trata de personas" |
                            delito_corto == "Privación de la libertad" |
                            delito_corto == "Posesión con fines de comercio o suministro"|
                            delito_corto == "Suministro de narcóticos"|
                            delito_corto == "Comercio de narcóticos"|
                            delito_corto == "Contra la salud" ~ "PPO",
                            delito_corto != "Delincuencia organizada" |
                            delito_corto != "Robo a transportista" |
                            delito_corto != "Robo a casa habitación" |
                            delito_corto != "Hidrocarburos y sus derivados"|
                            delito_corto != "Feminicidio" |
                            delito_corto != "Enriquecimiento ilícito"|
                            delito_corto != "Ejercicio abusivo de funciones" |
                            delito_corto != "Desaparición forzada de personas" |
                            delito_corto != "Delitos electorales" |
                            delito_corto != "Armas de fuego" |
                            delito_corto != "Homicidio" |
                            delito_corto != "Secuestro" |
                            delito_corto != "Violación" |
                            delito_corto != "Trata de personas" |
                            delito_corto != "Privación de la libertad" |
                            delito_corto != "Posesión con fines de comercio o suministro"|
                            delito_corto != "Suministro de narcóticos"|
                            delito_corto != "Comercio de narcóticos"|
                            delito_corto != "Contra la salud" ~ "No PPO",
                            delito_corto == delito_corto ~ delito_corto))

ppo_20 <- ppl %>%
  filter(year == 2020) %>%
  mutate(pponel = case_when(delito_corto == "Delincuencia organizada" |
                            delito_corto == "Robo a transportista" |
                            delito_corto == "Robo a casa habitación" |
                            delito_corto == "Hidrocarburos y sus derivados"|
                            delito_corto == "Feminicidio" |
                            delito_corto == "Enriquecimiento ilícito"|
                            delito_corto == "Ejercicio abusivo de funciones" |
                            delito_corto == "Desaparición forzada de personas" |
                            delito_corto == "Armas de fuego" |
                            delito_corto == "Delitos electorales" |
                            delito_corto == "Homicidio" |
                            delito_corto == "Secuestro" |
                            delito_corto == "Violación" |
                            delito_corto == "Trata de personas" |
                            delito_corto == "Privación de la libertad" |
                            delito_corto == "Posesión con fines de comercio o suministro"|
                            delito_corto == "Suministro de narcóticos"|
                            delito_corto == "Comercio de narcóticos"|
                            delito_corto == "Contra la salud" ~ "PPO",
                            delito_corto != "Delincuencia organizada" |
                            delito_corto != "Robo a transportista" |
                            delito_corto != "Robo a casa habitación" |
                            delito_corto != "Hidrocarburos y sus derivados"|
                            delito_corto != "Feminicidio" |
                            delito_corto != "Enriquecimiento ilícito"|
                            delito_corto != "Ejercicio abusivo de funciones" |
                            delito_corto != "Desaparición forzada de personas" |
                            delito_corto != "Delitos electorales" |
                            delito_corto != "Armas de fuego" |
                            delito_corto != "Homicidio" |
                            delito_corto != "Secuestro" |
                            delito_corto != "Violación" |
                            delito_corto != "Trata de personas" |
                            delito_corto != "Privación de la libertad" |
                            delito_corto != "Posesión con fines de comercio o suministro"|
                            delito_corto != "Suministro de narcóticos"|
                            delito_corto != "Comercio de narcóticos"|
                            delito_corto != "Contra la salud" ~ "No PPO",
                            delito_corto == delito_corto ~ delito_corto))

ppo_21 <- ppl %>%
  filter(year == 2021) %>%
  mutate(pponel = case_when(delito_corto == "Delincuencia organizada" |
                              delito_corto == "Robo a transportista" |
                              delito_corto == "Robo a casa habitación" |
                              delito_corto == "Hidrocarburos y sus derivados"|
                              delito_corto == "Feminicidio" |
                              delito_corto == "Enriquecimiento ilícito"|
                              delito_corto == "Ejercicio abusivo de funciones" |
                              delito_corto == "Desaparición forzada de personas" |
                              delito_corto == "Armas de fuego" |
                              delito_corto == "Delitos electorales" |
                              delito_corto == "Homicidio" |
                              delito_corto == "Secuestro" |
                              delito_corto == "Violación" |
                              delito_corto == "Trata de personas" |
                              delito_corto == "Privación de la libertad" |
                              delito_corto == "Posesión con fines de comercio o suministro"|
                              delito_corto == "Suministro de narcóticos"|
                              delito_corto == "Comercio de narcóticos"|
                              delito_corto == "Contra la salud" ~ "PPO",
                              delito_corto != "Delincuencia organizada" |
                              delito_corto != "Robo a transportista" |
                              delito_corto != "Robo a casa habitación" |
                              delito_corto != "Hidrocarburos y sus derivados"|
                              delito_corto != "Feminicidio" |
                              delito_corto != "Enriquecimiento ilícito"|
                              delito_corto != "Ejercicio abusivo de funciones" |
                              delito_corto != "Desaparición forzada de personas" |
                              delito_corto != "Delitos electorales" |
                              delito_corto != "Armas de fuego" |
                              delito_corto != "Homicidio" |
                              delito_corto != "Secuestro" |
                              delito_corto != "Violación" |
                              delito_corto != "Trata de personas" |
                              delito_corto != "Privación de la libertad" |
                              delito_corto != "Posesión con fines de comercio o suministro"|
                              delito_corto != "Suministro de narcóticos"|
                              delito_corto != "Comercio de narcóticos"|
                              delito_corto != "Contra la salud" ~ "No PPO",
                            delito_corto == delito_corto ~ delito_corto))

ppl_ppo <- bind_rows(ppo_10, ppo_11, ppo_12, ppo_13, ppo_14, ppo_15,
                     ppo_16, ppo_17, ppo_18, ppo_19, ppo_20, ppo_21)

##### construcción de delitos con ppo vs no ppo POR REFORMAS
ppo_10_ref <- ppl %>%
    filter(year == 2010) %>%
    mutate(reforma = case_when(  delito_corto == "Delincuencia organizada" |
                                    delito_corto == "Homicidio" |
                                    delito_corto == "Secuestro" |
                                    delito_corto == "Violación" |
                                    delito_corto == "Contra la salud" ~ "Delitos que ameritaban PPO antes de la reforma de 2019",
                                delito_corto != "Delincuencia organizada" |
                                    delito_corto != "Homicidio" |
                                    delito_corto != "Secuestro" |
                                    delito_corto != "Violación" |
                                    delito_corto != "Contra la salud" ~ "Delitos que no ameritan prisión preventiva oficiosa",
                                delito_corto == delito_corto ~ delito_corto))

ppo_11_ref <- ppl %>%
    filter(year == 2011) %>%
    mutate(reforma = case_when(delito_corto == "Delincuencia organizada" |
                                  delito_corto == "Homicidio" |
                                  delito_corto == "Secuestro" |
                                  delito_corto == "Violación" |
                                  delito_corto == "Trata de personas" |
                                  delito_corto == "Contra la salud" ~ "Delitos que ameritaban PPO antes de la reforma de 2019",
                              delito_corto != "Delincuencia organizada" |
                                  delito_corto != "Homicidio" |
                                  delito_corto != "Secuestro" |
                                  delito_corto != "Violación" |
                                  delito_corto != "Trata de personas" |
                                  delito_corto != "Contra la salud" ~ "Delitos que no ameritan prisión preventiva oficiosa",
                              delito_corto == delito_corto ~ delito_corto))

ppo_12_ref <- ppl %>%
    filter(year == 2012) %>%
    mutate(reforma = case_when(delito_corto == "Delincuencia organizada" |
                                  delito_corto == "Homicidio" |
                                  delito_corto == "Secuestro" |
                                  delito_corto == "Violación" |
                                  delito_corto == "Trata de personas" |
                                  delito_corto == "Privación de la libertad" |
                                  delito_corto == "Contra la salud" ~ "Delitos que ameritaban PPO antes de la reforma de 2019",
                              delito_corto != "Delincuencia organizada" |
                                  delito_corto != "Homicidio" |
                                  delito_corto != "Secuestro" |
                                  delito_corto != "Violación" |
                                  delito_corto != "Trata de personas" |
                                  delito_corto != "Privación de la libertad" |
                                  delito_corto != "Contra la salud" ~ "Delitos que no ameritan prisión preventiva oficiosa",
                              delito_corto == delito_corto ~ delito_corto))

ppo_13_ref <- ppl %>%
    filter(year == 2013) %>%
    mutate(reforma = case_when(delito_corto == "Delincuencia organizada" |
                                  delito_corto == "Homicidio" |
                                  delito_corto == "Secuestro" |
                                  delito_corto == "Violación" |
                                  delito_corto == "Trata de personas" |
                                  delito_corto == "Privación de la libertad" |
                                  delito_corto == "Contra la salud" ~ "Delitos que ameritaban PPO antes de la reforma de 2019",
                              delito_corto != "Delincuencia organizada" |
                                  delito_corto != "Homicidio" |
                                  delito_corto != "Secuestro" |
                                  delito_corto != "Violación" |
                                  delito_corto != "Trata de personas" |
                                  delito_corto != "Privación de la libertad" |
                                  delito_corto != "Contra la salud" ~ "Delitos que no ameritan prisión preventiva oficiosa",
                              delito_corto == delito_corto ~ delito_corto))

ppo_14_ref <- ppl %>%
    filter(year == 2014) %>%
    mutate(reforma = case_when(delito_corto == "Delincuencia organizada" |
                                  delito_corto == "Homicidio" |
                                  delito_corto == "Secuestro" |
                                  delito_corto == "Violación" |
                                  delito_corto == "Trata de personas" |
                                  delito_corto == "Privación de la libertad" |
                                  delito_corto == "Contra la salud" ~ "Delitos que ameritaban PPO antes de la reforma de 2019",
                              delito_corto != "Delincuencia organizada" |
                                  delito_corto != "Homicidio" |
                                  delito_corto != "Secuestro" |
                                  delito_corto != "Violación" |
                                  delito_corto != "Trata de personas" |
                                  delito_corto != "Privación de la libertad" |
                                  delito_corto != "Contra la salud" ~ "Delitos que no ameritan prisión preventiva oficiosa",
                              delito_corto == delito_corto ~ delito_corto))

ppo_15_ref <- ppl %>%
    filter(year == 2015) %>%
    mutate(reforma = case_when(delito_corto == "Delincuencia organizada" |
                                  delito_corto == "Homicidio" |
                                  delito_corto == "Secuestro" |
                                  delito_corto == "Violación" |
                                  delito_corto == "Trata de personas" |
                                  delito_corto == "Privación de la libertad" |
                                  delito_corto == "Posesión con fines de venta y suministro"|
                                  delito_corto == "Venta o suministro"|
                                  delito_corto == "Contra la salud" ~ "Delitos que ameritaban PPO antes de la reforma de 2019",
                              delito_corto != "Delincuencia organizada" |
                                  delito_corto != "Homicidio" |
                                  delito_corto != "Secuestro" |
                                  delito_corto != "Violación" |
                                  delito_corto != "Trata de personas" |
                                  delito_corto != "Privación de la libertad" |
                                  delito_corto != "Posesión con fines de venta y suministro"|
                                  delito_corto != "Venta o suministro"|
                                  delito_corto != "Contra la salud" ~ "Delitos que no ameritan prisión preventiva oficiosa",
                              delito_corto == delito_corto ~ delito_corto))

ppo_16_ref <- ppl %>%
    filter(year == 2016) %>%
    mutate(reforma = case_when(delito_corto == "Delincuencia organizada" |
                                  delito_corto == "Homicidio" |
                                  delito_corto == "Secuestro" |
                                  delito_corto == "Violación" |
                                  delito_corto == "Trata de personas" |
                                  delito_corto == "Privación de la libertad" |
                                  delito_corto == "Posesión con fines de venta y suministro"|
                                  delito_corto == "Venta o suministro"|
                                  delito_corto == "Comercio de narcóticos"|
                                  delito_corto == "Contra la salud" ~ "Delitos que ameritaban PPO antes de la reforma de 2019",
                              delito_corto != "Delincuencia organizada" |
                                  delito_corto != "Homicidio" |
                                  delito_corto != "Secuestro" |
                                  delito_corto != "Violación" |
                                  delito_corto != "Trata de personas" |
                                  delito_corto != "Privación de la libertad" |
                                  delito_corto != "Posesión con fines de venta y suministro"|
                                  delito_corto != "Venta o suministro"|
                                  delito_corto != "Comercio de narcóticos"|
                                  delito_corto != "Contra la salud" ~ "Delitos que no ameritan prisión preventiva oficiosa",
                              delito_corto == delito_corto ~ delito_corto))

ppo_17_ref <- ppl %>%
    filter(year == 2017) %>%
    mutate(reforma = case_when(delito_corto == "Delincuencia organizada" |
                                  delito_corto == "Homicidio" |
                                  delito_corto == "Secuestro" |
                                  delito_corto == "Violación" |
                                  delito_corto == "Trata de personas" |
                                  delito_corto == "Privación de la libertad" |
                                  delito_corto == "Posesión con fines de comercio o suministro"|
                                  delito_corto == "Suministro de narcóticos"|
                                  delito_corto == "Comercio de narcóticos"|
                                  delito_corto == "Contra la salud" ~ "Delitos que ameritaban PPO antes de la reforma de 2019",
                              delito_corto != "Delincuencia organizada" |
                                  delito_corto != "Homicidio" |
                                  delito_corto != "Secuestro" |
                                  delito_corto != "Violación" |
                                  delito_corto != "Trata de personas" |
                                  delito_corto != "Privación de la libertad" |
                                  delito_corto != "Posesión con fines de comercio o suministro"|
                                  delito_corto != "Suministro de narcóticos"|
                                  delito_corto != "Comercio de narcóticos"|
                                  delito_corto != "Contra la salud" ~ "Delitos que no ameritan prisión preventiva oficiosa",
                              delito_corto == delito_corto ~ delito_corto))

ppo_18_ref <- ppl %>%
    filter(year == 2018) %>%
    mutate(reforma = case_when(delito_corto == "Delincuencia organizada" |
                                  delito_corto == "Homicidio" |
                                  delito_corto == "Secuestro" |
                                  delito_corto == "Violación" |
                                  delito_corto == "Trata de personas" |
                                  delito_corto == "Privación de la libertad" |
                                  delito_corto == "Posesión con fines de comercio o suministro"|
                                  delito_corto == "Suministro de narcóticos"|
                                  delito_corto == "Comercio de narcóticos"|
                                  delito_corto == "Contra la salud" ~ "Delitos que ameritaban PPO antes de la reforma de 2019",
                              delito_corto != "Delincuencia organizada" |
                                  delito_corto != "Homicidio" |
                                  delito_corto != "Secuestro" |
                                  delito_corto != "Violación" |
                                  delito_corto != "Trata de personas" |
                                  delito_corto != "Privación de la libertad" |
                                  delito_corto != "Posesión con fines de comercio o suministro"|
                                  delito_corto != "Suministro de narcóticos"|
                                  delito_corto != "Comercio de narcóticos"|
                                  delito_corto != "Contra la salud" ~ "Delitos que no ameritan prisión preventiva oficiosa",
                              delito_corto == delito_corto ~ delito_corto))

ppo_19_ref <- ppl %>%
    filter(year == 2019) %>%
    mutate(reforma = case_when(delito_corto == "Delincuencia organizada" |
                                    delito_corto == "Homicidio" |
                                    delito_corto == "Secuestro" |
                                    delito_corto == "Violación" |
                                    delito_corto == "Trata de personas" |
                                    delito_corto == "Privación de la libertad" |
                                    delito_corto == "Posesión con fines de comercio o suministro"|
                                    delito_corto == "Suministro de narcóticos"|
                                    delito_corto == "Comercio de narcóticos"|
                                    delito_corto == "Contra la salud" ~ "Delitos que ameritaban PPO antes de la reforma de 2019",
                               delito_corto == "Robo a transportista" |
                                   delito_corto == "Robo a casa habitación" |
                                   delito_corto == "Hidrocarburos y sus derivados"|
                                   delito_corto == "Feminicidio" |
                                   delito_corto == "Enriquecimiento ilícito"|
                                   delito_corto == "Ejercicio abusivo de funciones" |
                                   delito_corto == "Desaparición forzada de personas" |
                                   delito_corto == "Armas de fuego" |
                                   delito_corto == "Delitos electorales" ~ "Delitos que ameritaron PPO con la reforma de 2019",
                                  delito_corto != "Delincuencia organizada" |
                                  delito_corto != "Robo a transportista" |
                                  delito_corto != "Robo a casa habitación" |
                                  delito_corto != "Hidrocarburos y sus derivados"|
                                  delito_corto != "Feminicidio" |
                                  delito_corto != "Enriquecimiento ilícito"|
                                  delito_corto != "Ejercicio abusivo de funciones" |
                                  delito_corto != "Desaparición forzada de personas" |
                                  delito_corto != "Delitos electorales" |
                                  delito_corto != "Armas de fuego" |
                                  delito_corto != "Homicidio" |
                                  delito_corto != "Secuestro" |
                                  delito_corto != "Violación" |
                                  delito_corto != "Trata de personas" |
                                  delito_corto != "Privación de la libertad" |
                                  delito_corto != "Posesión con fines de comercio o suministro"|
                                  delito_corto != "Suministro de narcóticos"|
                                  delito_corto != "Comercio de narcóticos"|
                                  delito_corto != "Contra la salud" ~ "Delitos que no ameritan prisión preventiva oficiosa",
                                  delito_corto == delito_corto ~ delito_corto))

ppo_20_ref <- ppl %>%
    filter(year == 2020) %>%
    mutate(reforma = case_when(delito_corto == "Delincuencia organizada" |
                                   delito_corto == "Homicidio" |
                                   delito_corto == "Secuestro" |
                                   delito_corto == "Violación" |
                                   delito_corto == "Trata de personas" |
                                   delito_corto == "Privación de la libertad" |
                                   delito_corto == "Posesión con fines de comercio o suministro"|
                                   delito_corto == "Suministro de narcóticos"|
                                   delito_corto == "Comercio de narcóticos"|
                                   delito_corto == "Contra la salud" ~ "Delitos que ameritaban PPO antes de la reforma de 2019",
                               delito_corto == "Robo a transportista" |
                                   delito_corto == "Robo a casa habitación" |
                                   delito_corto == "Hidrocarburos y sus derivados"|
                                   delito_corto == "Feminicidio" |
                                   delito_corto == "Enriquecimiento ilícito"|
                                   delito_corto == "Ejercicio abusivo de funciones" |
                                   delito_corto == "Desaparición forzada de personas" |
                                   delito_corto == "Armas de fuego" |
                                   delito_corto == "Delitos electorales" ~ "Delitos que ameritaron PPO con la reforma de 2019",
                               delito_corto != "Delincuencia organizada" |
                                   delito_corto != "Robo a transportista" |
                                   delito_corto != "Robo a casa habitación" |
                                   delito_corto != "Hidrocarburos y sus derivados"|
                                   delito_corto != "Feminicidio" |
                                   delito_corto != "Enriquecimiento ilícito"|
                                   delito_corto != "Ejercicio abusivo de funciones" |
                                   delito_corto != "Desaparición forzada de personas" |
                                   delito_corto != "Delitos electorales" |
                                   delito_corto != "Armas de fuego" |
                                   delito_corto != "Homicidio" |
                                   delito_corto != "Secuestro" |
                                   delito_corto != "Violación" |
                                   delito_corto != "Trata de personas" |
                                   delito_corto != "Privación de la libertad" |
                                   delito_corto != "Posesión con fines de comercio o suministro"|
                                   delito_corto != "Suministro de narcóticos"|
                                   delito_corto != "Comercio de narcóticos"|
                                   delito_corto != "Contra la salud" ~ "Delitos que no ameritan prisión preventiva oficiosa",
                               delito_corto == delito_corto ~ delito_corto))

ppo_21_ref <- ppl %>%
    filter(year == 2021) %>%
    mutate(reforma = case_when(delito_corto == "Delincuencia organizada" |
                                   delito_corto == "Homicidio" |
                                   delito_corto == "Secuestro" |
                                   delito_corto == "Violación" |
                                   delito_corto == "Trata de personas" |
                                   delito_corto == "Privación de la libertad" |
                                   delito_corto == "Posesión con fines de comercio o suministro"|
                                   delito_corto == "Suministro de narcóticos"|
                                   delito_corto == "Comercio de narcóticos"|
                                   delito_corto == "Contra la salud" ~ "Delitos que ameritaban PPO antes de la reforma de 2019",
                               delito_corto == "Robo a transportista" |
                                   delito_corto == "Robo a casa habitación" |
                                   delito_corto == "Hidrocarburos y sus derivados"|
                                   delito_corto == "Feminicidio" |
                                   delito_corto == "Enriquecimiento ilícito"|
                                   delito_corto == "Ejercicio abusivo de funciones" |
                                   delito_corto == "Desaparición forzada de personas" |
                                   delito_corto == "Armas de fuego" |
                                   delito_corto == "Delitos electorales" ~ "Delitos que ameritaron PPO con la reforma de 2019",
                               delito_corto != "Delincuencia organizada" |
                                   delito_corto != "Robo a transportista" |
                                   delito_corto != "Robo a casa habitación" |
                                   delito_corto != "Hidrocarburos y sus derivados"|
                                   delito_corto != "Feminicidio" |
                                   delito_corto != "Enriquecimiento ilícito"|
                                   delito_corto != "Ejercicio abusivo de funciones" |
                                   delito_corto != "Desaparición forzada de personas" |
                                   delito_corto != "Delitos electorales" |
                                   delito_corto != "Armas de fuego" |
                                   delito_corto != "Homicidio" |
                                   delito_corto != "Secuestro" |
                                   delito_corto != "Violación" |
                                   delito_corto != "Trata de personas" |
                                   delito_corto != "Privación de la libertad" |
                                   delito_corto != "Posesión con fines de comercio o suministro"|
                                   delito_corto != "Suministro de narcóticos"|
                                   delito_corto != "Comercio de narcóticos"|
                                   delito_corto != "Contra la salud" ~ "Delitos que no ameritan prisión preventiva oficiosa",
                               delito_corto == delito_corto ~ delito_corto))

base_reforma <- bind_rows(ppo_10_ref, ppo_11_ref, ppo_12_ref, ppo_13_ref, ppo_14_ref, ppo_15_ref,
                     ppo_16_ref, ppo_17_ref, ppo_18_ref, ppo_19_ref, ppo_20_ref, ppo_21_ref)

#distribución personas sin sentencia y delitos pre y post reforma 2019
prueba <- base_reforma %>%
    filter(estatus == "Sin sentencia") %>%
    group_by(reforma, year)%>%
    summarize(tot_personas = sum(total, na.rm=T))%>%
    ungroup()%>%
    group_by(year)%>%
    mutate(tot_por_sexo = sum(tot_personas, na.rm=T),
           porcent = round(tot_personas / tot_por_sexo * 100, digits=1), na.rm=T)

prueba$reforma <- gsub("Delitos que no ameritan prisión preventiva oficiosa", "Delitos que no ameritan\nprisión preventiva oficiosa", prueba$reforma)
prueba$reforma <- gsub("Delitos que ameritaron PPO con la reforma de 2019", "Delitos que ameritaron prisión preventiva\noficiosa a partir de 2019", prueba$reforma)
prueba$reforma <- gsub("Delitos que ameritaban PPO antes de la reforma de 2019", "Delitos que ya ameritaban prisión\npreventiva oficiosa antes de 2019", prueba$reforma)

prueba$reforma <- factor(prueba$reforma, levels = c("Delitos que no ameritan\nprisión preventiva oficiosa",
                                                    "Delitos que ya ameritaban prisión\npreventiva oficiosa antes de 2019",
                                                    "Delitos que ameritaron prisión preventiva\noficiosa a partir de 2019"))

ggplot(prueba, aes(x = year, y=porcent, fill= reforma)) +
    geom_bar(stat="identity", position="stack") +
    scale_fill_manual(values = (v_azules)) +
    geom_label(aes(label=paste0(porcent, "%"), group = reforma),
               position = position_stack(1), size=3, hjust=.5, vjust=.5, 
               angle = 0, color="black", fill = "white", family = "Fira Sans")+
    labs(title="Los delitos que se le atribuyen a personas\nen prisión preventiva en México, ¿ya ameritaban\nprisión preventiva oficiosa o no?",
         caption="\n Fuentes: Censo Nacional de Gobierno, Seguridad Pública y Sistema Penitenciario Estatales 2011 a 2020 y\nCenso Nacional de Sistemas Penitenciarios Estatales 2021 y 2022.
       Los datos fueron procesados por Intersecta (intersecta.org).\
       \n", x="", y="\n Porcentaje \n",
       subtitle = "Por año \n", fill="Tipo de delito") +
    scale_y_continuous(label = scales::percent_format()) +
    tema +
    showtext_auto()+
    theme(legend.position = "top",
          plot.title = element_text(family = "Fira Sans", face = "bold", size=15))  
ggsave(paste(out2, "reforma_sin_sentencia.png", sep = "/"), 
       device = "png", type = "cairo",
       width = 9, height = 6)

#distribución personas sin sentencia y tipo de delitos
delitos <- ppl_ppo %>%
    filter(estatus == "Sin sentencia") %>%
    group_by(pponel, year)%>%
    summarize(tot_personas = sum(total, na.rm=T))%>%
    ungroup()%>%
    group_by(year)%>%
    mutate(tot_por_sexo = sum(tot_personas, na.rm=T),
           porcent = round(tot_personas / tot_por_sexo * 100, digits=1), na.rm=T)

delitos$pponel <- gsub("No PPO", "Delitos que no ameritan\nprisión preventiva oficiosa", delitos$pponel)
delitos$pponel <- gsub("PPO", "Delitos que ameritan\nprisión preventiva oficiosa", delitos$pponel)

delitos$pponel <- factor(delitos$pponel, levels = c("Delitos que no ameritan\nprisión preventiva oficiosa",
                                                    "Delitos que ameritan\nprisión preventiva oficiosa"))

ggplot(delitos, aes(x = year, y=porcent, fill= pponel)) +
    geom_bar(stat="identity", position="stack") +
    scale_fill_manual(values = (v_azules)) +
    geom_label(aes(label=paste0(porcent, "%"), group = pponel),
               position = position_stack(1), size=3, hjust=.5, vjust=.5, 
               angle = 0, color="black", fill = "white", family = "Fira Sans")+
    labs(title="Los delitos que se le atribuyen a personas en prisión preventiva en México,\n¿ameritan prisión preventiva oficiosa o no?",
         caption="\n Fuentes: Censo Nacional de Gobierno, Seguridad Pública y Sistema Penitenciario Estatales 2011 a 2020 y\nCenso Nacional de Sistemas Penitenciarios Estatales 2021 y 2022.
       Los datos fueron procesados por Intersecta (intersecta.org).\
       \n", x="", y="\n Porcentaje \n",
       subtitle = "Por año \n", fill="Tipo de delito") +
    tema +
    showtext_auto()+
    theme(legend.position = "top",
          plot.title = element_text(family = "Fira Sans", face = "bold", size=15))  
ggsave(paste(out2, "delitos_sin_sentencia.png", sep = "/"), 
       device = "png", type = "cairo", 
       width = 9, height = 6)

#proporción de hombres y mujeres sin sentencia por delitos de PPO
ppo_sex <- ppl_ppo %>%
    filter(estatus == "Sin sentencia")%>%
    group_by(pponel, year, sexo)%>%
    summarize(tot_personas = sum(total, na.rm=T))%>%
    ungroup()%>%
    group_by(year, sexo)%>%
    mutate(tot_por_sexo = sum(tot_personas, na.rm=T),
           porcent = round(tot_personas / tot_por_sexo * 100, digits=1), na.rm=T)%>%
    filter(pponel == "PPO")

ggplot(ppo_sex, aes(x=year, y=porcent, group=sexo)) +
    geom_line(aes(color=sexo),size=1.5) +
    geom_point(aes(color=sexo),size=2) +
    scale_color_manual(values = (v_rojos)) +
    #geom_label(aes(label=paste0(porcent, "%"), color = sexo),
               #position = position_stack(1), size=3, hjust=.5, vjust=.5, 
               #angle = 0, color="black", fill = "white", family = "Fira Sans")+
    labs(title="De los delitos que se les atribuyen a hombres y mujeres en prisión preventiva,\n¿qué porcentaje amerita prisión preventiva oficiosa?",
         caption="\n Fuentes: Censo Nacional de Gobierno, Seguridad Pública y Sistema Penitenciario Estatales 2011 a 2020 y\nCenso Nacional de Sistemas Penitenciarios Estatales 2021 y 2022.
       Los datos fueron procesados por Intersecta (intersecta.org).\
       \n", x="", y="\n Porcentaje \n",
       subtitle = "Por año \n", color="Sexo") +
    tema +
    showtext_auto()+
    theme(legend.position = "top",
          plot.title = element_text(family = "Fira Sans", face = "bold"))  
ggsave(paste(out2, "ppo_sexo.png", sep = "/"), width = 9, height = 6)

#¿cuáles delitos son los que tienen ppo?
deli_ppo <- filter(ppl_ppo, pponel == "PPO")
prueba <- deli_ppo %>% 
    filter(estatus == "Sin sentencia") %>% 
    mutate(deli = case_when(delito_corto == "Armas de fuego" ~ "Armas de fuego",
                            delito_corto == "Comercio de narcóticos" ~ "Contra la salud",
                            delito_corto == "Contra la salud" ~ "Contra la salud",
                            delito_corto == "Delincuencia organizada" ~ "Delincuencia organizada",
                            delito_corto == "Delitos electorales" ~ "Delitos electorales",
                            delito_corto == "Ejercicio abusivo de funciones" ~ "Corrupción",
                            delito_corto == "Enriquecimiento ilícito" ~ "Corrupción",
                            delito_corto == "Feminicidio" ~ "Feminicidio",
                            delito_corto == "Hidrocarburos y sus derivados" ~ "Hidrocarburos y sus derivados",
                            delito_corto == "Homicidio" ~ "Homicidio",
                            delito_corto == "Posesión con fines de comercio o suministro" ~ "Contra la salud",
                            delito_corto == "Posesión con fines de venta y suministro" ~ "Contra la salud",
                            delito_corto == "Privación de la libertad" ~ "Secuestro/privación de la libertad",
                            delito_corto == "Robo a casa habitación" ~ "Robo a casa habitación",
                            delito_corto == "Robo a transportista" ~ "Robo a transportista",
                            delito_corto == "Secuestro" ~ "Secuestro/privación de la libertad",
                            delito_corto == "Suministro de narcóticos" ~ "Contra la salud",
                            delito_corto == "Trata de personas" ~ "Trata de personas",
                            delito_corto == "Venta o suministro" ~ "Contra la salud",
                            delito_corto == "Violación" ~ "Violación"))%>%
    group_by(deli, year)%>%
    summarize(tot_personas = sum(total, na.rm=T))%>%
    ungroup()%>%
    group_by(year)%>%
    mutate(tot_por_sexo = sum(tot_personas, na.rm=T),
           porcent = round(tot_personas / tot_por_sexo * 100, digits=1), na.rm=T) 

prueba$deli <- factor(prueba$deli, levels = c("Homicidio", "Secuestro/privación de la libertad", "Contra la salud", 
                                                  "Violación", "Delincuencia organizada", "Trata de personas","Armas de fuego", "Robo a casa habitación",
                                                  "Feminicidio", "Hidrocarburos y sus derivados", "Robo a transportista", "Delitos electorales",
                                                  "Corrupción"))

ggplot(prueba, aes(x = year, y=porcent, fill= "")) +
    geom_bar(stat="identity", position="stack", color="black") +
    scale_fill_manual(values = v_azules) +
    #geom_label(aes(label=paste0(porcent, "%"), group = deli),
    #position = position_stack(1), size=3, hjust=.5, vjust=.5, 
    #angle = 0, color="black", fill = "white", family = "Avenir Next Condensed")+
    labs(title="¿Cuáles son los delitos que ameritan prisión preventiva oficiosa\nque se le atribuyen a las personas en prisión preventiva?",
         caption="\n Fuentes: Censo Nacional de Gobierno, Seguridad Pública y Sistema Penitenciario Estatales 2011 a 2020\ny Censo Nacional de Sistemas Penitenciarios Estatales 2021 y 2022.
       Los datos fueron procesados por Intersecta (intersecta.org).\
       \n", x="", y="\n Porcentaje \n",
       subtitle = "Para cada año, se muestra lo que cada delito representa del total \n", fill="") +
    facet_wrap(~ deli)+
    tema +
    theme(legend.position = "none",
          axis.text.x = element_text(angle=90))
ggsave(paste(out2, "delito_ppo.png", sep = "/"), width = 11, height = 7)

#delitos que ameritan ppo reforma por rango de años
entidad <- base_reforma %>%
    filter(estatus == "Sin sentencia") %>%
    mutate(tiempo = case_when(year < 2019 ~ "2010 a 2018",
                              year > 2018 ~ "2019 a 2021")) %>%
    group_by(reforma, entidad, tiempo)%>%
    summarize(tot_personas = sum(total, na.rm=T))%>%
    ungroup()%>%
    group_by(entidad, tiempo)%>%
    mutate(tot_por_sexo = sum(tot_personas, na.rm=T),
           porcent = round(tot_personas / tot_por_sexo * 100, digits=1), na.rm=T)
    
entidad$reforma <- gsub("Delitos que no ameritan prisión preventiva oficiosa", "Delitos que no ameritan\nprisión preventiva oficiosa", entidad$reforma)
entidad$reforma <- gsub("Delitos que ameritaron PPO con la reforma de 2019", "Delitos que ameritaron prisión preventiva\noficiosa a partir de 2019", entidad$reforma)
entidad$reforma <- gsub("Delitos que ameritaban PPO antes de la reforma de 2019", "Delitos que ya ameritaban prisión\npreventiva oficiosa antes de 2019", entidad$reforma)

entidad$reforma <- factor(entidad$reforma, levels = c("Delitos que no ameritan\nprisión preventiva oficiosa",
                                                    "Delitos que ya ameritaban prisión\npreventiva oficiosa antes de 2019",
                                                    "Delitos que ameritaron prisión preventiva\noficiosa a partir de 2019"))

ggplot(entidad, aes(x = tiempo, y=porcent, fill= reforma)) +
    geom_bar(stat="identity", position="stack") +
    scale_fill_manual(values = (v_azules)) +
    #geom_label(aes(label=paste0(porcent, "%"), group = pponel),
               #position = position_stack(1), size=3, hjust=.5, vjust=.5, 
               #angle = 0, color="black", fill = "white", family = "Fira Sans")+
    labs(title="Los delitos que se le atribuyen a personas en prisión preventiva,\n¿ya ameritaban prisión preventiva oficiosa o no?",
         caption="\n Fuentes: Censo Nacional de Gobierno, Seguridad Pública y Sistema Penitenciario Estatales 2011 a 2020 y\nCenso Nacional de Sistemas Penitenciarios Estatales 2021 y 2022.
       Los datos fueron procesados por Intersecta (intersecta.org).\
       \n", x="", y="\n Porcentaje \n",
       subtitle = "Por rango de años y entidad \n", fill="Tipo de delito") +
    tema +
    facet_wrap(~ entidad)+
    showtext_auto()+
    theme(legend.position = "top",
          plot.title = element_text(family = "Fira Sans", face = "bold"))  
ggsave(paste(out2, "entidad_reforma_pp.png", sep = "/"), width = 11, height = 7)

#delitos que ameritan ppo reforma por cada año
entidad <- ppl_ppo %>%
    filter(estatus == "Sin sentencia") %>%
    group_by(pponel, entidad, year)%>%
    summarize(tot_personas = sum(total, na.rm=T))%>%
    ungroup()%>%
    group_by(entidad, year)%>%
    mutate(tot_por_sexo = sum(tot_personas, na.rm=T),
           porcent = round(tot_personas / tot_por_sexo * 100, digits=1), na.rm=T)

entidad$pponel <- gsub("No PPO", "Delitos que no ameritan\nprisión preventiva oficiosa", entidad$pponel)
entidad$pponel <- gsub("PPO", "Delitos que ameritan\nprisión preventiva oficiosa", entidad$pponel)


entidad$pponel <- factor(entidad$pponel, levels = c("Delitos que no ameritan\nprisión preventiva oficiosa",
                                   "Delitos que ameritan\nprisión preventiva oficiosa"))

ggplot(entidad, aes(x = year, y=porcent, fill= pponel)) +
    geom_bar(stat="identity", position="stack") +
    scale_fill_manual(values = (v_azules)) +
    #geom_label(aes(label=paste0(porcent, "%"), group = pponel),
    #position = position_stack(1), size=3, hjust=.5, vjust=.5, 
    #angle = 0, color="black", fill = "white", family = "Fira Sans")+
    labs(title="Los delitos que se le atribuyen a personas en prisión preventiva,\n¿ameritan prisión preventiva oficiosa o no?",
         caption="\n Fuentes: Censo Nacional de Gobierno, Seguridad Pública y Sistema Penitenciario Estatales 2011 a 2020 y\nCenso Nacional de Sistemas Penitenciarios Estatales 2021 y 2022.
       Los datos fueron procesados por Intersecta (intersecta.org).\
       \n", x="", y="\n Porcentaje \n",
       subtitle = "Por año y entidad \n", fill="Tipo de delito") +
    tema +
    facet_wrap(~entidad)+
    showtext_auto()+
    theme(legend.position = "top",
          axis.text.x = element_text(angle=90),
          plot.title = element_text(family = "Fira Sans", face = "bold"))  
ggsave(paste(out2, "entidad_pp.png", sep = "/"), width = 11, height = 7)





