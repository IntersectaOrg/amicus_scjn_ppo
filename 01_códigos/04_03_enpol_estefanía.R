#------------------------------------------------------------------------------#
# Proyecto:                   Personas con sentencia y sin sentencia
# Objetivo:                   Extraer las tendencias de los Cuadernos mensuales de información estadística penitenciaria nacional
#                             Con base en el número mensual de personas privadas de la libertad
#
# Encargada:                  Estefanía Vela Baraba
# Correo:                     evela@intersecta.org
# Fecha de creación:          14 de julio de 2022
# Última actualización:       14 de julio de 2022
#------------------------------------------------------------------------------#

# 4D4C7D, 827397, 363062, E9D5DA, 88304E

rm(list=ls())

require(pacman)
p_load(tidyverse, stringi, dplyr, plyr, foreign, readxl, janitor, beepr, ggbump,
       extrafont, treemapify, ggmosaic, srvyr, ggrepel, lubridate, scales, ggtext)

options(dplyr.summarise.inform = FALSE) 

########## Temas para las gráficas

tema <-  theme_linedraw() +
    theme(text = element_text(family = "Avenir Next Condensed", color = "black"),
          plot.title = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(5,0,5,5), family="Avenir Next Condensed", color = "black"),
          plot.subtitle = element_text(size = 10, color = "#666666", hjust = 0.5, margin = margin(5, 0, 5, 5), family="Avenir Next Condensed"),
          plot.caption = element_text(hjust = .5, size = 8, family = "Avenir Next Condensed", color = "black"),
          panel.grid = element_blank(), 
          panel.border = element_blank(), 
          legend.position = "top",
          panel.grid.minor = element_blank(),
          legend.title = element_text(size = 8, face = "bold", family="Avenir Next Condensed"),
          legend.text = element_text(size = 8, family="Avenir Next Condensed"),
          axis.title = element_text(size = 8, hjust = .5, margin = margin(1,1,1,1), family="Avenir Next Condensed"),
          axis.text.y = element_text(size = 9, family="Avenir Next Condensed", angle=0, hjust=.5),
          axis.text.x = element_text(size = 6, family="Avenir Next Condensed", angle=90, vjust = 0.5, hjust = 1),
          strip.background = element_rect(fill="white", colour = NA),
          strip.text.x = element_text(size=9, family = "Avenir Next Condensed", face = "bold", color = "black"),
          strip.text.y = element_text(size=9, family = "Avenir Next Condensed"))


# 0. Directorios

inp     <- "cuadernos/"
pobl     <- "cuadernos/"
out     <- "figuras/"

# 1. Cargar datos --------------------------------------------------------------

base <- read_csv(paste0(inp, "integrada_86_22_0622.csv"))

# Datos poblacionales 
poblacion <- read_csv(paste(pobl, "pob_mitadanio_ents.csv", sep = "/"))

# 2. Limpieza de las bases de datos --------------------------------------------

base = filter(base, year > 2015)
base = filter(base, year < 2023)

poblacion = filter(poblacion, year > 2015)
poblacion = filter(poblacion, year < 2023) 
poblacion = filter(poblacion, edad >= 18) ######## solo la población adulta

######## Creación de fechas para poder hacer análisis mensuales 

base$month = base$mes
base$day = 1
base = mutate(base, fecha = as_date(paste(year, month, day))) 

poblacion = rename(poblacion, entidad = ent)
poblacion$entidad = gsub("San Luis PotosÌ", "San Luis Potosí", poblacion$entidad) ######## corregir SLP
poblacion$entidad = gsub("Ciudad de México", "CDMX", poblacion$entidad) ######## 
poblacion$entidad = gsub("México", "Estado de México", poblacion$entidad) ######## 
poblacion$entidad = gsub("CDMX", "Ciudad de México", poblacion$entidad) ######## 

base$estatus = gsub("Con sentencia", "Con condena", base$estatus) ######## 
base$estatus = gsub("Sin sentencia", "Sin condena", base$estatus) ######## 

base = filter(base, fecha > "2016-05-01")


#####

# Hombres vs mujeres en prisión preventiva

View(base)

personas = group_by(base, fecha, estatus)
personas = summarize(personas, total = sum(total, na.rm = T))
personas = ungroup(personas)
personas = group_by(personas, fecha)
personas = mutate(personas, total_total = sum(total, na.rm=T),
                  porcentaje = round(total / total_total * 100, 1))
personas = filter(personas, estatus == "Sin condena")

ggplot(data=subset(personas)) +
    geom_line(aes(x = fecha,  y = total), size=1, color = "#E9D5DA") +
    geom_point(aes(x = fecha, y = total), size=.5, color = "#363062") +
    geom_vline(xintercept = as.numeric(as.Date("2019-04-01")), linetype=2, color = "#88304E") +
    labs(title = "¿Cuántas personas había en prisión preventiva en México?", subtitle = "Por mes, desde junio de 2016 hasta junio de 2022 \n", 
         y = "\n Total de personas \n", x="",
         caption = "\n Nota: la línea guinda representa el mes en el que fue aprobada la reforma constitucional para ampliar la prisión preventiva oficiosa.
        Fuente: Cuaderno mensual de información estadística penitenciaria nacional.
       Los datos fueron procesados por Intersecta (intersecta.org).\n", 
         color ="") +
    tema+
    theme(axis.text.x = element_text(angle = 90, size = 4, vjust = 0.5, hjust = 0.5))+
    theme(strip.text.x = element_text(size=10, face="bold", angle=0, color = "black"),
          strip.text.y = element_text(size=10, face="bold", color = "black"),
          strip.background = element_rect(colour="black", fill="white")) +
    scale_x_date(date_labels="%Y-%m", date_breaks = "1 month", expand=c(.01,.01))
ggsave(paste(out, "2016 total por mes en pp.png", sep = "/"), width = 6, height = 4)


ggplot(data=subset(personas)) +
    geom_line(aes(x = fecha,  y = total), size=1, color = "#827397") +
    geom_point(aes(x = fecha, y = total), size=.5, color = "#363062") +
    geom_vline(xintercept = as.numeric(as.Date("2019-04-01")), linetype=2, color = "#88304E") +
    labs(title = "¿Cuántas personas había en prisión preventiva en México?", subtitle = "Por mes, desde junio de 2016 hasta junio de 2022 \n", 
         y = "\n Total de personas \n", x="",
         caption = "\n Nota: la línea guinda representa el mes en el que fue aprobada la reforma constitucional para ampliar la prisión preventiva oficiosa.
        Fuente: Cuaderno mensual de información estadística penitenciaria nacional.
       Los datos fueron procesados por Intersecta (intersecta.org).\n", 
         color ="") +
    tema+
    theme(axis.text.x = element_text(angle = 90, size = 4, vjust = 0.5, hjust = 0.5))+
    theme(strip.text.x = element_text(size=10, face="bold", angle=0, color = "black"),
          strip.text.y = element_text(size=10, face="bold", color = "black"),
          strip.background = element_rect(colour="black", fill="white")) +
    scale_x_date(date_labels="%Y-%m", date_breaks = "1 month", expand=c(.01,.01))
ggsave(paste(out, "2016 total por mes en pp 2.png", sep = "/"), width = 6, height = 4)


personas = group_by(base, fecha, estatus, sexo)
personas = summarize(personas, total = sum(total, na.rm = T))
personas = ungroup(personas)
personas = group_by(personas, fecha, sexo)
personas = mutate(personas, total_total = sum(total, na.rm=T),
                  porcentaje = round(total / total_total * 100, 1))
personas = filter(personas, estatus == "Sin condena")

View(personas)

ggplot(data=subset(personas)) +
    geom_line(aes(x = fecha,  y = total), size=1, color = "#457b9d") +
    geom_point(aes(x = fecha, y = total), size=.5, color = "#1d3557") +
    geom_vline(xintercept = as.numeric(as.Date("2019-04-01")), linetype=2, color = "red") +
    labs(title = "Número de personas en prisión preventiva en México", subtitle = "Por mes \n", 
         y = "\n Total de personas \n", x="",
         caption = "\n Nota: la línea roja representa el mes en el que fue aprobada la reforma constitucional para ampliar la prisión preventiva oficiosa.
        Fuente: Cuaderno mensual de información estadística penitenciaria nacional.
       Los datos fueron procesados por Intersecta (intersecta.org).\n", 
         color ="") +
    tema+
    facet_wrap(~sexo, scales = "free") +
    theme(axis.text.x = element_text(angle = 90, size = 3, vjust = 0.5, hjust = 0.5))+
    theme(strip.text.x = element_text(size=10, face="bold", angle=0, color = "black"),
          strip.text.y = element_text(size=10, face="bold", color = "black"),
          strip.background = element_rect(colour="black", fill="white")) +
    scale_x_date(date_labels="%Y-%m", date_breaks = "1 month", expand=c(.01,.01))
ggsave(paste(out, "2016 total por mes en pp sexo.png", sep = "/"), width = 8, height = 6)



#------------------------------------------------------------------------------#
# Proyecto:                   Personas con sentencia y sin sentencia
# Objetivo:                   Extraer las tendencias de los Cuadernos mensuales de información estadística penitenciaria nacional
#                             Con base en el número mensual de personas privadas de la libertad
#
# Encargada:                  Estefanía Vela Baraba
# Correo:                     evela@intersecta.org
# Fecha de creación:          14 de julio de 2022
# Última actualización:       14 de julio de 2022
#------------------------------------------------------------------------------#

rm(list=ls())

require(pacman)
p_load(tidyverse, stringi, dplyr, plyr, foreign, readxl, janitor, beepr, ggbump,
       extrafont, treemapify, ggmosaic, srvyr, ggrepel, lubridate, scales, ggtext)

########## Temas para las gráficas

tema <-  theme_linedraw() +
    theme(text = element_text(family = "Fira Sans", color = "black"),
          plot.title = element_text(size = 11, face = "bold", hjust = 0.5, margin = margin(5,0,5,5), family="Fira Sans", color = "black"),
          plot.subtitle = element_text(size = 10, color = "#666666", hjust = 0.5, margin = margin(5, 0, 5, 5), family="Fira Sans"),
          plot.caption = element_text(hjust = .5, size = 6, family = "Fira Sans", color = "black"),
          panel.grid = element_blank(), 
          panel.border = element_blank(), 
          legend.position = "top",
          panel.grid.minor = element_blank(),
          legend.title = element_text(size = 8, face = "bold", family="Fira Sans"),
          legend.text = element_text(size = 8, family="Fira Sans"),
          axis.title = element_text(size = 8, hjust = .5, margin = margin(1,1,1,1), family="Fira Sans"),
          axis.text.y = element_text(size = 9, family="Fira Sans", angle=0, hjust=.5),
          axis.text.x = element_text(size = 6, family="Fira Sans", angle=90, vjust = 0.5, hjust = 1),
          strip.background = element_rect(fill="white", colour = NA),
          strip.text.x = element_text(size=9, family = "Fira Sans", face = "bold", color = "black"),
          strip.text.y = element_text(size=9, family = "Fira Sans"))


# 0. Directorios

inp     <- "cuadernos/"
pobl     <- "cuadernos/"
out     <- "figuras/"

# 1. Cargar datos --------------------------------------------------------------

base <- read_xlsx(paste(inp, "enpol_tiemposinsentencia.xlsx", sep = "/"))

base$tiempo = gsub("De 3 meses a menos de 6 meses", "De 3 meses \na menos de 6 meses", base$tiempo) ######## 
base$tiempo = gsub("De 6 meses a menos de 1 año", "De 6 meses \na menos de 1 año", base$tiempo) ######## 
base$tiempo = gsub("De 1 año a menos de 2 años", "De 1 año \na menos de 2 años", base$tiempo) ######## 


View(base)

ggplot(base, aes(x = reorder(tiempo, clave), y=porcentaje, fill= sexo)) +
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = c("#4D4C7D", "#827397")) +
    geom_label(aes(label=paste0(porcentaje, "%"), group = sexo),
               position = position_dodge(1), size=2.5, hjust=.5, vjust=.5, 
               angle = 0, color="black", fill = "white", family = "Fira Sans")+
    labs(title="¿Cuánto tiempo llevaban esperando una sentencia las personas \nque se encontraban en prisión preventiva en México en 2021?",
         caption="\n Fuente: Resultados generales del Censo Nacional de Sistema Penitenciario Estatal y Federal (2022).
         Nota: El 5% restante en el caso de los hombres es 'No identificado'.", x="", y="\n Porcentaje \n",
         subtitle = "", fill="Sexo de las personas privadas de la libertad:") +
    tema +
    theme(legend.position = "top")+
    theme(axis.text.x = element_text(angle = 0, face = "bold", size = 6, vjust = 0.5, hjust = 0.5))
ggsave(paste(out, "tiempo sin sentencia.png", sep = "/"), width = 6, height = 4)


