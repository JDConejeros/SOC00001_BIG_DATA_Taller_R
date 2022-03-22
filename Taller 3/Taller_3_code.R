########################################################################/
# Talleres de Iniciación en R ------
# SOC00001 - BIG DATA - 09/03/2022
########################################################################/
# Taller 3: Procesamiento y visualización
# José Daniel Conejeros - jdconejeros@uc.cl
# Talleres: https://github.com/JDConejeros/SOC00001_BIG_DATA_Taller_R
########################################################################/

# Los objetivos del taller:
# (2) Manipulación de BBDD
# (1) Ejemplos avanzados de visualización 

# Fuente de los datos: 
# Ejemplo 1: http://datosabiertos.mineduc.cl/
# Ejemplo 2: https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto13/CasosNuevosCumulativo.csv

########################################################################/
# 0. Ajustes iniciales -------------
########################################################################/

# Desactivar notación científica y limpiar la memoria
options(scipen=999)
rm(list=(ls()))

# Vamos a cargar las las librerías 
#install.packages("") # Puedes escribir entre las comillas sino tienes instalada alguna librería
library(rio)       # Importar BBDD
library(dplyr)     # Manipulación de datos
library(forecast)  # Series de tiempo
library(ggplot2)   # Visualización
library(viridis)   # Gradiente de colores
library(RColorBrewer) # Paletas de colores
library(ggsci)        # Paletas de journals
library(lubridate) # Manejo de fechas  
library(tidyr)     # Manipulación de tablas
library(stringr)   # Manipular caracteres
library(gganimate) # Animaciones

# Referencias de colores
# https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
# https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html

########################################################################/
# 1. Procesamiento de varias bases de datos -------------
########################################################################/
# a. Abrimos las BBDD
datas <- list.files("Taller 3/input/", pattern = "*.csv")
datas
for(i in datas) { 
  name <- paste0("tit_", stringr::str_extract(i, pattern = "20[0-1][0-9]")) # Nombre para cada objeto
  data <- import(paste0("Taller 3/input/", i))               # Importamos
  assign(name, data) # Asignamos
}

# b. Realizamos otro loop para procesar las datas
names_data <- as.list(ls(pattern = "tit*"))
names_data
for(i in names_data) {
  data <- get(i)  # Cargamos la data 
  
  colnames(data) <- tolower(names(data))  # Nombres de las columnas en minúscula
  
  # Procesamiento de cada base de datos
  data <- data %>% 
    filter(!is.na(mrun) & nivel_global %in% c("Pregrado", "PREGRADO")) %>% 
    mutate(tit_univ = if_else(tipo_inst_1 %in% c("Universidades", "UNIVERSIDADES"), 1, 0),
           tit_ip = if_else(tipo_inst_1 %in% c("Institutos Profesionales", "INSTITUTOS PROFESIONALES"), 1, 0),
           tit_cft = if_else(tipo_inst_1 %in% c("Centros de Formación Técnica", "CENTROS DE FORMACIÓN TÉCNICA"), 1, 0)) %>% 
    select(cat_periodo, mrun, tit_univ, tit_ip, tit_cft) 
  
  # Ahora vamos a agrupar nuestro resultados por mrun
  data <- data %>% 
    group_by(mrun) %>% 
    mutate(tit_univ=sum(tit_univ),
           tit_ip=sum(tit_ip),
           tit_cft=sum(tit_cft)) %>% 
    ungroup()
  
  assign(i, data)
  
}

# c. Vamos a unir todo en una gran base de datos
data_append <- data.frame()
for(i in names_data) {
  # Cargamos la data 
  data <- get(i) 
  
  # Unimos la data 
  data_append <- rbind(data_append, data)
}
nrow(data_append)

# d. Validemos sumando las filas de todas las BBDD
rows <- c()
for(i in names_data) {
  data <- get(i) 
  rows[i] <- nrow(data)
}
sum(rows)

# e. Colapsamos todo para que nos quede por mrun el max valor en cada variable
data_append <- data_append %>% 
  group_by(mrun) %>% 
  mutate(tit_univ=max(tit_univ),
         tit_ip=max(tit_ip),
         tit_cft=max(tit_cft)) %>% 
  ungroup()

table(data_append$tit_univ)
table(data_append$tit_ip)
table(data_append$tit_cft)

# Ajustamos los valores 2 y 3
data_append <- data_append %>% 
  mutate(tit_univ=if_else(tit_univ>1, 1, tit_univ),
         tit_ip=if_else(tit_ip>1, 1, tit_ip),
         tit_cft=if_else(tit_cft>1, 1, tit_cft))

# Volvemos a revisar:
table(data_append$tit_univ)
table(data_append$tit_ip)
table(data_append$tit_cft)

########################################################################/
# 3. Algunas dudas realizadas -------------
########################################################################/

# Ajuste de la BBDD y uso de expresiones regulares:
# https://stringr.tidyverse.org/articles/regular-expressions.html
data_append 

# Ajustamos el año con una expresión regular
data_append <- data_append %>% 
  rename(agno=cat_periodo) %>% 
  mutate(agno=stringr::str_extract(agno, pattern = "20[0-1][0-9]"))

# Variaciones del filter
data_ejemplo1 <- data_append %>% filter(agno=="2012")
data_ejemplo2 <- data_append %>% filter(agno=="2012" & agno=="2013")
data_ejemplo2 <- data_append %>% filter(agno=="2012" | agno=="2013")   
data_ejemplo3 <- data_append %>% filter(c(agno=="2012", agno=="2013")) # ERROR
data_ejemplo4 <- data_append %>% filter(agno %in% c("2012", "2013"))

# Variaciones del arrange
data_ejemplo1 <- data_append %>% arrange(agno, mrun)
data_ejemplo2 <- data_append %>% arrange(agno, desc(mrun))

# Reorder para reordenar factores en un gráfico.
?reorder
data_ejemplo3 <- reorder(data_append$agno, data_append$mrun)

########################################################################/
# 4. Resumen de datos y visualización -------------
########################################################################/
rm(list=ls()[! ls() %in% c("data_append")])
# Generamos una tabla con resultados
# Titulados por año
titulados <- data_append %>% 
  group_by(agno) %>% 
  dplyr::summarise(n=n(),
                   prob_univ=mean(tit_univ),
                   prob_ip=mean(tit_ip),
                   prob_cft=mean(tit_cft)) 

titulados  

# Generamos una visualización

# Usemos el pivot_longer: ajusta los datos en un formato largo
tit_long <- titulados %>% 
  pivot_longer(cols=!c(agno, n), 
               names_to = "titulados", 
               values_to = c("prob"))
tit_long
# Forma Equivalente:
tit_long2 <- titulados %>% 
  gather(key="titulados", value="prob", -c(agno, n))
tit_long2 

# ¿Podemos volver a la base original?
names <- colnames(tit_long)[c(2,4)]
names

tit_wide <- tit_long %>% 
  pivot_wider(names_from = agno, 
              values_from = all_of(names), 
              names_sep="_") %>% 
  select(-starts_with("n_"))
tit_wide 

# Podría armar una figura
f1 <- tit_long %>% 
  mutate(agno=as.Date(as.factor(agno), format("%Y")),
         agno=round_date(agno, "year")) %>% 
  ggplot(aes(x=agno, y=prob, color=titulados)) +
  geom_line(aes(linetype=factor(titulados)), size=0.8) +
  geom_point(alpha = 0.5, size=2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     n.breaks = 10, limits=c(0, 1)) +
  scale_x_date(date_labels = "%Y",
               date_breaks = "1 year",
               expand = c(0.02, 0.02)) +
  scale_colour_manual(values= c("#f8766d","#67a9cf", "#17A589"), labels=c("Universidad", "IP", "CFT")) +
  geom_text(aes(label=round(prob,2)*100), size=4.5, vjust = -0.75, hjust="middle", fontface = "bold", show.legend = FALSE) + 
  labs(title="Titulados de la educación superior en los últimos 6 años", x="Año", y = "Porcentaje", 
       color = "Institución", caption=paste("Fuente: Centro de estudios MINEDUC.", "N =", nrow(data_append)))  +
  guides(linetype = FALSE) +
  theme_light(base_size = 12) + 
  theme(axis.text=element_text(size=12),
        legend.position="top",
        legend.text=element_text(size=12),
        axis.text.x = element_text(size=10),
        strip.background = element_rect(fill="white", colour="gray", linetype="dashed"),
        strip.text = element_text(size = 12, face = "bold", color = "gray40"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour="grey", linetype="dotted", size=0.3)) 

f1 # Ver la figura

png(file = "Taller 3/output/figura1.png", height = 620, width = 1000, pointsize = 16)
f1
dev.off()

########################################################################/
# 5. Serie de tiempo -------------
########################################################################/

# Cargamos la BBDD: 
# Reporte de casos nuevos COVID. Datos en formato ancho:
data <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto13/CasosNuevosCumulativo.csv")
data %>% glimpse()
View(data)

# Ajustamos como serie de tiempo
data_figura <- data %>% slice(-17) %>% 
  pivot_longer(cols=!Region, names_to="date", values_to = "casos") %>% # Pivoteamos la BBDD  
  mutate(date=str_replace_all(date, c("[X]"="", "[.]"="-")),           # Reemplazamos valores a partir de una expresión regular
         date=lubridate::ymd(date))                                    # Ajustamos la fecha

# Verifiquemos la cantidad de filas
data_figura %>% head(n=30)
unique(data_figura$Region) # Veamos los valores para las regiones

reg <- unique(data$Region)
reg

# Graficamos
# Podemos fijar una paleta de colores:  http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
display.brewer.all(colorblindFriendly = TRUE)
colores <- brewer.pal(n=4, name="Set2")
colores

g1 <- data_figura %>% 
  ggplot(aes(x=date, y=casos, color=Region)) +
  geom_line() +
  #scale_colour_manual(values=paleta) +
  #scale_color_viridis(discrete = TRUE, option = "D") +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "12 week") +
  labs(x="Semanas del año", y = "Casos de Covid-19",
       color = "Región")  +
  geom_vline(xintercept=ymd("2021-02-01"), linetype="dotdash") +
  annotate("text", x = ymd("2021-02-01")+120, y = 10, label = 'Inicio vacunación', size=3) + 
  facet_wrap(~factor(Region, levels=reg), ncol = 4, scale = "free") +
  theme_light(base_size = 12) + 
  theme(axis.text=element_text(size=8),
        legend.position="none",
        strip.text = element_text(size = 10, face = "bold", color = "gray40"),
        strip.background = element_rect(fill="white", colour="gray", linetype="dashed"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour="grey", linetype="dotted", size=0.3)) 
g1

png(file = "Taller 3/output/figura2.png", height = 620, width = 1000, pointsize = 16)
g1
dev.off()

########################################################################/
# 6. Animar figuras -------------
########################################################################/

ganimado <- g1 + transition_reveal(date) + 
  labs(title = "Casos de Covid-19 para el día: {frame_along}")  + 
  ease_aes('linear')

animate(ganimado, width = 1200, height = 900, fps = 25, duration = 25, 
        rewind = FALSE, renderer = gifski_renderer("Taller 3/output/gganim_covid2.gif"))

########################################################################/
# FIN TALLER 3 -------------
########################################################################/

