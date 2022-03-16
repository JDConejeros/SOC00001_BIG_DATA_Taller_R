########################################################################/
# Talleres de Iniciación en R ------
# SOC00001 - BIG DATA - 09/03/2022
########################################################################/
# Taller 2: Manipulación con DPLYR
# José Daniel Conejeros - jdconejeros@uc.cl
# Talleres: https://github.com/JDConejeros/SOC00001_BIG_DATA_Taller_R
########################################################################/

# En este taller nos enfocaremos a revisar las principales herramientas de 
# Tidyverse para la importación, procesamiento Y análisis 

# Fuente de los datos: http://datos.mineduc.cl/dashboards/20195/descarga-bases-de-datos-matricula-en-educacion-superior/

########################################################################/
# 1. Librerias de Tidyverse -------------
########################################################################/
# Puedes revisar el detalle de cada librería en: https://www.tidyverse.org/

# dplyr: procesamiento de variables y datos. 
# ggplot2: visualización de datos. 
# tidyr: trabajo con bases de datos ordenadas. 
# readr: lectura de bases de datos. 
# purrr: Herramienta para trabajar funciones, vectores e iteraciones. 
# tibble: gestiñon de marco de datos. 
# stringr: trabajo con variables de tipo caracter (textos).
# forcats: trabajo con variables de tipo factor (variables cualitativas). 

install.packages("tidyverse") # Podemos descargar todas las librerías
#library(tidyverse)

# Cargamos la BBDD
library(readr)
rendimiento_19 <- read_csv("Taller 2/input/Rendimiento_estudiantes_extracto_2019.csv") 

library(dplyr) 
# Uso del operador pipe %>% 
rendimiento_19 %>% glimpse() # Vista previa 
rendimiento_19 %>% head()    # Primera 6 observaciones
rendimiento_19 %>% colnames() # Nombres columnas/variables
rendimiento_19 %>% select(AGNO,NOM_COM_RBD,MRUN) %>% head(n=20)

# Ajustemos nombre de las variables en minúsculas
library(stringr)
colnames(rendimiento_19) <- str_to_lower(colnames(rendimiento_19))

rendimiento_19 %>% colnames()

########################################################################/
# 2. Procesamiento de BBDD -------------
########################################################################/

## 2.1 Select -------------
# Seleccionamos columnas/variables
data1 <- rendimiento_19 %>% select(10:14)
data2 <- rendimiento_19 %>% select("mrun", "agno", "cod_reg_rbd":"nom_com_rbd")
data3 <- rendimiento_19 %>% select("mrun", "rbd", "cod_depe", "cod_depe2", "cod_ense2", "asistencia")

data1 %>% colnames()
data2 %>% colnames()
data3 %>% colnames()

## 2.2 Rename -------------

data2 <- data2 %>% rename(id=mrun)
data3 <- data3 %>% rename(folio=mrun)

## 2.3 Filter -------------
# Filtramos por una condición 
table(data1$gen_alu) # 1: Masculino // 2: Femenino
data_m <- data1 %>% filter(gen_alu==1) # Masculino
data_f <- data1 %>% filter(gen_alu!=1) # Femenino

# Generamos dos condicionnes:
data_asistencia <- data3 %>% filter(asistencia>=90 & rbd==3442)
data_asistencia2 <- data3 %>% filter(asistencia>=90 | rbd==3442)
data_asistencia3 <- data3 %>% filter(asistencia>=90)

nrow(data_asistencia) # N de la muestra
nrow(data_asistencia2) # N de la muestra

## 2.4 Arrange -------------
# Ordenamos la BBDD con una variable 
head(data2, n=15)

data_arrange <- data2 %>% arrange(cod_com_rbd)  # Por defecto el orden es creciente
head(data_arrange, n=15)

data_arrange2 <- data2 %>% arrange(desc(cod_com_rbd)) # Decreciente
head(data_arrange2, n=15)

########################################################################/
# 3. Fundir BBDD -------------
########################################################################/

## 3.1 Append BBDD  -----------------------------------

# Dplyr
glimpse(data_m)
glimpse(data_f)

data_append <- data_m %>% add_row(data_f)
glimpse(data_append)
tail(data_append)
# R Base:
data_append2 <- rbind(data_m, data_f)

## 3.2 Join/Antijoin  -----------------------------------

# Un ejemplo senncillo
x <- data.frame(idx = 1:5, letras = letters[1:5])
y <- data.frame(idy = c(2:6,7), num = c(12:16,3))

x
y

# Trae los resultados de las tablas que cumplen con la condición de comparación entre columnas.
x %>% inner_join(y, by=c("idx"="idy")) #Utilizan una lleva para realizar el match. Solo los match.

# Trae todos los resultados de las tablas que cumplen con la condición de comparación entre columnas
# y, adicionalmente, trae todos los datos de la tabla de la izquierda.
x %>% left_join(y, by=c("idx"="idy"))

# Trae todos los resultados de las tablas que cumplen con la condición de comparación entre columnas
# y, adicionalmente, trae todos los datos de la tabla de la derecha.
x %>% right_join(y, by=c("idx"="idy"))

# Trae los resultados de las tablas que cumplen con la condición de comparación entre columnas, 
# además de los resultados de las o registros de las tablas de la derecha y la izquierda.
x %>% full_join(y, by=c("idx"="idy"))

# Trae los elementos que no tiene información para la base de destino.
x %>% anti_join(y, by=c("idx"="idy"))

########################################################################/
# 4. Procesamiento de variables -------------
########################################################################/
# Vamos a despejar la memoria 
rm(list=ls()[! ls() %in% c("rendimiento_19")])

## 4.1 Generemos una variable continua  ---------------------------
table(rendimiento_19$prom_gral)
rendimiento_19 <- rendimiento_19 %>% 
  mutate(promedio_ajustado=prom_gral,
         promedio_ajustado=if_else(promedio_ajustado>10 & promedio_ajustado<70, promedio_ajustado/10, promedio_ajustado),
         promedio_ajustado=if_else(promedio_ajustado==0, NA_real_, promedio_ajustado))

table(rendimiento_19$promedio_ajustado, useNA = "ifany")

summary(rendimiento_19$promedio_ajustado) # Vemos el resultado
rendimiento_19 %>% select(mrun, prom_gral, promedio_ajustado) %>% head()

## 4.2 Generemos un factor (categórica)  ---------------------------

#install.packages("labelled")
library(labelled) # Manejador de etiquetas
var_label(rendimiento_19$gen_alu)
set_variable_labels(rendimiento_19$gen_alu)

rendimiento_19 <- rendimiento_19 %>% mutate(genero=as.numeric(gen_alu)-1,
                          genero=factor(genero, 
                                      levels=c(0,1), 
                                      labels=c("Hombre", "Mujer")))

var_label(rendimiento_19$genero) <- "Género del estudiante"
var_label(rendimiento_19$genero)
set_variable_labels(rendimiento_19$genero)

rendimiento_19 %>% select(gen_alu, genero) %>% head()
table(rendimiento_19$gen_alu, rendimiento_19$genero, useNA = "ifany")

## 4.3 A partir del uso de condicionales  ---------------------------

summary(rendimiento_19$promedio_ajustado) 
table(rendimiento_19$promedio_ajustado, useNA = "ifany")
summary(is.na(rendimiento_19$promedio_ajustado)) # Veamos los missing

# Vamos a generar una variable categórica de 3 grupos
# 25% "inferior" (1)
# 50% "normal"   (2) 
# 25% "superior" (3)

### 4.3.1 if_else()  ---------------------------
quantile(rendimiento_19$promedio_ajustado, probs = 0.25, na.rm = T)
quantile(rendimiento_19$promedio_ajustado, probs = 0.75, na.rm = T)

rendimiento_19 <- rendimiento_19 %>% 
  mutate(prom_cat = if_else(promedio_ajustado<=quantile(promedio_ajustado, probs = 0.25, na.rm = T), 1, 
                         if_else(promedio_ajustado>quantile(promedio_ajustado, probs = 0.25, na.rm = T) &
                                   promedio_ajustado<quantile(promedio_ajustado, probs = 0.75, na.rm = T),2,
                                 if_else(promedio_ajustado>=quantile(promedio_ajustado, probs = 0.75, na.rm = T), 3, NA_real_))))

# Etiquetamos la variable
var_label(rendimiento_19$prom_cat) <- "Grupos de rendimiento académico" 

# Validamos 
table(rendimiento_19$prom_cat, useNA = "ifany")
aggregate(rendimiento_19$promedio_ajustado~rendimiento_19$prom_cat, FUN=summary)
rendimiento_19 %>% select(promedio_ajustado, prom_cat) %>% head()

### 4.3.2 case_when()  ---------------------------
rendimiento_19 <- rendimiento_19 %>% 
  mutate(prom_cat2 =  case_when(promedio_ajustado <= quantile(promedio_ajustado, probs = 0.25, na.rm = T)    ~ 1, 
                            promedio_ajustado > quantile(promedio_ajustado, probs = 0.25, na.rm = T) & 
                              promedio_ajustado < quantile(promedio_ajustado, probs = 0.75, na.rm = T) ~ 2, 
                            promedio_ajustado >= quantile(promedio_ajustado, probs = 0.75, na.rm = T)    ~ 3, 
                            TRUE ~ NA_real_))

# Validamos 
table(rendimiento_19$prom_cat, rendimiento_19$prom_cat2, useNA = "ifany")
rendimiento_19 %>% select(promedio_ajustado, prom_cat, prom_cat2) %>% head()

### 4.3.1 Indexación  ---------------------------
rendimiento_19$prom_cat3[rendimiento_19$promedio_ajustado <= quantile(rendimiento_19$promedio_ajustado, probs = 0.25, na.rm = T)] <- 1
rendimiento_19$prom_cat3[rendimiento_19$promedio_ajustado >  quantile(rendimiento_19$promedio_ajustado, probs = 0.25, na.rm = T) & 
              rendimiento_19$promedio_ajustado < quantile(rendimiento_19$promedio_ajustado, probs = 0.75, na.rm = T)] <- 2
rendimiento_19$prom_cat3[rendimiento_19$promedio_ajustado >= quantile(rendimiento_19$promedio_ajustado, probs = 0.75, na.rm = T)] <- 3

table(rendimiento_19$prom_cat, rendimiento_19$prom_cat3, useNA = "ifany")
rendimiento_19 %>% select(promedio_ajustado, prom_cat, prom_cat2, prom_cat3) %>% head()

# Eliminamos variables creadas
rendimiento_19$prom_cat2 <- NULL
rendimiento_19$prom_cat3 <- NULL

########################################################################/
# 5. Visualización -------------
########################################################################/
rm(list=ls()[! ls() %in% c("rendimiento_19")])

#install.packages("ggplot2")
# https://ggplot2.tidyverse.org/index.html

library(ggplot2)
ggplot(data=rendimiento_19)

## 5.1 Argumentos Básicos ----------------------------------------------------------------
# Datos: input de información para generar la figura. 
# Geometrías (geom_) forma geométrica que representaran los datos,
# Estética (aes()): estética de los objetos geométricos y estadísticos, 
# Escalas (scale): mapas entre los datos y las dimensiones estéticas, 
# Transformaciones estadísticas (stat_): resúmenes estadísticos de los datos
# Sistemas de coordenadas (coord_): mapear los datos del gráfico.
# Facetas:la disposición de los datos en una cuadrícula de gráficos.
# Temas (theme()): ajuste visuales del gráfico.

# Es importante ir agregando información por capas
ggplot(data=rendimiento_19, aes(x=asistencia, y=promedio_ajustado)) #Agrego ejes: capa2
g1 <- ggplot(data=rendimiento_19, aes(x=asistencia, y=promedio_ajustado)) #Guardo mi gráfico en un objeto
g1

rendimiento_19 <- rendimiento_19 %>% slice(1:50000)
# + vs %>% 

ggplot(data=rendimiento_19, aes(x=asistencia, y=promedio_ajustado)) +  
  geom_point()  #Agrego geometría: points. capa3

g1 + geom_point(size=0.5, alpha=0.5)

?geom_point

ggplot(data=rendimiento_19, aes(x=asistencia, y=promedio_ajustado)) + 
  geom_point(size=0.5, alpha=0.5) +  #Agrego geometría: points. capa3
  geom_line()  #Agrego geometría: lines. capa3

ggplot(data=rendimiento_19, aes(x=asistencia, y=promedio_ajustado))  + 
  geom_point(color="steelblue", shape="triangle", size=1, alpha=0.5)  # Ajusto parámetros de la capa

ggplot(data=rendimiento_19, aes(x=asistencia, y=promedio_ajustado)) + 
  geom_line(color = "firebrick", linetype = "dotted", size = 0.3)  # Ajusto parámetros de la capa

# Generemos un gráfico mejor 
ggplot(data=rendimiento_19, aes(x=asistencia, y=promedio_ajustado))  + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth(method = "lm") + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Asistencia del niño/a", y="Promedio de notas ajustado") 

ggplot(data=rendimiento_19, aes(x=asistencia, y=promedio_ajustado,color=factor(genero))) + 
  geom_point(size=0.2) + # Puedo ver lo que ocurre por grupos 
  geom_smooth(method = "lm") + # Agregar una línea de tendencia 
  labs(color = "Género") +
  labs(title = "Mi primer ggplot", x="Asistencia", y="Promedio de notas ajustado") +
  theme_light() +
  theme(plot.title = element_text(size = 15, face="bold"),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=10), 
        axis.title.y=element_text(size=10))

# Aplicación de temas:

# Podemos dejar fijo un tema con el siguiente código:
theme_set(theme_bw())

# theme_bw()
# theme_classic()
# theme_light()
# theme_minimal()

## 5.2 Facetas  ----------------------------------------------------------------

# Podemos agregar facetas
ggplot(data=rendimiento_19, aes(x=asistencia, y=promedio_ajustado, color=factor(genero))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth(method = "lm") + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Asistencia", y="Promedio de notas ajustado", 
       caption="Fuente: Elaboración propia") + 
  labs(color = "Género") +
  scale_color_manual(values = c("#67a9cf", "#f8766d")) + 
  theme_light() +
  facet_wrap(~factor(nom_reg_rbd_a), nrow = 4) 

## 5.3 Guardar gráficos  ----------------------------------------------------------------

# Camino 1: Directo
g1 <- ggplot(data=rendimiento_19, aes(x=asistencia, y=promedio_ajustado, color=factor(genero))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth(method = "lm") + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Asistencia", y="Promedio de notas ajustado") + 
  labs(color = "Género") +
  scale_color_manual(values = c("#67a9cf", "#f8766d")) + 
  theme_light() +
  facet_wrap(~factor(rendimiento_19$nom_reg_rbd_a), nrow = 4)  +
  theme(legend.position = "bottom") 

# Guardar gráficos (ajustar a ruta específica)
ggsave("Taller 2/output/figura1.png", plot=g1)
?ggsave

# Camino 2: Múltiples gráficos
unique(rendimiento_19$cod_depe2)
g2 <- ggplot(data=rendimiento_19, aes(x=asistencia, y=promedio_ajustado, color=factor(genero))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth(method = "lm") + # Agregar una línea de tendencia 
  labs(title = "Mi segundo ggplot", x="Asistencia", y="Promedio de notas ajustado") + 
  labs(color = "Género") +
  scale_color_manual(values = c("#67a9cf", "#f8766d")) + 
  theme_light() +
  facet_wrap(~factor(rendimiento_19$cod_depe2, 
                     labels=c("Municipal", "Part. Subvencionado",
                              "Part. Pagado", "Corp. Adm. Delegada",
                              "Serv. Local")), nrow = 3)  +
  theme(legend.position = "bottom") 

ggsave("Taller 2/output/figura2.png", plot=g2)

#install.packages("ggpubr")
library(ggpubr)
g3 <- ggarrange(g1, g2, ncol=1, nrow=2, common.legend = TRUE, legend="top")
ggsave("Taller 2/output/figura3.png", plot=g3)

# Camino 3: Ejecutar y guardar
png(file = "Taller 2/output/figura4.png", height = 900, width = 1200)
ggarrange(g1, g2, ncol = 1, nrow = 2,  common.legend = TRUE, legend="right")
dev.off()

########################################################################/
# 6. Ejemplos de gráficos --------------------
########################################################################/

## 6.1 Histogramas ----------------------------------------------------------------

ggplot(data=rendimiento_19, aes(x=promedio_ajustado)) +
  geom_histogram(aes(y=..density..),
                 position = "identity", binwidth = 0.1,
                 colour="black", fill="white") +
  scale_x_continuous(limits = c(4,7)) +
  labs(title="Distribución Notas",
       x="Promedio general", y = "Densidad") +
  geom_density(col="red") + 
  theme_bw()

## 6.2 Gráficos de Barras ----------------------------------------------------------------
names <- c("Promovido", "Reprobado", "Retirado", "NA")

rendimiento_19 %>% 
  filter(!is.na(sit_fin)) %>% 
ggplot(aes(x=sit_fin, fill=as.factor(sit_fin))) + 
  geom_bar(color="black") +
  scale_fill_grey(labels=names) +
  xlab("Situación final del estudiante") + 
  ylab("Cantidades") +
  ggtitle("Gráfico de Barras") + 
  labs(fill = "Situación Final") +
  theme(legend.position = "top")

## 6.3 Gráficos de torta ----------------------------------------------------------------
pie_info <- rendimiento_19 %>%
  filter(!is.na(sit_fin)) %>% 
  dplyr::group_by(sit_fin) %>%
  dplyr::arrange(sit_fin) %>% 
  dplyr::summarize(n_all = n()) %>% 
  dplyr::mutate(prop=(n_all/sum(n_all)*100))

colores <- c("#0073C2FF", "#EFC000FF", "#868686FF")

ggplot(pie_info, aes(x = "", y = prop, fill = sit_fin)) +
  geom_bar(stat = "identity", color = "white", width = 1) +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(x=1, label = paste0(round(prop,1), "%")), position = position_stack(vjust = .5), 
            color = "white", size=5, fontface = "bold")+
  labs(title="Gráfico de torta", caption = "Fuente: Elaboración propia") +
  scale_fill_manual(values = colores, name="Dependencia") +
  theme_void() 

# Formato Rosquilla

rosquilla_info  <- rendimiento_19  %>%                               
  filter(!is.na(sit_fin)) %>% 
  group_by(sit_fin) %>%
  dplyr::summarize(count= n()) %>%
  mutate(categoria=as.factor(sit_fin)) %>%
  mutate(fraction=count/sum(count)) %>%
  mutate(percent=round((fraction*100),2)) %>%
  mutate(ymax=cumsum(fraction)) %>%
  mutate(ymin=c(0, head(ymax, n=-1))) %>%
  mutate(labelPosition=(ymax + ymin)/2) %>%
  mutate(label= paste0(round(percent,1), "%"))

ggplot(rosquilla_info, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, 
                           fill=forcats::fct_inorder(categoria))) +
  geom_rect() +
  ggrepel::geom_label_repel(x=3.5, aes(label = paste(percent,"%"), x = 100, y = (ymin + ymax)/2),inherit.aes = F, show.legend = F, size = 4) +
  scale_fill_brewer(palette=1) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  labs(fill=" ", size= 3,
       title="Distribución por tipo de salida")+
  theme_void() +
  theme(legend.position = "right", 
        legend.text=element_text(size=10),
        plot.title = element_text(hjust = 0.15, vjust = 3))

## 6.4 Box-Plots ----------------------------------------------------------------
library(ggthemes)
box <- ggplot(data=rendimiento_19, aes(x=as.factor(nom_reg_rbd_a), y=asistencia, color=factor(nom_reg_rbd_a))) + 
  geom_boxplot(width=0.5)+ 
  xlab("Distribución Puntaje") + 
  ylab(" ") + 
  labs(color = "Region") +
  ggtitle("Boxplot Asistencia a la escuela") +
  theme_minimal()

box

box + geom_jitter(width = .3, alpha = .5)
box + geom_violin(fill = "gray80", size = 1, alpha = .5)

# Combinemos
box + geom_violin(fill = "gray80", size = 1, alpha = .5) +
  geom_jitter(alpha = .25, width = .3) +
  coord_flip() +
  guides(color = FALSE)

########################################################################/
# Actividad en grupos -------------
########################################################################/

# Replique la figura que está en la carpeta del taller.

########################################################################/
# FIN TALLER 2 -------------
########################################################################/

