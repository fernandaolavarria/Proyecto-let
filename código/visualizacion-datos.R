library(readr)
library(dplyr)
library(ggplot2)

datos_final = read_csv("datos/datos-final.csv")

#Gráfico 1: Accidentes principales----

#Se crea una base con los datos anuales 

datos_anuales = datos_final %>% filter(mes == "total")

#Se procede a crear una base con los datos anuales agrupados por accidente_tipo_1 con los valores de accidentes, heridos y muertes en cada caso.

graf_1 = datos_anuales %>%  group_by(accidente_tipo_1, anio,.add = TRUE) %>% 
  summarize(accidentes = sum(numero_accidentes),
            heridos = sum(numero_heridos),
            muertes  = sum(numero_muertes))

#Se presenta el gráfico

graf_1 %>%
  ggplot(aes(anio, accidentes, color = accidente_tipo_1))+
  geom_line(size = 1) +
  scale_y_continuous(breaks = seq(0, 180000, by =15000),
                     labels = c("0","15k","30k","45k","60k","75k","90k","105k","120k","135k","150k","165k","180k")) + 
  labs(title = "Cantidad de accidentes principales a través de los años en unidades 
       de miles (k)",
       subtitle = "(2005-2021)",
       x = NULL,
       y = "cantidad de accidentes",
       color = "Tipo de accidente",
       tag = "Figura 1") + 
  scale_color_hue(labels = c("auto vs auto",
                             "auto vs humano",
                             "auto solo",
                             "cruce de vía ferroviaria"))



#Gráfico 2: Heridos en accidentes principales----

graf_1 %>% ggplot(aes(anio, heridos, color = accidente_tipo_1)) + 
  geom_line(size = 1) + 
  scale_y_continuous(limits = c(0,300000),
                     breaks = seq(0,300000,by=50000),
                     labels = c("0","50k","100k","150k","200k","250k","300k"))+
  labs(title = "Cantidad de heridos según accidentes principales a través de los años en unidades
       de miles (k)",
       subtitle = "(2005-2021)",
       x = NULL,
       y = "cantidad de heridos",
       color = "Tipo de accidente") + 
  scale_color_hue(labels = c("auto vs auto",
                             "auto vs humano",
                             "auto solo",
                             "cruce de vía ferroviaria"))

#Gráfico 3: Muertos en accidentes principales----

graf_1 %>% ggplot(aes(anio, muertes, color = accidente_tipo_1)) + 
  geom_line(size = 1) + 
  scale_y_continuous(breaks = seq(0,3000,by=500))+
  labs(title = "Cantidad de muertes según accidentes principales a través de los años",
       subtitle = "(2005-2021)",
       x = NULL,
       y = "cantidad de muertes",
       color = "Tipo de accidente") + 
  scale_color_hue(labels = c("auto vs auto",
                             "auto vs humano",
                             "auto solo",
                             "cruce de vía ferroviaria"))
