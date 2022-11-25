library(readr)
library(dplyr)
library(ggplot2)

datos_final = read_csv("datos/datos-final.csv")
datos_anuales = datos_final %>% filter(mes == "total")
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
  scale_color_hue(labels = c("auto_auto" = "auto vs auto",
                                "auto_humano" = "auto vs humano",
                                "auto_solo" = "auto solo",
                                "cruce_via_ferroviaria" = "cruve vía ferroviaria")) +
  scale_y_continuous(breaks = seq(0, 180000, by =15000),
                     labels = c("0","15k","30k","45k","60k","75k","90k","105k","120k","135k","150k","165k","180k")) + 
  labs(title = "Cantidad de accidentes principales a través de los años en unidades 
       de miles (k)",
       subtitle = "(2005-2021)",
       x = NULL,
       y = "cantidad de accidentes",
       color = "Tipo de accidente",
       tag = "Figura 1") 

#Gráfico 1.5: explicación de por qué sacar variable "tren"----

graf_1 %>% filter(accidente_tipo_1 == "cruce_via_ferroviaria") %>% 
  ggplot(aes(anio, accidentes)) + 
  geom_line(color = "red") + 
  scale_y_continuous(limits=c(0,20))+
  labs(x = NULL,
       title = "Accidentes a través de los años por accidente tipo 'En la vía ferroviaria'")



#Gráfico 2: Heridos en accidentes principales----

graf_1 %>% filter(accidente_tipo_1 %in% c("auto_auto","auto_humano","auto_solo")) %>% 
  ggplot(aes(anio, heridos, color = accidente_tipo_1)) + 
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

graf_1 %>% filter(accidente_tipo_1 %in% c("auto_auto","auto_humano","auto_solo")) %>% 
  ggplot(aes(anio, muertes, color = accidente_tipo_1)) + 
  geom_line(size = 1) + 
  scale_y_continuous(limits = c(0,2700),
                     breaks = seq(0,3000,by=500))+
  labs(title = "Cantidad de muertes según accidentes principales a través de los años",
       subtitle = "(2005-2021)",
       x = NULL,
       y = "cantidad de muertes",
       color = "Tipo de accidente") + 
  scale_color_hue(labels = c("auto vs auto",
                             "auto vs humano",
                             "auto solo",
                             "cruce de vía ferroviaria"))
#Gráfico 4:Accidentes AUTO VS AUTO----
library(gghighlight)
library(scales)
auto_auto = c("colision" = "#E69F00",
              "colision_trasera" = "#56B4E9",
              "colision_trasera_en_progreso" = "#3C8200",
              "colision trasera_estacionando" = "009E73",
              "otros" = "black")
#2 variables descartadas, ya que han tenido un deceso total antes del 2021
datos_anuales %>% filter(accidente_tipo_1 == "auto_auto") %>% 
  ggplot(aes(anio,numero_accidentes, color = accidente_tipo_2))+
  geom_line(size = 1) +
  gghighlight(accidente_tipo_2 %in% c("colision", "otros", "colision_trasera"), use_direct_label = FALSE) + 
  labs(x = NULL,
       y = "accidentes",
       title = "Accidentes a través de los años para accidente tipo 'Auto vs Auto'",
       subtitle = "(2005-2021)",
       color = "Subcategorías")+
  scale_color_manual(values = auto_auto,
                     labels = c("colision" = "colisión",
                                "colision_trasera" = "colisión trasera",
                                "colision_trasera_en_progreso" = "colisión trasera en progreso",
                                "colision trasera_estacionando" = "colisión trasera por estacionar"),
                     limits=c("colision","otros","colision_trasera")) + 
  scale_y_continuous(labels = c("0","25k","50k","75k","100k"))
  
#Gráfico 5: Muertos y Heridos Accidente AUTO VS AUTO----
color_graf5 = c("colision" = "#E69F00",
                "colision_trasera" = "#56B4E9",
                "otros" = "black")
#MUERTES
#colision va en bajada significativa, pero los otros ahi nomas
datos_anuales %>% filter(accidente_tipo_1 == "auto_auto",
                         accidente_tipo_2 %in% c("colision", "otros", "colision_trasera")) %>% 
  ggplot(aes(anio,numero_muertes, color = accidente_tipo_2))+
  geom_line()+
  scale_color_manual(values = color_graf5,
                     labels = c("colision" = "colisión",
                                "colision_trasera" = "colisión trasera",
                                "otros" = "otros"))+
  labs(x = NULL,
       y = "muertes",
       color = "Subcategorías ",
       title = "Muertes a través de los años para accidente tipo 'Auto vs Auto",
       subtitle = "(2005-2021)")

#HERIDOS
#misma cantidad de heridos por accidentes
datos_anuales %>% filter(accidente_tipo_1 == "auto_auto",
                         accidente_tipo_2 %in% c("colision", "otros", "colision_trasera")) %>% 
  ggplot(aes(anio,numero_heridos, color = accidente_tipo_2))+
  geom_line()+
  scale_color_manual(values = color_graf5,
                     labels = c("colision" = "colisión",
                                "colision_trasera" = "colisión trasera",
                                "otros" = "otros"))+
  labs(x = NULL,
       y = "heridos",
       color = "Subcategorías ",
       title = "Heridos a través de los años para accidente tipo 'Auto vs Auto'",
       subtitle = "(2005-2021)")+
  scale_y_continuous(labels=c("0","50k","100k","150k","200k"))

#Gráfico 6:Numero accidentes de AUTO VS HUMANO----
auto_humano =  c("cruzando" = "#DD708E",
                 "en_borde_camino" = "#E06046",
                 "en_calzada" = "#33608C",
                 "en_vereda" = "#7ABE6F",
                 "otros" = "black")
#unica baja significativa es "cruzando"
datos_anuales %>% filter(accidente_tipo_1 == "auto_humano") %>% 
  ggplot(aes(anio, numero_accidentes, color = accidente_tipo_2)) +
  geom_line(size = 1) + 
  scale_y_continuous(limits = c(0,30000))+
  labs(x = NULL,
       y = "accidentes",
       title = "Accidentes a través de los años para accidentes tipo 'Auto vs Humano'",
       color = "Subcategorías",
       subtitle = "(2005-2021)")+
  scale_color_manual(values = auto_humano,
                     labels = c("cruzando" = "cruzando",
                                "en_borde_camino" = "en el borde del camino",
                                "en_calzada" = "en la calzada",
                                "en_vereda" = "en la vereda",
                                "otros" = "otros"))+
  scale_y_continuous(breaks=seq(0,30000,5000),
                     labels = c("0","5k","10k","15k","20k","25k","30k"))
#Gráfico 7: Muertos y Heridos AUTO VS HUMANO----

#HERIDOS
#misma tendencia que en cantidad accidentes
datos_anuales %>% filter(accidente_tipo_1 == "auto_humano") %>% 
  ggplot(aes(anio,numero_heridos,color = accidente_tipo_2))+
  geom_line(size  = 1)+
  scale_color_hue(labels= c(
    "en_borde_camino" = "en el borde del camino",
    "en_calzada" = "en la calzada",
    "en_vereda" = "en la vereda",
    "otros" = "otros"
  )) + 
  labs(x = NULL,
       y = "heridos",
       color = "subcategorías",
       title = "Heridos a través de los años para accidente tipo 'Auto vs Humano'",
       subtitle = "(2005-2021)") +
  scale_y_continuous(breaks=seq(0,25000,5000),
                     labels=c("0","5k","10k","15k","20k","25k"))

#MUERTOS
#solo deceso significativo en "cruzando"
datos_anuales %>% filter(accidente_tipo_1 == "auto_humano") %>% 
  ggplot(aes(anio,numero_muertes,color = accidente_tipo_2))+
  geom_line(size = 1)+
  scale_color_hue(labels= c(
    "en_borde_camino" = "en el borde del camino",
    "en_calzada" = "en la calzada",
    "en_vereda" = "en la vereda",
    "otros" = "otros"
  )) + 
  labs(x = NULL,
       y = "muertos",
       color = "Subcategorías",
       title = "Muertos a través de los años para accidente tipo 'Auto vs Humano'",
       subtitle = "(2005-2021)") +
  scale_y_continuous(breaks=seq(0,2000,250))


#Gráfico 8: Accidentes AUTO SOLO----
auto_solo = c("caida" = "#E0CB48",
              "colision" = "#2A883E",
              "desviacion_camino" = "#4F98C4",
              "otros" = "black",
              "vuelco" = "#FD8E3F")
datos_anuales %>% filter(accidente_tipo_1 == "auto_solo") %>% 
  ggplot(aes(anio,numero_accidentes, color = accidente_tipo_2)) + 
  geom_line(size = 1) +
  scale_color_manual(values = auto_solo,
                     labels= c(
    "caida" = "caída",
    "colision" = "colisión",
    "desviacion_camino" = "desviación del camino"
  )) + 
  labs(x = NULL,
       y = "accidentes",
       color = "Subcategorías",
       title = "Accidentes a través de los años para accidente tipo 'Auto Solo'",
       subtitle = "(2005-2021)") 
#Gráfico 9: Muerto y Heridos AUTO SOLO----

#HERIDOS
datos_anuales %>% filter(accidente_tipo_1=="auto_solo") %>% 
  ggplot(aes(anio, numero_heridos, color = accidente_tipo_2)) + 
  geom_line(size = 1)+
  scale_color_manual(values = auto_solo,
                     labels= c(
    "caida" = "caída",
    "colision" = "colisión",
    "desviacion_camino" = "desviación del camino",
    "otros" = "otros",
    "vuelvo" = "vuelco"
  )) + 
  labs(x = NULL,
       y = "heridos",
       color = "Subcategorías",
       title = "Heridos a través de los años para accidente tipo 'Auto Solo'",
       subtitle = "(2005-2021)") + 
  scale_y_continuous(breaks=seq(0,7000,by=1000))

  

#MUERTOS
datos_anuales %>% filter(accidente_tipo_1=="auto_solo") %>% 
  ggplot(aes(anio, numero_muertes, color = accidente_tipo_2)) + 
  geom_line(size  = 1)+
  scale_color_manual(values = auto_solo,
                     labels= c(
    "caida" = "caída",
    "colision" = "colisión",
    "desviacion_camino" = "desviación del camino",
    "otros" = "otros",
    "vuelvo" = "vuelco"
  )) + 
  labs(x = NULL,
       y = "muertos",
       color = "Subcategorías",
       title = "Muertos a través de los años para accidente tipo 'Auto Solo'",
       subtitle = "(2005-2021)")+
  scale_y_continuous(breaks=seq(0,801,200))
#RECORDATORIO----

graf_1 %>%
  ggplot(aes(anio, accidentes, color = accidente_tipo_1))+
  geom_line(size = 1) +
  scale_color_manual(values = color,labels = c("auto_auto" = "auto vs auto",
                                               "auto_humano" = "auto vs humano",
                                               "auto_solo" = "auto solo",
                                               "cruce_via_ferroviaria" = "cruve vía ferroviaria")) +
  scale_y_continuous(breaks = seq(0, 180000, by =15000),
                     labels = c("0","15k","30k","45k","60k","75k","90k","105k","120k","135k","150k","165k","180k")) + 
  labs(title = "Cantidad de accidentes principales a través de los años en unidades 
       de miles (k)",
       subtitle = "(2005-2021)",
       x = NULL,
       y = "cantidad de accidentes",
       color = "Tipo de accidente",
       tag = "Figura 1") 
names(datos_final)

datos_anuales %>% filter(accidente_tipo_1 == "auto_auto") %>% 
  ggplot(aes(anio,numero_accidentes, color = accidente_tipo_2))+
  geom_line(size = 1) +
  scale_color_manual(values = auto_auto, 
                     labels = c("colision" = "colisión",
                                "colision_trasera" = "colisión trasera",
                                "colision_trasera_en_progreso" = "colisión trasera en progreso",
                                "colision trasera_estacionando" = "Colisión trasera por estacionar",
                                "otros" = "otros"))+
  gghighlight(accidente_tipo_2 %in% c("colision", "otros", "colision_trasera")) + 
  labs(x = NULL,
       y = "número de accidentes",
       title = "cantidad de accidentes")

