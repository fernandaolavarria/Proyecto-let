#activación de paquetes
library(readxl)
library(tidyr)
library(janitor)
library(dplyr)
library(stringr)
library(readr)

#Se cambia la disposición de datos originales para que queden solamente 7 variables en las columnas (trasponer el set de datos)----

columnas1 = read_excel("datos/datos-sin-procesar/Traffic_Accident_by_Accident_Type_20221018052005NOCELLMERGE.xlsx",.name_repair = "minimal") %>% names()

columnas2 = read_excel("datos/datos-sin-procesar/Traffic_Accident_by_Accident_Type_20221018052005NOCELLMERGE.xlsx",skip = 1,.name_repair = "minimal") %>% names()


columnas2[1] = ""
columnas2[2] = ""
columnas2[3] = ""


nombres_columnas = paste(columnas1, "-", columnas2)

datos_1 = read_excel("datos/datos-sin-procesar/Traffic_Accident_by_Accident_Type_20221018052005NOCELLMERGE.xlsx", skip = 2 , col_names = nombres_columnas)

pivot_1 = select(datos_1,1:3, ends_with("(Case)"))
accidentes = pivot_longer(pivot_1, cols = 4:20, names_to = "Anio", values_to = "Numero_accidentes", names_pattern = "(.+) - .+") %>% clean_names()

pivot_2 = select(datos_1,1:3, matches("Injured"))
heridos = pivot_longer(pivot_2, cols = 4:20, names_to = "Anio", values_to = "Numero_heridos", names_pattern = "(.+) - .+") %>% clean_names()

pivot_3 = select(datos_1,1:3, matches("Deaths"))
muertes = pivot_longer(pivot_3, cols = 4:20, names_to = "Anio", values_to = "Numero_muertes", names_pattern = "(.+) - .+") %>% clean_names()

#consolidación de base de datos final----

join_1 = full_join(accidentes, heridos)

datos_final = full_join(join_1, muertes) 


#cambio de nombre de las variables ---- 

datos_final = datos_final %>% rename(accidente_tipo_1=by_accident_1,
                                     accidente_tipo_2= by_accident_2,
                                     mes = by_month_1)

#cambio de nombre de los valores de las variables (del inglés al español)----

datos_final = datos_final %>% 
  mutate(accidente_tipo_1 = 
           case_when(accidente_tipo_1 == "Car Vs. Man" ~ "auto_humano",
                     accidente_tipo_1 == "Car Vs. Car" ~ "auto_auto",
                     accidente_tipo_1 == "Car Alone"~ "auto_solo",
                     accidente_tipo_1 == "Railway Level Crossing" ~ "cruce_via_ferroviaria"),
         accidente_tipo_2 =
           case_when(accidente_tipo_2 == "In The Process of Crossing" ~ "cruzando",
                     accidente_tipo_2 == "In The Process of Passing The Roadway" ~ "en_calzada",
                     accidente_tipo_2 == "In The Process of Passing The Roadside Zone" ~ "en_borde_camino",
                     accidente_tipo_2 == "In The Process of Passing The Sidewalk" ~ "en_vereda",
                     accidente_tipo_2 == "Other/Unknown" ~ "otros", 
                     accidente_tipo_2 == "Collision"  ~ "colision", 
                     accidente_tipo_2 == "Rear-End Collision" ~ "colision_trasera" , 
                     accidente_tipo_2 == "rear-end collision in progress" ~ "colision_trasera_en_progreso",
                     accidente_tipo_2 == "rear-end collision in parking"  ~ "colision_trasera_estacionando" ,
                     accidente_tipo_2 == "Rollover/Overturn" ~ "vuelco" , 
                     accidente_tipo_2 == "Fall"   ~ "caida" ,
                     accidente_tipo_2 == "Deviation From Road" ~ "desviacion_camino" , 
                     accidente_tipo_2 == "Railway Level Crossing" ~ "cruzar_nivel_ferroviario" ,
                     accidente_tipo_2 == "Breakthrough of Crossing Arm" ~ "pasar_barrera_ferrovia" , 
                     accidente_tipo_2 == "Neglect of Alarm" ~ "negligencia_alarma", 
                     accidente_tipo_2 == "Preceding Progress" ~ "en_progreso_via_tren"),
         mes = 
           case_when(mes == "January" ~ "enero",
                     mes == "February" ~ "febrero",
                     mes == "March" ~ "marzo",
                     mes == "April" ~ "abril",
                     mes == "May" ~ "mayo",
                     mes == "June" ~ "junio",
                     mes == "July" ~ "julio",
                     mes == "August" ~ "agosto",
                     mes == "September" ~ "septiembre",
                     mes == "October" ~ "octubre",
                     mes == "November" ~ "noviembre",
                     mes == "December" ~ "diciembre",
                     TRUE ~ "total")
  )



#cambiar la clase de ciertas variables
is.na(datos_final)
datos_final = datos_final %>% mutate(numero_accidentes = as.numeric(numero_accidentes),
                         numero_heridos = as.numeric(numero_heridos),
                         numero_muertes = as.numeric(numero_muertes),
                         anio = as.numeric(anio))
datos_final[is.na(datos_final)] = 0

#guardar los datos

write_csv(datos_final, "datos/datos-final.csv")
