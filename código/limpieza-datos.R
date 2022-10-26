#activación de paquetes
library(readxl)
library(tidyr)
library(janitor)
library(dplyr)
library(stringr)

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
