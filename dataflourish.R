library(readxl)
library(dplyr)
library(tidyverse)
Graduados <- read_xlsx("Datos.xlsx")

#GRAFICO DE LINEAS
evoluciongeneral <- Graduados %>% group_by(YEAR_SEMESTER) %>% count()
write.csv(evoluciongeneral, file = "evoluciongeneral.csv", row.names = F)

#GRAFICO DE LINEAS SEGMNETADO POR UNA DIMENSION (MODALIDAD DE FORMACION)
modalidadformacion <- Graduados %>% group_by(YEAR_SEMESTER, TIPO_NIVEL) %>% 
  count() %>% 
  spread(key = TIPO_NIVEL, value = n) %>% 
  mutate(Total = Pregrado + Postgrado) %>% 
  mutate(Porcentajepregrado = paste(round((Pregrado/Total)*100, 2), "%")) %>% 
  mutate(Porcentajepostgrado = paste(round((Postgrado/Total)*100, 2), "%"))
write.csv(modalidadformacion, file = "modalidadformacion.csv", row.names = F)

#TABLA (MODALIDAD DE FORMACION)
modalidadformaciontabla <- Graduados %>% group_by(YEAR, SEMESTRE, TIPO_NIVEL) %>% 
  count() %>% 
  spread(key = TIPO_NIVEL, value = n) %>% 
  mutate(Total = Pregrado + Postgrado) %>% 
  mutate(Porcentajepregrado = paste(round((Pregrado/Total)*100, 2), "%")) %>% 
  mutate(Porcentajepostgrado = paste(round((Postgrado/Total)*100, 2), "%"))
write.csv(modalidadformaciontabla, file = "modalidadformaciontabla.csv", 
          row.names = F)

#GRAFICO CIRCULAR (MODALIDAD DE FORMACION) CON VARIABLE YEAR_SEMESTER
modalidadformacioncircular <- Graduados %>% 
  group_by(YEAR_SEMESTER, TIPO_NIVEL) %>% 
  mutate(YEAR_SEMESTER = as.factor(YEAR_SEMESTER)) %>% 
  summarise(Cantidad = n()) %>% 
  mutate(Total = aggregate(Cantidad~YEAR_SEMESTER, FUN = sum)[,2]) %>%  
  mutate(Porcentaje = paste(round((Cantidad/Total)*100, 2), "%"))
  write.csv(modalidadformacioncircular, file = "modalidadformacioncircular.csv", 
          row.names = F)

#FILTRADO POR PERIODO 2020-1
modalidadformacioncircularfiltrada <- Graduados %>% 
  group_by(YEAR_SEMESTER, TIPO_NIVEL) %>% 
  summarise(Cantidad = n()) %>% 
  filter(YEAR_SEMESTER == "2020 - 1") %>% 
  mutate(Porcetaje = round((Cantidad/sum(Cantidad))*100, 2))
write.csv(modalidadformacioncircularfiltrada, 
          file = "modalidadformacioncircularfiltrada.csv", 
          row.names = F)

#GRAFICO DE BARRAS (NIVEL DE FORMACION)
nivelformacion <- Graduados %>% 
  group_by(YEAR_SEMESTER, NIVEL) %>% 
  summarise(Cantidad = n()) %>% 
  mutate(Total = aggregate(Cantidad~YEAR_SEMESTER, FUN = sum)[,2]) %>% 
  mutate(Porcentaje = paste(round((Cantidad/Total)*100, 2), "%")) %>% 
  filter(YEAR_SEMESTER == "2020 - 1")
write.csv(nivelformacion, file = "nivelformacion.csv", 
          row.names = FALSE)

#MAPA POR DEPARTAMENTOS
deptos <- Graduados %>% 
  group_by(YEAR_SEMESTER, DEP_NAC) %>% 
  count() %>% 
  filter(YEAR_SEMESTER == "2020 - 1")

#PUNTOS MUNICIPIOS
puntos <- Graduados %>% 
  group_by(CIU_NAC, LAT_CIU_NAC, LON_CIU_NAC) %>% 
  count()

puntos$LON_CIU_NAC <- str_replace_all(puntos$LON_CIU_NAC, 
                                      "^(-?\\d{2})\\.?(\\d+)", "\\1.\\2")

latitud <- as.data.frame(str_replace_all(Graduados$LAT_CIU_NAC, 
                                         "^(-?\\d{1})\\.?(\\d+)", "\\1.\\2"))
