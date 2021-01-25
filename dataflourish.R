library(readxl)
library(dplyr)
library(tidyverse)
Graduados <- read_xlsx("Datos.xlsx")

#GRAFICO DE LINEAS
evoluciongeneral <- Graduados %>% group_by(YEAR_SEMESTER) %>% count()
write.csv(evoluciongeneral, file = "evoluciongeneral.csv", row.names = F)

#GRAFICO DE LINEAS SEGMNETADO POR UNA DIMENSION (MODALIDAD DE FORMACION)
modalidadfromacion <- Graduados %>% group_by(YEAR_SEMESTER, TIPO_NIVEL) %>% 
  count() %>% 
  spread(key = TIPO_NIVEL, value = n) %>% 
  mutate(Porcentajepregrado = (Pregrado/(Pregrado + Postgrado)*100)) %>% 
  mutate(Porcentajepostgrado = (Postgrado/(Pregrado + Postgrado)*100))
write.csv(modalidadfromacion, file = "modalidadfromacion.csv", row.names = F)

#GRAFICO DE LINEAS SEGMNETADO POR UNA DIMENSION (NIVEL DE FORMACION)
nivelfromacion <- Graduados %>% group_by(YEAR_SEMESTER, NIVEL) %>% 
  count() %>% spread(key = NIVEL, value = n) %>% 
  mutate(Porcentajepregrado = (Pregrado/(Pregrado + Postgrado)*100)) %>% 
  mutate(Porcentajepostgrado = (Postgrado/(Pregrado + Postgrado)*100)) %>% 
  mutate(Porcentajepregrado = (Pregrado/(Pregrado + Postgrado)*100)) %>% 
  mutate(Porcentajepostgrado = (Postgrado/(Pregrado + Postgrado)*100)) %>% 
write.csv(nivelfromacion, file = "nivelfromacion.csv", row.names = F)
