library(readxl)
library(dplyr)
library(tidyverse)
library(magrittr)
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

#Departamentos periodo 2020-1
deptos <- Graduados %>% 
  group_by(YEAR_SEMESTER, DEP_NAC) %>% 
  count() %>% 
  filter(YEAR_SEMESTER == "2020 - 1") %>% 
  mutate(DEP_NAC = str_to_upper(DEP_NAC, locale = "es"))

#Importar, agregar nueva propiedad y exportar el archivo Json  
library(rjson)
data.json <- fromJSON(file="colombiageo.json")
x <- length(data.json$features)
for (i in 1:x) {
  pos <- str_detect(deptos$DEP_NAC, 
                    paste0("^", data.json$features[[i]]$properties$NOMBRE_DPT, "$"))
  data.json$features[[i]]$properties$n = as.character(deptos[pos, "n"])
}
library(jsonlite)
write_json(data.json, "depts.json")


#PUNTOS MUNICIPIOS
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Bogota(|,) d.c.", 
                                     "Bogota D.C.")
Graduados$CIU_NAC <- chartr('áéíóúü','aeiouu', Graduados$CIU_NAC)

puntos <- Graduados %>% 
  group_by(CIU_NAC, LAT_CIU_NAC, LON_CIU_NAC) %>% 
  count()
puntos <- puntos[!duplicated(puntos$CIU_NAC), ]
puntos <- puntos %>% select(-n)

Graduados <- Graduados %>% select(-c("LAT_CIU_NAC", "LON_CIU_NAC"))

#UNION NETRE PUNTOS Y GRADUADOS
Graduados <- left_join(Graduados, puntos, by = "CIU_NAC")

municipios <- Graduados %>% 
  group_by(CIU_NAC, LON_CIU_NAC, LAT_CIU_NAC) %>% 
  count()

write.csv(municipios, file = "municipios.csv", 
          row.names = FALSE)

#EXTRAER CAPITALES DE LOS DEPARTAMENTOS, DE LA BASE MUNICIPIOS
capitales <-c("Leticia", "Medellin", "Arauca", "Barranquilla", 
              "Cartagena de indias", "Tunja", "Manizales", "Florencia", "Yopal", 
              "Popayan", "Valledupar", "Quibdo", "Monteria", "Bogota D.C.", 
              "Inirida","San jose del guaviare","Neiva","Riohacha","Santa marta",
              "Villavicencio", "Pasto", "Cucuta", "Mocoa", "Armenia", "Pereira", 
              "San andres", "Bucaramanga", "Sincelejo", "Cali", "Mitu", 
              "Puerto carreno", "Ibague")
Capitales <- municipios %>% filter(CIU_NAC %in% capitales)
write.csv(Capitales, file = "capitales.csv", 
          row.names = FALSE)

#MAPA POR MUNICIPIOS

#Municipios periodo 2020-1
mps <- Graduados %>% 
  group_by(YEAR_SEMESTER, DEP_NAC, CIU_NAC) %>% 
  count() %>% 
  filter(YEAR_SEMESTER == "2020 - 1") %>% 
  mutate(CIU_NAC = str_to_upper(CIU_NAC, locale = "es"), 
         DEP_NAC = str_to_upper(chartr('áéíóú','aeiou', DEP_NAC), locale = "es"))

#Importar, agregar nueva propiedad y exportar el archivo Json  
library(rjson)
mpios.json <- fromJSON(file="mpio.json")
x <- length(mpios.json$features)
for (i in 1:x) {
  pos <- str_detect(mps$CIU_NAC, 
                    paste0("^", mpios.json$features[[i]]$properties$NOMBRE_MPI, "$"))
  mpios.json$features[[i]]$properties$n = as.character(mps[pos, "n"])
}



library(jsonlite)
write_json(mpios.json, "municipios.json")


id <- c() 
dpto <- c() 
mpio <- c()
for (i in 1:length(mpios.json$features)) {
  id[i] <- i
  dpto[i] <- mpios.json$features[[i]]$properties$NOMBRE_DPT
  mpio[i] <- mpios.json$features[[i]]$properties$NOMBRE_MPI
}
basejson <- data.frame(id, dpto, mpio)

union <- left_join(basejson, mps, by = c("dpto" = "DEP_NAC", "mpio" = "CIU_NAC"))
union %<>% mutate(n = replace_na(n, 0))



for (i in 1:length(union$id)) {
  mpios.json$features[[i]]$properties$n = union[i, "n"]
}

union[pos]
pos_dpt <- str_detect(union$dpto, mps$DEP_NAC[80])
pos_ciu <- str_detect(union$mpio, mps$CIU_NAC[80])
mpios.json$features[[i]]$properties$n = as.character(mps[pos, "n"])