library(readxl)
library(dplyr)
library(tidyverse)
library(magrittr)
library(rjson)
library(jsonlite)
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
data.json <- fromJSON(file="colombiageo.json")
x <- length(data.json$features)
for (i in 1:x) {
  pos <- str_detect(deptos$DEP_NAC, 
                    paste0("^", data.json$features[[i]]$properties$NOMBRE_DPT, "$"))
  data.json$features[[i]]$properties$n = as.character(deptos[pos, "n"])
}
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
Graduados$DEP_NAC <- str_replace_all(Graduados$DEP_NAC, "Bogota D.C.", "Bogota D.C")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Bogot(á|a),? d.c.", "Bogota D.C.")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Calima", "Darien")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Guadalajara de buga", "Buga")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "San andres de tumaco", "Tumaco")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Tolu viejo", "Toluviejo")
#Municipios periodo 2020-1
mps <- Graduados %>% 
  group_by(YEAR_SEMESTER, DEP_NAC, CIU_NAC) %>% 
  count() %>% 
  filter(YEAR_SEMESTER == "2020 - 1") %>% 
  mutate(CIU_NAC = str_to_upper(chartr("áéíóúü", "aeiouu", CIU_NAC), locale = "es"), 
         DEP_NAC = str_to_upper(DEP_NAC, locale = "es"))

#Importar, agregar nueva propiedad y exportar el archivo Json  
mpios.json <- fromJSON(file="mpio.json")
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
write_json(mpios.json, "municipios.json")

#MAPA DE PUNTOS 3D
municipios3d <- Graduados %>% group_by(YEAR, LON_CIU_NAC, LAT_CIU_NAC,
                                 DEP_NAC, CIU_NAC) %>% count()

write.csv(municipios3d, "municipios3d.csv", row.names = FALSE)

#Globo de conexiones
dpto <- c() 
mpio <- c()
coddepto <- c()
codmpio <- c()
for (i in 1:length(mpios.json$features)) {
  id[i] <- i
  dpto[i] <- mpios.json$features[[i]]$properties$NOMBRE_DPT
  mpio[i] <- mpios.json$features[[i]]$properties$NOMBRE_MPI
  coddepto[i] <- mpios.json$features[[i]]$properties$DPTO
  codmpio[i] <- mpios.json$features[[i]]$properties$MPIO
}
codigos <- data.frame(dpto, coddepto, mpio, codmpio)
municipios <- Graduados %>% 
  group_by(DEP_NAC, CIU_NAC, LAT_CIU_NAC, LON_CIU_NAC) %>% 
  count() %>% 
  mutate(CIU_NAC = str_to_upper(chartr("áéíóúü", "aeiouu", CIU_NAC), locale = "es"), 
         DEP_NAC = str_to_upper(DEP_NAC, locale = "es"))
locaciones <- left_join(codigos, municipios, by = c("dpto" = "DEP_NAC", "mpio" = "CIU_NAC"))
locaciones %<>% mutate(n = replace_na(n, 0))
s_amazonas <- c(NA, NA, "SEDE AMAZONIA", "s_amazonia", -4.189787, -69.938876)
s_bogota <- c(NA, NA, "SEDE BOGOTA", "s_bogota", 4.636445, -74.082885)
s_caribe <- c(NA, NA, "SEDE CARIBE", "s_caribe", 12.536244, -81.707912)
s_medellin <- c(NA, NA, "SEDE MEDELLIN", "s_medellin", 6.261541, -75.577196)
s_manizales <- c(NA, NA, "SEDE MANIZALES", "s_manizales", 5.056195, -75.490887)
s_palmira <- c(NA, NA, "SEDE PALMIRA", "s_palmira", 3.512328, -76.307490)
locaciones <- rbind(locaciones, s_amazonas, s_bogota, s_caribe, s_medellin,
                    s_manizales, s_palmira)


#VALORES: categoria, año, origen (coincidir con codigo), destino(coincidir 
#con codigo), vallor(conteo de estudiantes)


valores <- Graduados %>% group_by(YEAR, TIPO_NIVEL, DEP_NAC, CIU_NAC, 
                                  SEDE_NOMBRE_MAT) %>% 
  mutate(CIU_NAC = str_to_upper(chartr("áéíóúü", "aeiouu", CIU_NAC), locale = "es"), 
         DEP_NAC = str_to_upper(DEP_NAC, locale = "es"),
         SEDE_NOMBRE_MAT = str_to_upper(chartr("áéíóúü", "aeiouu", SEDE_NOMBRE_MAT), 
                                        locale = "es")) %>% 
  count()


valores <- left_join(valores, codigos, by = c("DEP_NAC" = "dpto", 
                                                    "CIU_NAC" = "mpio"))
