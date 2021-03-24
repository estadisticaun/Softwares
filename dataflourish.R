library(readxl)
library(dplyr)
library(tidyverse)
library(magrittr)
library(rjson)
library(jsonlite)
Graduados <- read_xlsx("Datos.xlsx")
puntos <- Graduados %>% 
  group_by(CIU_NAC, LAT_CIU_NAC, LON_CIU_NAC) %>% 
  count()
puntos <- puntos[!duplicated(puntos$CIU_NAC), ]
puntos <- puntos %>% select(-n)
Graduados <- Graduados %>% select(-c("LAT_CIU_NAC", "LON_CIU_NAC"))
Graduados <- left_join(Graduados, puntos, by = "CIU_NAC")
Graduados$DEP_NAC <- str_replace_all(Graduados$DEP_NAC, "Bogota D.C.", 
                                     "Bogota D.C")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Bogot(á|a),? d.c.", 
                                     "Bogota D.C.")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Calima", "Darien")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Guadalajara de buga", 
                                     "Buga")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "San andres de tumaco", 
                                     "Tumaco")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Tolu viejo", 
                                     "Toluviejo")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Don matias", 
                                     "Donmatias")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Itsmina", 
                                     "Istmina")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Ubate", 
                                     "Villa de san diego de ubate")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "San vicente chucuri", 
                                     "San vicente de chucuri")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Entrerios", 
                                     "Entrerrios")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, 
                                     "Cartagena de indias de indias", 
                                     "Cartagena de indias")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, 
                                     "San vicente( ferrer)?", 
                                     "San vicente ferrer")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, 
                                     "Santafe de antioquia", 
                                     "Santa fe de antioquia")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, 
                                     "San andres sotavento", 
                                     "San andres de sotavento")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, 
                                     "San juan de rio(\\s)?seco", 
                                     "San juan de rioseco")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, 
                                     "San pedro de cartago?", 
                                     "San pedro de cartago")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, 
                                     "Valle del gaumez", 
                                     "Valle del guamuez")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, 
                                     "San vicente ferrer de chucuri", 
                                     "San vicente de chucuri")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, 
                                     "San vicente ferrer del caguan", 
                                     "San vicente del caguan")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Cartagena(\\sde\\sindias)?", 
                                     "Cartagena de indias")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "San luis de since", 
                                     "Since")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Armero(\\sguayabal)?", 
                                     "Armero")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "San sebastian de mariquita", 
                                     "Mariquita")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Cartagena de indias del chaira", 
                                     "Cartagena del chaira")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Güican de la sierra", 
                                     "Guican")

Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Ariguani", 
                                     "Ariguaini")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Cerro de san antonio", 
                                     "Cerro san antonio")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Lopez de micay", 
                                     "Lopez")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Santacruz", 
                                     "Santa cruz")
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "El canton del san pablo", 
                                     "Canton del san pablo")
#falta corregir esta
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Manaure(\\sbalcon\\sdel\\cesar)?", 
                                     "Manaure")
Graduados[Graduados$DEP_NAC == "Antioquia" & 
            Graduados$CIU_NAC == "San pedro de los milagros", 6] <- "San pedro"
Graduados[Graduados$DEP_NAC == "Tolima" & 
            Graduados$CIU_NAC == "Colon", 6] <- "Santa isabel"
Graduados[Graduados$DEP_NAC == "Antioquia" & 
            Graduados$CIU_NAC == "San andres", 6] <- "San andres de cuerquia"
Graduados[Graduados$DEP_NAC == "Antioquia" & 
            Graduados$CIU_NAC == "San andres de cuerquia", 
          32] <- "-75.672773119699997"
Graduados[Graduados$DEP_NAC == "Antioquia" & 
            Graduados$CIU_NAC == "San andres de cuerquia", 
          31] <- "6.9236096390800004"
Graduados[Graduados$DEP_NAC == "Antioquia" & 
            Graduados$CIU_NAC == "Samana", 5] <- "Caldas"
Graduados[Graduados$DEP_NAC == "Bolivar" & 
            Graduados$CIU_NAC == "San agustin", 5] <- "Huila"
Graduados[Graduados$DEP_NAC == "Cesar" & 
            Graduados$CIU_NAC == "Libano", 5] <- "Tolima"
Graduados[Graduados$DEP_NAC == "Antioquia" & 
            Graduados$CIU_NAC == "Puerto lopez", 5] <- "Meta"
Graduados[Graduados$DEP_NAC == "Cesar" & 
            Graduados$CIU_NAC == "San martin", 
          32] <- "-73.510987"
Graduados[Graduados$DEP_NAC == "Cesar" & 
            Graduados$CIU_NAC == "San martin", 
          31] <- "7.999580"
Graduados[Graduados$DEP_NAC == "Antioquia" & 
            Graduados$CIU_NAC == "San martin", 5] <- "Meta"
Graduados[Graduados$CIU_NAC == "Entrerrios", 5] <- "Antioquia"
Graduados[Graduados$PROGRAMA == "Ingenieria Forestal" & 
                     Graduados$FACULTAD == "Minas", 23] <- "Ciencias agrarias"

#GRAFICO DE LINEAS
evoluciongeneral <- Graduados %>% group_by(YEAR_SEMESTER) %>% count()
write.csv(evoluciongeneral, file = "evoluciongeneral.csv", row.names = F)

#GRAFICO DE LINEAS SEGMNETADO POR UNA DIMENSION (MODALIDAD DE FORMACION)
modalidadformacion <- Graduados %>% group_by(YEAR_SEMESTER, TIPO_NIVEL) %>% 
  count() %>% 
  pivot_wider(names_from = TIPO_NIVEL, values_from = n) %>% 
  mutate(Total = Pregrado + Postgrado) %>% 
  mutate(Porcentajepregrado = paste(round((Pregrado/Total)*100, 2), "%")) %>% 
  mutate(Porcentajepostgrado = paste(round((Postgrado/Total)*100, 2), "%"))
write.csv(modalidadformacion, file = "modalidadformacion.csv", row.names = F)

#TABLA (MODALIDAD DE FORMACION)
modalidadformaciontabla <- Graduados %>% group_by(YEAR, SEMESTRE, TIPO_NIVEL) %>% 
  count() %>% 
  pivot_wider(names_from = TIPO_NIVEL, values_from = n) %>% 
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

#GLOBO DE CONEXIONES
dpto <- c() 
mpio <- c()
codigos <- c()
for (i in 1:length(mpios.json$features)) {
  dpto[i] <- mpios.json$features[[i]]$properties$NOMBRE_DPT
  mpio[i] <- mpios.json$features[[i]]$properties$NOMBRE_MPI
  codigos[i] <- paste(mpios.json$features[[i]]$properties$DPTO,
                      mpios.json$features[[i]]$properties$MPIO,
                      sep = "-")
}
codigos <- data.frame(dpto, mpio, codigos)
municipios <- Graduados %>% 
  group_by(DEP_NAC, CIU_NAC, LAT_CIU_NAC, LON_CIU_NAC) %>% 
  count() %>% 
  mutate(CIU_NAC = str_to_upper(chartr("áéíóúü", "aeiouu", CIU_NAC), 
                                locale = "es"), 
         DEP_NAC = str_to_upper(DEP_NAC, locale = "es"))
locaciones <- left_join(codigos, municipios, by = c("dpto" = "DEP_NAC", 
                                                    "mpio" = "CIU_NAC"))
locaciones %<>% select(-n) 
s_amazonas <- c("AMAZONAS", "SEDE AMAZONIA", "s_amazonia", -4.189787, 
                -69.938876)
s_bogota <- c("BOGOTA D.C", "SEDE BOGOTA", "s_bogota", 4.636445, 
              -74.082885)
s_caribe <- c("ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CANTALINA", 
              "SEDE CARIBE", "s_caribe", 12.536244, -81.707912)
s_medellin <- c("ANTIOQUIA", "SEDE MEDELLIN", "s_medellin", 6.261541, 
                -75.577196)
s_manizales <- c("CALDAS", "SEDE MANIZALES", "s_manizales", 5.056195, 
                 -75.490887)
s_palmira <- c("VALLE DEL CAUCA", "SEDE PALMIRA", "s_palmira", 3.512328, 
               -76.307490)
locaciones <- rbind(locaciones, s_amazonas, s_bogota, s_caribe, s_medellin,
                    s_manizales, s_palmira)
write.csv(locaciones, "locaciones.csv", row.names = F)

#VALORES: categoria, año, origen (coincidir con codigo), destino(coincidir 
#con codigo), vallor(conteo de estudiantes)
valores <- Graduados %>% group_by(YEAR, TIPO_NIVEL, DEP_NAC, CIU_NAC, 
                                  SEDE_NOMBRE_MAT) %>% 
  mutate(CIU_NAC = str_to_upper(chartr("áéíóúü", "aeiouu", CIU_NAC), 
                                locale = "es"), 
         DEP_NAC = str_to_upper(DEP_NAC, locale = "es"),
         SEDE_NOMBRE_MAT = paste("SEDE", str_to_upper(chartr("áéíóúü", "aeiouu",
                                                             SEDE_NOMBRE_MAT), 
                                        locale = "es"),
                                 " ")) %>%count()
valores <- left_join(valores, codigos, by = c("DEP_NAC" = "dpto", 
                                              "CIU_NAC" = "mpio"))
valores$codsede <- paste("s", str_to_lower(substr(valores$SEDE_NOMBRE_MAT, 6,
                                                  nchar(valores$SEDE_NOMBRE_MAT)))
                         , sep = "_")
write.csv(valores, "valores.csv", row.names = F)

#JERARQUIA
jerarquia <- Graduados %>% 
  group_by(SEDE_NOMBRE_MAT, FACULTAD, PROGRAMA, NIVEL) %>% 
  mutate(SEDE_NOMBRE_MAT = chartr("áéíóúü", "aeiouu", SEDE_NOMBRE_MAT),
         FACULTAD = chartr("áéíóúü", "aeiouu", FACULTAD),
         NIVEL = chartr("áéíóúü", "aeiouu", NIVEL),
         PROGRAMA = chartr("áéíóúü", "aeiouu", PROGRAMA)) %>% 
  summarise(n = n()) %>% 
  arrange(SEDE_NOMBRE_MAT, FACULTAD, NIVEL, desc(n)) 
 
write.csv(jerarquia, "jerarquia.csv", row.names = F)

#DIAGRAMA DE SANKEY
sankey <- Graduados %>% 
  group_by(SEDE_NOMBRE_ADM, SEDE_NOMBRE_MAT) %>% 
  count() %>% 
  filter(SEDE_NOMBRE_ADM != SEDE_NOMBRE_MAT)
write.csv(sankey, file = "sankey.csv", row.names = F)

#CARRERA DE BARRAS
car_barras <- Graduados %>% 
  group_by(YEAR_SEMESTER, AREAC_SNIES, NIVEL) %>% 
  count() %>% 
  pivot_wider(names_from = YEAR_SEMESTER, values_from = n) %>% 
  mutate_all(~replace(., is.na(.), 0))
write.csv(car_barras, file = "car_barras.csv", row.names = F)

#CARRERA DE LINEAS
pregrado <- Graduados %>% 
  group_by(YEAR_SEMESTER, DEP_NAC, NIVEL) %>% 
  filter(NIVEL == "Pregrado", DEP_NAC != "NA") %>% 
  summarise(n = n()) 

car_lineas <- Graduados %>% 
  group_by(YEAR_SEMESTER, DEP_NAC, NIVEL) %>% 
  count() %>% 
  filter(DEP_NAC != "NA") %>% 
  pivot_wider(names_from = YEAR_SEMESTER, values_from = n) %>%
  mutate_all(~replace(., is.na(.), 0))
  
write.csv(car_lineas, file = "car_lineas.csv", row.names = F)

#GRAFICO DE RADAR
radar <- Graduados %>% 
  group_by(EDAD_MOD, NIVEL, SEDE_NOMBRE_MAT) %>% 
  filter(EDAD_MOD > 15, EDAD_MOD <70) %>% 
  count() %>% 
  mutate(CAT_EDAD = case_when(
    EDAD_MOD <= 23 ~ "23 años o menos",
    EDAD_MOD >= 24 & EDAD_MOD <= 25 ~ "24 a 25 años",
    EDAD_MOD >= 26 & EDAD_MOD <= 30 ~ "26 a 30 años",
    EDAD_MOD >= 31 & EDAD_MOD <= 35 ~ "31 a 35 años",
    EDAD_MOD >= 36 ~ "36 años o mas"
  )) %>% 
  pivot_wider(names_from = NIVEL, values_from = n) %>% 
  mutate_all(~replace(., is.na(.), 0))

write.csv(radar, file = "radar.csv", row.names = F)

estrato <- Graduados %>% 
  group_by(ESTRATO_ORIG, NIVEL, SEDE_NOMBRE_MAT) %>% 
  filter(ESTRATO_ORIG != "NA") %>% 
  count() %>% 
  pivot_wider(names_from = NIVEL, values_from = n) %>% 
  mutate_all(~replace(., is.na(.), 0))
write.csv(estrato, file = "estrato.csv", row.names = F)

#GRAFICO DE PENDIENTES
Facultades <- Graduados %>% 
  group_by(SEDE_NOMBRE_MAT, PROGRAMA, FACULTAD) %>% 
  summarise(n = n())
#se retiran los duplicados existentes
Programas <- Facultades %>% 
  distinct(SEDE_NOMBRE_MAT, PROGRAMA, .keep_all = TRUE) %>% 
  select(-n)
#Elimar facultad de la base general para luego realizar la union
Graduados <- Graduados %>% select(-FACULTAD)
Graduados <- left_join(Graduados, y, by = c("SEDE_NOMBRE_MAT", "PROGRAMA"))

#Pendiente sede medellin
pendiente_medellin <- Graduados %>% 
  group_by(YEAR, PROGRAMA, FACULTAD, TIPO_NIVEL, SEDE_NOMBRE_MAT) %>% 
  filter(SEDE_NOMBRE_MAT == "Medellín", NIVEL == "Pregrado", 
         PROGRAMA != "Minas", PROGRAMA != "Ciencias", 
         PROGRAMA != "Ciencias Humanas Y Economicas")  %>% 
  count() %>% 
  pivot_wider(names_from = YEAR, values_from = n) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  select(PROGRAMA, FACULTAD, TIPO_NIVEL, SEDE_NOMBRE_MAT, "2009", "2019")
write.csv(pendiente_medellin, file = "pendiente_medellin.csv", row.names = F)

#Pendiente sede bogota
pendiente_bogota <- Graduados %>% 
  group_by(YEAR, PROGRAMA, FACULTAD, TIPO_NIVEL, SEDE_NOMBRE_MAT) %>% 
  filter(SEDE_NOMBRE_MAT == "Bogotá", NIVEL == "Pregrado")  %>% 
  count() #%>% 
  pivot_wider(names_from = YEAR, values_from = n) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  select(PROGRAMA, FACULTAD, TIPO_NIVEL, SEDE_NOMBRE_MAT, "2009", "2019")
write.csv(pendiente_medellin, file = "pendiente_medellin.csv", row.names = F)

#Pendiente sede manizales
pendiente_manizales <- Graduados %>% 
  group_by(YEAR, PROGRAMA, FACULTAD, TIPO_NIVEL, SEDE_NOMBRE_MAT) %>% 
  filter(SEDE_NOMBRE_MAT == "Manizales", NIVEL == "Pregrado")  %>% 
  count() #%>% 
pivot_wider(names_from = YEAR, values_from = n) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  select(PROGRAMA, FACULTAD, TIPO_NIVEL, SEDE_NOMBRE_MAT, "2009", "2019")
write.csv(pendiente_medellin, file = "pendiente_medellin.csv", row.names = F)


, 
PROGRAMA != "Minas", PROGRAMA != "Ciencias", 
PROGRAMA != "Ciencias Humanas Y Economicas"




 









#GRAFICO DE RADAR
puntos <- Graduados %>% 
  group_by(SEDE_NOMBRE_ADM) %>% 
  count()
write.csv(puntos, file = "puntos.csv", row.names = F)

#BOXPLOT EDADES
edad <- Graduados %>% 
  group_by(YEAR_SEMESTER, EDAD_MOD, NIVEL, SEDE_NOMBRE_MAT) %>% count() %>% 
  filter(EDAD_MOD > 15, EDAD_MOD <70)
write.csv(edad, file = "edad.csv", row.names = F)
