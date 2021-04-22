library(dplyr)
library(tidyverse)
library(magrittr)
library(rjson)
library(R.utils)
library(jsonlite)
Graduados <- read.csv("Datos.csv", sep = ",", header = TRUE)
#es necesario volver a mayusculas para coincidir con el archivo GeoJson
Graduados$DEP_NAC <- toupper(Graduados$DEP_NAC)

#GRAFICO DE LINEAS
evoluciongeneral <- Graduados %>% group_by(YEAR_SEMESTER) %>% count()
write.csv(evoluciongeneral, file = "Datos para Flourish/evoluciongeneral.csv", row.names = F)

#GRAFICO DE LINEAS SEGMNETADO POR UNA DIMENSION (MODALIDAD DE FORMACION)
modalidadformacion <- Graduados %>% group_by(YEAR_SEMESTER, TIPO_NIVEL) %>% 
  count() %>% 
  pivot_wider(names_from = TIPO_NIVEL, values_from = n) %>% 
  mutate(Total = Pregrado + Postgrado) %>% 
  mutate(Porcentajepregrado = paste(round((Pregrado/Total)*100, 2), "%")) %>% 
  mutate(Porcentajepostgrado = paste(round((Postgrado/Total)*100, 2), "%"))
write.csv(modalidadformacion, file = "Datos para Flourish/modalidadformacion.csv", row.names = F)

#TABLA (MODALIDAD DE FORMACION)
modalidadformaciontabla <- Graduados %>% group_by(YEAR, SEMESTRE, TIPO_NIVEL) %>% 
  count() %>% 
  pivot_wider(names_from = TIPO_NIVEL, values_from = n) %>% 
  mutate(Total = Pregrado + Postgrado) %>% 
  mutate(Porcentajepregrado = paste(round((Pregrado/Total)*100, 2), "%")) %>% 
  mutate(Porcentajepostgrado = paste(round((Postgrado/Total)*100, 2), "%"))
write.csv(modalidadformaciontabla, file = "Datos para Flourish/modalidadformaciontabla.csv", 
          row.names = F)

#GRAFICO CIRCULAR (MODALIDAD DE FORMACION) CON VARIABLE YEAR_SEMESTER
modalidadformacioncircular <- Graduados %>% 
  group_by(YEAR_SEMESTER, TIPO_NIVEL) %>% 
  mutate(YEAR_SEMESTER = as.factor(YEAR_SEMESTER)) %>% 
  summarise(Cantidad = n()) %>% 
  mutate(Total = aggregate(Cantidad~YEAR_SEMESTER, FUN = sum)[,2]) %>%  
  mutate(Porcentaje = paste(round((Cantidad/Total)*100, 2), "%"))
write.csv(modalidadformacioncircular, file = "Datos para Flourish/modalidadformacioncircular.csv", 
          row.names = F)

#FILTRADO POR PERIODO 2020-1
modalidadformacioncircularfiltrada <- Graduados %>% 
  group_by(YEAR_SEMESTER, TIPO_NIVEL) %>% 
  summarise(Cantidad = n()) %>% 
  filter(YEAR_SEMESTER == "2020 - 1") %>% 
  mutate(Porcetaje = round((Cantidad/sum(Cantidad))*100, 2))
write.csv(modalidadformacioncircularfiltrada, 
          file = "Datos para Flourish/modalidadformacioncircularfiltrada.csv", 
          row.names = F)

#GRAFICO DE LINEAS SEGMNETADO POR UNA DIMENSION (NIVEL DE FORMACION)
lineasnivelformacion <- Graduados %>% group_by(YEAR_SEMESTER, NIVEL) %>% 
  count() %>% 
  pivot_wider(names_from = NIVEL, values_from = n) %>% 
  mutate(Total = Pregrado + Maestría + `Especialidades Médicas` + 
           Especialización + Doctorado) %>% 
  mutate(Porcentajepregrado = paste(round((Pregrado/Total)*100, 2), "%")) %>% 
  mutate(PorcentajeMaestria = paste(round((Maestría/Total)*100, 2), "%")) %>% 
  mutate(Porcentajeespecialidades = paste(round((`Especialidades Médicas`/Total)*100, 2), "%")) %>%
  mutate(Porcentajeespecializacion = paste(round((Especialización/Total)*100, 2), "%")) %>% 
  mutate(Porcentajedoctorado = paste(round((Doctorado/Total)*100, 2), "%"))
write.csv(lineasnivelformacion, 
          file = "Datos para Flourish/lineasnivelformacion.csv", 
          row.names = F)

#GRAFICO DE BARRAS (NIVEL DE FORMACION)
nivelformacion <- Graduados %>% 
  group_by(YEAR_SEMESTER, NIVEL) %>% 
  summarise(Cantidad = n()) %>% 
  mutate(Total = aggregate(Cantidad~YEAR_SEMESTER, FUN = sum)[,2]) %>% 
  mutate(Porcentaje = paste(round((Cantidad/Total)*100, 2), "%")) %>% 
  filter(YEAR_SEMESTER == "2020 - 1")
write.csv(nivelformacion, file = "Datos para Flourish/nivelformacion.csv", 
          row.names = FALSE)

#GRAFICO DE LINEAS SEGMNETADO POR UNA DIMENSION (SEDE DE ADMISION)
lineasedeadmision <- Graduados %>% group_by(YEAR_SEMESTER, SEDE_NOMBRE_ADM) %>% 
  count() %>% 
  pivot_wider(names_from = SEDE_NOMBRE_ADM, values_from = n) %>%
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(Total = Amazonía + Bogotá + Manizales + Medellín + Palmira +
           Caribe + Orinoquía) %>% 
  mutate(Porcentajeamazonia = paste(round((Amazonía/Total)*100, 2), "%")) %>% 
  mutate(Porcentajebogota= paste(round((Bogotá/Total)*100, 2), "%")) %>% 
  mutate(Porcentajemanizales = paste(round((Manizales/Total)*100, 2), "%")) %>%
  mutate(Porcentajemedellin = paste(round((Medellín/Total)*100, 2), "%")) %>% 
  mutate(Porcentajepalmira = paste(round((Palmira/Total)*100, 2), "%")) %>% 
  mutate(Porcentajecaribe = paste(round((Caribe/Total)*100, 2), "%")) %>% 
  mutate(Porcentajeorinoquia = paste(round((Orinoquía/Total)*100, 2), "%"))
write.csv(lineasedeadmision, 
          file = "Datos para Flourish/lineasedeadmision.csv", 
          row.names = F)

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
write_json(data.json, "Datos para Flourish/depts.json")


#PUNTOS MUNICIPIOS
municipios <- Graduados %>% 
  group_by(CIU_NAC, LON_CIU_NAC, LAT_CIU_NAC) %>% 
  count()
write.csv(municipios, file = "Datos para Flourish/municipios.csv", 
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
write.csv(Capitales, file = "Datos para Flourish/capitales.csv", 
          row.names = FALSE)

#MAPA POR MUNICIPIOS
#Municipios periodo 2020-1
mps <- Graduados %>% 
  group_by(YEAR_SEMESTER, DEP_NAC, CIU_NAC) %>% 
  count() %>% 
  filter(YEAR_SEMESTER == "2020 - 1") %>% 
  mutate(CIU_NAC = str_to_upper(CIU_NAC))

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
write_json(mpios.json, "Datos para Flourish/municipios.json")

#MAPA DE PUNTOS 3D
municipios3d <- Graduados %>% group_by(YEAR, LON_CIU_NAC, LAT_CIU_NAC,
                                 DEP_NAC, CIU_NAC) %>% count()
write.csv(municipios3d, "Datos para Flourish/municipios3d.csv", row.names = FALSE)

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
write.csv(locaciones, "Datos para Flourish/locaciones.csv", row.names = F)

#VALORES: categoria, año, origen (coincidir con codigo), destino(coincidir 
#con codigo), valor(conteo de estudiantes)
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
write.csv(valores, "Datos para Flourish/valores.csv", row.names = F)

#JERARQUIA
jerarquia <- Graduados %>% 
  group_by(SEDE_NOMBRE_MAT, FACULTAD, PROGRAMA, NIVEL) %>% 
  mutate(SEDE_NOMBRE_MAT = chartr("áéíóúü", "aeiouu", SEDE_NOMBRE_MAT),
         FACULTAD = chartr("áéíóúü", "aeiouu", FACULTAD),
         NIVEL = chartr("áéíóúü", "aeiouu", NIVEL),
         PROGRAMA = chartr("áéíóúü", "aeiouu", PROGRAMA)) %>% 
  summarise(n = n()) %>% 
  arrange(SEDE_NOMBRE_MAT, FACULTAD, NIVEL, desc(n)) 
 
write.csv(jerarquia, "Datos para Flourish/jerarquia.csv", row.names = F)

#DIAGRAMA DE SANKEY
sankey <- Graduados %>% 
  group_by(SEDE_NOMBRE_ADM, SEDE_NOMBRE_MAT) %>% 
  count() %>% 
  filter(SEDE_NOMBRE_ADM != SEDE_NOMBRE_MAT)
write.csv(sankey, file = "Datos para Flourish/sankey.csv", row.names = F)

#CARRERA DE BARRAS
car_barras <- Graduados %>% 
  group_by(YEAR_SEMESTER, AREAC_SNIES, NIVEL) %>% 
  count() %>% 
  pivot_wider(names_from = YEAR_SEMESTER, values_from = n) %>% 
  mutate_all(~replace(., is.na(.), 0))
write.csv(car_barras, file = "Datos para Flourish/car_barras.csv", row.names = F)

#CARRERA DE LINEAS
Regiones <- read.csv("Regiones_colombia.csv", sep = ";", header = TRUE)
Departamentos <- Graduados %>% group_by(DEP_NAC, YEAR_SEMESTER) %>% 
  mutate(DEP_NAC = capitalize(tolower(DEP_NAC)),
         DEP_NAC = str_replace_all(DEP_NAC, "d.c", "D.C")) %>% 
  count()
car_lineas <- left_join(Regiones, Departamentos, 
                        by = c("Departamento" = "DEP_NAC")) %>% 
  pivot_wider(names_from = YEAR_SEMESTER, values_from = n) %>% 
  mutate_all(~replace(., is.na(.), 0))
write.csv(car_lineas, file = "Datos para Flourish/car_lineas.csv", row.names = F)

#GRAFICO DE PENDIENTES

#Pendiente sede bogota
pendiente_bogota <- Graduados %>% 
  group_by(YEAR, PROGRAMA, FACULTAD, TIPO_NIVEL, SEDE_NOMBRE_MAT) %>% 
  filter(SEDE_NOMBRE_MAT == "Bogotá", NIVEL == "Pregrado")  %>% 
  count() %>% 
  pivot_wider(names_from = YEAR, values_from = n) %>% 
  select(PROGRAMA, FACULTAD, TIPO_NIVEL, SEDE_NOMBRE_MAT, "2009", "2019") %>% 
  drop_na()
write.csv(pendiente_bogota, file = "Datos para Flourish/pendiente_bogota.csv", row.names = F)

#Pendiente sede medellin
pendiente_medellin <- Graduados %>% 
  group_by(YEAR, PROGRAMA, FACULTAD, TIPO_NIVEL, SEDE_NOMBRE_MAT) %>% 
  filter(SEDE_NOMBRE_MAT == "Medellín", NIVEL == "Pregrado", 
         PROGRAMA != "Minas", PROGRAMA != "Ciencias", 
         PROGRAMA != "Ciencias Humanas Y Economicas")  %>% 
  count() %>% 
  pivot_wider(names_from = YEAR, values_from = n) %>% 
  select(PROGRAMA, FACULTAD, TIPO_NIVEL, SEDE_NOMBRE_MAT, "2009", "2019") %>% 
  drop_na()
write.csv(pendiente_medellin, file = "Datos para Flourish/pendiente_medellin.csv", row.names = F)

#Pendiente sede manizales
pendiente_manizales <- Graduados %>% 
  group_by(YEAR, PROGRAMA, FACULTAD, TIPO_NIVEL, SEDE_NOMBRE_MAT) %>% 
  filter(SEDE_NOMBRE_MAT == "Manizales", NIVEL == "Pregrado")  %>% 
  count() %>% 
  pivot_wider(names_from = YEAR, values_from = n) %>% 
  select(PROGRAMA, FACULTAD, TIPO_NIVEL, SEDE_NOMBRE_MAT, "2009", "2019") %>% 
  drop_na()
write.csv(pendiente_manizales, file = "Datos para Flourish/pendiente_manizales.csv", row.names = F)

#Pendiente sede palmira
pendiente_palmira <- Graduados %>% 
  group_by(YEAR, PROGRAMA, FACULTAD, TIPO_NIVEL, SEDE_NOMBRE_MAT) %>% 
  filter(SEDE_NOMBRE_MAT == "Palmira", NIVEL == "Pregrado")  %>% 
  count() %>% 
  pivot_wider(names_from = YEAR, values_from = n) %>% 
  select(PROGRAMA, FACULTAD, TIPO_NIVEL, SEDE_NOMBRE_MAT, "2009", "2019") %>% 
  drop_na()
write.csv(pendiente_palmira, file = "Datos para Flourish/pendiente_palmira.csv", row.names = F)


#BOXPLOT EDADES
edad <- Graduados %>% 
  group_by(YEAR_SEMESTER, EDAD_MOD, NIVEL, SEDE_NOMBRE_MAT) %>% count() %>% 
  filter(EDAD_MOD > 15, EDAD_MOD <70)
write.csv(edad, file = "Datos para Flourish/edad.csv", row.names = F)

#GRAFICO DE RADAR
#resultados saber pro
Saberpro <- read_xlsx("saberpro2019.xlsx")
programas <- Graduados %>% 
  filter(NIVEL == "Pregrado") %>% 
  group_by(SEDE_NOMBRE_MAT, FACULTAD, PROGRAMA) %>% 
  mutate(PROGRAMA = capitalize(tolower(chartr('áéíóúü','aeiouu', PROGRAMA))),
         FACULTAD = capitalize(tolower(FACULTAD))) %>% 
  count()

resultados_unal <- Saberpro %>% 
  filter(AGREGACION == "PROGRAMA_ACÁDEMICO", 
         NOMBRE_INSTITUCION == "UNIVERSIDAD NACIONAL DE COLOMBIA",
         MEDIDA_AGREGACION == "PUNTAJE_PRUEBA", 
         NOMBRE_PRUEBA %in% c("LECTURA CRÍTICA",
                              "RAZONAMIENTO CUANTITATIVO",
                              "COMPETENCIAS CIUDADANAS",
                              "COMUNICACIÓN ESCRITA",
                              "INGLÉS")) %>% 
  select("EXAMEN", "CANTIDADEVALUADOS", "NOMBRE_PRUEBA", "NOMBRE_SEDE",
         "ID_PROGRAMA_ACAD", "NOMBRE_PROGRAMA_ACAD", "NOMBRE_GRUPOREF",
         "PROMEDIO_PRUEBA") %>% 
  mutate(NOMBRE_PRUEBA = capitalize(tolower(NOMBRE_PRUEBA)),
         NOMBRE_SEDE = capitalize(tolower(str_replace(NOMBRE_SEDE ,
                                                      '[\\w\\s]+-(\\w+)', '\\1'))),
         NOMBRE_PROGRAMA_ACAD = capitalize(tolower(NOMBRE_PROGRAMA_ACAD)),
         NOMBRE_GRUPOREF = capitalize(tolower(NOMBRE_GRUPOREF)))

resultados_unal$NOMBRE_SEDE <- str_replace_all(resultados_unal$NOMBRE_SEDE,
                                               " d.c.", "")
resultados_unal$NOMBRE_SEDE <- str_replace_all(resultados_unal$NOMBRE_SEDE,
                                               "Medellin", "Medellín")
resultados_unal$NOMBRE_PROGRAMA_ACAD <- str_replace_all(resultados_unal$NOMBRE_PROGRAMA_ACAD,
                                                        "Literatura",
                                                        "Literatura (estudios literarios)")
union_saber <- left_join(resultados_unal, programas, 
                         by = c("NOMBRE_SEDE" = "SEDE_NOMBRE_MAT",
                                "NOMBRE_PROGRAMA_ACAD" = "PROGRAMA")) %>% 
  pivot_wider(names_from = NOMBRE_PRUEBA, values_from = PROMEDIO_PRUEBA) %>% 
  select(-n) %>% 
  mutate_all(~replace(., is.na(.), 0))


saber_bogota <- union_saber %>% filter(NOMBRE_SEDE == "Bogotá")
saber_medellin <- union_saber %>% filter(NOMBRE_SEDE == "Medellín")
saber_manizales <- union_saber %>% filter(NOMBRE_SEDE == "Manizales")
saber_palmira <- union_saber %>% filter(NOMBRE_SEDE == "Palmira")

write.csv(saber_bogota, file = "Datos para Flourish/saber_bogota.csv", row.names = F)
write.csv(saber_medellin, file = "Datos para Flourish/saber_medellin.csv", row.names = F)
write.csv(saber_manizales, file = "Datos para Flourish/saber_manizales.csv", row.names = F)
write.csv(saber_palmira, file = "Datos para Flourish/saber_palmira.csv", row.names = F)

#DIAGRAMAS DE DISPERSION
sample <- read_xls("Sample - Superstore.xls", sheet = "Orders")
sample$`Order Date` <- format(sample$`Order Date`, '%Y')
ventas <- write.csv(sample, file = "Datos para Flourish/ventas.csv", row.names = F)

#GRÁFICO DE PUNTOS
puntos <- Graduados %>% group_by(YEAR_SEMESTER, SEDE_NOMBRE_MAT) %>% count()
write.csv(puntos, file = "Datos para Flourish/graficopuntos.csv", row.names = F)

#GRAFICO DE PUNTOS CONECTADOS
conectados <- Graduados %>% 
  group_by(YEAR, SEDE_NOMBRE_ADM) %>% 
  filter(YEAR == 2009 | YEAR == 2019) %>% 
  count()
write.csv(conectados, file = "Datos para Flourish/conectados.csv", row.names = F)
