library(readxl)
library(R.utils)
library(stringr)
library(dplyr)

#LECTURA Y UNION DE BASES DE DATOS
Graduados <- data.frame()
for(archivo in list.files("C:/Users/camil/Escritorio/Estudiante Auxiliar/Softwares")) {
  if(str_detect(archivo, "P\\d{4,5} Graduados")){
    Graduados <- rbind(
      Graduados, 
      read_excel(
        str_c(
          "C:/Users/camil/Escritorio/Estudiante Auxiliar/Softwares", 
          archivo, 
          sep = "/")))
  }
}

#ELIMINAR COLUMNAS INNECESARIAS
nombre <- c("ID","TID", "COD_DEP_NAC", "COD_CIU_NAC", "COD_DEP_PROC", 
            "COD_CIU_PROC", "CODS_NAC", "CODN_NAC", "SNIES_SEDE_ADM", 
            "SNIES_SEDE_MAT", "SNIESU_CONVENIO")
Graduados <- Graduados[, !(names(Graduados) %in% nombre)]

#QUITAR NUMEROS DE LA VARIABLE CIU_NAC
Graduados$CIU_NAC <- str_remove_all(Graduados$CIU_NAC, "\\d")
Graduados$CIU_NAC <- capitalize(tolower(Graduados$CIU_NAC))

#VERIFICAR NA's
NAs <- data.frame()
for (columna in colnames(Graduados)) {
  resultado <- data.frame(nombre = columna, 
                          NAs = sum(is.na(Graduados[,columna])))
  NAs <- rbind(NAs, resultado)
}

#ELIMINAR COLUMNAS QUE ESTAN COMPLETAMENTE VACIAS Y ALGUNAS INNECESARIAS
nombre <- c("DEP_PROC", "CIU_PROC", "LON_CIU_PROC", "LAT_CIU_PROC", "TIPO_COL",
            "PBM_ORIG", "PBM", "MAT_PVEZ", "DISCAPACIDAD", "TIPO_DISC", 
            "MOV_PEAMA", "FACULTAD_S", "PROGRAMA_S")
Graduados <- Graduados[,!(names(Graduados) %in% nombre)]

#IDENTIFICACIÓN DE CATEGORIAS

#se corrige la ciudad de Bogotá, d.c por Bogotá D.C 
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "d\\.c\\.", "D.C.")
#AGRUPAR POR CIU_NAC PARA IDENTIFICAR INCONSISTENCIAS
ciunac <- Graduados %>% group_by(CIU_NAC) %>% count()
#Cambiar todo a minuscula
Graduados$CIU_NAC <- tolower(Graduados$CIU_NAC)
#eliminar tildes de la columna CIU_NAC
Graduados$CIU_NAC <- chartr('áéíóú','aeiou', Graduados$CIU_NAC)
#capitalizar para que la primera letra de cada palabra sea mayuscula
Graduados$CIU_NAC <- capitalize(Graduados$CIU_NAC)

#Se agrupa por ciudad, latitud y longitud
puntos <- Graduados %>% 
  group_by(CIU_NAC, LAT_CIU_NAC, LON_CIU_NAC) %>% 
  count()
#Se identifica información duplicada y se debe eliminar
puntos <- puntos[!duplicated(puntos$CIU_NAC), ]
puntos <- puntos %>% select(-n)
#Se eliminan las columnas latitud y longitud de la base original para realizar la union
Graduados <- Graduados %>% select(-c("LAT_CIU_NAC", "LON_CIU_NAC"))
Graduados <- left_join(Graduados, puntos, by = "CIU_NAC")

#se corrige nombres de algunas ciudades
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Bogota(|,) d.c.", 
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
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "Manaure(\\sbalcon\\sdel\\scesar)?", 
                                     "Manaure")

#AGRUPAR POR DEP_NAC PARA IDENTIFICAR INCONSISTENCIAS
depnac <- Graduados %>% group_by(DEP_NAC) %>% count()
#INICIALES CON MAYUSCULA 
Graduados$DEP_NAC <- capitalize(tolower(Graduados$DEP_NAC))
#eliminar tildes de la columna DEP_NAC
Graduados$DEP_NAC <- chartr('áéíóú','aeiou', Graduados$DEP_NAC)
#Corrección de archipielago de san andres
Graduados$DEP_NAC <- 
  str_replace_all(Graduados$DEP_NAC,
                  "Archipielago de san andres, providencia y( santa catalina)?",
                  "Archipielago de san andres, providencia y santa catalina")
#se corrige el departamento Bogotá por Bogotá D.C 
Graduados$DEP_NAC <- str_replace_all(Graduados$DEP_NAC, ", d\\. c\\.", " D.C")
Graduados$DEP_NAC <- str_replace_all(Graduados$DEP_NAC, "Bogota( D.C)?", 
                                     "Bogota D.C")

#Cambiar el nombre de algunas ciudades dependiendo de su departameto
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
















#AGRUPAR POR NIVEL PARA IDENTIFICAR INCONSISTENCIAS
nivel <- Graduados %>% group_by(NIVEL) %>% count()
#convertir a título para eliminar inconsistencia con especialidades medicas
Graduados$NIVEL <- str_to_title(Graduados$NIVEL)

#Se agrupa por la sede de matricula y se verifica inconcistencia con sede Amazonia
sede_matricula <- Graduados %>% group_by(SEDE_NOMBRE_MAT) %>% count()
#correccion de sede Amazonia
Graduados$SEDE_NOMBRE_MAT <- str_replace_all(Graduados$SEDE_NOMBRE_MAT,
                                             "Amazon(í|i)a", "Amazonía")

#Se agrupa por tipo de admision
tipo_adm <- Graduados %>% group_by(TIPO_ADM) %>% count()
#se corrige incosistencia detectada con el tipo PEAMA
Graduados$TIPO_ADM <- str_replace_all(Graduados$TIPO_ADM,
                                      "PEAM?A", "PEAMA")

#Se agrupa por facultad y sede de matricula
Facultad <- Graduados %>%  group_by(SEDE_NOMBRE_MAT, FACULTAD) %>% count()
#Se corrige tilde en facultad Amazonía
Graduados$FACULTAD <- str_to_title(Graduados$FACULTAD)
Graduados$FACULTAD <- str_replace_all(Graduados$FACULTAD,
                                      "Amazon(i|í)a", "Amazonía")
#sede Medellín no tiene falcultad de ciencias agropecuarias, por lo que debe
#ser reasignada a la de ciencias agrarias
Graduados[Graduados$SEDE_NOMBRE_MAT == "Medellín" & 
            Graduados$FACULTAD == "Ciencias Agropecuarias", "FACULTAD"] <- "Ciencias Agrarias"
#sede Bogotá no tiene falcultad de Agronomía, por lo que debe ser reasignada a 
#la de ciencias agrarias
Graduados[Graduados$SEDE_NOMBRE_MAT == "Bogotá" & 
            Graduados$FACULTAD == "Agronomía", "FACULTAD"] <- "Ciencias Agrarias"

#Se realiza la agrupacion por sede de matricula, programa y facultad
Facultades <- Graduados %>% 
  group_by(SEDE_NOMBRE_MAT, PROGRAMA, FACULTAD) %>% 
  summarise(n = n())
#Con la agrupacion anterior se identifica que hay programas en dos faculates,
#lo cual no es correcto, por lo tanto se retiran los duplicados existentes
Programas <- Facultades %>% 
  distinct(SEDE_NOMBRE_MAT, PROGRAMA, .keep_all = TRUE) %>% 
  select(-n)
#Eliminar facultad de la base general para luego realizar la union
Graduados <- Graduados %>% select(-FACULTAD)
Graduados <- left_join(Graduados, Programas, by = c("SEDE_NOMBRE_MAT", "PROGRAMA"))

#Agrupar por programa
programa <- Graduados %>% group_by(PROGRAMA) %>% count()
Graduados$PROGRAMA <- tolower(Graduados$PROGRAMA)
#eliminar tildes con el fin de eliminar inconsistencias
Graduados$PROGRAMA <- chartr('áéíóú','aeiou', Graduados$PROGRAMA)
Graduados$PROGRAMA <- str_to_title(Graduados$PROGRAMA, locale = "en")
#se remueven palabras sobrantes en los programas de formación
Graduados$PROGRAMA <- str_remove_all(Graduados$PROGRAMA,"Especializacion En ")
Graduados$PROGRAMA <- str_remove_all(Graduados$PROGRAMA,"Especializacion ")
Graduados$PROGRAMA <- str_remove_all(Graduados$PROGRAMA,"Ciencias - ")
Graduados$PROGRAMA <- str_remove_all(Graduados$PROGRAMA,"Ciencias-")
#se reemplazan nombres de programas que son incorrectos
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Accion Sin Daño(\\sY\\s)?(C|c)onstruccion De Paz", 
                                      "Accion Sin Daño Y Construccion De Paz")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Actuaria(\\sY\\s)?(F|f)inanzas", 
                                      "Actuaría Y Finanzas")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Administracion De Empresas \\(D(iurno)?\\)", 
                                      "Administración De Empresas (Diurno)")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Administracion De Empresas \\(N(octurno)?\\)", 
                                      "Administración De Empresas (Nocturno)")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Administracion De Sistemas Informaticos", 
                                      "Administración De Sistemas Informáticos")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Administracion (En )?Salud Publica", 
                                      "Administración En Salud Pública")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Alimentacion(\\sY\\s)?(N|n)utricion (En\\s)?Promoci(ó|o)n De La(\\s)?(S|s)alud",
                                      "Alimentación Y Nutrición En Promoción De La Salud")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Anestesiologia(\\sY\\s)?(R|r)eanimacion", 
                                      "Anestesiología Y Reanimación")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Aprovechamiento De Recursos Hidraulicos", 
                                      "Aprovechamiento De Recursos Hidráulicos")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Arquitectura De La(\\s)?(V|v)ivienda", 
                                      "Arquitectura De La Vivienda")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Arte(\\sY\\s)?(A|a)rquitectura", 
                                      "Arte Y Arquitectura")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Artes Plasticas(\\sY\\s)?(V|v)isuales", 
                                      "Artes Plásticas Y Visuales")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Biociencias(\\sY\\s)?(d|D)erecho", 
                                      "Biociencias Y Derecho")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Bosques(\\sY\\s)?(c|C)onservacion Ambiental", 
                                      "Bosques Y Conservación Ambiental")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ciencia Politica", 
                                      "Ciencia Política")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ciencia(\\sY\\s)?(T|t)ecnologia De Alimentos", 
                                      "Ciencia Y Tecnología De Alimentos")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ciencia(\\sY\\s)?(T|t)ecnologia Cosmetica", 
                                      "Ciencia Y Tecnología Cosmética")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ciencias Humanas(\\sY\\s)?(S|s)ociales", 
                                      "Ciencias Humanas Y Sociales")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Cine(\\sY\\s)?(T|t)elevision", 
                                      "Cine Y Television")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Cirugia Oral(\\sY\\s)?(M|m)axilofacial", 
                                      "Cirugia Oral Y Maxilofacial")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Comunicacion(\\sY\\s)?(M|m)edios", 
                                      "Comunicacion Y Medios")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Contabilidad(\\sY\\s)?(F|f)inanzas", 
                                      "Contabilidad Y Finanzas")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Cultivos Pere?nnes Industriales", 
                                      "Cultivos Perennes Industriales")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Derechos Humanos(\\sY\\s)?(D|d)erecho Internacional Humanitario", 
                                      "Derechos Humanos Y Derecho Internacional Humanitario")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Direccion De Produccion(\\sY\\s)?(O|o)peraciones", 
                                      "Direccion De Produccion Y Operaciones")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Diseño(\\sY\\s)?(D|d)esarrollo Del? Producto", 
                                      "Diseño Y Desarrollo Del Producto")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Enseñanza De Las Ciencias Exactas(\\sY\\s)?(N|n)aturales", 
                                      "Enseñanza De Las Ciencias Exactas Y Naturales")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Español(\\sY\\s)?(F|f)ilologia Clasica", 
                                      "Español Y Filologia Clasica")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Estomatologia Pediatrica(\\sY\\s)?(O|o)rtopedia Maxilar", 
                                      "Estomatologia Pediatrica Y Ortopedia Maxilar")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Estudios De Genero ?-?Area Mujer(\\sY\\s)?(D|d)esarrollo", 
                                      "Estudios De Genero - Area Mujer Y Desarrollo")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Estudios Feministas(\\sY\\s)?(D|d)e Genero", 
                                      "Estudios Feministas Y De Genero")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Estudios Politicos(\\sY\\s)?(R|r)elaciones Internacionales", 
                                      "Estudios Politicos Y Relaciones Internacionales")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Estudios Sociales De La ?(C|c)iencia", 
                                      "Estudios Sociales De La Ciencia")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Estudios Urbano ?- ?Regionales", 
                                      "Estudios Urbano - Regionales")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Fisioterapia\\sDel\\sDeporte( Y | Y La |la )?Actividad\\sFisica",
                                      "Fisioterapia Del Deporte Y La Actividad Fisica")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Geomorfologia De Suelos", 
                                      "Geomorfologia Y Suelos")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Gestion Cultural,?\\s(Con\\s)?Enfasis\\s(En\\s)?Planeacion(\\sY\\s)?(P|p)oliticas\\sC(ulturales)?",
                                      "Gestion Cultural Con Enfasis En Planeacion Y Politicas Culturales")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Gestion Cultural(\\sY\\s)?(C|c)omunicativa", 
                                      "Gestion Cultural Y Comunicativa")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Gestion De Redes De Datos", 
                                      "Gestion De Redes Y Datos")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Transito, Diseño(\\sY\\s)?(S|s)eguridad Vial", 
                                      "Transito, Diseño Y Seguridad Vial")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Historia(\\sY\\s)?(T|t)eoria Del Arte, La ?(A|a)rquitectura(\\sY\\s)?(L|l)a Ciudad", 
                                      "Historia Y Teoria Del Arte, La Arquitectura Y La Ciudad")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Iluminacion Publica(\\sY\\s)?(P|p)rivada", 
                                      "Iluminacion Publica Y Privada")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Infecciones(\\sY\\s)?(S|s)alud (En )?El Tropico", 
                                      "Infecciones Y Salud En El Tropico")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ingenieria - Ciencia(\\sY\\s)?(T|t)ecnologia De (Los )?Materiales", 
                                      "Ingenieria - Ciencia Y Tecnologia De Materiales")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ingenieria - Industria(\\sY\\s)?(O|o)rgan?izaciones", 
                                      "Ingenieria - Industria Y Organizaciones")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ingenieria - Infraestructura(\\sY\\s)?(S|s)istemas De Transporte", 
                                      "Ingenieria - Infraestructura Y Sistemas De Transporte")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ingenieria - Ingenieria De Sistemas(\\sY\\s)?(C|c)omputacion", 
                                      "Ingenieria - Ingenieria De Sistemas Y Computacion")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ingenieria - Ingenieria Mecanica(\\sY\\s)?(M|m)ecatronica", 
                                      "Ingenieria - Ingenieria Mecanica Y Mecatronica")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ingenieria - Materiales(\\sY\\s)?(P|p)rocesos", 
                                      "Ingenieria - Materiales Y Procesos")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ingenieria - Sistemas(\\sY\\s)?(C|c)omputacion", 
                                      "Ingenieria - Sistemas Y Computacion")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ingenieria - Transportes?", 
                                      "Ingenieria - Transporte")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ingenieria\\sAmbiental,?\\s-?\\s?Area\\sSanitaria",
                                      "Ingenieria Ambiental - Area Sanitaria")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ingenieria De Minas(\\sY\\s)?(M|m)etalurgia", 
                                      "Ingenieria De Minas Y Metalurgia")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ingenieria De Sistemas(\\sY\\s)?(C|c)omputacion", 
                                      "Ingenieria De Sistemas Y Computacion")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ingenieria Hidraulica(\\sY\\s)?(A|a)mbiental", 
                                      "Ingenieria Hidraulica Y Ambiental")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Instituciones Juridicas De La ?(S|s)eguridad Social", 
                                      "Instituciones Juridicas De La Seguridad Social")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Instituciones Juridico-? ?Procesales", 
                                      "Instituciones Juridico-Procesales")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "(Maestria )?Interdisc?i?plinaria (En )?Teatro(\\sY\\s)?(A|a)rtes Vivas", 
                                      "Interdisciplinaria En Teatro Y Artes Vivas")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Interpretacion(\\sY\\s)?(P|p)edagogia Instrumental", 
                                      "Interpretacion Y Pedagogia Instrumental")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Interventoria De Proyectos(\\sY\\s)?(O|o)bras", 
                                      "Interventoria De Proyectos Y Obras")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Materiales(\\sY\\s)?(P|p)rocesos", 
                                      "Materiales Y Procesos")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Medicina Fisica(\\sY\\s)?(R|r)ehabilitacion", 
                                      "Medicina Fisica Y Rehabilitacion")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Medio Ambiente(\\sY\\s)?(D|d)esarrollo", 
                                      "Medio Ambiente Y Desarrollo")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Mercados(\\sY\\s)?(P|p)oliticas De Suelo (En )?America Latina", 
                                      "Mercados Y Politicas De Suelo En America Latina")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Museologia(\\sY\\s)?(G|g)estion Del Patrimonio", 
                                      "Museologia Y Gestion Del Patrimonio")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Nutricion(\\sY\\s)?(D|d)ietetica", 
                                      "Nutricion Y Dietetica")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Obstetricia(\\sY\\s)?(G|g)inecologia", 
                                      "Obstetricia Y Ginecologia")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Onco(\\s-\\s)?(H|h)ematologia Pediatrica", 
                                      "Onco - Hematologia Pediatrica")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ortodoncia(\\sY\\s)?(O|o)rtopedia Maxilar", 
                                      "Ortodoncia Y Ortopedia Maxilar")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Ortopedia(\\sY\\s)?(T|t)raumatologia", 
                                      "Ortopedia Y Traumatologia")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Patologia\\sDe\\sLa\\s?(E|e)dificacion(\\sY\\s)?(T|t)ecnicas\\sDe\\sIntervencion\\sY(\\sPrevencion)?",
                                      "Patologia De La Edificacion Y Tecnicas De Intervencion Y Prevencion")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Perinatologia(\\sY\\s)?(N|n)eonatologia", 
                                      "Perinatologia Y Neonatologia")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Psicoanalisis, Subjetividad(\\sY\\s)?(C|c)ultura(\\s\\(De Freud A Lacan\\))?", 
                                      "Psicoanalisis, Subjetividad Y Cultura (De Freud A Lacan)")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Salud Animal O ?(P|p)roduccion Animal", 
                                      "Salud Animal O Produccion Animal")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Salud(\\sY\\s)?(S|s)eguridad (En )?El Trabajo", 
                                      "Salud Y Seguridad En El Trabajo")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Seguridad Alimentaria(\\sY\\s)?(N|n)utricional", 
                                      "Seguridad Alimentaria Y Nutricional")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Salud(\\sY\\s)?(P|p)roduccion Animal", 
                                      "Salud Y Produccion Animal")
Graduados$PROGRAMA <- str_replace_all(Graduados$PROGRAMA,
                                      "Vias(\\sY\\s)?(T|t)ransportes?", 
                                      "Vias Y Transportes")





Graduados$YEAR_SEMESTER <- str_c(Graduados$YEAR, Graduados$SEMESTRE,
                                 sep = " - ")



write.csv(Graduados, file = "Datos.csv", row.names = F)