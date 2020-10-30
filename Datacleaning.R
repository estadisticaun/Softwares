library(readxl)
library(R.utils)
library(stringr)
library(dplyr)

#LECTURA Y UNION DE BASES DE DATOS
Graduados <- data.frame()
for(archivo in list.files()) {
  if(grepl("Graduados", archivo, fixed = T)){
    Graduados <- rbind(Graduados, read_excel(archivo))
  }
}

#ELIMINAR COLUMNAS INNECESARIAS
nombre <- c("ID","TID", "COD_DEP_NAC", "COD_CIU_NAC", "COD_DEP_PROC", 
            "COD_CIU_PROC", "CODS_NAC", "CODN_NAC", "SNIES_SEDE_ADM", 
            "SNIES_SEDE_MAT", "SNIESU_CONVENIO")
Graduados <- Graduados[, !(names(Graduados) %in% nombre)]

#INICIALES CON MAYUSCULA 
Graduados$DEP_NAC <- capitalize(tolower(Graduados$DEP_NAC))
Graduados$DEP_NAC <- str_replace_all(Graduados$DEP_NAC, ", d\\. c\\.", " D.C.")

#QUITAR NUMEROS DE LA VARIABLE CIU_NAC
Graduados$CIU_NAC <- str_remove_all(Graduados$CIU_NAC, "\\d")
Graduados$CIU_NAC <- capitalize(tolower(Graduados$CIU_NAC))
Graduados$CIU_NAC <- str_replace_all(Graduados$CIU_NAC, "d\\.c\\.", "D.C.")

#VERIFICAR NA's
NAs <- data.frame()
for (columna in colnames(Graduados)) {
  resultado <- data.frame(nombre = columna, 
                          NAs = sum(is.na(Graduados[,columna])))
  NAs <- rbind(NAs, resultado)
}

#ELIMINAR COLUMNAS QUE ESTAN COMPLETAMENTE VACIAS
nombre <- c("DEP_PROC", "CIU_PROC", "LON_CIU_PROC", "LAT_CIU_PROC", "TIPO_COL",
            "PBM_ORIG", "PBM", "MAT_PVEZ", "DISCAPACIDAD", "TIPO_DISC", 
            "MOV_PEAMA")
Graduados <- Graduados[,!(names(Graduados) %in% nombre)]

#IDENTIFICACIÓN DE CATEGORIAS
nacionalidad <- Graduados %>% group_by(NACIONALIDAD) %>% count()

Graduados$DEP_NAC <- 
  str_replace_all(Graduados$DEP_NAC,
                  "Archipiélago de san andrés, providencia y( santa catalina)?",
                  "Archipiélago de san andrés, providencia y santa catalina")
Graduados$DEP_NAC <- str_replace_all(Graduados$DEP_NAC,"Bogot(á|a)( D.C.)?",
                                     "Bogotá D.C.")
depnac <- Graduados %>% group_by(DEP_NAC) %>% count()

ciunac <- Graduados %>% group_by(CIU_NAC) %>% count()

