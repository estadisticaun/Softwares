library(ggplot2)
library(dplyr)
EdadesFormacion <- data.frame(Graduados$EDAD_MOD, Graduados$TIPO_NIVEL, 
                              Graduados$SEDE_NOMBRE_ADM)
EdadesFormacion <- na.omit(EdadesFormacion)
colnames(EdadesFormacion) <- c("EDAD_MOD", "TIPO_NIVEL", "SEDE_NOMBRE_ADM")
EdadesFormacion <- EdadesFormacion[-c(which(EdadesFormacion$EDAD_MOD < 15), 
                                      which(EdadesFormacion$EDAD_MOD > 70)), ]
EdadesFormacion$TIPO_NIVEL <- as.factor(EdadesFormacion$TIPO_NIVEL)

#Histograma para edades y modalidad de fromación
ggplot(EdadesFormacion, aes(x=EDAD_MOD, fill = TIPO_NIVEL)) + 
  geom_histogram(binwidth = 5, show.legend = FALSE) +
  labs(title = "Distribución de edades por modalidad de formación",
       y = "Número de graduados", x= "Edad") +
  facet_wrap(~TIPO_NIVEL) + scale_fill_manual(values=c("#f15a24", "#8cc63f"))
table(EdadesFormacion)

#Box-plot
EdadesNivel <- data.frame(Graduados$EDAD_MOD, Graduados$NIVEL, 
                          Graduados$SEDE_NOMBRE_ADM)
EdadesNivel <- na.omit(EdadesNivel)
colnames(EdadesNivel) <- c("EDAD_MOD", "NIVEL", "SEDE_NOMBRE_ADM")
EdadesNivel <- EdadesNivel[-c(which(EdadesNivel$EDAD_MOD < 15), 
                                      which(EdadesNivel$EDAD_MOD > 70)), ]
EdadesNivel$NIVEL <- as.factor(EdadesNivel$NIVEL)
ggplot(EdadesNivel, aes(x=NIVEL, y=EDAD_MOD, fill=NIVEL)) + 
  geom_boxplot(show.legend = F) + 
  scale_fill_manual(values=c("#6d6666", "#fbb03b", "#29abe2", "#c1272d", 
                           "#8cc63f")) +
  labs(title = "Boxplot para edad por nivel de formación",
       y = "Edad")

