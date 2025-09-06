#-------------------------------------------------------------------------------
#La adherencia la deberíamos tener en los 4 puntos de la intervención, a saber:
#ejercicio físico (medido como cantidad de veces que van al gimnasio),
#estimulación cognitiva (medido como la cantidad de niveles de BrainHQ semanales)
#Dieta medido como el mind score + contactos telefónicos
#monitoreo de salud como asistencia a las reuniones grupales
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#Materia prima
Adherencia <- df_v4 %>%
  filter(Randomization == "Yes",
         Arm == "Systematic")
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#Empecemos por el principio: actividad física
#La actividad está programada para hacerse 4 veces por semana
#Empieza en la semana 5.
#Entonces tengo 48 semanas en el primer año
#y 52 en el segundo

PorcentajeMedioEF <- Adherencia %>%
  group_by(record_id)%>%
  mutate(
    BrutoSEF = sum(ef_count, na.rm = TRUE), #sesiones en crudo
    PorcentajeSEF = (BrutoSEF / 400) * 100) #porcentaje de sesiones
    
#Media por centro
MediaEF <- PorcentajeMedioEF %>%
  group_by(center)%>%
  summarise(
    mediaEF = mean(PorcentajeSEF))
MediaEF

#-------------------------------------------------------------------------------
#Tabla
library(gt)
TablaSesionesEF <- MediaEF %>%
  gt() %>%
  fmt_number(columns = mediaEF, decimals = 2) %>%
  tab_header(
    title = "% Adherencia a sessiones de ejercicio físico por centro")
TablaSesionesEF
gtsave(TablaSesionesEF, "Adherence/MediaEF.html")
#-------------------------------------------------------------------------------

SesionesEF <- ggplot(PorcentajeMedioEF, aes(x = center,
                              y = PorcentajeSEF))+
  geom_point(size = 3, alpha = 0.4, color = "dodgerblue4")+
  stat_summary(fun = mean, geom = "point",
               size = 3, color = "#B23AEE")+
  stat_summary(fun = mean, geom = "line", color = "#B23AEE",
               linetype = "dashed")+
  labs(x = "Center", y = "% adherence to physical exercise sessions",
       title = "% Physical Exercise by center")+
  theme_light()

ggsave("Adherence/PorcentajeSesionesFis.png",
       plot = SesionesEF, width = 12, height = 6, dpi = 300, bg = "white")

#-------------------------------------------------------------------------------
#Estimulación cognitiva
#-------------------------------------------------------------------------------















