dir.create(file.path(getwd(), "Adherence"), showWarnings = FALSE)

#-------------------------------------------------------------------------------
#La adherencia la deberíamos tener en los 4 puntos de la intervención, a saber:
#ejercicio físico (medido como cantidad de veces que van al gimnasio),
#estimulación cognitiva (medido como la cantidad de niveles de BrainHQ semanales)
#Dieta medido como el mind score + contactos telefónicos
#monitoreo de salud como asistencia a las reuniones grupales
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#Empecemos por el principio: actividad física
#La variable se llama: ef_count
# (52*4)*2

#Ejercicio en el gimnasio
#La actividad está programada para hacerse 4 veces por semana
#Empieza en la semana 5.
#Entonces tengo 48 semanas en el primer año
#y 52 en el segundo

df_v4 <- df_v4 %>%
  group_by(record_id)%>%
  mutate(
    EjercicioGYM = sum(ef_count, na.rm = TRUE), #suma total
    PorcentajeGYM = (EjercicioGYM / 400) * 100) #porcentaje

#Crudo por centro
GraficoFisico <- df_v4 %>%
  filter(Eventos %in% c("base","12m"))

GraficoFis <- ggplot(GraficoFisico, aes(y = PorcentajeGYM,
                  x = center))+
  geom_point(size = 3, alpha = 0.4,
             color = "dodgerblue4")+
  labs(title = "% Physical Exercise by center")+
  theme_light()

ggsave("Adherence/GraficoFis.svg",
       plot = GraficoFis, width = 12, height = 6, dpi = 300,
       bg = "white")

#Cantidad de sesiones totales por centro
ggplot(df_v5, aes(x = center,
                  y = ADH_ActFis))+
  geom_boxplot()+
  theme_light()

#Porcentaje por centro
ggplot(df_v5, aes(x = center,
                  y = PorcentajeActFis))+
  geom_point(size = 3, alpha = 0.1, 
             col = "#8968CD")+
  geom_boxplot(alpha = 0)+
  theme_light()

#-------------------------------------------------------------------------------
#Actividad cognitiva

#La variable se llama: ec_count
#728 sesiones posibles (52 * 7) * 2

df_v5 <- df_v5 %>%
  group_by(record_id)%>%
  mutate(
    ADH_Cogni = sum(ec_count, na.rm = TRUE), #suma total
    PorcentajeCogni = (ADH_Cogni / 728) * 100) #porcentaje

#Visualización en bruto por evento
ggplot(df_v5, aes(x = center,
                  y = ec_count, fill = Eventos))+
  geom_boxplot()+
  theme_light()

#Ahora vamos con el porcentaje
ggplot(df_v5, aes(x = center, y = PorcentajeCogni))+
  geom_boxplot()+theme_light()

#-------------------------------------------------------------------------------
#Control cardiovascular
#Me faltan reuniones porque no están en el dataset.

df_v5 <- df_v5 %>%
  group_by(record_id)%>%
  mutate(
    ADH_Salud = rowSums(
      across(c(mgr_5grupal1, mgr_5grupal2, mgr_6grupal1, mgr_6grupal2, 
               mgr_7grupal1, mgr_8grupal1, mgr_9grupal1, mgr_10grupal1, 
               mgr_11grupal1, mgr_12grupal1)),
      na.rm = TRUE),
    PorcentajeSalud = (ADH_Salud / 20) * 100)

ggplot(df_v5, aes(x = center,
                  y = PorcentajeSalud))+
  geom_boxplot()+theme_light()


#-------------------------------------------------------------------------------
#Nutrición
#Acá tenemos el envío de qué comieron: nis_count
#y el seguimiento telefónico total: nis_count_telefonica

df_v5 <- df_v5 %>%
  group_by(record_id)%>%
  mutate(
    ADH_Nutri = sum(nis_count, na.rm = TRUE), #suma total
    PorcentajeNutri = (ADH_Nutri / 104) * 100,
    ADH_NutriTel = sum(nis_count_telefonica, na.rm = TRUE),
    PorcentajeNutriTel = (ADH_NutriTel / 34) * 100)

ggplot(df_v5, aes(x = center,
                  y = PorcentajeNutri))+
  geom_point(size = 3, alpha = 0.1, 
             col = "#8968CD")+
  geom_boxplot(alpha = 0) 

#-------------------------------------------------------------------------------
#Adherencia total

df_v5 <- df_v5 %>%
  mutate(
    AdherenciaTotal = rowMeans(
      cbind(PorcentajeActFis, PorcentajeCogni, PorcentajeSalud, PorcentajeNutri),
      na.rm = TRUE))

