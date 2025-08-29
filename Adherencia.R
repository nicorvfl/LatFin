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

df_v5 <- df_v4 %>%
  group_by(record_id)%>%
  mutate(
    ADH_ActFis = sum(ef_count, na.rm = TRUE), #suma total
    PorcentajeActFis = (ADH_ActFis / 416) * 100) #porcentaje

#-------------------------------------------------------------------------------
#Actividad cognitiva

#La variable se llama: ec_count
#728 sesiones posibles (52 * 7) * 2

df_v5 <- df_v5 %>%
  group_by(record_id)%>%
  mutate(
    ADH_Cogni = sum(ec_count, na.rm = TRUE), #suma total
    PorcentajeCogni = (ADH_Cogni / 728) * 100) #porcentaje

#-------------------------------------------------------------------------------
#Control cardiovascular
df_v5 <- df_v5 %>%
  group_by(record_id)%>%
  mutate(
    ADH_Salud = rowSums(
      across(c(mgr_5grupal1, mgr_5grupal2, mgr_6grupal1, mgr_6grupal2, 
               mgr_7grupal1, mgr_8grupal1, mgr_9grupal1, mgr_10grupal1, 
               mgr_11grupal1, mgr_12grupal1)),
      na.rm = TRUE),
    PorcentajeSalud = (ADH_Salud / 20) * 100)

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

#-------------------------------------------------------------------------------
#Adherencia total

df_v5 <- df_v5 %>%
  mutate(
    AdherenciaTotal = rowMeans(
      cbind(PorcentajeActFis, PorcentajeCogni, PorcentajeSalud, PorcentajeNutri),
      na.rm = TRUE))


