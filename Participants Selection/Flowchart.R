#------------------------------FLOWCHART----------------------------------------
#¿Qué debería de tener?
#1. Screeneados
#2. Randomizados -> flexibles y sistemáticos.
#3. Evaluación basal.
#4. Inicio de intervención.
#5. Evaluación de 6 meses.
#6. Evaluación de 12 meses.
#7. Evaluación de 18 meses.
#8. Evaluación de 24 meses.
#9. Dropouts
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#Primero voy a calcular todo a mano para doble-check 
#-------------------------------------------------------------------------------

#¿Cuántos randomizados hay?
num_rdz <- df_v4 %>%
  filter(Eventos == "base") %>%
  group_by(Arm) %>%
  summarise(suma = sum(Randomization == "Yes", na.rm = TRUE))
num_rdz
#1111 randomizados
#551 flexibles y 560 sistemáticos

#¿Cuántos randomizados hay por centro?
num_rdz_center <- df_v4 %>%
  filter(Eventos == "base")%>%
  group_by(center) %>%
  summarise(suma = sum(Randomization == "Yes", na.rm = TRUE))
num_rdz_center

#Cantidad de evaluaciones
Cuentita <- df_v4 %>%
  mutate(Eventos = trimws(tolower(as.character(Eventos))),
         EvaluacionCompleta = as.integer(EvaluacionCompleta)) %>%
  filter(!is.na(Arm), Randomization == "Yes") %>%   
  group_by(Arm, Eventos) %>%
  summarise(
    n_completas = sum(EvaluacionCompleta == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Arm, Eventos)
View(Cuentita)

#¿Cuántos follow-up'
library(dplyr)

res_1follow <- df_flags %>%
  filter(Randomization == "Yes", !is.na(Arm)) %>%
  mutate(tiene_1_follow = base_complete & (m6_complete | m12_complete | m18_complete | m24_complete)) %>%
  group_by(Arm) %>%
  summarise(
    n_al_menos_1_follow = sum(tiene_1_follow, na.rm = TRUE),
    n_con_base          = sum(base_complete,  na.rm = TRUE),
    pct_sobre_base      = n_al_menos_1_follow / n_con_base,
    .groups = "drop"
  ) %>%
  arrange(Arm)

res_1follow

