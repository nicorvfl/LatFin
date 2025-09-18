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
  group_by(center, Arm) %>%
  summarise(suma = sum(Randomization == "Yes", na.rm = TRUE))
View(num_rdz_center)


#Cantidad de evaluaciones
Cuentita <- df_v4 %>%
  mutate(Eventos = trimws(tolower(as.character(Eventos))),
         EvaluacionCompleta = as.integer(EvaluacionCompleta50)) %>%
  filter(!is.na(Arm), Randomization == "Yes") %>%   
  group_by(Arm, Eventos) %>%
  summarise(
    n_completas = sum(EvaluacionCompleta50 == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Arm, Eventos)

ggplot(Cuentita, aes(x = Eventos,
                     y = n_completas,
                     fill = Arm))+
  geom_col(position = "dodge")

#¿Cuántos follow-up'
res_1follow <- df_flags %>%
  filter(Randomization == "Yes", !is.na(Arm)) %>%
  mutate(tiene_1_follow = base_complete & (m6_complete | m12_complete | m18_complete | m24_complete),
         no_tiene_baseline = !base_complete & (m6_complete | m12_complete | m18_complete | m24_complete)) %>%
  group_by(Arm) %>%
  summarise(
    n_al_menos_1_follow = sum(tiene_1_follow, na.rm = TRUE),
    n_con_base          = sum(base_complete,  na.rm = TRUE),
    pct_sobre_base      = n_al_menos_1_follow / n_con_base,
    n_sin_base          = sum(no_tiene_baseline),
    .groups = "drop"
  ) %>%
  arrange(Arm)

View(res_1follow)

#¿Cuántos han iniciado la intervención?
Intervention <- df_v4 %>%
  filter(Eventos == "base",
         Randomization == "Yes") %>%
  group_by(Arm)%>%
  mutate(
    Started = if_else(!is.na(ef_date) | !is.na(gf_date), 1, 0)
  ) %>%
  filter(Started == 1)%>%
  summarise(
    FinallyStarted = n()
  )

Intervention #1065

InterventionStarted <- ggplot(Intervention, aes(x = center, y = FinallyStarted,
                         fill = Arm))+
  geom_col(position = "dodge")+
  theme_light()

ggsave("Participants Selection/InterventionStarted.png",
       plot = InterventionStarted,
       width = 13, height = 6, units = "in",
       bg = "white")

#Cuántos quedan seleccionados según criterio de evaluaciones
conteos <- df_v4 %>%
  summarise(
    `>=3 tests` = sum(EvaluacionCompleta3, na.rm = TRUE),
    `>=50% tests` = sum(EvaluacionCompleta50, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = "Criterio",
                      values_to = "Cantidad")

cantidadseguncriterio <- ggplot(conteos, aes(x = Criterio, y = Cantidad, fill = Criterio)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = Cantidad), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("#1F77B4", "#D62728")) + # azul fuerte y rojo fuerte
  labs(title = "Comparación de evaluaciones completas según criterio",
       y = "Cantidad de evaluaciones",
       x = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave("Participants Selection/cantidadseguncriterio.png",
       plot = cantidadseguncriterio,
       width = 13, height = 6, units = "in",
       bg = "white")

#Cuántas evaluaciones hay (eligiendo criterio 50%)
Cuentita <- Cuentita %>%
  mutate(
    Eventos = factor(Eventos,
                     levels = c("pre","scr","base",
                         "6m","12m","18m","24m")))

CantidadEv <- ggplot(Cuentita, aes(x = Eventos,
                     y = n_completas, fill = Arm)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.9) +
  scale_fill_manual(values = c("#1F77B4", "#D62728")) +
  geom_text(aes(label = n_completas),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 3.5) +
  labs(x = NULL, y = "Completed assessments",
       fill = "Arm") +
  theme_light()

ggsave("Participants Selection/CantidadEv.png",
       plot = CantidadEv,
       width = 10, height = 6, units = "in",
       bg = "white")
