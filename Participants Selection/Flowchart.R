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
#1105 randomizados
#546 flexibles y 559 sistemáticos

#¿Cuántos randomizados hay por centro?
num_rdz_center <- df_v4 %>%
  filter(Eventos == "base")%>%
  group_by(center, Arm) %>%
  summarise(suma = sum(Randomization == "Yes", na.rm = TRUE))
View(num_rdz_center)


#Cantidad de evaluaciones
Cuentita <- df_v4 %>%
  mutate(
    Eventos = trimws(tolower(as.character(Eventos))),
    EvaluacionCompleta = as.integer(EvaluacionCompleta50)
  ) %>%
  filter(!is.na(Arm), Randomization == "Yes") %>%
  group_by(Eventos, center) %>%
  summarise(
    n_completas    = sum(EvaluacionCompleta50 == 1, na.rm = TRUE),
    n_randomizados = n(),  
    .groups = "drop"
  ) %>%
  arrange(Eventos) %>%
  filter(Eventos == "base")

View(Cuentita)

library(dplyr)

# IDs que no cumplen criterio en baseline
ids_no_completas <- df_v4 %>%
  mutate(
    Eventos = trimws(tolower(as.character(Eventos))),
    EvaluacionCompleta = as.integer(EvaluacionCompleta50)
  ) %>%
  filter(Eventos == "base",
         !is.na(Arm),
         Randomization == "Yes",
         EvaluacionCompleta == 0) %>%
  select(record_id, center, Arm, EvaluacionCompleta)

# Traer la columna dropout_phase desde el evento 24m
dropout_24m <- df_v4 %>%
  mutate(Eventos = trimws(tolower(as.character(Eventos)))) %>%
  filter(Eventos == "24m") %>%
  select(record_id, dropout_phase)

# Unir ambas tablas
ids_no_completas <- ids_no_completas %>%
  left_join(dropout_24m, by = "record_id")

View(ids_no_completas)

Cuentita_long <- Cuentita %>%
  pivot_longer(cols = c(n_completas, n_randomizados),
               names_to = "Metrica",
               values_to = "Cantidad")

ggplot(Cuentita_long, aes(x = center, y = Cantidad, fill = Metrica)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = Cantidad),
            position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("n_completas" = "#1F77B4",
                               "n_randomizados" = "#D62728"),
                    labels = c("Evaluaciones completas", "Randomizados")) +
  labs(x = "Centro", y = "Cantidad",
       title = "evaluacion completa vs rdz",
       fill = "") +
  theme_light(base_size = 12) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


View(Cuentita)
#-------------------------------------------------------------------------------
#Interludio para mirar dropouts

dropout <- df_v4 %>%
  filter(!is.na(dropout_phase))%>%
  select(record_id, center, Randomization,
         dropout_phase, dropout_reason)%>%
  filter(dropout_phase == 0,
         dropout_reason == 2,
         Randomization == "Yes")

View(dropout)

#-------------------------------------------------------------------------------

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

ids_sin_base <- df_flags %>%
  filter(Randomization == "Yes", !is.na(Arm)) %>%
  mutate(no_tiene_baseline = !base_complete & (m6_complete | m12_complete | m18_complete | m24_complete)) %>%
  filter(no_tiene_baseline) %>%
  select(record_id, Arm, base_complete, m6_complete, m12_complete, m18_complete, m24_complete)

ids_sin_base

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
    FinallyStarted = n())

Intervention #1065

InterventionStarted <- ggplot(Intervention, aes(x = center, y = FinallyStarted,
                         fill = Arm))+
  geom_col(position = "dodge")+
  theme_light()

InterventionStarted

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

cantidadseguncriterio
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



