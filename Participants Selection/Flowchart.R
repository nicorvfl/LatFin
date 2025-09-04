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
#Primero voy a calcular todo a mano para doble-check y después 
#usamos una librería que lo hace solo.
#-------------------------------------------------------------------------------

#¿Cuántos randomizados hay?
num_rdz <- df_v4 %>%
  filter(Eventos == "base") %>%
  #group_by(Arm) %>%
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

#¿Cuántas evaluaciones hay?
NumEv <- CantidadEvaluaciones(df_v4)
NumEv <- NumEv %>%
  filter(Eventos != c("pre","scr"))
View(NumEv)

library(reshape2)

#baseline (1079), 6m (938), 12m (879), 18m (829), 24m (852)

#¿Cuántas personas iniciaron la intervención?
InicioIntervencionSis <- df_bruto %>%
  summarise(
    numero = sum(!is.na((ef_date_base))))
InicioIntervencionSis #539

InicioIntervencionFlex <- df_bruto %>%
  summarise(
    numero = sum(!is.na((gf_date_base))))
InicioIntervencionFlex #526

#-------------------------------------------------------------------------------
#Flowchart 
#-------------------------------------------------------------------------------

#install.packages("flowchart")
library(flowchart)

# -------------------------
# Parte inicial (baseline)
# -------------------------

df_flags <- df_v4 %>%
  mutate(Eventos = trimws(tolower(as.character(Eventos))),
         EvaluacionCompleta = as.integer(EvaluacionCompleta)) %>%
  group_by(record_id, Randomization, Arm) %>%
  summarise(
    base_complete = any(Eventos == "base" & EvaluacionCompleta == 1, na.rm=TRUE),
    m6_complete   = any(Eventos == "6m"   & EvaluacionCompleta == 1, na.rm=TRUE),
    m12_complete  = any(Eventos == "12m"  & EvaluacionCompleta == 1, na.rm=TRUE),
    m18_complete  = any(Eventos == "18m"  & EvaluacionCompleta == 1, na.rm=TRUE),
    m24_complete  = any(Eventos == "24m"  & EvaluacionCompleta == 1, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    TieneSeguimiento = base_complete & (m6_complete | m12_complete | m18_complete | m24_complete),
    upto6  = base_complete | m6_complete,
    upto12 = base_complete | m6_complete | m12_complete,
    upto18 = base_complete | m6_complete | m12_complete | m18_complete,
    upto24 = base_complete | m6_complete | m12_complete | m18_complete | m24_complete
  )

Flowchart <- df_flags %>%
  as_fc(label = "Participants assessed by eligibility") %>%
  fc_filter(Randomization == "Yes" & !is.na(Arm),
            label = "Randomized Participants", show_exc = TRUE,
            text_pattern = "{label}\n n = {n}") %>%
  fc_split(Arm, text_pattern = "{label}\n n = {n}") %>%
  fc_filter(base_complete, label = "Baseline assessment",
            show_exc = FALSE, text_pattern = "{label}\n n = {n}") %>%
  fc_filter(TieneSeguimiento, label = "Baseline + ≥1 follow-up complete",
            show_exc = FALSE, text_pattern = "{label}\n n = {n}") %>%
  fc_filter(upto6,  label = "6-month assessment",  show_exc = FALSE, text_pattern = "{label}\n n = {n}") %>%
  fc_filter(upto12, label = "12-month assessment", show_exc = FALSE, text_pattern = "{label}\n n = {n}") %>%
  fc_filter(upto18, label = "18-month assessment", show_exc = FALSE, text_pattern = "{label}\n n = {n}") %>%
  fc_filter(upto24, label = "24-month assessment", show_exc = FALSE, text_pattern = "{label}\n n = {n}") %>%
  fc_draw() %>%
  fc_export("FlowChart.png", width = 2900, height = 4000, res = 380)
