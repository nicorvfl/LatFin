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
 # group_by(Arm) %>%
  summarise(suma = sum(Randomization == "Yes", na.rm = TRUE))
num_rdz
#1115 randomizados
#553 flexibles y 562 sistemáticos

#¿Cuántos randomizados hay por centro?
num_rdz_center <- df_v4 %>%
  filter(Eventos == "base")%>%
  group_by(center) %>%
  summarise(suma = sum(Randomization == "Yes", na.rm = TRUE))
num_rdz_center

#¿Quiénes son esos 5 con NA en rdz? O:
null_rdz <- df_v4 %>%
  filter(Eventos == "base", is.na(Arm),
         Randomization == "Yes")%>%
  select(record_id, center, Arm)
null_rdz  
#Los voy a imputar porque sé que no fueron randomizados
df_v4 <- df_v4 %>%
  mutate(
    Randomization = if_else(record_id %in% null_rdz$record_id,
                            "No", Randomization))

#¿Cuántas evaluaciones hay?
NumEv <- CantidadEvaluaciones(df_v4)
NumEv <- NumEv %>%
  filter(Eventos != c("pre","scr"))
library(reshape2)
View(NumEv)

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

#¿Cuántos dropouts hay?
Dropouts <- df_v4 %>%
  summarise(
    drop = rowSums(dropout_phase)
  )

table(df_v4$dropout_phase)

#-------------------------------------------------------------------------------
#Flowchart 
#-------------------------------------------------------------------------------

#install.packages("flowchart")
library(flowchart)

#1)Randomización
df_fc <- df_v4 %>% filter(Eventos == "base")

rdz_fc <- df_fc %>%
  as_fc(label = "Participants assessed for eligibility") %>%
  fc_filter(Randomization == "Yes", label = "Randomized", show_exc = TRUE) %>%
  fc_split(Arm, na.rm = TRUE)

#2) Iniciaron intervención
Intervencion <- c("Systematic", "Flexible")
Cantidad     <- c(539, 526)

interv_fc <- as_fc(
  N = sum(Cantidad),
  label = "Participants who started intervention"
) %>%
  fc_split(N = Cantidad, label = Intervencion)

#3) Dropouts por fase
df_drop <- df_v4 %>%
  filter(!is.na(dropout_phase)) %>%
  mutate(
    dropout_phase = factor(
      dropout_phase, levels = 0:4,
      labels = c("Pre-intervention","Months 1–6","Months 7–12","Months 13–18","Months 19–24")
    )
  )

drop_fc <- df_drop %>%
  as_fc(label = "Participants who dropped out (by phase)") %>%
  fc_split(dropout_phase, na.rm = TRUE)


list(rdz_fc, interv_fc, drop_fc) %>%
  fc_stack(unite = TRUE) %>%
  fc_draw()%>%
  fc_export("flowchart.png", width = 3000, height = 4000, res = 700)

