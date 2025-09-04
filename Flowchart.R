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


#-------------------------------------------------------------------------------

library(dplyr)
library(reshape2)
library(ggplot2)

dfCuenta <- df_flags %>%
  select(Arm, base_complete, m6_complete,
         m12_complete, m18_complete, m24_complete) %>%
  melt(id.var = "Arm") %>%
  group_by(Arm, variable) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop")

conteo <- ggplot(dfCuenta, aes(x = variable, y = total, fill = Arm)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Evento", y = "Evaluaciones completas") +
  theme_minimal()

ggsave("Participants Selection/conteo.png",
       plot = conteo, width = 8, height = 6, dpi = 300)



