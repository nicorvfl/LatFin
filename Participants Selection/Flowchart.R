#-------------------------------------------------------------------------------
#                       ¿CUÁNTAS FILAS TIENE LA BASE?                            
#-------------------------------------------------------------------------------

df_v4_filas <- df_v4 %>%
  filter(Eventos == "base")
nrow(df_v4_filas)

#-------------------------------------------------------------------------------
#                   ¿CUÁNTOS RANDOMIZADOS HAY?
#-------------------------------------------------------------------------------

<<<<<<< HEAD
RDZ <- df_v4 %>%
  filter(Eventos == "base")
=======
#¿Cuántos randomizados hay?
num_rdz <- df_v4 %>%
  filter(Eventos == "base") %>%
  group_by(Arm) %>%
  summarise(suma = sum(Randomization == "Yes", na.rm = TRUE))
num_rdz
#1105 randomizados
#546 flexibles y 559 sistemáticos
>>>>>>> a60aaecf5147cc5ad39bfc8672784001803e76ec

ggplot(RDZ, aes(x = center, fill = Randomization)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Randomizados por centro",
    subtitle = "Excluyendo los mal-incluídos",
    x = "Randomización",
    y = "Cantidad"
  ) +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.9),
    vjust = -0.3,  
    size = 3
  )+
  scale_fill_manual(values = c("Yes" = "#D02090",
                               "No" = "#36648B"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------
#                   ¿CUÁNTOS SISTEMÁTICOS Y FLEXIBLES HAY?
#-------------------------------------------------------------------------------

GROUPS <- df_v4 %>%
  filter(Eventos == "base" & Randomization == "Yes")

ggplot(GROUPS, aes(x = center, fill = Arm)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Asignación de tipo de intervención",
    
    x = "Brazos",
    y = "Cantidad") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.9),
    vjust = -0.3,  
    size = 3
  )+
  scale_fill_manual(values = c("Flexible" = "#008B45",
                               "Systematic" = "#7D26CD"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
theme_replace()


#-------------------------------------------------------------------------------
#                   ¿QUIÉNES DEJARON ANTES DE INICIAR?
#-------------------------------------------------------------------------------


Iniciaron <- df_v4 %>%
  filter(Eventos == "base", IniciaIntervencion == 1)

ggplot(Iniciaron, aes(x = center, fill = Arm)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Flexible" = "#008B45",
                               "Systematic" = "#7D26CD"))+
  labs(
    title = "¿Cuántos iniciaron la intervención por centro?",
    x = "Centro",
    y = "Cantidad"
  ) +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.9),
    vjust = -0.3,  size = 3)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))


CuantosIniciaron <- df_v4 %>%
  group_by(Arm)%>%
  filter(Eventos == "base",
         Randomization == "Yes")%>%
  filter(IniciaIntervencion == 0)%>%
  count()

CuantosIniciaron  

#-------------------------------------------------------------------------------
#            ¿QUIÉNES SE FUERON ANTES DE INICIAR LA INTERVENCIÓN?
#-------------------------------------------------------------------------------

Flex_Started <- df_v4 %>%
  filter(Randomization == "Yes",
         Arm == "Flexible",
         Eventos == "base",
         IniciaIntervencion == 1) %>%
  distinct(record_id, center, Arm)

Dropout24m_form_Flex <- df_v4 %>%
  filter(Eventos == "24m",
         Arm == "Flexible",
         record_id %in% Flex_Started$record_id) %>%
  group_by(record_id) %>%
  summarise(
    dropout_phase  = first(na.omit(dropout_phase)),
    dropout_reason = first(na.omit(dropout_reason)),
    .groups = "drop"
  )

FlexStarted_con_form <- Flex_Started %>%
  left_join(Dropout24m_form_Flex, by = "record_id") %>%
  mutate(
    tiene_formulario = !is.na(dropout_phase) | !is.na(dropout_reason),
    form_status2     = if_else(tiene_formulario, "con_formulario", "sin_formulario"),
    phase_label = case_when(
      dropout_phase == 0 ~ "before_start",
      dropout_phase == 1 ~ "during_intervention",
      dropout_phase == 2 ~ "post_intervention",
      TRUE ~ NA_character_
    )
  )

FlexStarted_con_form %>% count(form_status2, sort = TRUE)

Flex_con_form <- FlexStarted_con_form %>%
  filter(tiene_formulario) %>%
  select(record_id, center, dropout_phase, phase_label, dropout_reason) %>%
  arrange(center, record_id)
View(Flex_con_form)


dfDrop <- df_v4 %>%
  filter(Eventos == "24m",
         Randomization == "Yes")%>%
  mutate(
    EsDrop = if_else(!is.na(dropout_phase), 1, 0)
  )


ggplot(dfDrop, aes(x = Arm, fill = factor(IniciaIntervencion))) +
  geom_bar(position = position_dodge(width = 0.9)) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.25, size = 3.5
  ) +
  scale_fill_manual(values = c("1" = "#D02090",
                               "0" = "#36648B"))+
  labs(
    title = "Inicio de intervención por brazo",
    fill  = "Inició (0/1)", x = "Brazo", y = "Cantidad"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()

library(dplyr)
library(ggplot2)

dfDrop_sum <- dfDrop %>%
  group_by(center, Arm) %>%
  summarise(EsDrop = sum(EsDrop == 1, na.rm = TRUE), .groups = "drop")

ggplot(dfDrop_sum, aes(x = center, y = EsDrop, fill = Arm)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = EsDrop),
            position = position_dodge(width = 0.9),
            vjust = -0.25, size = 3.5) +
  scale_fill_manual(values = c("Systematic" = "#D02090",
                               "Flexible"  = "#36648B")) +
  labs(title = "Dropouts por centro",
       x = "Centro", y = "Nº de dropouts", fill = "Brazo") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


#-------------------------------------------------------------------------------
#                           CANTIDAD DE EVALUACIONES
#-------------------------------------------------------------------------------

#Cantidad de evaluaciones
Cuentita <- df_v4 %>%
<<<<<<< HEAD
=======
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
>>>>>>> a60aaecf5147cc5ad39bfc8672784001803e76ec
  mutate(
    Eventos = trimws(tolower(as.character(Eventos))),
    EvaluacionCompleta = as.integer(EvaluacionCompleta50)
  ) %>%
  filter(!is.na(Arm), Randomization == "Yes") %>%
  group_by(Eventos, Arm) %>%
  summarise(
<<<<<<< HEAD
    n_completas    = sum(EvaluacionCompleta50 == 1, na.rm = TRUE),
    n_randomizados = n(),  
    .groups = "drop"
=======
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
>>>>>>> a60aaecf5147cc5ad39bfc8672784001803e76ec
  ) %>%
  arrange(Eventos, Arm)
View(Cuentita)

<<<<<<< HEAD
=======
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
>>>>>>> a60aaecf5147cc5ad39bfc8672784001803e76ec
Cuentita <- Cuentita %>%
  mutate(
    Eventos = factor(Eventos, 
                     levels = c("scr","pre","base","6m",
                                "12m","18m","24m")))

View(Cuentita)

<<<<<<< HEAD
ggplot(Cuentita, aes(x = Eventos, y = n_completas,
                     fill = Arm))+
  geom_col(position = "dodge")+
  geom_text(aes(label = n_completas,
                group = Arm),
            position = position_dodge(width = 0.9),
            vjust = -0.25)+
  scale_fill_manual(values = c("Flexible" = "#EEB422",
                               "Systematic" = "#008B00"))+
  theme_bw()+
  labs(title = "Número de evaluaciones por evento")                     
=======
ggsave("Participants Selection/CantidadEv.png",
       plot = CantidadEv,
       width = 10, height = 6, units = "in",
       bg = "white")



>>>>>>> a60aaecf5147cc5ad39bfc8672784001803e76ec
