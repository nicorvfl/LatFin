#-------------------------------------------------------------------------------
#                       ¿CUÁNTAS FILAS TIENE LA BASE?                            
#-------------------------------------------------------------------------------

dfcuenta <- df %>%
  filter(Eventos == "base")
nrow(dfcuenta)

#-------------------------------------------------------------------------------
#                   ¿QUÉ PASA CON LOS NO RANDOMIZADOS?
#-------------------------------------------------------------------------------

#Cuántos "NO" tenemos
dfRDZ1 <- df %>%
  filter(Eventos == "base", Randomization == "No")%>%
  summarise(
    cantidad = n()
  )
dfRDZ1

#Por qué no se randomizó.
dfRDZ <- df %>%
  filter(Randomization == "No", 
         Eventos == "scr")%>%
  select(id, center, Randomization,
         reason_rdz_scr)

View(dfRDZ)


#Quiénes tienen el dato incompleto
dfNAcount <- df %>%
  filter(Randomization == "No", Eventos == "scr") %>%
  group_by(center)%>%
  summarise(CantidadNA = sum(is.na(dfRDZ)))
dfNAcount

#-------------------------------------------------------------------------------
#                   ¿CUÁNTOS RANDOMIZADOS HAY?
#-------------------------------------------------------------------------------

RDZ <- df %>%
  filter(Eventos == "base")

#¿Cuántos randomizados hay?
num_rdz <- df %>%
  filter(Eventos == "base", TieneBase == TRUE) %>%
 group_by(Arm) %>%
  summarise(suma = sum(Randomization == "Yes", na.rm = TRUE))
num_rdz
#1105 randomizados
#543 flexibles y 556 sistemáticos

ggplot(RDZ, aes(x = center, fill = Randomization)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Randomization by center",
    subtitle = "(Randomization = Baseline)",
    x = "Randomization status",
    y = "Amount",
    caption = "7 November 2025"
  ) +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.9),
    vjust = -0.3,  
    size = 3
  )+
  scale_fill_manual(values = c("Yes" = "#D02090",
                               "No" = "#36648B"),
                    labels = c("Yes" = "Yes",
                               "No" = "No, screen failure"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 9))

#-------------------------------------------------------------------------------
#                   ¿CUÁNTOS SISTEMÁTICOS Y FLEXIBLES HAY?
#-------------------------------------------------------------------------------

GROUPS <- df %>%
  filter(Eventos == "base" & Randomization == "Yes")

ggplot(GROUPS, aes(x = center, fill = Arm)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Intervention Arms",
    subtitle= "Only RDZ participants",
    x = "Arm",
    y = "Participants") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.9),
    vjust = -0.3,  
    size = 3
  )+
  scale_fill_manual(values = c("Flexible" = "#9ACD32",
                               "Systematic" = "#5D478B"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 8))


#-------------------------------------------------------------------------------
#                   ¿QUIÉNES DEJARON ANTES DE INICIAR?
#-------------------------------------------------------------------------------

#rama flexible------------------------------------------------------------------
Randomizados <- dataset %>%
  filter(Eventos == "base",Randomization == "Yes" & 
           Arm == "Flexible")%>%
  summarise(Cantidad = n())
Randomizados

#cuántos dropouts antes de iniciar
FechaInicio <- df %>%
  filter(Eventos == "24m",Randomization == "Yes" & 
           Arm == "Systematic", IniciaIntervencion == 0)%>%
  select(id, Arm, EsDropout, FaseDropout, DropoutReason, TieneBase)
View(FechaInicio)

#Cuántos inician la intervención
IniciaFlex <- df %>%
  filter(Randomization == "Yes",
         Arm == "Flexible", Eventos == "base",
         IniciaIntervencion == 1)
View(IniciaFlex)

#rama sistemática---------------------------------------------------------------
#cuántos dropouts antes de iniciar la intervencion
FechaInicio <- df %>%
  filter(Eventos == "24m",Randomization == "Yes" & 
           Arm == "Systematic", IniciaIntervencion == 0)%>%
  select(id, Arm, EsDropout, FaseDropout, DropoutReason)
View(FechaInicio)

#Cuántos inician la intervención
IniciaFlex <- df %>%
  filter(Randomization == "Yes",
         Arm == "Systematic", Eventos == "24m",
         IniciaIntervencion == 1)
View(IniciaFlex)


#-------------------------------------------------------------------------------
#            ¿QUIÉNES SE FUERON DESPUÉS DE INICIAR LA INTERVENCIÓN?
#-------------------------------------------------------------------------------

#
Flex_Empezaron <- dataset %>%
  filter(Randomization == "Yes",
         Eventos == "24m",
         EsDropout == "Dropout") %>%
  distinct(id, center, Arm, IniciaIntervencion,DropoutReason)%>%
  filter(IniciaIntervencion == 1)%>%
  group_by(Arm)%>%
  select(id, Arm, DropoutReason, center)%>%
  filter(Arm == "Flexible")%>%
  group_by(DropoutReason)

Flex_Empezaron %>%
  summarise(cantidad = n())

88View(Flex_Empezaron)


dfDrop <- df %>%
  filter(Eventos == "24m",
         Randomization == "Yes",
         EsDropout == "Dropout")%>%
  mutate(
    IniciaIntervencion = case_when(
      IniciaIntervencion == 1 ~ "Started the intervention (n = 181)",
      IniciaIntervencion == 0 ~ "Did not start the intervention (n = 10)"
    )
  )

dfDrop_sum <- dfDrop %>%
  group_by(center, Arm, IniciaIntervencion) %>%
  summarise(EsDrop = n(), .groups = "drop")

ggplot(dfDrop_sum, aes(x = center, y = EsDrop, fill = Arm)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = EsDrop),
            position = position_dodge(width = 0.8),
            hjust = -0.15, vjust = 0.5, size = 3.5)+
  scale_fill_manual(values = c("Systematic" = "#5D478B",
                               "Flexible"  = "#9ACD32")) +
  labs(title = "Number of dropouts by center",
       subtitle = "Divided by Intervention Status",
       x = "center", y = "Nº dropouts", fill = "Arm") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10))+
  facet_wrap(~ IniciaIntervencion)+
  coord_flip()


#-------------------------------------------------------------------------------
#                           CANTIDAD DE EVALUACIONES
#-------------------------------------------------------------------------------

#Cantidad de evaluaciones
Cuentita <- df %>%
  filter(Randomization == "Yes", !is.na(Arm))%>%
  mutate(
    Eventos = trimws(tolower(as.character(Eventos))),
    EvaluacionCompleta = as.integer(EvaluacionCompleta50)
  ) %>%
  filter(!is.na(Arm), Randomization == "Yes") %>%
  group_by(Eventos, Arm) %>%
  summarise(
    n_completas    = sum(EvaluacionCompleta50 == 1, na.rm = TRUE),
    n_randomizados = n(),  
    .groups = "drop"
  ) %>%
  arrange(Eventos)

#Cuántas evaluaciones hay (eligiendo criterio 50%)
Cuentita <- Cuentita %>%
  mutate(
    Eventos = factor(Eventos, 
                     levels = c("scr","pre","base","6m",
                                "12m","18m","24m")))%>%
  filter(Eventos %in% c("base","6m",
                        "12m","18m","24m"))
View(Cuentita)

ggplot(Cuentita, aes(x = Eventos, y = n_completas,
                     fill = Arm))+
  geom_col(position = "dodge")+
  geom_text(aes(label = n_completas,
                group = Arm),
            position = position_dodge(width = 0.9),
            vjust = -0.25, size = 2)+
  scale_fill_manual(values = c("Systematic" = "#5D478B",
                               "Flexible"  = "#9ACD32"))+
  theme_bw()+
  labs(title = "Number of assessments by Event",
       subtitle = "RDZ = Baseline",
       x = "Events", y = "Number of completed assessments")+
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+
  facet_wrap(~center)



#-------------------------------------------------------------------------------
#                                DROPOUTS
#-------------------------------------------------------------------------------

#Cuántos dropouts hay?
dropouts <- df %>%
  filter(EsDropout == "Dropout",
         Randomization == "Yes",
         Eventos == "24m")%>%
  select(id, center, Arm, DropoutPhase, DropoutReason,
         TieneBase, Tiene6m, Tiene12m, Tiene18m, Tiene24m)
View(dropouts)

dropouts %>% summarise(
  cantidad = n(),
  cantidad_na = sum(is.na(DropoutPhase) | is.na(DropoutReason))) 
# 238 dropouts
# hasta ahora: 25 personas sin fase.

SinFase <- dropouts%>%
  filter(is.na(DropoutPhase))%>%
  select(id, center, DropoutPhase)%>%
  group_by(center)
SinFase



#-------------------------------------------------------------------------------

#Quiero ver dropouts antes de iniciar la intervención
Drop <- df %>%
  group_by(Arm)%>%
  filter(EsDropout == "Dropout", #Busco Dropouts
         Randomization == "Yes", #Que estén randomizados
         IniciaIntervencion == 0, #Que no hayan iniciado la intervencion
         Eventos == "24m")%>% #Controlo por evento
  select(id, center, Arm,
         IniciaIntervencion, EsDropout, DropoutPhase,
         DropoutReason, Tiene24m)

DropPre <- Drop %>% group_by(DropoutReason, Arm)%>%summarise(
  CantidadDrop = n())

View(DropPre)

#Quiero ver dropouts post iniciar la intervención
DropPost <- df %>%
  group_by(Arm)%>%
  filter(EsDropout == "Dropout", #Busco Dropouts
         Randomization == "Yes", #Que estén randomizados
         IniciaIntervencion == 1, #Que hayan iniciado la intervencion
         Eventos == "24m")%>% #Controlo por evento
  select(id, center, Arm,
         IniciaIntervencion, EsDropout, DropoutPhase,
         DropoutReason, Tiene24m)
View(DropPost)

DropPostStat <- DropPost %>% group_by(DropoutReason, Arm)%>%summarise(
  CantidadDrop = n())

View(DropPostStat)

DropPostNA <- DropPost %>%
  filter(is.na(DropoutReason))
View(DropPostNA)


#-------------------------------------------------------------------------------
#                        DUDAS A PARTIR DE ACÁ
#-------------------------------------------------------------------------------

dudas <- df %>%
  filter(IniciaIntervencion == 1,
         Eventos == "base")%>%
  select(id, center, Randomization, Arm,
         TieneBase, IniciaIntervencion, adher_0_6, EsDropout)
View(dudas)

dudas %>%
  summarise(
    Cantidad = sum(IniciaIntervencion),
    CantidadBase = sum(TieneBase)
  )

#Pregunta 1: 
#¿Todos los que tienen Baseline inician la intervención?

baseline <- df%>%
  filter(TieneBase== TRUE,
         IniciaIntervencion == 0,
         Eventos == "24m")%>%
  select(id, center, EsDropout, TieneBase,
         DropoutReason, IniciaIntervencion,
         adher_0_6, adher_6_12, adher_12_18,
         adher_18_24, Arm, gf_date, ef_date)
View(baseline)

#Pregunta 2
#¿Hay gente que inicia la intervención y no tiene baseline?

intervencion <- df %>%
  filter(IniciaIntervencion == 1,
         TieneBase == FALSE,
         Eventos == "24m")%>%
  select(id, center, EsDropout, TieneBase, Arm,
         IniciaIntervencion, DropoutReason,
         adher_0_6, adher_6_12, gf_date, ef_date)
View(intervencion)

#IMPONGO CONDICIÓN

mITT <- df %>%
  filter(IniciaIntervencion == 1,
         TieneBase == TRUE,
         Randomization == "Yes")

mITT %>% filter(Eventos == "base") %>% group_by(Arm) %>% 
  count(Arm)



#Cantidad de evaluaciones
Cuentita <- mITT %>%
  mutate(
    Eventos = trimws(tolower(as.character(Eventos))),
    EvaluacionCompleta = as.integer(EvaluacionCompleta50)
  ) %>%
  filter(!is.na(Arm), Randomization == "Yes") %>%
  group_by(Eventos, Arm) %>%
  summarise(
    n_completas    = sum(EvaluacionCompleta50 == 1, na.rm = TRUE),
    n_randomizados = n(),  
    .groups = "drop"
  ) %>%
  arrange(Eventos)

#Cuántas evaluaciones hay (eligiendo criterio 50%)
Cuentita <- Cuentita %>%
  mutate(
    Eventos = factor(Eventos, 
                     levels = c("scr","pre","base","6m",
                                "12m","18m","24m")))%>%
  filter(Eventos %in% c("base","6m",
                        "12m","18m","24m"))
View(Cuentita)

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
  labs(title = "Número de evaluaciones por evento",
       subtitle = "modify Intention to Treat (mITT)")



#Pregunta 3: Cuántos dropouts hay

dropouts <- mITT %>%
  filter(Eventos == "24m",
         EsDropout == "Dropout")%>%
  select(id, center, Arm, TieneBase, Tiene24m,
         adher_0_6, adher_18_24,
         EsDropout, DropoutReason, FaseDropout,
         DropoutPhase)
View(dropouts)



#Pregunta 4: 
#¿Quiénes son los que se quedan en el camino?

EnElCamino <- df %>%
  filter(Randomization == "Yes",
         IniciaIntervencion == 0,
         Eventos == "24m")%>%
  select(id, center, Arm,
         TieneBase, EsDropout, FaseDropout, DropoutReason)
View(EnElCamino)

ggplot(EnElCamino,
       aes(x = DropoutReason,
           fill = Arm)) +
  geom_bar(position = position_dodge(width = 0.9)) +
  labs(
    title = "Randomized but never started (n = 28)",
    x = "Dropout reason",
    y = "Number of participants"
  ) +
  facet_wrap(~ center, scales = "free_y") +  # opcional si querés ver por centro
  theme_bw()



Raros <- df %>%
  filter(EsDropout == "Dropout",
         Randomization == "Yes",
         Eventos == "24m", TieneBase == FALSE,
         IniciaIntervencion == 1,
         )%>%
  select(id, center, Arm, TieneBase,EsDropout, IniciaIntervencion)


View(Raros)
View(CuantosNoIniciaron)


#Randomizados final

datasetrdz <- dataset%>%filter(Eventos == "base")

ggplot(datasetrdz, aes(x = center, fill = Arm)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Flexible" = "#008B45",
                               "Systematic" = "#7D26CD"))+
  labs(
    title = "Randomized participants by center",
    subtitle = "m-Intention to Treat",
    x = "center",
    y = "Amount"
  ) +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.9),
    vjust = -0.3,  size = 3)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5))

View(mITT)

library(dplyr)
library(ggplot2)
library(tidyr)

dfDrop_sum <- mITT %>%
  filter(Eventos == "24m", EsDropout == "Dropout") %>%
  group_by(center, Arm) %>%
  summarise(EsDropout = n(), .groups = "drop")

# (Opcional) completar combinaciones faltantes con 0
dfDrop_sum <- dfDrop_sum %>%
  tidyr::complete(center, Arm, fill = list(EsDropout = 0))

# Gráfico
ggplot(dfDrop_sum, aes(x = center, y = EsDropout, fill = Arm)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = EsDropout),
            position = position_dodge(width = 0.9),
            vjust = -0.25, size = 3.5) +
  scale_fill_manual(values = c("Systematic" = "#D02090",
                               "Flexible"  = "#36648B")) +
  labs(title = "Dropouts by center",
       x = "Centro", y = "Nº de dropouts", fill = "Brazo") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



sindrop <- dataset %>%
  filter(Eventos == "24m", center == "Argentina",
         id == "312-74")%>%
  select(id, EsDropout, center, Tiene24m,TieneBase)
View(sindrop)



