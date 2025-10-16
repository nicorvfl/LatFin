#-------------------------------------------------------------------------------
#                       ¿CUÁNTAS FILAS TIENE LA BASE?                            
#-------------------------------------------------------------------------------

df_v4_filas <- df %>%
  filter(Eventos == "base")
nrow(df_v4_filas)

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
  select(record_id, center, Randomization,
         reason_rdz_scr)

View(dfRDZ)


#Quiénes tienen el dato incompleto
dfNAcount <- df %>%
  filter(Randomization == "No", Eventos == "scr") %>%
  group_by(center)%>%
  summarise(CantidadNA = sum(is.na(RDZReason)))
dfNAcount

#-------------------------------------------------------------------------------
#                   ¿CUÁNTOS RANDOMIZADOS HAY?
#-------------------------------------------------------------------------------

RDZ <- df %>%
  filter(Eventos == "base")

#¿Cuántos randomizados hay?
num_rdz <- df %>%
  filter(Eventos == "base") %>%
 # group_by(Arm) %>%
  summarise(suma = sum(Randomization == "Yes", na.rm = TRUE))
num_rdz
#1105 randomizados
#546 flexibles y 559 sistemáticos

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

GROUPS <- df %>%
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


#-------------------------------------------------------------------------------
#                   ¿QUIÉNES DEJARON ANTES DE INICIAR?
#-------------------------------------------------------------------------------

#randomizados en flex
Randomizados <- df %>%
  filter(Eventos == "base",Randomization == "Yes" & 
           Arm == "Flexible")%>%
  summarise(Cantidad = n())
Randomizados

#cuántos tienen fecha de inicio
FechaInicio <- df %>%
  filter(Eventos == "base",Randomization == "Yes" & 
           Arm == "Flexible", IniciaIntervencion == 0)%>%
  select(record_id, Arm, EsDropout, dropout_phase)
View(FechaInicio)

SinFecha <- df%>%
  filter(Randomization == "Yes" & Eventos =="base" &
           Arm == "Flexible", dropout_phase == 0
         )%>%
  select(record_id, center, gf_date,
         EsDropout, dropout_phase,Eventos, IniciaIntervencion)

View(SinFecha)

drop <- df %>%
  filter(Randomization == "Yes" & Eventos == "base" &
           Arm == "Flexible")%>%
  group_by(dropout_phase)%>%
  summarise(cantidad = n())
View(drop)

dftest <- df %>%
  filter(Eventos == "base", Randomization == "Yes")%>%
  group_by(Arm)

table(dftest$IniciaIntervencion, dftest$Arm)

Iniciaron <- df %>%
  filter(Eventos == "base", IniciaIntervencion == 1)

SinIniciaFlex <- df %>%
  filter(is.na(gf_date),
         Randomization == "Yes", Arm == "Flexible",
         Eventos == "24m")%>%
  select(record_id, center, Randomization,
         Arm, EsDropout, dropout_phase)

SinIniciaFlex %>%
  summarise(
    Cantidad = sum(dropout_phase == 0, na.rm = TRUE)
  )


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


CuantosNoIniciaron <- df %>%
  group_by(Arm)%>%
  filter(Eventos == "base",
         Randomization == "Yes")%>%
  filter(IniciaIntervencion == 0)%>%
  count()

CuantosNoIniciaron  

#-------------------------------------------------------------------------------
#            ¿QUIÉNES SE FUERON ANTES DE INICIAR LA INTERVENCIÓN?
#-------------------------------------------------------------------------------

Flex_Started <- df %>%
  filter(Randomization == "Yes",
         Eventos == "24m",
         EsDropout == "Dropout") %>%
  distinct(record_id, center, Arm, IniciaIntervencion,DropoutReason)%>%
  filter(IniciaIntervencion == 1)%>%
  group_by(Arm)%>%
  select(record_id, Arm, DropoutReason, center)%>%
  filter(Arm == "Systematic")%>%
  group_by(DropoutReason)

Flex_Started %>%
  summarise(cantidad = n())

View(Flex_Started)

dfdrop <- df%>%
  filter(Eventos == "24m",
         EsDropout == "Dropout",
         Randomization == "Yes")%>%
  select(record_id, EsDropout)%>%
  summarise(cantidad = n())
View(dfdrop)

dfDrop <- df %>%
  filter(Eventos == "24m",
         Randomization == "Yes",
         EsDropout == "Dropout")


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
Cuentita <- df %>%
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
  labs(title = "Número de evaluaciones por evento")   



#-------------------------------------------------------------------------------
#                                DROPOUTS
#-------------------------------------------------------------------------------

#Cuántos dropouts hay?
dropouts <- df %>%
  filter(EsDropout == "Dropout",
         Randomization == "Yes",
         Eventos == "24m")%>%
  select(record_id, center, Arm, DropoutPhase, DropoutReason,
         TieneBase, Tiene6m, Tiene12m, Tiene18m, Tiene24m)
View(dropouts)

dropouts %>% summarise(
  cantidad = n(),
  cantidad_na = sum(is.na(DropoutPhase) | is.na(DropoutReason))) 
# 238 dropouts
# hasta ahora: 25 personas sin fase.

SinFase <- dropouts%>%
  filter(is.na(DropoutPhase))%>%
  select(record_id, center, DropoutPhase)%>%
  group_by(center)%>%
  summarise(cantidad = n())
SinFase







