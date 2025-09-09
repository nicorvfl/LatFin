#Primero, definamos dropout.

df_v4 <- df_v4 %>%
  mutate(
    #Tendría que ser un condicional
    Dropout = if_else(!is.na(dropout_phase) | !is.na(dropout_reason),
                      "Dropout", "No Dropout"),
    Dropout = as.factor(Dropout),
    DropoutFase = case_when(
      dropout_phase == 0 ~ "Between RDZ & Baseline",
      dropout_phase == 1 ~ "Between 1-6 months",
      dropout_phase == 2 ~ "Between 7-12 months",
      dropout_phase == 3 ~ "Between 13-18 months",
      dropout_phase == 4 ~ "Between 19-24 months"
    ),
    DropoutFase = factor(DropoutFase,
                         levels = c("Between RDZ & Baseline","Between 1-6 months",
                         "Between 7-12 months","Between 13-18 months",
                         "Between 19-24 months")))

DropGraf <- df_v4 %>%
  filter(Dropout == "Dropout")

#Vamos a analizar los dropouts por centro
library(ggplot2)
DropByCenter <- ggplot(DropGraf, aes(x = center, fill = DropoutFase))+
  geom_bar(stat = "count", position = "dodge")+
  theme_light()+
  labs(title = "Dropouts by center")

ggsave("Dropout/Dropouts por centro.png",
       plot = DropByCenter, width = 15, height = 6, dpi = 300, bg = "white")

Tabla <- DropGraf %>%
  group_by(center) %>%
  summarise(
    DropoutsCantidad = sum(!is.na(dropout_phase), na.rm = TRUE),
    .groups = "drop"
  )
Tabla

library(gt)
TablaDrop <- Tabla %>%
  gt() %>%
  fmt_number(columns = DropoutsCantidad, decimals = 0) %>%
  tab_header(
    title = "Cantidad de Dropouts por centro")
TablaDrop
gtsave(TablaDrop, "Dropout/Tabla de cantidad de dropouts por centro.html")

#Ahora por rama de intervención

DropoutByArm <- ggplot(DropGraf, aes(x = center, fill = Arm)) +
  geom_bar(position = "dodge") +
  theme_light() +
  labs(title = "Dropouts by arm and center", 
       x = "Center", y = "Dropouts", fill = "Arm")
ggsave("Dropout/Dropouts por Intervencion.png",
       plot = DropoutByArm, width = 15, height = 6, dpi = 300, bg = "white")


#Un mini testeo
dfChi <- df_v4 %>%
  filter(Eventos == "24m")
tabla <- table(dfChi$Arm, dfChi$Dropout)
print(tabla)
chi_res <- chisq.test(tabla)
chi_res
chi_res$expected

#log
# Aseguramos que Dropout sea binaria 0/1
df_v4$Dropout_bin <- ifelse(df_v4$Dropout == "Dropout", 1, 0)
modelodrop <- glm(Dropout_bin ~ Arm, data = df_v4, family = binomial)
summary(modelodrop)

