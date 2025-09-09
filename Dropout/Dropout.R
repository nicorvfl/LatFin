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

library(ggplot2)
ggplot(DropGraf, aes(x = center, fill = DropoutFase))+
  geom_bar(stat = "count", position = "dodge")+
  theme_light()+
  labs(title = "Dropouts by center")

