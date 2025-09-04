library(ggplot2)

#Hombres y mujeres según brazo de intervención

Baseline <- df_v4 %>%
  filter(Eventos == "base",
         Randomization == "Yes")

Sex <- ggplot(Baseline, aes(x = Sex, fill = Arm)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.9)) +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            position = position_dodge(width = 0.9),
            vjust = -0.3) +
  labs(title = "Sex Distribution across Arms")+
  theme_light()

ggsave("Sociodemograficos/DistSex.png",
       plot = Sex, width = 8, height = 6, dpi = 300,
       bg = "white")


#Cómo es la educación a lo largo de los centros?

Education <- ggplot(Baseline, aes(x = center,
                                  y = education_years,
                                  color = Arm))+
  geom_point(size = 2, position = position_jitter(width = 0.2,
                                        height = 0.2),
             alpha = 0.5)+
  geom_boxplot(alpha = 0)+
  labs(title = "Education years across centers")+
  theme_light()

ggsave("Sociodemograficos/DistEduc.png",
       plot = Education, width = 14, height = 6, dpi = 300,
       bg = "white")
