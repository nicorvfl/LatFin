#-------------------------------------------------------------------------------
#                        CRITERIOS DE ELEGIBILIDAD
#-------------------------------------------------------------------------------

#Me voy a armar un dataframe con estos 614.
Renegados <- df %>%
  filter(Randomization == "No", Eventos == "pre")%>%
  select(record_id, center, crit_ex1_scr, crit_ex2_scr, crit_ex3_scr,
         crit_ex4_scr,crit_ex5_scr,crit_ex6_scr,crit_ex7_scr,crit_ex8_scr,
         crit_ex9_scr,crit_ex10_scr,crit_ex11_scr,crit_ex12_scr,crit_ex13_scr,
         crit_ex14_scr,crit_ex15_scr,crit_ex16_scr,crit_ex17_scr,crit_ex18_scr,
         crit_ex19_scr,crit_ex20_scr,crit_ex21_scr,crit_ex22_scr,crit_ex23_scr,
         crit_in1_scr, crit_in2_scr, crit_in5_scr,
         crit_in3_scr, crit_in4_scr,crit_ex5_scr,
         crit_in6_scr, reason_rdz_scr, reason_not_rdz_scr,
         crit_in1_pre:crit_in6_pre, crit_ex1_pre:crit_ex23_pre,
         age_pre, total_caide, education_years)

#¿Quién se va por su propia cuenta?
Abandonadores <- Renegados %>%
  filter(reason_not_rdz_scr == 1)%>%
  select(record_id, reason_not_rdz_scr)
View(Abandonadores)

#¿A quién echamos?
Echados <- Renegados %>%
  filter(reason_not_rdz_scr %in% c(2, 3) | is.na(reason_not_rdz_scr))
View(Echados)

#-------------------------------------------------------------------------------
#                                ECHADOS
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                           SE DIVIDEN LAS AGUAS
#-------------------------------------------------------------------------------

#Ok, ahora empieza el baile.
#Tengo que colocar ese dato en los id.

# Lista 1: Solo Cognitivos = 1
ids_cognitivos <- c(
  "312-100","312-105","312-106","312-111","312-114","312-120","312-130",
  "312-133","312-134","312-137","312-139","312-140","312-144","312-154",
  "312-156","312-161","312-163","312-164","312-165","312-166","312-174",
  "312-186","312-188","312-191","312-192","312-193","312-201","312-214",
  "312-217","312-218","312-219","312-223","312-226","312-227","312-228",
  "312-229","312-232","312-234","312-235","312-237","312-239","312-246",
  "312-251","312-252","312-255","312-257","312-260","312-261","312-262",
  "312-263","312-29","312-35","312-39","312-41","312-46","312-50","312-53",
  "312-54","312-59","312-62","312-66","312-69","312-71","312-72","312-77",
  "312-82","312-83","312-87","312-95","312-98","314-148","318-119")

# Lista 2: Cognitivos = 1 y NoCognitivos = 1
ids_ambos <- c(
  "312-112","312-119","312-13","312-135","312-142","312-145","312-153",
  "312-159","312-169","312-17","312-170","312-178","312-18","312-180",
  "312-183","312-187","312-19","312-190","312-194","312-207","312-21",
  "312-22","312-220","312-230","312-236","312-240","312-249","312-254",
  "312-259","312-26","312-61","312-78","314-36","318-106","318-122",
  "318-40","318-83","325-189")

# Lista 3: Etiqueta Cumplió Inclusión
ids_cumplio <- c("312-204","312-258","318-117","318-21", "316-32")

PrimeraPuerta <- Echados %>% 
  mutate(
    # --- Lógica original ---
    NoCognitivos = if_else(
      (crit_in1_pre ==0 | crit_in2_pre == 0 | crit_in4_pre == 0) |
        (crit_in1_scr == 0 | crit_in2_scr == 0 | crit_in4_scr == 0),
      1, 0, missing = 0
    ),
    Cognitivos = if_else(
      (crit_in3_pre == 0 | crit_in5_pre == 0 | crit_in6_pre == 0) |
        (crit_in3_scr == 0 | crit_in5_scr == 0 | crit_in6_scr == 0),
      1, 0, missing = 0
    ),
    
    # --- Sobrescribir según listas ---
    Cognitivos   = if_else(record_id %in% ids_cognitivos | record_id %in% ids_ambos, 1, Cognitivos),
    NoCognitivos = if_else(record_id %in% ids_ambos, 1, NoCognitivos),
    
    # --- Etiqueta general ---
    SeVa = if_else((NoCognitivos == 0 & Cognitivos == 0),
                   "Cumplió Inclusión", "Se va primero"),
    
    # --- Sobrescribir etiqueta para lista 3 ---
    SeVa = if_else(record_id %in% ids_cumplio, "Cumplió Inclusión", SeVa))

PrimeraPuerta %>%
  summarise(
    Cantidad = sum(if_else(SeVa == "Se va primero", 1, 0), na.rm = TRUE))



#-------------------------------------------------------------------------------
#                           LOS QUE SE VAN DESPUÉS
#-------------------------------------------------------------------------------

SegundaPuerta <- PrimeraPuerta %>%
  filter(SeVa == "Cumplió Inclusión" | is.na(SeVa))

#Grupo 1: CONDICIONES NEUROLÓGICAS
cols_grupo1 <- c(
  paste0(c("crit_ex1","crit_ex3","crit_ex23","crit_ex18"), "_pre"),
  paste0(c("crit_ex1","crit_ex3","crit_ex23","crit_ex18"), "_scr"))

#GRUPO 2: CONDICIONES VASCULARES
cols_grupo2 <- c(
  paste0(c("crit_ex7","crit_ex5","crit_ex6","crit_ex8","crit_ex17"), "_pre"),
  paste0(c("crit_ex7","crit_ex5","crit_ex6","crit_ex8","crit_ex17"), "_scr"))

#GRUPO 3: CONDICIONES PSIQUIÁTRICAS
cols_grupo3 <- c(
  paste0(c("crit_ex4","crit_ex20","crit_ex22"), "_pre"),
  paste0(c("crit_ex4","crit_ex20","crit_ex22"), "_scr"))

#GRUPO 4: CONDICIONES FÍSICAS
cols_grupo4 <- c(
  paste0(c("crit_ex13","crit_ex14"), "_pre"),
  paste0(c("crit_ex13","crit_ex14"), "_scr"))

#GRUPO 5: CONDICIONES MÉDICO CLÍNICAS
cols_grupo5 <- c(
  paste0(c("crit_ex2","crit_ex10","crit_ex11","crit_ex12","crit_ex16"), "_pre"),
  paste0(c("crit_ex2","crit_ex10","crit_ex11","crit_ex12","crit_ex16"), "_scr"))

#GRUPO 6: RESPIRATORIAS
cols_grupo6 <- c(
  paste0(c("crit_ex9","crit_ex15"), "_pre"),
  paste0(c("crit_ex9","crit_ex15"), "_scr"))

#GRUPO 7: OTRO ENSAYO 
cols_grupo7 <- c(
  paste0(c("crit_ex21"), "_pre"),
  paste0(c("crit_ex21"), "_scr")
)

Exclusiones <- SegundaPuerta %>%
  mutate(
    Grupo1 = rowSums(
      across(all_of(cols_grupo1), ~ .x == 1),
      na.rm = TRUE
    ) > 0,
    Grupo2 = rowSums(
      across(all_of(cols_grupo2), ~ .x == 1), na.rm = TRUE
    ) > 0,
    Grupo3 = rowSums(
      across(all_of(cols_grupo3), ~ .x == 1), na.rm = TRUE) > 0,
    Grupo4 = rowSums(
      across(all_of(cols_grupo4), ~ .x == 1), na.rm = TRUE) > 0,
    Grupo5 = rowSums(
      across(all_of(cols_grupo5), ~ .x == 1), na.rm = TRUE) > 0,
    Grupo6 = rowSums(
      across(all_of(cols_grupo6), ~ .x == 1), na.rm = TRUE) > 0,
    Grupo7 = rowSums(
      across(all_of(cols_grupo7), ~ .x == 1), na.rm = TRUE) >0
  )


TotalGrupo <- sum(Exclusiones$Grupo7, na.rm = TRUE)
TotalGrupo








