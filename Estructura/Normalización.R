library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)

#Me traigo la base de datos
df_bruto <- read_csv("C:/Users/nicor/OneDrive/Desktop/LatAmFINGERS/total_data_2025-10-10.csv")


#-------------------------------------------------------------------------------
#----------------------- CAMBIOS SIMPLES ---------------------------------------
#Antes que nada, por las dudas, voy a convertir el formato minutos:segundos a 
#segundos (números) porque se da en TMT y presiento que me va a romper todo.
#-------------------------------------------------------------------------------

df_bruto <- df_bruto %>%
  mutate(across(matches("tima"), ConvertirSegundosTMT)) 

df_bruto <- df_bruto %>%
  mutate(across(matches("tiempo"), ConvertirSegundosTMT)) 


#-------------------------------------------------------------------------------
#También de paso vuelo a PR
#-------------------------------------------------------------------------------

df_bruto <- df_bruto %>%
  filter(country_pre != 10)

#-------------------------------------------------------------------------------
#--------------------------- ESTRUCTURO LA BASE --------------------------------
#Como el evento está "pegado" al nombre de la variable, tengo que "romper" la
#variable en 2, pasarla a longer y con eso lo paso a wider a fin de armar 
#mi nueva variable "evento".
#Ahora está así:
#id |  edad  |  memoria_pre  |  memoria_base  |  memoria_6m
#Y yo quiero:
# id |  edad  |  evento  |  memoria
#-------------------------------------------------------------------------------

Eventos <- c("pre","scr","base","6m","12m","18m","24m")
Cuerpos <- c("imm_recalltotal","score_wais_bruto","score_wais_escalar",
             "forwardtotcorrect","backwardtotcorrect","sequencetotcorrect",
             "fwdlongspanleng","backlongspanleng","seqlongspanleng",
             "trail_b_error","trail_a_error","trail_interrupt_test",
             "tima_trail_a","tima_trail_b","stroop_p","stroop_c","stroop_pc",
             "stroop_errores","csta","cstb","cstc","shifting_score",
             "delayed_recalltotal","contlearntot","immcuetotal","total_evoc_1l",
             "total_evoc_1g","total_evoc_1l1g","total_evoc_2l","total_evoc_2g",
             "total_evoc_2l2g","total_evoc_3l","total_evoc_3g","total_evoc_3l3g",
             "totalfreerecall","totalfreerecall_2","tiempo_parte_a",
             "tiempo_parte_b","tiempo_parte_c","tiempo1","tiempo2","totale_rdl",
             "totale_rdc","totales_tardia","animaltotcorrect_vc",
             "p_total_score","m_total_score", "bp_systolic1","bp_systolic2",
             "bp_diastolic1","bp_diastolic2","hr1","hr2","height","weight","bmi",
             "waist","glucose","hba1c","total_cholesterol","triglycerides","hdl",
             "ldl","hemoglobin","hematocrit","creatinine","urea","vhs","insulinemia",
             "mmarg_date","mmarg_puntaje_total","mmbr_puntaje_total",
             "mmch_puntaje_total","cerad1_escore","cerad2_escore","cerad3_escore",
             "cerad_total_score","ipaq_score","ipaq_classification","total_caide",
             "score_diferido","date_memory", "gds_total_score", "cdr_sb",
             "score_final_score","iadl_score_total","apoe_genotype","apoe4",
             "aeyn","aeterm","aeclassif","nis_count","nis_count_telefonica","ec_count",
             "dropout_phase","dropout_reason", "fra_score", "mmse_total",
             "ifa18", "ifa19", "ifa20", "ifa21", "education_mother","mgr_count",
             "education_father", "live_area", "house_type", "ef_count",
             "mgr_1grupal1","mgr_2grupal1","mgr_3grupal1","mgr_4grupal1",
             "mgr_4grupal1", "mgr_5grupal1","mgr_5grupal2","mgr_6grupal1",
             "mgr_6grupal2","mgr_7grupal1","mgr_8grupal1","mgr_9grupal1",
             "mgr_10grupal1","mgr_11grupal1","mgr_12grupal1",
             "pointerfood1", "pointerfood2", "pointerfood3", "pointerfood4", 
             "pointerfood5","pointerfood6", "pointerfood7", "pointerfood8",
             "pointerfood9", "pointerfood10", "pointerfood11", 
             "pointerfood12", "pointerfood13", "pointerfood14", "ifa19",
             "ef_grupal1","ef_grupal2","ef_grupal3","ef_grupal4",
             "nis_grupal1","nis_grupal2","nis_grupal3","nis_grupal4",
             "ec_grupal1","ec_grupal2","ec_grupal3","ec_grupal4",
             "hmi_4capacitacion1", "hmi_4capacitacion2", "hmi_4capacitacion3",
             "hmi_4capacitacion4", "ef_date", "gf_date", "dropout_reason3",
             "mother_problem", "father_problem",
             "trail_interrupt_test","flexible_yn",
             paste0("ef_week_", rep(1:52, each = 4), "_", 1:4),
             paste0("nis_week_",rep(1:52)), paste0("ec_niveles_",rep(1:52)),
             paste0("ec_week_", rep(1:52, each = 4), "_", 1:7),
             "nis_telefonica1",  "nis_telefonica2",  "nis_telefonica3_1",  
             "nis_telefonica3_2","nis_telefonica4_1", "nis_telefonica4_2",
             "nis_telefonica5_1" ,"nis_telefonica5_2",
             "nis_telefonica6_1", "nis_telefonica6_2", "nis_telefonica7",   
             "nis_telefonica8","nis_telefonica9" ,  "nis_telefonica10",  
             "nis_telefonica11","nis_telefonica12","nis_telefonica13"
)


PatronColumnas <- paste0("^(", paste(Cuerpos, collapse ="|"), ")_(",
                         paste(Eventos, collapse = "|"), ")$")
PatronNombres <- paste0("^(.*)_(", paste(Eventos, collapse = "|"), ")$")

df_v1 <- df_bruto %>% 
  select(record_id, male_pre, retirement_pre,race_pre, date_start_pre,country_pre,age_pre,
         marital_status_pre,living_alone_pre,education_pre,education_years_base,
         tobacco_pre, dislipidemia_caide_pre, myocardial_infarction_pre,
         heart_failure_pre,cardiac_surgery_pre,stroke_pre,ait_pre,
         glicemia_pre,diabetes_pre,rdz_yn_scr,rdz_rdz,job_pre,
         reason_not_rdz_scr, reason_rdz_scr,
         # Exclusión SCR
         crit_ex1_scr,  crit_ex2_scr,  crit_ex3_scr,  crit_ex4_scr,  crit_ex5_scr,  crit_ex6_scr, 
         crit_ex7_scr,  crit_ex8_scr,  crit_ex9_scr,  crit_ex10_scr, crit_ex11_scr, crit_ex12_scr,
         crit_ex13_scr, crit_ex14_scr, crit_ex15_scr, crit_ex16_scr, crit_ex17_scr, crit_ex18_scr,
         crit_ex19_scr, crit_ex20_scr, crit_ex21_scr, crit_ex22_scr, crit_ex23_scr,
         # Inclusión SCR
         crit_in1_scr, crit_in2_scr, crit_in3_scr, crit_in4_scr, crit_in5_scr, crit_in6_scr,
         # Exclusión PRE
         crit_ex1_pre,  crit_ex2_pre,  crit_ex3_pre,  crit_ex4_pre,  crit_ex5_pre,  crit_ex6_pre, 
         crit_ex7_pre,  crit_ex8_pre,  crit_ex9_pre,  crit_ex10_pre, crit_ex11_pre, crit_ex12_pre,
         crit_ex13_pre, crit_ex14_pre, crit_ex15_pre, crit_ex16_pre, crit_ex17_pre, crit_ex18_pre,
         crit_ex19_pre, crit_ex20_pre, crit_ex21_pre, crit_ex22_pre, crit_ex23_pre,
         # Inclusión PRE
         crit_in1_pre, crit_in2_pre, crit_in3_pre, crit_in4_pre, crit_in5_pre, crit_in6_pre,
         matches(PatronColumnas)) %>%
  mutate(across(matches(PatronColumnas),        
                ~ readr::parse_number(as.character(.x))
  )) %>%
  pivot_longer( 
    cols = -c(record_id, male_pre, date_start_pre,country_pre,age_pre,
              marital_status_pre,living_alone_pre,education_pre,education_years_base,
              tobacco_pre, retirement_pre, job_pre, dislipidemia_caide_pre, myocardial_infarction_pre,
              heart_failure_pre,cardiac_surgery_pre,stroke_pre,ait_pre,
              glicemia_pre,diabetes_pre,rdz_yn_scr,rdz_rdz,reason_not_rdz_scr, reason_rdz_scr,
              crit_ex1_scr:crit_ex23_scr, crit_in1_scr:crit_in6_scr,
              crit_ex1_pre:crit_ex23_pre, crit_in1_pre:crit_in6_pre),
    names_to = c("Cuerpos","Eventos"),
    names_pattern = PatronNombres,
    values_to = "valor") %>%
  pivot_wider(
    id_cols = c(record_id, male_pre, date_start_pre,country_pre,age_pre,
                job_pre, retirement_pre,
                marital_status_pre,living_alone_pre,education_pre,education_years_base,
                tobacco_pre, dislipidemia_caide_pre, myocardial_infarction_pre,
                heart_failure_pre,cardiac_surgery_pre,stroke_pre,ait_pre,
                reason_not_rdz_scr, reason_rdz_scr,
                crit_ex1_scr:crit_ex23_scr, crit_in1_scr:crit_in6_scr,
                crit_ex1_pre:crit_ex23_pre, crit_in1_pre:crit_in6_pre,
                glicemia_pre,diabetes_pre,rdz_yn_scr,rdz_rdz,Eventos),
    names_from = Cuerpos,
    values_from = valor) %>%
  arrange(record_id, Eventos) %>%
  mutate(
    Eventos = factor(Eventos,
                     levels = c("pre","scr","base","6m","12m","18m","24m"))
  )


#-------------------------------------------------------------------------------
#Hay algunas cosas previas por arreglar.
#Voy a unificar educación
#-------------------------------------------------------------------------------

df_v2 <- df_v1 %>%
  mutate(
    education_years = if_else(!is.na(education_years_base),education_years_base,
                              education_pre))%>%
  select(-c(education_years_base,education_pre))

#-------------------------------------------------------------------------------
#Voy a pasar a factor el status de grupo y rdz_yn + cambio sexo
#-------------------------------------------------------------------------------

df_v3 <- df_v2 %>%
  mutate(
    Arm = factor(rdz_rdz,
                 levels = c("0","1"),
                 labels = c("Flexible","Systematic")),
    Randomization = factor(rdz_yn_scr,
                           levels = c("0", "1"),
                           labels = c("No", "Yes")))%>%
  select(-c(rdz_yn_scr,rdz_rdz))%>%
  mutate(
    Sex = if_else(
      male_pre == 1, "Men", "Women"
    )
  )

#-------------------------------------------------------------------------------
#Vamos a ver el tema países
#Ya hay una variable que es country_pre, pero no la entiendo del todo y prefiero
#no complicarme, así que como ya conozco el ID y su respectivo país, me armo
#una variable que me indique el país en función del id.
#Además, me va a permitir ser más fino en términos del centro ya que Brasil
#cuenta con 2.
#-------------------------------------------------------------------------------

df_v4 <- df_v3 %>%
  mutate(
    CodigoCentro = str_extract(record_id, "^[0-9]+"),
    center = case_when(
      CodigoCentro == "312" ~ "Argentina",
      CodigoCentro == "325" ~ "Bolivia",
      CodigoCentro == "317" ~ "Brasil",
      CodigoCentro == "316" ~ "Brasil",
      CodigoCentro == "315" ~ "Chile",
      CodigoCentro == "314" ~ "Colombia",
      CodigoCentro == "321" ~ "Costa Rica",
      CodigoCentro == "318" ~ "Ecuador",
      CodigoCentro == "313" ~ "México",
      CodigoCentro == "320" ~ "Perú",
      CodigoCentro == "319" ~ "Uruguay",
      CodigoCentro == "324" ~ "República Dominicana",
      TRUE ~ "Otro"))

#-------------------------------------------------------------------------------
#Arreglo personas que no están randomizadas
#-------------------------------------------------------------------------------

df_v4 <- df_v4 %>%
  mutate(
    Randomization = if_else(is.na(Randomization),
                            "No", Randomization))

#-------------------------------------------------------------------------------
#Tengo también personas que no están randomizadas pero tienen un "Yes"
#-------------------------------------------------------------------------------

id_erroneos_2 <- c("316-3","320-24","320-39","324-60","324-73",
                   "315-1","315-2","315-3","315-9","315-10","315-11",
                   "315-13","315-15","315-16","315-17","315-18","315-19",
                   "315-21","315-37","315-48", "314-76",
                   "314-101", "314-105", "314-124", "314-148",
                   "314-154",
                   "315-1", "315-2", "315-3", "315-9", "315-10", 
                   "315-11", "315-13", "315-15", "315-16", "315-17", 
                   "315-18", "315-19", "315-21", "315-37", "315-48", 
                   "312-167", "318-41", "316-102", "321-154", 
                   "321-156", "321-157", "321-163", "321-159")

df_v4 <- df_v4 %>%
  mutate(
    Randomization = if_else(record_id %in% id_erroneos_2,
                            "No",
                            Randomization))
#-------------------------------------------------------------------------------
#Etnicidad
#-------------------------------------------------------------------------------
race_pre <- df_v4 %>%
  filter(Eventos == "pre") %>%
  transmute(record_id,
            race_pre = str_trim(as.character(race)))
df_v4 <- df_v4 %>%
  left_join(race_pre, by = "record_id") %>%
  mutate(
    race = if_else(!is.na(race_pre), race_pre, as.character(race)),
    race = na_if(str_trim(race), "")
  ) %>%
  select(-race_pre)

#-------------------------------------------------------------------------------
#Arreglo algunos errores de laboratorio.
#Ejemplo: en brasil pusieron el VHS faltante con un 999.
#México tiene ceros pero no lo tiene tampoco.
#-------------------------------------------------------------------------------

df_v4 <- df_v4 %>%
  mutate(
    vhs = if_else(
      vhs == 999 | center == "México",
      NA_real_,   
      vhs))

#-------------------------------------------------------------------------------
#Voy a adicionar una columna que sea la clasificación del Framingham.
#-------------------------------------------------------------------------------

#Quiero mirar la distribución para entender el puntaje
df_v4 <- df_v4 %>%
  mutate(
    Fra_Clase = if_else(
      Sex == "Men", 
      if_else(
        fra_score <= 12, "Low",
        if_else(fra_score < 16, "Medium", "High")),
      if_else(
        fra_score <= 19, "Low",
        if_else(fra_score < 23, "Medium", "High"))),
    Fra_Clase = factor(Fra_Clase) |> fct_relevel("Low", "Medium", "High"))

#-------------------------------------------------------------------------------
#Agrego una variable dicotómica que sea si tiene o no APOE e4
#-------------------------------------------------------------------------------

df_v4 <- df_v4 %>%
  mutate(
    APOE = case_when(
      apoe4 == 0 ~ "Non-carrier",
      apoe4 > 0  ~ "Carrier"
    )
  )


#-------------------------------------------------------------------------------
#Paso el MMSE a baseline
#-------------------------------------------------------------------------------

df_v4 <- df_v4 %>%
  group_by(record_id) %>% 
  mutate(
    mmse_total = if_else(
      Eventos == "base" & is.na(mmse_total),  
      mmse_total[Eventos == "pre"][1],        
      mmse_total                          
    )
  ) %>%
  ungroup()

#-------------------------------------------------------------------------------
#Fumadores + consumo de alcohol
#-------------------------------------------------------------------------------

#Pregunta 1: ¿Cuántas personas han fumado a lo largo de su vida?
#593

fumadores <- df_v4 %>%
  filter(Eventos == "base" &
           Randomization == "Yes")%>%
  summarise(
    Cantidad_Fumadores = sum(tobacco_pre == 1, na.rm = TRUE)
  )

#Recodifico el nombre
df_v4 <- df_v4 %>%
  mutate(
    History_Smoking = if_else(
      tobacco_pre == 1,
      "Smoking history", "No smoking history"
    )
  )

#Vamos con el alcohol
df_v4 <- df_v4 %>%
  mutate(
    Alcohol_Consumption = if_else(
      if_any(c(ifa18, ifa19, ifa20, ifa21), ~ .x >= 3),
      "At least once a week", "Less than once a week"
    )
  )

#-------------------------------------------------------------------------------
#Renombro el CDR y el IPAQ
#-------------------------------------------------------------------------------

df_v4 <- df_v4 %>%
  rename(
    CDR = score_final_score
  ) %>%
  mutate(
    ipaq = case_when(
      ipaq_score == 0 ~ "Sedentary",
      ipaq_score == 1 ~ "Irregulary active",
      ipaq_score == 2 ~ "Active",
      ipaq_score == 3 ~ "Very active"
    ))

#-------------------------------------------------------------------------------
#Determinantes sociales de la salud
#-------------------------------------------------------------------------------

df_v4 <- df_v4 %>%
  mutate(
    EscolaridadMadre = case_when(
      education_mother == 0 ~ "No schooling",
      education_mother == 1 ~ "Incomplete primary education",
      education_mother == 2 ~ "Complete primary education",
      education_mother == 3 ~ "Incomplete secondary education",
      education_mother == 4 ~ "Complete secondary education",
      education_mother == 5 ~ "Incomplete tertiary education",
      education_mother == 6 ~ "Complete tertiary education",
      education_mother == 7 ~ "Incomplete university education",
      education_mother == 8 ~ "Complete university education",
      education_mother == 9 ~ "Incomplete postgraduate education",
      education_mother == 10 ~ "Complete postgraduate education",
      education_mother == 1 ~ "Does not know"
    ),
    EscolaridadPadre = case_when(
      education_father == 0 ~ "No schooling",
      education_father == 1 ~ "Incomplete primary education",
      education_father == 2 ~ "Complete primary education",
      education_father == 3 ~ "Incomplete secondary education",
      education_father == 4 ~ "Complete secondary education",
      education_father == 5 ~ "Incomplete tertiary education",
      education_father == 6 ~ "Complete tertiary education",
      education_father == 7 ~ "Incomplete university education",
      education_father == 8 ~ "Complete university education",
      education_father == 9 ~ "Incomplete postgraduate education",
      education_father == 10 ~ "Complete postgraduate education",
      education_father == 1 ~ "Does not know"
    ),
    Area = case_when(
      live_area == 1 ~ "Urban",
      live_area == 2 ~ "Rural",
      live_area == 9 ~ "Unknown"
    ),
    TipoHogar = case_when(
      house_type == 1 ~ "House",
      house_type == 2 ~ "Apartment",
      house_type == 3 ~ "Room in boarding house",
      house_type == 4 ~ "Room in hotel/guesthouse",
      house_type == 5 ~ "Premises not built for housing",
      house_type == 9 ~ "Unknown"
    )
  )

#-------------------------------------------------------------------------------
#Quiero crear una columna que me diga si el sujeto tiene o no la ev. completa
#-------------------------------------------------------------------------------

TestNps = c(
  "imm_recalltotal","score_wais_bruto","score_wais_escalar",
  "forwardtotcorrect","backwardtotcorrect","sequencetotcorrect",
  "fwdlongspanleng","backlongspanleng","seqlongspanleng",
  "trail_b_error","trail_a_error","trail_interrupt_test",
  "tima_trail_a","tima_trail_b","stroop_p","stroop_c","stroop_pc",
  "stroop_errores","csta","cstb","cstc","shifting_score",
  "delayed_recalltotal","contlearntot","immcuetotal","total_evoc_1l",
  "total_evoc_1g","total_evoc_1l1g","total_evoc_2l","total_evoc_2g",
  "total_evoc_2l2g","total_evoc_3l","total_evoc_3g","total_evoc_3l3g",
  "totalfreerecall","totalfreerecall_2","tiempo_parte_a","tiempo_parte_b",
  "tiempo_parte_c","tiempo1","tiempo2","totale_rdl","totale_rdc",
  "totales_tardia","animaltotcorrect_vc","p_total_score","m_total_score")

# cuántas columnas de TestNps existen realmente en df_v4
eval_present <- intersect(TestNps, names(df_v4))
n_present_cols <- length(eval_present)

df_v4 <- df_v4 %>%
  mutate(
    n_no_na = rowSums(across(all_of(eval_present), ~ !is.na(.))), 
    EvaluacionCompleta3 = if_else(n_no_na >= 3, 1, 0),            
    EvaluacionCompleta50 = if_else(n_no_na >= ceiling(0.5 * n_present_cols), 1,
                                   0))

#-------------------------------------------------------------------------------
#Área laboral y modifico de nuevo educación
#-------------------------------------------------------------------------------

df_v4 <- df_v4 %>%
  mutate(
    EducationLevel = case_when(
      education_years < 7 ~ "Incomplete Primary",
      education_years < 9 ~ "Complete Primary",
      education_years < 13 ~ "Complete Secondary",
      education_years > 12 ~ "University or higher"
    ),
    Job = case_when(
      job_pre == 1 ~ "Clerical/Office",
      job_pre == 2 ~ "Equipment/Vehicle Operator",
      job_pre == 3 ~ "Farmer",
      job_pre == 4 ~ "Laborer",
      job_pre == 5 ~ "Manager/Owner",
      job_pre == 6 ~ "Military",
      job_pre == 7 ~ "Professional/Technical",
      job_pre == 8 ~ "Sales",
      job_pre == 9 ~ "Service",
      job_pre == 10 ~ "Craftsman/Repair"
    ),
    Retirement = if_else(retirement_pre == 1, "Yes", "No"))

#-------------------------------------------------------------------------------
#Recodificamos dropout
#-------------------------------------------------------------------------------

df_v4 <- df_v4 %>%
  mutate(
    DropoutReason = case_when(
      dropout_reason == 0 ~ "Death",
      dropout_reason == 1 ~ "Adverse event",
      dropout_reason == 2 ~ "Eligibility criteria error",
      dropout_reason == 3 ~ "Participant withdrew",
      TRUE ~ NA_character_  
    )
  )

#-------------------------------------------------------------------------------
#Empezó la intervención
#-------------------------------------------------------------------------------

df_v4 <- df_v4 %>%
  group_by(record_id) %>%
  mutate(
    IniciaIntervencion = as.integer(any(!is.na(ef_date) | !is.na(gf_date)))
  ) %>%
  ungroup()


ids_peru <- c("320-18","320-6")
df_v4 <- df_v4 %>%
  mutate(
    IniciaIntervencion = if_else(record_id %in% ids_peru,
                                 1L, IniciaIntervencion))

#-------------------------------------------------------------------------------
#                        MIND SCORE 
#-------------------------------------------------------------------------------

df <- df_v4 %>%
  mutate(
    mind1 = if_else(pointerfood1 < 2, 0, if_else(pointerfood1 == 3, 0.5, 1)),#check
    mind2 = if_else(pointerfood2 < 4, 1, if_else(pointerfood2 == 4, 0.5, 0)),#check
    mind3 = if_else(pointerfood3 < 4, 0, if_else(pointerfood3 < 6, 0.5, 1)),#check
    mind4 = if_else(pointerfood4 < 5, 1, if_else(pointerfood4 == 5, 0.5, 0)),#check
    mind5 = if_else(pointerfood5 < 5, 0, if_else(pointerfood5 == 5, 0.5, 1)),#check
    mind6 = if_else(pointerfood6 < 3, 0, if_else(pointerfood6 == 3, 0.5, 1)),#check
    mind7 = if_else(pointerfood7 == 1, 0, if_else(pointerfood7 == 2, 0.5, 1)),#check
    mind8 = if_else(pointerfood8 < 3, 0, if_else(pointerfood8 == 3, 0.5, 1)),#check
    mind9 = if_else(pointerfood9 == 6, 0, if_else(pointerfood9 < 3, 1, 0.5)),#check
    mind10 = if_else(pointerfood10 < 3, 0, if_else(pointerfood10 < 5, 0.5, 1)),#check
    mind11 = if_else(pointerfood11 == 6, 0.5, 0),#check
    mind12 = if_else(pointerfood12 < 5, 1, if_else(pointerfood12 == 5, 0.5, 0)),#check
    mind13 = if_else(pointerfood13 > 4, 1, if_else(pointerfood13 == 1, 0, 0.5)),#check
    mind14 = if_else(pointerfood14 > 3, 0, if_else(pointerfood14 == 3, 0.5, 1)),#check
    MIND = rowSums(across(starts_with("mind")), na.rm = TRUE))

#-------------------------------------------------------------------------------
#                              PROMEDIAMOS PRESIÓN
#-------------------------------------------------------------------------------

df <- df %>%
  mutate(
    Diastolic = (bp_diastolic1 + bp_diastolic2) / 2,
    Systolic = (bp_systolic1 + bp_systolic2) / 2)


#-------------------------------------------------------------------------------
#                         MISSING AT RANDOM
#-------------------------------------------------------------------------------

#Tienen todas las evaluaciones
df <- df %>%
  group_by(record_id) %>%
  mutate(
    cuenta = sum(EvaluacionCompleta50 == 1, na.rm = TRUE),
    AsistenciaPerfecta = if_else(cuenta == 5, 1, 0)
  ) %>%
  ungroup()

df <- df %>%
  group_by(record_id) %>%
  mutate(
    TieneBase  = any(EvaluacionCompleta50 == 1 & Eventos == "base", na.rm = TRUE),
    Tiene6m  = any(EvaluacionCompleta50 == 1 & Eventos == "6m", na.rm = TRUE),
    Tiene12m  = any(EvaluacionCompleta50 == 1 & Eventos == "12m", na.rm = TRUE),
    Tiene18m  = any(EvaluacionCompleta50 == 1 & Eventos == "18m", na.rm = TRUE),
    Tiene24m   = any(EvaluacionCompleta50 == 1 & Eventos == "24m", na.rm = TRUE),
    AsistenciaInicioFin = if_else(TieneBase & Tiene24m, 1, 0),
    BaseMasUno = if_else(TieneBase == TRUE & 
                           (Tiene6m == TRUE | Tiene12m == TRUE |
                              Tiene18m == TRUE | Tiene24m == TRUE), 1, 0)
  ) %>%
  ungroup()


#-------------------------------------------------------------------------------
#                         ANTECEDENTES FAMILIARES
#-------------------------------------------------------------------------------

df <- df %>%
  mutate(
    father_problems = case_when(
      father_problem == 5 ~ "Psiquiátrico",
      father_problem %in% c(1, 2, 3, 4) ~ "Neurológico",
      father_problem == 8 ~ "Sin problemas",
      father_problem == 9 ~ "Desconocido",
      TRUE ~ NA_character_),
    mother_problems = case_when(
      mother_problem == 5 ~ "Psiquiátrico",
      mother_problem %in% c(1, 2, 3, 4) ~ "Neurológico",
      mother_problem == 8 ~ "Sin problemas",
      mother_problem == 9 ~ "Desconocido",
      TRUE ~ NA_character_)
  )

#-------------------------------------------------------------------------------
#                       DECISIONES DE LIMPIEZA
#-------------------------------------------------------------------------------

df <- df %>%
  mutate(
    #Si el TMT-A es mayor a 300, lo dejo en 300.
    tima_trail_a = if_else(tima_trail_a > 300, 300, tima_trail_a),
    tima_trail_a = if_else(tima_trail_a == 300, NA, tima_trail_a),
    #Este ID se olvidó los anteojos a los 24m.
    tima_trail_a = if_else(record_id == "320-45" & 
                             Eventos == "24m", NA, tima_trail_a),
    tima_trail_b = if_else(record_id == "320-45" & 
                             Eventos == "24m", NA, tima_trail_b),
    #Todos los interrumpidos pasan a ser NA
    tima_trail_b = if_else(trail_interrupt_test == 1, NA, tima_trail_b),
    #Si el TMT tiene un 0, va NA.
    tima_trail_a = if_else(tima_trail_a == 0, NA, tima_trail_a),
    tima_trail_b = if_else(tima_trail_b == 0,
                           NA, tima_trail_b),
    #Si el stroop está en 0, va NA.
    stroop_p = if_else(stroop_p == 0, NA, stroop_p),
    stroop_c = if_else(stroop_c == 0, NA, stroop_c),
    stroop_pc = if_else(stroop_pc == 0, NA, stroop_pc),
    #Modificación de un valor en particular.
    p_total_score = if_else(record_id == "318-121" & Eventos == "6m",
                            18, p_total_score),
    m_total_score = if_else(record_id == "314-123" & Eventos == "base",
                            3, m_total_score),
    stroop_p = if_else(record_id == "320-45" & Eventos == "24m",
                       NA, stroop_p),
    stroop_c = if_else(record_id == "320-45" & Eventos == "24m",
                       NA, stroop_c),
    stroop_pc = if_else(record_id == "320-45" & Eventos == "24m",
                        NA, stroop_pc),
    #CST
    tiempo_parte_a = if_else(tiempo_parte_a == 0 |
                               tiempo_parte_a == 300,
                             NA, tiempo_parte_a),
    tiempo_parte_b = if_else(tiempo_parte_b == 0 |
                               tiempo_parte_b == 300,
                             NA, tiempo_parte_b),
    tiempo_parte_c = if_else(tiempo_parte_c == 0 |
                               tiempo_parte_c == 300,
                             NA, tiempo_parte_c))

#-------------------------------------------------------------------------------
#                              ¿ES DROPOUT?
#-------------------------------------------------------------------------------

df <- df %>%
  group_by(record_id) %>%
  mutate(
    EsDropout = if_else(Randomization == "Yes" & Tiene24m == FALSE,
                          "Dropout", "No-Dropout"))%>%
  ungroup()

dfcheckeo <- df %>%
  filter(Eventos == "24m", Randomization == "Yes")%>%
  select(record_id, Tiene24m, EsDropout, dropout_phase)

df <- df %>%
  group_by(record_id) %>%
  arrange(Eventos, .by_group = TRUE) %>%
  fill(dropout_phase, .direction = "downup") %>%
  ungroup()

#-------------------------------------------------------------------------------
#                      ADHERENCIA MÍNIMA
#-------------------------------------------------------------------------------

# ¿Qué tengo que hacer?
# Tengo que crear una columna que indique si la persona adhirió (sea en 
# la actividad que sea) a la intervención en el intervalo. 
ef_w   <- function(weeks) paste0("ef_week_", rep(weeks,  each = 4), "_", 1:4)
nis_w  <- function(weeks) paste0("nis_week_", weeks)
ec_w   <- function(weeks) paste0("ec_week_",  rep(weeks,  each = 7), "_", 1:7)
w_0_6   <- 1:26
w_6_12  <- 27:52
w_12_18 <- w_0_6
w_18_24 <- w_6_12
cols_0_6   <- c(ef_w(w_0_6),   nis_w(w_0_6),   ec_w(w_0_6))
cols_6_12  <- c(ef_w(w_6_12),  nis_w(w_6_12),  ec_w(w_6_12))
cols_12_18 <- c(ef_w(w_12_18), nis_w(w_12_18), ec_w(w_12_18))
cols_18_24 <- c(ef_w(w_18_24), nis_w(w_18_24), ec_w(w_18_24))
cols_0_6   <- intersect(cols_0_6,   names(df))
cols_6_12  <- intersect(cols_6_12,  names(df))
cols_12_18 <- intersect(cols_12_18, names(df))
cols_18_24 <- intersect(cols_18_24, names(df))
teams_0_6 <- c(
  "ec_grupal1","ec_grupal2","ec_grupal3","ec_grupal4",
  "nis_grupal1","nis_grupal2","nis_grupal3","nis_grupal4",
  "ef_grupal1","ef_grupal2","ef_grupal3","ef_grupal4",
  "hmi_4capacitacion1","hmi_4capacitacion2","hmi_4capacitacion3","hmi_4capacitacion4",
  "mgr_5grupal1","mgr_5grupal2","mgr_6grupal1","mgr_6grupal2"
)

teams_6_12  <- c("mgr_7grupal1","mgr_8grupal1","mgr_9grupal1",
                 "mgr_10grupal1","mgr_11grupal1","mgr_12grupal1")

teams_12_18 <- c("mgr_1grupal1","mgr_2grupal1","mgr_3grupal1",
                 "mgr_4grupal1","mgr_5grupal1","mgr_6grupal1")

teams_18_24 <- c("mgr_7grupal1","mgr_8grupal1","mgr_9grupal1",
                 "mgr_10grupal1","mgr_11grupal1","mgr_12grupal1")
teams_0_6   <- intersect(teams_0_6,   names(df))
teams_6_12  <- intersect(teams_6_12,  names(df))
teams_12_18 <- intersect(teams_12_18, names(df))
teams_18_24 <- intersect(teams_18_24, names(df))

all_0_6   <- c(cols_0_6,   teams_0_6)
all_6_12  <- c(cols_6_12,  teams_6_12)
all_12_18 <- c(cols_12_18, teams_12_18)
all_18_24 <- c(cols_18_24, teams_18_24)

cond <- function(x) {
  if (is.numeric(x)) return(!is.na(x) & x != 0)
  if (is.logical(x)) return(!is.na(x) & x)
  if (is.character(x)) {
    vx <- trimws(tolower(x))
    return(!is.na(vx) & vx %in% c("1","si","sí","yes","true","x","hecho","ok"))
  }
  # fallback
  !is.na(x) & x != 0
}

row_any <- function(data, cols) {
  if (length(cols) == 0) return(rep(FALSE, nrow(data)))
  rowSums(sapply(cols, function(c) cond(data[[c]]))) > 0
}

df_flags <- df %>%
  mutate(
    has_0_6   = (Eventos == "base") & row_any(cur_data(), all_0_6),
    has_6_12  = (Eventos == "base") & row_any(cur_data(), all_6_12),
    has_12_18 = (Eventos == "12m")  & row_any(cur_data(), all_12_18),
    has_18_24 = (Eventos == "12m")  & row_any(cur_data(), all_18_24))

adher_por_id <- df_flags %>%
  dplyr::group_by(record_id) %>%
  dplyr::summarise(
    adher_0_6   = as.integer(any(has_0_6,   na.rm = TRUE)),
    adher_6_12  = as.integer(any(has_6_12,  na.rm = TRUE)),
    adher_12_18 = as.integer(any(has_12_18, na.rm = TRUE)),
    adher_18_24 = as.integer(any(has_18_24, na.rm = TRUE)),
    .groups = "drop")


df <- df %>% dplyr::left_join(adher_por_id, by = "record_id")
View(df)

dfCuenta <- df %>%
  group_by(record_id)%>%
  filter(Randomization == "Yes" & Arm == "Systematic" &
           Eventos == "base")%>%
  mutate(
    SumaAdhMin = sum(adher_0_6,adher_6_12,
                     adher_18_24, adher_12_18))%>%
  select(record_id, center, 
         adher_0_6,adher_6_12,
         adher_18_24, adher_12_18, EsDropout,
         TieneBase, Tiene6m, Tiene12m, Tiene18m,
         Tiene24m, SumaAdhMin)
  
View(dfCuenta)

