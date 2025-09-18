library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)

#Me traigo la base de datos
df_bruto <- read_csv("C:/Users/nicor/OneDrive/Desktop/LatAmFINGERS/copia_total_data.csv")

#-------------------------------------------------------------------------------
#----------------------- CAMBIOS SIMPLES ---------------------------------------
#Antes que nada, por las dudas, voy a convertir el formato minutos:segundos a 
#segundos (números) porque se da en TMT y presiento que me va a romper todo.
#-------------------------------------------------------------------------------

df_bruto <- df_bruto %>%
  mutate(across(matches("tima"), ConvertirSegundosTMT)) 

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
             "hmi_4capacitacion4", "ef_date", "gf_date")


PatronColumnas <- paste0("^(", paste(Cuerpos, collapse ="|"), ")_(",
                         paste(Eventos, collapse = "|"), ")$")
PatronNombres <- paste0("^(.*)_(", paste(Eventos, collapse = "|"), ")$")

df_v1 <- df_bruto %>%
  select(record_id, male_pre, retirement_pre,race_pre, date_start_pre,country_pre,age_pre,
         marital_status_pre,living_alone_pre,education_pre,education_years_base,
         tobacco_pre, dislipidemia_caide_pre, myocardial_infarction_pre,
         heart_failure_pre,cardiac_surgery_pre,stroke_pre,ait_pre,
         glicemia_pre,diabetes_pre,rdz_yn_scr,rdz_rdz,job_pre,
         matches(PatronColumnas))%>%
  mutate(across(matches(PatronColumnas),        
      ~ readr::parse_number(as.character(.x))
    ))%>%
  pivot_longer(
    cols = -c(record_id, male_pre, date_start_pre,country_pre,age_pre,
              marital_status_pre,living_alone_pre,education_pre,education_years_base,
              tobacco_pre, retirement_pre, job_pre, dislipidemia_caide_pre, myocardial_infarction_pre,
              heart_failure_pre,cardiac_surgery_pre,stroke_pre,ait_pre,
              glicemia_pre,diabetes_pre,rdz_yn_scr,rdz_rdz),
    names_to = c("Cuerpos","Eventos"),
    names_pattern = PatronNombres,
    values_to = "valor")%>%
  pivot_wider(
    id_cols = c(record_id, male_pre, date_start_pre,country_pre,age_pre,
                job_pre, retirement_pre,
                marital_status_pre,living_alone_pre,education_pre,education_years_base,
                tobacco_pre, dislipidemia_caide_pre, myocardial_infarction_pre,
                heart_failure_pre,cardiac_surgery_pre,stroke_pre,ait_pre,
                glicemia_pre,diabetes_pre,rdz_yn_scr,rdz_rdz,Eventos),
    names_from = Cuerpos,
    values_from = valor) %>%
  arrange(record_id, Eventos)%>%
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
      CodigoCentro == "317" ~ "Brasil_UFMG",
      CodigoCentro == "316" ~ "Brasil_USP",
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
fumadores

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
#Asimismo debería de excluir los ID mal randomizados de Costa Rica
#-------------------------------------------------------------------------------

id_erroneos <- c("315-1", "315-2", "315-3", "315-9", "315-10", 
                 "315-11", "315-13", "315-15", "315-16", "315-17", 
                 "315-18", "315-19", "315-21", "315-37", "315-48", 
                 "312-167", "318-41", "316-102", "321-154", 
                 "321-156", "321-157", "321-163")
df_v4 <- df_v4 %>%
  filter(!record_id %in% id_erroneos)

#-------------------------------------------------------------------------------
#Tengo también personas que no están randomizadas pero tienen un "Yes"
#-------------------------------------------------------------------------------

id_erroneos_2 <- c("316-3","320-24","320-39","324-60","324-73",
                   "315-1","315-2","315-3","315-9","315-10","315-11",
                   "315-13","315-15","315-16","315-17","315-18","315-19",
                   "315-21","315-37","315-48")
df_v4 <- df_v4 %>%
  mutate(
    Randomization = if_else(record_id %in% id_erroneos_2,
                            "No",
                            Randomization)
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

df_v4 <- df_v4 %>%
  mutate(
    EvaluacionCompleta = rowSums(across(all_of(TestNps), ~ !is.na(.))) >= 3)%>%
  mutate(
    EvaluacionCompleta = if_else(
      EvaluacionCompleta == TRUE, 1,0))

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

