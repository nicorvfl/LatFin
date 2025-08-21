library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)

#Me traigo la base de datos
df_bruto <- read_csv("C:/Users/nicor/OneDrive/Desktop/LatAmFINGERS/total_data_2025-08-14.csv")

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
             "dropout_phase","dropout_reason")

PatronColumnas <- paste0("^(", paste(Cuerpos, collapse ="|"), ")_(",
                         paste(Eventos, collapse = "|"), ")$")
PatronNombres <- paste0("^(.*)_(", paste(Eventos, collapse = "|"), ")$")

df_v1 <- df_bruto %>%
  select(record_id, male_pre, race_pre, date_start_pre,country_pre,age_pre,
         marital_status_pre,living_alone_pre,education_pre,education_years_base,
         tobacco_pre, dislipidemia_caide_pre, myocardial_infarction_pre,
         heart_failure_pre,cardiac_surgery_pre,stroke_pre,ait_pre,
         glicemia_pre,diabetes_pre,rdz_yn_scr,rdz_rdz,
         matches(PatronColumnas))%>%
  mutate(across(matches(PatronColumnas),        
      ~ readr::parse_number(as.character(.x))
    ))%>%
  pivot_longer(
    cols = -c(record_id, male_pre, date_start_pre,country_pre,age_pre,
              marital_status_pre,living_alone_pre,education_pre,education_years_base,
              tobacco_pre, dislipidemia_caide_pre, myocardial_infarction_pre,
              heart_failure_pre,cardiac_surgery_pre,stroke_pre,ait_pre,
              glicemia_pre,diabetes_pre,rdz_yn_scr,rdz_rdz),
    names_to = c("Cuerpos","Eventos"),
    names_pattern = PatronNombres,
    values_to = "valor")%>%
  pivot_wider(
    id_cols = c(record_id, male_pre, date_start_pre,country_pre,age_pre,
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
#Voy a pasar a factor el status de grupo y rdz_yn
#-------------------------------------------------------------------------------

df_v3 <- df_v2 %>%
  mutate(
    Arm = factor(rdz_rdz,
                   levels = c("0","1"),
                   labels = c("Flexible","Systematic")),
    Randomization = factor(rdz_yn_scr,
                           levels = c("0", "1"),
                           labels = c("No", "Yes")))%>%
  select(-c(rdz_yn_scr,rdz_rdz))

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
    Randomization = if_else(record_id %in% null_rdz$record_id,
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




