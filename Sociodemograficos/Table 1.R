library(dplyr)
library(arsenal)
library(forcats)


df_base_rdz <- df_v4 %>%
  filter(Eventos == "base", Randomization == "Yes") %>%
  distinct(record_id, .keep_all = TRUE) %>%
  mutate(Arm = factor(Arm))

Ns <- df_base_rdz %>% count(Arm, name = "N", .drop = FALSE)
N_flex <- Ns %>% filter(Arm == "Flexible")  %>% pull(N) %>% {if(length(.)==0) 0 else .}
N_sys  <- Ns %>% filter(Arm == "Systematic") %>% pull(N) %>% {if(length(.)==0) 0 else .}
N_tot  <- nrow(df_base_rdz)

df_tab1 <- df_base_rdz %>%
  mutate(
    Age  = as.numeric(age_pre),
    Sex  = factor(male_pre, levels = c(0,1), labels = c("Female","Male")),
    Ethnicity = factor(race, levels = c(1,2,3,4,5,6,7),
                       labels = c("White", "Mestizo (European/Indigenous mix)",
                                  "Indigenous (Native American)", "Mixed (Black/White)",
                                  "Black", "Other / Mixed", "Prefer not to say")),
    Education = as.numeric(education_years),
    MaritalStatus = factor(marital_status_pre,
                           levels = c(1,2,3,4,5),
                           labels = c("Married/Partnered","Divorced","Widowed",
                                      "Never married","Prefer not to say")),
    Area = as.factor(Area),
    TipoHogar = as.factor(TipoHogar),
    EscolaridadPadre = as.factor(EscolaridadPadre),
    EscolaridadMadre = as.factor(EscolaridadMadre),
    History_Smoking = as.factor(History_Smoking),
    Alcohol_Consumption = as.factor(Alcohol_Consumption),
    ipaq = as.factor(ipaq),
    BMI       = as.numeric(bmi),
    systolic  = as.numeric(bp_systolic1),
    diastolic = as.numeric(bp_diastolic1),
    APOE      = as.factor(APOE),
    mmse      = as.numeric(mmse_total),
    gds       = as.numeric(gds_total_score),
    glucose   = as.numeric(glucose),
    hba1c     = as.numeric(hba1c),
    colesterol= as.numeric(total_cholesterol),
    ldl       = as.numeric(ldl),
    hdl       = as.numeric(hdl),
    triglycerides = as.numeric(triglycerides),
    hemoglobin    = as.numeric(hemoglobin),
    hematocrit    = as.numeric(hematocrit),
    creatinine    = as.numeric(creatinine),
    urea          = as.numeric(urea),
    vhs           = as.numeric(vhs),
    insulinemia   = as.numeric(insulinemia),
    FRS           = as.factor(Fra_Clase),
    cdr           = as.factor(CDR),
    imm_recalltotal_z = as.numeric(imm_recalltotal),
    delayed_recalltotal_z = as.numeric(delayed_recalltotal),
    FCSRT_imm = as.numeric(totalfreerecall_2),
    FCSRT_dif = as.numeric(totales_tardia),
    wais = as.numeric(score_wais_bruto),
    dig_dir = as.numeric(forwardtotcorrect),
    dig_inv = as.numeric(backwardtotcorrect),
    dig_sec = as.numeric(sequencetotcorrect),
    tmta = as.numeric(tima_trail_a),
    tmtb = as.numeric(tima_trail_b),
    stroop_p = as.numeric(stroop_p),
    stroop_c = as.numeric(stroop_c),
    stroop_pc = as.numeric(stroop_pc),
    cst_a = as.numeric(csta),
    cst_b = as.numeric(cstb),
    cst_c = as.numeric(cstc),
    flu_sem = as.numeric(animaltotcorrect_vc),
    flu_fon_p = as.numeric(p_total_score),
    flu_fon_m = as.numeric(m_total_score)
  )

labels(df_tab1$Arm)               <- "Group"
labels(df_tab1$Age)               <- "Age (years)"
labels(df_tab1$Sex)               <- "Sex"
labels(df_tab1$Ethnicity)         <- "Race/Ethnicity"
labels(df_tab1$Education)         <- "Years of Education"
labels(df_tab1$MaritalStatus)     <- "Marital Status"
labels(df_tab1$Area)              <- "Living Area"
labels(df_tab1$TipoHogar)         <- "House Type"
labels(df_tab1$EscolaridadPadre)  <- "Father highest level of education"
labels(df_tab1$EscolaridadMadre)  <- "Mother highest level of education"
labels(df_tab1$History_Smoking)   <- "Smoking History"
labels(df_tab1$Alcohol_Consumption)<- "Alcohol Consumption by week"
labels(df_tab1$ipaq)              <- "IPAQ Classification"
labels(df_tab1$BMI)               <- "Body Mass Index (kg/m²)"
labels(df_tab1$systolic)          <- "Blood Pressure: Systolic (mmHg)"
labels(df_tab1$diastolic)         <- "Blood Pressure: Diastolic (mmHg)"
labels(df_tab1$APOE)              <- "APOE ε4 carrier"
labels(df_tab1$cdr)               <- "Clinical Dementia Rating"
labels(df_tab1$mmse)              <- "MiniMental State Examination"
labels(df_tab1$gds)               <- "Geriatric Depression Scale"
labels(df_tab1$glucose)           <- "Glucose (mg/dL)"
labels(df_tab1$hba1c)             <- "Hemoglobin A1c (%)"
labels(df_tab1$colesterol)        <- "Total Cholesterol (mg/dL)"
labels(df_tab1$ldl)               <- "LDL cholesterol (mg/dL)"
labels(df_tab1$hdl)               <- "HDL cholesterol (mg/dL)"
labels(df_tab1$triglycerides)     <- "Triglycerides (mg/dL)"
labels(df_tab1$hemoglobin)        <- "Hemoglobin (g/dL)"
labels(df_tab1$hematocrit)        <- "Hematocrit (%)"
labels(df_tab1$creatinine)        <- "Creatinine (mg/dL)"
labels(df_tab1$urea)              <- "Urea (mg/dL)"
labels(df_tab1$vhs)               <- "ESR (mm/hr)"
labels(df_tab1$insulinemia)       <- "Insulin (µIU/mL)"
labels(df_tab1$FRS)               <- "FRS CVD risk and prevalence"

#Cognitivas
labels(df_tab1$imm_recalltotal_z)<- "Logical Memory - Immediate"
labels(df_tab1$delayed_recalltotal_z)<- "Logical Memory - Delayed"
labels(df_tab1$FCSRT_imm)<- "FCSRT Immediate"
labels(df_tab1$FCSRT_dif)<- "FCSRT Delayed"
labels(df_tab1$wais)<- "Clave de Números"
labels(df_tab1$dig_dir)<- "Digit Span Forward"
labels(df_tab1$dig_inv)<- "Digit Span Backward"
labels(df_tab1$dig_sec)<- "Digit Span Sequencing"
labels(df_tab1$tmta)<- "Trail Making Test A"
labels(df_tab1$tmtb)<- "Trail Making Test B"
labels(df_tab1$stroop_p)<- "Stroop P"
labels(df_tab1$stroop_c)<- "Stroop C"
labels(df_tab1$stroop_pc)<- "Stroop PC"
labels(df_tab1$cst_a)<- "Concept Shifting Test A"
labels(df_tab1$cst_b)<- "Concept Shifting Test B"
labels(df_tab1$cst_c)<- "Concept Shifting Test C"
labels(df_tab1$flu_sem)<- "Semantic Fluency"
labels(df_tab1$flu_fon_p)<- "Phonological Fluency P"
labels(df_tab1$flu_fon_m)<- "Phonological Fluency m"


ctrl <- tableby.control(
  test = TRUE, total = TRUE, na.include = FALSE,
  numeric.stats = c("meansd","medianq1q3","range"),
  cat.stats     = c("countpct"),
  stats.labels  = list(meansd = "Mean (SD)",
                       medianq1q3 = "Median [Q1–Q3]",
                       range = "Range"),
  digits = 1, digits.p = 3
)

tab_A <- tableby(
  Arm ~ Age + Sex + Ethnicity + Education + MaritalStatus +
    Area + TipoHogar + EscolaridadPadre + EscolaridadMadre +
    History_Smoking + Alcohol_Consumption + ipaq,
  data = df_tab1, control = ctrl
)
sum_A <- summary(
  tab_A,
  title = sprintf("A. Demographics, Socio-living & Lifestyle (Flexible N=%s, Systematic N=%s, Total N=%s)",
                  N_flex, N_sys, N_tot)
)

tab_B <- tableby(
  Arm ~ BMI + systolic + diastolic +
    glucose + hba1c + colesterol + ldl + hdl + triglycerides +
    hemoglobin + hematocrit + creatinine + urea + vhs + insulinemia + FRS +
    APOE + mmse + gds + cdr,
  data = df_tab1, control = ctrl
)
sum_B <- summary(tab_B, title = "B. Clinical & Laboratory")

tab_C <- tableby(
  Arm ~ imm_recalltotal_z + delayed_recalltotal_z +
    FCSRT_imm + FCSRT_dif + wais +
    dig_dir + dig_inv + dig_sec +
    tmta + tmtb +
    stroop_p + stroop_c + stroop_pc +
    cst_a + cst_b + cst_c +
    flu_sem + flu_fon_p + flu_fon_m,
  data = df_tab1, control = ctrl
)
sum_C <- summary(tab_C, title = "C. Cognitive Measures")

write2(
  list(sum_A, sum_B, sum_C),
  file  = "Sociodemograficos/Table1.html",
  title = "Table 1. Baseline characteristics by group")

# Abrir en el navegador
browseURL(normalizePath("Sociodemograficos/Table1.html"))


