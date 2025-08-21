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
    BMI       = as.numeric(bmi),
    systolic  = as.numeric(bp_systolic1),
    diastolic = as.numeric(bp_diastolic1),
    hba1c     = as.numeric(hba1c),
    colesterol= as.numeric(total_cholesterol),
    ldl       = as.numeric(ldl),
    hdl       = as.numeric(hdl),
    glucose   = as.numeric(glucose),
    triglycerides = as.numeric(triglycerides),
    hemoglobin    = as.numeric(hemoglobin),
    creatinine    = as.numeric(creatinine),
    urea          = as.numeric(urea),
    hematocrit    = as.numeric(hematocrit),
    vhs           = as.numeric(vhs),          
    insulinemia   = as.numeric(insulinemia)    
  )

# ---- 3) Labels (inglés + unidades consistentes)
labels(df_tab1$Arm)           <- "Group"
labels(df_tab1$Age)           <- "Age (years)"
labels(df_tab1$Sex)           <- "Sex"
labels(df_tab1$Ethnicity)     <- "Race/Ethnicity"
labels(df_tab1$Education)     <- "Years of Education"
labels(df_tab1$MaritalStatus) <- "Marital Status"
labels(df_tab1$BMI)           <- "Body Mass Index (kg/m²)"
labels(df_tab1$systolic)      <- "Blood Pressure: Systolic (mmHg)"
labels(df_tab1$diastolic)     <- "Blood Pressure: Diastolic (mmHg)"
labels(df_tab1$hba1c)         <- "Hemoglobin A1c (%)"
labels(df_tab1$colesterol)    <- "Total Cholesterol (mg/dL)"
labels(df_tab1$ldl)           <- "LDL cholesterol (mg/dL)"
labels(df_tab1$hdl)           <- "HDL cholesterol (mg/dL)"  
labels(df_tab1$glucose)       <- "Glucose (mg/dL)"
labels(df_tab1$triglycerides) <- "Triglycerides (mg/dL)"
labels(df_tab1$hemoglobin)    <- "Hemoglobin (g/dL)"         
labels(df_tab1$creatinine)    <- "Creatinine (mg/dL)"
labels(df_tab1$urea)          <- "Urea (mg/dL)"
labels(df_tab1$hematocrit)    <- "Hematocrit (%)"
labels(df_tab1$vhs)           <- "ESR (mm/hr)"              
labels(df_tab1$insulinemia)   <- "Insulin (µIU/mL)"         

ctrl <- tableby.control(
  test = TRUE, total = TRUE, na.include = FALSE,
  numeric.stats = c("meansd","medianq1q3","range"),
  cat.stats     = c("countpct"),
  stats.labels  = list(meansd = "Mean (SD)",
                       medianq1q3 = "Median [Q1–Q3]",
                       range = "Range"),
  digits = 1, digits.p = 3
)

tab_demo <- tableby(
  Arm ~ Age + Sex + Ethnicity + Education + MaritalStatus +
    BMI + systolic + diastolic,
  data = df_tab1, control = ctrl
)
sum_demo <- summary(
  tab_demo,
  title = sprintf("A. Demographics & Clinical (Flexible N=%s, Systematic N=%s, Total N=%s)",
                  N_flex, N_sys, N_tot)
)

tab_cardio <- tableby(
  Arm ~ glucose + hba1c + colesterol + ldl + hdl + triglycerides,
  data = df_tab1, control = ctrl
)
sum_cardio <- summary(tab_cardio, title = "B. Cardiometabolic")

tab_hema_renal <- tableby(
  Arm ~ hemoglobin + hematocrit + creatinine + urea + vhs + insulinemia,
  data = df_tab1, control = ctrl
)
sum_hema_renal <- summary(tab_hema_renal, title = "C. Hematologic & Renal/Others")

#------------------------

sum_demo      <- summary(tab_demo,      title = sprintf("A. Demographics & Clinical (Flexible N=%s, Systematic N=%s, Total N=%s)", N_flex, N_sys, N_tot))
sum_cardio    <- summary(tab_cardio,    title = "B. Cardiometabolic")
sum_hema_renal<- summary(tab_hema_renal,title = "C. Hematologic & Renal/Others")
write2(list(sum_demo, sum_cardio, sum_hema_renal),
       file  = "table1.1.html",
       title = "Table 1. Baseline characteristics by group")

browseURL(normalizePath("table1.1.html"))


