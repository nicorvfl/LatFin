library(dplyr)
library(arsenal)

out_dir  <- file.path(getwd(), "Sociodemograficos")
out_html <- file.path(out_dir, "Table1.html")
dir.create(out_dir, showWarnings = FALSE)

df_base <- data %>%
  filter(Eventos == "base") %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(Arm = factor(Arm))

df_tab1 <- df_base %>%
  transmute(
    Arm,
    
    Age  = as.numeric(age),
    Sex  = factor(male_pre, levels = c(0, 1), labels = c("Female", "Male")),
    EducationLevel = as.factor(EducationLevel),
    Ethnicity = factor(
      race,
      levels = c(1,2,3,4,5,6,7),
      labels = c("White",
                 "Mestizo (European/Indigenous mix)",
                 "Indigenous (Native American)",
                 "Mulato",
                 "Black",
                 "Other / Mixed",
                 "Prefer not to say")
    ),
    Area = as.factor(Area),
    TipoHogar = as.factor(TipoHogar),
    MaritalStatus = factor(
      marital_status_pre,
      levels = c(1,2,3,4,5),
      labels = c("Married/Partnered", "Divorced", "Widowed",
                 "Never married", "Prefer not to say")
    ),
    
    BMI        = as.numeric(bmi),
    systolic   = as.numeric(bp_systolic1),
    hemoglobin = as.numeric(hemoglobin),
    colesterol = as.numeric(total_cholesterol),
    hba1c      = as.numeric(hba1c),
    glucose    = as.numeric(glucose),
    insulinemia= as.numeric(insulinemia),
    
    mmse = as.numeric(mmse_total),
    CDR  = as.factor(CDR),
    APOE = as.factor(APOE),
    gds = as.numeric(gds_total_score),
    framingham = as.factor(Fra_Clase),
    fam_problems = as.factor(fam_problems),
    fra_score = fra_score
  )

labels(df_tab1$Arm) <- "Group"

labels(df_tab1$Age)  <- "Age (years), mean (SD)"
labels(df_tab1$BMI)  <- "Body Mass Index (kg/m²), mean (SD)"
labels(df_tab1$systolic)   <- "Blood Pressure: Systolic (mmHg), mean (SD)"
labels(df_tab1$hemoglobin) <- "Hemoglobin (g/dL), mean (SD)"
labels(df_tab1$colesterol) <- "Total Cholesterol (mg/dL), mean (SD)"
labels(df_tab1$hba1c)      <- "Hemoglobin A1c (%), mean (SD)"
labels(df_tab1$glucose)    <- "Glucose (mg/dL), mean (SD)"
labels(df_tab1$insulinemia)<- "Insulin (µIU/mL), mean (SD)"
labels(df_tab1$mmse)       <- "MiniMental State Examination, mean (SD)"

labels(df_tab1$Sex)            <- "Sex"
labels(df_tab1$EducationLevel) <- "Educational Level"
labels(df_tab1$Ethnicity)      <- "Race/Ethnicity"
labels(df_tab1$Area)           <- "Living Area"
labels(df_tab1$TipoHogar)      <- "House Type"
labels(df_tab1$MaritalStatus)  <- "Marital Status"
labels(df_tab1$CDR)            <- "Clinical Dementia Rating"
labels(df_tab1$APOE)           <- "APOE-ε4 carrier"
labels(df_tab1$gds)            <- "Geriatric Depression Scale, mean (SD)"
labels(df_tab1$framingham)     <- "FRS CVD"
labels(df_tab1$fam_problems)   <- "Family History of Memory Loss"

ctrl <- tableby.control(
  test = TRUE,
  total = FALSE,
  na.include = FALSE,
  numeric.stats = "meansd",
  cat.stats = "countpct",
  digits = 1,
  numeric.simplify = TRUE,
  digits.p = 2
)


summary_1row <- function(tab, title) {
  s3 <- getS3method("summary", "tableby")
  f  <- names(formals(s3))
  
  candidates <- c("simplify", "simple", "collapse", "numeric.simplify")
  arg <- candidates[candidates %in% f][1]
  
  if (!is.na(arg)) {
    do.call(s3, c(list(object = tab, title = title),
                  setNames(list(TRUE), arg)))
  } else {
    s3(tab, title = title)
  }
}


tab_A <- tableby(
  Arm ~ 
    #Sociodemográficos
    Age + Sex + EducationLevel + Ethnicity +
    #Laboratorio
    BMI + systolic + hemoglobin + colesterol + hba1c +
    framingham +
    # Clínico
    mmse + CDR + fam_problems + APOE + gds,
  data = df_tab1,
  control = ctrl)

sum_A <- summary_1row(tab_A, "A. Sociodemographic characteristics")


tab_B <- tableby(
  Arm ~ BMI + systolic + hemoglobin + colesterol + hba1c+
    framingham,
  data = df_tab1,
  control = ctrl
)
sum_B <- summary_1row(tab_B, "B. Cardiovascular factors")

tab_C <- tableby(
  Arm ~ mmse + CDR + APOE + gds,
  data = df_tab1,
  control = ctrl
)
sum_C <- summary_1row(tab_C, "C. Clinical status")


write2(
  list(sum_A, sum_B, sum_C),
  file  = out_html,
  title = "Table 1. Baseline characteristics by group"
)

# ------------------------------------------------------------------
# CSS (p-value prolijo y columnas fijas)
# ------------------------------------------------------------------
css <- "
<style>
body{
  font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Arial,sans-serif;
  font-size:13px;
  color:#111;
}

/* Tabla base */
table{
  border-collapse:collapse;
  width:100% !important;
  table-layout:fixed !important;
  margin-bottom:24px;
}

th, td{
  padding:4px 4px !important;
  vertical-align:top;
}

/* Encabezados */
th{
  background-color:#f0f0f0;
  font-weight:700;
  border-bottom:2px solid #d0d0d0;
}

/* Alternancia filas */
tbody tr:nth-child(even) td{ background-color:#f8f8f8; }
tbody tr:nth-child(odd)  td{ background-color:#ffffff; }

/* Filas de sección (Sex, Education, etc.) */
tr td[colspan]{
  background-color:#e9eef3 !important;
  font-weight:700;
  border-top:1px solid #cfd8e3;
}

/* =======================================================
   COLUMNAS
   ======================================================= */

/* Columna 1 – MUY angosta, texto largo en vertical */
th:nth-child(1), td:nth-child(1){
  width:12%;
  text-align:left;
  white-space:normal !important;
  word-break:break-word !important;
  overflow-wrap:anywhere !important;
  hyphens:auto;
}

/* Columnas de grupo */
th:nth-child(2), td:nth-child(2){
  width:30%;
  text-align:right;
}

th:nth-child(3), td:nth-child(3){
  width:30%;
  text-align:right;
}

/* Columna p value */
th:nth-child(4), td:nth-child(4){
  width:8%;
  text-align:right;
  white-space:nowrap;
}

/* p value SOLO cuando existe */
td:nth-child(4):empty{
  color:transparent;
}
</style>
"


# ------------------------------------------------------------------
# Inyectar CSS en el HTML
# ------------------------------------------------------------------
html <- readLines(out_html, warn = FALSE)
html <- sub("</head>", paste0(css, "\n</head>"), html)
writeLines(html, out_html)