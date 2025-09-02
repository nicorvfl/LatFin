library(stringr)
library(dplyr)

#--------------------CONVERTIR SEGUNDOS A MINUTOS-------------------------------
ConvertirSegundosTMT <- function(x) {
  v <- as.character(x) #paso todo a caracter
  v <- str_trim(v) #le quito espacios
  v[v %in% c("", "NA", "NaN")] <- NA #unifico NA
  out <- suppressWarnings(as.numeric(v)) #si es un número, se queda en número
  tiene_punto <- !is.na(v) & str_detect(v, ":") #condicional para detectar ":"
  if (!any(tiene_punto)) return(out) #si no tiene punto, número
  partes <- str_split(v[tiene_punto], ":", simplify = TRUE) #creo partes
  nums <- suppressWarnings(apply(partes, 2, as.numeric)) #le aplico as.numeric
  if (is.null(dim(nums))) nums <- matrix(nums, ncol = ncol(partes))
  secs <- rep(NA_real_, nrow(partes)) #me armo una variable vacía
  if (ncol(partes) == 3) {
    mins <- nums[,1]; segs <- nums[,2]; cents <- nums[,3]
    secs <- mins*60 + segs + cents/100 #calculo el tiempo en caso de 3 cols
  } else if (ncol(partes) == 2) {
    mins <- nums[,1]; segs <- nums[,2]
    secs <- mins*60 + segs #calculo el tiempo en caso de 2 cols
  } else {
    secs <- NA_real_
  }
  out[tiene_punto] <- secs
  out
}
#-------------------------------------------------------------------------------

#-----------------------CALCULAR Nº DE EVALUACIONES-----------------------------

CantidadEvaluaciones <- function(data,
                                 id = record_id,
                                 event = Eventos,
                                 eval_cols = c(
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
                                   "totales_tardia","animaltotcorrect_vc","p_total_score","m_total_score"
                                 )) {
  
  eval_present <- intersect(eval_cols, names(data))
  
  data %>%
    ungroup() %>%
    mutate(tiene_eval = if (length(eval_present) == 0) FALSE
           else rowSums(across(all_of(eval_present), ~ !is.na(.))) > 2) %>%
    group_by({{ event }}, {{ id }}) %>%
    summarise(tiene_eval = any(tiene_eval), .groups = "drop_last") %>%
    summarise(
      CantEvaluaciones = sum(tiene_eval),
      TotalSujetos = n(),         
      .groups = "drop")
}


#---------------------------------ESCALAR COGNICIÓN-----------------------------

library(dplyr)

EscalarCognicion <- function(df,
                                 vars,  
                                 group_vars = NULL,
                                 reverse_vars = NULL,
                                 method = c("z","robust_z","minmax"),
                                 suffix = "_z") {
  method <- match.arg(method)
  stopifnot(is.data.frame(df))
  
  vars_present <- intersect(vars, names(df))
  if (length(vars_present) == 0)
    stop("Ninguna variable de 'vars' está en el data frame.")
  
  calc_fun <- switch(
    method,
    z = function(x){
      m <- mean(x, na.rm=TRUE); s <- sd(x, na.rm=TRUE)
      if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
      (x - m)/s},
    robust_z = function(x){
      med <- median(x, na.rm=TRUE)
      md  <- mad(x, constant=1.4826, na.rm=TRUE)
      if (is.na(md) || md == 0) return(rep(NA_real_, length(x)))
      (x - med)/md},
    minmax = function(x){
      r <- range(x, na.rm=TRUE); d <- diff(r)
      if (!is.finite(d) || d == 0) return(rep(NA_real_, length(x)))
      (x - r[1]) / d})
  
  if (!is.null(group_vars)) df <- df %>% group_by(across(all_of(group_vars)))
  
  df %>%
    mutate(across(all_of(vars_present), ~ {
      x <- suppressWarnings(as.numeric(.x))
      
      if (!is.null(reverse_vars) && cur_column() %in% reverse_vars) x <- -x

      calc_fun(x)
    }, .names = "{.col}{suffix}")) %>%
    ungroup()
}
