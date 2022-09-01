## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----1------------------------------------------------------------------------
library(nphRCT)
set.seed(1)
sim_data <- sim_events_delay(
  event_model=list(
    duration_c = 36,
    duration_e = c(12 ,24),
    lambda_c = log(2)/9,
    lambda_e = c(log(2)/9,log(2)/18)),
  recruitment_model=list(
    rec_model="power",
    rec_period=12,
    rec_power=1),
  n_c=5,
  n_e=5,
  max_cal_t = 36
)
sim_data

## ----2------------------------------------------------------------------------
find_at_risk(formula=Surv(event_time,event_status)~group,
  data=sim_data,
  include_cens=FALSE)

## ----3------------------------------------------------------------------------
find_weights(formula=Surv(event_time,event_status)~group,
  data=sim_data,
  method="lr",
  include_cens = FALSE)

## ----4------------------------------------------------------------------------
find_weights(formula=Surv(event_time,event_status)~group,
  data=sim_data,
  method="fh",
  rho = 0,
  gamma= 1,
  include_cens = FALSE)

## ----5------------------------------------------------------------------------
find_weights(formula=Surv(event_time,event_status)~group,
  data=sim_data,
  method="mw",
  s_star = 0.5,
  include_cens = FALSE)

## ----6------------------------------------------------------------------------
wlrt(formula=Surv(event_time,event_status)~group,
  data=sim_data,
  method="mw",
  s_star = 0.5)

## ----7------------------------------------------------------------------------
df_scores_mw<-find_scores(formula=Surv(event_time,event_status)~group,
  data=sim_data,
  method="mw",
  s_star = 0.5)
plot(df_scores_mw)
df_scores_fh<-find_scores(formula=Surv(event_time,event_status)~group,
  data=sim_data,
  method="fh",
  rho = 0,
  gamma=1)
plot(df_scores_fh)

## ----8------------------------------------------------------------------------
sim_data_0 <- sim_data
sim_data_0$ecog=0
sim_data_1 <- sim_events_delay(
  event_model=list(
    duration_c = 36,
    duration_e = c(6,30),
    lambda_c = log(2)/6,
    lambda_e = c(log(2)/6,log(2)/12)),
  recruitment_model=list(
    rec_model="power",
    rec_period=12,
    rec_power=1),
  n_c=5,
  n_e=5,
  max_cal_t = 36
)
sim_data_1$ecog=1
sim_data_strata<-rbind(sim_data_0,sim_data_1)
wlrt(formula=Surv(event_time,event_status)~group+strata(ecog),
  data=sim_data_strata,
  method="mw",
  t_star = 4
)

