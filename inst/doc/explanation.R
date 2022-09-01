## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
library(nphRCT)
library(dplyr)
library(survival)
library(ggplot2)

data("moderate_cross")
dat <- moderate_cross


km <- survfit(Surv(time, event) ~ arm,
                  data = dat)

p_km <- survminer::ggsurvplot(km, 
                      data = dat, 
                      risk.table = TRUE, 
                      break.x.by = 6,
                      legend.title = "",
                      xlab = "Time (months)",
                      ylab = "Overall survival",
                      risk.table.fontsize = 4,
                      legend = c(0.8,0.8))

df_lr <- find_scores(formula=Surv(time, event) ~ arm, 
                     data=dat,
                     method = "lr")

p_lrt <- plot(df_lr,title="Log rank")

cowplot::plot_grid(p_km[[1]],p_lrt,rel_widths=c(2,3))

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
df_rmst_pseudo <- find_scores(formula=Surv(time, event) ~ arm, 
                     tau=18,
                     data=dat,
                     method = "rmst")
p_rmst <- plot(df_rmst_pseudo, title = "RMST")
cowplot::plot_grid(p_km[[1]],p_rmst,rel_widths=c(2,3))

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
df_milestone_pseudo <- find_scores(formula=Surv(time, event) ~ arm, 
                     tau=12,
                     data=dat,
                     method = "ms")
p_surv <- plot(df_milestone_pseudo, title = "Milestone")
cowplot::plot_grid(p_km[[1]],p_surv,rel_widths=c(2,3))

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
df_mwlrt <- find_scores(formula=Surv(time, event) ~ arm, 
                     data=dat,
                     t_star=9,
                     method = "mw")
p_mwlrt <- plot(df_mwlrt,title="MWLRT")
cowplot::plot_grid(p_km[[1]],p_mwlrt,rel_widths=c(2,3))

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
cowplot::plot_grid(p_rmst, p_lrt,p_mwlrt,p_surv, nrow = 2)

