## ----carrega_dados, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----
# ----------------------------------------------------
# Carregando pacotes do R
library(here)
library(haven)
library(tidyr)
library(ggplot2)
library(dplyr)
# ----------------------------------------------------
# Carregando o arquivo de dados
af <- read_dta(
  file = here::here("data", "exercise.dta"))
af


## ----transforma_dados, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----
names(af)[which(names(af) == "group")] <- "trt"
af.longo <- gather(data = af,
                        key = "tempo",
                        value = "fc", -id, -trt)
af.longo
af.longo <- subset(af.longo, tempo != "y2" & tempo != "y10")

af.longo$dia <- factor(af.longo$tempo,
                       labels = c(0, 12, 4, 6, 8))
af.longo$dia <- factor(af.longo$dia,
                       levels = c("0", "4", "6", "8", "12"))
af.longo$tempo <- as.numeric(
  factor(af.longo$dia))
af.longo$trt <- factor(af.longo$trt)
af.longo


## ----gls_os, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------
af.longo <- as.data.frame(af.longo)
library(nlme)
# matriz de covariância não estruturada
mod1 <- gls(fc ~ trt*dia,
            na.action = na.omit,
            corr = corSymm(form = ~ tempo | id),
            weights = varIdent(form = ~ 1 | tempo),
            method = "REML",
            data = af.longo)
# matriz de covariância autoregressiva
mod2 <- gls(fc ~ trt*dia,
            na.action = na.omit,
            corr = corAR1(form = ~ tempo | id),
            method = "REML",
            data = af.longo)


## ----cov_est, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------
library(lavaSearch2)

knitr::kable(
  getVarCov2(mod1)$Omega,
  digits = 3)


## ----compara, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------
anova(mod1, mod2)

