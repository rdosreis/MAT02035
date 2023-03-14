## ----carrega-dados, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE----------------
# ----------------------------------------------------
# Carregando pacotes do R

library(here)
library(haven)
library(tidyr)
library(ggplot2)

# ----------------------------------------------------
# Carregando o arquivo de dados

af <- read_dta(file = here::here("data",
                                 "exercise.dta"))

names(af)[which(names(af) == "group")] <- "trt"

# ----------------------------------------------------
# Reformando o objeto: de "largo" para "longo"

af.longo <- gather(data = af,
                        key = "tempo",
                        value = "fc", -id, -trt)

# ----------------------------------------------------
# Filtra dados

af.longo <- subset(x = af.longo,
                   subset = tempo != "y2" & tempo != "y10")

# ----------------------------------------------------
# Formata variáveis

af.longo$dia <- factor(af.longo$tempo,
                       labels = c(0, 12, 4, 6, 8))

af.longo$dia <- factor(af.longo$dia,
                       levels = c("0", "4", "6", "8", "12"))

af.longo$tempo <- as.numeric(af.longo$dia)

af.longo$trt <- factor(af.longo$trt)



## ----gls_ne, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE------------------------

library(nlme)

# matriz de covariância não estruturada
mod1 <- gls(fc ~ trt*dia,
            na.action = na.omit,
            
            corr = corSymm(form = ~ tempo | id),
            weights = varIdent(form = ~ 1 | tempo),
            
            method = "REML",
            data = af.longo)



## ----cov_est, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------------

library(lavaSearch2)

knitr::kable(
  getVarCov2(mod1)$Omega,
  digits = 3)



## ----cov_est2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------------------

mod1$modelStruct$corStruct



## ----gls_ar1, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------------

# library(nlme)

# matriz de covariância autorregressiva
mod2 <- gls(fc ~ trt*dia,
            na.action = na.omit,
            
            corr = corAR1(form = ~ tempo | id),
            
            method = "REML",
            data = af.longo)



## ----cor_est, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------------

summary(mod2)$sigma^2

coef(mod2$modelStruct$corStruct,
     uncons = FALSE, allCoef = TRUE)



## ----cor_est2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------------------

getVarCov(mod2)/summary(mod2)$sigma^2



## ----gls_exp, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------------

af.longo$dia.num <- as.numeric(as.character(af.longo$dia))

# library(nlme)

# matriz de covariância exponencial
mod3 <- gls(fc ~ trt*dia,
            na.action = na.omit,
            
            corr = corExp(form = ~ dia.num | id),
            
            method = "REML",
            data = af.longo)



## ----cor_est_exp, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-------------------

summary(mod3)$sigma^2

getVarCov(mod3)/summary(mod3)$sigma^2



## ----compara, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------------

anova(mod1, mod2, mod3, test = FALSE)



## ----compara2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------------------

anova(mod1, mod2)



## ----compara3, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------------------

anova(mod1, mod3)


