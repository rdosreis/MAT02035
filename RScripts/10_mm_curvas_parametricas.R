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
chumbo <- read_dta(
  file = here::here("data", "tlc.dta"))


## ----transforma_dados, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----
chumbo.longo <- gather(data = chumbo,
                        key = "tempo",
                        value = "chumbo", -id, -trt)
chumbo.longo$semana <- as.numeric(
  as.character(
    factor(chumbo.longo$tempo,
           labels = c(0, 1, 4, 6))))
chumbo.longo$tempo <- as.numeric(
  factor(chumbo.longo$semana))
chumbo.longo$trt <- factor(chumbo.longo$trt,
                           labels = c("Placebo",
                                      "Succimer"))


## ----time_plot3a, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----
chumbo.resumo <- chumbo.longo %>% 
  group_by(trt, semana) %>% 
  summarise(chumbo.m = mean(chumbo))
p <- ggplot(data = chumbo.resumo,
            mapping = aes(x = semana,
                          y = chumbo.m,
                          colour = trt)) +
  geom_point() +
  geom_line() +
  labs(x = "Tempo (semanas)",
       y = "Média do nível de chumbo no sangue (mcg/dL)",
       colour = "Grupo de tratamento")
p


## ----gls_os, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------
chumbo.longo <- as.data.frame(chumbo.longo)
library(nlme)
# matriz de covariância não estruturada
mod.unst <- gls(chumbo ~ semana + I( (semana - 1) * (semana >= 1) ) +
                  semana:trt + I( (semana - 1) * (semana >= 1) ):trt,
                corr = corSymm(form = ~ tempo | id),
                weights = varIdent(form = ~ 1 | tempo),
                method = "REML",
                data = chumbo.longo)
summary(mod.unst)


## ----gls_ls, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------
library(lspline)
# matriz de covariância não estruturada
mod.unst <- gls(chumbo ~ lspline(x = semana,
                                 knots = 1,
                                 marginal = TRUE) +
                  lspline(x = semana,
                          knots = 1,
                          marginal = TRUE):trt,
                corr = corSymm(form = ~ tempo | id),
                weights = varIdent(form = ~ 1 | tempo),
                method = "REML",
                data = chumbo.longo)
summary(mod.unst)


## ----coef, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE------------
knitr::kable(
  summary(mod.unst)$tTable[,-4],
  digits = c(3, 3, 2),
  col.names = c("Estimativa", "EP", "Z"))


## ----coef_2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------
library(ggeffects)

mydf <- ggpredict(mod.unst, terms = c("semana", "trt"))
ggplot(mydf, aes(x, predicted, colour = group)) +
  geom_line() +
  labs(x = "Tempo (semanas)",
       y = "Média ajustada do nível de chumbo no sangue (mcg/dL)",
       colour = "Grupo de tratamento")


## ----cov_est, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------
library(lavaSearch2)

knitr::kable(
  getVarCov2(mod.unst)$Omega,
  digits = 1)

