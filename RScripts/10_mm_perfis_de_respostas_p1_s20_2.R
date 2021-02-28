## ----perfis_medios, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"----
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

chumbo.longo <- gather(data = chumbo,
                        key = "tempo",
                        value = "chumbo", -id, -trt)

chumbo.longo$tempo <- as.numeric(
  as.character(
    factor(chumbo.longo$tempo,
           labels = c(0, 1, 4, 6))))

chumbo.longo$trt <- factor(chumbo.longo$trt,
                           labels = c("Placebo",
                                      "Succimer"))

chumbo.resumo <- chumbo.longo %>% 
  group_by(trt, tempo) %>% 
  summarise(chumbo.m = mean(chumbo))

p <- ggplot(data = chumbo.resumo,
            mapping = aes(x = tempo,
                          y = chumbo.m,
                          colour = trt)) +
  geom_point() +
  geom_line() +
  labs(x = "Tempo (semanas)",
       y = "Média do nível de chumbo no sangue (mcg/dL)",
       colour = "Grupo de tratamento")
p








## ----carrega_dados, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE------------------------
# ----------------------------------------------------
# Carregando pacotes do R
library(here)
library(haven)
library(tidyr)
library(ggplot2)
# ----------------------------------------------------
# Carregando o arquivo de dados
here::here("data", "tlc.dta")
chumbo <- read_dta(
  file = here::here("data", "tlc.dta"))


## ----carrega_dados2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------------
chumbo


## ----transforma_dados, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------------------
chumbo.longo <- gather(data = chumbo,
                        key = "tempo",
                        value = "chumbo", -id, -trt)

chumbo.longo


## ----transforma_dados2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------------------
chumbo.longo$tempo <- as.numeric(
  as.character(
    factor(chumbo.longo$tempo,
           labels = c(0, 1, 4, 6))))

chumbo.longo$trt <- factor(chumbo.longo$trt,
                           labels = c("Placebo",
                                      "Succimer"))

chumbo.longo


## ----time_plot3a, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------------------------
library(dplyr)

chumbo.resumo <- chumbo.longo %>% 
  group_by(trt, tempo) %>% 
  summarise(chumbo.m = mean(chumbo))

chumbo.resumo


## ----time_plot3b, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"----
p <- ggplot(data = chumbo.resumo,
            mapping = aes(x = tempo,
                          y = chumbo.m,
                          colour = trt)) +
  geom_point() +
  geom_line() +
  labs(x = "Tempo (semanas)",
       y = "Média do nível de chumbo no sangue (mcg/dL)",
       colour = "Grupo de tratamento")
p


## ----gls, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------------------------------
chumbo.longo$tempo <- factor(chumbo.longo$tempo)

library(nlme)

# matriz de covariância não estruturada
mod.unst <- gls(chumbo ~ trt * tempo,
                corr = corSymm(form = ~ 1 | id),
                weights = varIdent(form = ~ 1 | tempo),
                method = "REML",
                data = chumbo.longo)

summary(mod.unst)


## ----cov_est, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE------------------------------
library(lavaSearch2)

knitr::kable(
  getVarCov2(mod.unst)$Omega,
  digits = 1)


## ----wald, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------------------------------
library(car)

knitr::kable(
  Anova(mod.unst),
  digits = c(0, 2, 4))


## ----coef, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------------------------------
knitr::kable(
  summary(mod.unst)$tTable[,-4],
  digits = c(3, 3, 2),
  col.names = c("Estimativa", "EP", "Z"))


## ----mat.del, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE------------------------------
chumbo.longo <- arrange(chumbo.longo, id)
chumbo.longo

model.matrix(chumbo ~ trt * tempo,
             data = chumbo.longo[chumbo.longo$id == 1,])

model.matrix(chumbo ~ trt * tempo,
             data = chumbo.longo[chumbo.longo$id == 2,])


## ----mat.del2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------------------
model.matrix(chumbo ~ -1 + trt * tempo,
             data = chumbo.longo[chumbo.longo$id == 1,])

model.matrix(chumbo ~ -1 + trt * tempo,
             data = chumbo.longo[chumbo.longo$id == 2,])


## ----mat.del3, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------------------
chumbo.longo$trt <- relevel(chumbo.longo$trt,
                            ref = "Succimer")

chumbo.longo$tempo <- relevel(chumbo.longo$tempo,
                              ref = "6")
model.matrix(chumbo ~ trt * tempo,
             data = chumbo.longo[chumbo.longo$id == 1,])

model.matrix(chumbo ~ trt * tempo,
             data = chumbo.longo[chumbo.longo$id == 2,])

