## ----tlc_p, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, fig.align='center', out.width="80%"----
# ----------------------------------------------------
# Carregando pacotes do R

library(here)
library(haven)
library(tidyr)
library(ggplot2)

# ----------------------------------------------------
# Carregando o arquivo de dados

chumbo.df <- read_dta(file = here::here("data", "tlc.dta"))

chumbo.df$trt <- factor(chumbo.df$trt, labels = c("Placebo", "Succimer"))

# ----------------------------------------------------
# Reformando o objeto: de "largo" para "longo"

chumbo.df.longo <- gather(data = chumbo.df, key = "tempo", value = "chumbo", -id, -trt)

# ----------------------------------------------------
# Formata variáveis

chumbo.df.longo$tempo <- factor(chumbo.df.longo$tempo, labels = c("0", "1", "4", "6"))
chumbo.df.longo$tempo.num <- as.numeric(as.character(chumbo.df.longo$tempo))

# ----------------------------------------------------

library(plyr)

chumbo.resumo <- ddply(chumbo.df.longo, ~ trt + tempo.num, summarize, chumbo.m = mean(chumbo))

p <- ggplot(data = chumbo.resumo,
            mapping = aes(x = tempo.num, y = chumbo.m, group = trt, colour = trt)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = c(0, 1, 4, 6)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Semana",
       y = expression("Média nível de chumbo no sangue"~(mu*g/dL)),
       colour = "Grupo de tratamento") +
  theme_bw() +
  theme(legend.position = "bottom")
p








## ----tlc_perfis, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, fig.align='center', out.width="80%"----

p



## ----factor, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------------------
class(chumbo.df.longo$tempo)
class(chumbo.df.longo$trt)


## ----gls, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------------------------------

library(nlme)

# modelo de perfis de respostas
# com matriz de covariância não estruturada
mod.pr <- gls(chumbo ~ trt * tempo,
                corr = corSymm(form = ~ 1 | id),
                weights = varIdent(form = ~ 1 | tempo),
                method = "REML",
                data = chumbo.df.longo)



## ----summary.gls, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE------------------------

summary(mod.pr)



## ----cov_est, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------------------------

getVarCov(mod.pr)



## ----cov_est2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------------------------

knitr::kable(x = matrix(getVarCov(mod.pr),
                        ncol = 4),
             digits = 1)



## ----wald, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-------------------------------
library(car)

Anova(mod.pr)



## ----wald2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE------------------------------

knitr::kable(
  Anova(mod.pr),
  digits = c(0, 2, 4))



## ----trv, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------------------------------

mod.comp <- gls(chumbo ~ trt * tempo,
                corr = corSymm(form = ~ 1 | id),
                weights = varIdent(form = ~ 1 | tempo),
                method = "ML",
                data = chumbo.df.longo)

mod.red <- gls(chumbo ~ trt + tempo,
                corr = corSymm(form = ~ 1 | id),
                weights = varIdent(form = ~ 1 | tempo),
                method = "ML",
                data = chumbo.df.longo)



## ----trv2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-------------------------------

anova(mod.comp, mod.red)



## ----coef, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-------------------------------
knitr::kable(
  summary(mod.pr)$tTable[,-4],
  digits = c(3, 3, 2),
  col.names = c("Estimativa", "EP", "Z"))

