## ----carrega_dados, echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE, results='asis'----
# ---------------------------------------------------
# Carregando pacotes do R

library(haven)
library(tidyr)
library(dplyr)
library(nlme)

# ---------------------------------------------------
# Carregando o arquivo de dados

thks <- read_dta(file = here::here("data", "tvsfp.dta"))

# ---------------------------------------------------
# Formata dados

thks$curriculum <- factor(thks$curriculum)
thks$tvprevent <- factor(thks$tvprevent)



## ----dados, echo=TRUE, eval=TRUE-----------------------------------------------------

thks



## ----ajuste, echo=TRUE, eval=TRUE----------------------------------------------------

mod1 <- lme(
  fixed = postscore ~ prescore + curriculum*tvprevent,
            random = ~1 | sid/cid, # (turmas aninhadas em escolas)
            data = thks,
            na.action = na.omit)



## ----coef, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE-----------------------

knitr::kable(
  summary(mod1)$tTable[,-c(3,5)],
  digits = c(4, 4, 2),
  col.names = c("Estimativa", "EP", "Z"),
  format = "pandoc")



## ----cov_est, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------------------

VarCorr(mod1)



## ----ajuste2, echo=TRUE, eval=TRUE---------------------------------------------------

mod2 <- lme(
  fixed = postscore ~ prescore + curriculum + tvprevent,
            random = ~1 | sid/cid,
            data = thks,
            na.action = na.omit)



## ----coef2, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE----------------------

knitr::kable(
  summary(mod2)$tTable[,-c(3,5)],
  digits = c(4, 4, 2),
  col.names = c("Estimativa", "EP", "Z"),
  format = "pandoc")



## ----ajuste3, echo=TRUE, eval=TRUE---------------------------------------------------

mod3 <- lm(
  formula = postscore ~ prescore + curriculum*tvprevent,
            data = thks)



## ----coef3, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE----------------------

knitr::kable(
  summary(mod3)$coef[,-4],
  digits = c(4, 4, 2),
  col.names = c("Estimativa", "EP", "Z"),
  format = "pandoc")


