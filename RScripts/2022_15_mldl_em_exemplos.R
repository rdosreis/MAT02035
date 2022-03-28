## ----fev_carrega_dados, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------------------------

library(haven)
# library(foreign)
# library(readstata13)

fev <- read_dta(file = here::here("data", "fev1.dta"))

# cria variável log(fev1/ht)
fev$fev1 <- exp(fev$logfev1)
fev$logfh <- log(fev$fev1/fev$ht)

# uma observação atípica (?)
fev <- fev[- which(fev$logfh < -0.5), ]

names(fev) <- c("id", "altura", "idade", "altura_basal",
                "idade_basal", "logFEV_1", "FEV_1", "logfh")



## ----fev, echo=FALSE, eval=TRUE, results='asis', message=FALSE, warning=FALSE--------------------------------

knitr::kable(x = fev[which(fev$id %in% 13:16),],
             align = "c")



## ----fev3, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"-----------

library(ggplot2)

set.seed(5)
id.s <- sample(x = unique(fev$id), size = 50,
               replace = FALSE)

p <- ggplot(data = fev[which(fev$id %in% id.s),],
            mapping = aes(x = idade, y = logfh,
                          group = id)) +
  geom_point(alpha = 0.3) +
  geom_line(alpha = 0.3) +
  labs(x = "Idade (anos)",
       y = "Log(FEV1/Altura)") +
  theme_bw()
p



## ----lme, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-------------------------------------------------

library(nlme)

mod1 <- lme(fixed = logFEV_1 ~ idade + log(altura) +
              idade_basal + log(altura_basal),
            random = ~ idade | id,
            # method = "REML",
            data = fev,
            na.action = na.omit)



## ----lmer, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE------------------------------------------------

# library(lme4)
# 
# mod <- lmer(formula = logFEV_1 ~ idade + log(altura) +
#               idade_basal + log(altura_basal) +
#               (1 + idade | id),
#             REML = TRUE,
#             data = fev,
#             na.action = na.omit)



## ----coef, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE-----------------------------------------------

knitr::kable(
  summary(mod1)$tTable[,-c(3,5)],
  digits = c(4, 4, 2),
  col.names = c("Estimativa", "EP", "Z"))



## ----cov_est, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------------------------------------------

getVarCov(mod1, type = "random.effects")
summary(mod1)$sigma^2
VarCorr(mod1)
 



## ----corr_marginal, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE--------------------------------------

knitr::kable(
  x = cov2cor(getVarCov(mod1, type = "marginal", individuals = 18)[[1]]),
  row.names = F, col.names = 7:18,
  digits = 2)



## ----lme_mod2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------

mod2 <- lme(fixed = logFEV_1 ~ idade + log(altura) +
              idade_basal + log(altura_basal),
            random = ~ log(altura) | id,
            # method = "REML",
            data = fev,
            na.action = na.omit)



## ----coef_mod2, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE------------------------------------------

knitr::kable(
  summary(mod2)$tTable[,-c(3,5)],
  digits = c(4, 4, 2),
  col.names = c("Estimativa", "EP", "Z"))



## ----mod1_vs_mod2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------------------------------------

logLik(mod1)
logLik(mod2)



## ----lme_mod3, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------

mod3 <- lme(fixed = logFEV_1 ~ idade + log(altura) +
              idade_basal + log(altura_basal),
            random = ~ idade + log(altura) | id,
            # method = "REML",
            data = fev,
            na.action = na.omit)
 
logLik(mod3)



## ----mod2_vs_mod3, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----------------------------------------

library(varTestnlme)

vt <- varCompTest(m1 = mod3, m0 = mod2)

library(EnvStats)

print(vt)

summary(vt)


