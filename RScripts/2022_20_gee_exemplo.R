## ----geeglm_sintax, echo=TRUE, eval=FALSE------------------------------------------------------
## geeglm(formula, family = gaussian, data, id,
##        zcor = NULL, constr, std.err = "san.se")


## ----esticon_sintax, echo=TRUE, eval=FALSE-----------------------------------------------------
## 
## esticon(obj, cm, beta0, joint.test = FALSE)
## 


## ----ex_ohio_1, echo=TRUE, eval=FALSE----------------------------------------------------------
## # Instala e carrega os pacotes geepack e doBy
## install.packages("geepack")
## install.packages("doBy")
library(geepack)
library(doBy)
## # conjunto de dados ohio do geepack - Efeito da poluição do ar na saúde
## # Crianças acompanhadas por quatro anos, com chiado registrado anualmente
data(ohio) # carrega o conjunto de dados
head(ohio)
## str(ohio)
## # Variável resposta é binária - ajuste um modelo GEE logístico
## # tempo (idade; age) como var. contínua
fit.exch <- geeglm(resp ~ age + smoke,
                   family = binomial(link = "logit"),
                   data = ohio, id = id,
                   corstr = "exchangeable", std.err = "san.se")
fit.unstr <- geeglm(resp ~ age + smoke,
                    family = binomial(link = "logit"),
                    data = ohio, id = id,
                    corstr = "unstructured", std.err = "san.se")
summary(fit.exch)
summary(fit.unstr)


## ----ex_ohio_2, echo=TRUE, eval=FALSE----------------------------------------------------------
## # tempo (idade; age) como var. categórica
## fit <- geeglm(resp ~ factor(age) + smoke,
##               family = binomial(link = "logit"),
##               data = ohio, id = id,
##               corstr = "exchangeable", std.err = "san.se")
## summary(fit)
## # Teste o efeito de smoke usando anova()
## fit1 <- geeglm(resp ~ factor(age) + smoke,
##                family = binomial(link = "logit"),
##                data = ohio, id = id,
##                corstr = "exchangeable", std.err = "san.se")
## fit2 <- geeglm(resp ~ factor(age),
##                family = binomial(link = "logit"),
##                data = ohio, id = id,
##                corstr = "exchangeable", std.err = "san.se")
## anova(fit1, fit2)
## # Teste Wald individual e intervalo de confiança para cada parâmetro
## est <- esticon(fit, diag(5))
## # Odds ratio and confidence intervals
## OR.CI <- exp(cbind(est$estimate, est$lwr, est$upr))
## rownames(OR.CI) <- names(coef(fit))
## colnames(OR.CI) <- c("OR", "OR 95% LI", "OR 95% LS")


## ----ex_ohio_3, echo=TRUE, eval=FALSE----------------------------------------------------------
## # Razão de chances de chiado no peito para uma criança de 9 anos com uma mãe que
## # fumou durante o primeiro ano do estudo em  comparação com uma criança de 8
## # anos com uma mãe que não fumou  durante o primeiro ano do estudo.
## # Isto é, estimar [smoke+factor(age)0] - [factor(age)-1]
## esticon(fit, c(0,-1,1,0,1))
## exp(.Last.value$estimate)
## # 9 anos de idade com mãe que fumava tem maior risco de chiado no peito
## # Teste conjuntamente os efeitos usando esticon()
## fit <- geeglm(resp ~ factor(age)*smoke,
##               family = binomial(link = "logit"),
##               data = ohio, id = id,
##               corstr = "exchangeable", std.err = "san.se")
## summary(fit)
## L <- cbind(matrix(0, nrow=3, ncol=5), diag(3))
## esticon(fit, L, joint.test=TRUE)
## # Também poderia usar anova()
## fit1 <- geeglm(resp ~ factor(age)*smoke,
##                family = binomial(link = "logit"),
##                data = ohio, id = id,
##                corstr = "exchangeable", std.err = "san.se")
## fit2 <- geeglm(resp ~ factor(age) + smoke,
##                family = binomial(link = "logit"),
##                data = ohio, id = id,
##                corstr = "exchangeable", std.err = "san.se")
## anova(fit1, fit2)


## ----antib, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, results='asis'----------------
# ---------------------------------------------------
# Carregando pacotes do R

library(haven)
library(tidyr)
library(dplyr)
library(compareGroups)
library(geepack)
library(doBy)

# ---------------------------------------------------
# Carregando o arquivo de dados

ds <- read_dta(file = here::here("data", "leprosy.dta"))

# ds

# ---------------------------------------------------
# De largo para longo

ds$id <- 1:length(ds$y1)
ds.longo <- gather(ds, key = "time", value = "y", -id, -drug)
                   
ds.longo$tempo <- as.numeric(
  as.character(factor(ds.longo$time,
                      levels = c("y1", "y2"),
                      labels = c(0, 1))))

# ds.longo$drug <- factor(ds.longo$drug)

ds.longo <- ds.longo %>% 
  select(-time) %>% 
  arrange(id, tempo)
  
# ds.longo <- ds.longo[order(ds.longo$id, ds.longo$tempo),]

# ds.longo$drugn <- as.numeric(ds.longo$drug)
ds.longo$tempoA <- ds.longo$tempo*(ds.longo$drug == 1)
ds.longo$tempoB <- ds.longo$tempo*(ds.longo$drug == 2)
ds.longo$tempoAB <- ds.longo$tempo*I(ds.longo$drug != 0)

# ---------------------------------------------------
# Compara grupos de tratamento

res <- compareGroups(data = ds.longo, drug ~ y, method = 1,
                     subset = tempo == 0)
tempo0 <- createTable(res, show.p.overall = FALSE)

res <- compareGroups(data = ds.longo, drug ~ y, method = 1,
                     subset = tempo == 1)
tempo1 <- createTable(res, show.p.overall = FALSE)

restab <- rbind(tempo0, tempo1)
export2md(restab, format = "latex", position = "center")
# export2latex(rbind(restab0, restab1)) #, format = "markdown")



## ----antib2, echo=TRUE, eval=TRUE--------------------------------------------------------------
fit <- geeglm(y ~ tempo + tempoA + tempoB,
              family = poisson(link = "log"), 
              data = ds.longo, id = id, waves = tempo,
              corstr = "exchangeable", std.err = "san.se")
summary(fit)


## ----antib3, echo=TRUE, eval=TRUE--------------------------------------------------------------
L <- rbind(c(0,0,1,0),
          c(0,0,0,1))
esticon(fit, L, joint.test = TRUE)


## ----antib3.2, echo=TRUE, eval=TRUE------------------------------------------------------------

L <- c(0,0,1,-1)
esticon(fit, L, joint.test = TRUE)



## ----antib4, echo=TRUE, eval=TRUE--------------------------------------------------------------

fit <- geeglm(y ~ tempo + tempoAB,
              family = poisson(link = "log"), 
              data = ds.longo, id = id, waves = tempo,
              corstr = "exchangeable", std.err = "san.se")
summary(fit)



## ----antib5, echo=FALSE, eval=TRUE-------------------------------------------------------------

est <- esticon(fit, diag(3))
# Razão de taxas e intervalos de confiança
RT.CI <- exp(cbind(est$estimate, est$lwr, est$upr))
rownames(RT.CI) <- names(coef(fit))
colnames(RT.CI) <- c("RT", "RT 95% LI", "RT 95% LS")
knitr::kable(RT.CI, digits = c(2, 2, 2), format = "pandoc")


