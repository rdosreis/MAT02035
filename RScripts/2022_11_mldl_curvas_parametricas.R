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

chumbo.df.longo <- gather(data = chumbo.df,
                          key = "tempo", value = "chumbo", -id, -trt)

# ----------------------------------------------------
# Formata variáveis

chumbo.df.longo$semana <- factor(chumbo.df.longo$tempo, labels = c("0", "1", "4", "6"))
chumbo.df.longo$semana <- as.numeric(as.character(chumbo.df.longo$semana))
chumbo.df.longo$tempo <- as.numeric(factor(chumbo.df.longo$semana))

# ----------------------------------------------------

library(plyr)

chumbo.resumo <- ddply(chumbo.df.longo, ~ trt + semana, summarize, chumbo.m = mean(chumbo))

p <- ggplot(data = chumbo.resumo,
            mapping = aes(x = semana, y = chumbo.m, group = trt, colour = trt)) +
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


## ----df, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------------------

chumbo.df.longo



## ----spline_mod, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-------------

library(nlme)

# modelo de curvas paramétricas
#     splines lineares
# com matriz de covariância não estruturada
mod.spline <- gls(chumbo ~ semana + I( (semana - 1) * (semana > 1) ) +
                    semana:trt + I( (semana - 1) * (semana > 1) ):trt,
                  corr = corSymm(form = ~ tempo | id),
                  weights = varIdent(form = ~ 1 | tempo),
                  method = "REML",
                  data = chumbo.df.longo)



## ----summary_spline, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------

summary(mod.spline)



## ----spline_mod2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE------------

library(lspline)

# modelo de curvas paramétricas
#     splines lineares
# com matriz de covariância não estruturada
mod.spline2 <- gls(chumbo ~ lspline(x = semana,
                                    knots = 1,
                                    marginal = TRUE) +
                     lspline(x = semana,
                             knots = 1,
                             marginal = TRUE):trt,
                   corr = corSymm(form = ~ tempo | id),
                   weights = varIdent(form = ~ 1 | tempo),
                   method = "REML",
                   data = chumbo.df.longo)



## ----summary_spline2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------

summary(mod.spline2)



## ----coef, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-------------------

knitr::kable(
  summary(mod.spline)$tTable[,-4],
  digits = c(4, 4, 2),
  col.names = c("Estimativa", "EP", "Z"))



## ----media_estimada, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------

library(ggeffects)

media_chap_df <- ggpredict(mod.spline, terms = c("semana", "trt"))
media_chap_df



## ----media_estimada2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------

df_aux <- as.data.frame(media_chap_df)[c(6, 2, 1)]
names(df_aux) <- c("trt", "chumbo.m", "semana")

p2 <- p + geom_line(data = df_aux,
              linetype = "dashed")



## ----media_estimada3, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="70%", out.height="60%"----

p2



## ----trv, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------------------

# modelo linear por partes
mod.comp <- gls(chumbo ~ semana + I( (semana - 1) * (semana > 1) ) +
                    semana:trt + I( (semana - 1) * (semana > 1) ):trt,
                  corr = corSymm(form = ~ tempo | id),
                  weights = varIdent(form = ~ 1 | tempo),
                  method = "ML",
                  data = chumbo.df.longo)

# modelo de tendências lineares
mod.red <- gls(chumbo ~ semana +
                    semana:trt,
                  corr = corSymm(form = ~ tempo | id),
                  weights = varIdent(form = ~ 1 | tempo),
                  method = "ML",
                  data = chumbo.df.longo)



## ----trv2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-------------------

anova(mod.comp, mod.red)



## ----quadratica, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-------------

# modelo linear por partes
mod.linpartes <- mod.comp

# modelo de tendências lineares
mod.lin <- mod.red

# modelo de tendência biquadrática
mod.quad <- gls(chumbo ~ semana + I(semana^2) +
                    semana:trt + I(semana^2):trt,
                  corr = corSymm(form = ~ tempo | id),
                  weights = varIdent(form = ~ 1 | tempo),
                  method = "ML",
                  data = chumbo.df.longo)


## ----quadratica2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE------------

logLik(mod.linpartes)
logLik(mod.quad)
logLik(mod.lin)

