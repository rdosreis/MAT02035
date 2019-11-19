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
fat <- read_dta(
  file = here::here("data", "fat.dta"))
fat


## ----time_plot2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"----
p <- ggplot(data = fat,
            mapping = aes(x = time, y = pbf,
                          group = id)) +
  geom_line(alpha = 0.3) +
  geom_vline(xintercept = 0,
             colour = "lightsalmon",
             linetype = "longdash") +
  labs(x = "Tempo (anos)",
       y = "Percentual de gordura corporal")
p +   geom_smooth(data = fat,
              mapping = aes(x = time, y = pbf,
                            group = NULL),
              method = "loess", se = FALSE)


## ----mlem, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE------------
fat <- as.data.frame(fat)
library(nlme)
library(lspline)
mod1 <- lme(pbf ~ lspline(x = time,
                          knots = 0,
                          marginal = TRUE),
            random = ~ lspline(x = time,
                          knots = 0,
                          marginal = TRUE) | id,
            data = fat)


## ----coefm1, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, results='asis'----
coef.tab <- summary(mod1)$tTable[,-c(3,5)]
row.names(coef.tab) <- c("(Intercepto)", "tempo", "(tempo)$_{+}$")
knitr::kable(
  coef.tab,
  digits = c(4, 4, 2),
  col.names = c("Estimativa", "EP", "Z"),
  caption = "Coeficientes de regressão estimados
  (efeitos fixos) e erros padrões para os o 
  modelo linear por partes para dos dados de 
  percentual de gordura.")


## ----Gm1, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-------------
G <- getVarCov(mod1, type = "random.effects")
matrix(G, ncol = 3, byrow = T)

mod1$sigma^2 # sigma^2 ("R_i")


## ----resm1, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------
library(mgcv)
# Calculate the residuals
res <- residuals(mod1, level = 0)
res2 <- residuals(mod1, level = 0, type = "normalized")
pred <- fitted(mod1, level = 0)
# Cholesky residuals
est.cov <- extract.lme.cov(mod1, fat) # We extract the blocked covariance function of the residuals
Li <- t(chol(est.cov)) # We find the Cholesky transformation of the residuals. (The transform is to get the lower trangular matrix.)
rest <- solve(Li) %*% res # We then calculate the transformed residuals.
predt <- solve(Li) %*% pred


## ----histres, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, out.height="90%", fig.align='center'----
par(mfrow = c(1,2))
hist(x = res, prob = T,
     col = "lightgrey",
     border = "white",
     main = "",
     xlab = "Resíduos",
     ylab = "Densidade")
lines(x = sort(res),
      y = dnorm(x = sort(res),
                mean = mean(res),
                sd = sd(res)),
      col  = "steelblue")

hist(x = rest, prob = T,
     col = "lightgrey",
     border = "white",
     main = "",
     xlab = "Resíduos transformados",
     ylab = "Densidade")
lines(x = sort(rest),
      y = dnorm(x = sort(rest),
                mean = mean(rest),
                sd = sd(rest)),
      col  = "steelblue")


## ----qqres, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, out.height="90%", fig.align='center'----
par(mfrow = c(1,2))
qqnorm(y = res, main = "",
     xlab = "Quantis amostrais dos resíduos",
     ylab = "Quantis teóricos da normal padrão")
qqline(y = res)
qqnorm(y = rest, main = "",
     xlab = "Quantis amostrais dos resíduos transformados",
     ylab = "Quantis teóricos da normal padrão")
qqline(y = rest)


## ----scatres, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, out.height="80%", fig.align='center'----
par(mfrow = c(1,2))
plot(pred, res,
     xlab = "Valores preditos",
     ylab = "Resíduos")
abline(h = 0)
lines(lowess(pred, res),
      type = "l", lwd = 2,
      col = "steelblue")
plot(predt, rest, 
     xlab = "Valores preditos transformados",
     ylab = "Resíduos transformados")
abline(h = 0)
lines(lowess(predt, rest),
      type = "l", lwd = 2,
      col = "steelblue")


## ----variograma, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, out.height="80%", fig.align='center'----
par(mfrow = c(1,1))
plot(Variogram(mod1,
               form = ~ lspline(x = time,
                                knots = 0,
                                marginal = TRUE) | id,
               resType = "normalized"))

