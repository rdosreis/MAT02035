# ------------------------------------
# Carregando pacotes do R
# ------------------------------------

library(MASS)
library(dplyr)
library(ggplot2)
library(nlme)

# ------------------------------------
# Parâmetros da simulação
# ------------------------------------

N <- 100 # número de indivíduos
n <- 4 # número de medidas repetidas
t <- c(0, 1, 4, 6) # tempos de medida

# Matriz de covariáveis grupo controle (placebo)
X_0 <- matrix(data = c(rep(1, n),
                       t,
                       t * 0),
              byrow = FALSE, ncol = 3, nrow = n)
colnames(X_0) <- c("X1", "X2", "X3")

# Matriz de covariáveis grupo tratamento (succimer)
X_1 <- matrix(data = c(rep(1, n),
                       t,
                       t * 1),
              byrow = FALSE, ncol = 3, nrow = n)
colnames(X_1) <- c("X1", "X2", "X3")

# coeficientes de regressão
beta <- c(30, -1, -2.5)

# componentes de variância
sigma2 <- 10
rho <- 0.75
theta <- -log(rho)

# Matriz de covariância
Sigma <- matrix(0, nrow = n, ncol = n)
for (j in 1:n){
  for (k in 1:n){
    Sigma[j,k] <- sigma2 * exp(-theta * abs(t[j] - t[k]))
  }
}

# ------------------------------------
# Gerando os daos
# ------------------------------------

df <- list()

set.seed(1540)

for (i in 1:N){
  if (i <= N/2){
    e <- mvrnorm(n = 1, mu = rep(0, n), Sigma = Sigma)
    y <- X_0%*%beta + e
    df[[i]] <- data.frame(id = rep(i, n), y, tempo = t, trt = rep(0, n), j = 1:n)
  }else{
    e <- mvrnorm(n = 1, mu = rep(0, n), Sigma = Sigma)
    y <- X_1%*%beta + e
    df[[i]] <- data.frame(id = rep(i, n), y, tempo = t, trt = rep(1, n), j = 1:n)
  }
}

df <- data.frame(do.call(rbind, df))
df$trt <- factor(df$trt, labels = c("Placebo", "Succimer"))

# ------------------------------------
# Gráficos de perfis
# ------------------------------------

# perfis individuais
p <- ggplot(data = df,
            mapping = aes(x = tempo, y = y,
                          group = id, colour = trt)) +
  geom_point() + geom_line() +
  scale_colour_brewer(palette = "Accent", direction = -1) +
  labs(x = "Tempo (semanas)",
       y = expression("Média nível de chumbo no sangue"~(mu*g/dL)),
       colour = "Grupo de tratamento") +
  theme_bw() + theme(legend.position = "bottom")
p

# perfis de médias
chumbo.resumo <- df %>%
  group_by(trt, tempo) %>%
  summarise(chumbo.m = mean(y))

p <- ggplot(data = chumbo.resumo,
            mapping = aes(x = tempo, y = chumbo.m,
                          colour = trt)) +
  geom_point(size = 1.5) + geom_line(size = 1) +
  scale_x_continuous(breaks = c(0, 1, 4, 6)) +
  scale_colour_brewer(palette = "Accent", direction = -1) +
  labs(x = "Tempo (semanas)",
       y = expression("Média nível de chumbo no sangue"~(mu*g/dL)),
       colour = "Grupo de tratamento") +
  theme_bw() + theme(legend.position = "bottom")
p

# ------------------------------------
# Ajuste do modelo:
#   Intercepto comum
#   Modelo covariância: não-estruturada
# ------------------------------------

# Método da máxima verossimilhança (ml)
mod.ml <- gls(y ~ tempo + tempo:trt,
              data = df,
              corr = corSymm(form = ~ 1 | id),
              weights = varIdent(form = ~ 1 | j),
              method = "ML")

# Método da máxima verossimilhança restrita (reml)
mod.reml <- gls(y ~ tempo + tempo:trt,
                data = df,
                corr = corSymm(form = ~ 1 | id),
                weights = varIdent(form = ~ 1 | j),
                method = "REML")

# ------------------------------------
# Estiva dos parâmetros
# coeficientes de regressão (beta)
# ------------------------------------

# ml
summary(mod.ml)$coef
# round(summary(mod.ml)$coef, 2)

# reml
summary(mod.reml)$coef
# round(summary(mod.reml)$coef, 2)

# ------------------------------------
# Estiva dos parâmetros
# Matriz de covariância (Sigma)
# ------------------------------------

# ml
getVarCov(mod.ml)
# round(getVarCov(mod.ml), 2)

# reml
getVarCov(mod.reml)
# round(getVarCov(mod.reml), 2)
