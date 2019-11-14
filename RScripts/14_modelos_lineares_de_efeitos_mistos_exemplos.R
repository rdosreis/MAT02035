# ---------------------------------------------------
# MAT02035 - Modelos para dados correlacionados
# Modelos lineares de efeitos mistos
# 
# Rodrigo Citton P. dos Reis
# citton.padilha@ufrgs.br
# 
# Universidade Federal do Rio Grande do Sul
# Instituto de Matemática e Estatística
# Departamento de Estatística
# 
# Porto Alegre, 2019
# ---------------------------------------------------

# Six Cities Study of Air Pollution and Health

# O Estudo das Seis Cidades de Poluição do Ar e Saúde 
# foi um estudo longitudinal projetado para caracterizar
# o crescimento pulmonar medido por mudanças na função 
# pulmonar em crianças e adolescentes e os fatores que 
# influenciam o crescimento da função pulmonar (Dockery 
# et al., 1983). Uma coorte de 13.379 crianças nascidas 
# em ou após 1967 foi matriculada em seis comunidades 
# nos Estados Unidos: Watertown (Massachusetts), 
# Kingston e Harriman (Tennessee), uma seção de St. 
# Louis (Missouri), Steubenville (Ohio), Portage 
# (Wisconsin) ) e Topeka (Kansas). A maioria das 
# crianças estava matriculada na primeira ou segunda 
# série (entre 6 e 7 anos de idade) e as medidas dos 
# participantes do estudo eram obtidas anualmente até 
# a conclusão do ensino médio ou a perda para o 
# acompanhamento. Em cada exame anual, a espirometria, 
# a medida da função pulmonar, era realizada e um 
# questionário de saúde respiratória era preenchido 
# pelos pais ou responsáveis. A manobra básica da 
# espirometria simples é a inspiração máxima (ou a 
# inspiração) seguida da expiração forçada
# rapidamente possível em um recipiente fechado. 
# Muitas medidas diferentes podem ser derivadas da 
# curva espirométrica do volume expirado versus o tempo.
# Uma medida amplamente utilizada é o __volume total de 
# ar expirado__ no primeiro segundo da manobra (FEV 1).

# Os dados consistem em todas as medidas de FEV 1, 
# altura e idade obtidas de um subconjunto selecionado 
# aleatoriamente das participantes do sexo feminino que 
# vivem em Topeka, Kansas.

# ---------------------------------------------------
# Carregando pacotes do R

library(here)
library(haven)
library(tidyr)
library(ggplot2)
library(dplyr)

# ---------------------------------------------------
# Carregando o arquivo de dados

fev <- read_dta(
  file = here::here("data", "fev1.dta"))

# ---------------------------------------------------
# Conhecendo (a estrutura d)os dados

fev
dim(fev)
glimpse(fev)
unique(fev$id)
length(unique(fev$id))
tibble::as.tibble(as.data.frame(table(fev$id)))
summary(as.numeric(table(fev$id)))
summary(as.factor(as.numeric(table(fev$id))))

# ---------------------------------------------------
# Conhecendo os dados: análise exploratória

p <- ggplot(data = fev,
            mapping = aes(x = age, y = logfev1/ht,
                          group = id)) +
  geom_point(alpha = 0.3) +
  geom_line(alpha = 0.3) +
  labs(x = "Idade (anos)",
       y = "Log(FEV1/Altura)")
p

# ---------------------------------------------------
# Removendo observação atípica

fev <- fev[- which(fev$logfev1/fev$ht < -0.5), ]

# ---------------------------------------------------
# Conhecendo os dados: análise exploratória

p <- ggplot(data = fev,
            mapping = aes(x = age, y = logfev1/ht,
                          group = id)) +
  geom_point(alpha = 0.3) +
  geom_line(alpha = 0.3) +
  labs(x = "Idade (anos)",
       y = "Log(FEV1/Altura)")
p

# ---------------------------------------------------
# Criando variável log-altura 

fev$loght <- log(fev$ht)
fev$logbht <- log(fev$baseht)

# ---------------------------------------------------
# Um modelo linear de efeitos mistos com 
# "componentes" transversais e longitudinais

library(nlme)

modelo1 <- lme(logfev1 ~ age + loght + baseage + logbht, # efeitos fixos
               random = ~  age | id, # efeitos aleatórios
               data = fev)

summary(modelo1)

# Coeficientes de regressão estimados (efeitos fixos)
# e erros padrões para os dados de log(FEV1) do 
# Estudo das Seis Cidades

knitr::kable(
  summary(modelo1)$tTable[,-c(3,5)],
  digits = c(4, 4, 2),
  col.names = c("Estimativa", "EP", "Z"),
  caption = "Coeficientes de regressão estimados (efeitos fixos)
  e erros padrões para os dados de log(FEV1) do 
  Estudo das Seis Cidades")

# Interpretação dos efeitos fixos

mod1.coef <- fixed.effects(modelo1)

mod1.coef

exp(mod1.coef[3])
exp(mod1.coef[3] * 0.1)

# Gráfico dos efeitos fixos

df <- expand.grid(unique(fev$baseage), unique(fev$logbht))
names(df) <- c("age", "ht")
df$e <- 0

for (i in 1:dim(df)[1]){
  df$e[i] <- mod1.coef[1] + (mod1.coef[2] + mod1.coef[4]) * df$age[i] + (mod1.coef[3] + mod1.coef[5]) * df$ht[i]
  df$ee[i] <- exp(mod1.coef[1] + (mod1.coef[2] + mod1.coef[4]) * df$age[i] + (mod1.coef[3] + mod1.coef[5]) * df$ht[i])
}

v <- ggplot(df, aes(age, ht, z = ee))
v + geom_contour()

# Covariância estimada dos efeitos aleatórios
# para os dados de log(FEV1) do Estudo das 
# Seis Cidades

getVarCov(modelo1, type = "random.effects") #  G
modelo1$sigma^2 # sigma^2 ("R_i")

# Correlações marginais estimadas entre as medidas
# repetidas de log(FEV1) entre as idades 7 e 18

Sigma.chapeu35 <- getVarCov(modelo1, type = "marginal", individuals = 35)
Sigma.chapeu35

knitr::kable(round(cov2cor(Sigma.chapeu35[[1]]), 2))

# ---------------------------------------------------
# Mais um modelo linear de efeitos mistos com 
# "componentes" transversais e longitudinais

modelo2 <- lme(logfev1 ~ age + loght + baseage + logbht, # efeitos fixos
               random = ~  loght | id, # efeitos aleatórios
               data = fev)

summary(modelo2)

# Coeficientes de regressão estimados (efeitos fixos)
# e erros padrões para os dados de log(FEV1) do 
# Estudo das Seis Cidades

knitr::kable(
  summary(modelo2)$tTable[,-c(3,5)],
  digits = c(4, 4, 2),
  col.names = c("Estimativa", "EP", "Z"),
  caption = "Coeficientes de regressão estimados (efeitos fixos)
  e erros padrões para os dados de log(FEV1) do 
  Estudo das Seis Cidades")

# Comparação dos modelos 1 e 2

modelo1$logLik

modelo2$logLik

# ---------------------------------------------------
# Mais um outro modelo linear de efeitos mistos com 
# "componentes" transversais e longitudinais

modelo3 <- lme(logfev1 ~ age + loght + baseage + logbht, # efeitos fixos
               random = ~  age + loght | id, # efeitos aleatórios
               data = fev)

summary(modelo3)

# Coeficientes de regressão estimados (efeitos fixos)
# e erros padrões para os dados de log(FEV1) do 
# Estudo das Seis Cidades

knitr::kable(
  summary(modelo3)$tTable[,-c(3,5)],
  digits = c(4, 4, 2),
  col.names = c("Estimativa", "EP", "Z"),
  caption = "Coeficientes de regressão estimados (efeitos fixos)
  e erros padrões para os dados de log(FEV1) do 
  Estudo das Seis Cidades")

# Comparação dos modelos 2 e 3
# (Ver Apêndice C de Fitzmaurice et al., 2011)

G2 <- 2 * (modelo3$logLik - modelo2$logLik)
0.5 * pchisq(q = G2, df = 2,lower.tail = F) + 0.5 * pchisq(q = G2, df = 3,lower.tail = F)

# install.packages("emdbook")
library(emdbook)

# ?pchibarsq
pchibarsq(p = G2, df = 3, mix = 0.5, lower.tail = TRUE, log.p = FALSE)





## ----fev4, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"----
p <- ggplot(data = fev,
            mapping = aes(x = age, y = logfev1/ht)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Idade (anos)",
       y = "Log(FEV1/Altura)")
p
