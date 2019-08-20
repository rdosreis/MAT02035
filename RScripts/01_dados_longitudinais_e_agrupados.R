## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='70%', paged.print=FALSE----
knitr::include_graphics(here::here('images', 'dims.jpg'))


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='90%', paged.print=FALSE----
knitr::include_graphics(here::here('images', 'levels_explanation.png'))


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----
knitr::include_graphics(here::here('images', 'CS_Vs_Longitudinal_Study.jpg'))


## ----long_eh_agrupado, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, fig.align='center', out.width="80%", out.height="60%"----
# ----------------------------------------------------
# Carregando pacotes do R

library(here)
library(haven)
library(tidyr)
library(ggplot2)

# ----------------------------------------------------
# Carregando o arquivo de dados

chol.df <- read_dta(file = here::here("data", "cholesterol.dta"))

# ----------------------------------------------------
# Reformando o objeto: de "largo" para "longo"

chol.df.longo <- gather(data = chol.df, key = "tempo", value = "colesterol", -id, -group)

# ----------------------------------------------------
# Formata variáveis

chol.df.longo$tempo <- as.factor(as.numeric(as.factor(chol.df.longo$tempo)))
chol.df.longo$group <- as.factor(chol.df.longo$group)
chol.df.longo$id <- as.factor(chol.df.longo$id)
# ----------------------------------------------------
# Gráfico de perfis

set.seed(10)
ids <- sample(x = unique(chol.df.longo$id), size = 5, replace = FALSE)

p <- ggplot(data = chol.df.longo[which(chol.df.longo$id %in% ids), ],
            mapping = aes(x = tempo, y = colesterol, group = id, colour = id)) +
  geom_line(size = 0.8) +
  labs(x = "Tempo", y = "Colesterol", colour = "ID")
p


## ----tlc_tb, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, results='asis'----
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
# ----------------------------------------------------
# Gráfico de perfis

set.seed(10)

knitr::kable(chumbo.df[sample(x = chumbo.df$id, size = 10, replace = F),], col.names = c("ID", "Grupo", "Linha de base", "Semana 1", "Semana 4", "Semana 6"))


## ----tlc_p, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, fig.align='center', out.width="100%"----

library(plyr)

chumbo.resumo <- ddply(chumbo.df.longo, ~ trt + tempo, summarize, chumbo.m = mean(chumbo))

p <- ggplot(data = chumbo.resumo,
            mapping = aes(x = tempo, y = chumbo.m, group = trt, colour = trt)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Semana", y = "Média nível de chumbo no sangue (mcg/dL)", colour = "Grupo de tratamento")
p


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----
knitr::include_graphics(here::here('images', 'long_study.jpg'))

