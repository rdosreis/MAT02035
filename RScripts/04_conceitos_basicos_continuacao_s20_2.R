## ----tlc, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, out.width='80%'-------------------------------------
# ----------------------------------------------------
# Carregando os pacotes

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
# Versões placebo

chumbo.df.p <- subset(chumbo.df, subset = trt == "Placebo")
names(chumbo.df.p)[3:6] <- paste0("y", 1:4)

chumbo.df.longo.p <- subset(chumbo.df.longo, subset = trt == "Placebo")



## ----tlc_pairs, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, fig.align='center', out.width="80%"-----------

pairs(chumbo.df.p[,3:6], pch = 19, upper.panel = NULL)



## ----tlc_cov, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE--------------------------------------------------

knitr::kable(x = cov(chumbo.df.p[,3:6]),
             digits = 1)



## ----tlc_cor, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE--------------------------------------------------

knitr::kable(x = cor(chumbo.df.p[,3:6]),
             digits = 2)



## ----tlc_timeplot, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, fig.align='center', out.width="100%"-------

chumbo.df.longo.p$id <- as.factor(chumbo.df.longo.p$id)
# ----------------------------------------------------
# Gráfico de perfis

set.seed(10)
ids <- sample(x = unique(chumbo.df.longo.p$id), size = 5, replace = FALSE)

p <- ggplot(data = chumbo.df.longo.p[which(chumbo.df.longo.p$id %in% ids), ],
            mapping = aes(x = tempo.num, y = chumbo, group = id, colour = id)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = c(0,1,4,6)) +
  labs(x = "Tempo",
       y = expression("Média nível de chumbo no sangue"~(mu*g/dL)),
       colour = "ID")
p


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE-------------
knitr::include_graphics(here::here('images', 'fontes_varia_entre.png'))


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE-------------
knitr::include_graphics(here::here('images', 'fontes_varia_intra.png'))


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE-------------
knitr::include_graphics(here::here('images', 'fontes_varia_erro.png'))


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', paged.print=FALSE--------------
knitr::include_graphics(here::here('images', 'presuntinho.jpg'))

