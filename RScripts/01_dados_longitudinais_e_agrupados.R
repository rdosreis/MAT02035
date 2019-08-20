## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='70%', paged.print=FALSE----
knitr::include_graphics(here('images', 'dims.jpg'))


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='90%', paged.print=FALSE----
knitr::include_graphics(here('images', 'levels_explanation.png'))


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----
knitr::include_graphics(here('images', 'CS_Vs_Longitudinal_Study.jpg'))


## ----long_eh_agrupado, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, fig.align='center', out.width="80%", out.height="70%"----
# ----------------------------------------------------
# Carregando pacotes do R

library(here)
library(haven)
library(tidyr)
library(ggplot2)

# ----------------------------------------------------
# Carregando o arquivo de dados

chol.df <- read_dta(file = here("data", "cholesterol.dta"))

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


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----
knitr::include_graphics(here('images', 'long_study.jpg'))

