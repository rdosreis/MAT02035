## ----tlc_p, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, fig.align='center', out.width="80%"----
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

library(plyr)
library(ggplot2)

chumbo.resumo <- ddply(chumbo.df.longo, ~ trt + tempo, summarize, chumbo.m = mean(chumbo))

p <- ggplot(data = chumbo.resumo,
            mapping = aes(x = tempo, y = chumbo.m, group = trt, colour = trt)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Semana", y = "Média nível de chumbo no sangue (mcg/dL)", colour = "Grupo de tratamento")
p


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----
knitr::include_graphics(here::here('images', 'conceitos-basicos-de-trade.jpg'))

