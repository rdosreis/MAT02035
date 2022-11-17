## ----setup, include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----carrega-dados, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------------
# Carrega pacotes
library(haven)
library(tidyr)
library(dplyr)

# Carrega dados
chumbo <- read_dta(file = here::here("data","tlc.dta"))

# Largo para longo
chumbo.longo <- gather(data = chumbo,
                        key = "tempo",
                        value = "chumbo", -id, -trt)

# Transforma dados
chumbo.longo$tempo <- as.numeric(
  as.character(factor(chumbo.longo$tempo,
                      labels = c(0, 1, 4, 6))))

chumbo.longo$trt <- factor(chumbo.longo$trt,
                           labels = c("Placebo", "Succimer"))

# Filtra dados do grupo succimer
chumbo.longo <- chumbo.longo %>% 
  filter(trt == "Succimer")



## ----dp-var, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, results='asis'----

chumbo.resumo <- chumbo.longo %>% 
  group_by(tempo) %>% 
  summarise(chumbo.m = mean(chumbo),
            chumbo.dp = sd(chumbo),
            chumbo.var = var(chumbo))

knitr::kable(chumbo.resumo,
             format = 'pandoc',
             digits = c(0,1,1,1),
             row.names = FALSE,
             col.names = c("Tempo", "Média", "Desvio Padrão", "Variância"),
             align = 'c')



## ----perfis-medios, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width='80%'----

library(ggplot2)

p <- ggplot(data = chumbo.resumo,
            mapping = aes(x = tempo, y = chumbo.m)) +
  geom_point(color = "#D95F02") + geom_line(color = "#D95F02") +
  scale_x_continuous(breaks = c(0, 1, 4, 6)) +
  labs(x = "Tempo (semanas)",
       y = expression("Média nível de chumbo no sangue"~(mu*g/dL))) +
  theme_bw()
p



## ----cov, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE-----------------------

chumbo.succimer <- chumbo %>% 
  filter(trt == 1) %>% 
  select(y0, y1, y4, y6) %>% 
  mutate(y0 = as.numeric(y0),
         y1 = as.numeric(y1),
         y4 = as.numeric(y4),
         y6 = as.numeric(y6))

knitr::kable(cov(chumbo.succimer),
             format = 'pandoc',
             digits = 2,
             row.names = TRUE,
             # col.names = c("Tempo", "Desvio Padrão", "Variância"),
             align = 'c')




## ----cor, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE-----------------------


knitr::kable(cor(chumbo.succimer),
             format = 'pandoc',
             digits = 2,
             row.names = TRUE,
             # col.names = c("Tempo", "Desvio Padrão", "Variância"),
             align = 'c')



