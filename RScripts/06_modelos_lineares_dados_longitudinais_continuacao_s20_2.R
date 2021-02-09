## ----carrega_dados, echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE--------------------------------------------
## 
## # install.packages("haven")
## library(haven)
## 
## chumbo <- read_dta(file = "tlc.dta"))
## 


## ----largo, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------
head(chumbo)




## ----reshape, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------------------------------------------------

# install.packages("tidyr")
library(tidyr)

chumbo.longo <- gather(data = chumbo,
                        key = "tempo",
                        value = "chumbo", -id, -trt)



## ----reshape2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------------

head(chumbo.longo)



## ----transforma_dados, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE------------------------------------------

chumbo.longo$tempo <- as.numeric(
  as.character(factor(chumbo.longo$tempo,
                      labels = c(0, 1, 4, 6))))

chumbo.longo$trt <- factor(chumbo.longo$trt,
                           labels = c("Placebo", "Succimer"))



## ----transforma_dados2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------------------------------

head(chumbo.longo)



## ----time_plot, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"------------

# install.packages("ggplot2")
library(ggplot2)

p <- ggplot(data = chumbo.longo,
            mapping = aes(x = tempo, y = chumbo)) +
  geom_point() +
  labs(x = "Tempo (semanas)",
       y = "Nível de chumbo no sangue (mcg/dL)")
p



## ----time_plot2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"-----------

p <- ggplot(data = chumbo.longo,
            mapping = aes(x = tempo, y = chumbo,
                          group = id)) +
  geom_point() + geom_line() +
  labs(x = "Tempo (semanas)",
       y = expression("Média nível de chumbo no sangue"~(mu*g/dL))) +
  theme_bw()
p



## ----time_plot3, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"-----------

p <- ggplot(data = chumbo.longo,
            mapping = aes(x = tempo, y = chumbo,
                          group = id, colour = trt)) +
  geom_point() + geom_line() +
  labs(x = "Tempo (semanas)",
       y = expression("Média nível de chumbo no sangue"~(mu*g/dL)),
       colour = "Grupo de tratamento") +
  theme_bw() + theme(legend.position = "bottom")
p



## ----resumo_medias, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE---------------------------------------------

# install.packages("dplyr")
library(dplyr)

chumbo.resumo <- chumbo.longo %>% 
  group_by(trt, tempo) %>% 
  summarise(chumbo.m = mean(chumbo))



## ----resumo_medias2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------

chumbo.resumo



## ----time_plot4, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"-----------

p <- ggplot(data = chumbo.resumo,
            mapping = aes(x = tempo, y = chumbo.m,
                          colour = trt)) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = c(0, 1, 4, 6)) +
  labs(x = "Tempo (semanas)",
       y = expression("Média nível de chumbo no sangue"~(mu*g/dL)),
       colour = "Grupo de tratamento") +
  theme_bw() + theme(legend.position = "bottom")
p



## ----resumo_ep, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-------------------------------------------------

chumbo.resumo <- chumbo.longo %>% 
  group_by(trt, tempo) %>% 
  summarise(chumbo.m = mean(chumbo),
            dp = sd(chumbo), n = n()) %>% 
  mutate(ep = dp/sqrt(n))



## ----resumo_ep2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE------------------------------------------------

chumbo.resumo



## ----time_plot5, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"-----------

p <- ggplot(data = chumbo.resumo,
            mapping = aes(x = tempo, y = chumbo.m,
                          colour = trt)) +
  geom_errorbar(aes(ymin = chumbo.m - ep,
                    ymax = chumbo.m + ep),
                width = .1) +
  geom_point() + geom_line() +
  labs(x = "Tempo (semanas)",
       y = expression("Média nível de chumbo no sangue"~(mu*g/dL)),
       colour = "Grupo de tratamento") +
  theme_bw() + theme(legend.position = "bottom")
p



## ----boxplot, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"--------------

chumbo.longo$tempof <- factor(chumbo.longo$tempo)

p <- ggplot(data = chumbo.longo,
            mapping = aes(x = tempof, y = chumbo,
                          fill = trt)) +
  geom_boxplot() +
  labs(x = "Tempo (semanas)",
       y = expression("Média nível de chumbo no sangue"~(mu*g/dL)),
       fill = "Grupo de tratamento") +
  theme_bw() + theme(legend.position = "bottom")
p





## ----fev_carrega_dados, echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE----------------------------------------
## 
## fev <- read_dta(file = "fev1.dta")
## 
## # cria variável log(fev1/ht)
## fev$fev1 <- exp(fev$logfev1)
## fev$logfh <- log(fev$fev1/fev$ht)
## 
## # uma observação atípica (?)
## fev <- fev[- which(fev$logfev1/fev$ht < -0.5), ]
## 


## ----fev, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------

head(fev, 16)



## ----fev2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"-----------------

p <- ggplot(data = fev,
            mapping = aes(x = age, y = logfh)) +
  geom_point() +
  labs(x = "Idade (anos)",
       y = "Log(FEV1/Altura)") +
  theme_bw()
p



## ----fev3, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"-----------------

set.seed(5)
id.s <- sample(x = unique(fev$id), size = 50,
               replace = FALSE)

p <- ggplot(data = fev[which(fev$id %in% id.s),],
            mapping = aes(x = age, y = logfh,
                          group = id)) +
  geom_point(alpha = 0.3) +
  geom_line(alpha = 0.3) +
  labs(x = "Idade (anos)",
       y = "Log(FEV1/Altura)") +
  theme_bw()
p





## ----fev4, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"-----------------

p <- ggplot(data = fev,
            mapping = aes(x = age, y = logfh)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Idade (anos)",
       y = "Log(FEV1/Altura)") +
  theme_bw()
p



## ----filtra_succimer, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-------------------------------------------

chumbo.succimer <- chumbo %>% 
  filter(trt == 1) %>% 
  select(y0, y1, y4, y6) %>% 
  mutate(y0 = as.numeric(y0),
         y1 = as.numeric(y1),
         y4 = as.numeric(y4),
         y6 = as.numeric(y6))



## ----grupo_succimer, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------------------------------------------

chumbo.succimer



## ----correlacao, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="90%"-----------

library(GGally)

p <- ggpairs(chumbo.succimer,
             columnLabels = paste("Semana", c(0, 1, 4, 6))) +
  theme_bw()
p


