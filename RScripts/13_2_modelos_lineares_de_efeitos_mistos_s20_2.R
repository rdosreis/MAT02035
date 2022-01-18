## ----carrega_dados, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE--------------------------------------
# ----------------------------------------------------
# Carregando pacotes do R
library(here)
library(haven)
library(tidyr)
library(ggplot2)
library(dplyr)
# ----------------------------------------------------
# Carregando o arquivo de dados
af <- read_dta(
  file = here::here("data", "exercise.dta"))
af


## ----transforma_dados, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------------------------
names(af)[which(names(af) == "group")] <- "trt"
af.longo <- gather(data = af,
                        key = "tempo",
                        value = "fc", -id, -trt)
af.longo
af.longo <- subset(af.longo, tempo != "y2" & tempo != "y10")

af.longo$dia <- factor(af.longo$tempo,
                       labels = c(0, 12, 4, 6, 8))
af.longo$dia <- factor(af.longo$dia,
                       levels = c("0", "4", "6", "8", "12"))
af.longo$tempo <- as.numeric(
  as.character(af.longo$dia))
af.longo$trt <- factor(af.longo$trt)
af.longo


## ----time_plot2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"----
p <- ggplot(data = af.longo,
            mapping = aes(x = dia, y = fc,
                          group = id, colour = trt)) +
  geom_point() +
  geom_line() +
  labs(x = "Tempo (dias)",
       y = "Força corporal",
       colour = "Tratamento")
p + theme_gray()


## ----time_plot3b, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"----
library(dplyr)

af.resumo <- af.longo %>% 
  group_by(trt, dia) %>% 
  summarise(fc.m = mean(fc, na.rm = T)) %>% 
  mutate(dia = as.numeric(as.character(dia)))

p <- ggplot(data = af.resumo,
            mapping = aes(x = dia,
                          y = fc.m,
                          colour = trt)) +
  geom_point() +
  geom_line() +
  labs(x = "Tempo (dias)",
       y = "Força corporal",
       colour = "Tratamento")
p


## ----mlem, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------------------------------------
af.longo <- as.data.frame(af.longo)
library(nlme)
mod1 <- lme(fc ~ trt*tempo,
            random = ~ 1 + tempo | id,
            na.action = na.omit,
            data = af.longo)

