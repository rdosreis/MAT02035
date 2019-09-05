## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='40%', paged.print=FALSE----
knitr::include_graphics(here::here('images', 'Rlogo.png'))


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', paged.print=FALSE----

library(cowplot)
library(ggplot2)

p1 <- ggdraw() + draw_image(here::here('images', 'bell_labs.jpg'), scale = 0.9)
p2 <- ggdraw() + draw_image(here::here('images', 'john_chambers.jpg'), scale = 0.9)

plot_grid(p1, p2)


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='45%', paged.print=FALSE----

library(cowplot)
library(ggplot2)

p1 <- ggdraw() + draw_image(here::here('images', 'University_of_Auckland_Coat_of_Arms.png'), scale = 0.7)
p2 <- ggdraw() + draw_image(here::here('images', 'Robert_e_Ross.png'), scale = 0.9)

plot_grid(p1, p2)


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'R_console.jpg'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Hello_bloco.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Hello_notepad.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'RStudio.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'geom_scatter.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='90%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'compare_groups.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'stargazer.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='60%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'forest.jpg'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'coef_variacao_modelo-1.png'))



## ---- echo=TRUE, eval=FALSE----------------------------------------------
## # Run the application
## shinyApp(ui = ui, server = server)


## ---- echo=TRUE, eval=FALSE----------------------------------------------
## install.packages("tidyverse")


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'install_packs.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'install_packs2.png'))



## ---- echo=TRUE, eval=FALSE----------------------------------------------
## library("tidyverse")
## require("tidyverse")


## ---- echo=TRUE, eval=FALSE----------------------------------------------
## help(package = "tidyverse")


## ---- echo=TRUE, eval=FALSE----------------------------------------------
## ?glm
## help("glm")


## ---- echo=TRUE, eval=FALSE----------------------------------------------
## help.search("t.test")


## ---- echo=TRUE, eval=FALSE----------------------------------------------
## apropos("lm")


## ---- echo=TRUE, eval=FALSE----------------------------------------------
## demo(graphics)
## demo(persp)
## demo(Hershey)
## demo(plotmath)


## ----carrega_dados, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----
# ----------------------------------------------------
# Carregando pacotes do R
library(here)
library(haven)
library(tidyr)
library(ggplot2)
# ----------------------------------------------------
# Carregando o arquivo de dados
here::here("data", "tlc.dta")
chumbo <- read_dta(
  file = here::here("data", "tlc.dta"))


## ----carrega_dados2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----
chumbo


## ----transforma_dados, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----
chumbo.longo <- gather(data = chumbo,
                        key = "tempo",
                        value = "chumbo", -id, -trt)

chumbo.longo


## ----transforma_dados2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE----
chumbo.longo$tempo <- as.numeric(
  as.character(
    factor(chumbo.longo$tempo,
           labels = c(1, 2, 4, 6))))

chumbo.longo$trt <- factor(chumbo.longo$trt,
                           labels = c("Placebo",
                                      "Succimer"))

chumbo.longo


## ----time_plot, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"----
p <- ggplot(data = chumbo.longo,
            mapping = aes(x = tempo, y = chumbo)) +
  geom_point() +
  labs(x = "Tempo (semanas)",
       y = "Nível de chumbo no sangue (mcg/dL)")
p


## ----time_plot2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"----
p <- ggplot(data = chumbo.longo,
            mapping = aes(x = tempo, y = chumbo,
                          group = id)) +
  geom_point() +
  geom_line() +
  labs(x = "Tempo (semanas)",
       y = "Nível de chumbo no sangue (mcg/dL)")
p + theme_gray()


## ----time_plot3a, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----
library(dplyr)

chumbo.resumo <- chumbo.longo %>% 
  group_by(trt, tempo) %>% 
  summarise(chumbo.m = mean(chumbo))

chumbo.resumo


## ----time_plot3b, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"----
p <- ggplot(data = chumbo.resumo,
            mapping = aes(x = tempo,
                          y = chumbo.m,
                          colour = trt)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(direction = -1) +
  labs(x = "Tempo (semanas)",
       y = "Média do nível de chumbo no sangue (mcg/dL)",
       colour = "Grupo de tratamento")
p + theme_dark()


## ----time_plot3c, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----

chumbo.resumo <- chumbo.longo %>% 
  group_by(trt, tempo) %>% 
  summarise(chumbo.m = mean(chumbo),
            dp = sd(chumbo), n = n()) %>% 
  mutate(ep = dp/sqrt(n))

chumbo.resumo


## ----time_plot3d, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"----
p <- ggplot(data = chumbo.resumo,
            mapping = aes(x = tempo,
                          y = chumbo.m,
                          colour = trt)) +
  geom_errorbar(aes(ymin = chumbo.m - ep,
                    ymax = chumbo.m + ep),
                width = .1,
                position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) +
  geom_line(position = position_dodge(0.1)) +
  labs(x = "Tempo (semanas)",
       y = "Média do nível de chumbo no sangue (mcg/dL)",
       colour = "Grupo de tratamento")
p + theme_bw() + theme(legend.position = "bottom")


## ----fev, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-------------
fev <- read_dta(
  file = here::here("data", "fev1.dta"))
fev
fev <- fev[- which(fev$logfev1/fev$ht < -0.5), ]


## ----fev2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"----
p <- ggplot(data = fev,
            mapping = aes(x = age, y = logfev1/ht)) +
  geom_point() +
  labs(x = "Idade (anos)",
       y = "Log(FEV1/Altura)")
p + theme_gray()


## ----fev3, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"----
p <- ggplot(data = fev,
            mapping = aes(x = age, y = logfev1/ht,
                          group = id)) +
  geom_point(alpha = 0.3) +
  geom_line(alpha = 0.3) +
  labs(x = "Idade (anos)",
       y = "Log(FEV1/Altura)")
p + theme_gray()


## ----fev4, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"----
p <- ggplot(data = fev,
            mapping = aes(x = age, y = logfev1/ht)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Idade (anos)",
       y = "Log(FEV1/Altura)")
p + theme_gray()


## ----correlacao, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE------
chumbo.succimer <- chumbo %>% 
  filter(trt == 1) %>% 
  select(y0, y1, y4, y6)

chumbo.succimer


## ----correlacao2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="80%"----
# library(GGally)
# 
# p <- ggpairs(chumbo.succimer,
#              columnLabels = paste("Semana", c(0, 1, 4, 6)))
# p 

pairs(chumbo.succimer, , pch = 19, upper.panel = NULL)


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----
knitr::include_graphics(here::here('images', 'supeRRR.jpg'))

