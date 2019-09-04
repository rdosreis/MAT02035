## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='40%', paged.print=FALSE----
knitr::include_graphics(here::here('images', 'Rlogo.png'))


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', paged.print=FALSE----

library(cowplot)
library(ggplot2)

p1 <- ggdraw() + draw_image(here::here('images', 'bell_labs.jpg'), scale = 0.9)
p2 <- ggdraw() + draw_image(here::here('images', 'john_chambers.jpg'), scale = 0.9)

plot_grid(p1, p2)


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', paged.print=FALSE----

library(cowplot)
library(ggplot2)

p1 <- ggdraw() + draw_image(here::here('images', 'University_of_Auckland_Coat_of_Arms.png'), scale = 0.9)
p2 <- ggdraw() + draw_image(here::here('images', 'Robert_e_Ross.png'), scale = 0.9)

plot_grid(p1, p2)


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'R_console.jpg'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Hello_bloco.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Hello_notepad.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'RStudio.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='90%', paged.print=FALSE----
knitr::include_graphics('Figuras/compare_groups.png')


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', paged.print=FALSE----
knitr::include_graphics('Figuras/stargazer.png')


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='60%', paged.print=FALSE----
knitr::include_graphics('Figuras/forest.jpg')


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', paged.print=FALSE----
knitr::include_graphics('Figuras/coef_variacao_modelo-1.png')


## ---- echo=TRUE, eval=FALSE----------------------------------------------
## # Run the application
## shinyApp(ui = ui, server = server)


## ---- echo=TRUE, eval=FALSE----------------------------------------------
## install.packages("survey")


## ---- echo=TRUE, eval=FALSE----------------------------------------------
## library("survey")
## require("survey")


## ---- echo=TRUE, eval=FALSE----------------------------------------------
## help(package = "survey")


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


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----
knitr::include_graphics(here::here('images', 'supeRRR.jpg'))

