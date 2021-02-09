## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE-------------

library(ggplot2)
library(cowplot)

p1 <- ggplot(data.frame(y = c(-4, 4)), aes(x = y)) +
  stat_function(fun = dnorm, args = list(0, 1), aes(colour = "N(0,1)"),  size = 1.5) +
  stat_function(fun = dnorm, args = list(1, 1), aes(colour = "N(1,1)"),  size = 1.5) +
  stat_function(fun = dnorm, args = list(-1, 2), aes(colour = "N(-1,2)"),  size = 1.5) +
  scale_y_continuous(name = "f(y)") +
  scale_colour_brewer(palette="Accent") +
  labs(colour = "Distribuições") +
  theme_bw() +
  theme(legend.position = c(0.2,0.8))

funcShaded <- function(x) {
    y <- dnorm(x, mean = 0, sd = 1)
    y[x < 0 | x > (0 + 2 * 1)] <- NA
    return(y)
}

p2 <- ggplot(data.frame(y = c(-4, 4)), aes(x = y)) +
  stat_function(fun = dnorm, args = list(0, 1), colour = "#BEAED4", size = 1.5) +
  scale_y_continuous(name = "f(y)") +
  theme_bw() +
  stat_function(fun = funcShaded, geom = "area", fill = "#BEAED4", alpha = 0.2) +
  annotate(geom = 'text', x = 0.025, y = 0.05, color = 'black', label = 'Pr(0 < Y < 2)', hjust = -0.1)

plot_grid(p1, p2)

