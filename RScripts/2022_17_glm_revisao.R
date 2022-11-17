## ----scatres, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, out.height="85%", fig.align='center'----

plot(x = seq(-5, 5, by = .1),
     y = plogis(q = seq(-5, 5, by = .1)), type = "l",
     xlab = "x",
     ylab = "Probabilidade de sucesso",
     lwd = 2,
     col = "purple",
     lty = 2)



## ----reglog, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, out.height="85%", fig.align='center'----

reglog <- function (x){
  pi <- exp(4.0343 - x * 0.0042)/(1 + exp(4.0343 - x * 0.0042))
  pi
}

plot(x = seq(400, 1800, by = 1),
     y = reglog(x = seq(400, 1800, by = 1)), type = "l",
     xlab = "Peso (gramas)",
     ylab = "Probabilidade estimada de DBP",
     lwd = 2,
     col = "purple",
     lty = 2)


