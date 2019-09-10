## ----exp_corr, echo=FALSE, eval=TRUE, fig.align='center', message=FALSE, warning=FALSE, out.width='95%', paged.print=FALSE----

rho <- function(u, phi){exp(- phi * u)}
u <- seq(0, 5, by = 0.1)
par(mfrow = c(1, 3))
plot(u, rho(u = u, phi = 0.5),
     type = "l",
     main = expression(phi == 0.5),
     xlab = "u", ylab = expression(rho(u)),
     col = "steelblue", lwd = 2, ylim = c(0, 1))
plot(u, rho(u = u, phi = 1),
     type = "l",
     main = expression(phi == 1),
     xlab = "u", ylab = expression(rho(u)),
     col = "steelblue", lwd = 2, ylim = c(0, 1))
plot(u, rho(u = u, phi = 2),
     type = "l",
     main = expression(phi == 2),
     xlab = "u", ylab = expression(rho(u)),
     col = "steelblue", lwd = 2, ylim = c(0, 1))

