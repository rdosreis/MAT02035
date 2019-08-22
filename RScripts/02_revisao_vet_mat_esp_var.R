## ----escalar-------------------------------------------------------------
b <- 5
b


## ----vetor---------------------------------------------------------------
V <- c(2, 4, 9, 7, 3)
V
length(V)


## ----matriz--------------------------------------------------------------
A <- matrix(data = c(2, 7, 11, 5,
                     4, 9, 3, 1,
                     13, 8, 2, 6),
            nrow = 3, byrow = TRUE)
A
dim(A)


## ---- echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE----------------
library(here)
library(haven)
library(tidyr)

chumbo.df <- read_dta(file = here::here("data", "tlc.dta"))
chumbo.df.longo <- gather(data = chumbo.df, key = "tempo", value = "chumbo", -id, -trt)
chumbo.df.longo$tempo <- factor(chumbo.df.longo$tempo, labels = c("0", "1", "4", "6"))

chumbo1 <- chumbo.df.longo[chumbo.df.longo$id == 1, ]
chumbo1 <- chumbo1[,-1]
chumbo1 <- chumbo1[c(3,1,2)]

knitr::kable(chumbo1, col.names = c("Chumbo no sangue", "Grupo de tratamento", "Semana"), caption = "Dados de um indivíduo participante do estudo do Chumbo.")


## ---- echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE----------------
library(mat2tex)

y <- chumbo1$chumbo


## ---- echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, results='asis'----
"$$ Y  = \\left(\\begin{array}{c}
Y_1\\\\
Y_2\\\\
Y_3\\\\
Y_4
\\end{array}\\right) = " %_% xm(t(t(y)), 1) %_% ".$$"


## ---- echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE----------------
x <- cbind(rep(1, 4), chumbo1[c(2,3)])


## ---- echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE, results='asis'----
"$$ X  = \\left(\\begin{array}{ccc}
X_{11} & X_{12} & X_{13} \\\\
X_{21} & X_{22} & X_{23} \\\\
X_{31} & X_{32} & X_{33} \\\\
X_{41} & X_{42} & X_{43} 
\\end{array}\\right) = " %_% xm(x, 1) %_% ".$$"


## ----matriz_transposta---------------------------------------------------
A
t(A)


## ----matriz_quadrada-----------------------------------------------------
S <- matrix(data = c(2, 3, 7, 11,
                     3, 9, 1, 2,
                     7, 1, 5, 8,
                     11, 2, 8, 4),
            nrow = 4, ncol = 4,
            byrow = TRUE)
all.equal(S, t(S))


## ----matriz_diagonal-----------------------------------------------------
# cria matriz diagonal 
I <- diag(x = rep(1, 4))
I
# retorna os elementos da diagonal principal da matriz
diag(x = S)


## ----soma_matriz---------------------------------------------------------
G <- matrix(data = c(2, 7, 11, 4, 9, 3, 13, 8, 2),
            nrow = 3, byrow = TRUE)
J <- matrix(data = c(3, 2, 14, 7, 8, 4, 6, 5, 9),
            nrow = 3, byrow = TRUE)
G + J
G - J


## ----escalar_matriz------------------------------------------------------
2 * A


## ----multiplica_matriz, error=TRUE---------------------------------------
A <- matrix(data = c(2, 7, 11, 4, 9, 3, 13, 8, 2),
            nrow = 3, byrow = TRUE)
B <- matrix(data = c(1, 2, 3, 1, 2, 4),
            nrow = 3, byrow = TRUE)
A %*% B


## ----multiplica_matriz2, error=TRUE--------------------------------------
G %*% J
J %*% G

# CUIDADO!
G * J


## ----inversa_det, error=TRUE---------------------------------------------
# determinante de G
det(G)

# G é singular?
det(G) != 0

# inversa de G
solve(G)


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='60%', paged.print=FALSE----
knitr::include_graphics(here::here('images', 'matrix.jpg'))

