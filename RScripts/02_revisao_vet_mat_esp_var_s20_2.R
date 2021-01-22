## ----escalar------------------------------------------------------------------------------
b <- 5
b


## ----vetor--------------------------------------------------------------------------------
V <- c(2, 4, 9, 7, 3)
V
length(V)


## ----matriz-------------------------------------------------------------------------------
A <- matrix(data = c(2, 7, 11, 5,
                     4, 9, 3, 1,
                     13, 8, 2, 6),
            nrow = 3, byrow = TRUE)
A
dim(A)








## ---- echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE---------------------------------
x <- cbind(rep(1, 4), chumbo1[c(2,3)])




## ----matriz_transposta--------------------------------------------------------------------
A
t(A)


## ----matriz_quadrada----------------------------------------------------------------------
S <- matrix(data = c(2, 3, 7, 11,
                     3, 9, 1, 2,
                     7, 1, 5, 8,
                     11, 2, 8, 4),
            nrow = 4, ncol = 4,
            byrow = TRUE)
all.equal(S, t(S))


## ----matriz_diagonal----------------------------------------------------------------------
# cria matriz diagonal 
I <- diag(x = rep(1, 4))
I
# retorna os elementos da diagonal principal da matriz
diag(x = S)


## ----soma_matriz--------------------------------------------------------------------------
G <- matrix(data = c(2, 7, 11, 4, 9, 3, 13, 8, 2),
            nrow = 3, byrow = TRUE)
J <- matrix(data = c(3, 2, 14, 7, 8, 4, 6, 5, 9),
            nrow = 3, byrow = TRUE)
G + J
G - J


## ----escalar_matriz-----------------------------------------------------------------------
2 * A


## ----multiplica_matriz, error=TRUE--------------------------------------------------------
A <- matrix(data = c(2, 7, 11, 4, 9, 3, 13, 8, 2),
            nrow = 3, byrow = TRUE)
B <- matrix(data = c(1, 2, 3, 1, 2, 4),
            nrow = 3, byrow = TRUE)
A %*% B


## ----multiplica_matriz2, error=TRUE-------------------------------------------------------
G %*% J
J %*% G



## ----multiplica_matriz3, error=TRUE-------------------------------------------------------
# CUIDADO!
G * J


## ----inversa_det, error=TRUE--------------------------------------------------------------
# determinante de G
det(G)

# G é singular?
det(G) != 0

# inversa de G
solve(G)

