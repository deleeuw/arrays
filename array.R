set.seed(12345)

a <- array(rnorm(60), c(3, 4, 5))

x <- qr.Q(qr(matrix(c(1, 1, 1, rnorm(6)), 3, 3)))
y <- qr.Q(qr(matrix(c(1, 1, 1, 1, rnorm(12)), 4, 4)))
z <- qr.Q(qr(matrix(c(1, 1, 1, 1, 1, rnorm(20)), 5, 5)))

avec <- as.vector(a)
b <- array(0, dim(a))

for (i in 1:3) {
  for (j in 1:4) {
    for (k in 1:5) {
      b[i, j, k] <- sum(avec * kronecker(z[, k], kronecker(y[, j], x[, i])))
    }
  }
}

c <- array(0, dim(a))

for (i in 1:3) {
  for (j in 1:4) {
    for (k in 1:5) {
      c[i, j, k] <- sum(a * outer(outer(x[, i], y[, j]), z[, k]))
    }
  }
}

