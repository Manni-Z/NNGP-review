### simulation of GP 

## kernel function
k <- function(x, x_prime, type = c('se', 'brownian', 'laplace')){
  if (type == 'se') 
    return(exp(-100*(x-x_prime)^2))
  if (type == 'brownian')
    return(min(x,x_prime))
  if (type == 'laplace')
    return(exp(-abs(x-x_prime)))
}

## points to sample
x <- seq(0, 1, 0.005)
n <- length(x)

## construct kernel
t <- c('se', 'brownian', 'laplace')
# squared exponential kernel
c1 <- matrix(rep(0,n*n), nrow = n)
for (i in 1:n) {
  for (j in 1:n) {
    c1[i,j] <- k(x[i], x[j], t[1])
  }
}
# standard brownian motion
c2 <- matrix(rep(0,n*n), nrow = n)
for (i in 1:n) {
  for (j in 1:n) {
    c2[i,j] <- k(x[i], x[j], t[2])
  }
}

# sample
# squared exponential kernel
set.seed(3)
u <- rnorm(n)
s1 <- svd(c1)
d1 <- diag(s1$d)
z1 <- s1$u%*%sqrt(d1)%*%u
# standard brownian motion
s2 <- svd(c2)
d2 <- diag(s2$d)
z2 <- s2$u%*%sqrt(d2)%*%u

## plot
plot(x, z1, pch = 20, cex = 0.5, main = 'Squared exponential kernel')
lines(x, z1, type = 'l')
plot(x, z2, pch = 20, cex = 0.5, main = 'Standard Brownian motion')
lines(x, z2, type = 'l')
