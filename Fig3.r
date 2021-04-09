# generate inputs
x <- seq(-6, 6, 0.01)
## left
# generate weights and biases
set.seed(3907)   # right: 123
n <- 300
a <- rnorm(n)
u_1 <- rnorm(n)
b_k <- rnorm(1)
v_k <- rnorm(n, sd = rep(sqrt(1/n), n))
# step function
stepf <- function(val){
  if (val < 0)
    return(-1)
  else
    return(1)
}
# compute output
nn_step <- function(input, ba=a, wu=u_1, bb=b_k, wv=v_k){
  h <- as.numeric(lapply(a + u_1*input, stepf))
  f <- b_k + sum(v_k*h)
  return(f)
}
# generate prior over function
f_k11 <- lapply(x, nn_step)
# generate weights and biases
set.seed(307)   # right: 345
a <- rnorm(n)
u_1 <- rnorm(n)
b_k <- rnorm(1)
v_k <- rnorm(n, sd = rep(sqrt(1/n), n))
# generate prior over function
f_k12 <- lapply(x, nn_step)
df11 <- data.frame(matrix(c(as.numeric(x), as.numeric(f_k11)), byrow = F, ncol = 2))
names(df11) <- c('x', 'Prior_over_function')
df12 <- data.frame(matrix(c(as.numeric(x), as.numeric(f_k12)), byrow = F, ncol = 2))
names(df12) <- c('x', 'Prior_over_function')
p11 <- ggplot() +
  geom_line(df11, mapping = aes(x=x, y=Prior_over_function)) +
  geom_line(df12, mapping = aes(x=x, y=Prior_over_function), color = 'indianred1')
p11
p12 <- ggplot() +
  geom_line(df11, mapping = aes(x=x, y=Prior_over_function)) +
  geom_line(df12, mapping = aes(x=x, y=Prior_over_function), color = 'indianred1') + 
  xlim(-1, 1) + 
  ylim(-2, 1.6)
p12
