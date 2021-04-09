"""
Priors over functions drawn from networks with Tanh function and width = 10000. 
Left plots: sigma_u = sigma_a = 5, sigma_b = omega_v = 1
Right plots: sigma_u = sigma_a = 20, sigma_b = omega_v = 1
"""

# generate inputs
x <- seq(-6, 6, 0.01)
## left plots, change seed to get the right ones
# generate weights and biases
set.seed(123)
n <- 10000
a <- rnorm(n, sd = rep(5, n))   # right ones have sd = rep(20, n)
u_1 <- rnorm(n, sd = rep(5, n))   # right ones have sd = rep(20, n)
b_k <- rnorm(1)
v_k <- rnorm(n, sd = rep(sqrt(1/n), n))
# compute output
nn_tanh <- function(input, ba=a, wu=u_1, bb=b_k, wv=v_k){
  h <- tanh(a + u_1*input)
  f <- b_k + sum(v_k*h)
  return(f)
}
# draw prior over function
f_k11 <- lapply(x, nn_tanh)
# generate weights and biases
set.seed(345)
a <- rnorm(n, sd = rep(5, n))   # right ones have sd = rep(20, n)
u_1 <- rnorm(n, sd = rep(5, n))   # right ones have sd = rep(20, n)
b_k <- rnorm(1)
v_k <- rnorm(n, sd = rep(sqrt(1/n), n))
# draw another prior over function
f_k12 <- lapply(x, nn_tanh)
df11 <- data.frame(matrix(c(as.numeric(x), as.numeric(f_k11)), byrow = F, ncol = 2))
names(df11) <- c('x', 'Prior_over_function')
df12 <- data.frame(matrix(c(as.numeric(x), as.numeric(f_k12)), byrow = F, ncol = 2))
names(df12) <- c('x', 'Prior_over_function')
# plot
p1 <- ggplot() +
  geom_line(df11, mapping = aes(x=x, y=Prior_over_function)) +
  geom_line(df12, mapping = aes(x=x, y=Prior_over_function), color = 'indianred1')
p1
