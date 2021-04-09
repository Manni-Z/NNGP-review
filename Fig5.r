

# generate inputs
x <- seq(-6, 6, 0.01)
## left, for the right graphs just change the seed and eta
# generate weights and biases
set.seed(45)   # right: 159
n <- 10000
eta <- 1.3   # right: 1.7
# sample u a 
tlike <- function(x, eta) {
  (1 + x^2/(eta-1))^(-(eta-1)/2)
}
xrange <- 100 # function range from 0 (implicit) to x
N <- 100000 # number of samples
xy <- data.frame(proposed = runif(N, min = 0, max = xrange))
xy$fit <- tlike(x = xy$proposed, eta=eta)/2
xy$random <- runif(N, min = 0, max = 1)
maxDens <- max(xy$fit)
xy$accepted <- with(xy, random <= fit/maxDens)
xy <- xy[xy$accepted, ]
cand <- xy$proposed*sample(c(-1,1), length(xy$proposed), replace = T)
u_1 <- sample(cand, n, replace = T)
a <- sample(cand, n, replace = T)
b_k <- rnorm(1)
v_k <- rnorm(n, sd = rep(sqrt(1/n), n))
# compute output
nn_tanh <- function(input, ba=a, wu=u_1, bb=b_k, wv=v_k){
  h <- tanh(a + u_1*input)
  f <- b_k + sum(v_k*h)
  return(f)
}
# generate prior over function
f_k11 <- lapply(x, nn_tanh)
# generate weights and biases
set.seed(32456)   # right: 159
# xx <- rgamma(n,shape = eta/2+1, rate = (eta-1)/2)
# A <- xx^(-1/2)
# a <- rt(n, eta-1)
# u_1 <- rt(n, eta-1)
xy <- data.frame(proposed = runif(N, min = 0, max = xrange))
xy$fit <- tlike(x = xy$proposed, eta=eta)/2
xy$random <- runif(N, min = 0, max = 1)
maxDens <- max(xy$fit)
xy$accepted <- with(xy, random <= fit/maxDens)
xy <- xy[xy$accepted, ]
cand <- xy$proposed*sample(c(-1,1), length(xy$proposed), replace = T)
u_1 <- sample(cand, n, replace = T)
a <- sample(cand, n, replace = T)
b_k <- rnorm(1)
v_k <- rnorm(n, sd = rep(sqrt(1/n), n))
# generate prior over function
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
