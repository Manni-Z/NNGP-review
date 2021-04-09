library(ggplot2)

x <- log(c(100, 300, 500, 700, 1000, 3000, 5000, 7000))
relu_ac <- c(0.768, 0.862, 0.888, 0.905, 0.924, 0.955, 0.965, 0.965)
relu_mse <- c(0.442462, 0.032599, 0.027526, 0.024321, 0.021215, 0.014121, 0.011559, 0.011316)
tanh_ac <- c(0.76, 0.857, 0.883, 0.901, 0.926, 0.954, 0.965, 0.959)
tanh_mse <- c(0.045034, 0.033284, 0.027927, 0.024479, 0.021232, 0.013928, 0.011316, 0.013487)

df11 <- data.frame(matrix(c(x, relu_ac), byrow = F, ncol = 2))
names(df11) <- c('x', 'Accuracy')
df12 <- data.frame(matrix(c(x, relu_mse), byrow = F, ncol = 2))
names(df12) <- c('x', 'MSE')
df21 <- data.frame(matrix(c(x, tanh_ac), byrow = F, ncol = 2))
names(df21) <- c('x', 'Accuracy')
df22 <- data.frame(matrix(c(x, tanh_mse), byrow = F, ncol = 2))
names(df22) <- c('x', 'MSE')

baseplot_singleWay=function(g){
  g=g+xlab('Trainning set size')+ylab('Accuracy')
  #g=g+scale_y_continuous(breaks = newpoints$x,minor_breaks=NULL) 
  g=g+scale_x_continuous(breaks = df11$x,minor_breaks=NULL,labels=c(100, 300, 500, 700, 1000, 3000, 5000, 7000))
  #g=g+coord_fixed()
  g=g+guides(size=FALSE)+theme_bw()
  g
}
g_ac=baseplot_singleWay(ggplot())
p <- g_ac +
  geom_point(df11, mapping = aes(x=x, y=Accuracy, color = 'Tanh')) +
  geom_line(df11, mapping = aes(x=x, y=Accuracy, color = 'Tanh')) +
  geom_point(df21, mapping = aes(x=x, y=Accuracy, color = 'ReLU'), shape = 2) +
  geom_line(df21, mapping = aes(x=x, y=Accuracy, color = 'ReLU')) +
  labs(color="Nonlinearity") +
  theme(legend.position = c(.13,0.87))
p


baseplot_singleWay=function(g){
  g=g+xlab('Trainning set size')+ylab('MSE')
  #g=g+scale_y_continuous(breaks = newpoints$x,minor_breaks=NULL) 
  g=g+scale_x_continuous(breaks = df11$x,minor_breaks=NULL,labels=c(100, 300, 500, 700, 1000, 3000, 5000, 7000))
  #g=g+coord_fixed()
  g=g+guides(size=FALSE)+theme_bw()
  g
}
g_mse=baseplot_singleWay(ggplot())

p_mse <- g_mse +
  geom_point(df12, mapping = aes(x=x, y=MSE, color = 'Tanh')) +
  geom_line(df12, mapping = aes(x=x, y=MSE, color = 'Tanh')) +
  geom_point(df22, mapping = aes(x=x, y=MSE, color = 'ReLU'), shape = 2) +
  geom_line(df22, mapping = aes(x=x, y=MSE, color = 'ReLU')) +
  labs(color="Nonlinearity") +
  theme(legend.position = c(0.85,0.87))
p_mse

#scale_x_continuous(trans='log2') +
#coord_trans(x="log10") +
