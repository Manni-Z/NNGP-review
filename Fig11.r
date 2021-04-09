"""
sigma_w and sigma_b space. 
csv file for tanh and relu are named tanh_ac.csv and relu_ac.csv
"""

library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)
library( tidyverse)
library(ggplot2)
library(scales)

# relu  
# change csv file for tanh
dir <- file.choose()
exceldata = read.csv(dir,header = F)

data <- data.frame(exceldata)
names(data) <- c('weight_var', 'bias_var','accuracy')
heat <- data %>%
  group_by(weight_var, bias_var) %>%
  summarise(accuracy)

# Convert to wide-form and move Sector names to rownames so that we can get a numeric matrix

heat.mat <- heat %>%
  # Convert long-form to wide-form
  spread(key = weight_var, value = accuracy) %>%
  as.data.frame %>%
  # Extract column 'Sector' and use it to name rows. 
  # This is necessary so the final output is a numeric matrix, the input which the heatmap function takes
  column_to_rownames(var = "bias_var") %>%
  as.matrix

# The matrix looks already like how we want to visualise it
heat.mat
plot1 <- heatmap(heat.mat, Colv = NA, Rowv = NA, scale = "column")

ggplot(heat, aes(x = weight_var, bias_var)) +
  geom_tile(aes(fill = accuracy)) +
  scale_fill_gradientn(colours = c("#F3E5F5", "#9966CC","#330066"),limits = c(0.88, 0.96),oob = squish) +
  labs(fill = 'Accuracy')+
  xlab(expression(sigma[w]^2))+
  ylab(expression(sigma[b]^2))+
  theme(panel.background = element_blank())

ggplot(heat, mapping = aes(x = weight_var, bias_var)) +
  geom_vline(xintercept = 2.25, color="#9966CC",lwd=1) +
  xlab(expression(sigma[w]^2))+
  ylab(expression(sigma[b]^2))+
  xlim(1,5)+
  ylim (0,4)+
  annotate("text", x=1.5, y=2.5, label= "bounded") + 
  annotate("text", x = 4, y=2.5, label = "unbounded")+
  theme_bw()





