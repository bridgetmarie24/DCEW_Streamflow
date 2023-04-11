# Calculate linear regression for UAA and discharge #

# Author: Bridget Bittmann
# Date created: 04-06-2023

# Purpose: This script will check if data is normalized, normalize data, and run a linear regression.

# Import packages ####
library(dplyr)
library(Matrix) # Gets tidyverse to import
library(tidyverse) # data manipulation
library(ggplot2)
library(ggpubr) #multiplot figures

# Set your directory ####

cd <- '~/Desktop/Field Methods/DCEW_streamflow/'
cd_figures <- '~/Desktop/Field Methods/Figures/'

# Import the data ####

q_vals <- read.csv(paste(cd, 'Data/all_sites.csv', sep=''))
q_vals$Date <- as.Date(q_vals$Date)

# Check for normality ####
hist(q_vals$q)
shapiro.test(q_vals$q)
# p-value is less than 0.05, meaning data is significantly different 
shapiro.test(log(q_vals$q)) # test on log values
# p_value is 0.32, so not signficnatly differnet
q_vals$lt <- log(q_vals$q)

# Linear regression ####
mod <- lm(lt ~ uaa, data = q_vals)
summary(mod)

# Visualize the regression ####
reg <- ggplot(data = q_vals, aes(x = uaa, y = lt)) +
  geom_point() +
  geom_smooth(method = 'lm', fill = 'grey', color = 'black') +
  theme_bw() +
  ylab('log(Discharge)') +
  xlab('Upslope Accumulated Area (km^2)') +
  geom_text(x=10,y=4.5, label = 'R2 = 0.73')
reg

ggsave(paste(cd_figures, 'regress.png', sep=''),
       plot = reg,
       width = 4,
       height = 4)

# Visualize regression for individual weeks ####
reg_ind <- ggplot(data = q_vals, aes(x = uaa, y = q, color = as.character(Date))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  ylab('Discharge (L/s)') +
  xlab('Upslope Accumulated Area (km^2)') 
reg_ind
ggsave(paste(cd_figures,'ind_reg.svg', sep=''),
       plot = reg,
       width = 6.5,
       height = 4)
