# Calculate linear regression for UAA and discharge #

# Author: Bridget Bittmann
# Date created: 04-06-2023

# Purpose: This script will check if data is normalized, normalize data, and run a linear regression.

# Import packages ####
library(dplyr)
library(Matrix) # Gets tidyverse to import
library(tidyverse) # data manipulation
library(ggplot2)

# Set your directory ####

cd <- '~/Desktop/Field Methods/DCEW_streamflow/'

# Import the data ####

q_vals <- read.csv(paste(cd, 'Data/q_vals.csv', sep=''))
q_vals$Date <- as.Date(q_vals$Date)

# Check for normality ####
hist(q_vals$q)
shapiro.test(q_vals$q)
# p-value is 0.32, meaning data not signficantly different from normal dist.

# Linear regression ####
mod <- lm(q ~ uaa, data = q_vals)
summary(mod)

# Visualize the regression ####
reg <- ggplot(data = q_vals, aes(x = uaa, y = q)) +
  geom_point() +
  geom_smooth(method = 'lm', fill = 'grey', color = 'black') +
  theme_bw() +
  ylab('Discharge (L/s)') +
  xlab('Upslope Accumulated Area (km^2)') +
  geom_text(x=26.15,y=30, label = 'R2 = 0.11')
reg

ggsave(paste(cd, 'Figures/regress.png', sep=''),
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
ggsave(paste(cd,'Figures/ind_reg.svg', sep=''),
       plot = reg,
       width = 6.5,
       height = 4)
