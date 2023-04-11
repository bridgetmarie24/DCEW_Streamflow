# Figure generation #

# Author: Bridget Bittmann
# Date: April 11, 2023

# Purpose: Create figures for streamflow report. Figures will be saved locally, not in Github.

# Import pacakges ####
library(dplyr)
library(Matrix) # Gets tidyverse to import
library(tidyverse) # data manipulation
library(ggplot2)
library(ggpubr) #multiplot figures

# Set directories

cd_data <- '~/Desktop/Field Methods/DCEW_Streamflow/Data/'
cd_figures <- '~/Desktop/Field Methods/Figures/'

# Figure for sensitivity analysis of K ####

k.sens <- read.csv(paste(cd_data, 'k_sens.csv', sep='')) #read in data

sens.plt <- ggplot(data = k.sens, aes(x = K, y = q)) +
  geom_point() + # add points for dataframe 
  geom_point(aes(x = 0.756, y = 38.65), color = 'red', size = 3.5, shape = 17)+ # add actual value
  theme_bw() + # change theme to look nice
  ylab('Discharge (L/s)') +
  xlab('K ((mg/L)/(uS/cm))') +
  theme(text = element_text(size=15, family = 'Arial')) # change font size and type
sens.plt

out_file <- paste(cd_figures, 'ksens.svg', sep='')
ggsave(out_file, 
      plot = sens.plt,
      width = 4,
      height = 4)

# Box plot for the different sites ####

df.full <- read.csv(file = paste(cd_data, 'all_sites.csv', sep=''))
df.full$lt <- log(df.full$q)

box.full <- ggplot(data = df.full, aes(x = ID, y = lt, fill = SiteType)) +
  geom_boxplot() +
  theme_bw() +
  ylab('log(Discharge)') +
  xlab('Site Name') +
  scale_fill_manual(values = c('Continuous' = 'grey', 
                    'Dilution Gauging' = '#69b3a2')) +
  theme(text = element_text(size=15, family = 'Arial'))
box.full

# Save the figure
out_file <- paste(cd_figures, 'box_site.png', sep='')
ggsave(out_file,
       plot = box.full,
       width = 6.35,
       height = 4)
