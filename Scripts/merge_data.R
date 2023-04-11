# Merge Dilution Gauging and Continuous Datasets #

# Author: Bridget Bittmann
# Date: 04/11/2023

# Purpose: This script merges data from the continous gauging stations in the DCEW and dilution 
#         gauging streamflow measurements. Data will then be exported to use in regression analysis.

# Import packages ####
library(Matrix)
library(tidyverse)

# Set working directory ####

cd <- '~/Desktop/Field Methods/DCEW_Streamflow'

# Import the data for continuous gauging stations ####

uaa <- read.csv(paste(cd,'/Data/Cont_Stations/ContSites_UAA.csv', sep=''))
file_names <- c('C1E_StreamHrlySummary_2023_Preliminary.csv',
                'C2E_StreamHrlySummary_2023_Preliminary.csv',
                'LG_StreamHrlySummary_2023_Preliminary.csv')

# Read in the dataframes and remove unimportant rows
df.cont <- list()
for (i in file_names) {
  file_path <- paste(cd, '/Data/Cont_Stations/', i, sep='')
  df <- read.csv(file_path, header=T, skip = 18)
  df$DateTime <- as.POSIXct(df$DateTime, format = '%m/%d/%Y %H:%M')
  df.cont[[i]] <- df
}

# Select the date/time for each day in the field #
dt <- c('2023-02-24 10:00:00',
        '2023-02-24 11:00:00',
        '2023-02-24 12:00:00',
        '2023-03-03 10:00:00',
        '2023-03-03 11:00:00',
        '2023-03-03 12:00:00',
        '2023-03-10 10:00:00',
        '2023-03-10 11:00:00',
        '2023-03-10 12:00:00')
dt <- as.POSIXct(dt)

# Set the site IDs for dataframe
site_names <- c('C1E',
                'C2E',
                'LG')
# Create dataframe to store discharge
discharge <- data.frame(Date = dt)
df_sites <- list()

for (i in 1:length(file_names)){
  df <- df.cont[[file_names[i]]]
  sub_df <- subset(df, DateTime %in% dt) #subset dataframe to match date/time
  discharge$q <- sub_df$Discharge.L.s # pull discharge into new dataframe
  discharge$ID <- site_names[i] # save site ID
  discharge$uaa <- uaa$UAA[uaa$Name == site_names[i]] # add UAA column
  df_sites[[i]] <- discharge
}

# Merge all dataframes into one
df.cont.full <- bind_rows(df_sites)
df.cont.full$SiteType <- 'Continuous'

# Merge dilution gauging and continuous monitoring sites streamflow ####
dilu_g <- read.csv(paste(cd,'/Data/q_vals.csv', sep=''))
dilu_g$Date <- as.POSIXct(dilu_g$Date) 
dilu_g$ID <- str_sub(dilu_g$ID, end = -2) # Remove sample number to just get the site name
dilu_g$SiteType <- 'Dilution Gauging'

df.full <- bind_rows(dilu_g, df.cont.full) # concatenates the two dataframes
df.full <- subset(df.full, select = -c(X, Area)) # drops unneccessary columns

# Export the csv 
out_file <- paste(cd, '/Data/all_sites.csv', sep = '')
write.csv(df.full, out_file)

