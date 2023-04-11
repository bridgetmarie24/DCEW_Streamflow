# Dilution gauging discharge calculations ##

# Date created: 03-01-23
# Author: Bridget Bittmann
# Purpose: This script will import raw .dat files for specific conductivity and will output
#          streamflow measurements. This exports a .csv file with flow measurements calculated.


# Import packages ####
library(Matrix) # for tidyverse to load
library(tidyverse) # to manipulate dataframes
library(ggplot2) # for plotting
library(ggpubr) # for creating multi-plot figures
library(haven) # to import .dat files
library(pracma) # area under the curve

cd <- '~/Desktop/Field Methods/DCEW_Streamflow/'

# Import the data ####

field <- read.csv('~/Desktop/Field Methods/DCEW_Streamflow/Data/Streamflow_Data.csv')
field$Date <- as.Date(field$Date)

# This function will import the .dat file and only take the necessary data between start
# and end time points
dat_file <- function (file.path, start.time, end.time) {
  df <- read.delim(file.path, header = T, sep = ',', skip = 1)
  df <- df[-c(1,2),]
  str(df) # This is used to check the datatype of each column
  df$Time <- as.POSIXct(df$TIMESTAMP) # makes TIMESTAMP column in date/time format
  df$Cond <- as.numeric(df$Cond)
  df$Temp <- as.numeric(df$PTemp_C)
  df <- df[,c('Time', 'Cond', 'Temp')]
  df <- subset(df, Time > start.time & Time < end.time) # crop data to start and end time
  return(df)
}

# Import day 3 files # 
# This is imported separately because it is all one .dat file rather than individuals

day3 <- subset(field, Date == '2023-03-10') # deal with data all in one file
day3 <- subset(day3, Site_Name != 'MidMain1') # bad data, need to not use this

# Creat a list to store the dataframes for day 3
df_field <- list()
for (i in 1:length(day3$Date)) {
  name <- day3$ID[i] # create unique name for list
  print(name)
  
  # Import the data and clip to start and end time
  file <- dat_file('~/Desktop/Field Methods/DCEW_Streamflow/FieldData/everything_2023.dat',
                   start.time = paste(as.character(day3$Date[i]), day3$Start_time[i], sep = ' '),
                   end.time = paste(as.character(day3$Date[i]), day3$End_time[i], sep = ' '))
  
  # Visualize to make sure you have full time extent
  print(ggplot(data = file, aes(x = Time, y = Cond)) +
          geom_point() +
          geom_line() +
          ggtitle(name))
  
  #Save to list
  df_field[[name]] <- file
}

# Import rest of files #

# Missing LowMain 2 from field day 1
field_sub <- subset(field, ID != '022423_LowMain_2' & Date != '2023-03-10')

for (i in 1:length(field_sub$Date)) {
  name <- field_sub$ID[i] # create unique name for list
  file_name <- paste('~/Desktop/Field Methods/DCEW_Streamflow/FieldData/', field_sub$ID[i], '.dat', sep = '')
  # Import the data and clip to start and end time
  file <- dat_file(file_name,
                   start.time = paste(as.character(field_sub$Date[i]), field_sub$Start_time[i], sep = ' '),
                   end.time = paste(as.character(field_sub$Date[i]), field_sub$End_time[i], sep = ' '))
  
  # Visualize to make sure you have full time extent
  print(ggplot(data = file, aes(x = Time, y = Cond)) +
          geom_point() +
          geom_line() +
          ggtitle(name))
  
  #Save to list
  df_field[[name]] <- file
}

# Steps for function to calculate discharge ####
  # 1) Subtract out background conductivity
  # 2) Calculate the area under the curve
  # 3) Use calculation to get discharge

discharge <- function (df, mass, k_val, name){
  
  # Take the first and last 5 values of the dataframe to compare beginning and end background
  interp <- rbind(head(df, 5), tail(df,5))
  
  # If there is a difference between beginning and end background, 
  # fit a linear regression to subtract out the background
  if (as.numeric(mean(head(df$Cond, 5))- mean(tail(df$Cond, 5))) > 0.015 ){
    reg <- lm(Cond ~ Time, data = interp) #Fit a linear regression between date/time and cond
    df$pred <- predict(reg, newdata = data.frame(Time = df$Time)) # predict the regression
    df$dif <- df$Cond - df$pred # calculate the difference in conductivity
    df$dif[df$dif < 0] <- 0 # replace any negative values with 0
  }
  
  # If there is no difference in background between beginning and end,
  # Use the mean background conductivity to get rid of background
  else { 
    df$dif <- df$Cond- mean(interp$Cond) # subtract the mean background value from the conductivity
    df$dif[df$dif < 0] <- 0# replace any negative values with 0
  }
  
  # store the dataframe
  ret <- list(data = df)
  
  # Plot again , will look the same but shifted down so background conductivity is gone
  ret$plt<- ggplot(data = df, aes(x = Time, y = dif)) +
    geom_point() +
    geom_line() +
    ylab('Difference in EC') +
    ggtitle(name) +
    theme_bw()
  
  # Take the area under the curve
  time <- df[['Time']] # pull time column
  df$step <- 1:length(df$Time)*2 # create the time step
  cond <- df[['dif']] * 1000 # converts from mS/cm to uS/cm 
  area = trapz(df$step, cond) # integrate using trapezoidal method
  ret$area <- area # store area in output
  
  # Use area under the curve and equation to get streamflow
  
  # Equation #
  # -------- #
  # Q = M/(K*A*t)
  
  # K value: specific conductivity for the salt of the slug injection in the water you are using
  # relationship between specific conductance and concentration
  # Q : calculated discharge (L/s)
  # M : Mass of salt used (mg)
  # A : The area 
  # t: Time step
  
  k = k_val
  m = mass * 1000
  t = 2
  
  ret$q <- m/(area*k*t)
  return(ret)
}

# Calculate discharge across the different days ####

# Missing lowmain2 from day one so remove from field dataframe and remove MidMain1 from last day
sub <- subset(field, !(ID %in% c('022423_LowMain_2', '031023_Midmain_1' )))

# Creating a list to store discharge
dis_full <- list() #creating a list to store discharge
q_vals <- data.frame(q = 1:length(sub$Date)) # dataframe for q values
plots <- list()

for (i in 1:length(sub$ID)){
  sub_field <- subset(sub, ID == sub$ID[i]) #subset the original csv
  mass <- sub_field$NaBr_g[1] # grab the mass of the salt
  df <- df_field[[sub$ID[i]]] # get the correct df from list of dataframes
  dis <- discharge(df = df, mass = mass, k_val = .756, name = sub$ID[i]) # run discharge function
  dis_full[[sub$ID[i]]] <- dis # store discharge output in a list
  q_vals$q[i] <- dis$q
  q_vals$ID[i] <- sub$Site_Name[i]
  q_vals$Date[i] <- as.character(sub$Date[i])
  q_vals$Area[i] <- dis$area
  q_vals$uaa[i] <- sub$UAA[i]
  plots[[i]] <- dis$plt
}

# Export the dataframe #
out_path <- paste('~/Desktop/Field Methods/DCEW_Streamflow/Data/q_vals.csv')
write.csv(q_vals, out_path)

# Create multiplot figure of breakthrough curves
breakthrough.plt <- ggarrange(plotlist = plots)
out_path <- paste('~/Desktop/Field Methods/Figures/breakthrough.png')
ggsave(out_path,
       breakthrough.plt, 
       width = 16,
       height = 13)

# Run a sensitivity analysis on the K values ####
k.sens <- seq(0.30, 1.1, by =0.05)
df <- dis_full[[sub$ID[1]]]$data # High Main Sample 1 02-24-23
m <- sub$NaBr_g[1] # pull mass of salt
df.sens <- data.frame(K = k.sens) # create dataframe to store discharge

# Loop over and store the different discharge with different k values
for (i in 1:length(k.sens)){
  dis <- discharge(df = df, mass = m, k_val = k.sens[i])
  df.sens$q[i] <- dis$q
}

# Export the csv
out_path <- paste(cd, '/Data/k_sens.csv', sep='') 
write.csv(df.sens, out_path)



