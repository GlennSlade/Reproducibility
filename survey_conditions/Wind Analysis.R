#This script is part of the "Reproducibility Project" investigating the reproducibility of
# canopy height measurements from drone acquired SfM photogrammetry under different wind and illumination conditions
#This script imports logging data from a Kestrel Weather station and calculates 
#max, min, average and SD for wind data during survey

#---------- 0. Setup Environment----
library (tidyverse)   

#---------- 1. Read Data, clean columns and assign headers----

# reading wind and environmental data exported from Kestrel Weather station
headers = read.csv("C:/Workspace/Reproducibility/Wind/S999_wind_data.csv", skip = 3, header = F, nrows = 1, as.is = T)
df = read.csv("C:/Workspace/Reproducibility/Wind/S999_wind_data.csv", skip = 5, header = F)
colnames(df)= headers
#view (df)
#str (df)
df2 <- select(df, 'Wind Speed', 'Barometric Pressure', 'Temperature', 'Relative Humidity')
#view (df2)
#str (df2)


#Reading in survey data table for results
Survey_data <- read_csv ("C:/Workspace/Reproducibility/Survey_Data.csv") # Data Frame with Survey Date, Time, Wind and illumination Conditions
view (Survey_data)

#-----2. Calculate Metrics and write to file  -----

mean (df2$'Wind Speed')
max (df2$'Wind Speed')
sd (df2$'Wind Speed')
mean (df2$'Temperature')
mean (df2$'Barometric Pressure')
mean (df2$'Relative Humidity')

#Survey_data$SurveyID <- "S999"

Survey_data$Wind_Av <- mean (df2$'Wind Speed')
Survey_data$Temp <- mean (df2$'Temperature')
Survey_data$Humidity <- mean (df2$'Relative Humidity')
Survey_data$Wind_Max <- max (df2$'Wind Speed')
Survey_data$Wind_SD <- sd (df2$'Wind Speed')
view (Survey_data)

