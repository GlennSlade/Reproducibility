#This script is part of the "Reproducibility Project" investigating the reproducibility of
# canopy height measurements from drone acquired SfM photogrammetry under different wind and illumination conditions
#This script imports logging data from a Kestrel Weather station and calculates 
#max, min, average and SD for wind data during survey

#---------- 0. Setup Environment----
library (tidyverse)   

#---------- 1. Read Data ----
Wind_data <- read_csv ("C:/Workspace/Reproducibility/S1_Wind_Data.csv") # Wind data

str(Wind_data)
