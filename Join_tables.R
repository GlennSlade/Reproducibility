#This script is part of the "Reproducibility Project" investigating the reproducibility of
# canopy height measurements from drone acquired SfM photogrammetry under different wind and illumination conditions
#This script merges three csv files containing SfM Canopy Height Metrics, Survey Data and Plot data into one data frame (R_data)

#---------- 0. Setup Environment----
library (tidyverse)   

#---------- 1. Read Data ----
Plot_data <- read_csv ("C:/Workspace/Reproducibility/Plot_Data.csv") # Plot Species, AGB and coordinate data

# Plot_data <- read_csv ("C:/Workspace/Reproducibility/Plot_Data.csv",
#                        col_types = cols(PlotGenus = col_character))

str(Plot_data)

Survey_data <- read_csv ("C:/Workspace/Reproducibility/Survey_Data.csv") # Survey Date, Time, Wind and illumination Conditions

SfM_data <- read_csv ("C:/Workspace/Reproducibility/SfM_CH_Data.csv") # Reconstructed Canopy Height Metrics for each plot calculated from .las files from each survey
# as_tibble(Plot_data) #turns data frame into tidy tibble
# view (Plot_data)
# as_tibble(Survey_data) #turns data frame into tidy tibble
# view (Survey_data)
# as_tibble(SfM_data) #turns data frame into tidy tibble
# view (SfM_data)

# ---- 2. Tidy Data ----
SfM_Plot_data <- left_join(Plot_data,SfM_data,by = "PlotID") # Merges Plot and SfM Data using Plot ID



Survey_SfM_Plot_data <- left_join(SfM_Plot_data,Survey_data,by = "SurveyID") # Merges Survey and SfM_Plot Data using Survey ID

R_data <- Survey_SfM_Plot_data # Final data set named R_data to be used in all subsequent analysis

write.csv(R_data,"C:/Workspace/Reproducibility/R_data.csv") # exports the tibble as a csv file


