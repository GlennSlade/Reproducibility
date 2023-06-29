### Script to Extract Data from Reproducibility DEM data for each survey based on the 64 survey plots

library(terra)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(MASS)
library(splines)
library(rgeos)
library(tidyverse)
library(viridis)
library(gridExtra)
library(DescTools)
library(sf)
library(exactextractr)
library(writexl) 
library (raster)
library(dplyr)
library(remotes)
library(cowplot)
library(reshape2)

#----1. Read in files for DEM ----

#DEM read in the 60 surveys
S1 <- rast("E:/Glenn/Reproducibility/Processed/DEM/S1_DEM.tif")
S2 <- rast("E:/Glenn/Reproducibility/Processed/DEM/S2_DEM.tif")
S3 <- rast("E:/Glenn/Reproducibility/Processed/DEM/S3_DEM.tif")
S4 <- rast("E:/Glenn/Reproducibility/Processed/DEM/S4_DEM.tif")
S5 <- rast("E:/Glenn/Reproducibility/Processed/DEM/S5_DEM.tif")
#S5 <-terra::project(S5, y="EPSG:7405")


#Plots - read in the shapefile of 64 polygons
plots <- read_sf(dsn = "E:/Glenn/Reproducibility/Plot", layer = "plot_polygons")
#plots <- vect(plots)
#plots  <-terra::project(plots, y="EPSG:7405")
plots
# Tidy up Sf table

plots <-plots%>%dplyr:: select (-c(2,3))
plot(plots)


# Extract Data

Data <- exact_extract(S1,plots,"mean")
names(Data) <- c('S1')
Data_DF <-bind_cols(plots,Data)

Data <- exact_extract(S1,plots,"max" )
names(Data) <- c('S1_max')
Data_DF <- dplyr::mutate(Data_DF, "S1_max" = Data)

Data <- exact_extract(S2,plots,"max" )
names(Data) <- c('S2_max')
Data_DF <- dplyr::mutate(Data_DF, "S2_max" = Data)

Data <- exact_extract(S3,plots,"max" )
names(Data) <- c('S3_max')
Data_DF <- dplyr::mutate(Data_DF, "S3_max" = Data)

Data <- exact_extract(S4,plots,"max" )
names(Data) <- c('S4_max')
Data_DF <- dplyr::mutate(Data_DF, "S4_max" = Data)

Data <- exact_extract(S5,plots,"max" )
names(Data) <- c('S5_max')
Data_DF <- dplyr::mutate(Data_DF, "S5_max" = Data)

Data <- exact_extract(S6,plots,"max" )
names(Data) <- c('S6_max')
Data_DF <- dplyr::mutate(Data_DF, "S6_max" = Data)

Data <- exact_extract(S7,plots,"max" )
names(Data) <- c('S7_max')
Data_DF <- dplyr::mutate(Data_DF, "S7_max" = Data)


Data <- exact_extract(S8,plots,"max" )
names(Data) <- c('S8_max')
Data_DF <- dplyr::mutate(Data_DF, "S8_max" = Data)

Data <- exact_extract(S9,plots,"max" )
names(Data) <- c('S9_max')
Data_DF <- dplyr::mutate(Data_DF, "S9_max" = Data)

Data <- exact_extract(S10,plots,"max" )
names(Data) <- c('S10_max')
Data_DF <- dplyr::mutate(Data_DF, "S10_max" = Data)
Data <- exact_extract(S11,plots,"max" )
names(Data) <- c('S11_max')
Data_DF <- dplyr::mutate(Data_DF, "S11_max" = Data)
