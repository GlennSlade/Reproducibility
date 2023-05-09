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


#write_xlsx(Data_DF, path = "E:/Glenn/Reproducibility/test.xlsx" )

# Extract Data

Data <- exact_extract(S1,plots,"mean")
names(Data) <- c('S1')
Data_DF <-bind_cols(plots,Data)

Data <- exact_extract(S1,plots,"mean" )
names(Data) <- c('S1')
Data_DF <- dplyr::mutate(Data_DF, "S1" = Data)

Data <- exact_extract(S2,plots,"mean" )
names(Data) <- c('S2')
Data_DF <- dplyr::mutate(Data_DF, "S2" = Data)


Data <- exact_extract(S3,plots,"mean" )
names(Data) <- c('S3')
Data_DF <- dplyr::mutate(Data_DF, "S3" = Data)


Data <- exact_extract(S4,plots,"mean" )
names(Data) <- c('S4')
Data_DF <- dplyr::mutate(Data_DF, "S4" = Data)


Data <- exact_extract(S5,plots,"mean" )
names(Data) <- c('S5')
Data_DF <- dplyr::mutate(Data_DF, "S5" = Data)

Data_DF <-Data_DF%>%dplyr:: select (-c(3))



Data <- exact_extract(S6,plots,"mean" )
names(Data) <- c('S6')
Data_DF <- dplyr::mutate(Data_DF, "S6" = Data)

Data <- exact_extract(S7,plots,"mean" )
names(Data) <- c('S7')
Data_DF <- dplyr::mutate(Data_DF, "S7" = Data)

Data <- exact_extract(S8,plots,"mean" )
names(Data) <- c('S8')
Data_DF <- dplyr::mutate(Data_DF, "S8" = Data)

Data <- exact_extract(S9,plots,"mean" )
names(Data) <- c('S9')
Data_DF <- dplyr::mutate(Data_DF, "S9" = Data)


Data <- exact_extract(S10,plots,"mean" )
names(Data) <- c('S10')
Data_DF <- dplyr::mutate(Data_DF, "S10" = Data)

Data <- exact_extract(S11,plots,"mean" )
names(Data) <- c('S11')
Data_DF <- dplyr::mutate(Data_DF, "S11" = Data)


