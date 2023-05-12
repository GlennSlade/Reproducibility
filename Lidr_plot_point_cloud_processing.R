# Lidr processing of point cloud

library(lidR)
library(sp)
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
library(readxl)
library(tictoc)

# load in sample area
#P1_clip <- vect("E:/Glenn/Reproducibility/Plot/P1.shp")

P1_sf <- read_sf(dsn = "E:/Glenn/Reproducibility/Plot", layer = "P1")
P45_sf <- read_sf(dsn = "E:/Glenn/Reproducibility/Plot", layer = "P45")

plots_sf <- read_sf(dsn = "E:/Glenn/Reproducibility/Plot", layer = "plot_polygons")


#P1_clip = readOGR("data/lidarArea.gpkg")


# seems to take a while....
S1_laz= readLAS("E:/Glenn/Reproducibility/Processed/LAS/S1.laz")
S1_las= readLAS("E:/Glenn/Reproducibility/Processed/LAS/S1.las")


# crop out the sample area
S1_P1 = clip_roi(S1_las, P1_sf)
S1_P45 = clip_roi(S1_las, P45_sf)
plot(S1_P1)
plot(S1_P45)


#write P1 las file

writeLAS(S1_P1, "E:/Glenn/Reproducibility/Processed/LAS/Plots/S1_P1.las")
writeLAS(S1_P45, "E:/Glenn/Reproducibility/Processed/LAS/Plots/S1_P45.las")



#Experminenting with polygon_metrics 
tic()
m <- plot_metrics(S1_las, .stdmetrics_z, plots_sf)
toc()

# clean up the RAM
#rm(S1_las) # removes a object from R
#gc() # "garbage collector" -> cleans up the RAM