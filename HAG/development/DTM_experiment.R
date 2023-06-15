# Lidr processing of point cloud - DTM Generation and Heigth above ground calculation

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
library(gstat)

# load in sample area
#P1_clip <- vect("E:/Glenn/Reproducibility/Plot/P1.shp")

P1_sf <- read_sf(dsn = "E:/Glenn/Reproducibility/Plot", layer = "P1")
P45_sf <- read_sf(dsn = "E:/Glenn/Reproducibility/Plot", layer = "P45")

#P1_clip = readOGR("data/lidarArea.gpkg")


#Read P1 and P45 las files from survey 1
#
S1_P1_las = readLAS( "E:/Glenn/Reproducibility/Processed/LAS/Plots/S1_P1.las")
S1_P45_las = readLAS("E:/Glenn/Reproducibility/Processed/LAS/Plots/S1_P45.las")

plot(S1_P45_las)


# Read in las files for GNSS ground points of plot corners
Plots_las = readLAS("E:/Glenn/Reproducibility/Processed/LAS/plot_corners/all_points_corner_test.laz")
PlotP45_las = readLAS("E:/Glenn/Reproducibility/Plot/P45.laz")
Test = readLAS("E:/Glenn/Reproducibility/Processed/LAS/plot_corners/Test.laz")
All = readLAS("E:/Glenn/Reproducibility/Processed/LAS/plot_corners/All_GNSS.laz")


plot(Plots_las)
plot(PlotP45_las)

#DTM interpolation of ground surface for All plots
las <- classify_ground(Plots_las, algorithm = pmf(ws = 5, th = 3)) # need to classify points as ground
plot(las, color = "Classification", size = 3, bg = "white") 

dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())# at 1m resolution
plot_dtm3d(dtm_tin, bg = "white") 

#DTM interpolation of ground surface for P45 using different methods
las2 <- classify_ground(PlotP45_las, algorithm = pmf(ws = 5, th = 3))# need to classify points as ground
plot(las2, color = "Classification", size = 3, bg = "white") 


dtm_tin2 <- rasterize_terrain(las2, res = .01, algorithm = tin()) #TIN method at 1cm resolution
plot(dtm_tin2, bg = "white")

dtm_idw <- rasterize_terrain(las2, res = .01, algorithm = knnidw(k = 100, p = 2, rmax = 1000)) # inverse distance weighting
plot(dtm_idw, bg = "black") 
writeRaster(dtm_idw, "E:/Glenn/Reproducibility/Plot/P45_IDW3.tif", overwrite=TRUE )


dtm_kriging <- rasterize_terrain(las2,res = .01, algorithm = kriging(k = 100)) # Kriging
plot(dtm_kriging, bg = "grey") 

writeRaster(dtm_tin2, "E:/Glenn/Reproducibility/Plot/P45_DTM.tif" )
writeRaster(dtm_idw, "E:/Glenn/Reproducibility/Plot/P45_IDW.tif",overwrite = TRUE )
writeRaster(dtm_kriging, "E:/Glenn/Reproducibility/Plot/P45dtm_kriging.tif" , overwrite = TRUE)

All_las <- classify_ground(All, algorithm = pmf(ws = 5, th = 3)) # need to classify points as ground
plot(All_las, color = "Classification", size = 3, bg = "white") 

All_tin <- rasterize_terrain(All_las, res = .01, algorithm = tin())# at 1cm resolution
plot_dtm3d(All_tin, bg = "white") 
writeRaster(All_tin, "E:/Glenn/Reproducibility/Plot/All_dtm_1cm.tif" )


## Height above ground HAG - in Lidr - normalised height

nlas_idw <- S1_P45_las  - dtm_idw
plot(nlas_idw, size = 1, bg = "black")

nlas <- S1_P45_las  - dtm_tin2
plot(nlas, size = 1, bg = "white")

nlas_k <- S1_P45_las  - dtm_kriging
plot(nlas_k, size = 1, bg = "grey")

nlas_a <- S1_P45_las  - All_tin
plot(nlas_k, size = 1, bg = "grey")

Test = rast("E:/Glenn/Reproducibility/Plot/All_dtm_1cm.tif" )

nlas_a <- S1_P45_las  - Test
plot(nlas_a, size = 1, bg = "grey")

writeLAS(nlas, "E:/Glenn/Reproducibility/Processed/LAS/Plots/S1_P45_HAG.las")



#Experminenting with polygon_metrics 
tic()
m <- plot_metrics(S1_las, .stdmetrics_z, plots_sf)
toc()

# clean up the RAM
#rm(S1_las) # removes a object from R
#gc() # "garbage collector" -> cleans up the RAM