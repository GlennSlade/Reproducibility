# Draft Workflow plot CHM metrics Site wide

library(terra)
library(gstat)
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
#-----1. For each plot - once---------
# Read in las files for GNSS ground points of plot corners - Done once
Plots_las = readLAS("/raid/home/gs558/share/Reproducibility/Processed/LAS/plot_corners/all_points_corner_test.laz")
PlotP45_las = readLAS("/raid/home/gs558/share/Reproducibility/Plot/P45.laz")
Test = readLAS("/raid/home/gs558/share/Reproducibility/Processed/LAS/plot_corners/Test.laz")
All = readLAS("/raid/home/gs558/share/Reproducibility/Processed/LAS/plot_corners/all_GNSS.laz")


#DTM interpolation of ground surface for P45 using IDW - Repeated for all plots
# las2 <- classify_ground(All, algorithm = pmf(ws = 5, th = 3))# need to classify points as ground
# plot(las2, color = "Classification", size = 3, bg = "white")
# tic()
# dtm_idw <- rasterize_terrain(las2, res = .01, algorithm = knnidw(k = 100, p = 2, rmax = 1000)) # inverse distance weighting
# plot(dtm_idw, bg = "black") 
# writeRaster(dtm_idw, "/raid/home/gs558/share/Reproducibility/Processed/DTM_interpolated/IDW/DTM_site_IDW.tif", overwrite=TRUE )
# toc()

dtm_idw <- rast("/raid/home/gs558/share/Reproducibility/Processed/DTM_interpolated/IDW/DTM_site_IDW.tif")

#------2. For each plot and each survey---------

#Read in LAZ file for survey and clip to plot

tic()
S1_las = readLAS("/raid/home/gs558/share/Reproducibility/Processed/LAZ/S1_dpc_export.laz")# heres one i have already prepared
toc()

# Calculate the normalisedLaz - which is the CHM but in point form
tic()
nlas <- S1_las  - dtm_idw
toc()

# Point to raster Canopy height takes the max Z value in each raster pixel (defined here as 0.01m)
tic()
chm <- rasterize_canopy(nlas, res = .01, algorithm = p2r())
col <- height.colors(25)
#plot(chm, col = col)
toc()

# Use gstat to interpolate any holes in the raster chm using IDW


tic()
gs <- gstat(formula=Z~1, locations=~x+y, data=as.data.frame(chm, xy=TRUE))
idw <- interpolate(chm, gs, debug.level=0)[[1]]
toc()

writeRaster(idw,"/raid/home/gs558/share/Reproducibility/Processed/CHM_interpolated/S1_gstat_idw.tif", overwrite = TRUE )


#use terra::interpIDW to intepret holes in raster CHM NB radius is crucial !
x <- chm
tic()
idw2 <- terra::interpIDW(x, as.points(x), field="Z", radius=0.1, power=2, smooth=0)
toc()

writeRaster(idw,"/raid/home/gs558/share/Reproducibility/Processed/CHM_interpolated/S1_terra_idw.tif", overwrite = TRUE )

