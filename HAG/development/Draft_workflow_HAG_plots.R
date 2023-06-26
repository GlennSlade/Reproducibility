# Draft Workflow plot CHM metrics

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
#Test = readLAS("/raid/home/gs558/share/Reproducibility/Processed/LAS/plot_corners/Test.laz")
All = readLAS("/raid/home/gs558/share/Reproducibility/Processed/LAS/plot_corners/all_GNSS.laz")


#DTM interpolation of ground surface for P45 using IDW - Repeated for all plots
las2 <- classify_ground(PlotP45_las, algorithm = pmf(ws = 5, th = 3))# need to classify points as ground
#plot(las2, color = "Classification", size = 3, bg = "white")

dtm_idw <- rasterize_terrain(las2, res = .01, algorithm = knnidw(k = 100, p = 2, rmax = 1000)) # inverse distance weighting
plot(dtm_idw, bg = "black") 
#writeRaster(dtm_idw, "/raid/home/gs558/share/Reproducibility/Plot/P45_IDW3.tif", overwrite=TRUE )


#------2. For each plot and each survey---------

#Read in LAZ file for survey and clip to plot

#S1_laz= readLAS("/raid/home/gs558/share/Reproducibility/Processed/LAZ/S1_dpc_export.laz")
#S1_P45 = clip_roi(S1_laz, P45_sf)

S1_P45_las = readLAS("/raid/home/gs558/share/Reproducibility/Processed/LAS/Plots/S1_P45a.laz")# heres on i have already prepared


# Calculate the normalisedLaz - which is the CHM but in point form

nlas <- S1_P45_las  - dtm_idw
plot(nlas, size = 1, bg = "grey")

# Point to raster Canopy height takes the max Z value in each raster pixel (defined here as 0.01m)

chm <- rasterize_canopy(nlas, res = .01, algorithm = p2r())
col <- height.colors(25)
plot(chm, col = col)


# Use gstat to interpolate any holes in the raster chm using IDW

pfunc <- function(.chm, .idw) {
  par(mfrow = c(1, 2))
  plot(.chm)
  plot(.idw)
  par(mfrow = c(1, 1))
}

gs <- gstat(formula=Z~1, locations=~x+y, data=as.data.frame(chm, xy=TRUE))
idw <- interpolate(chm, gs, debug.level=0)[[1]]
pfunc(chm, idw)

#writeRaster(idw,"/raid/home/gs558/share/Reproducibility/Plot/S1_P45_gstat_idw.tif", overwrite = TRUE )

# using terra:: interpIDW NB radius is very influential - as the radius increases the value in the filled areas changes a  0.1 radius
# give same result as the gstat method - larger radius tends to increase the height used to fill the holes.

library(terra)

x <- chm

idw2 <- terra::interpIDW(x, as.points(x), field="Z", radius=0.1, power=1, smooth=0)

#pfunc(x, idw2)
plot(idw2)
# looking at the difference 

Diff <- idw2 - chm
plot(Diff)

Diff2 <- idw2 - idw
plot(Diff2)
