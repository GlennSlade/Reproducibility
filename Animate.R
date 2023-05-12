### Script to animate clip of survey plots

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

# filenames <- list.files("E:/Glenn/Reproducibility/Processed/Clip", pattern="*.tif", full.names=TRUE)
# 
# clip <- read_sf(dsn = "E:/Glenn/Reproducibility/Processed/Clip", layer = "Clip")
# clip <- vect(clip)
# 
# 
# library(raster)
# img <- list.files("E:/Glenn/Reproducibility/Processed/Clip", pattern="*.tif", full.names=TRUE)
# 
# 
# 
# stack <- stack(img)


#Ortho read in the 60 surveys
S1c <- rast("E:/Glenn/Reproducibility/Processed/S1_7405.tif")
S2c <- rast("E:/Glenn/Reproducibility/Processed/S2_7405.tif")
S3c <- rast("E:/Glenn/Reproducibility/Processed/S3_7405.tif")
S4c <- rast("E:/Glenn/Reproducibility/Processed/S4_7405.tif")
S5c <- rast("E:/Glenn/Reproducibility/Processed/S5_7405.tif")
S6c <- rast("E:/Glenn/Reproducibility/Processed/S6_7405.tif")
#S5 <-terra::project(S5, y="EPSG:7405")

clip <- read_sf(dsn = "E:/Glenn/Reproducibility/Processed/Clip", layer = "Clip")
clip <- vect(clip)

S1c<- crop(S1c,clip)
S2c<- crop(S2c,clip)
S3c<- crop(S3c,clip)
S4c<- crop(S4c,clip)
S5c<- crop(S5c,clip)

writeRaster(S1c, filename = "E:/Glenn/Reproducibility/Processed/Clip/S1c.tif", overwrite=TRUE)
writeRaster(S2c, filename = "E:/Glenn/Reproducibility/Processed/Clip/S2c.tif", overwrite=TRUE)
writeRaster(S3c, filename = "E:/Glenn/Reproducibility/Processed/Clip/S3c.tif", overwrite=TRUE)
writeRaster(S4c, filename = "E:/Glenn/Reproducibility/Processed/Clip/S4c.tif", overwrite=TRUE)
writeRaster(S5c, filename = "E:/Glenn/Reproducibility/Processed/Clip/S5c.tif", overwrite=TRUE)
writeRaster(S6c, filename = "E:/Glenn/Reproducibility/Processed/Clip/S6c.tif", overwrite=TRUE)
