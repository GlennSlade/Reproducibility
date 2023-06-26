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

DTM_site <- rast ("/raid/home/gs558/share/Reproducibility/Processed/DTM_interpolated/IDW/DTM_site_IDW.tif")
crs(DTM_site)  <- "epsg:7405"

ortho_site <- rast ("/raid/home/gs558/share/Reproducibility/Processed/ortho/S6_7405.tif")

DTM_site_w <- extend(DTM_site,ortho_site)
#smallextend = extend(small, big)

#p <-as.points(DTM_site) |>sample()

idw <- terra::interpIDW(DTM_site_w, as.points(DTM_site_w), field="Z", radius=0.40, power=2, smooth=0, near=T)

#idw <- terra::interpIDW(chm, as.points(chm), field="Z", radius=0.1, power=2, smooth=0)
writeRaster(idw,paste0 ("/raid/home/gs558/share/Reproducibility/Processed/DTM_interpolated/IDW/DTM_site_IDW_w.tif"), overwrite = TRUE )

