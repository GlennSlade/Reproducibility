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


# Read in laz file for 4 corner points

DTM = rast("/raid/home/gs558/share/Reproducibility/Processed/DTM_interpolated/IDW/DTM_site_IDW_w.tif")


for (i in 1:64) {

# Read in laz file for 4 corner points


#Clip sitewide DTM for each plot

P_sf <- read_sf(dsn = "/raid/home/gs558/share/Reproducibility/Plot/Plot_Polygon_Shape", layer = paste0("Plot_P", i))
P_buf <-st_buffer(P_sf, .4)
aoi_terra <- vect(P_buf)
aoi_terra
ras_crop <- crop(DTM, aoi_terra)
#plot (ras_crop)
ras_mask <- mask(ras_crop, aoi_terra)
#plot(ras_mask)
writeRaster(ras_mask,paste0 ("/raid/home/gs558/share/Reproducibility/Plot/DTM/Pbuf",i,"_DTM.tif"), overwrite = TRUE )
next
}


