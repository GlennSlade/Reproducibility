# Draft CHM for Plots for each survey - produced raster to points CHM for each plot for each survey

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



#Read in Survey 

for (x in 1:2) {

S_laz = readLAS(paste0("/raid/home/gs558/share/Reproducibility/Processed//LAZ/S",x,"_dpc_export.laz"))


for (i in 1:64) {
  
  
  #Clip DPC Laz file for each plot and save as individual laz file
  
  P_sf <- read_sf(dsn = "/raid/home/gs558/share/Reproducibility/Plot/Plot_Polygon_Shape", layer = paste0("Plot_P", i))
  Laz_p = clip_roi(S_laz, P_sf)
  writeLAS(Laz_p, paste0 ("/raid/home/gs558/share/Reproducibility/Processed/LAZ/Plots/S",x,"_P",i,".laz"))
  
  #Import DTM for each plot         
           
  dtm <- rast (paste0 ("/raid/home/gs558/share/Reproducibility/Plot/DTM/P",i,"_DTM.tif"))
  
  #Calculate CHM
         
  nlas <- Laz_p  - dtm
            
  # Point to raster Canopy height takes the max Z value in each raster pixel (defined here as 0.01m)
  
  chm <- rasterize_canopy(nlas, res = .01, algorithm = p2r())
  
  # Save the plot level CHM raster raw data (no hole filling)  
  writeRaster(chm,paste0 ("/raid/home/gs558/share/Reproducibility/Processed/CHM_raw/S",x,"_P",i,"_chm.tif"), overwrite = TRUE )
  
  # Use terra::interpIDW to fill in holes in the point to raster CHM
  idw <- terra::interpIDW(chm, as.points(chm), field="Z", radius=0.1, power=2, smooth=0)
  
  aoi_terra <- vect(P_sf)
  aoi_terra
  ras_crop <- crop(idw, aoi_terra)
  #plot (ras_crop)
  ras_mask <- mask(ras_crop, aoi_terra)
  
  # Save the plot level interpreted CHM raster (with hole filling)  
  writeRaster(ras_mask,paste0 ("/raid/home/gs558/share/Reproducibility/Processed/CHM_interpolated/S",x,"_P",i,"_chm_interpolated.tif"), overwrite = TRUE )
  
  
           
  next
}
  next
}



# Testing different settings


for (i in 45:45) {
  
  P_sf <- read_sf(dsn = "/raid/home/gs558/share/Reproducibility/Plot/Plot_Polygon_Shape", layer = paste0("Plot_P", i))
  Laz_p = clip_roi(S_laz, P_sf)
  #writeLAS(Laz_p, paste0 ("/raid/home/gs558/share/Reproducibility/Processed/LAZ/Plots/S",x,"_P",i,".laz"))
  
  #Import DTM for each plot         
  
  dtm <- rast (paste0 ("/raid/home/gs558/share/Reproducibility/Plot/DTM/P",i,"_DTM.tif"))
  
  #Calculate CHM
  
  nlas <- Laz_p  - dtm
  
  # Point to raster Canopy height takes the max Z value in each raster pixel (defined here as 0.01m)
  
  chm <- rasterize_canopy(nlas, res = .01, algorithm = p2r())
  
  # Save the plot level CHM raster raw data (no hole filling)  
  #writeRaster(chm,paste0 ("/raid/home/gs558/share/Reproducibility/Processed/CHM_raw/S",x,"_P",i,"_chm.tif"), overwrite = TRUE )
  
  # Use terra::interpIDW to fill in holes in the point to raster CHM
  idw <- terra::interpIDW(chm, as.points(chm), field="Z", radius=0.1, power=2, smooth=0)
  idw2 <- terra::interpIDW(chm, as.points(chm), field="Z", radius=0.1, power=1, smooth=0)
  idw3 <- terra::interpIDW(chm, as.points(chm), field="Z", radius=0.03, power=2, smooth=0)
  idw4 <- terra::interpIDW(chm, as.points(chm), field="Z", radius=0.07, power=2, smooth=0)
  idw5 <- terra::interpIDW(chm, as.points(chm), field="Z", radius=0.08, power=1, smooth=0)
  
  
  
}

plot(chm)
plot (idw)

diff <- chm - idw
plot(diff)

diff2 <- idw2 - idw
plot(diff2)

diff3 <- idw3 - idw
plot(diff3)

diff4 <- idw4 - idw
plot(diff4)

diff5 <- idw5 - idw
plot(diff5)


