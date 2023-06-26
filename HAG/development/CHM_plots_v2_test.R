# Draft CHM for Plots for each survey - produced raster to points CHM for each plot for each survey
#This workflow is based on using point cloud data from a buffer outside the plot to inform the interpolation of empty 
# raster cells that may be on the edge of the plot.

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



# Create empty data frame to input plot metrics
tic()

df <- data.frame(survey = integer(),plot = integer(),Mn_chm = double(),Na_chm = double(), Mn_idw = double(),Na_idw = double())


#Read in Survey 

for (x in 1:5) {

S_laz = readLAS(paste0("/raid/home/gs558/share/Reproducibility/Processed//LAZ/S",x,"_dpc_export.laz"))

#v <- x-1
#df <- read_xlsx(paste0("/raid/home/gs558/share/Reproducibility/Processed/plot_chm_metrics_temp",v,".xlsx"))


for (i in 1:64) {
  
  
  #Clip DPC Laz file for each plot and save as individual laz file
  
  #Import plot polygon
  P_sf <- read_sf(dsn = "/raid/home/gs558/share/Reproducibility/Plot/Plot_Polygon_Shape", layer = paste0("Plot_P", i))
  #Buffer plot polygon
  P_buf <-st_buffer(P_sf, .2)
  #Clip laz file to plot
  Laz_p = clip_roi(S_laz, P_sf)
  #save laz file incase we need in the future
  writeLAS(Laz_p, paste0 ("/raid/home/gs558/share/Reproducibility/Processed/LAZ/Plots/S",x,"_P",i,".laz"))
  
  #clip laz file to buffered plot
  Laz_buf <- clip_roi(S_laz, P_buf)
  
  #Import DTM for each buffered plot - already prepared in separate script (DTM_plots_w.R)    
       
  dtm_buf <- rast (paste0 ("/raid/home/gs558/share/Reproducibility/Plot/DTM/Pbuf",i,"_DTM.tif"))
  
  #Calculate CHM - normalised Laz (buffered plot)

  nlas_buf <- Laz_buf - dtm_buf
            
  # Point to raster Canopy height takes the max Z value in each raster pixel (defined here as 0.01m) for buffered plot
  
  chm <- rasterize_canopy(nlas_buf, res = .01, algorithm = p2r())
  
    #plot(chm)
  # Crop the CHM to the plot polygon and Save the plot level CHM raster data (no hole filling) incase needed in future
  
  aoi_terra <- vect(P_sf)
  chm_crop <- crop(chm, aoi_terra)
  chm_mask <- mask(chm_crop, aoi_terra)
  
  writeRaster(chm_mask,paste0 ("/raid/home/gs558/share/Reproducibility/Processed/CHM_raw/S",x,"_P",i,"_chm.tif"), overwrite = TRUE )
  
  # Use terra::interpIDW to fill in holes in the point to raster CHM for buffered plot
  p <-as.points(chm) |>sample()
  
  idw <- terra::interpIDW(chm, p, field="Z", radius=0.1, power=2, smooth=0, near=T)
  #plot(idw)
  # Crop the interpolated data to the plot polygon
  ras_crop <- crop(idw, aoi_terra)
  idw_mask <- mask(ras_crop, aoi_terra)
  
  # Save the plot level interpreted CHM raster (with idw hole filling) in case needed in future
  writeRaster(idw_mask,paste0 ("/raid/home/gs558/share/Reproducibility/Processed/CHM_interpolated/S",x,"_P",i,"_chm_interpolated.tif"), overwrite = TRUE )
  
  # Get plot level summary statistics from the CHM
  c <- summary(chm_mask)
  chm_df <-as.data.frame(c)
  
  Na<- chm_df [7,3]
  Na_chm <- parse_number(Na)
  Mn <-chm_df [4,3]
  Mn_chm <- parse_number(Mn)
  
  # Get plot level summary statistics from the interpolated CHM
  r <- summary(idw_mask)
  
  
  idw_df <-as.data.frame(r)
  
  Nai<-idw_df [7,3]
  Na_idw <- parse_number(Nai)
  Mni<-idw_df [4,3]
  Mn_idw <- parse_number(Mni)
  
  # make data frame for plot level statistics
  df_p<-data.frame (survey = c(x),  plot = c(i),Mn_chm = c(Mn_chm), Na_chm = c(Na_chm), Mn_idw= c(Mn_idw), Na_idw = c(Na_idw))
  #add the plot data row to the master df for all plots and all surveys
  df <- rbind(df,df_p)
  
  write_xlsx(df,paste0("/raid/home/gs558/share/Reproducibility/Processed/plot_chm_metrics_temp",x,".xlsx"))  

  next
}
  next
}



# save the master data frame with all survey and plot data

#write_xlsx(df,"/raid/home/gs558/share/Reproducibility/Processed/plot_chm_metrics_master.xlsx")

toc()



