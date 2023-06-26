# Draft CHM Metrics for Plots for each survey.
#This workflow is based on using point cloud data from a buffered plot size to inform the interpolation(IDW) of empty 
# raster cells that may be on the edge of the plot.

# Script results in a data frame with mean canopy height for each plot and each survey with data taken from the initial CHM and from the interpolated CHM.
# Additional data is also extracted for empty cell coverage in the CHM and interpolated(IDW) CHM

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

#df <- data.frame(survey = integer(),plot = integer(),Md_chm = double(),Mn_chm = double(),Na_chm = double(),Max_chm = double(),Sd_chm = double(),Ct_chm = double(), Md_idw = double(), Max_idw = double(), Sd_idw = double(), Mn_idw = double(),Na_idw = double(), Na_dtm = double(), Ct_idw = double(),Ct_dtm = double())


#Read in Survey 

for (x in 2:3) {

S_laz = readLAS(paste0("/raid/home/gs558/share/Reproducibility/Processed//LAZ/S",x,"_dpc_export.laz"))

# or read in df if starting from later survey

v <- x-1
df <- read_xlsx(paste0("/raid/home/gs558/share/Reproducibility/Processed/plot_chm_metrics_temp",v,".xlsx"))


for (i in 1:64) {
  
  #Printing plot and survey number to monitor progress in program
  
  print (paste0("processing plot number ",i," of survey number ",x))
  
  #Clip DPC Laz file for each plot and save as individual laz file
  
  #Import plot polygon
  P_sf <- read_sf(dsn = "/raid/home/gs558/share/Reproducibility/Plot/Plot_Polygon_Shape", layer = paste0("Plot_P", i))
  #Buffer plot polygon by 30cm - so that any point cloud info within 30cm of the plot can influence the interpolation for empty cells
  P_buf <-st_buffer(P_sf, .3)
  #Clip laz file to plot
  Laz_p = clip_roi(S_laz, P_sf)
  #save plot laz file incase we need in the future
  #writeLAS(Laz_p, paste0 ("/raid/home/gs558/share/Reproducibility/Processed/LAZ/Plots/S",x,"_P",i,".laz"))
  
  #clip laz file to buffered plot
  Laz_buf <- clip_roi(S_laz, P_buf)
  
  #Import DTM for each buffered plot - already prepared in separate script (DTM_plots_w.R)    
       
  dtm_buf <- rast (paste0 ("/raid/home/gs558/share/Reproducibility/Plot/DTM/Pbuf",i,"_DTM.tif"))
  

  #Import DTM for each plot - already prepared in separate script (DTM_plots.R) 
  
  dtm <- rast (paste0 ("/raid/home/gs558/share/Reproducibility/Plot/DTM/P",i,"_DTM.tif"))
  crs(dtm)  <- "epsg:7405"
  
  #Calculate CHM - normalised Laz (buffered plot)

  nlas_buf <- Laz_buf - dtm_buf
            
  # Point to raster Canopy height takes the max Z value in each raster pixel (defined here as 0.01m) for buffered plot
  
  chm <- rasterize_canopy(nlas_buf, res = .01, algorithm = p2r())
  
 # chm <- rasterize_canopy(Laz_buf, res = .01, algorithm = p2r())
  
  #set any reconstruced heights which are below zero to zero
  chm[chm < 0] <- 0
  
  #plot(chm)
  # Crop the CHM to the plot polygon and Save the plot level CHM raster data (no hole filling) incase needed in future
  
  aoi_terra <- vect(P_sf)
  chm_crop <- crop(chm, aoi_terra)
  chm_mask <- mask(chm_crop, aoi_terra)
  
  writeRaster(chm_mask,paste0 ("/raid/home/gs558/share/Reproducibility/Processed/CHM_raw/S",x,"_P",i,"_chm.tif"), overwrite = TRUE )
  
  # Use terra::interpIDW to fill in holes in the point to raster CHM for buffered plot
 
  idw <- terra::interpIDW(chm, as.points(chm), field="Z", radius=0.25, power=2, smooth=0, near=T)
  #plot(idw)
  # Crop the interpolated data to the plot polygon
  ras_crop <- crop(idw, aoi_terra)
  idw_mask <- mask(ras_crop, aoi_terra)
  
  # Save the plot level interpreted CHM raster (with idw hole filling) in case needed in future
  writeRaster(idw_mask,paste0 ("/raid/home/gs558/share/Reproducibility/Processed/CHM_interpolated/S",x,"_P",i,"_chm_interpolated.tif"), overwrite = TRUE )
  
  # Get plot level summary statistics from the CHM - and get mean CH (Mn_chm) and number of empty cells (Na_chm)

  
  # exact extract statistics - ignores Na values 
  Max_chm <- exact_extract(chm_mask,P_sf, fun = "max")
  Md_chm <- exact_extract(chm_mask,P_sf, fun = "median")
  Mn_chm <- exact_extract(chm_mask,P_sf, fun = "mean")
  Sd_chm <- exact_extract(chm_mask,P_sf, fun = "stdev")
  Ct_chm <- exact_extract(chm_mask,P_sf, fun = "count")
  
  
  print (paste0("mean point cloud canopy height ",Mn_chm))

  # Get plot level summary statistics from the interpolated CHM - and get mean CH (Mn_idw) and number of empty cells (Na_idw)

  Max_idw <- exact_extract(idw_mask,P_sf, fun = "max")
  Mn_idw <- exact_extract(idw_mask,P_sf, fun = "mean")
  Sd_idw <- exact_extract(idw_mask,P_sf, fun = "stdev")
  Md_idw <- exact_extract(idw_mask,P_sf, fun = "median")
  Ct_idw <- exact_extract(idw_mask,P_sf, fun = "count")
  
  print (paste0("mean interpolated cloud canopy height ",Mn_idw))
  
  #Get plot level summary statistics for the plot DTM - get filled cell count - NB we are only doing this to work out how many empty cells
  #there are in chm and idw raster images (DTM has full raster coverage)
  
  Ct_dtm <- exact_extract(dtm,P_sf, fun = "count")
  
    # make data frame row for plot level statistics
  df_p<-data.frame (survey = c(x),  plot = c(i),Md_chm = c(Md_chm),Mn_chm = c(Mn_chm),Max_chm = c(Max_chm),Sd_chm = c(Sd_chm),Ct_chm = c(Ct_chm),Md_idw = c(Md_idw),Mn_idw = c(Mn_idw),Sd_idw = c(Sd_idw),Ct_idw = c(Ct_idw),Ct_dtm = c(Ct_dtm))
  #add the plot data row to the master df for all plots and all surveys
  df <- rbind(df,df_p)

 
  next
}

# save the data frame once all 64 plots have been processed as xcel spreadsheet in case we want to start processing from a particular survey number
write_xlsx(df,paste0("/raid/home/gs558/share/Reproducibility/Processed/plot_chm_metrics_temp",x,".xlsx"))  

  next
}


# save the master data frame with all survey and plot data

#write_xlsx(df,"/raid/home/gs558/share/Reproducibility/Processed/plot_chm_metrics_master.xlsx")

toc()

# plot(chm)
# plot(idw)
# plot(chm_mask)
# plot(idw_mask)
