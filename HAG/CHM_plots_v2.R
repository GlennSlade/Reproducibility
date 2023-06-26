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

df <- data.frame(survey = integer(),plot = integer(),Md_chm = double(),Mn_chm = double(),Max_chm = double(),Na_chm = double(),Md_idw = double(), Mn_idw = double(),Max_idw = double(),Na_idw = double(), Na_dtm = double())


#Read in Survey 

for (x in 1:1) {

S_laz = readLAS(paste0("/raid/home/gs558/share/Reproducibility/Processed//LAZ/S",x,"_dpc_export.laz"))

# or read in df if starting from later survey

#v <- x-1
#df <- read_xlsx(paste0("/raid/home/gs558/share/Reproducibility/Processed/plot_chm_metrics_temp",v,".xlsx"))


for (i in 1:64) {
  
  #Printing plot and survey number to monitor progress in program
  
  print (paste0("processing plot number",i,"of survey number",x))
  
  #Clip DPC Laz file for each plot and save as individual laz file
  
  #Import plot polygon
  P_sf <- read_sf(dsn = "/raid/home/gs558/share/Reproducibility/Plot/Plot_Polygon_Shape", layer = paste0("Plot_P", i))
  #Buffer plot polygon by 30cm - so that any point cloud info within 30cm of the plot can influence the interpolation for empty cells
  P_buf <-st_buffer(P_sf, .3)
  #Clip laz file to plot
  Laz_p = clip_roi(S_laz, P_sf)
  #save plot laz file incase we need in the future
  writeLAS(Laz_p, paste0 ("/raid/home/gs558/share/Reproducibility/Processed/LAZ/Plots/S",x,"_P",i,".laz"))
  
  #clip laz file to buffered plot
  Laz_buf <- clip_roi(S_laz, P_buf)
  
  #Import DTM for each buffered plot - already prepared in separate script (DTM_plots_w.R)    
       
  dtm_buf <- rast (paste0 ("/raid/home/gs558/share/Reproducibility/Plot/DTM/Pbuf",i,"_DTM.tif"))
  
  #Import DTM for each plot - already prepared in separate script (DTM_plots.R) 
  
  dtm <- rast (paste0 ("/raid/home/gs558/share/Reproducibility/Plot/DTM/P",i,"_DTM.tif"))
  
  
  #Calculate CHM - normalised Laz (buffered plot)

  nlas_buf <- Laz_buf - dtm_buf
            
  # Point to raster Canopy height takes the max Z value in each raster pixel (defined here as 0.01m) for buffered plot
  
  chm <- rasterize_canopy(nlas_buf, res = .01, algorithm = p2r())
  
  #set any reconstruced heights which are below zero to zero
  chm[chm < 0] <- 0
  
  #plot(chm)
  # Crop the CHM to the plot polygon and Save the plot level CHM raster data (no hole filling) incase needed in future
  
  aoi_terra <- vect(P_sf)
  chm_crop <- crop(chm, aoi_terra)
  chm_mask <- mask(chm_crop, aoi_terra)
  
  writeRaster(chm_mask,paste0 ("/raid/home/gs558/share/Reproducibility/Processed/CHM_raw/S",x,"_P",i,"_chm.tif"), overwrite = TRUE )
  
  # Use terra::interpIDW to fill in holes in the point to raster CHM for buffered plot
  p <-as.points(chm) |>sample()
  
  idw <- terra::interpIDW(chm, p, field="Z", radius=0.25, power=2, smooth=0, near=T)
  #plot(idw)
  # Crop the interpolated data to the plot polygon
  ras_crop <- crop(idw, aoi_terra)
  idw_mask <- mask(ras_crop, aoi_terra)
  
  # Save the plot level interpreted CHM raster (with idw hole filling) in case needed in future
  writeRaster(idw_mask,paste0 ("/raid/home/gs558/share/Reproducibility/Processed/CHM_interpolated/S",x,"_P",i,"_chm_interpolated.tif"), overwrite = TRUE )
  
  # Get plot level summary statistics from the CHM - and get mean CH (Mn_chm) and number of empty cells (Na_chm)
  # Summmary produces a matrix (hence as.data.frame) with character definitions next to the number values (hence parse_number)
  c <- summary(chm_mask)
  chm_df <-as.data.frame(c)
  Md <-chm_df [3,3]
  Mn <-chm_df [4,3]
  Max <-chm_df [6,3]
  Na<- chm_df [7,3]
  Md_chm <- parse_number(Md)
  Mn_chm <- parse_number(Mn)
  Max_chm <- parse_number(Max)
  Na_chm <- parse_number(Na)
  
  print (paste0("mean point cloud canopy height",Mn_chm))

  # Get plot level summary statistics from the interpolated CHM - and get mean CH (Mn_idw) and number of empty cells (Na_idw)
  r <- summary(idw_mask)
  idw_df <-as.data.frame(r)
  
  Md <-idw_df [3,3]
  Mn <-idw_df [4,3]
  Max <-idw_df [6,3]
  Na<- idw_df [7,3]
  Md_idw <- parse_number(Md)
  Mn_idw <- parse_number(Mn)
  Max_idw <- parse_number(Max)
  Na_idw <- parse_number(Na)
  
  print (paste0("mean interpolated cloud canopy height",Mn_idw))
  
  #Get plot level summary statistics for the plot DTM - and get empty cells (Na_dtm) - NB we are only doing this to get the "NA" statistics to work out how many empty
  #cells there are around the raster image - we can then later use this to calculate how many empty cells there are in chm and idw raster images
  
  q <- summary(dtm)
  dtm_df <-as.data.frame(q)
  Nai<-dtm_df [7,3]
  Na_dtm <- parse_number(Nai)

  # make data frame row for plot level statistics
  df_p<-data.frame (survey = c(x),  plot = c(i),Md_chm = C(Md_chm),Mn_chm = c(Mn_chm),Max_chm = C(Max_chm),Na_chm = c(Na_chm),Md_idw = C(Md_idw),Mn_idw = c(Mn_idw),Max_idw = C(Max_idw),Na_idw = c(Na_idw),Na_dtm = c(Na_dtm))
  #add the plot data row to the master df for all plots and all surveys
  df <- rbind(df,df_p)

 
  next
}

# save the data frame as xcel spreadsheet in case we want to start processing from a particular survey number
write_xlsx(df,paste0("/raid/home/gs558/share/Reproducibility/Processed/plot_chm_metrics_temp",x,".xlsx"))  
#gc()
  next
}


# save the master data frame with all survey and plot data

#write_xlsx(df,"/raid/home/gs558/share/Reproducibility/Processed/plot_chm_metrics_master.xlsx")

toc()

#plot(chm)
#plot(idw)
#plot(chm_mask)
#plot(idw_mask)
