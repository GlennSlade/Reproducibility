
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

library (raster)
library(dplyr)
library(remotes)
library(cowplot)
library(reshape2)
library(readxl)
library(tictoc)

P_sf <- read_sf(dsn = "C:/Workspace/Reproducibility/GIS_Layers/", layer = "plot_polygons")

plot<-parse_number(P_sf$Plot)

P_sf <- dplyr::mutate (P_sf,plot)  

plot_data <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/Plot_Data.xlsx")

master_df <- full_join(P_sf,plot_data, by = "plot")

st_write (master_df, "C:/Workspace/Reproducibility/GIS_Layers/plot_polygons_data.shp") 
