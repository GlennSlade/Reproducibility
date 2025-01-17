# Libraries
library(tidyverse)
library(viridis)
library(readxl)
library(writexl)
library(DescTools)
#library(ggsave2)

#----1. Import data -----

# Import environmental data for each survey: wind sun elevation etc
survey <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/Survey_Data.xlsx")
# Import reconstructed canopy height estimates for each plot and each survey  
CHM <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/plot_chm_metrics_temp30.xlsx")
# Import plot data: species, plot measurements etc
plot <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/Plot_Data.xlsx")

# -----2. Join data tables ------

# Join the survey and chm tables - for each plot CHM measurement you now have sun elevation wind speed etc 
survey_df <- full_join(CHM, survey, by = "survey")
# Now join with plot characteristics 
master_df <- full_join(survey_df,plot, by = "plot")

#save the master_df for use in other scripts
write_xlsx(master_df,"C:/Workspace/R_Scripts/Reproducibility/data/master_df.xlsx")

#------3. Wind Simple analysis-------

# Lets start by looking at how mean canopy height from point cloud (not interpolated) plots against average wind speed during the survey
# To start lets look individually for each plot - and separate these out for the different plot dominant species in a panel of 16 plots

## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 8, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 8,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 8, color = "black"),
      legend.title = element_text(size = 8, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.9, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      )
    )
}
windowsFonts("Helvetica" = windowsFont("Helvetica")) # Ensure font is mapped correctly

# make empty data fram for statistics

df_all <- data.frame(plot = integer(),Mean_chm = double(),R2 = double(),MAD= double(),Slope= double())

# loop through each plot number - generating graph

for (i in 1:64) {

#Filter master df for plot number i 
df<- filter(master_df,plot == i)

# Assign Average wind to X and Mean Canopy height to Y

x <- as.vector(df$Wind_Av)
y <- as.vector(df$Mn_chm)
df_temp <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df_temp)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x") # equation for printing

# Compute the Lin's  correlation concordance coefficient
#ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
#ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

Mean_chm <- mean(df$Mn_chm)

# Prepare statistics for summary table

# make data frame row for plot level statistics
df_p<-data.frame (plot = c(i), Mean_chm = c(Mean_chm),R2 = c(r2val),MAD= c(MADval),Slope = c(tls_slp))
#add the plot data row to the master df for all plots and all surveys
df_all <- rbind(df_all,df_p)


#Plot the graph

#(paste0("P",i,"_w")) <- ggplot(df_temp) +
P1 <- ggplot(df_temp) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  #add the statistics
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Plot ",i,": Comparison of Canopy Height estimated \n from surveys with different wind speeds"))+
  #theme(aspect.ratio=1)+
  xlab('Wind Speed (m/s)')+
  ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
  #coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
#plot(  (paste0("P",i,"_w")))
plot(P1)
 ggplot2::ggsave(
  P1,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/plots/P",i,"_wind_chm.jpg"),
  width = 10,
  height = 10,
  units = "cm"
)

  
  next
}

write_xlsx(df_all,"C:/Workspace/R_Scripts/Reproducibility/data/plot_statistics_wind.xlsx")


#-----4. Sun elevation Simple analysis -------


df_all_sun <- data.frame(plot = integer(),Mean_chm = double(),R2 = double(),MAD= double(),Slope= double())



# loop through each plot number - generating graph



for (i in 1:64) {
  
  #Filter master df for plot number i 
  df<- filter(master_df,plot == i)
  
  # Assign sun elevation to X and Mean Canopy height to Y
  
  x <- as.vector(df$Sun_Elev_calc)
  y <- as.vector(df$Mn_chm)
  df_temp <- data.frame(x = x, y = y,
                        d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df_temp)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x") # equation for printing
  
  # Compute the Lin's  correlation concordance coefficient
 # ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  Mean_chm <- mean(df$Mn_chm)
  
  # make data frame row for plot level statistics
  df_p<-data.frame (plot = c(i), Mean_chm = c(Mean_chm),R2 = c(r2val),MAD= c(MADval),Slope = c(tls_slp))
  #add the plot data row to the master df for all plots and all surveys
  df_all_sun <- rbind(df_all_sun,df_p)
  
  
  
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P1 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot ",i,": Comparison of Canopy Height estimated from \n surveys with different survey sun elevations"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
  #coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
  #plot(  (paste0("P",i,"_w")))
  plot(P1)
  ggplot2::ggsave(
    P1,
    # filename = "/plots/test.png",
    filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/plots/Sun/P",i,"_sun_chm.jpg"),
    width = 10,
    height = 10,
    units = "cm"
  )
  
  
  next
}

write_xlsx(df_all_sun,"C:/Workspace/R_Scripts/Reproducibility/data/plot_statistics_sun.xlsx")



#------5. Is there covariance or multi-collinearity between sun. wind or cloud cover ?-----

#Filter master df for plot number i 
df<- master_df

# Assign Average wind to X and Mean Canopy height to Y

x <- as.vector(df$Sun_Elev_calc)
y <- as.vector(df$Wind_Av)
df_temp <- data.frame(x = x, y = y,
                      d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df_temp)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x") # equation for printing

# Compute the Lin's  correlation concordance coefficient
# ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
#ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

#Plot the graph

#(paste0("P",i,"_w")) <- ggplot(df_temp) +
P1 <- ggplot(df_temp) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  #add the statistics
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Comparison of Wind speed and sun \n elevation from surveys"))+
  #theme(aspect.ratio=1)+
  xlab('Sun Elevation (degrees)')+
  ylab('Wind Speed (m/s)')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
#plot(  (paste0("P",i,"_w")))
plot(P1)
ggplot2::ggsave(
  P1,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/wind_sun.jpg"),
  width = 10,
  height = 10,
  units = "cm"
)

#----6. Wind vs idw chm

for (i in 1:64) {
  
  #Filter master df for plot number i 
  df<- filter(master_df,plot == i)
  
  # Assign Average wind to X and Mean Canopy height to Y
  
  x <- as.vector(df$Wind_Av)
  y <- as.vector(df$Mn_idw)
  df_temp <- data.frame(x = x, y = y,
                        d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df_temp)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x") # equation for printing
  
  # Compute the Lin's  correlation concordance coefficient
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P1 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot ",i,": Comparison of Interploated Canopy Height estimated  \n from surveys with different wind speeds"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
  #coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
  #plot(  (paste0("P",i,"_w")))
  plot(P1)
  ggplot2::ggsave(
    P1,
    # filename = "/plots/test.png",
    filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/plots/P",i,"_wind_chm_idw.jpg"),
    width = 10,
    height = 10,
    units = "cm"
  )
  
  
  next
}


#----7. sun vs idw chm
# loop through each plot number - generating graph

for (i in 1:64) {
  
  #Filter master df for plot number i 
  df<- filter(master_df,plot == i)
  
  # Assign Average wind to X and Mean Canopy height to Y
  
  x <- as.vector(df$Sun_Elev_calc)
  y <- as.vector(df$Mn_idw)
  df_temp <- data.frame(x = x, y = y,
                        d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  pca <- prcomp(~x+y,df_temp)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x") # equation for printing
  
  # Compute the Lin's  correlation concordance coefficient
  # ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P1 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot ",i,": Comparison of Interpolated Canopy Height estimated  \n from surveys with different survey sun elevations"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
  #coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
  #plot(  (paste0("P",i,"_w")))
  plot(P1)
  ggplot2::ggsave(
    P1,
    # filename = "/plots/test.png",
    filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/plots/Sun/P",i,"_sun_ichm.jpg"),
    width = 10,
    height = 10,
    units = "cm"
  )
  
  
  next
}

