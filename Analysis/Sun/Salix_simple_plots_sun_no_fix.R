# Libraries
library(tidyverse)
library(viridis)
library(readxl)
library(writexl)
library(DescTools)
library(cowplot)
library(ggpubr)
#library(ggsave2)

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



#----1. Import data -----

# Import environmental data for each survey: wind sun elevation etc
survey <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/Survey_Data.xlsx")
# Import reconstructed canopy height estimates for each plot and each survey  
CHM <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/plot_chm_metrics_temp20.xlsx")
# Import plot data: species, plot measurements etc
plot <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/Plot_Data.xlsx")

# -----2. Join data tables ------

# Join the survey and chm tables - for each plot CHM measurement you now have sun elevation wind speed etc 
survey_df <- full_join(CHM, survey, by = "survey")
# Now join with plot characteristics 
master_df <- full_join(survey_df,plot, by = "plot")


# filter for the betula Genus
#df2<- filter(master_df,PlotGenus == "Betula")
#df2<- filter(master_df,PlotGenus == "Ulex europaeus")
df2<- filter(master_df,PlotGenus == "Salix aurita")
#df2<- filter(master_df,PlotGenus == "Festuca arundinacea")

#Filter master df for plot number i 
df<- filter(df2,plot == 51)

{# Assign Average wind to X and Mean Canopy height to Y

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
#ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
#ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared




#Plot the graph

#(paste0("P",i,"_w")) <- ggplot(df_temp) +
P51 <- ggplot(df_temp) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  #add the statistics
  #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
  #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=46))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Plot 51"))+
  #theme(aspect.ratio=1)+
  xlab('Sun Elevation (degrees)')+
  ylab('Mean Canopy Height (m)')
#coord_equal(ratio=1)
coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
#plot(  (paste0("P",i,"_w")))
plot(P51)
}
df<- filter(df2,plot == 64)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P64 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=46))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 64"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
  #plot(  (paste0("P",i,"_w")))
  plot(P64)
}
df<- filter(df2,plot == 60)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P60 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=46))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 60"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
  #plot(  (paste0("P",i,"_w")))
  plot(P60)
}
df<- filter(df2,plot == 46)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P46 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=46))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 46"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
  #plot(  (paste0("P",i,"_w")))
  plot(P46)
}
df<- filter(df2,plot == 38)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P38 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=38))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 38"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
  #plot(  (paste0("P",i,"_w")))
  plot(P38)
}

df<- filter(df2,plot == 49)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P49 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=49))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 49"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
  #plot(  (paste0("P",i,"_w")))
  plot(P49)
}
df<- filter(df2,plot == 53)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P53 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=53))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 53"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
  #plot(  (paste0("P",i,"_w")))
  plot(P53)
}
df<- filter(df2,plot == 56)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P56 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=56))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 56"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
  #plot(  (paste0("P",i,"_w")))
  plot(P56)
}
df<- filter(df2,plot == 39)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P39 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=39))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 39"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
  #plot(  (paste0("P",i,"_w")))
  plot(P39)
}
df<- filter(df2,plot == 55)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P55 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=55))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 55"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
  #plot(  (paste0("P",i,"_w")))
  plot(P55)
}

df<- filter(df2,plot == 57)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P57 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=57))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 57"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
  #plot(  (paste0("P",i,"_w")))
  plot(P57)
}
df<- filter(df2,plot == 54)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P54 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=54))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 54"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
  #plot(  (paste0("P",i,"_w")))
  plot(P54)
}
df<- filter(df2,plot == 58)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P58 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=58))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 58"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
  #plot(  (paste0("P",i,"_w")))
  plot(P58)
}
df<- filter(df2,plot == 42)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P42 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=42))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 42"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
  #plot(  (paste0("P",i,"_w")))
  plot(P42)
}
df<- filter(df2,plot == 40)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P40 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=40))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 40"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
  #plot(  (paste0("P",i,"_w")))
  plot(P40)
}
df<- filter(df2,plot == 41)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  #ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  #ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P41 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=20),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.53),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=41))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 41"))+
    #theme(aspect.ratio=1)+
    xlab('Sun Elevation (degrees)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#coord_fixed(xlim=c(20,55),ylim=c(0,3.4))
  #plot(  (paste0("P",i,"_w")))
  plot(P41)
}

# P_All16_n <- ggarrange(P41,P42,P39,P40,P64,P46,P49,P60,P38,P53,P58, P56,P55,P51,P57,P54, ncol = 4, nrow = 4)
# plot(P_All16_n)
P_All16_n <- ggarrange(P58,P57,P55,P54,P49,P60,P42,P40,P41,P56,P53,P38, P46,P51,P39,P64, ncol = 4, nrow = 4)
plot(P_All16_n)


ggsave(
  P_All16_n,
  # filename = "/plots/test.png",
  filename = "output_data/plots/sun/salix_16_plots_sun.png",
  width = 16,
  height = 16,
  units = "cm"
)