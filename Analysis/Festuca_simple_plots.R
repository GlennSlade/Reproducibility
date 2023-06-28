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
#df2<- filter(master_df,PlotGenus == "Salix aurita")
df2<- filter(master_df,PlotGenus == "Festuca arundinacea")

#Filter master df for plot number i 
df<- filter(df2,plot == 8)

{# Assign Average wind to X and Mean Canopy height to Y

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




#Plot the graph

#(paste0("P",i,"_w")) <- ggplot(df_temp) +
P8 <- ggplot(df_temp) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  #add the statistics
  #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
  #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=6))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Plot 8"))+
  #theme(aspect.ratio=1)+
  xlab('Wind Speed (m/s)')+
  ylab('Mean Canopy Height (m)')+
#coord_equal(ratio=1)
coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
#plot(  (paste0("P",i,"_w")))
plot(P8)
}
df<- filter(df2,plot == 16)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P16 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=6))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 16"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')+
  #coord_equal(ratio=1)
 coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
  #plot(  (paste0("P",i,"_w")))
  plot(P16)
}
df<- filter(df2,plot == 15)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P15 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=6))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 15"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')+
  #coord_equal(ratio=1)
 coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
  #plot(  (paste0("P",i,"_w")))
  plot(P15)
}
df<- filter(df2,plot == 6)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P6 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=6))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 6"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')+
  #coord_equal(ratio=1)
 coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
  #plot(  (paste0("P",i,"_w")))
  plot(P6)
}
df<- filter(df2,plot == 1)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P1 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=1))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 1"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')+
  #coord_equal(ratio=1)
 coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
  #plot(  (paste0("P",i,"_w")))
  plot(P1)
}

df<- filter(df2,plot == 7)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P7 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=7))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 7"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')+
  #coord_equal(ratio=1)
 coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
  #plot(  (paste0("P",i,"_w")))
  plot(P7)
}
df<- filter(df2,plot == 9)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P9 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=9))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 9"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')+
  #coord_equal(ratio=1)
 coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
  #plot(  (paste0("P",i,"_w")))
  plot(P9)
}
df<- filter(df2,plot == 12)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P12 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=12))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 12"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')+
  #coord_equal(ratio=1)
 coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
  #plot(  (paste0("P",i,"_w")))
  plot(P12)
}
df<- filter(df2,plot == 2)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P2 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=2))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 2"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')+
  #coord_equal(ratio=1)
 coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
  #plot(  (paste0("P",i,"_w")))
  plot(P2)
}
df<- filter(df2,plot == 11)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P11 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=11))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 11"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')+
  #coord_equal(ratio=1)
 coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
  #plot(  (paste0("P",i,"_w")))
  plot(P11)
}

df<- filter(df2,plot == 13)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P13 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=13))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 13"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')+
  #coord_equal(ratio=1)
 coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
  #plot(  (paste0("P",i,"_w")))
  plot(P13)
}
df<- filter(df2,plot == 10)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P10 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=10))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 10"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')+
  #coord_equal(ratio=1)
 coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
  #plot(  (paste0("P",i,"_w")))
  plot(P10)
}
df<- filter(df2,plot == 14)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P14 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=14))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 14"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')+
  #coord_equal(ratio=1)
 coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
  #plot(  (paste0("P",i,"_w")))
  plot(P14)
}
df<- filter(df2,plot == 5)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P5 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=5))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 5"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')+
  #coord_equal(ratio=1)
 coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
  #plot(  (paste0("P",i,"_w")))
  plot(P5)
}
df<- filter(df2,plot == 3)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P3 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=3))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 3"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')+
  #coord_equal(ratio=1)
 coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
  #plot(  (paste0("P",i,"_w")))
  plot(P3)
}
df<- filter(df2,plot == 4)

{# Assign Average wind to X and Mean Canopy height to Y
  
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
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  P4 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.39),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.9),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.4),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=4))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 4"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')+
  #coord_equal(ratio=1)
 coord_fixed(xlim=c(0.39,2.55),ylim=c(0,0.6))
  #plot(  (paste0("P",i,"_w")))
  plot(P4)
}

# P_All16_n <- ggarrange(P4,P5,P2,P3,P16,P6,P7,P15,P1,P9,P14, P12,P11,P8,P13,P10, ncol = 4, nrow = 4)
# plot(P_All16_n)
P_All16_n <- ggarrange(P14,P13,P11,P10,P7,P15,P5,P3,P4,P12,P9,P1, P6,P8,P2,P16, ncol = 4, nrow = 4)
plot(P_All16_n)


ggsave(
  P_All16_n,
  # filename = "/plots/test.png",
  filename = "output_data/plots/Festuca_16_plots_fixed_coords.png",
  width = 16,
  height = 16,
  units = "cm"
)