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
CHM <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/plot_chm_metrics_temp30.xlsx")
# Import plot data: species, plot measurements etc
plot <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/Plot_Data.xlsx")

# -----2. Join data tables ------

# Join the survey and chm tables - for each plot CHM measurement you now have sun elevation wind speed etc 
survey_df <- full_join(CHM, survey, by = "survey")
# Now join with plot characteristics 
master_df <- full_join(survey_df,plot, by = "plot")


# filter for the Ulex Genus
#df<- filter(master_df,PlotGenus == "Betula")
df2<- filter(master_df,PlotGenus == "Ulex europaeus")
#df<- filter(master_df,PlotGenus == "Salix aurita")
#df<- filter(master_df,PlotGenus == "Festuca arundinacea")

#Filter master df for plot number i 
df<- filter(df2,plot == 17)

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
P17 <- ggplot(df_temp) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  #add the statistics
  #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Plot 17"))+
  #theme(aspect.ratio=1)+
  xlab('Wind Speed (m/s)')+
  ylab('Mean Canopy Height (m)')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
#plot(  (paste0("P",i,"_w")))
plot(P17)
}
df<- filter(df2,plot == 18)

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
  P18 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 18"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P18)
}
df<- filter(df2,plot == 19)

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
  P19 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 19"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P19)
}
df<- filter(df2,plot == 20)

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
  P20 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=20))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 20"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P20)
}
df<- filter(df2,plot == 21)

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
  P21 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=21))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 21"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P21)
}

df<- filter(df2,plot == 22)

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
  P22 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=22))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 22"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P22)
}
df<- filter(df2,plot == 23)

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
  P23 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=23))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 23"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P23)
}
df<- filter(df2,plot == 24)

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
  P24 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=24))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 24"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P24)
}
df<- filter(df2,plot == 25)

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
  P25 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=25))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 25"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P25)
}
df<- filter(df2,plot == 26)

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
  P26 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=26))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 26"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P26)
}

df<- filter(df2,plot == 27)

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
  P27 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=27))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 27"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P27)
}
df<- filter(df2,plot == 28)

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
  P28 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=28))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 28"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P28)
}
df<- filter(df2,plot == 29)

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
  P29 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=29))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 29"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P29)
}
df<- filter(df2,plot == 30)

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
  P30 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=30))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 30"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P30)
}
df<- filter(df2,plot == 31)

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
  P31 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=31))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 31"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P31)
}
df<- filter(df2,plot == 32)

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
  P32 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=32))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 32"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('Mean Canopy Height (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P32)
}

P_All16_n <- ggarrange(P32,P30,P25,P31,P18,P20,P22,P19,P21,P23,P29, P24,P26,P17,P27,P28, ncol = 4, nrow = 4)
plot(P_All16_n)

ggsave(
  P_All16_n,
  # filename = "/plots/test.png",
  filename = "output_data/plots/ulex_16_plots.png",
  width = 16,
  height = 16,
  units = "cm"
)