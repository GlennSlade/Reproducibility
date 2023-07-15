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


# filter for the betula Genus
df2<- filter(master_df,PlotGenus == "Betula")
#df2<- filter(master_df,PlotGenus == "Ulex europaeus")
#df2<- filter(master_df,PlotGenus == "Salix aurita")
#df2<- filter(master_df,PlotGenus == "Festuca arundinacea")

#Filter master df for plot number i 
df<- filter(df2,plot == 33)

{# Assign Average wind to X and MCH to Y

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
P33 <- ggplot(df_temp) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  #add the statistics
  #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=36))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Plot 33"))+
  #theme(aspect.ratio=1)+
  xlab('Wind Speed (m/s)')+
  ylab('MCH (m)')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
#plot(  (paste0("P",i,"_w")))
plot(P33)
}
df<- filter(df2,plot == 34)

{# Assign Average wind to X and MCH to Y
  
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
  P34 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=36))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 34"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('MCH (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P34)
}
df<- filter(df2,plot == 35)

{# Assign Average wind to X and MCH to Y
  
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
  P35 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=36))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 35"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('MCH (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P35)
}
df<- filter(df2,plot == 36)

{# Assign Average wind to X and MCH to Y
  
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
  P36 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=36))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 36"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('MCH (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P36)
}
df<- filter(df2,plot == 37)

{# Assign Average wind to X and MCH to Y
  
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
  P37 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=37))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 37"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('MCH (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P37)
}

df<- filter(df2,plot == 43)

{# Assign Average wind to X and MCH to Y
  
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
  P43 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=43))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 43"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('MCH (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P43)
}
df<- filter(df2,plot == 44)

{# Assign Average wind to X and MCH to Y
  
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
  P44 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=44))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 44"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('MCH (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P44)
}
df<- filter(df2,plot == 45)

{# Assign Average wind to X and MCH to Y
  
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
  P45 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=45))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 45"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('MCH (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P45)
}
df<- filter(df2,plot == 47)

{# Assign Average wind to X and MCH to Y
  
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
  P47 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=47))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 47"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('MCH (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P47)
}
df<- filter(df2,plot == 48)

{# Assign Average wind to X and MCH to Y
  
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
  P48 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=48))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 48"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('MCH (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P48)
}

df<- filter(df2,plot == 50)

{# Assign Average wind to X and MCH to Y
  
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
  P50 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=50))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 50"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('MCH (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P50)
}
df<- filter(df2,plot == 52)

{# Assign Average wind to X and MCH to Y
  
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
  P52 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=52))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 52"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('MCH (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P52)
}
df<- filter(df2,plot == 59)

{# Assign Average wind to X and MCH to Y
  
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
  P59 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=59))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 59"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('MCH (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P59)
}
df<- filter(df2,plot == 61)

{# Assign Average wind to X and MCH to Y
  
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
  P61 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=61))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 61"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('MCH (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P61)
}
df<- filter(df2,plot == 62)

{# Assign Average wind to X and MCH to Y
  
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
  P62 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=62))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 62"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('MCH (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P62)
}
df<- filter(df2,plot == 63)

{# Assign Average wind to X and MCH to Y
  
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
  P63 <- ggplot(df_temp) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    geom_point(aes(x, y), alpha=0.3, size = 1) +
    #add the statistics
    #geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
    #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
    #geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
    #theme(text = element_text(size=63))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Plot 63"))+
    #theme(aspect.ratio=1)+
    xlab('Wind Speed (m/s)')+
    ylab('MCH (m)')
  #coord_equal(ratio=1)
#  coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
  #plot(  (paste0("P",i,"_w")))
  plot(P63)
}

# P_All16_n <- ggarrange(P63,P61,P47,P62,P34,P36,P43,P35,P37,P44,P59, P45,P48,P33,P50,P52, ncol = 4, nrow = 4)
# plot(P_All16_n)
P_All16_n <- ggarrange(P59,P50,P48,P52,P43,P35,P61,P62,P63,P45,P44,P37, P36,P33,P47,P34, ncol = 4, nrow = 4)
plot(P_All16_n)


ggsave(
  P_All16_n,
  # filename = "/plots/test.png",
  filename = "output_data/plots/betula_16_plots.png",
  width = 16,
  height = 16,
  units = "cm"
)