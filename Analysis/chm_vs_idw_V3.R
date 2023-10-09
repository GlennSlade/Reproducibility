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
survey <- read_xlsx("data/Survey_Data.xlsx")
# Import reconstructed canopy height estimates for each plot and each survey  
idw <- read_xlsx("data/plot_chm_metrics_temp61.xlsx")
# Import plot data: species, plot measurements etc
plot <- read_xlsx("data/Plot_Data.xlsx")

# -----2. Join data tables ------

# Join the survey and idw tables - for each plot idw measurement you now have sun elevation wind speed etc 
survey_df <- full_join(idw, survey, by = "survey")
# Now join with plot characteristics 
master_df <- full_join(survey_df,plot, by = "plot")

df<-master_df

x <- as.vector(df$Mn_chm)
x <- na.omit(x)
y <- as.vector(df$Mn_idw)
y <- na.omit(y)
df_temp2 <- data.frame(x = x, y = y,
                       d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))


#df_temp <- na.omit(df_temp2)

# Calculate Total Least Squares Regression (extracted from base-R PCA function)

pca <- prcomp(~x+y,df_temp2)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x") # equation for printing

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

#Plot the graph

#(paste0("P",i,"_w")) <- ggplot(df_temp) +
Pcompare <- ggplot(df_temp2) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  #  add the statistics
  geom_text(aes(x=0.0,y=3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=2.8),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=2.6),label=ccc,hjust='left', size=3.0)+
  geom_text(aes(x=0.0,y=2.4),label=equation,hjust='left', size=3.0)+
  theme(text = element_text(size=36))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Intercomparison of CHM and IDW derived canopy heights"))+
  theme(aspect.ratio=1)+
  xlab('CHM Mean Canopy Height (m)')+
  ylab('IDW Mean Canopy Height (m)')+
  coord_equal(ratio=1)
#coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
#plot(  (paste0("P",i,"_w")))
plot(Pcompare)

ggsave(
  Pcompare,
  # filename = "/plots/test.png",
  filename = "output_data/plots/comparison_chm_idw_v2.png",
  width = 10,
  height = 10,
  units = "cm"
)

  # Individual Species 
  
  # filter for the diiferent Plot Genus
  df_B <- filter(master_df,PlotGenus == "Betula")
  df_U <- filter(master_df,PlotGenus == "Ulex europaeus")
  df_S <- filter(master_df,PlotGenus == "Salix aurita")
  df_F <- filter(master_df,PlotGenus == "Festuca arundinacea") 
  
 
 df <- df_F
  
  x <- as.vector(df$Mn_chm)
  x <- na.omit(x)
  y <- as.vector(df$Mn_idw)
  y <- na.omit(y)
  df_temp2 <- data.frame(x = x, y = y,
                         d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  
  
  #df_temp <- na.omit(df_temp2)
  
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  
  pca <- prcomp(~x+y,df_temp2)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x") # equation for printing
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  PcompareF <- ggplot(df_temp2) +
    geom_point(aes(x, y), col = "steelblue",alpha=0.3, size = 1) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    
    #  add the statistics
    geom_text(aes(x=0.0,y=0.7),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.65),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=0.6),label=ccc,hjust='left', size=3.0)+
    geom_text(aes(x=0.0,y=0.55),label=equation,hjust='left', size=3.0)+
    theme(text = element_text(size=36))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Festuca arundinacea"))+
    theme(aspect.ratio=1)+
    xlab('CHM Mean Canopy Height (m)')+
    ylab('IDW Mean Canopy Height (m)')+
    coord_equal(ratio=1)+
  coord_fixed(xlim=c(0,0.7),ylim=c(0,0.7))
  #plot(  (paste0("P",i,"_w")))
  plot(PcompareF)
  
  
  df <- df_S
  
  # Plot Salix only
  
  x <- as.vector(df$Mn_chm)
  x <- na.omit(x)
  y <- as.vector(df$Mn_idw)
  y <- na.omit(y)
  df_temp2 <- data.frame(x = x, y = y,
                         d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  
  
  #df_temp <- na.omit(df_temp2)
  
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  
  pca <- prcomp(~x+y,df_temp2)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x") # equation for printing
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  PcompareS <- ggplot(df_temp2) +
    geom_point(aes(x, y), col='darkgreen',alpha=0.3, size = 1) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    
    #  add the statistics
    geom_text(aes(x=0.0,y=3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=2.8),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=2.6),label=ccc,hjust='left', size=3.0)+
    geom_text(aes(x=0.0,y=2.4),label=equation,hjust='left', size=3.0)+
    theme(text = element_text(size=36))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Salix aurita"))+
    theme(aspect.ratio=1)+
    xlab('CHM Mean Canopy Height (m)')+
    ylab('IDW Mean Canopy Height (m)')+
    coord_equal(ratio=1)+
  coord_fixed(xlim=c(0,3),ylim=c(0,3))
  #plot(  (paste0("P",i,"_w")))
  plot(PcompareS)

  
  df <- df_B
  
  # Plot Betula only
  
  x <- as.vector(df$Mn_chm)
  x <- na.omit(x)
  y <- as.vector(df$Mn_idw)
  y <- na.omit(y)
  df_temp2 <- data.frame(x = x, y = y,
                         d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  
  
  #df_temp <- na.omit(df_temp2)
  
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  
  pca <- prcomp(~x+y,df_temp2)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x") # equation for printing
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  PcompareB <- ggplot(df_temp2) +
    geom_point(aes(x, y),col='purple', alpha=0.3, size = 1) +
    geom_smooth(aes(x, y,col='black', weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    
    #  add the statistics
    geom_text(aes(x=0.0,y=3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=2.8),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=2.6),label=ccc,hjust='left', size=3.0)+
    geom_text(aes(x=0.0,y=2.4),label=equation,hjust='left', size=3.0)+
    theme(text = element_text(size=36))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Betula"))+
    theme(aspect.ratio=1)+
    xlab('CHM Mean Canopy Height (m)')+
    ylab('IDW Mean Canopy Height (m)')+
    coord_equal(ratio=1)+
  coord_fixed(xlim=c(0,3),ylim=c(0,3))
  #plot(  (paste0("P",i,"_w")))
  plot(PcompareB)    
  
  
  df <- df_U
  
  # Plot Ulex only
  
  x <- as.vector(df$Mn_chm)
  x <- na.omit(x)
  y <- as.vector(df$Mn_idw)
  y <- na.omit(y)
  df_temp2 <- data.frame(x = x, y = y,
                         d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  
  
  #df_temp <- na.omit(df_temp2)
  
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  
  pca <- prcomp(~x+y,df_temp2)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x") # equation for printing
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  PcompareU <- ggplot(df_temp2) +
    geom_point(aes(x, y), col='#F0E442',alpha=0.3, size = 1) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    
    #  add the statistics
    geom_text(aes(x=0.0,y=1.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=1.4),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=1.3),label=ccc,hjust='left', size=3.0)+
    geom_text(aes(x=0.0,y=1.2),label=equation,hjust='left', size=3.0)+
    theme(text = element_text(size=36))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Ulex europaeus"))+
    theme(aspect.ratio=1)+
    xlab('CHM Mean Canopy Height (m)')+
    ylab('IDW Mean Canopy Height (m)')+
    coord_equal(ratio=1)+
  coord_fixed(xlim=c(0,1.55),ylim=c(0,1.55))
  #plot(  (paste0("P",i,"_w")))
  plot(PcompareU)    

  
  Panel4<- ggarrange(PcompareF,PcompareU,PcompareB,PcompareS, ncol = 2, nrow = 2)
  plot(Panel4)
  
  
  ggsave(
    Panel4,
    # filename = "/plots/test.png",
    filename = "output_data/plots/chm_vs_idw_species_v2.png",
    width = 16,
    height = 16,
    units = "cm"
  )  
  
  
 # Repeat comparison for Plot Means from plot statistics
  
  Mean_Plot_Statistics <- read_xlsx("output_data/summary_plot_statistics.xlsx")
  
  df <- Mean_Plot_Statistics
  
  # All Species
  
  x <- as.vector(df$CHM_MEAN)
  x <- na.omit(x)
  y <- as.vector(df$IDW_Mean)
  y <- na.omit(y)
  df_temp2 <- data.frame(x = x, y = y,
                         d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
  
  
  #df_temp <- na.omit(df_temp2)
  
  # Calculate Total Least Squares Regression (extracted from base-R PCA function)
  
  pca <- prcomp(~x+y,df_temp2)
  tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
  tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
  equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x") # equation for printing
  
  # Compute the Lin's  correlation concordance coefficient
  ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
  ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))
  
  MADval <- mean(abs(x-y))
  MADrel <- MADval/mean(x)*100
  lmres <- lm(y~x)
  r2val <- summary(lmres)$r.squared
  
  #Plot the graph
  
  #(paste0("P",i,"_w")) <- ggplot(df_temp) +
  PcompareMN <- ggplot(df_temp2) +
    geom_point(aes(x, y), col='black',alpha=0.3, size = 1) +
    geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
    
    #  add the statistics
    geom_text(aes(x=0.0,y=1.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=1.4),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
    geom_text(aes(x=0.0,y=1.3),label=ccc,hjust='left', size=3.0)+
    geom_text(aes(x=0.0,y=1.2),label=equation,hjust='left', size=3.0)+
    theme(text = element_text(size=36))+
    scale_color_identity() +
    theme_fancy() +
    #add title and labels
    ggtitle(paste0("Mean Plot Data - all surveys"))+
    theme(aspect.ratio=1)+
    xlab('CHM Mean Canopy Height (m)')+
    ylab('IDW Mean Canopy Height (m)')+
    coord_equal(ratio=1)+
    coord_fixed(xlim=c(0,4),ylim=c(0,4))
  #plot(  (paste0("P",i,"_w")))
  plot(PcompareMN)  
  
  