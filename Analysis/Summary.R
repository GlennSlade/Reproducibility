# Libraries
library(tidyverse)
library(viridis)
library(readxl)
library(writexl)
library(DescTools)
library(ggeffects)
library(splines)
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
#write_xlsx(master_df,"C:/Workspace/R_Scripts/Reproducibility/data/master_df.xlsx")

df4 <- master_df %>% mutate(empty = Ct_dtm - Ct_chm)# number of empty cells
df4 <- df4 %>% mutate(empty_prop = empty/Ct_dtm)# proportion of plot that is empty cells


df2 <- df4 %>% select (survey,plot,Mn_chm,Wind_Av,Sun_Elev_calc, Sun_Percent, empty_prop)

df3<-summarise(group_by(df2, survey),
               CHM=mean(Mn_chm),
               Wind=mean(Wind_Av),Sun = mean(Sun_Elev_calc), SunP = mean(Sun_Percent), Empty = mean(empty_prop))              


df<- df3

#Plotting


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





# Assign Average wind to X and Mean Canopy height to Y

x <- as.vector(df$Wind)
y <- as.vector(df$CHM)
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
  ggtitle(paste0("Comparison of Wind speed and CHM mean values from surveys"))+
  #theme(aspect.ratio=1)+
  xlab('Wind Speed (m/s)')+
  ylab('CHM - Mean all plots')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
#plot(  (paste0("P",i,"_w")))
plot(P1)
ggplot2::ggsave(
  P1,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_wind.jpg"),
  width = 10,
  height = 10,
  units = "cm"
) 

#---- Different data frames for different species



df_B <- filter(df4,PlotGenus == "Betula")
df_U <- filter(df4,PlotGenus == "Ulex europaeus")
df_S <- filter(df4,PlotGenus == "Salix aurita")
df_F <- filter(df4,PlotGenus == "Festuca arundinacea") 

           




#------Betula Summary -------

# Assign Average wind to X and Mean Canopy height to Y

df7 <- df_B %>% select (survey,plot,Mn_chm,Wind_Av,Sun_Elev_calc, Sun_Percent, empty_prop)

df<-summarise(group_by(df7, survey),
               CHM=mean(Mn_chm),
               Wind=mean(Wind_Av),Sun = mean(Sun_Elev_calc), SunP = mean(Sun_Percent), Empty = mean(empty_prop))              





x <- as.vector(df$Wind)
y <- as.vector(df$CHM)
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
  ggtitle(paste0("Betula Plots \n Comparison of Wind speed and CHM mean values from surveys"))+
  #theme(aspect.ratio=1)+
  xlab('Wind Speed (m/s)')+
  ylab('CHM - Mean all plots')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
#plot(  (paste0("P",i,"_w")))
plot(P1)
ggplot2::ggsave(
  P1,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_wind_betula.jpg"),
  width = 10,
  height = 10,
  units = "cm"
) 


#------Salix Summary -------

# Assign Average wind to X and Mean Canopy height to Y

df7 <- df_S %>% select (survey,plot,Mn_chm,Wind_Av,Sun_Elev_calc, Sun_Percent, empty_prop)

df<-summarise(group_by(df7, survey),
              CHM=mean(Mn_chm),
              Wind=mean(Wind_Av),Sun = mean(Sun_Elev_calc), SunP = mean(Sun_Percent), Empty = mean(empty_prop))              





x <- as.vector(df$Wind)
y <- as.vector(df$CHM)
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
  ggtitle(paste0("Salix Plots \n Comparison of Wind speed and CHM mean values from surveys"))+
  #theme(aspect.ratio=1)+
  xlab('Wind Speed (m/s)')+
  ylab('CHM - Mean all plots')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
#plot(  (paste0("P",i,"_w")))
plot(P1)
ggplot2::ggsave(
  P1,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_wind_salix.jpg"),
  width = 10,
  height = 10,
  units = "cm"
) 



#Ulex Summary plot

# Assign Average wind to X and Mean Canopy height to Y

df7 <- df_U %>% select (survey,plot,Mn_chm,Wind_Av,Sun_Elev_calc, Sun_Percent, empty_prop)

df<-summarise(group_by(df7, survey),
              CHM=mean(Mn_chm),
              Wind=mean(Wind_Av),Sun = mean(Sun_Elev_calc), SunP = mean(Sun_Percent), Empty = mean(empty_prop))              





x <- as.vector(df$Wind)
y <- as.vector(df$CHM)
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
  ggtitle(paste0("Ulex Plots \n Comparison of Wind speed and CHM mean values from surveys"))+
  #theme(aspect.ratio=1)+
  xlab('Wind Speed (m/s)')+
  ylab('CHM - Mean all plots')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
#plot(  (paste0("P",i,"_w")))
plot(P1)
ggplot2::ggsave(
  P1,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_wind_Ulex.jpg"),
  width = 10,
  height = 10,
  units = "cm"
) 


#Festuca Plots


# Assign Average wind to X and Mean Canopy height to Y

df7 <- df_F %>% select (survey,plot,Mn_chm,Wind_Av,Sun_Elev_calc, Sun_Percent, empty_prop)

df<-summarise(group_by(df7, survey),
              CHM=mean(Mn_chm),
              Wind=mean(Wind_Av),Sun = mean(Sun_Elev_calc), SunP = mean(Sun_Percent), Empty = mean(empty_prop))              





x <- as.vector(df$Wind)
y <- as.vector(df$CHM)
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
  ggtitle(paste0("Festuca Plots \n Comparison of Wind speed and CHM mean values from surveys"))+
  #theme(aspect.ratio=1)+
  xlab('Wind Speed (m/s)')+
  ylab('CHM - Mean all plots')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
#plot(  (paste0("P",i,"_w")))
plot(P1)
ggplot2::ggsave(
  P1,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_wind_festuca.jpg"),
  width = 10,
  height = 10,
  units = "cm"
) 



# Sun to x and CHM to y

df <- filter

x <- as.vector(df$Sun)
y <- as.vector(df$CHM)
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
  ggtitle(paste0("Comparison of Sun Elevation and CHM mean values (all plots) from surveys"))+
  #theme(aspect.ratio=1)+
  xlab('Sun elevation (degrees)')+
  ylab('CHM - Mean all plots')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(22,55),ylim=c(0.4,1))
#plot(  (paste0("P",i,"_w")))
plot(P1)
ggplot2::ggsave(
  P1,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_sun.jpg"),
  width = 10,
  height = 10,
  units = "cm"
)  

# x to sun percent y to CHM

x <- as.vector(df$SunP)
y <- as.vector(df$CHM)
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
  ggtitle(paste0("Comparison of Illumination and CHM mean values from surveys"))+
  #theme(aspect.ratio=1)+
  xlab('Sunshine percent during survey')+
  ylab('CHM - Mean all plots')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
#plot(  (paste0("P",i,"_w")))
plot(P1)
ggplot2::ggsave(
  P1,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_illumination.jpg"),
  width = 10,
  height = 10,
  units = "cm"
) 

# Assign Proportion empty cells to X and Mean Canopy height to Y

x <- as.vector(df$Wind)
y <- as.vector(df$Empty)
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
  ggtitle(paste0("Comparison of wind speed \n and mean proportion of plot without reconstructed points"))+
  #theme(aspect.ratio=1)+
  xlab('Wind speed (m/s)')+
  ylab('Proportion of plot without reconstructed points')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
#plot(  (paste0("P",i,"_w")))
plot(P1)
ggplot2::ggsave(
  P1,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_proportion empty.jpg"),
  width = 10,
  height = 10,
  units = "cm"
) 