# Libraries
library(tidyverse)
library(viridis)
library(readxl)
library(writexl)
library(DescTools)
library(ggeffects)
library(splines)
library(cowplot)
library(ggpubr)
library(patchwork)
#library(ggsave2)

#----1. Import data -----

# Import environmental data for each survey: wind sun elevation etc
survey <- read_xlsx("data/Survey_Data.xlsx")
# Import reconstructed canopy height estimates for each plot and each survey  
CHM <- read_xlsx("data/plot_chm_metrics_temp61.xlsx")
# Import plot data: species, plot measurements etc
plot <- read_xlsx("data/Plot_Data.xlsx")

# -----2. Join data tables ------

# Import environmental data for each survey: wind sun elevation etc
survey <- read_xlsx("data/Survey_Data.xlsx")
# Import reconstructed canopy height estimates for each plot and each survey  
CHM <- read_xlsx("data/plot_chm_metrics_temp61.xlsx")# needs to be updated with latest survey data
# Import plot data: species, plot measurements etc
plot <- read_xlsx("data/Plot_Data.xlsx")

#
plot_statistics <- read_xlsx("output_data/summary_plot_statistics.xlsx")# script needs to be run to generate this. 
# Select Mean of all plot heights (proxy for plant height) and % Variance
statistics <- plot_statistics

# Join the survey and chm tables - for each plot CHM measurement you now have sun elevation wind speed etc 
survey_df <- full_join(CHM, survey, by = "survey")
# Now join with plot characteristics 
master<- full_join(survey_df,plot, by = "plot")

# join with plot statistics
master_df <- full_join(master,statistics, by = "plot")

#calculate empty cells

df4 <- master_df %>% mutate(empty = Ct_dtm - Ct_chm)# number of empty cells
df4 <- df4 %>% mutate(empty_prop = empty/Ct_dtm)# proportion of plot that is empty cells

#Calculate relative difference in canopy height from Max height recorded for plot (any survey)
df4 <- df4 %>% mutate(RDCHM = CHM_MAX - Mn_chm)# GLMER Gamma needs positive numbers hence Max - chm
df4 <- df4 %>% mutate(RDCHM_Percent = (RDCHM/CHM_MAX)* 100)# Difference as a percentage of height of plant


# Fudge for now - GLMER gamma does not support non positive values which includes zero
# adding a small amount to RDCHM to ensure all values non zero
df4 <- df4 %>% mutate(RDCHM = RDCHM+0.00001)


# 3 class illumination  0=  diffuse light no shadows 1= varied shadow mixed conditions 2= direct light strong shadows 
df4 <- df4 %>% mutate(illumination = case_when(Sun_Percent <= 20 ~ 0,
                                               (Sun_Percent > 20 &  Sun_Percent <80 )~ 1, Sun_Percent >= 80 ~ 2,
                                               TRUE ~ NA_real_))

# binary sky code 1= sun 0 = cloudy
df4 <- df4  %>% mutate(binary_skycode = case_when(Sky_Code <= 5 ~ 1,
                                                  Sky_Code > 5 ~ 0,
                                                  TRUE ~ NA_real_))


df2 <- df4 %>% dplyr::select (survey,plot,RDCHM,RDCHM_Percent,Mn_chm,Wind_Av,Sun_Elev_calc, Sun_Percent, empty_prop,Wind_SD,Wind_Max)

df3<-summarise(group_by(df2, survey),
               CHM=mean(Mn_chm),
               Wind=mean(Wind_Av),Sun = mean(Sun_Elev_calc), SunP = mean(Sun_Percent), Empty = mean(empty_prop),Wind_SD = mean (Wind_SD), Wind_Max = mean (Wind_Max))              


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



# WSind vs proportion of plot reconstructed
df3
x <- as.vector(df3$Wind)
y <- as.vector(1-df3$Empty)
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
par(pty="s")
#(paste0("P",i,"_w")) <- ggplot(df_temp) +
P1 <- ggplot(df_temp) +
  geom_smooth(aes(x, y,col='darkgrey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  #add the statistics
  geom_text(aes(x=1.8,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.25)+
  geom_text(aes(x=1.8,y=0.48),label=paste0('R2: ',round(r2val,3)),hjust='left',size=2.25)+
  #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=1.8,y=0.46),label=equation,hjust='left', size=2.25)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0(""))+
  #theme(aspect.ratio=1)+
  xlab('Wind speed (m/s)')+
  ylab('Proportion of plot reconstructed')+
  scale_x_continuous(expand = c(0, 0), limits = c(0.25, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0.4, 0.8))
#  coord_cartesian(ylim = c(0.4, 0.8))+expand_limits(x = 0, y = 0.4)

#plot(  (paste0("P",i,"_w")))
plot(P1)
ggplot2::ggsave(
  P1,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/summary_proportion_reconstructed.jpg"),
  width = 8,
  height = 8,
  units = "cm"
) 

# as above but  Sun Percentage / proportion of plot reconstructed

x <- as.vector(df3$SunP)
y <- as.vector(1-df3$Empty)
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
P2 <- ggplot(df_temp) +
  geom_smooth(aes(x, y,col='darkgrey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  #add the statistics
  geom_text(aes(x=70,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.25)+
  geom_text(aes(x=70,y=0.48),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.25)+
  #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=70,y=0.46),label=equation,hjust='left', size=2.25)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0(""))+
  #theme(aspect.ratio=1)+
  xlab('Sun Percentage (%)')+
  ylab('Proportion of plot reconstructed')+
  scale_x_continuous(expand = c(0, 0), limits = c(-1, 101)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0.4, 0.8))
#coord_equal(ratio=1)
#coord_fixed(xlim=c(20,60))
#plot(  (paste0("P",i,"_w")))
plot(P2)
ggplot2::ggsave(
  P2,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/summary_proportion_reconstructed_sunpercentage.jpg"),
  width = 8,
  height = 8,
  units = "cm"
)

# as above but  Sun Elevation / proportion of plot reconstructed

x <- as.vector(df3$Sun)
y <- as.vector(1-df3$Empty)
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
P3 <- ggplot(df_temp) +
  geom_smooth(aes(x, y,col='darkgrey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  #add the statistics
  geom_text(aes(x=42,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.25)+
  geom_text(aes(x=42,y=0.48),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.25)+
  #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=42,y=0.46),label=equation,hjust='left', size=2.25)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0(""))+
  #theme(aspect.ratio=1)+
  xlab('Sun Elevation (degrees)')+
  ylab('Proportion of plot reconstructed')+
  scale_x_continuous(expand = c(0, 0), limits = c(20, 55)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0.4, 0.8))
#coord_equal(ratio=1)
#coord_fixed(xlim=c(20,60))
#plot(  (paste0("P",i,"_w")))
plot(P3)
ggplot2::ggsave(
  P3,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/summary_proportion_reconstructed_sun.jpg"),
  width = 8,
  height = 8,
  units = "cm"
)


#CHM vs idw

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
P4 <- ggplot(df_temp2) +
  geom_smooth(aes(x, y,col='darkgrey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.2, size = 1) +
  #  add the statistics
  geom_text(aes(x=2.5,y=1),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.25)+
  geom_text(aes(x=2.5,y=0.8),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.25)+
  geom_text(aes(x=2.5,y=0.6),label=ccc,hjust='left', size=2.25)+
  geom_text(aes(x=2.5,y=0.4),label=equation,hjust='left', size=2.25)+
  theme(text = element_text(size=36))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0(""))+
  theme(aspect.ratio=1)+
  xlab('Uninterpolated RCH (m)')+
  ylab('IDW interpolated RCH (m)')+
  coord_equal(ratio=1)+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 4)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4))+geom_abline(slope=1, intercept = 0,linetype="dashed", linewidth = 1)
#coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
#plot(  (paste0("P",i,"_w")))
plot(P4)

ggsave(
  P4,
  # filename = "/plots/test.png",
  filename = "output_data/full_model/comparison_chm_idw_v2.png",
  width = 8,
  height = 8,
  units = "cm"
)



#patchwork1 <- (P1 + P2)/(P3 + P4)
#P_All4CHMIDW<- patchwork1 + plot_annotation(tag_levels = 'A') & 
#  theme(plot.tag = element_text(size = 10))

#P1 + P2 + P3 + P4 + plot_layout(ncol = 2, widths = c(1, 1))

patchwork1 <- P1 + P2 + P3 + P4 + plot_layout(ncol = 2, widths = c(1, 1))
P_All4CHMIDW<- patchwork1 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))

P_All4CHMIDW

ggplot2::ggsave(
  P_All4CHMIDW,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/Proportion reconstructed CHM vs IDW combined figure_ABCD.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 


# filter for the diiferent Plot Genus
df_B <- filter(master_df,PlotGenus.x == "Betula")
df_U <- filter(master_df,PlotGenus.x == "Ulex europaeus")
df_S <- filter(master_df,PlotGenus.x == "Salix aurita")
df_F <- filter(master_df,PlotGenus.x == "Festuca arundinacea") 


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
  geom_point(aes(x, y), col = "black",alpha=0.3, size = 1) +
  geom_smooth(aes(x, y,col='darkgrey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  
  #  add the statistics
  geom_text(aes(x=0.0,y=0.7),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.65),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.6),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.55),label=equation,hjust='left', size=2.5)+
  theme(text = element_text(size=36))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Festuca arundinacea"))+
  theme(aspect.ratio=1)+
  xlab('Uninterpolated RCH (m)')+
  ylab('IDW interpolated RCH (m)')+
  coord_equal(ratio=1)+
  coord_fixed(xlim=c(0,0.7),ylim=c(0,0.7))+geom_abline(slope=1, intercept = 0,linetype="dashed", linewidth = 1)
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
  geom_point(aes(x, y), col='black',alpha=0.3, size = 1) +
  geom_smooth(aes(x, y,col='darkgrey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  
  #  add the statistics
  geom_text(aes(x=0.0,y=3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=2.8),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=2.6),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=2.4),label=equation,hjust='left', size=2.5)+
  theme(text = element_text(size=36))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Salix aurita"))+
  theme(aspect.ratio=1)+
  xlab('Uninterpolated RCH (m)')+
  ylab('IDW interpolated RCH (m)')+
  coord_equal(ratio=1)+
  coord_fixed(xlim=c(0,3),ylim=c(0,3))+geom_abline(slope=1, intercept = 0,linetype="dashed", linewidth = 1)
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
  geom_point(aes(x, y),col='black', alpha=0.3, size = 1) +
  geom_smooth(aes(x, y,col='darkgrey', weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  
  #  add the statistics
  geom_text(aes(x=0.0,y=3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=2.8),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=2.6),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=2.4),label=equation,hjust='left', size=2.5)+
  theme(text = element_text(size=36))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Betula"))+
  theme(aspect.ratio=1)+
  xlab('Uninterpolated RCH (m)')+
  ylab('IDW interpolated RCH (m)')+
  coord_equal(ratio=1)+
  coord_fixed(xlim=c(0,3),ylim=c(0,3))+geom_abline(slope=1, intercept = 0,linetype="dashed", linewidth = 1)
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
  geom_point(aes(x, y), col='black',alpha=0.3, size = 1) +
  geom_smooth(aes(x, y,col='darkgrey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  
  #  add the statistics
  geom_text(aes(x=0.0,y=1.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=1.4),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=1.3),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=1.2),label=equation,hjust='left', size=2.5)+
  theme(text = element_text(size=36))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Ulex europaeus"))+
  theme(aspect.ratio=1)+
  xlab('Uninterpolated RCH (m)')+
  ylab('IDW interpolated RCH (m)')+
  coord_equal(ratio=1)+
  coord_fixed(xlim=c(0,1.55),ylim=c(0,1.55))+geom_abline(slope=1, intercept = 0,linetype="dashed", linewidth = 1)
#plot(  (paste0("P",i,"_w")))
plot(PcompareU)    


Panel4<- ggarrange(PcompareF,PcompareU,PcompareB,PcompareS, ncol = 2, nrow = 2)
plot(Panel4)


patchwork2 <- PcompareF+PcompareU+PcompareB+PcompareS+ plot_layout(ncol = 2, widths = c(1, 1))
Panel4<<- patchwork2 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))

Panel4<

ggsave(
  Panel4,
  # filename = "/plots/test.png",
  filename = "output_data/full_model/chm_vs_idw_species_v4.png",
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
  geom_text(aes(x=0.0,y=1.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=1.4),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=1.3),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=1.2),label=equation,hjust='left', size=2.5)+
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
