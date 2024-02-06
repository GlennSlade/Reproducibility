# Develpoment script for looking at why plot reconstructions are lower
# is it just because the tallest features are omitted ? or are they reconstructed lower?

library(tidyverse)
library(viridis)
library(readxl)
library(writexl)
library(DescTools)
library(lme4)
library(nlme)
library(arm)
library(dplyr)
library(ggeffects)
library(splines)
library(emmeans)
library(qqplotr)
library(marginaleffects)
library(see)
library (cowplot)
library(ggpubr)

#GLMM development

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

#----1. Data Prep------

#read in data

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

# Fudge for now - GLMER gamma does not support non positive values which includes zero
# adding a small amount to RDCHM to ensure all values non zero
df4 <- df4 %>% mutate(RDCHM = RDCHM+0.00001)
df4 <- df4 %>% mutate(RDCHM_Percent = (RDCHM/CHM_MAX)* 100)# Difference as a percentage of height of plant



# 3 class illumination  0=  diffuse light no shadows 1= varied shadow mixed conditions 2= direct light strong shadows 
df4 <- df4 %>% mutate(illumination = case_when(Sun_Percent <= 20 ~ 0,
                                               (Sun_Percent > 20 &  Sun_Percent <80 )~ 1, Sun_Percent >= 80 ~ 2,
                                               TRUE ~ NA_real_))

# binary sky code 1= sun 0 = cloudy
df4 <- df4  %>% mutate(binary_skycode = case_when(Sky_Code <= 5 ~ 1,
                                                  Sky_Code > 5 ~ 0,
                                                  TRUE ~ NA_real_))

# df %>% mutate(g = case_when(a == 2 | a == 5 | a == 7 | (a == 1 & b == 4) ~ 2,
#                             a == 0 | a == 1 | a == 4 | a == 3 |  c == 4 ~ 3,
#                             TRUE ~ NA_real_))


df_wind <- df4 %>% dplyr::select (survey,RDCHM_Percent,plot,Max_chm,Sd_chm,Mn_chm,Wind_Av,Sun_Elev_calc, Sun_Percent, empty_prop, PlotGenus.x,RDCHM,illumination,binary_skycode,CHM_MEAN)


df_wind <- df_wind %>% na.omit(df_wind)# get rid of any na rows belonging to surveys not processed yet

# Produce DF for each Plot Genus
df_windb<- filter(df_wind,PlotGenus.x == "Betula")
df_winds<- filter(df_wind,PlotGenus.x == "Salix aurita")
df_windu<- filter(df_wind,PlotGenus.x == "Ulex europaeus")
df_windf<- filter(df_wind,PlotGenus.x == "Festuca arundinacea")
df_wind_sun<- filter(df_wind,Sun_Percent >50)
df_wind_cloud <- filter(df_wind,Sun_Percent <50)
df_wind_low_elev <- filter(df_wind,Sun_Elev_calc <40)
df_wind_high_elev <- filter(df_wind,Sun_Elev_calc >40)

#### ALL SPECIES

#Basic wind model - Max
df_wind<- as.data.frame(df_wind)
mod <- lm(Max_chm~ Wind_Av ,data = df_wind)
#summary(mod)
P1 <-ggpredict(mod, 
               terms = c("Wind_Av"))# |>  plot()


P1a <-plot(P1, colors = "orange") +
  labs(
    x = "Average wind speed (m/s)",
    y = " Max CHM reconstructed (m)",
    title = "Predicted effect of wind on max canopy  \n all data"
  )+ theme_fancy()+theme(legend.position = c(0.3, 0.8)) 
P1a


mod2 <- lm(Max_chm~ Wind_Av * PlotGenus.x,data = df_wind)
summary(mod2)
P4 <-ggpredict(mod2, 
               terms = c("Wind_Av","PlotGenus.x")) |>  plot()
P4
P4a <-plot(P4) +
  labs(
    x = "Average wind speed (m/s)",
    y = "Max Reconstructed canopy height (m)",
    title = "Predicted effect of wind speed on Maximum reconstructed \n canopy heights ",
    colour = "Plant Species"
  )+theme_fancy()+theme(legend.position = c(0.3, 0.6))
P4a

ggplot2::ggsave(
  P4a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/linear_model_wind_Max_CHM.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 


#Basic wind model SD
df_wind<- as.data.frame(df_wind)
mod <- lm(Sd_chm~ Wind_Av ,data = df_wind)
#summary(mod)
P1 <-ggpredict(mod, 
               terms = c("Wind_Av"))# |>  plot()


P1a <-plot(P1, colors = "orange") +
  labs(
    x = "Average wind speed (m/s)",
    y = " SD CHM reconstructed (m)",
    title = "Predicted effect of wind on SD of canopy reconstruction \n all data"
  )+ theme_fancy()+theme(legend.position = c(0.3, 0.8)) 
P1a


mod2 <- lm(Sd_chm~ Wind_Av * PlotGenus.x,data = df_wind)
summary(mod2)
P4 <-ggpredict(mod2, 
               terms = c("Wind_Av","PlotGenus.x")) |>  plot()
P4
P4a <-plot(P4) +
  labs(
    x = "Average wind speed (m/s)",
    y = "SD Reconstructed canopy height (m)",
    title = "Predicted effect of wind speed on SD of reconstructed \n canopy heights ",
    colour = "Plant Species"
  )+theme_fancy()+theme(legend.position = c(0.3, 0.3))
P4a

ggplot2::ggsave(
  P4a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/linear_model_wind_SD_CHM.jpg"),
  width = 16,
  height = 16,
  units = "cm"
)
# linear model of empty cells against RDCHM
mod3 <- lm(RDCHM~  empty_prop * PlotGenus.x,data = df_wind)

summary(mod3)
P5 <-ggpredict(mod3, 
               terms = c("empty_prop","PlotGenus.x")) |>  plot()
P5
P5a <-plot(P5) +
  labs(
    x = "Proportion of cell empty",
    y = "RDCHM (m)",
    title = "Predicted effect of Proportion of plot without reconstructed points on \n mean canopy heights ",
    colour = "Plant Species"
  )+theme_fancy()+theme(legend.position = c(0.2, 0.6))
P5a

ggplot2::ggsave(
  P5a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/linear_model_Empty_RDCHM.jpg"),
  width = 16,
  height = 16,
  units = "cm"
)
# as above but Percnetage reduction in Canopy height

mod4 <- lm(RDCHM_Percent~  empty_prop,data = df_windu)

summary(mod4)
P6 <-ggpredict(mod4, 
               terms = c("empty_prop")) |>  plot()
P6
P6a <-plot(P6) +
  labs(
    x = "Proportion of cell empty",
    y = "Percentage reduction in Canopy Height(m)",
    title = "Predicted effect of Proportion of plot without reconstructed points on \n percentage reduction in heights ",
    colour = "Plant Species"
  )+theme_fancy()+theme(legend.position = c(0.2, 0.6))
P6a

ggplot2::ggsave(
  P6a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/linear_model_Empty_RDCHM_percent.jpg"),
  width = 16,
  height = 16,
  units = "cm"
)

#Plots of empty cells 

#Betula

df3 <- df_windb

x <- as.vector(1-df3$empty_prop)
y <- as.vector(df3$RDCHM)
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
  #geom_text(aes(x=20,y=0.55),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  #geom_text(aes(x=20,y=0.53),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  #geom_text(aes(x=20,y=0.51),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Comparison of proportion of plot reconstructed \n against the reduction in height - Betula"))+
  #theme(aspect.ratio=1)+
  xlab('Proportion of plot reconstructed')+
  ylab('Reduction in height')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(20,60))
#plot(  (paste0("P",i,"_w")))
plot(P1)

ggplot2::ggsave(
  P1,
  # filename = "/plots/test.png",
  filename = paste0("output_data/plot_reconstructed_RDCHM_Betula.jpg"),
  width = 16,
  height = 16,
  units = "cm"
)

# Salix
df3 <- df_winds

x <- as.vector(1-df3$empty_prop)
y <- as.vector(df3$RDCHM)
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
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  #add the statistics
  #geom_text(aes(x=20,y=0.55),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  #geom_text(aes(x=20,y=0.53),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  #geom_text(aes(x=20,y=0.51),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Comparison of proportion of plot reconstructed \n against the reduction in height - Salix"))+
  #theme(aspect.ratio=1)+
  xlab('Proportion of plot reconstructed')+
  ylab('Reduction in height')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(20,60))
#plot(  (paste0("P",i,"_w")))
plot(P2)

ggplot2::ggsave(
  P2,
  # filename = "/plots/test.png",
  filename = paste0("output_data/plot_reconstructed_RDCHM_Salix.jpg"),
  width = 16,
  height = 16,
  units = "cm"
)


# Ulex
df3 <- df_windu

x <- as.vector(1-df3$empty_prop)
y <- as.vector(df3$RDCHM)
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
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  #add the statistics
  #geom_text(aes(x=20,y=0.55),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  #geom_text(aes(x=20,y=0.53),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  #geom_text(aes(x=20,y=0.51),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Comparison of proportion of plot reconstructed \n against the reduction in height - Ulex"))+
  #theme(aspect.ratio=1)+
  xlab('Proportion of plot reconstructed')+
  ylab('Reduction in height')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(20,60))
#plot(  (paste0("P",i,"_w")))
plot(P3)

ggplot2::ggsave(
  P3,
  # filename = "/plots/test.png",
  filename = paste0("output_data/plot_reconstructed_RDCHM_Ulex.jpg"),
  width = 16,
  height = 16,
  units = "cm"
)

# Festuca

df3 <- df_windf

x <- as.vector(1-df3$empty_prop)
y <- as.vector(df3$RDCHM)
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
P4 <- ggplot(df_temp) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  #add the statistics
  #geom_text(aes(x=20,y=0.55),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  #geom_text(aes(x=20,y=0.53),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  #geom_text(aes(x=20,y=0.51),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Comparison of proportion of plot reconstructed \n against the reduction in height - Festuca"))+
  #theme(aspect.ratio=1)+
  xlab('Proportion of plot reconstructed')+
  ylab('Reduction in height')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(20,60))
#plot(  (paste0("P",i,"_w")))
plot(P4)

ggplot2::ggsave(
  P4,
  # filename = "/plots/test.png",
  filename = paste0("output_data/plot_reconstructed_RDCHM_Festuca.jpg"),
  width = 16,
  height = 16,
  units = "cm"
)
# All species

df3 <- df_wind

x <- as.vector(1-df3$empty_prop)
y <- as.vector(df3$RDCHM)
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
P5 <- ggplot(df_temp) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  #add the statistics
  #geom_text(aes(x=20,y=0.55),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  #geom_text(aes(x=20,y=0.53),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  #geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  #geom_text(aes(x=20,y=0.51),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Comparison of proportion of plot reconstructed \n against the reduction in height - all species"))+
  #theme(aspect.ratio=1)+
  xlab('Proportion of plot reconstructed')+
  ylab('Reduction in height')
#coord_equal(ratio=1)
#coord_fixed(xlim=c(20,60))
#plot(  (paste0("P",i,"_w")))
plot(P5)

ggplot2::ggsave(
  P5,
  # filename = "/plots/test.png",
  filename = paste0("output_data/plot_reconstructed_RDCHM_all_species.jpg"),
  width = 16,
  height = 16,
  units = "cm"
)
