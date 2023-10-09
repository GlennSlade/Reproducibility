# Develpoment script for GLMM

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

# df %>% mutate(g = case_when(a == 2 | a == 5 | a == 7 | (a == 1 & b == 4) ~ 2,
#                             a == 0 | a == 1 | a == 4 | a == 3 |  c == 4 ~ 3,
#                             TRUE ~ NA_real_))


df_wind <- df4 %>% dplyr::select (survey,RDCHM_Percent,plot,Mn_chm,Wind_Av,Sun_Elev_calc, Sun_Percent, empty_prop, PlotGenus.x,RDCHM,illumination,binary_skycode,CHM_MEAN)


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

#Basic wind linear model
df_wind<- as.data.frame(df_wind)
mod <- lm(RDCHM~ Wind_Av ,data = df_wind)
#summary(mod)
P1 <-ggpredict(mod, 
               terms = c("Wind_Av"))# |>  plot()


P1a <-plot(P1, colors = "orange") +
  labs(
    x = "Average wind speed (m/s)",
    y = "Reduction in reconstructed canopy height from Max (m)",
    title = "Predicted reduction in reconstructed canopy \n heights with increased wind speed \n all data"
  ) + scale_y_reverse()+
  theme_fancy()+theme(legend.position = c(0.3, 0.8)) 
P1a

ggplot2::ggsave(
  P1a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/linear_model_wind_summary_RDCHM_reverse.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 

# Basic wind linear model - chm

modchm <- lm(Mn_chm~ Wind_Av ,data = df_wind)
#summary(mod)
P1chm <-ggpredict(modchm, 
                  terms = c("Wind_Av"))# |>  plot()


P1chm2 <-plot(P1chm, colors = "orange") +
  labs(
    x = "Average wind speed (m/s)",
    y = "Reduction in reconstructed canopy height from Max (m)",
    title = "Predicted reduction in reconstructed canopy \n heights with increased wind speed \n all data"
  )+ theme_fancy()+theme(legend.position = c(0.3, 0.8)) 
P1chm2

ggplot2::ggsave(
  P1chm2,
  # filename = "/plots/test.png",
  filename = paste0("output_data/linear_model_wind_summary_CHM.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 

P_All2_basic <- ggarrange(P1chm2,P1a, ncol = 2, nrow = 1)
plot(P_All2_basic)

ggplot2::ggsave(
  P_All2_basic,
  # filename = "/plots/test.png",
  filename = paste0("output_data/Combined figure_basic linear_model_CHM_RDCHM.jpg"),
  width = 16,
  height = 10,
  units = "cm"
) 

##11. Full GLMER model for Wind with Plot Genus as fixed effect and Sun Elevation Sun percent and plot as random effects

wind_model11 <-lme4::glmer(RDCHM ~  Wind_Av * PlotGenus.x + (1+Sun_Elev_calc+Sun_Percent|plot),
                           data = df_wind,
                           family = gaussian(link = "log"),
                           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

(P11 <-ggpredict(wind_model11 , 
                 terms = c("Wind_Av","PlotGenus.x")) |>  plot())

P11a<-plot(P11) +
  labs(
    x = "Average wind speed (m/s)",
    y = "Reduction in reconstructed canopy height from Max (m)",
    title = "",
    fill = "Plot Genus"
      )+ scale_y_reverse()+ theme_fancy()+theme(legend.position = c(0.2, 0.4))+
  theme(legend.title = element_blank())+scale_color_viridis(discrete = TRUE) 
P11a

ggplot2::ggsave(
  P11a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/glmer_RDCHM_Wind.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 

summary(wind_model11)

##12. Full GLMER model for RDCHM as a function of Illumination with Plot Genus as fixed effect and Wind Av and plot as random effects



sun_model12<-lme4::glmer(RDCHM ~   Sun_Elev_calc+Sun_Percent * PlotGenus.x + (1+Wind_Av|plot),
                             data = df_wind,
                             family = gaussian(link = "log"))
P12 <-ggpredict(sun_model12 , 
                terms = c("Sun_Elev_calc","Sun_Percent [0:100, by=50]","PlotGenus.x")) |>  plot()
P12

P12a<-plot(P12) +
  labs(
    x = "Sun Elevation (degrees)",
    y = "Reduction in reconstructed canopy height from Max (m)",
    title = "",
    fill = "Sun Percent"
  )+ scale_y_reverse()+ theme_fancy()+theme(legend.position = c(0.1, 0.1))
P12a

summary(sun_model12)

ggplot2::ggsave(
  P12a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/glmer_RDCHM_Illumination.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 
