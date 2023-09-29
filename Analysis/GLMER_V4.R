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


df_wind <- df4 %>% dplyr::select (survey,plot,Mn_chm,Wind_Av,Sun_Elev_calc, Sun_Percent, empty_prop, PlotGenus.x,RDCHM,illumination,binary_skycode,CHM_MEAN)


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

#Basic wind model
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
          )+ theme_fancy()+theme(legend.position = c(0.3, 0.8)) 
P1a

ggplot2::ggsave(
  P1a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/linear_model_wind_summary_RDCHM.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 

# Basic wind model - chm

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

###1 Linear model wind for effect of wind on RDCHM 
mod1 <- lm(RDCHM~ Wind_Av * PlotGenus.x,data = df_wind)
summary(mod1)
P3 <-ggpredict(mod1, 
               terms = c("Wind_Av","PlotGenus.x")) |>  plot()
P3

P3a <-plot(P3) +
  labs(
    x = "Average wind speed (m/s)",
    y = "Reduction in reconstructed canopy height from Max recorded (m)",
    title = "Predicted reduction in reconstructed canopy \n heights with increased wind speed",
    colour = "Plant Species"
  )+ theme_fancy()+theme(legend.position = c(0.3, 0.8))
P3a
ggplot2::ggsave(
  P3a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/linear_model_wind_all_plant genus_RDCHM.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 
## repeat for Canopy height - effect of wind with interaction of plot genus
mod2 <- lm(Mn_chm~ Wind_Av * PlotGenus.x,data = df_wind)
summary(mod2)
P4 <-ggpredict(mod2, 
               terms = c("Wind_Av","PlotGenus.x")) |>  plot()
P4

P4a <-plot(P4) +
  labs(
    x = "Average wind speed (m/s)",
    y = "Reconstructed canopy height (m)",
    title = "Predicted effect of wind speed on reconstructed \n canopy heights ",
    colour = "Plant Species"
  )+theme_fancy()+theme(legend.position='none')
P4a
ggplot2::ggsave(
  P4a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/linear_model_wind_all_plant genus_CHM.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 

P_All2 <- ggarrange(P4a,P3a, ncol = 2, nrow = 1)
plot(P_All2)

ggplot2::ggsave(
  P_All2,
  # filename = "/plots/test.png",
  filename = paste0("output_data/Combined figure_linear_model_wind_all_plant genus_CHM_RDCHM.jpg"),
  width = 16,
  height = 10,
  units = "cm"
) 

## GLMER model with plant height ### without Plot Genus
wind_model11 <-lme4::glmer(RDCHM ~  Wind_Av + CHM_MEAN + (1|plot),
                          data = df_wind,
                          family = gaussian(link = "log"))


P11 <-ggpredict(wind_model11 , 
               terms = c("Wind_Av", "CHM_MEAN")) |>  plot() # This looks like the Plant Height classes
# are basically the Plot Genus
P11

performance::check_model(wind_model11)  # Evaluate model performance
performance::r2(wind_model11)
summary(wind_model11)

P11a <-plot(P11) +
  labs(
    x = "Average wind speed m/s",
    y = "Reduction in reconstructed canopy height from Max",
    title = "Predicted effect of wind speed on reconstructed canopy \n heights (m) - all species data",
    colour = "Plant Height (m)"
  )
P11a

ggplot2::ggsave(
  P11a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/glmer_wind_all_plant genus_RDCHM_plant height.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 

### But when you add in the Plot Genus ... it looks wrong !


wind_model1b <-lme4::glmer(RDCHM ~  Wind_Av + CHM_MEAN +(1|PlotGenus.x),
                            data = df_wind,
                            family = gaussian(link = "log"))


P1b <-ggpredict(wind_model1b , 
               terms = c("Wind_Av", "CHM_MEAN","PlotGenus.x")) |>  plot()
P1b

performance::check_model(wind_model1b)  # Evaluate model performance
performance::r2(wind_model1b)
summary(wind_model1b)

## Now trying to Sun Percent 

wind_model8 <-lme4::glmer(RDCHM ~  Wind_Av *  Sun_Percent  + (1|plot),
                          data = df_wind,
                          family = gaussian(link = "log"))

P8 <-ggpredict(wind_model8 , 
               terms = c("Wind_Av","Sun_Percent")) |>  plot()
P8

P8a <-plot(P8) +
  labs(
    x = "Average wind speed m/s",
    y = "Reduction in reconstructed canopy height from Max",
    title = "Predicted effect of Wind with interaction effect of \n illumination on reconstructed canopy \n heights (m) - all species data",
    colour = "Direct Sunlight \nPercentage of \n Survey(%)"
  )+theme_fancy()+theme(legend.position = c(0.2, 0.75))
P8a

ggplot2::ggsave(
  P8a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/glmer_wind_all_plant genus_RDCHM_Sun_percent.jpg"),
  width = 16,
  height = 16,
  units = "cm"
)

## Now trying to Sun Percent for just low sun angles

wind_model8_low <-lme4::glmer(RDCHM ~  Wind_Av *  Sun_Percent  + (1|plot),
                          data = df_wind_low_elev,
                          family = gaussian(link = "log"))

P8_low <-ggpredict(wind_model8_low , 
               terms = c("Wind_Av","Sun_Percent")) |>  plot()


P8_lowa <-plot(P8_low) +
  labs(
    x = "Average wind speed m/s",
    y = "Reduction in reconstructed canopy height from Max",
    title = "Predicted effect of Wind with interaction effect of \n illumination on reconstructed canopy heights \n all species - low sun elevations (<40) only",
    colour = "Direct Sunlight \nPercentage of \n Survey(%)"
  )+theme_fancy()+theme(legend.position = c(0.2, 0.75))
P8_lowa

ggplot2::ggsave(
  P8_lowa,
  # filename = "/plots/test.png",
  filename = paste0("output_data/glmer_wind_all_plant genus_RDCHM_Sun_percent_low_sun_elev.jpg"),
  width = 16,
  height = 16,
  units = "cm"
)

## Now trying to Sun Percent for just high sun angles


wind_model8_high <-lme4::glmer(RDCHM ~  Wind_Av *  Sun_Percent  + (1|plot),
                              data = df_wind_high_elev,
                              family = gaussian(link = "log"))

P8_high <-ggpredict(wind_model8_high , 
                   terms = c("Wind_Av","Sun_Percent")) |>  plot()


P8_higha <-plot(P8_high) +
  labs(
    x = "Average wind speed m/s",
    y = "Reduction in reconstructed canopy height from Max",
    title = "Predicted effect of Wind with interaction effect of \n illumination on reconstructed canopy heights \n all species - high sun elevations (>40) only",
    colour = "Direct Sunlight \nPercentage of \n Survey(%)"
  )+theme_fancy()+theme(legend.position = c(0.2, 0.75))
P8_higha

ggplot2::ggsave(
  P8_higha,
  # filename = "/plots/test.png",
  filename = paste0("output_data/glmer_wind_all_plant genus_RDCHM_Sun_percent_high_sun_elev.jpg"),
  width = 16,
  height = 16,
  units = "cm"
)

## Now trying to Sun Elevation 

wind_model9 <-lme4::glmer(RDCHM ~  Wind_Av *  Sun_Elev_calc  + (1|plot),
                          data = df_wind,
                          family = gaussian(link = "log"))

P9 <-ggpredict(wind_model9 , 
               terms = c("Wind_Av","Sun_Elev_calc")) |>  plot()
P9

P9a <-plot(P9) +
  labs(
    x = "Average wind speed m/s",
    y = "Reduction in reconstructed canopy height from Max",
    title = "Predicted effect of Wind with interaction effect of \n Sun Elevation on reconstructed canopy \n heights (m) - all species data",
    colour = "Sun Elevation"
  )+theme_fancy()+theme(legend.position = c(0.2, 0.8))
P9a

ggplot2::ggsave(
  P9a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/glmer_wind_all_plant genus_RDCHM_Sun_Elevation.jpg"),
  width = 16,
  height = 16,
  units = "cm"
)

# performance::check_model(wind_model9)  # Evaluate model performance
# performance::r2(wind_model9)
# summary(wind_model9)


## Now trying Sun Elevation filtered for sunny data only 

wind_model10 <-lme4::glmer(RDCHM ~  Wind_Av *  Sun_Elev_calc  + (1|plot),
                          data = df_wind_sun,
                          family = gaussian(link = "log"))

P10 <-ggpredict(wind_model10 , 
               terms = c("Wind_Av","Sun_Elev_calc")) |>  plot()
P10

P10a <-plot(P10) +
  labs(
    x = "Average wind speed m/s",
    y = "Reduction in reconstructed canopy height from Max",
    title = "Predicted effect of Wind with interaction effect of \n Sun Elevation on reconstructed canopy \n heights (m) - all species - sunny condtions only",
    colour = "Sun Elevation"
  )+theme_fancy()+theme(legend.position = c(0.2, 0.8))
P10a

ggplot2::ggsave(
  P10a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/glmer_wind_all_plant genus_RDCHM_Sun_Elevation_sunny conditions.jpg"),
  width = 16,
  height = 16,
  units = "cm"
)

P_All4_sun <- ggarrange(P8a,P8_lowa,P9a,P10a, ncol = 2, nrow = 2)
plot(P_All4_sun)

ggplot2::ggsave(
  P_All4_sun,
  # filename = "/plots/test.png",
  filename = paste0("output_data/glmer_wind_illumination_sun_elev_interaction_4_plots.jpg"),
  width = 16,
  height = 16,
  units = "cm"
)


performance::check_model(wind_model10)  # Evaluate model performance
performance::r2(wind_model10)
summary(wind_model10)



# Linear model Wind vs Mean CHM Betula only
### Towards a  model for Betula
###

df_windb <- as.data.frame(df_windb)

mod12 <- lm(Mn_chm~ Wind_Av,data = df_windb)
summary(mod12)
PLM <-ggpredict(mod12, 
                terms = c("Wind_Av"))# |>  plot()
PLM

PLMa <-plot(PLM, colors = "green") +
  labs(
    x = "Average wind speed m/s",
    y = "Reconstructed canopy height",
    title = "Predicted effect of Wind on reconstructed canopy \n heights (m) - Betula"
  )+theme_fancy()
PLMa
ggplot2::ggsave(
  PLMa,
  # filename = "/plots/test.png",
  filename = paste0("output_data/summary_wind_betula_ggeffect_Mn_CHM.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 


mod13 <- lm(RDCHM~ Wind_Av,data = df_windb)
summary(mod13)
PLMRD <-ggpredict(mod13, 
                  terms = c("Wind_Av"))# |>  plot()
PLMRD
PLMRDa <-plot(PLMRD, colors="green") +
  labs(
    x = "Average wind speed m/s",
    y = "Reduction in reconstructed canopy height from Max recorded (m)",
    title = "Predicted effect of Wind on reconstructed canopy \n heights - Betula"
  )+theme_fancy()
PLMRDa
ggplot2::ggsave(
  PLMRDa,
  # filename = "/plots/test.png",
  filename = paste0("output_data/summary_wind_betula_ggeffect_RDCHM.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 

P_All2_betula <- ggarrange(PLMa,PLMRDa, ncol = 2, nrow = 1)
plot(P_All2_betula)

ggplot2::ggsave(
  P_All2_betula,
  # filename = "/plots/test.png",
  filename = paste0("output_data/linear_wind_basic_betula.jpg"),
  width = 16,
  height = 10,
  units = "cm"
)


## Now just for Betula Wind and Plant Height

wind_model14 <-lme4::glmer(RDCHM ~  Wind_Av * CHM_MEAN +(1|plot),
                           data = df_windb,
                           family = gaussian(link = "log"))


P14 <-ggpredict(wind_model14 , 
                terms = c("Wind_Av", "CHM_MEAN"))# |>  plot()
P14a <-plot(P14) +
  labs(
    x = "Average wind speed m/s",
    y = "Reduction in reconstructed canopy height from Max recorded",
    title = "Predicted effect of wind speed on Betula reconstructed canopy heights \n interaction with plant height",
    colour = "Plant Height (m)"
  )+theme_fancy()+theme(legend.position = c(0.2, 0.8))
P14a 
ggplot2::ggsave(
  P14a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/glmer_wind_plant height_betula_RDCHM.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 


performance::check_model(wind_model1b)  # Evaluate model performance
performance::r2(wind_model1b)
summary(wind_model1b)



## Now just for Betula Wind and Sun Percent
wind_model8b <-lme4::glmer(RDCHM ~  Wind_Av *  Sun_Percent  + (1|plot),
                          data = df_windb,
                          family = gaussian(link = "log"))

P8b <-ggpredict(wind_model8b , 
               terms = c("Wind_Av","Sun_Percent")) |>  plot()
P8b
P8ba <-plot(P8b) +
  labs(
    x = "Average wind speed m/s",
    y = "Reduction in reconstructed canopy height from Max recorded",
    title = "Predicted effect of Wind with interaction effect of \n illumination on reconstructed canopy \n heights (m) - Betula",
    colour = "Direct Sunlight \nPercentage of \n Survey(%)"
  )
P8ba
ggplot2::ggsave(
  P8ba,
  # filename = "/plots/test.png",
  filename = paste0("output_data/glmer_wind_sun percent_betula_RDCHM.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 

performance::check_model(wind_model8b)  # Evaluate model performance
performance::r2(wind_model8b)
summary(wind_model8b)


###2 Just for Betula Response = RDCHM,  Fixed effect = wind + Sun Elevation  Random effect= plant height

df_wind_sunny <- filter(df_windb,Sun_Percent >80)

wind_model2 <-lme4::glmer(RDCHM ~  Wind_Av +  Sun_Elev_calc  + (1|plot),
                          data = df_wind_sunny,
                          family = gaussian(link = "log"))

P2 <-ggpredict(wind_model2 , 
               terms = c("Wind_Av", "Sun_Elev_calc")) |>  plot()
P2

performance::check_model(wind_model2)  # Evaluate model performance
performance::r2(wind_model2)
summary(wind_model2)

####(filtered for sunny conditions)
df_wind_sunny <- filter(df_windb,Sun_Percent >80)

wind_model2 <-lme4::glmer(RDCHM ~  Wind_Av +  Sun_Elev_calc  + (1|plot),
                          data = df_wind_sunny,
                          family = gaussian(link = "log"))

P2 <-ggpredict(wind_model2 , 
                terms = c("Wind_Av", "Sun_Elev_calc")) |>  plot()
P2

performance::check_model(wind_model2)  # Evaluate model performance
performance::r2(wind_model2)
summary(wind_model2)

###3 Response = RDCHM,  Fixed effect = wind + Sun Elevation (filtered for sunny conditions) + Plant Height, Random effect= plot

wind_model3 <-lme4::glmer(RDCHM ~  Wind_Av +  Sun_Elev_calc  + CHM_MEAN +(1|plot),
                          data = df_wind_sunny,
                          family = gaussian(link = "log"))

P3 <-ggpredict(wind_model3 , 
               terms = c("Wind_Av", "Sun_Elev_calc","CHM_MEAN")) |>  plot()
P3

performance::check_model(wind_model3)  # Evaluate model performance
performance::r2(wind_model3)
summary(wind_model3)

###4 Response = RDCHM,  Fixed effect = wind + Sun Elevation (all cloud conditions) + Plant Height, Random effect= plot
### worth looking at as time of day may on its own be influential (for example of wind speed was colinear)

wind_model4 <-lme4::glmer(RDCHM ~  Wind_Av +  Sun_Elev_calc  + CHM_MEAN +(1|plot),
                          data = df_wind,
                          family = gaussian(link = "log"))

P4 <-ggpredict(wind_model4 , 
               terms = c("Wind_Av", "Sun_Elev_calc","CHM_MEAN")) |>  plot()
P4

performance::check_model(wind_model4)  # Evaluate model performance
performance::r2(wind_model4)
summary(wind_model4)

###5 Response = RDCHM,  Fixed effect = wind + Sun Elevation (all cloud conditions) + Plant Height, Random effect= illumination (sun or no sun- or partial)

wind_model5 <-lme4::glmer(RDCHM ~  Wind_Av +  Sun_Elev_calc  + CHM_MEAN +(1|illumination),
                          data = df_wind,
                          family = gaussian(link = "log"))
P5 <-ggpredict(wind_model5 , 
               terms = c("Wind_Av", "Sun_Elev_calc","CHM_MEAN")) |>  plot()
P5


performance::check_model(wind_model5)  # Evaluate model performance
performance::r2(wind_model5)
summary(wind_model5)



### 6 Wind model fixed effect wind interaction effect sun elevation - sunny conditions only



wind_model6 <-lme4::glmer(RDCHM ~  Wind_Av *  Sun_Elev_calc  + (1|plot),
                          data = df_wind_sunny,
                          family = gaussian(link = "log"))

performance::check_model(wind_model6)  # Evaluate model performance
performance::r2(wind_model6)
summary(wind_model6)

P6 <-ggpredict(wind_model6 , 
               terms = c("Wind_Av","Sun_Elev_calc")) |>  plot()
P6

### 7 Wind model fixed effect wind interaction effect illumination

wind_model7 <-lme4::glmer(RDCHM ~  Wind_Av *  Sun_Percent  + (1|plot),
                          data = df_wind,
                          family = gaussian(link = "log"))

performance::check_model(wind_model7)  # Evaluate model performance
performance::r2(wind_model7)
summary(wind_model7)

P7 <-ggpredict(wind_model7 , 
               terms = c("Wind_Av","Sun_Percent")) |>  plot()
P7

### 8 Wind model fixed effect wind and illumination


wind_model8 <-lme4::glmer(RDCHM ~  Wind_Av +  Sun_Percent  + (1|plot),
                          data = df_wind,
                          family = gaussian(link = "log"))

P8 <-ggpredict(wind_model8 , 
               terms = c("Wind_Av","Sun_Percent")) |>  plot()
P8
performance::check_model(wind_model8)  # Evaluate model performance
performance::r2(wind_model8)
summary(wind_model8)

### 9 Wind model fixed effect wind and illumination interacting with sun elevation


wind_model9 <-lme4::glmer(RDCHM ~  Wind_Av +  Sun_Percent * Sun_Elev_calc  + (1|plot),
                          data = df_wind,
                          family = gaussian(link = "log"))

P9 <-ggpredict(wind_model9 , 
               terms = c("Wind_Av","Sun_Percent","Sun_Elev_calc")) |>  plot()
P9
performance::check_model(wind_model9)  # Evaluate model performance
performance::r2(wind_model9)
summary(wind_model9)


### 10 Sun model fixed effect sun elev and illumination interacting with sun elevation

mod10 <- lm(RDCHM~ Sun_Elev_calc,data = df5)
summary(mod10)
PLMRD <-ggpredict(mod10, 
                  terms = c("Sun_Elev_calc")) |>  plot()
PLMRD



sun_model10 <-lme4::glmer(RDCHM ~  Sun_Elev_calc *  Sun_Percent    + (1|plot),
                          data = df_wind,
                          family = gaussian(link = "log"))

P10 <-ggpredict(sun_model10 , 
               terms = c("Sun_Elev_calc", "Sun_Percent")) |>  plot()
P10
performance::check_model(sun_model10)  # Evaluate model performance
performance::r2(sun_model10)
summary(sun_model10)
