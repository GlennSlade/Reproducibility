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

#GLMM development

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


#df_wind<- filter(df_wind,PlotGenus.x == "Betula")




# 0. Linear model Wind vs Mean CHM


df5 <- as.data.frame(df_wind)

mod2 <- lm(Mn_chm~ Wind_Av,data = df5)
summary(mod2)
PLM <-ggpredict(mod2, 
                terms = c("Wind_Av")) |>  plot()
PLM
ggplot2::ggsave(
  PLM,
  # filename = "/plots/test.png",
  filename = paste0("output_data/summary_wind_All_ggeffect_Mn_CHM.jpg"),
  width = 16,
  height = 10,
  units = "cm"
) 


mod3 <- lm(RDCHM~ Wind_Av,data = df5)
summary(mod3)
PLMRD <-ggpredict(mod3, 
                  terms = c("Wind_Av")) |>  plot()
PLMRD
ggplot2::ggsave(
  PLMRD,
  # filename = "/plots/test.png",
  filename = paste0("output_data/summary_All_ggeffect_RDCHM.jpg"),
  width = 16,
  height = 10,
  units = "cm"
) 




###1 Response = RDCHM,  Fixed effect = wind, Random effect plot

wind_model1 <-lme4::glmer(RDCHM ~  Wind_Av + PlotGenus.x +(1|plot),
                          data = df_wind,
                          family = gaussian(link = "log"))

performance::check_model(wind_model1)  # Evaluate model performance
performance::r2(wind_model1)
summary(wind_model1)

P1 <-ggpredict(wind_model1 , 
               terms = c("Wind_Av", "PlotGenus.x")) |>  plot()
P1

###1b Response = RDCHM,  Fixed effect = wind +  Plant Height, Random effect= plot

wind_model1b <-lme4::glmer(RDCHM ~  Wind_Av + PlotGenus.x + CHM_MEAN +(1|plot),
                            data = df_wind,
                            family = gaussian(link = "log"))


P1b <-ggpredict(wind_model1b , 
               terms = c("Wind_Av", "CHM_MEAN","PlotGenus.x")) |>  plot()
P1b

performance::check_model(wind_model1b)  # Evaluate model performance
performance::r2(wind_model1b)
summary(wind_model1b)



###2 Response = RDCHM,  Fixed effect = wind + Sun Elevation (filtered for sunny conditions) Random effect= plant height
df_wind_sunny <- filter(df_wind,Sun_Percent >80)

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