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
survey <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/Survey_Data.xlsx")
# Import reconstructed canopy height estimates for each plot and each survey  
CHM <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/plot_chm_metrics_temp61.xlsx")# needs to be updated with latest survey data
# Import plot data: species, plot measurements etc
plot <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/Plot_Data.xlsx")

#
plot_statistics <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_plot_statistics.xlsx")# script needs to be run to generate this. 
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


df_wind <- df4 %>% dplyr::select (survey,plot,Mn_chm,Wind_Av,Sun_Elev_calc, Sun_Percent, empty_prop, PlotGenus.x,RDCHM,illumination,binary_skycode)


df_wind <- df_wind %>% na.omit(df_wind)# get rid of any na rows belonging to surveys not processed yet


df_wind<- filter(df_wind,PlotGenus.x == "Betula")





# 1. Fixed Effect Wind_Av Random effect Plot - Relative difference of CH to Max


wind_model1 <-lme4::glmer(RDCHM ~  Wind_Av  +(1|plot),
                         data = df_wind,
                         family = gaussian(link = "log"))

performance::check_model(wind_model1)  # Evaluate model performance
summary(wind_model1)  # See model summary

# 2. Fixed Effect Wind_Av Random effect Plot - Mean CH

wind_model2 <-lme4::glmer( Mn_chm ~  Wind_Av  +(1|plot),
                          data = df_wind,
                          family = gaussian(link = "log"))

performance::check_model(wind_model2)  # Evaluate model performance
summary(wind_model2)  # See model summary



# 3. Fixed effect wind and sun elevation (all cloud conditions) - random effect plot number

wind_model3 <-lme4::glmer(Mn_chm ~  Wind_Av +  Sun_Elev_calc +(1|plot),
                          data = df_wind,
                          family = gaussian(link = "log"))

performance::check_model(wind_model3)  # Evaluate model performance
summary(wind_model3)

# model nearly unidentifiable large eigenvalue

# 4. Fixed effect wind and sun elevation (filtered for sunny conditions) - random effect plot number

df_wind_sunny <- filter(df_wind,Sun_Percent >25)

wind_model4 <-lme4::glmer(Mn_chm ~  Wind_Av +  Sun_Elev_calc +(1|plot),
                          data = df_wind_sunny,
                          family = gaussian(link = "log"))

performance::check_model(wind_model4)  # Evaluate model performance
summary(wind_model4)

# model nearly unidentifiable large eigenvalue
# model nearly unidentifiable large eigenvalue

# 5. Fixed effect wind and sun elevation (filtered for no sun conditions) - random effect plot number

df_wind_nosun <- filter(df_wind, Sun_Percent <25)

wind_model5 <-lme4::glmer(Mn_chm ~  Wind_Av +  Sun_Elev_calc +(1|plot),
                          data = df_wind_nosun,
                          family = gaussian(link = "log"))

performance::check_model(wind_model5)  # Evaluate model performance
summary(wind_model5)

# model nearly unidentifiable large eigenvalue


#6. randomised slope - intercept of sun elev for each plot - all cloud conditions
wind_model6 <-lme4::glmer(Mn_chm ~ Wind_Av +(1+Sun_Elev_calc |plot),
                          data = df_wind
                          ,family = Gamma(link = "identity")
)


performance::check_model(wind_model6)  # Evaluate model performance
summary(wind_model6)
marginal_means(wind_model6, variables = c( "Wind_Av"))
predictions(wind_model6,newdata = data_grid())
slopes(wind_model6)
avg_slopes(wind_model6)
plot_slopes(wind_model6,variables = c( "Wind_Av") )

# 7. Fixed effect wind with sun elevation (filtered for sunny conditions) - random effect plot number

df_wind_sunny <- filter(df_wind,Sun_Percent >25)

wind_model7 <-lme4::glmer(Mn_chm ~  Wind_Av *  Sun_Elev_calc +(1|plot),
                          data = df_wind_sunny,
                          family = gaussian(link = "log"))

performance::check_model(wind_model7)  # Evaluate model performance
summary(wind_model7)

#7. 
