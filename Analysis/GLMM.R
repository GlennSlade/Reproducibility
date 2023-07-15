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


#GLMM development

#----1. Data Prep------

#read in data

# Import environmental data for each survey: wind sun elevation etc
survey <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/Survey_Data.xlsx")
# Import reconstructed canopy height estimates for each plot and each survey  
CHM <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/plot_chm_metrics_temp30.xlsx")# needs to be updated with latest survey data
# Import plot data: species, plot measurements etc
plot <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/Plot_Data.xlsx")

#
plot_statistics <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_plot_statistics.xlsx")# script needs to be run to generate this. 
# Select Mean of all plot heights (proxy for plant height) and % Variance
statistics <- plot_statistics%>% dplyr::select (plot,CHM_MEAN,CHM_VAR_PERC)

# Join the survey and chm tables - for each plot CHM measurement you now have sun elevation wind speed etc 
survey_df <- full_join(CHM, survey, by = "survey")
# Now join with plot characteristics 
master<- full_join(survey_df,plot, by = "plot")

# join with plot statistics
master_df <- full_join(master,statistics, by = "plot")

#calculate empty cells

df4 <- master_df %>% mutate(empty = Ct_dtm - Ct_chm)# number of empty cells
df4 <- df4 %>% mutate(empty_prop = empty/Ct_dtm)# proportion of plot that is empty cells

# simplified data frame on key stats

df3 <- df4 %>% dplyr::select (survey,plot,Mn_chm,Wind_Av,Sun_Elev_calc, Sun_Percent, empty_prop, PlotGenus, Sky_Code,Wind_SD,CHM_MEAN,CHM_VAR_PERC)

# summarise by survey and plot genus (giving average canopy height for each species on each survey) if required

df2<-summarise(group_by(df3, survey,PlotGenus),
               CHM=mean(Mn_chm),
               Wind=mean(Wind_Av),Sun = mean(Sun_Elev_calc), SunP = mean(Sun_Percent), Empty = mean(empty_prop), Wind_SD =mean(Wind_SD), Sky_Code = mean(Sky_Code),Plant_Height= mean (CHM_MEAN), Plot_Var = mean (CHM_VAR_PERC))              

data <- df2 %>% na.omit(df2)# get rid of any na rows belonging to surveys not processed yet


# Variables 
# Mean Canopy Height - Response 
# Wind speed
# Sun elevation (when its sunny) see below
# Percentage sunshine during survey
# Sky code - cloud cover
# Species - PlotGenus
# Plant height - (mean of al CHM for each plot)



#testing the random effect LMM
#a first model
mod1<-lme(CHM~Wind+PlotGenus,data=data,random=~1|Sun,method="REML")
mod1
plot(mod1)



# mixed model with random effect
data_all <- df3 %>% na.omit(df3)# get rid of any na rows belonging to surveys not processed yet

data_all

mixed.lmer <- lmer(Mn_chm ~ Wind_Av + (1|PlotGenus), data = data_all)
summary(mixed.lmer)
plot (mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!

pred.mm <- ggpredict(mixed.lmer, terms = c("Wind_Av"))  # this gives overall predictions for the model
P_lmer <- (ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = data_all,                      # adding the raw data (scaled values)
               aes(x = Wind_Av, y = Mn_chm, colour = PlotGenus)) + 
    labs(x = "Mean wind speed (m/s)", y = "Mean CHM", 
         title = "Wind speed effects canopy height reconstruction") + 
    theme_minimal()
)
P_lmer

ggpredict(mixed.lmer, terms = c("Wind_Av", "PlotGenus"), type = "re") %>% 
  plot() +
  labs(x = "Mean wind speed (m/s)", y = "Mean CHM", 
       title = "Wind speed effects canopy height reconstruction") +  theme_minimal()


# Repeat on summarised data


mixed.lmer2 <- lmer(CHM ~ Wind + (1|PlotGenus), data = data)
summary(mixed.lmer2)
plot (mixed.lmer2)
qqnorm(resid(mixed.lmer2))
qqline(resid(mixed.lmer2))  # points fall nicely onto the line - good!

pred.mm <- ggpredict(mixed.lmer2, terms = c("Wind"))  # this gives overall predictions for the model
P_lmer2 <- (ggplot(pred.mm) + 
             geom_line(aes(x = x, y = predicted)) +          # slope
             geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                         fill = "lightgrey", alpha = 0.5) +  # error band
             geom_point(data = data,                      # adding the raw data (scaled values)
                        aes(x = Wind, y = CHM, colour = PlotGenus)) + 
             labs(x = "Mean wind speed (m/s)", y = "Mean CHM", 
                  title = "Wind speed effects canopy height reconstruction") + 
             theme_minimal()
)
P_lmer2

ggpredict(mixed.lmer2, terms = c("Wind", "PlotGenus"), type = "re") %>% 
  plot() +
  labs(x = "Mean wind speed (m/s)", y = "Mean CHM", 
       title = "Wind speed effects canopy height reconstruction") +  theme_minimal()

df_B <- filter(df3,PlotGenus == "Betula")
df_U <- filter(df3,PlotGenus == "Ulex europaeus")
df_S <- filter(df3,PlotGenus == "Salix aurita")
df_F <- filter(df3,PlotGenus == "Festuca arundinacea") 

data <- df_B

mixed.lmer <- lmer(Mn_chm ~ Wind_Av + (1|Sky_Code), data = data)
summary(mixed.lmer)
plot (mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!

pred.mm <- ggpredict(mixed.lmer, terms = c("Wind_Av"))  # this gives overall predictions for the model
P_lmer <- (ggplot(pred.mm) + 
             geom_line(aes(x = x, y = predicted)) +          # slope
             geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                         fill = "lightgrey", alpha = 0.5) +  # error band
             geom_point(data = data,                      # adding the raw data (scaled values)
                        aes(x = Wind_Av, y = Mn_chm, colour = PlotGenus)) + 
             labs(x = "Mean wind speed (m/s)", y = "Mean CHM", 
                  title = "Wind speed effects canopy height reconstruction") + 
             theme_minimal()
)
P_lmer

ggpredict(mixed.lmer, terms = c("Wind_Av", "Sky_Code", type = "re")) %>% 
  plot() +
  labs(x = "Mean wind speed (m/s)", y = "Mean CHM", 
       title = "Wind speed effects canopy height reconstruction") +  theme_minimal()



# Plot 49

df_49 <- filter(df3,plot == "49")

data <- df_49

mixed.lmer <- lmer(Mn_chm ~ Wind_Av + (1|Sky_Code), data = data)
summary(mixed.lmer)
plot (mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line? - good!

pred.mm <- ggpredict(mixed.lmer, terms = c("Wind_Av"))  # this gives overall predictions for the model
P_lmer <- (ggplot(pred.mm) + 
             geom_line(aes(x = x, y = predicted)) +          # slope
             geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                         fill = "lightgrey", alpha = 0.5) +  # error band
             geom_point(data = data,                      # adding the raw data (scaled values)
                        aes(x = Wind_Av, y = Mn_chm, colour = PlotGenus)) + 
             labs(x = "Mean wind speed (m/s)", y = "Mean CHM", 
                  title = "Plot 49 - Wind speed effects canopy height reconstruction") + 
             theme_minimal()
)
P_lmer

ggpredict(mixed.lmer, terms = c("Wind_Av", "Sky_Code", type = "re")) %>% 
  plot() +
  labs(x = "Mean wind speed (m/s)", y = "Mean CHM", 
       title = "Wind speed effects canopy height reconstruction") +  theme_minimal()

