# Script for GLMER for proportion of plot reconstructed

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
library(patchwork)

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
#      legend.position = c(0.9, 0.9),
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
# Import RCH estimates for each plot and each survey  
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
df4 <- df4 %>% mutate(prop = (1-empty_prop))# proportion of plot that is reconstructed



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


df_wind <- df4 %>% dplyr::select (survey,RDCHM_Percent,prop,plot,Mn_chm,Wind_Av,Sun_Elev_calc, Sun_Percent, empty_prop, PlotGenus.x,RDCHM,illumination,binary_skycode,CHM_MEAN)



# Produce DF for each Plot Genus
df_windb<- filter(df_wind,PlotGenus.x == "Betula")
df_winds<- filter(df_wind,PlotGenus.x == "Salix aurita")
df_windu<- filter(df_wind,PlotGenus.x == "Ulex europaeus")
df_windf<- filter(df_wind,PlotGenus.x == "Festuca arundinacea")
df_wind_sun<- filter(df_wind,Sun_Percent >90)
df_wind_cloud <- filter(df_wind,Sun_Percent <50)
df_wind_low_elev <- filter(df_wind,Sun_Elev_calc <40)
df_wind_high_elev <- filter(df_wind,Sun_Elev_calc >40)

write_xlsx(df_wind,"output_data/df_wind_prop.xlsx")

#### ALL SPECIES

group.colors <- c("Betula" = "purple", "Ulex europaeus" = "#E3DB71", "Salix aurita" ="green3", "Festuca arundinacea" = "steelblue")





#8. predictiing for Wind only 

wind_model8 <-lme4::glmer(prop ~  Wind_Av + (1+PlotGenus.x+Sun_Elev_calc+Sun_Percent|plot),
                           data = df_wind,
                          family = gaussian(link = "log"),
                          control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#wind_model8 <- wind_model11

(P8 <-ggpredict(wind_model8 , 
                terms = c("Wind_Av")) |>  plot(colors = "black"))

P8a<-plot(P8,colors = "black") +
  labs(
    x = "Average wind speed (m/s)",
    y = "Proportion reconstructed",
    title = "")+
  coord_cartesian(ylim = c(0, 1)) + theme_fancy()+
  theme(legend.title = element_blank())

P8a


ggplot2::ggsave(
  P8a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/glmer_prop_Wind_no_species_A.jpg"),
  width = 8,
  height = 10,
  units = "cm"
) 

summary(wind_model8)


marginaleffects::avg_slopes(wind_model8)


## Model just sun percent no Genus

sun_model23<-lme4::glmer(prop ~   Sun_Percent+ (1+PlotGenus.x+Wind_Av+Sun_Elev_calc|plot),
                         data = df_wind,
                         family = gaussian(link = "log"),
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
P23 <-ggpredict(sun_model23 , 
                terms = c("Sun_Percent")) |>  plot(colors = "black")
P23

P23a<-plot(P23,colors = "black") +
  labs(
    x = "Sun Percent",
    y = "Proportion reconstructed",
    title = ""
  )+coord_cartesian(ylim = c(0, 1)) + theme_fancy()
P23a

summary(sun_model23)
marginaleffects::avg_slopes(sun_model23)
ggplot2::ggsave(
  P23a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/glmer_prop_SunPercent_noGenus.jpg"),
  width = 8,
  height = 10,
  units = "cm"
) 

## Model just sun elevation no Genus

sun_model24<-lme4::glmer(prop ~  Sun_Elev_calc + (1+PlotGenus.x+Wind_Av+Sun_Percent|plot),
                         data = df_wind,
                         family = gaussian(link = "log"),
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
P24 <-ggpredict(sun_model24 , 
                terms = c("Sun_Elev_calc")) |>  plot(colors = "black")

P24

P24a<-plot(P24,colors = "black") +
  labs(
    x = "Sun Elevation",
    y = "Proportion reconstructed",
    title = ""
  )+  coord_cartesian(ylim = c(0, 1))+ theme_fancy()
#+ coord_fixed(xlim=c(20,56),ylim=c(0,1))

P24a

summary(sun_model24)

marginaleffects::avg_slopes(sun_model24)


ggplot2::ggsave(
  P24a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/glmer_prop_SunElev_noGenus.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 


patchwork2 <- P24a + P23a + P8a + plot_layout(ncol = 2)

P_All4_illumination<- patchwork2 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))


P_All4_illumination


ggplot2::ggsave(
  P_All4_illumination,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/3_panel_proportion_reconstructed.jpg"),
  width = 16,
  height = 16,
  units = "cm"
)  

patchwork3 <- P8a+P24a + P23a

P_All3_illumination<- patchwork3 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))


P_All3_illumination


ggplot2::ggsave(
  P_All3_illumination,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/3_tryptich_proportion_reconstructed.jpg"),
  width = 16,
  height = 8,
  units = "cm"
)  



## Sun Elevation - sunny surveys only
sun_model94<-lme4::glmer(RDCHM ~   Sun_Elev_calc+ (1+Wind_Av+Sun_Percent|plot),
                         data = df_wind_sun,
                         family = gaussian(link = "log"))
P94 <-ggpredict(sun_model94 , 
                terms = c("Sun_Elev_calc")) |>  plot(colors = "black")

P94

P94a<-plot(P94,colors = "black") +
  labs(
    x = "Sun Elevation",
    y = "Reduction in RCH from Max (m)",
    title = ""
  )+  coord_cartesian(ylim = c(0.1, 0))+coord_cartesian(ylim = c(0.1, 0))+scale_y_reverse()+ theme_fancy()
#+ coord_fixed(xlim=c(20,56),ylim=c(0,1))

P94a

summary(sun_model94)

P_All2_illumination <- ggarrange(P24a,P23a, ncol = 2, nrow = 1)
plot(P_All2_illumination)

ggplot2::ggsave(
  P_All2_illumination,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/Combined figure_illumination_RDCHM.jpg"),
  width = 16,
  height = 10,
  units = "cm"
) 




#Effect on Sun Percent on RDCHM - with Genus
sun_model53<-lme4::glmer(RDCHM ~   Sun_Percent  * PlotGenus.x + (1+Wind_Av+Sun_Elev_calc|plot),
                         data = df_wind,
                         family = gaussian(link = "log"),
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
P53 <-ggpredict(sun_model53 , 
                terms = c("Sun_Percent","PlotGenus.x" )) |>  plot()
P53

P53a<-plot(P53) +
  labs(
    x = "Sun Percent",
    y = "Reduction in RCH from Max (m)",
    title = "")+ coord_cartesian(ylim = c(0.18, 0))+
    scale_y_reverse()+ theme_fancy()+theme(legend.position = c(0.33, 0.2))+
      theme(legend.title = element_blank())+scale_color_viridis(discrete = TRUE) 
P53a

summary(sun_model53)

ggplot2::ggsave(
  P53a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/glmer_RDCHM_SunPercent_Genus.jpg"),
  width = 8,
  height = 10,
  units = "cm"
) 

#patchwork2 <- (P24a + P94a)/(P23a+P53a)
patchwork2 <- (P24a + P94a)/(P23a+P53a)

P_All4_illumination<- patchwork2 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))

P_All4_illumination

ggplot2::ggsave(
  P_All4_illumination,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/4_panel_illumination.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 


sun_model92<-lme4::glmer(RDCHM ~   Sun_Percent * CHM_MEAN + (1+Wind_Av+Sun_Elev_calc+PlotGenus.x|plot),
                         data = df_wind,
                         family = gaussian(link = "log"))
P92 <-ggpredict(sun_model92 , 
                terms = c("Sun_Percent","CHM_MEAN [0:2, by=0.6]")) |>  plot()
P92

P92a<-plot(P92) +
  labs(
    x = "Sun Percent",
    y = "Reduction in RCH from Max (m)",
    title = "",
    colours = "Plant Height"
  )+ scale_y_reverse()+ theme_fancy()+theme(legend.position = c(0.1, 0.1))
P92a

summary(sun_model92)

ggplot2::ggsave(
  P92a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/glmer_RDCHM_sun_percent_plant height.jpg"),
  width = 8,
  height = 10,
  units = "cm"
) 

# Panel with two main wind effect plots

#P_All2_wind <- ggarrange(P8a,P11a, ncol = 2, nrow = 1)
#plot(P_All2_wind)
patchwork1 <- (P8a + P11a)
P_All2_wind<- patchwork1 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))

P_All2_wind

ggplot2::ggsave(
  P_All2_wind,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/Combined figure_wind models_RDCHM_AB.jpg"),
  width = 16,
  height = 10,
  units = "cm"
) 



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
    y = "Reduction in RCH from Max (m)",
    title = "",
    fill = "Sun Percent"
  )+ scale_y_reverse()+ theme_fancy()+theme(legend.position = c(0.1, 0.1))
P12a

summary(sun_model12)

ggplot2::ggsave(
  P12a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/glmer_RDCHM_Illumination.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 




#Influence of plant height on the effect of wind on RDCHM

wind_model30 <-lme4::glmer(RDCHM ~  Wind_Av * CHM_MEAN + (1+ PlotGenus.x+Sun_Elev_calc+Sun_Percent|plot),
                           data = df_wind,
                           family = gaussian(link = "log"))#,
#  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#P30 <-ggpredict(wind_model30 , 
#                terms = c("Wind_Av", "PlotGenus.x ","CHM_MEAN"))# |>  plot()
P30 <-ggpredict(wind_model30 , 
                terms = c("Wind_Av","CHM_MEAN [0.1:2.1, by=0.65]"))# |>  plot()

summary (wind_model30)

P30a <-plot(P30) +
  labs(
    x = "Average wind speed m/s",
    y = "Reduction in RCH from Max recorded (m)",
    title = "",#Predicted effect of wind speed RCHs \n interaction with plant height",
    colour = "Plant Height (m)"
  )+theme_fancy()+ scale_y_reverse()+ theme(legend.position = c(0.2, 0.2))

P30a 
ggplot2::ggsave(
  P30a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/glmer_wind_plant height_RDCHM_full_model.jpg"),
  width = 8,
  height = 10,
  units = "cm"
) 

## Model just sun percent

sun_model22<-lme4::glmer(RDCHM ~   Sun_Percent * PlotGenus.x + (1+Wind_Av|plot),
                         data = df_wind,
                         family = gaussian(link = "log"))
P22 <-ggpredict(sun_model22 , 
                terms = c("Sun_Percent","PlotGenus.x")) |>  plot()
P22

P22a<-plot(P22) +
  labs(
    x = "Sun Percent",
    y = "Reduction in RCH from Max (m)",
    title = ""
  )+ scale_y_reverse()+ theme_fancy()+theme(legend.position = c(0.2, 0.2))
P22a

summary(sun_model22)

ggplot2::ggsave(
  P22a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/glmer_RDCHM_SunPercent.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 
##11. Full GLMER model for Wind with Plot Genus as fixed effect and Sun Elevation Sun percent and plot as random effects

wind_model11 <-lme4::glmer(prop ~  Wind_Av * PlotGenus.x + (1+Sun_Elev_calc+Sun_Percent|plot),
                           data = df_wind,
                           family = gaussian(link = "log"),
                           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

(P11 <-ggpredict(wind_model11 , 
                 terms = c("Wind_Av","PlotGenus.x")) |>  plot())

P11a<-plot(P11) +
  labs(
    x = "Average wind speed (m/s)",
    y = "Proportion reconstructed",
    title = "",
    fill = "Plot Genus"
  ) +  coord_cartesian(ylim = c(0, 1)) + theme_fancy()+
  theme(legend.position = c(0.33, 0.2))+
  theme(legend.title = element_blank())+scale_color_viridis(discrete = TRUE) 
P11a

ggplot2::ggsave(
  P11a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/glmer_prop_Wind_B.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 

summary(wind_model11)

# modelling the influence of proportion on RDCHM

prop_model24<-lme4::glmer(RDCHM ~  prop + (1|plot),
                         data = df_wind,
                         family = gaussian(link = "log"),
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
Pp4 <-ggpredict(prop_model24 , 
                terms = c("prop")) |>  plot(colors = "black")

Pp4

Pp4a<-plot(Pp4,colors = "black") +
  labs(
    x = "Proportion reconstructed",
    y = "RDCHM",
    title = ""
  )+  coord_cartesian()+scale_y_reverse()+ scale_x_reverse()+theme_fancy()
#+ coord_fixed(xlim=c(20,56),ylim=c(0,1))

Pp4a


Pp4b<-plot(Pp4,colors = "black") +
  labs(
    x = "Proportion of plot reconstructed",
    y = "Reduction in RCH from Max (m)",
    title = ""
  )+  coord_cartesian(ylim = c(0.4, 0),xlim = c(1, 0.5), ) +scale_y_reverse()+ scale_x_reverse()+theme_fancy()
#+ coord_fixed(xlim=c(20,56),ylim=c(0,1))

Pp4b

coord_cartesian(ylim = c(0.4, 0),xlim = c(1, 0.5), ) +scale_y_reverse()+

summary(prop_model24)

marginaleffects::avg_slopes(prop_model24)

ggplot2::ggsave(
  Pp4b,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/glmer_prop_RDCHM.jpg"),
  width = 7,
  height = 8,
  units = "cm"
) 
