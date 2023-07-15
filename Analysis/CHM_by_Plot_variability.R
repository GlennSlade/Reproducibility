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

library(cowplot)
library(ggpubr)

#GLMM development

#----1. Data Prep------

#read in data

# Import environmental data for each survey: wind sun elevation etc
survey <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/Survey_Data.xlsx")
# Import reconstructed canopy height estimates for each plot and each survey  
CHM <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/plot_chm_metrics_temp30.xlsx")
# Import plot data: species, plot measurements etc
plot <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/Plot_Data.xlsx")

#
plot_statistics <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_plot_statistics.xlsx")

statistics <- plot_statistics%>% dplyr::select (plot,CHM_MEAN,CHM_VAR_PERC)



# Join the survey and chm tables - for each plot CHM measurement you now have sun elevation wind speed etc 
survey_df <- full_join(CHM, survey, by = "survey")
# Now join with plot characteristics 
master<- full_join(survey_df,plot, by = "plot")

master_df <- full_join(master,statistics, by = "plot")

#Summarise

df4 <- master_df %>% mutate(empty = Ct_dtm - Ct_chm)# number of empty cells
df4 <- df4 %>% mutate(empty_prop = empty/Ct_dtm)# proportion of plot that is empty cells


# Figure S1. Boxplot of canopy height observations for each plot----
# Create boxplot


## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 6, color = "black"),
      axis.title = element_text(size = 6, color = "black"),
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
      legend.text = element_text(size = 6, color = "black"),
      legend.title = element_text(size = 6, color = "black"),
      legend.position = c(0.3, 1),
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

group.colors <- c("Betula" = "#333BFF", "Ulex europaeus" = "#CC6600", "Salix aurita" ="palegreen2", "Festuca arundinacea" = "#E3DB71")


#64 Plot Boxplot (very crowded)


BP <- ggplot(data = df4, mapping = aes (x=reorder(plot, CHM_MEAN), y=Mn_chm, group=plot, fill= PlotGenus))+ 
  xlab("Plot Number") + stat_boxplot(lwd=0.3,outlier.shape = NA)+ scale_x_discrete()+
  ylab("Mean Reconstructed Canopy height (m)") + ggtitle("Reconstructed canopy height from all surveys") + 
  theme(axis.text.x = element_text(size = 2))+
  theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", coef=NULL, width = 0.3, lwd=0.3)+ theme_fancy()+
  scale_fill_manual(values=group.colors)+
  theme(
    legend.position = c(0.3, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
BP

ggsave(
  BP,
  filename = "output_data/plots/Figure S1 - Height boxplot_all.png",
  width = 16,
  height = 11,
  units = "cm"
)


# Inidividual plots for each species
# Flitering for each species
df_B <- filter(df4,PlotGenus == "Betula")
df_U <- filter(df4,PlotGenus == "Ulex europaeus")
df_S <- filter(df4,PlotGenus == "Salix aurita")
df_F <- filter(df4,PlotGenus == "Festuca arundinacea") 

#Betula plot


BP_B <- ggplot(data = df_B, mapping = aes (x=reorder(plot, CHM_MEAN), y=Mn_chm, group=plot, fill= PlotGenus))+ 
  xlab("Plot Number") + stat_boxplot(lwd=0.3,outlier.shape = NA)+ scale_x_discrete()+
  ylab("Mean Reconstructed Canopy height (m)") + ggtitle("Reconstructed canopy height from all surveys") + 
  scale_fill_manual(values=group.colors)+
  theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", coef=NULL, width = 0.3, lwd=0.3)+ theme_fancy()+
  theme(
    legend.position = c(0.3, 1.1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),legend.title=element_blank()
  )

BP_B

#Ulex plot

BP_U <- ggplot(data = df_U, mapping = aes (x=reorder(plot, CHM_MEAN), y=Mn_chm, group=plot, fill= PlotGenus))+ 
  xlab("Plot Number") + stat_boxplot(lwd=0.3,outlier.shape = NA)+ scale_x_discrete()+
  ylab("Mean Reconstructed Canopy height (m)") + ggtitle("Reconstructed canopy height from all surveys") + 
  scale_fill_manual(values=group.colors)+
  theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", coef=NULL, width = 0.3, lwd=0.3)+ theme_fancy()+
  theme(
    legend.position = c(0.5, 1.1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),legend.title=element_blank()
  )
BP_U


# Festuca plots
BP_F <- ggplot(data = df_F, mapping = aes (x=reorder(plot, CHM_MEAN), y=Mn_chm, group=plot, fill= PlotGenus))+ 
  xlab("Plot Number") + stat_boxplot(lwd=0.3,outlier.shape = NA)+ scale_x_discrete()+
  ylab("Mean Reconstructed Canopy height (m)") + ggtitle("Reconstructed canopy height from all surveys") + 
  scale_fill_manual(values=group.colors)+
  theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", coef=NULL, width = 0.3, lwd=0.3)+ theme_fancy()+
  theme(
    legend.position = c(0.6, 1.1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),legend.title=element_blank()
  )

BP_F

# Salix Plots

BP_S <- ggplot(data = df_S, mapping = aes (x=reorder(plot, CHM_MEAN), y=Mn_chm, group=plot, fill= PlotGenus))+ 
  xlab("Plot Number") + stat_boxplot(lwd=0.3,outlier.shape = NA)+ scale_x_discrete()+
  ylab("Mean Reconstructed Canopy height (m)") + ggtitle("Reconstructed canopy height from all surveys") + 
  scale_fill_manual(values=group.colors)+
  theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", coef=NULL, width = 0.3, lwd=0.3)+ theme_fancy()+
  theme(
    legend.position = c(0.4, 1.1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),legend.title=element_blank()
  )
BP_S




P_All <- ggarrange(BP_S,BP_B, BP_U, BP_F, ncol = 2, nrow = 2)

plot(P_All)


ggsave(
  P_All,
  # filename = "/plots/test.png",
  filename = "output_data/plots/Figure S1 Height boxplot 4 species panel.png",
  width = 16,
  height = 16,
  units = "cm"
)


# fixed Y plots

coord_fixed(xlim=c(0.39,2.55),ylim=c(0,2.4))

BP_B <- ggplot(data = df_B, mapping = aes (x=reorder(plot, CHM_MEAN), y=Mn_chm, group=plot, fill= PlotGenus))+ 
  xlab("Plot Number") + stat_boxplot(lwd=0.3,outlier.shape = NA)+ scale_x_discrete()+
  ylab("Mean Reconstructed Canopy height (m)") + ggtitle("Reconstructed canopy height from all surveys") + 
  scale_fill_manual(values=group.colors)+
  coord_fixed(ylim=c(0,3.5))+
  theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", coef=NULL, width = 0.3, lwd=0.3)+ theme_fancy()+
  theme(
    legend.position = c(0.3, 1.1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),legend.title=element_blank()
  )

BP_B

#Ulex plot

BP_U <- ggplot(data = df_U, mapping = aes (x=reorder(plot, CHM_MEAN), y=Mn_chm, group=plot, fill= PlotGenus))+ 
  xlab("Plot Number") + stat_boxplot(lwd=0.3,outlier.shape = NA)+ scale_x_discrete()+
  ylab("Mean Reconstructed Canopy height (m)") + ggtitle("Reconstructed canopy height from all surveys") + 
  scale_fill_manual(values=group.colors)+
  coord_fixed(ylim=c(0,3.5))+
  theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", coef=NULL, width = 0.3, lwd=0.3)+ theme_fancy()+
  theme(
    legend.position = c(0.5, 1.1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),legend.title=element_blank()
  )
BP_U


# Festuca plots
BP_F <- ggplot(data = df_F, mapping = aes (x=reorder(plot, CHM_MEAN), y=Mn_chm, group=plot, fill= PlotGenus))+ 
  xlab("Plot Number") + stat_boxplot(lwd=0.3,outlier.shape = NA)+ scale_x_discrete()+
  ylab("Mean Reconstructed Canopy height (m)") + ggtitle("Reconstructed canopy height from all surveys") + 
  scale_fill_manual(values=group.colors)+
  coord_fixed(ylim=c(0,3.5))+
  theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", coef=NULL, width = 0.3, lwd=0.3)+ theme_fancy()+
  theme(
    legend.position = c(0.6, 1.1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),legend.title=element_blank()
  )

BP_F

# Salix Plots

BP_S <- ggplot(data = df_S, mapping = aes (x=reorder(plot, CHM_MEAN), y=Mn_chm, group=plot, fill= PlotGenus))+ 
  xlab("Plot Number") + stat_boxplot(lwd=0.3,outlier.shape = NA)+ scale_x_discrete()+
  ylab("Mean Reconstructed Canopy height (m)") + ggtitle("Reconstructed canopy height from all surveys") + 
  scale_fill_manual(values=group.colors)+
  coord_fixed(ylim=c(0,3.5))+
  theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", coef=NULL, width = 0.3, lwd=0.3)+ theme_fancy()+
  theme(
    legend.position = c(0.4, 1.1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),legend.title=element_blank()
  )
BP_S




P_All <- ggarrange(BP_S,BP_B, BP_U, BP_F, ncol = 2, nrow = 2)

plot(P_All)


ggsave(
  P_All,
  # filename = "/plots/test.png",
  filename = "output_data/plots/Figure S1 Height boxplot 4 species panel_fixed.png",
  width = 16,
  height = 16,
  units = "cm"
)


