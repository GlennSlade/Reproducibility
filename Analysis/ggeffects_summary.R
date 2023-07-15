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

# -----2. Join data tables and summarise ------

# Join the survey and chm tables - for each plot CHM measurement you now have sun elevation wind speed etc 
survey_df <- full_join(CHM, survey, by = "survey")
# Now join with plot characteristics 
master_df <- full_join(survey_df,plot, by = "plot")

#save the master_df for use in other scripts
#write_xlsx(master_df,"C:/Workspace/R_Scripts/Reproducibility/data/master_df.xlsx")

df4 <- master_df %>% mutate(empty = Ct_dtm - Ct_chm)# number of empty cells
df4 <- df4 %>% mutate(empty_prop = empty/Ct_dtm)# proportion of plot that is empty cells


df2 <- df4 %>% dplyr::select (survey,plot,Mn_chm,Wind_Av,Sun_Elev_calc, Sun_Percent, empty_prop, PlotGenus)

# summarise by survey and plot genus (giving average canopy height for each species on each survey)

df3<-summarise(group_by(df2, survey,PlotGenus),
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

#------3. Wind Analysis------

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
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_wind_species.jpg"),
  width = 10,
  height = 10,
  units = "cm"
) 

# fit linear model for different species wind against canopy height and 

mod <- lm(CHM~ Wind,data = df)
summary(mod)
P2 <-ggpredict(mod, 
          terms = "Wind") |>  plot()
P2

ggplot2::ggsave(
  P2,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_wind_ggeffect.jpg"),
  width = 10,
  height = 10,
  units = "cm"
) 



mod1 <- lm(CHM~ Wind * PlotGenus,data = df)
summary(mod1)
P3 <-ggpredict(mod1, 
          terms = c("Wind","PlotGenus")) |>  plot()

P3
ggplot2::ggsave(
  P3,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_wind_species_ggeffect.jpg"),
  width = 16,
  height = 10,
  units = "cm"
) 

# For betula all data 

df5 <- filter(df2,PlotGenus == "Betula")

mod2 <- lm(Mn_chm~ Wind_Av,data = df5)
summary(mod2)
P4 <-ggpredict(mod2, 
               terms = c("Wind_Av")) |>  plot()
P4
ggplot2::ggsave(
  P4,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_wind_betula_ggeffect.jpg"),
  width = 16,
  height = 10,
  units = "cm"
) 

# For salix aurita all data 

df6 <- filter(df2,PlotGenus == "Salix aurita")

mod2 <- lm(Mn_chm~ Wind_Av,data = df6)
summary(mod2)
P5 <-ggpredict(mod2, 
               terms = c("Wind_Av")) |>  plot()
P5
ggplot2::ggsave(
  P5,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_wind_salix_ggeffect.jpg"),
  width = 16,
  height = 10,
  units = "cm"
) 


# For Ulex data 

# filter for the diiferent Plot Genus
df_B <- filter(master_df,PlotGenus == "Betula")
df_U <- filter(master_df,PlotGenus == "Ulex europaeus")
df_S <- filter(master_df,PlotGenus == "Salix aurita")
df_F <- filter(master_df,PlotGenus == "Festuca arundinacea") 

mod2 <- lm(Mn_chm~ Wind_Av,data = df_U)
summary(mod2)
P5 <-ggpredict(mod2, 
               terms = c("Wind_Av")) |>  plot()
P5
ggplot2::ggsave(
  P5,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_wind_ulex_ggeffect.jpg"),
  width = 16,
  height = 10,
  units = "cm"
) 

#For Festuca

mod2 <- lm(Mn_chm~ Wind_Av,data = df_F)
summary(mod2)
P5 <-ggpredict(mod2, 
               terms = c("Wind_Av")) |>  plot()
P5
ggplot2::ggsave(
  P5,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_wind_Festuca_ggeffect.jpg"),
  width = 16,
  height = 10,
  units = "cm"
) 


# ------- 4. Sun Analysis ---------

# fit linear model for different species Sun against mean canopy height

mod <- lm(CHM~ Sun,data = df)
summary(mod)
P2 <-ggpredict(mod, 
               terms = "Sun") |>  plot()
P2

ggplot2::ggsave(
  P2,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_sun_ggeffect.jpg"),
  width = 10,
  height = 10,
  units = "cm"
) 


# fit linear model for different species Sun against mean canopy height - with Plot Genus as factor


mod1 <- lm(CHM~ Sun * PlotGenus,data = df)
summary(mod1)
P3 <-ggpredict(mod1, 
               terms = c("Sun","PlotGenus")) |>  plot()

P3
ggplot2::ggsave(
  P3,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/summary_sun_species_ggeffect.jpg"),
  width = 16,
  height = 10,
  units = "cm"
) 
