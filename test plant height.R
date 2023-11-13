##50 Now Wind and Plant Height

wind_model50 <-lme4::glmer(RDCHM ~  Wind_Av +CHM_MEAN+ + (1+Sun_Elev_calc+Sun_Percent|plot),
                           data = df_windb,
                           family = gaussian(link = "log"),
control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#P50 <-ggpredict(wind_model50 , 
#                terms = c("Wind_Av", "PlotGenus.x ","CHM_MEAN"))# |>  plot()
P50 <-ggpredict(wind_model50 , 
                terms = c("Wind_Av","CHM_MEAN [0.1:2.1, by=0.5]"))# |>  plot()


P50a <-plot(P50) +
  labs(
    x = "Average wind speed m/s",
    y = "Reduction in reconstructed canopy height from Max recorded",
    title = "Predicted effect of wind speed reconstructed canopy heights \n interaction with plant height",
    colour = "Plant Height (m)"
  )+theme_fancy()+theme(legend.position = c(0.2, 0.8))

P50a 
ggplot2::ggsave(
  P50a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/glmer_wind_plant height_RDCHM.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 
