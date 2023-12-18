sun_model63<-lme4::glmer(RDCHM ~   Sun_Percent *CHM_MEAN  + (1+PlotGenus.x+Wind_Av+Sun_Elev_calc|plot),
                         data = df_wind,
                         family = gaussian(link = "log"),
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
P63 <-ggpredict(sun_model63 , 
                terms = c("Sun_Percent","CHM_MEAN" )) |>  plot()
P63

P63a<-plot(P63) +
  labs(
    x = "Sun Percent",
    y = "Reduction in reconstructed canopy height from Max (m)",
    title = "")+
  scale_y_reverse()+ theme_fancy()+theme(legend.position = c(0.33, 0.2))+
  theme(legend.title = element_blank())+scale_color_viridis(discrete = TRUE) 
P63a

summary(sun_model63)

ggplot2::ggsave(
  P63a,
  # filename = "/plots/test.png",
  filename = paste0("output_data/full_model/glmer_RDCHM_SunPercent_Genus.jpg"),
  width = 8,
  height = 10,
  units = "cm"
) 
