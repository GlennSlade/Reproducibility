
sun_model94<-lme4::glmer(RDCHM ~   Sun_Elev_calc+ (1+Wind_Av+Sun_Percent|plot),
                         data = df_wind_sun,
                         family = gaussian(link = "log"))
sun_model24<-lme4::glmer(RDCHM ~   Sun_Elev_calc+ (1+Wind_Av+Sun_Percent|plot),
                         data = df_wind,
                         family = gaussian(link = "log"))

Predict_Sunny <-ggpredict(sun_model94 , 
                terms = c("Sun_Elev_calc [20:55]")) # sunny data only


Predict_All <-ggpredict(sun_model24 , 
                terms = c("Sun_Elev_calc [20:55]")) # all data

# Predict_All2 <- Predict_All%>% dplyr::mutate(model = "All data",.keep = "all") # 2 = all data
# Predict_All3 <- Predict_All %>% add_column(model = "All Data")
# 
# Predict_All4 <-ggpredict(sun_model24 , 
#                         terms = c("Sun_Elev_calc [20:55]"))|>dplyr::mutate(model = "All data")

Predict_All5 <- tibble::as_tibble(Predict_All)|> dplyr::mutate(model="All data")



Predict_Sunny5 <-  tibble::as_tibble(Predict_Sunny)|> dplyr::mutate(model="Cloud free data")
P_Join <-dplyr::full_join(Predict_Sunny5, Predict_All5)

P <-ggplot(P_Join, aes(x, predicted, colour = model, fill = model)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  labs(x = "Sun Elevation")
P

PCombined<-plot(P) +
  labs(
    x = "Sun Elevation",
    y = "Reduction in RCH from Max (m)",
    title = ""
  )+  coord_cartesian(ylim = c(0.1, 0))+coord_cartesian(ylim = c(0.1, 0))+scale_y_reverse()+ theme_fancy()+
theme(legend.position = c(0.2, 0.1))

PCombined


