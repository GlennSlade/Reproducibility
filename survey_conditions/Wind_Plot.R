
#-------0. Set up environment --------

# Libraries
library(tidyverse)
library(viridis)

## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 10, color = "black"),
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
        size = 10,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 10, color = "black"),
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

#-------1. Read in data in CSV Files --------

# reading wind and environmental data exported from Kestrel Weather station
headers = read.csv("C:/Workspace/Reproducibility/Wind/S61_wind_data.csv", skip = 3, header = F, nrows = 1, as.is = T)
WD = read.csv("C:/Workspace/Reproducibility/Wind/S61_wind_data.csv", skip = 5, header = F)
colnames(WD)= headers
view (WD)
#str (WD)

#-------2. Preparing data --------

#-----3.   Plotting Data -------
#Plots basic survey conditions such as wind speed and sun conditions to check sampling so far

#Average wind speed vs Sun Elevation
(figurex <- WD %>% 
   ggplot(aes(x=Sun_Elev_calc,
              y=Wind_Av,
              colour = Sky_Code)) + 
   geom_point(aes(colour =Sky_Code )) +
   scale_color_manual(values = c(
     "7" = "purple",
     "6"= "orange",
     "5"="red",
     "4"="red",
     "2"="red",
     "0"="red")) +
   labs(x = "Sun Elevation (degrees)",
        y = expression("Mean Wind speed (m s"^"-1"*")"),
        title = "Survey Distribution") +  
   theme_fancy() +
   theme(legend.position = c(0.1, 0.85), legend.box.background = element_rect(colour = "black")) +
   geom_errorbar(aes(ymin=Wind_Av-Wind_SD, ymax=Wind_Av+Wind_SD)) 
 
)

ggsave(
  figurex,
  # filename = "/plots/test.png",
  filename = "plots/test_wind_vs_elevation.png",
  width = 16,
  height = 16,
  units = "cm"
)

