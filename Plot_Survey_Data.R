

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

# SD <- read_csv("C:/Workspace/Reproducibility/Survey_Data.csv",
SD <- read_csv("data/Survey_Data.csv",
                              col_types = cols(DateSurvey = col_date("%d/ %m/ %Y")))



#                                TIMESTAMP_END = col_datetime("%Y %m %d %H %M")))

#DF_SEG_EC0a <- read_csv(file=paste(mpath, "US-Seg_HH_201801010000_201901010000.csv", sep=""),
 #                       col_types = cols(TIMESTAMP_START = col_datetime("%Y %m %d %H %M"),
  #                                       TIMESTAMP_END = col_datetime("%Y %m %d %H %M")),
   #                     na = c("", "NA",-9999))
# dim (SD)
# summary (SD)
# head (SD)
# str(SD)

#-------2. Preparing data --------

# SD2 <- data.frame(SD)
str (SD)
# Changing column data types
SD$Sky_Code <- as.factor(SD$Sky_Code) # Needs to be factor to plot as discrete data




#-----3.   Plotting Data -------
#Plots basic survey conditions such as wind speed and sun conditions to check sampling so far

#Average wind speed vs Sun Elevation
(figurex <- SD %>% 
   ggplot(aes(x=Sun_Elev_calc,
              y=Wind_Av,
              colour = Sky_Code)) + 
  geom_point(aes(colour =Sky_Code )) +
               scale_color_manual(values = c(
                 "7" = "purple",
                 "6"= "orange",
                 "5"="red",
                 "4"="red",
                 "0"="red")) +
   labs(x = "Sun Elevation (degrees)",
        y = expression("Wind speed (m s"^"-1"*")"),
        title = "Survey distribtuion") +  
   theme_fancy() +
   theme(legend.position = c(0.1, 0.9)) +
  geom_errorbar(aes(ymin=Wind_Av-Wind_SD, ymax=Wind_Av+Wind_SD)) 
 
)


ggsave(
  figurex,
  # filename = "/plots/test.png",
  filename = "plots/test.png",
  width = 16,
  height = 16,
  units = "cm"
)
  
)






                                                                                                                                           
#Max wind speed vs Sun Elevation
SD2 %>% ggplot(aes (x=Sun_Elev_calc, y=Wind_Max, colour = Sky_Code))+ geom_point((aes (colour =Sky_Code )) + scale_color_manual(values = c("7" = "purple",
                                                                                                                                           "6"= "orange",
                                                                                                                                           "5"="red")) ) 
#SD wind speed vs Sun Elevation
SD2 %>% ggplot(aes (x=Sun_Elev_calc, y=Wind_SD, colour = Sky_Code))+ geom_point((aes (colour =Sky_Code )) + scale_color_manual(values = c("7" = "purple",
                                                                                                                                           "6"= "orange",
                                                                                                                                           "5"="red")) ) 
# Average Wind Speed vs Percentage Sun
SD2 %>% ggplot(aes (x=Sun_Percent, y=Wind_Av, colour = Sky_Code))+ geom_point(aes (colour =Sky_Code )) + scale_color_manual(values = c("7" = "purple",
                              "6"= "orange",
                                "5"="red", "0"="red","4"="red")) 
#Histogram

#SD2 %>% ggplot(aes(x=Wind_Av)) + geom_histogram(binwidth=0.5)

SD2 %>% ggplot(aes(x=Wind_Av, fill=Sky_Code)) + geom_histogram ()

