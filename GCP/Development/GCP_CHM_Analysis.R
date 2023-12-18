# Script analysing the canopy height metric of Ground Control Points to check on the consistency of 
# height measurements between different surveys

# Libraries
library(tidyverse)
library(viridis)
library(readxl)
library(writexl)
library(DescTools)
library(ggeffects)
library(splines)
#library(ggsave2)
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

#----1. Import data -----

# Import environmental data for each survey: wind sun elevation etc
survey <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/Survey_Data.xlsx")
# Import reconstructed canopy height estimates for each plot and each survey  
#CHM <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/plot_chm_metrics_GCP61.xlsx")

CHM <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/plot_chm_metrics_GCP61.xlsx")

# Import plot data: species, plot measurements etc
#plot <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/Plot_Data.xlsx")


df1 <- full_join(CHM, survey, by = "survey")
# Now join with plot characteristics and summarise to produce mean GCP CHM

df2 <- df1 %>% dplyr::select (survey,plot,Mn_chm,Wind_Av,Sun_Elev_calc, Sun_Percent, Wind_SD,Wind_Max)

df3<- summarise(group_by(df2, plot),na.rm = TRUE,
               MEANCHMGCP=mean(Mn_chm,na.rm = TRUE),
               Wind=mean(Wind_Av),Sun = mean(Sun_Elev_calc), SunP = mean(Sun_Percent),Wind_SD = mean (Wind_SD), Wind_Max = mean (Wind_Max))              
df4 <-df3 %>% dplyr::select (plot,MEANCHMGCP)

write_xlsx(df4,paste0("C:/Workspace/R_Scripts/Reproducibility/data/mean_GCP_recon.xlsx"))  

df4 <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/mean_GCP_recon_GNSS.xlsx")


# Now join summary statistics to main df and calculate difference from mean
master_df <- full_join (df2, df4, by = "plot")

master_df <- master_df %>% mutate(RDCHM = Mn_chm - MEANCHMGCP )
master_df <- master_df %>% mutate(RDGNSS = Mn_chm - GNSS )

write_xlsx(master_df,paste0("C:/Workspace/R_Scripts/Reproducibility/data/mean_GCP_recon_master.xlsx"))

# Look at Plot of difference over surveys

# GCP 1

df_GCP1<- filter(master_df,plot== "1")


p1<-ggplot(data=df_GCP1, aes(x=survey, y=RDCHM,fill=Sun_Percent)) +
  geom_bar(stat="identity")+ ggtitle("GCP 1") +
  xlab("Survey Number") + ylab("Reconstructed Height - difference from GNSS (m)")+theme_fancy()

plot(p1)

# GCP 2

df_GCP2<- filter(master_df,plot== "2")


p2<-ggplot(data=df_GCP2, aes(x=survey, y=RDCHM, fill=Wind_Av)) +
  geom_bar(stat="identity")+ ggtitle("GCP 2") +
  xlab("Survey Number") + ylab("Reconstructed Height - difference from GNSS (m)")+theme_fancy()

plot(p2)

# GCP 3

df_GCP3<- filter(master_df,plot== "3")


p3<-ggplot(data=df_GCP3, aes(x=survey, y=RDCHM, fill=Sun_Elev_calc)) +
  geom_bar(stat="identity")+ ggtitle("GCP 3") +
  xlab("Survey Number") + ylab("Reconstructed Height - difference from GNSS (m)")+theme_fancy()

plot(p3)

# GCP 4

df_GCP4<- filter(master_df,plot== "4")


p4<-ggplot(data=df_GCP4, aes(x=survey, y=RDCHM)) +
  geom_bar(stat="identity",fill="steelblue")+ ggtitle("GCP 4") +
  xlab("Survey Number") + ylab("Reconstructed Height - difference from GNSS (m)")+theme_fancy()

plot(p4)
# GCP 5

df_GCP5<- filter(master_df,plot== "5")


p5<-ggplot(data=df_GCP5, aes(x=survey, y=RDCHM)) +
  geom_bar(stat="identity",fill="steelblue")+ ggtitle("GCP 5") +
  xlab("Survey Number") + ylab("Reconstructed Height - difference from GNSS (m)")+theme_fancy()

plot(p5)
# GCP 6

df_GCP6<- filter(master_df,plot== "6")


p6<-ggplot(data=df_GCP6, aes(x=survey, y=RDCHM)) +
  geom_bar(stat="identity",fill="steelblue")+ ggtitle("GCP 6") +
  xlab("Survey Number") + ylab("Reconstructed Height - difference from GNSS (m)")+theme_fancy()

plot(p6)
# GCP 7

df_GCP7<- filter(master_df,plot== "7")


p7<-ggplot(data=df_GCP7, aes(x=survey, y=RDCHM)) +
  geom_bar(stat="identity",fill="steelblue")+ ggtitle("GCP 7") +
  xlab("Survey Number") + ylab("Reconstructed Height - difference from GNSS (m)")+theme_fancy()

plot(p7)
# GCP 8

df_GCP8<- filter(master_df,plot== "8")


p8<-ggplot(data=df_GCP8, aes(x=survey, y=RDCHM)) +
  geom_bar(stat="identity",fill="steelblue")+ ggtitle("GCP 8") +
  xlab("Survey Number") + ylab("Reconstructed Height - difference from GNSS (m)")+theme_fancy()

plot(p8)
# GCP 9

df_GCP9<- filter(master_df,plot== "9")


p9<-ggplot(data=df_GCP9, aes(x=survey, y=RDCHM)) +
  geom_bar(stat="identity",fill="steelblue")+ ggtitle("GCP 9") +
  xlab("Survey Number") + ylab("Reconstructed Height - difference from GNSS (m)")+theme_fancy()

plot(p9)
# GCP 10

df_GCP10<- filter(master_df,plot== "10")


p10<-ggplot(data=df_GCP10, aes(x=survey, y=RDCHM)) +
  geom_bar(stat="identity",fill="steelblue")+ ggtitle("GCP 10") +
  xlab("Survey Number") + ylab("Reconstructed Height - difference from GNSS (m)")+theme_fancy()

plot(p10)
# GCP 11

df_GCP11<- filter(master_df,plot== "11")


p11<-ggplot(data=df_GCP11, aes(x=survey, y=RDCHM)) +
  geom_bar(stat="identity",fill="steelblue")+ ggtitle("GCP 11") +
  xlab("Survey Number") + ylab("Reconstructed Height - difference from GNSS (m)")+theme_fancy()

plot(p11)
# GCP 12

df_GCP12<- filter(master_df,plot== "12")


p12<-ggplot(data=df_GCP12, aes(x=survey, y=RDCHM)) +
  geom_bar(stat="identity",fill="steelblue")+ ggtitle("GCP 12") +
  xlab("Survey Number") + ylab("Reconstructed Height - difference from GNSS (m)")+theme_fancy()

plot(p12)
# GCP 13

df_GCP13<- filter(master_df,plot== "13")


p13<-ggplot(data=df_GCP13, aes(x=survey, y=RDCHM)) +
  geom_bar(stat="identity",fill="steelblue")+ ggtitle("GCP 13") +
  xlab("Survey Number") + ylab("Reconstructed Height - difference from GNSS (m)")+theme_fancy()

plot(p13)


p14 <- ggplot(data = master_df, mapping = aes (x=survey, y=RDCHM, group = survey))+ 
  stat_boxplot(fill="steelblue",outlier.shape = NA)+
  xlab("Survey Number") + ylab("Reconstructed Height - difference from GNSS measurement (m)")+theme_fancy()+ 
  ggtitle("Ground Control Point Reconstructed Height - difference all surveys and all GCPs") + 
  geom_hline(yintercept=0, linetype="dashed", color = "grey")
plot (p14)


ggplot2::ggsave(
  p14,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/GCP_reconstructed_height_difference from GNSS.jpg"),
  width = 16,
  height = 8,
  units = "cm"
) 

p15 <- ggplot(data = master_df, mapping = aes (x=survey, y=RDGNSS, group = survey))+ 
  stat_boxplot(fill="steelblue",outlier.shape = NA)+
  xlab("Survey Number") + ylab("Reconstructed Height - difference from GNSS measurement (m)")+theme_fancy()+ 
  ggtitle("Ground Control Point Reconstructed Height - difference all surveys and all GCPs") + 
  geom_hline(yintercept=0, linetype="dashed", color = "grey")
plot (p15)


ggplot2::ggsave(
  p15,
  # filename = "/plots/test.png",
  filename = paste0("C:/Workspace/R_Scripts/Reproducibility/output_data/GCP_reconstructed_height_difference from GNSS.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 

df3<-summarise(group_by(master_df, survey),
               CHM=mean(Mn_chm),
               Wind=mean(Wind_Av),Mean_dif_GCP = mean(RDCHM))              
write_xlsx(df3,"output_data/summary_survey_GCP_reconstruction.xlsx")

