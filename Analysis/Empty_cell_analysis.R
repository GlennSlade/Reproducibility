# Empty cells in reconstruction compared to wind speed and sun angle


# Libraries
library(tidyverse)
library(viridis)
library(readxl)
library(writexl)
library(DescTools)
library(cowplot)
library(ggpubr)
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
CHM <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/plot_chm_metrics_temp10.xlsx")
# Import plot data: species, plot measurements etc
plot <- read_xlsx("C:/Workspace/R_Scripts/Reproducibility/data/Plot_Data.xlsx")

# -----2. Join data tables ------

# Join the survey and chm tables - for each plot CHM measurement you now have sun elevation wind speed etc 
survey_df <- full_join(CHM, survey, by = "survey")
# Now join with plot characteristics 
master_df <- full_join(survey_df,plot, by = "plot")


# ----3. Calculate empty spaces (pixels with no reconstructed canopy height)

df <- master_df %>% mutate(empty = Ct_dtm - Ct_chm)# number of empty cells
df <- df %>% mutate(empty_prop = empty/Ct_dtm)# proportion of plot that is empty cells

df$empty_prop

# ---4. Simple plot of empty cell proportion by wind speed


x <- as.vector(df$Wind_Av)
#x <- na.omit(x)
y <- as.vector(df$empty_prop)
#y <- na.omit(y)
df_temp2 <- data.frame(x = x, y = y,
                       d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))


#df_temp <- na.omit(df_temp2)

# Calculate Total Least Squares Regression (extracted from base-R PCA function)

pca <- prcomp(~x+y,df_temp2)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x") # equation for printing

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

#Plot the graph

#(paste0("P",i,"_w")) <- ggplot(df_temp) +
Pcompare <- ggplot(df_temp2) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  #  add the statistics
  geom_text(aes(x=0.0,y=3),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=2.8),label=paste0('R2: ',round(r2val,2)),hjust='left',size=3.0)+
  geom_text(aes(x=0.0,y=2.6),label=ccc,hjust='left', size=3.0)+
  geom_text(aes(x=0.0,y=2.4),label=equation,hjust='left', size=3.0)+
  theme(text = element_text(size=36))+
  scale_color_identity() +
  theme_fancy() +
  #add title and labels
  ggtitle(paste0("Intercomparison of wind and proportion of plot with no reconstructed points"))+
  theme(aspect.ratio=1)+
  xlab('Wind speed (m/s')+
  ylab('Proportion of plot without reconstructed points')+
  coord_equal(ratio=1)
#coord_fixed(xlim=c(0.39,2.55),ylim=c(0,1.2))
#plot(  (paste0("P",i,"_w")))
plot(Pcompare)

ggsave(
  Pcompare,
  # filename = "/plots/test.png",
  filename = "output_data/plots/comparison_wind_empty.png",
  width = 10,
  height = 10,
  units = "cm"
)




