# ------ 0 Setup Environment ----------


# Load required packages
# Install
library(tidyverse) # For dplyr and ggplot2
library(viridis) # load friendly colour palette for plotting.
library(patchwork) # this is Andy's favorite for multi-panel plots
library(propagate) # required for predicting confidence intervals on ESD : Biomass plot
library(ggpmisc) # for adding model parameters to plots
library(gvlma) # Global Validation of Linear Models Assumptions
library(polynom)
library(nlstools)
library(ggpubr) 
library(DescTools)
library(Metrics)
library(hydroGOF) # The RMSE function in this package allows for na.rm parameter

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
windowsFonts("Helvetica" = windowsFont("Helvetica"))  # Ensure font is mapped correctly


#-------------- 1. Extract Data --------------
# Load data
col_names <- names(read_csv("input_data/pinon_data.csv", n_max = 0))
pinon_data <- read_csv("input_data/pinon_data.csv", col_names = col_names, skip = 2)
rm(col_names)



# -------------- 2. Tidying Data --------------

# Calculate LMA from scanned and weighed needle subsamples (g m-2)
pinon_data$LMA = pinon_data$scanned_wt/(pinon_data$scanned_LA/10000)

# Calculate the mean and standard deviation of SLA 
LMA_mean = mean(pinon_data$LMA, na.rm = TRUE)
sd(pinon_data$LMA, na.rm = TRUE)
#mean LMA = 263.0, standard deviation = 28.9

# Estimate the total leaf area (m2) by scaling LMA with measured total needle weight 
pinon_data$LA_m2 = pinon_data$tot_needle_wt/LMA_mean

# Calculate canopy area of ellipse from a and b diameters
pinon_data$canopy_area_from_daim <- pi * (pinon_data$CanDia1)/2 * (pinon_data$CanDia2)/2

# Calculate the proportion of the total biomass that is < 3 cm
pinon_data$proportion_small_biomass <- pinon_data$dry_mass_small_partititon / pinon_data$dry_mass_total_g

# Calculate the proportion of the total biomass that is > 3 cm
pinon_data$proportion_large_biomass <- pinon_data$dry_mass_large_partititon / pinon_data$dry_mass_total_g

### RCD to DBH conversion from Chojnacky $ Rogers (1999)
# DBH = B0 + B1*RCD + B2*stm +B3*PIED + B4*RCDp + B5*QUGA +B6*RCDq

# DBH = diameter at 1.3 m above ground level
# RCD = diameter at root collar

# conversion coefficients
B0 = -6.8180
B1 = 1.0222
B2 = 1.8879
B3 = 1.8971
B4 = -0.0399
B5 = 3.1100
B6 = -0.689
stm = 1.0
PIED = 1.0
QUGA = 0.0
RCDq = 0.0

# Apply the conversion for all trees > 1.3 m tall (short trees won't have a DBH)
pinon_data$DBH_cm <- ifelse(pinon_data$max_height > 1.3,
                            B0 + B1*pinon_data$diameter_at_base_wet + B2*stm +B3*PIED + B4*pinon_data$diameter_at_base_wet + B5*QUGA +B6*RCDq,
                            NA)



# -------------- Figure 1 --------------

### compare maximum canopy heights ####
# Model for comparing field measured height to drone derived max height
Model_height <- lm(drone_canopy_height_max ~ max_height, data = pinon_data)
summary(Model_height)
# Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.11238    0.17395  -0.646    0.526    
# max_height   0.98708    0.04484  22.013 1.83e-14 ***

# calculate RMSE for this model
hydroGOF::rmse(pinon_data$drone_canopy_height_max, predict(Model_height, pinon_data))
# RMSE = 0.4000207

# Testing difference between field measured height and drone measured height
height_difference <- pinon_data$drone_canopy_height_max - pinon_data$max_height
# Assessing data distribution
length(height_difference)
qqnorm(height_difference)
qqline(height_difference)       
shapiro.test(height_difference)
# W = 0.83306, p-value = 0.00281
# these data are significantly non-normal, so a non-parametric test is needed.
# Wilcoxon signed-rank test
wilcox.test(pinon_data$drone_canopy_height_max, pinon_data$max_height,
            alternative=c("two.sided", "less", "greater"))
# Result: height derived from drone & field measured are not sig different (p=0.7763)

# Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~max_height+drone_canopy_height_max, pinon_data)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(pinon_data$max_height, pinon_data$drone_canopy_height_max, ci = "z-transform",
                  conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))


#Plot drone derived height as a function of measured height, with TLS model
fig1A <- ggplot(data = pinon_data, aes( x = max_height, y = drone_canopy_height_max )) +
  geom_point(size = 2) +
  stat_function(fun = function(x) tls_slp*x + tls_int, size = 1) +
  geom_abline(intercept = 0, slope = 1, color="black", 
              linetype="dashed", size= 0.5) +
  theme_fancy() +
  scale_y_continuous(limits=c(-0.5,7), breaks=seq(0,6,2), expand = c(0,0)) +
  scale_x_continuous(limits=c(-0.5,7), breaks=seq(0,6,2), expand = c(0,0)) +
  xlab("Ground measured max height (m)") +
  ylab("UAV measured max height (m)") +
  annotate("text", x=2.3, y=6.6, label=equation) +
  annotate("text", x=1.4, y=5.8, label=ccc)



### compare canopy areas ####
# Linear models

# Linear model for comparing canopy area from polygons (CA1) and orthogonally (CA2)
Model_CA1_CA2 <- lm(canopy_area_from_daim ~ canopy_area, data = pinon_data)
summary(Model_CA1_CA2)
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.13429    0.11805  -1.138     0.27    
#canopy_area  1.04942    0.01694  61.954   <2e-16 ***

# Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~canopy_area+canopy_area_from_daim, pinon_data)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(pinon_data$canopy_area, pinon_data$canopy_area_from_daim, ci = "z-transform",
                  conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

# Plot drone derived canopy area as a function of field estimated canopy area
fig1B <- ggplot(data = pinon_data, aes( x = canopy_area, y = canopy_area_from_daim )) +
  geom_point(size = 2) +
  stat_function(fun = function(x) tls_slp*x + tls_int, size = 1) +
  geom_abline(intercept = 0, slope = 1, color="black", 
              linetype="dashed", size= 0.5) +
  theme_fancy() +
  scale_y_continuous(limits=c(-0.5,21), breaks=seq(0,22,5), expand = c(0,0)) +
  scale_x_continuous(limits=c(-0.5,21), breaks=seq(0,22,5), expand = c(0,0)) +
  labs(x = expression(CA["1"]~(m^2)),
       y = expression(CA["2"]~(m^2))) +
  annotate("text", x=7, y=20, label=equation) +
  annotate("text", x=4.5, y=18, label=ccc)



### combine plot objects with Patchwork
pall <- (fig1A + fig1B) +
  plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97))

filename = "plots/Figure_1"

ggsave(
  pall,
  filename = paste0(filename, ".png"),
  width = 16,
  height = 8,
  units = "cm"
)

ggsave(
  pall,
  filename = paste0(filename, ".pdf"),
  width = 16,
  height = 8,
  units = "cm"
)



# -------------- Figure 2 --------------

# Wet base diameter as predictor of total tree biomass ####

# Statistical model
model.rcd.biomass <- nls(dry_mass_total_kg ~ a*diameter_at_base_wet^b,
                         data = pinon_data,
                         start = list(a =1, b =1),
                         na.action=na.exclude)
summary(model.rcd.biomass) # Return model parameters
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# a  0.01841    0.03096   0.595    0.559    
# b  2.85091    0.55210   5.164 6.52e-05 ***

# calculate RMSE for this model
hydroGOF::rmse(pinon_data$dry_mass_total_kg, predict(model.rcd.biomass, pinon_data))
# RMSE = 18.11884

# Aggregate the model residuals in a new dataframe
RCD.biomass.residuals <- subset(pinon_data, select=c(tree_id, diameter_at_base_wet, dry_mass_total_kg))
RCD.biomass.residuals$resid <- abs(resid(model.rcd.biomass))
RCD.biomass.residuals$Size_class <- ifelse(pinon_data$diameter_at_base_wet >= 20,
                                           "Large trees", "Small trees")

# make a basic plot of this data (not for manuscript)
ggplot(RCD.biomass.residuals, aes(x = Size_class, y = resid, fill = Size_class)) +
  geom_boxplot() +
  geom_jitter()

RCD.biomass.residuals %>%
  group_by(Size_class) %>%
  summarise(mean = mean(diameter_at_base_wet), sd = sd(diameter_at_base_wet))

# Size_class    mean     sd
# Large trees  21.60  0.989
# Small trees   8.74  5.680 


#Figure 2: Total biomass as a function of RCD
fig2A <- ggplot(data = pinon_data, aes(x = diameter_at_base_wet, y = dry_mass_total_kg)) +
  geom_point(na.rm = TRUE, size = 2) +
  #Darling 1967 (Table 12, also see Huang et al. 2009)
  stat_function(fun=function(x)0.024*x^2.67, geom="line", aes(colour="A"), lty = "dashed") +
  #Grier et al. 1992 (Table 2. Pinon young+mature combined total) 
  stat_function(fun = function(x) 10^(-1.468+2.582*log10(x)), aes(colour = "B"), size=0.5, lty = "dashed") +
  #Jenkins et al., 2003 (Table 1. Softwood, Pine)
  stat_function(fun = function(x) exp(-2.5356+2.4349*log(-6.8180+1.0222*x+1.8879*1)), aes(colour = "C"), size=0.5, lty = "dashed") +
  #Chojnacky et al. 2014 (Table 5. Woodland, Pinaceae)
  stat_function(fun = function(x) exp(-3.2007+2.5339*log(x)), aes(colour = "D"), size=0.5, lty = "dashed") +
  #Sprinkle & Klepac 2015
  stat_function(fun=function(x)0.62*0.45359237*2.8092*(x/2.54)^2.2884, geom="line", aes(colour="E"), lty = "dashed") +
  #This study
  stat_function(fun = function(x) (coef(summary(model.rcd.biomass))[, "Estimate"])[1]*(x)^(coef(summary(model.rcd.biomass))[, "Estimate"])[2],
                aes(colour="F"), size = 1, lty = "solid") +
  scale_color_manual(labels = c(  "Darling 1967",
                                  "Grier et al. 1992",
                                  "Jenkins et al. 2003",
                                  "Chojnacky et al. 2014",
                                  "Sprinkle & Klepac 2015",
                                  "This study"), 
                     values = c("orange",
                                "purple",
                                "red",
                                "blue", 
                                "darkgreen",
                                "black")) +
  theme_fancy() +
  theme(legend.title = element_blank(),
        legend.position = c(0.29, 0.8)) +
  scale_y_continuous(limits=c(0,200), breaks=seq(0,200,50), expand = c(0.01,0)) +
  scale_x_continuous(limits=c(0,24), breaks=seq(0,24,4), expand = c(0.01,0)) +
  xlab("RCD (cm)") +
  ylab("Dry biomass (kg)")

# Note that Jenkins equations requires measurements of DBH, thus we apply the 
# RCD to DBH conversion from Chojnacky and Rogers 1999.
# R warns that NaNs are produced, which is b/c 6 trees are < 1.3 m tall and will
# give negative predictions. However the curve looks correct. 

# Note that the Sprinkle & Klepac equation scale from inches RCD to pounds biomass
# Therefore, I have converted by dividing RCD by 2.54 (conversion for cm to in)
# then multiplying the result by 0.45359237 (conversion for lb to kg)



# max_height as predictor of total tree biomass
# The relationship between maximum height and biomass is  nonlinear (power)
# nls = nonlinear least squares, a method in R to directly fit an nonlinear model, and the default algorithm used in nls is a Gauss-Newton algorithm.
# Statistical model
model.maxheight.biomass <- nls(dry_mass_total_kg ~ a*max_height^b,
                               data = pinon_data,
                               start = list(a =1, b =1),
                               na.action=na.exclude,)
summary(model.maxheight.biomass) # Return model parameters
# Parameters:
#  Estimate Std. Error t value Pr(>|t|)    
# a   4.0871     3.5106   1.164  0.25953   
# b   1.8059     0.4891   3.692  0.00167 **

# calculate RMSE for this model
hydroGOF::rmse(pinon_data$dry_mass_total_kg, predict(model.maxheight.biomass, pinon_data))
# RMSE = 28.11426




# now plot this same model, but excluding the outlier (tree P14). Subset df.
Height_biomass_outlier <- subset(pinon_data, select=c(tree_id, max_height, dry_mass_total_kg))
Height_biomass_outlier$group <- ifelse(Height_biomass_outlier$tree_id == 'P14', 'out', 'in')
Height_biomass_no_outlier <-Height_biomass_outlier[Height_biomass_outlier$tree_id != "P14", ]   

#model biomass as a function of height excluding the P14 outlier
model.maxheight.biomass.no.outlier <- nls(dry_mass_total_kg ~ a*max_height^b,
                                          data = Height_biomass_no_outlier,
                                          start = list(a =1, b =1),
                                          na.action=na.exclude)

summary(model.maxheight.biomass.no.outlier) # Return model parameters
#  Estimate Std. Error t value Pr(>|t|)    
#a   0.9713     0.4784   2.030   0.0583 .  
#b   2.5633     0.2721   9.421 3.68e-08 ***

# calculate RMSE for this model excluding the outlier tree
hydroGOF::rmse(Height_biomass_no_outlier$dry_mass_total_kg, 
               predict(model.maxheight.biomass.no.outlier, Height_biomass_no_outlier))
# RMSE = 9.456181

#plot without the outlier
fig2B <- ggplot(data = Height_biomass_outlier, aes( x = max_height, y = dry_mass_total_kg, fill = group, color = group)) +
  geom_point(size = 2, shape = 21) +
  stat_function(fun=function(x)4.0871*x^1.8059, geom="line", color = "Red", size = 0.6, linetype = "dashed") +
  stat_function(fun = function(x) (coef(summary(model.maxheight.biomass.no.outlier))[, "Estimate"])[1]*(x)^(coef(summary(model.maxheight.biomass.no.outlier))[, "Estimate"])[2],
                size = 1, lty = "solid", color = "black") +
  scale_fill_manual(values = c("black", "white")) +
  scale_color_manual(values = c("black", "red")) +
  theme_fancy() +
  theme(legend.position = "none") +
  scale_y_continuous(limits=c(0,200), breaks=seq(0,200,50), expand = c(0.01,0)) +
  scale_x_continuous(limits=c(0,8), breaks=seq(0,8,2), expand = c(0.01,0)) +
  annotate("text", x = 2, y = 180, label = "italic(y)==0.971~italic(x)^2.563", parse = TRUE, size = 3.5) +
  annotate("text", x = 2, y = 160, label = "italic(RMSE)==9.46", parse = TRUE, size = 3.5) +
  labs(x = expression(Maximum~tree~height~(m)), 
       y = expression(Dry~biomass~(kg))) 




# Model for comparing biomass as a function of polygon derived canopy area (CA1) ####
Model_biomass_CA1 <- lm(dry_mass_total_kg ~ canopy_area, data = pinon_data)
summary(Model_biomass_CA1)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -6.099      4.635  -1.316    0.205    
# canopy_area   10.144      0.665  15.253 9.74e-12 ***

# calculate RMSE for this model
hydroGOF::rmse(pinon_data$dry_mass_total_kg, predict(Model_biomass_CA1, pinon_data))
# RMSE = 13.90078

# Model for comparing biomass as a function of orthogonal derived canopy area (CA2)
Model_biomass_CA2 <- lm(dry_mass_total_kg ~ canopy_area_from_daim, data = pinon_data)
summary(Model_biomass_CA2)
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)  
# (Intercept)            -4.6848     4.5730  -1.024    0.319    
# canopy_area_from_daim   9.6429     0.6327  15.241 9.87e-12 ***

# calculate RMSE for this model
hydroGOF::rmse(pinon_data$dry_mass_total_kg, predict(Model_biomass_CA2, pinon_data))
# RMSE = 13.91119

# Reformatting CA data for plotting with biomass
# separate the diameter variables
RCD <- data.frame(pinon_data$diameter_at_base_wet)
CA1 <- data.frame(pinon_data$canopy_area)
CA2 <- data.frame(pinon_data$canopy_area_from_daim)
Biomass <- data.frame(pinon_data$dry_mass_total_kg)
SWA <- data.frame(pinon_data$sapwood_area)
LA <- data.frame(pinon_data$LA_m2)

# combine RCD variable with disk variables
CA1_biomass <- cbind(RCD, CA1, Biomass, SWA, LA)
CA2_biomass <- cbind(RCD, CA2, Biomass, SWA, LA)

# assign the wet and dry categorical variables
CA1_biomass$Group <- "CA1"
CA2_biomass$Group <- "CA2"

#rename columns
colnames(CA1_biomass) <- c("RCD", "CA", "Biomass","SWA","LA", "Group")
colnames(CA2_biomass) <- c("RCD","CA", "Biomass","SWA","LA", "Group")

# combine into a single dataframe
CA_biomass <- rbind(CA1_biomass, CA2_biomass)

#calculate the ratio of CA to LA
CA_biomass$CA_LA_ratio <- CA_biomass$CA / CA_biomass$LA

ggplot(data = CA_biomass, aes( x = RCD, y = CA_LA_ratio)) +
  geom_point(shape = 21, size = 2)

# plot dry biomass as a function of the two canopy area estimates
fig2C <- ggplot(data = CA_biomass, aes( x = CA, y = Biomass, fill = Group )) +
  geom_point(shape = 21, size = 2) +
  geom_smooth(aes(group = Group, linetype = Group), method = "lm", formula = "y~x", color = "black", size = 0.6, fill = "grey20") +
  scale_fill_manual(values = c("black", "white")) +
  theme_fancy() +
  theme(legend.title = element_blank(),
        legend.position = c(0.4, 0.89)) +
  guides(fill=guide_legend(keywidth=0.1,keyheight=.6,default.unit="cm")) +
  guides(linetype=FALSE) +
  #this bit allows for adjusting the distance between legend symbols to align with the R2 values
  scale_y_continuous(limits=c(-20,200), breaks=seq(0,200,50), expand = c(0.01,0)) +
  scale_x_continuous(limits=c(-2,24), breaks=seq(0,20,5), expand = c(0.01,0)) +
  stat_poly_eq(formula = "y~x",
               label.x = "centre",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(stat(adj.rr.label),sep = "")), rr.digits = 3,
               label.x.npc = "left",
               parse = TRUE) +
  labs(x = expression(Canopy~area~(m^2)), 
       y = expression(Dry~biomass~(kg))) 


### combine plot objects with Patchwork
pall <- (fig2A + fig2B + fig2C) +
  plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97))

filename = "plots/Figure_2"

ggsave(
  pall,
  filename = paste0(filename, ".png"),
  width = 30,
  height = 9,
  units = "cm"
)

ggsave(
  pall,
  filename = paste0(filename, ".pdf"),
  width = 30,
  height = 9,
  units = "cm"
)


# -------------- Figure 3 --------------

# Sapwood area as a function of wet disk diameter ####
model.disk_sapwoodarea <- nls(sapwood_area ~ a*disk_diameter_wet^b,
                              data = pinon_data,
                              start = list(a =1, b =1),
                              na.action=na.exclude)
summary(model.disk_sapwoodarea) # Return model parameters
#Parameters:
#  Estimate Std. Error t value Pr(>|t|)    
#a   0.8777     0.5605   1.566    0.135    
#b   1.6948     0.2156   7.861 3.15e-07 ***

# calculate RMSE for this model
hydroGOF::rmse(pinon_data$sapwood_area, predict(model.disk_sapwoodarea, pinon_data))
# RMSE = 17.96945

# Calculate prediction intervals from function predictNLS in the propagate package
##preds.SWA <- data.frame(x = seq(1.0, 22.5, 0.5))  # create dataframe with example values for plotting
##colnames(preds.SWA) <- c("disk_diameter_wet")  # Label column.
##prop.SWA <- predictNLS(model.disk_sapwoodarea, newdata = preds.SWA)  # Predict upper and lower values

#aggregate the predicted values in a new dataframe
##preds.SWA$sapwood_area <- prop_SWA$summary[,1]  # Mean prediction
##preds.SWA$Lower.CI <- prop_SWA$summary[,5] # Lower bounds of the CI (2.5%)
##preds.SWA$Upper.CI <- prop_SWA$summary[,6] # Upper bounds of the CI (97.5%) 
#These CI values are also huge at the upper end. This is odd, b/c the 95% CI
#for a linear model appears much tighter, despite it being a worse fit...

#Figure: Total biomass as a function of RCD
fig3A <- ggplot(data = pinon_data, aes(x = disk_diameter_wet, y = sapwood_area)) +
  geom_point(na.rm = TRUE, size = 2) +
  stat_function(fun=function(x)0.8112*x^1.7341, geom="line", aes(color = "B"), size = 0.6, linetype = "dashed") +
  stat_function(fun=function(x)5.3*x-15, geom="line", aes(color = "C"), size = 0.6, linetype = "dashed") +
  stat_function(fun = function(x) (coef(summary(model.disk_sapwoodarea))[, "Estimate"])[1]*(x)^(coef(summary(model.disk_sapwoodarea))[, "Estimate"])[2],
                size = 1, aes(color = "A")) +
  scale_color_manual(labels = c("This study","Pangle et al. 2015","West et al. 2008"), 
                     values = c("black", "red","blue")) +
  theme_fancy() +
  theme(legend.title = element_blank(),
        legend.position = c(0.3, 0.65)) +
  scale_y_continuous(limits=c(0,200), breaks=seq(0,200,50), expand = c(0.01,0)) +
  scale_x_continuous(limits=c(0,24), breaks=seq(0,24,4), expand = c(0.01,0)) +
  labs(y = expression(Sapwood~area~(cm^2)), 
       x = expression(Disk~diameter~(cm))) +
  guides(size = FALSE, linetype = FALSE) +
  annotate("text", x = 6, y = 180, label = "italic(y)==0.877~italic(x)^1.695", parse = TRUE, size = 3.5) +
  annotate("text", x = 6, y = 160, label = "italic(RMSE)==17.969", parse = TRUE, size = 3.5) 



# Sapwood area as a function of canopy area ####
# (my thinking here is that drone photography could be used to estimate stand SWA)

# Statistical linear model for CA1
model.SWA.CA1 <- lm(sapwood_area ~ canopy_area, data = pinon_data)
summary(model.SWA.CA1)

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   12.595      9.631   1.308    0.207    
#canopy_area   11.196      1.382   8.102 2.05e-07 ***
#Residual standard error: 30.45 on 18 degrees of freedom
#Multiple R-squared:  0.7848,	Adjusted R-squared:  0.7728 
#F-statistic: 65.64 on 1 and 18 DF,  p-value: 2.047e-07

# calculate RMSE for this model
hydroGOF::rmse(pinon_data$sapwood_area, predict(model.SWA.CA1, pinon_data))
# RMSE = 28.88625

# Statistical linear model for CA2
model.SWA.CA2 <- lm(sapwood_area ~ canopy_area_from_daim, data = pinon_data)
summary(model.SWA.CA2)

#Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             14.506      9.720   1.492    0.153    
#canopy_area_from_daim   10.574      1.345   7.863 3.13e-07 ***
#Residual standard error: 31.17 on 18 degrees of freedom
#Multiple R-squared:  0.7745,	Adjusted R-squared:  0.762 
#F-statistic: 61.82 on 1 and 18 DF,  p-value: 3.133e-07

# calculate RMSE for this model
hydroGOF::rmse(pinon_data$sapwood_area, predict(model.SWA.CA2, pinon_data))
# RMSE = 29.56803

#plot total sapwood area as a function of canopy areas
fig3B <- ggplot(CA_biomass, aes(x=CA, y=SWA, group = Group, fill = Group)) +
  geom_point(shape = 21, size = 2) +
  geom_smooth(aes(group = Group, linetype = Group), method = "lm", formula = "y~x", color = "black", size = 0.6, fill = "grey20") +
  scale_fill_manual(values = c("black", "white")) +
  theme_fancy() +
  theme(legend.title = element_blank(),
        legend.position = c(0.4, 0.89)) +
  guides(fill=guide_legend(keywidth=0.1,keyheight=.6,default.unit="cm")) +
  guides(linetype=FALSE) +
  scale_y_continuous(limits=c(-10,250), breaks=seq(0,200,50), expand = c(0.01,0)) +
  scale_x_continuous(limits=c(0,20), breaks=seq(0,20,5), expand = c(0.01,0)) +
  stat_poly_eq(formula = "y~x",
               label.x = "centre",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(stat(adj.rr.label),sep = "")), rr.digits = 3,
               label.x.npc = "left",
               parse = TRUE) +
  labs(x = expression(Canopy~area~(m^2)), 
       y = expression(Sapwood~area~(cm^2))) 




### combine plot objects with Patchwork
pall <- (fig3A + fig3B) +
  plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97))

filename = "plots/Figure_3"

ggsave(
  pall,
  filename = paste0(filename, ".png"),
  width = 21,
  height = 9,
  units = "cm"
)

ggsave(
  pall,
  filename = paste0(filename, ".pdf"),
  width = 21,
  height = 9,
  units = "cm"
)



# -------------- Figure 4 --------------

# Subset trees with leaf area data ####
LA_data <- subset(pinon_data, select=c(tree_id,diameter_at_base_wet,LA_m2,
                                       canopy_area, canopy_area_from_daim,
                                       sapwood_area,max_height))
LA_data <- LA_data[!is.na(LA_data$LA_m2), ]

# Calculate the Huber value (HV,AL:AS) for trees with measured leaf area
LA_data$HV <- LA_data$LA_m2 / LA_data$sapwood_area

ggplot(LA_data, aes(x=max_height, y=HV)) +
  geom_point(size=2)

# total leaf area as a function of RCD
# Statistical model
model.RCD.LA <- nls(LA_m2 ~ a*diameter_at_base_wet^b,
                    data = LA_data,
                    start = list(a =1, b =1),
                    na.action=na.exclude,)
summary(model.RCD.LA) # Return model parameters
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# a 0.004025   0.002398   1.678  0.19189    
# b 3.215904   0.203406  15.810  0.00055 ***

# calculate RMSE for this model
hydroGOF::rmse(LA_data$LA_m2, predict(model.RCD.LA, LA_data))
# RMSE = 0.9690882 wicked good!


#plot this model with the Pangel et al. 2015 model
fig4A <- ggplot(pinon_data, aes(x=diameter_at_base_wet, y=LA_m2)) +
  geom_point(size=2) +
  stat_function(fun = function(x) (coef(summary(model.RCD.LA))[, "Estimate"])[1]*(x)^(coef(summary(model.RCD.LA))[, "Estimate"])[2],
                size = 1, lty = "solid", aes(color = "A")) +
  scale_color_manual(labels = c("This study","Pangle et al. 2015"), 
                     values = c("black", "red")) +
  stat_function(fun=function(x)0.0889*x^1.9172, geom="line", aes(color = "B"),
                size = 0.6, linetype = "dashed", xlim = c(0,20)) +
  theme_fancy() +
  theme(legend.title = element_blank(),
        legend.position = c(0.3, 0.70)) +
  scale_y_continuous(limits=c(0,60), breaks=seq(0,60,10), expand = c(0.01,0)) +
  scale_x_continuous(limits=c(0,24), breaks=seq(0,24,4), expand = c(0.01,0)) +
  labs(y = expression(Leaf~area~(m^2)), 
       x = expression(RCD~(cm))) +
  guides(size = FALSE, linetype = FALSE) +
  annotate("text", x = 6, y = 56, label = "italic(y)==0.004~italic(x)^3.216", parse = TRUE, size = 3.5) +
  annotate("text", x = 6, y = 52, label = "italic(RMSE)==0.969", parse = TRUE, size = 3.5)


# compute where our function intersects with the Pangle 2015 function ####
fun1 <- function(x)0.0889*x^1.9172      # define Pangle function
fun2 <- function(x)0.004025*x^3.215904  # define our function
# calculate where these 2 functions intersect
rt <- uniroot(function(x)  fun1(x) - fun2(x)  , c(.01,20), tol=1e-8) 
rt # Models intersect at RCD= 10.83 cm


# Total leaf area as a function of sapwood area ####
# Statistical model
model.SWA.LA <- lm(LA_m2 ~ sapwood_area, data = LA_data)
summary(model.SWA.LA)
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -0.91664    1.05374   -0.87 0.448369    
# sapwood_area  0.41301    0.01661   24.87 0.000143 ***
# Residual standard error: 1.864 on 3 degrees of freedom
# Multiple R-squared:  0.9952,	Adjusted R-squared:  0.9936 
# F-statistic: 618.5 on 1 and 3 DF,  p-value: 0.0001426

# calculate RMSE for this model
hydroGOF::rmse(LA_data$LA_m2, predict(model.SWA.LA, LA_data))
# RMSE = 1.444106

#plot total leaf area as a function of total sapwood area
fig4B <- ggplot(pinon_data, aes(x=sapwood_area, y=LA_m2)) +
  geom_point() +
  geom_smooth(method="lm", formula = y~x, color="black") +
  theme_fancy() +
  scale_y_continuous(limits=c(-5,65), breaks=seq(0,60,10), expand = c(0.01,0)) +
  scale_x_continuous(limits=c(-10,160), breaks=seq(0,240,20), expand = c(0.01,0)) +
  labs(y = expression(Leaf~area~(m^2)), 
       x = expression(Sapwood~area~(cm^2))) +
  stat_poly_eq(aes(label = paste("atop(", stat(adj.rr.label), ",", stat(eq.label), ")", sep = "")), 
               formula = "y~x", 
               parse = TRUE) #insert R2 and equation into the plot


# Models for comparing leaf area as a function of polygon derived canopy areas ####
#CA1
Model_LA_CA1 <- lm(LA_m2 ~ canopy_area, data = pinon_data)
summary(Model_LA_CA1)
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -1.8770     1.1675  -1.608 0.206254    
#canopy_area   7.5848     0.3305  22.953 0.000181 ***

# calculate RMSE for this model
hydroGOF::rmse(pinon_data$LA_m2, predict(Model_LA_CA1, pinon_data, na.rm=TRUE))
# RMSE = 1.564014

#CA2
Model_LA_CA2 <- lm(LA_m2 ~ canopy_area_from_daim, data = pinon_data)
summary(Model_LA_CA2)
#Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            -1.9025     1.2205  -1.559 0.216930    
#canopy_area_from_daim   7.7406     0.3524  21.965 0.000207 ***

# calculate RMSE for this model
hydroGOF::rmse(pinon_data$LA_m2, predict(Model_LA_CA2, pinon_data, na.rm=TRUE))
# RMSE = 1.633894

#subset a new dataframe only for trees with LA data
LA_subset <- na.omit(CA_biomass)

# summarize the LA and CA data by group
LA_CA_summary <- LA_subset %>%
  group_by(RCD) %>%
  summarise_at(vars(CA,LA), 
               funs(mean(., na.rm = TRUE)))

# calculate the ratio of canopy area to leaf area (LA/CA)
LA_CA_summary$CA_LA <- LA_CA_summary$CA / LA_CA_summary$LA
# CA:LA = 0.33 for both CA1 and CA2

CA_biomass$CA_LA_ratio <- CA_biomass$CA / CA_biomass$LA

# plot total leaf area as a function of the two canopy area estimates
fig4C <- ggplot(data = CA_biomass, aes( x = CA, y = LA, fill = Group )) +
  geom_point(shape = 21, size = 2) +
  geom_smooth(aes(group = Group, linetype = Group), method = "lm", formula = "y~x", color = "black", size = 0.6, fill = "grey20") +
  scale_fill_manual(values = c("black", "white")) +
  theme_fancy() +
  theme(legend.title = element_blank(),
        legend.position = c(0.4, 0.89)) +
  guides(fill=guide_legend(keywidth=0.1,keyheight=.6,default.unit="cm")) +
  guides(linetype=FALSE) +
  #this bit allows for adjusting the distance between legend symbols so I can align with the R2 values
  scale_y_continuous(limits=c(-5,65), breaks=seq(0,60,10), expand = c(0.01,0)) +
  scale_x_continuous(limits=c(0,8), breaks=seq(0,8,2), expand = c(0.01,0)) +
  stat_poly_eq(formula = "y~x",
               label.x = "centre",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(stat(adj.rr.label),sep = "")), rr.digits = 3,
               label.x.npc = "left",
               parse = TRUE) +
  labs(x = expression(Canopy~area~(m^2)), 
       y = expression(Leaf~area~(m^2))) 


### combine plot objects with Patchwork
pall <- (fig4A + fig4B + fig4C) +
  plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97))

filename = "plots/Figure_4"

ggsave(
  pall,
  filename = paste0(filename, ".png"),
  width = 30,
  height = 9,
  units = "cm"
)

ggsave(
  pall,
  filename = paste0(filename, ".pdf"),
  width = 30,
  height = 9,
  units = "cm"
)




#-------------------------------------------------------------------------------
# Model for total tree biomass as a function of the field measured BA
Model_BAwet <- lm(dry_mass_total_kg ~ base_ba_wet, data = pinon_data)
summary(Model_BAwet)
# Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -7.40437    6.59978  -1.122    0.277    
# base_ba_wet  0.33130    0.03106  10.666 3.28e-09 ***

#-------------------------------------------------------------------------------
# Model for total tree biomass as a function of the fresh disk measured BA
Model_disk_BAwet <- lm(dry_mass_total_kg ~ disk_diameter_wet, data = pinon_data)
summary(Model_disk_BAwet)
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -30.6859    11.1847  -2.744   0.0134 *  
# disk_diameter_wet   6.4158     0.8209   7.815 3.41e-07 ***

#-------------------------------------------------------------------------------
# Model for total tree biomass as a function of the dry disk measured BA
Model_disk_BAdry <- lm(dry_mass_total_kg ~ disk_diameter_dry, data = pinon_data)
summary(Model_disk_BAdry)
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -29.6225    11.2213  -2.640   0.0166 *  
# disk_diameter_dry   6.7203     0.8725   7.702 4.19e-07 ***



#-------------------------------------------------------------------------------
# Model for comparing RCD to field measured height
Model_RCD_height <- lm(max_height ~ diameter_at_base_wet, data = pinon_data) 
summary(Model_RCD_height) 
# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -0.04173    0.25048  -0.167     0.87    
# diameter_at_base_wet  0.27595    0.01784  15.469 7.69e-12 ***

# calculate RMSE for this model
hydroGOF::rmse(pinon_data$max_height, predict(Model_RCD_height, pinon_data))
# RMSE = 0.5561635



# Model for comparing max height to diameter
ggplot(pinon_data, aes(x = diameter_at_base_wet, y = max_height)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", formula = y~ x, color = "black") +
  theme_classic() +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
        axis.title = element_text(size = 10),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size = 0.8)) +
  scale_y_continuous(limits=c(-0.2,7), breaks=seq(0,6,2), expand = c(0,0)) +
  scale_x_continuous(limits=c(-0.5,24), breaks=seq(0,24,5), expand = c(0,0)) +
  xlab("RCD (cm)") +
  ylab("Maximum tree height  (m)") +
  stat_poly_eq(aes(label = paste("atop(", stat(adj.rr.label), ",", stat(eq.label), ")", sep = "")), 
               formula = "y~x", 
               parse = TRUE) #insert R2 and equation into the plot

# Save the figure to WD
# ggsave("plots/RCD_maxheight.tiff", width = 10, height = 10, units = "cm", dpi = 500)

#-------------------------------------------------------------------------------
# Model for comparing bark-thickness to RCD
Model_barkthickness <- lm(bark_thickness ~ disk_diameter_wet, data = pinon_data)
summary(Model_barkthickness)
#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       0.129403   0.063568   2.036   0.0568 .  
#disk_diameter_wet 0.055105   0.004666  11.811 6.52e-10 ***

# calculate RMSE for this model
hydroGOF::rmse(pinon_data$bark_thickness, predict(Model_barkthickness, pinon_data))
# RMSE = 0.1406549

# here's a more 'manual' way to compute the RMSE to verify...
RSS <- c(crossprod(Model_barkthickness$residuals))
MSE <- RSS / length(Model_barkthickness$residuals)
RMSE <- sqrt(MSE)
RMSE
# RMSE = 0.1406549

# Plot drone derive height as a function of measured height
ggplot(data = pinon_data, aes( x = disk_diameter_wet, y = bark_thickness )) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", formula = "y~x", color = "black") +
  theme_classic() +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
        axis.title = element_text(size = 10),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size = 0.8)) +
  scale_y_continuous(limits=c(0,2.2), breaks=seq(0,3,0.5), expand = c(0.01,0)) +
  scale_x_continuous(limits=c(0,24), breaks=seq(0,24,4), expand = c(0.01,0)) +
  xlab("Disk diameter (cm)") +
  ylab("Bark thickness (cm)") +
  stat_poly_eq(aes(label = paste("atop(", stat(adj.rr.label), ",", stat(eq.label), ")", sep = "")), 
               formula = "y~x", 
               parse = TRUE) #insert R2 and equation into the plot

# save the figure to WD
# ggsave("plots/barkthickness.tiff", width = 10, height = 10, units = "cm", dpi = 500)

#-------------------------------------------------------------------------------

# summarize wet disk data
pinon_data %>%
  summarise(mean_rcd = mean(diameter_at_base_wet), sd_rcd = sd(diameter_at_base_wet),
            min_rcd = min(diameter_at_base_wet), max_rcd = max(diameter_at_base_wet))
# mean_rcd   sd_rcd min_rcd max_rcd
#   11.965 7.539459     1.3    22.7

pinon_data %>%
  summarise(mean_wet = mean(disk_diameter_wet), sd_wet = sd(disk_diameter_wet),
            min_wet = min(disk_diameter_wet), max_wet = max(disk_diameter_wet))
# mean_wet   sd_wet min_wet max_wet
#   11.625 7.290215     1.2    22.3

# summarize dry disk data
pinon_data %>%
  summarise(mean_dry = mean(disk_diameter_dry), sd_dry = sd(disk_diameter_dry),
            min_dry = min(disk_diameter_dry), max_dry = max(disk_diameter_dry))
# mean_dry   sd_dry min_dry max_dry
#    10.94 6.936509     1.1    20.7

# percent difference between RCD and wet disk diameter 
((11.965 - 11.625) / 11.965) * 100
# 2.841621%

# percent difference between wet disk diameter and dry disk diameter 
((11.625 - 10.94) / 11.625) * 100
# 5.892473%

# Testing difference between the wet and dry disk diameter
disk_difference <- pinon_data$disk_diameter_wet - pinon_data$disk_diameter_dry
# Assessing data distribution
length(disk_difference)
qqnorm(disk_difference)
qqline(disk_difference)       
shapiro.test(disk_difference)
# W = 0.87833, p-value = 0.01651
# these data are signifcantly non-normal, so a non-parametric test is needed.
# Wilcoxon signed-rank test
wilcox.test(pinon_data$disk_diameter_wet, pinon_data$disk_diameter_dry,
            alternative=c("two.sided", "less", "greater"))
# Result: wet and dry disk diameters are not significantly different (p=0.57)

# Model for comparing wet and dry disk diameters
Model_disk_wetdry <- lm(disk_diameter_dry ~ disk_diameter_wet, data = pinon_data)
summary(Model_disk_wetdry)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -0.112872   0.116967  -0.965    0.347    
# disk_diameter_wet  0.950785   0.008585 110.750   <2e-16 ***


# Total Least Squares Regression comparing RCD to wet disk
pca <- prcomp(~diameter_at_base_wet+disk_diameter_wet, pinon_data)
slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
slp # 0.9668767
int # 0.0563201  

# Compute the Lin's correlation concordance coefficient comparing RCD to wet disk
CCC(pinon_data$diameter_at_base_wet, pinon_data$disk_diameter_wet, 
    ci = "z-transform",conf.level = 0.95)
#        est    lwr.ci    upr.ci
#  0.9963482 0.9916822 0.9983989

# Total Least Squares Regression comparing RCD to dry disk
pca <- prcomp(~diameter_at_base_wet+disk_diameter_dry, pinon_data)
slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
slp # 0.9199422
int # -0.06710851   

# Compute the Lin's correlation concordance coefficient comparing RCD to wet disk
CCC(pinon_data$diameter_at_base_wet, pinon_data$disk_diameter_dry, 
    ci = "z-transform",conf.level = 0.95)
#        est    lwr.ci    upr.ci
#  0.9850488 0.9710912 0.9922938


# Reformatting RCD data for plotting comparison with disk diameters
# separate the diameter variables
RCD_wet <- data.frame(pinon_data$diameter_at_base_wet)
Disk_wet <- data.frame(pinon_data$disk_diameter_wet)
Disk_dry <- data.frame(pinon_data$disk_diameter_dry)

# combine RCD variable with disk variables
Disk_wet <- cbind(RCD_wet, Disk_wet)
Disk_dry <- cbind(RCD_wet, Disk_dry)

# assign the wet and dry categorical variables
Disk_wet$Group <- "Wet"
Disk_dry$Group <- "Dry"

# rename columns
colnames(Disk_wet) <- c("RCD", "Disk_diam", "Group")
colnames(Disk_dry) <- c("RCD", "Disk_diam", "Group")

# combine into a single dataframe
RCD_compare <- rbind(Disk_wet, Disk_dry)

# plots the disk diameters (wet & dry) as a function of RCD
ggplot(data = RCD_compare, aes( x = RCD, y = Disk_diam, fill = Group )) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  geom_point(shape = 21, size = 2) +   
  geom_abline(intercept = 0.0563201, slope = 0.9668767, color="red", 
              linetype="solid", size= 0.5) +
  geom_abline(intercept = -0.06710851, slope = 0.9199422, color="blue", 
              linetype="solid", size= 0.5) +
  scale_fill_manual(values = c("black", "white")) +
  theme_classic() +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
        axis.title = element_text(size = 10),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size = 0.8),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1,"line"),
        legend.position = c(0.4, 0.88),
        legend.background=element_blank()) +
  guides(fill=guide_legend(
    keywidth=0.1,
    keyheight=.7,
    default.unit="cm")
  ) +
  scale_y_continuous(limits=c(0,24), breaks=seq(0,24,4), expand = c(0.01,0)) +
  scale_x_continuous(limits=c(0,24), breaks=seq(0,24,4), expand = c(0.01,0)) +
  xlab("RCD (cm)") +
  ylab("Disk diameter (cm)") +
  annotate("text", x = 4, y = 22, label = "italic(CCC)==0.996", parse = TRUE, size = 3.5) +
  annotate("text", x = 4, y = 20, label = "italic(CCC)==0.985", parse = TRUE, size = 3.5)

# save the figure to WD
# ggsave("plots/RCD_disk_compare_TLS.tiff", width = 10, height = 10, units = "cm", dpi = 500)

#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
# Relationship of RCD to biomass proportions

# In theory, you could make the case for the relationship in either direction,
# (i.e. using the <3 OR >3 portion) but, since 4 trees do not have a > 3 cm 
# sub-sample it's best to use the smaller stuff to include n=20 data points. 

model.rcd.smallportion <- lm(proportion_small_biomass ~ diameter_at_base_wet,
                             data = pinon_data)
summary(model.rcd.smallportion) # Return model parameters
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           1.007323   0.043985  22.902 9.18e-15 ***
# diameter_at_base_wet -0.030332   0.003133  -9.683 1.46e-08 ***

# calculate RMSE for this model
hydroGOF::rmse(pinon_data$proportion_small_biomass, predict(model.rcd.smallportion, pinon_data))
# RMSE = 0.09766312

# Figure: Proportion of dry mass <3 cm as a function of RCD
ggplot(data = pinon_data, aes(x = diameter_at_base_wet, y = proportion_small_biomass)) +
  geom_point(na.rm = TRUE, size = 2) +
  geom_smooth(method = "lm", formula = y~x, color = "black") +
  theme_classic() +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
        axis.title = element_text(size = 10),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size = 0.8),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1,"line"),
        legend.position = c(0.4, 0.89),
        legend.background=element_blank()) +
  guides(fill=guide_legend(keywidth=0.1,keyheight=.6,default.unit="cm")) +
  guides(linetype=FALSE) +
  #this bit allows for adjusting the distance between legend symbols so I can allign with the R2 values
  scale_y_continuous(limits=c(0,1.1), breaks=seq(0,1,0.2), expand = c(0.01,0),
                     labels = scales::percent) +
  scale_x_continuous(limits=c(-2,24), breaks=seq(0,20,5), expand = c(0.01,0)) +
  stat_poly_eq(formula = "y~x",
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(stat(adj.rr.label),sep = "")), rr.digits = 3,
               parse = TRUE,
               label.x = 0.75,
               label.y = 0.95) +
  xlab(("RCD (cm)")) +
  ylab(("Proportion of biomass < 3 cm (cm)")) +
  annotate("text", x = 15, y = 0.9, label = "italic(RMSE)==0.098", parse = TRUE, size = 3.5) 

#save the figure to WD
# ggsave("plots/RCD_biomass_proportion.tiff", width = 10, height = 10, units = "cm", dpi = 500)









#This plot shows what a generalized equation might look like (this study + Pangle)
ggplot(data = pinon_data, aes(x = disk_diameter_wet, y = sapwood_area)) +
  geom_point(na.rm = TRUE, size = 2) +
  stat_function(fun=function(x)0.8777*x^1.6948, geom="line", aes(color = "A"), size = 0.6, linetype = "solid") +
  stat_function(fun=function(x)0.8112*x^1.7341, geom="line", aes(color = "B"), size = 0.6, linetype = "solid") +
  stat_function(fun=function(x)0.84445*x^1.71445, geom="line", aes(color = "C"), size = 0.6, linetype = "dashed") +
  scale_color_manual(labels = c("This study","Pangle et al. 2015","Generalized"), 
                     values = c("red", "blue","black")) +
  theme_classic() +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
        axis.title = element_text(size = 10),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size = 0.8),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1,"line"),
        legend.position = c(0.3, 0.65),
        legend.background=element_blank()) +
  scale_y_continuous(limits=c(0,200), breaks=seq(0,200,50), expand = c(0.01,0)) +
  scale_x_continuous(limits=c(0,24), breaks=seq(0,24,4), expand = c(0.01,0)) +
  labs(y = expression(Sapwood~area~(cm^2)), 
       x = expression(Disk~diameter~(cm))) +
  guides(size = FALSE, linetype = FALSE)

# examining residuals (ABS)      
model.disk.SWA.res <-  resid(model.disk_sapwoodarea)

# Add residuals to dataframe, and compute normalised residuals (for plotting).
pinon_data$swa_residuals  <- model.disk.SWA.res
pinon_data$normalised_swa_residuals <- (pinon_data$swa_residuals/pinon_data$sapwood_area)

# plot the SWA residuals as a function of wet stem diameter
ggplot(data = pinon_data, aes(x=disk_diameter_wet, y=normalised_swa_residuals)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_classic() +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
        axis.title = element_text(size = 10),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size = 0.8)) +
  scale_y_continuous(limits=c(-1.2,1), breaks=seq(-1.5,1,0.5), expand = c(0.01,0)) +
  scale_x_continuous(limits=c(0,24), breaks=seq(0,24,4), expand = c(0.01,0)) +
  xlab("Disk diameter (cm)") +
  ylab("SWA normalized residuals")

#save the figure to WD
# ggsave("plots/SWA residuals.tiff", width = 10, height = 10, units = "cm", dpi = 500)

# ------------------------------------------------------------------------------





LA_estimated <- subset(pinon_data, select=c(tree_id,diameter_at_base_wet,LA_m2,
                                            sapwood_area,max_height))

LA_estimated$LA_estimated <- ifelse(is.na(LA_estimated$LA_m2), 
                                    0.004*LA_estimated$diameter_at_base_wet^3.216,
                                    LA_estimated$LA_m2)

LA_estimated$Group <- ifelse(is.na(LA_estimated$LA_m2), 
                             "Calculated",
                             "Measured")

LA_estimated$HV <- LA_estimated$sapwood_area / LA_estimated$LA_estimated

ggplot(LA_estimated, aes(x=sapwood_area, y=LA_estimated,fill=Group)) +
  geom_point(size=2,shape=21)

# RCD 
LA_estimated %>%
  group_by(Group) %>%
  summarise(mean_HV = mean(HV), sd_HV = sd(HV),
            min_HV = min(HV), max_HV = max(HV))

# mean_HV     sd_HV     min_HV    max_HV
# Calculated    8.13  9.41   1.44  37.6 
# Measured      4.04  1.84   2.35   6.77







#------------------------------------------------------------------------------
#Tabulate variable means and standard deviations

#RCD 
pinon_data %>%
  summarise(mean_RCD = mean(diameter_at_base_wet), sd_RCD = sd(diameter_at_base_wet),
            min_RCD = min(diameter_at_base_wet), max_RCD = max(diameter_at_base_wet))

#  mean_RCD   sd_RCD min_RCD max_RCD
#     11.965 7.539459     1.3    22.7

#Height 
pinon_data %>%
  summarise(mean_Height = mean(max_height), sd_Height = sd(max_height),
            min_Height = min(max_height), max_Height = max(max_height))

#  mean_Height sd_Height min_Height max_Height
#         3.26  2.157338        0.3        6.7

#Die-back 
pinon_data %>%
  summarise(mean_Dieback = mean(dieback_pc), sd_Dieback = sd(dieback_pc),
            min_Dieback = min(dieback_pc), max_Dieback = max(dieback_pc)) 

#  mean_Dieback sd_Dieback min_Dieback max_Dieback
#             8   8.175702           0          30

#Live crown ration
pinon_data %>%
  summarise(mean_LCR = mean(live_crown_ratio), sd_LCR = sd(live_crown_ratio),
            min_LCR = min(live_crown_ratio), max_LCR = max(live_crown_ratio))

# mean_LCR    sd_LCR min_LCR max_LCR
#    0.864 0.1455082    0.63       1

#------------------------------------------------------------------------------
# Evaluate model residuals with other observed variables

# Extract absolute value of residuals from the biomass models
height_resid <- abs(resid(model.maxheight.biomass))
CA1_resid <- abs(resid(Model_biomass_CA1))
CA2_resid <- abs(resid(Model_biomass_CA2))
RCD_resid <- abs(resid(model.rcd.biomass))


# Aggregate residuals in new dataframe with tree die-back and LCR
Biomass_resid <- subset(pinon_data, select=c(tree_id,dieback_pc,live_crown_ratio,max_height))
Biomass_resid$height_resid <- height_resid
Biomass_resid$CA1_resid <- CA1_resid
Biomass_resid$CA2_resid <- CA2_resid
Biomass_resid$RCD_resid <- RCD_resid

# Visual assessment does not reveal any correlation with model deviations and LCR (or die-back). 


# create bins for the die_back category in intervals of 10%
Biomass_resid$db_bin <- ifelse(Biomass_resid$dieback_pc >= 0 & Biomass_resid$dieback_pc < 10, '0-10%',
                               ifelse(Biomass_resid$dieback_pc >= 10 & Biomass_resid$dieback_pc < 20, '10-20%', 
                                      ifelse(Biomass_resid$dieback_pc >= 20, '20-30%', "NA")))

# Box plot of CA1~Biomass residuals to to canopy dieback
ggplot(Biomass_resid, aes(x = db_bin, y = CA1_resid)) +
  geom_boxplot()

# conduct ANOVA across the bins of dieback for CA1 residuals 
CA1_dieback_anova <- aov(CA1_resid ~ db_bin, data = Biomass_resid)
summary(CA1_dieback_anova)

#            Df Sum Sq Mean Sq F value Pr(>F)
# db_bin       2  327.6   163.8    1.59  0.233 ns
# Residuals   17 1751.4   103.0  
# This is not significant (perhaps it would be w/ more sampling)

# Boxplot of RCD~biomass residuals to to canopy dieback
ggplot(Biomass_resid, aes(x = db_bin, y = RCD_resid)) +
  geom_boxplot()

# conduct ANOVA across the bins of dieback for RCD~biomass residuals 
RCD_dieback_anova <- aov(RCD_resid ~ db_bin, data = Biomass_resid)
summary(RCD_dieback_anova)





# save the final data table as a csv file
write.csv(pinon_data,"all_data.csv")
