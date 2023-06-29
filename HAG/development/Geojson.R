# Libraries
library(tidyverse)
library(viridis)
library(geojsonR)

setwd ("C:/Workspace/Reproducibility")

#-------1. Read in CSV File --------

DF <- read.csv("C:/Workspace/Reproducibility/harvest/GCP.csv")
dim (DF)
summary (DF)
head (DF)
str(DF)
options(digits=10)
View (DF) 


