# Reproducibility
## Project investigating the reproducibility of canopy heights derived from drone acquired structure-from-motion photogrammetry.
This project aims to answer the research question:  
How do wind speed and solar illumination affect the reconstruction of canopy height and prediction of aboveground biomass with structure-from-motion photogrammetry?    
We captured 61 UAV SfM surveys at the same study area under a range of wind/sun/cloud conditions over a 24-day period during 2021 and used generalised linear mixed effects models to test how the structural reconstructions varied with environmental conditions. 

![readme figure](https://github.com/TESS-Laboratory/Reproducibility/assets/71012708/787487fa-9f22-4b8e-bdf0-002d7b14a067)

### This Repo contains the following folders:  
"Manuscript_Scripts" folder contains the final GLMER models and other analysis with plots and annotation developed to produce figures for the Manuscript.  
"metashape_automation" contains the python scripts used to automate the processing of raw drone image data into point clouds using  Agisoft Metashape software. 
"HAG" folder contains scripts used to calculate DTMs and Canopy Heights for each plot and each survey.  
"survey_conditions" folder contains scripts used to investigate and plot survey conditions.    
"data" folder contains survey, plot and, extracted canopy height data.  
"output_data" folder contains figures and plots generated by scripts.    
"GCP" folder contains the scripts used to extract the elevation of the GCP surface from the survey point clouds and compare with GNSS measured elevation.  
"Analysis" folder contains scripts used to analyse the relationship between reconstructed canopy height variation and wind and illumination conditions.  

Analysis and results submitted for publication in Remote Sensing for Ecology and Conservation:    
### Repeated drone photogrammetry surveys demonstrate that reconstructed canopy heights are sensitive to wind speed but relatively insensitive to illumination conditions.  
### Contributors: Glenn Slade*, Karen Anderson, Hugh A. Graham, Andrew M. Cunliffe  

