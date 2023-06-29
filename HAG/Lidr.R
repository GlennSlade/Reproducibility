#This script is currently a working test bed for experimenting with LidR package
install.packages("lidR")
library (lidR)
library (rgl)
las <- readLAS("C:/Workspace/Reproducibility/20210623_P4RTK_Test_Survey_Boldventure/bold_testsubset_5cm.las", select = "xyzc")  # load XYZc only
print(las)
las_check(las) # note high number of duplicate points in current file
plot(las)# 
