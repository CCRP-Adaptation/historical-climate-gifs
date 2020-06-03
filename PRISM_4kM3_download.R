
# CODE
library(raster)
library(prism)
rm(list=ls())

## PRISM download

options(prism.path = 'C:/Users/achildress/Documents/Data_Visualization/2020_Test/PRISM_Extract/tmean') # set directory where data will be downloaded

get_prism_annual(type = 'tmean', years = 1940:1950, keepZip = FALSE) # data download (.bil files) # this command/argument changes depending on whether you want annual, monthly, tmax, tmin, etc.

options(prism.path = 'C:/Users/achildress/Documents/Data_Visualization/2020_Test/PRISM_Extract/ppt') # set directory where data will be downloaded

get_prism_annual(type = 'ppt', years = 1940:1950, keepZip = FALSE) # data download (.bil files) # this command/argument changes depending on whether you want annual, monthly, tmax, tmin, etc.


BAND<-prism_slice(c(-106.3317004,35.77894938),ls_prism_data()[,1]) #plots slice of data from single location from list of prism files
BAND2<-BAND$data
rownames(BAND2)<-NULL
colnames(BAND2)<-BAND$labels
BAND2
