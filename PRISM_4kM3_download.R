
# CODE
library(raster)
library(prism)
rm(list=ls())

## PRISM download

options(prism.path = 'C:/Users/achildress/Documents/Data_Visualization/2020_Test/PRISM_Extract/tmean') # set directory where data will be downloaded

get_prism_annual(type = 'tmean', years = 1895:2019, keepZip = FALSE) # data download (.bil files) # this command/argument changes depending on whether you want annual, monthly, tmax, tmin, etc.

options(prism.path = 'C:/Users/achildress/Documents/Data_Visualization/2020_Test/PRISM_Extract/ppt') # set directory where data will be downloaded

get_prism_annual(type = 'ppt', years = 1895:2019, keepZip = FALSE) # data download (.bil files) # this command/argument changes depending on whether you want annual, monthly, tmax, tmin, etc.

