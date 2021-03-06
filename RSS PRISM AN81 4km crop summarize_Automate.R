#   PRISM_AN81_4km_crop_summarize vxx.R
#   John Gross
#   30 Oct 2015

###   Check file name length (~ line 87) for Windows (38?) vs Mac (37)

#   Input is single point, same as for extracting projection data.
#   This code buffers the point. Default is one cell buffer (e.g. averaged over 3x3 grid)

#  v01.1 - minor changes to variable names, cleaned up namespace.  16 Nov. Runs without error.
#  v01 - Robust and running

# Run time: iMac ~ 15 min w/ USB3 external drive; office PC: ~ 20 min w/ USB 2.0 (same drive)

library(rgdal)
library(raster)
library(fields)   # for image.plot
library(ggplot2)
library(WriteXLS)
rm(list = ls()) 

# to recover earlier session set WD and
# load("HOBE_32.9375_-85.6875_PptTminTmax_IntermediateFiles.RData")

#############  Initialize   ##############

#  for output file names
Centroids<-read.csv("~/Data_Visualization/nps_boundary_centroids/NPS_Centroid_USE.csv",header=T)
Centroids$Lat<-as.numeric(as.character(Centroids$Lat))
for (i in 1:length(Centroids$SiteID)){
SiteID <- Centroids$SiteID[i]  # identifier.  Use "" if not desired

# use center coordinate ONLY: .0625, .1875, .3125, .4375, .5625, .6875, .8125, .9375
Lat=   Centroids$Lat[i]
Lon = Centroids$Lon[i]

Buffer <- 0.06    # dec degree.  Cell size is 0.04166

BeginYr = 1895
EndYr =  2014      # some 2015 files are provisional
EndMo = 12
Day = 15          # in output files, day of month in Date variable (req by strptime)

#  Root Out File Dir MUST exist - can only create final subdirectory (not root of this)
#WinDataDir <- "F:/PRISM"
#WinOFDir <- "D:/CHOH RSS/Figs PRISM"   
WinDataDir <- "D:/PRISM4k_AN81M2"
WinOFDir <- paste("~/RSS Plots/",Centroids$SiteID[i],sep="")

OPath1 <-"/Volumes/Seagate1_Blue2TB/COLM RSS/Figs PRISM/"
OPath2 <- "/Volumes/Seagate1_Blue2TB/Projects/RSS Climate/HOBE"
MacOFDir <- OPath1

DPath1 <- "/Volumes/Seagate2_4TB/PRISM4k_AN81M2"    
DPath2 <- "/Volumes/Seagate1_Blue2TB/PRISM"        
MacDataDir <- DPath2
rm(OPath1, OPath2, DPath1, DPath2)

if(.Platform$OS.type=="windows"){   
  DataDir <- WinDataDir
  OFDir <- WinOFDir}

if(.Platform$OS.type=="unix"){     # does not distinguish MacOS from others
  DataDir <- MacDataDir
  OFDir <- MacOFDir}

rm(WinDataDir, WinOFDir,MacDataDir,MacOFDir)
#  Seagate Blue
#PptDir <- paste(DataDir, "/ppt/bil/", sep = "")
#TminDir <- paste(DataDir, "/tmin/bil/", sep = "")
#TmaxDir <- paste(DataDir, "/tmax/bil/", sep = "")

#   Seagate 4TB
PptDir <- paste(DataDir, "/ppt/", sep = "")
TminDir <- paste(DataDir, "/tmin/", sep = "")
TmaxDir <- paste(DataDir, "/tmax/", sep = "")

############################################     
###########  End of Initials  ##############

AoaExt <- extent(Lon-Buffer, Lon+Buffer, Lat-Buffer, Lat+Buffer)

GetFileNames <- function(DataDirectoryName){  
  files <- list.files(DataDirectoryName)
  
  afiles <- files[nchar(files) > 36]   # only files with month and "stable"
  bFiles <- afiles[grep("^[_a-zA-Z0-9]+\\.bil$", afiles)]   # files ending with.bil 
  return(bFiles)
}

GetMonMeans <- function(DataDirName, AOAExt){
  DataFileNames <- GetFileNames(DataDirName)
  DataFile = paste(DataDirName, DataFileNames[1], sep="")
  R <- raster(DataFile)
  RCrp <- crop(R, AOAExt)
  Means = data.frame(File = as.character(DataFile), Mean = cellStats(RCrp, stat='mean', na.rm=TRUE))
  
  for(NFiles in 2:length(DataFileNames)){           
    DataFile = paste(DataDirName, DataFileNames[NFiles], sep="")
    R <- raster(DataFile)
    RCrp <- crop(R, AOAExt)
    M = data.frame(File = DataFile, Mean = cellStats(RCrp, stat='mean', na.rm=TRUE))
    Means <- rbind(Means, M)
  }   # next NFiles
  return(Means)}

## For Debugging variables
NYr = BeginYr
#EndYr = BeginYr + 2

PptMeans = TminMeans = TmaxMeans = data.frame
for(NYr in BeginYr:EndYr){
  print(NYr); flush.console()
  
  PptYrDir <- paste(PptDir, NYr, "/", sep="")
  TminYrDir <- paste(TminDir, NYr, "/", sep="")
  TmaxYrDir <- paste(TmaxDir, NYr, "/", sep="")
  
  if(NYr == BeginYr){
    PptMeans <- GetMonMeans(PptYrDir, AoaExt)
    TminMeans <- GetMonMeans(TminYrDir, AoaExt)
    TmaxMeans <- GetMonMeans(TmaxYrDir, AoaExt)
  }   # end if(NYr == BeginYr)
  
  if(NYr > BeginYr){
    PptMeans <- rbind(PptMeans, GetMonMeans(PptYrDir, AoaExt))
    TminMeans <- rbind(TminMeans, GetMonMeans(TminYrDir, AoaExt))
    TmaxMeans <- rbind(TmaxMeans, GetMonMeans(TmaxYrDir, AoaExt))
  }   # end else
}	# next NYr 

#  Deal with dates and seasons
GetSeason <- function(DateVec){
  seas <- as.character(rep(NA, length(DateVec)))
  seas[which(format(DateVec,'%B') %in% c("December", "January", "February"))]<- "Winter"
  seas[which(format(DateVec,'%B') %in% c("March", "April", "May"))]<- "Spring"
  seas[which(format(DateVec,'%B') %in% c("June", "July", "August"))]<- "Summer"
  seas[which(format(DateVec,'%B') %in% c("September", "October", "November"))]<- "Fall"
  return(seas)
}

names(PptMeans)[2] <- "Ppt"
PptMeans$File <- as.character(PptMeans$File)
PptMeans$YearMon <- substring(PptMeans$File[], nchar(PptMeans$File[])-13,nchar(PptMeans$File[])-8)
PptMeans$Date <- strptime(paste(PptMeans$YearMon, Day, sep=""), "%Y%m%d")
Season <- GetSeason(PptMeans$Date)
PptMeans<- cbind(PptMeans, Season)

names(TminMeans)[2] <- "Tmin"
TminMeans$File <- as.character(TminMeans$File)
TminMeans$YearMon <- substring(TminMeans$File[], nchar(TminMeans$File[])-13,nchar(TminMeans$File[])-8)
TminMeans$Date <- strptime(paste(TminMeans$YearMon, Day, sep=""), "%Y%m%d")
Season <- GetSeason(TminMeans$Date)
TminMeans<- cbind(TminMeans, Season)

names(TmaxMeans)[2] <- "Tmax"
TmaxMeans$File <- as.character(TmaxMeans$File)
TmaxMeans$YearMon <- substring(TmaxMeans$File[], nchar(TmaxMeans$File[])-13,nchar(TmaxMeans$File[])-8)
TmaxMeans$Date <- strptime(paste(TmaxMeans$YearMon, Day, sep=""), "%Y%m%d")
Season <- GetSeason(TmaxMeans$Date)
TmaxMeans<- cbind(TmaxMeans, Season)

#  Data conversions

PptMeans$PptIn <- PptMeans$Ppt/25.4     # mm to in
TminMeans$TminF <- TminMeans$Tmin * 9/5 + 32
TmaxMeans$TmaxF <- TmaxMeans$Tmax * 9/5 + 32

# clean up before writing file
rm(PptDir, PptYrDir, TminDir, TminYrDir, TmaxDir, TmaxYrDir, NYr)

dir.create(OFDir)
setwd(OFDir)
save.image(sprintf("%s_%s_%s_PRISM_PptTminTmax_IntermediateFiles.RData", SiteID, Lat, Lon))

# reality check. Need print & flush to generate output to screen when full script run
print(qplot(PptMeans$Date, PptMeans$PptIn, data=PptMeans)); flush.console()
print(qplot(TmaxMeans$Date, TmaxMeans$TmaxF, data=TmaxMeans)); flush.console()
print(qplot(TminMeans$Date, TminMeans$TminF, data=TminMeans)); flush.console()

#### Create .xslx workbook with all data tables
#WriteXLS(c("PptMeans", "TmaxMeans", "TminMeans"), paste(OFDir, "/",SiteID, "_", Lat,"_", Lon,"_PRISM.xlsx", sep=""), BoldHeaderRow = TRUE)
write.csv(PptMeans,paste(OFDir,"/",SiteID,"_PptMeans.csv",sep=""))
write.csv(TmaxMeans,paste(OFDir,"/",SiteID,"_TmaxMeans.csv",sep=""))
write.csv(TminMeans,paste(OFDir,"/",SiteID,"_TminMeans.csv",sep=""))
}
## EOF ##