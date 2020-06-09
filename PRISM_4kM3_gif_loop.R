## Loop through file and create plots from 4k data


library(raster)
library(prism)
library(ggplot2)
library(plyr)
library(lubridate)
library(dplyr)
library(gridExtra)
library(ggExtra)
library(ggpubr)
library(ggridges)
library(cowplot)
library(animation)
library(magick)
library(RColorBrewer)
rm(list=ls())


Colorramp<-c('#f7f7f7','#d9d9d9','#bdbdbd','#969696','#636363','#252525','red')
# Colorramp<-c("#ffffff","#e0dede","#c6c2c2","#969494","#6d6d6d", "#494949", "brown")

setwd("C:/Users/achildress/Documents/Data_Visualization/2020_Test/")
Centroids<-read.csv("2020nps_boundary_centroids/NPS_Centroid_Test2.csv",header=T)
PptDir <- ("C:/Users/achildress/Documents/Data_Visualization/2020_Test/PRISM_Extract/ppt/")
TmeanDir <-  ("C:/Users/achildress/Documents/Data_Visualization/2020_Test/PRISM_Extract/tmean/")

for (i in 2:nrow(Centroids)){
  PARK <- Centroids$SiteID[i]  # Park ID
  LongPARK <- Centroids$Name[i] # Create column for long name 
  #set work directory - where all datasets are located
  Lat <- Centroids$Lat[i]
  Lon <- Centroids$Lon[i]
  
  # Parse data for park
  options(prism.path = PptDir)
  ppt <- prism_slice(c(Lon,Lat),ls_prism_data()[,1]) #plots slice of data from single location from list of prism files
  ppt2<-prism$data
  rownames(ppt2)<-NULL
  colnames(ppt2)<-ppt$labels
  
  options(prism.path = TmeanDir)
  tmean <- prism_slice(c(Lon,Lat),ls_prism_data()[,1]) 
  tmean2<-prism$data
  rownames(tmean2)<-NULL
  colnames(tmean2)<-tmean$labels
  
  #read the .csv with the climate data
  PPT<-read.csv(paste0(PARK,'_PptMeans.csv'),header=TRUE)
  Tmax<-read.csv(paste0(PARK,'_TmaxMeans.csv'),header=TRUE)
  Tmin<-read.csv(paste0(PARK,'_TminMeans.csv'),header=TRUE)
  
  Clim1<-data.frame(PPT$Date,PPT$PptIn,Tmax$TmaxF,Tmin$TminF)
  Clim1$Tmean<-rowMeans(Clim1[,c("Tmax.TmaxF","Tmin.TminF")],na.rm=TRUE)
  
  Clim1$PPT.Date<-as.Date(Clim1$PPT.Date)
  Clim1$Year<-format(Clim1$PPT.Date,format="%Y")
  
  #Create new data frame to aggregate yearly temp and precip
  Clim2<-data.frame(aggregate(PPT.PptIn~Year,Clim1,mean))
  names(Clim2)[names(Clim2)=="PPT.PptIn"]<-"Ppt"
  Clim2$Tmean<-aggregate(Tmean~Year,Clim1,mean);Clim2$Tmean<-Clim2$Tmean[,2]
  
  
  #### Set colors for every 20 years -- adjust this and color ramp to be whatever want
  Clim2$Color1<-"1895-1900"
  Clim2$Color1[Clim2$Year >= 1900] <-"1901-1920"
  Clim2$Color1[Clim2$Year >= 1920] <-"1921-1940"
  Clim2$Color1[Clim2$Year >= 1940] <-"1941-1960"
  Clim2$Color1[Clim2$Year >= 1960] <-"1961-1980"
  Clim2$Color1[Clim2$Year >= 1980] <-"1981-2000"
  Clim2$Color1[Clim2$Year >= 2000] <-"2001-2017"
  
  Clim2$Color1<-as.factor(Clim2$Color1)
  
  for (i in 1:nrow(Clim2)){
    Clim3<-Clim2[1:i,]
    Year<-Clim3$Year[i]
    print(paste(Year,PARK,sep=" "))
    ggplot<-ggplot(Clim3, aes(Tmean, Ppt,xmin=min(Clim2$Tmean),xmax=max(Clim2$Tmean),ymin=(min(Clim2$Ppt)),ymax=max(Clim2$Ppt))) +
      geom_point(shape=21,colour="black",aes(fill=Color1),size=4) +
      geom_point(shape=21,size=4,colour="black") +
      annotate(geom="text", x=max(Clim2$Tmean), y=max(Clim2$Ppt), label=Year, color="black",size=8,hjust=1) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.text=element_text(size=18),axis.title.x=element_text(size=18,vjust=-0.2),
            axis.title.y=element_text(size=18,vjust=0.2),
            plot.title=element_text(size=20,face="bold",vjust=2,hjust=0.5)) +
      labs(x= expression(paste("Annual average temperature ("~degree~F,")",sep="")),
           # x = expression("Annual average temperature (",(degree~F),")"), # Change expression("Temperature " ( degree*C))
           y = "Annual precipitation (in)") + 
      scale_color_manual(name=" ", values=Colorramp) +
      scale_fill_manual(name=" ",values = Colorramp) +
      geom_hline(aes(yintercept=mean(Clim2$Ppt[which(Clim2$Year>=1900 | Clim2$Year<2000)])),linetype=2,colour="black") +
      geom_vline(aes(xintercept=mean(Clim2$Tmean[which(Clim2$Year>=1900 | Clim2$Year<2000)])),linetype=2,colour="black") +
      border()
    
    Clim3$Year<-as.numeric(Clim3$Year)
    Clim3$decade<-ifelse(Clim3$Year<1900,1890,(Clim3$Year - Clim3$Year %% 20))
    temp<-ggplot(Clim3, aes(x=Tmean,y=decade,group=decade)) + 
      geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.01,aes(fill=Color1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.text=element_text(size=18), axis.title = element_text(size=18),legend.position="none") +
      xlab(expression(paste("Annual average temperature ("~degree~F,")"))) +
      ylab("") +
      scale_x_continuous(limits=c(min(Clim2$Tmean), max(Clim2$Tmean))) +
      scale_y_reverse(expand = c(0,0),breaks=c(2000, 1980, 1960, 1940, 1920, 1900)) +
      scale_fill_manual(name=" ",values = Colorramp)
    
    
    precip<-ggplot(Clim3, aes(x=Ppt,y=decade,group=decade)) + 
      geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03,aes(fill=Color1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.text = element_text(angle=270, size = 18), 
            axis.title.y.left = element_text(angle=270, size=18),legend.position="none") +
      xlab("Annual precipitation (in)") + ylab("") +
      scale_x_continuous(limits=c(min(Clim2$Ppt), max(Clim2$Ppt))) +
      scale_y_reverse(expand = c(0,0),breaks=c(2000, 1980, 1960, 1940, 1920, 1900)) +
      coord_flip() +
      scale_fill_manual(name=" ",values = Colorramp)
    
    setwd("C:/Users/achildress/Documents/Data_Visualization/Visualization-HV/Visualization/gifs/")
    
    gg<-ggarrange(temp, NULL, ggplot, precip, 
                  ncol = 2, nrow = 2,  align = "hv", 
                  widths = c(3, 2), heights = c(2, 3),
                  common.legend = TRUE,legend="right") 
    title<-paste(LongPARK, "Climate (1895-2017)")
    annotate_figure(gg,top=text_grob(title, 
                                     color="black", face="bold", size=20))
    
    ggsave(paste(PARK,"_",Year,".tiff",sep=""), width = 11, height = 11)
  }
  for (i in 1:40){
    ggsave(paste(PARK,"_",Year,"_",i,".tiff",sep=""), width = 11, height = 11)
  }
  ggsave(paste(PARK,".png",sep=""), width = 11, height = 11)
  
  ## Creating animation
  
  # https://www.rforge.net/doc/packages/animation/saveGIF.
  shell("convert -delay 20 *.tiff PARK.gif")
  file.rename("PARK.gif", paste(PARK,".gif"))
  
  dev.off()
  file.remove(list.files(pattern=".tiff"))
}

