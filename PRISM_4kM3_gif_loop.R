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

for (i in 1:nrow(Centroids)){
# for (i in 1:1){ # turn on for troubleshooting
  PARK <- Centroids$SiteID[i]  # Park ID
  LongPARK <- Centroids$Name[i] # Create column for long name 
  #set work directory - where all datasets are located
  Lat <- Centroids$Lat[i]
  Lon <- Centroids$Lon[i]
  
  # Parse data for park
  options(prism.path = PptDir)
  ppt <- prism_slice(c(Lon,Lat),ls_prism_data()[,1]) #plots slice of data from single location from list of prism files
  ppt2<-ppt$data
  rownames(ppt2)<-NULL
  colnames(ppt2)<-ppt$labels
  ppt2$PptIn <- ppt2[,1]/25.4
  ppt2$Year <- format(ppt2$Date,format="%Y")
  
  options(prism.path = TmeanDir)
  tmean <- prism_slice(c(Lon,Lat),ls_prism_data()[,1]) 
  tmean2<-tmean$data
  rownames(tmean2)<-NULL
  colnames(tmean2)<-tmean$labels
  tmean2$Tmean <- (tmean2[,1] * 9/5) +32
  tmean2$Year <- format(tmean2$Date,format="%Y")
  
  Clim1<-merge(ppt2[,c("Year","Date","PptIn")],tmean2[,c("Year","Date","Tmean")],by=c("Year","Date"))
  
  #### Set colors for every 20 years -- adjust this and color ramp to be whatever want
  Clim1$Color1<-"1895-1900"
  Clim1$Color1[Clim1$Year >= 1900] <-"1901-1920"
  Clim1$Color1[Clim1$Year >= 1920] <-"1921-1940"
  Clim1$Color1[Clim1$Year >= 1940] <-"1941-1960"
  Clim1$Color1[Clim1$Year >= 1960] <-"1961-1980"
  Clim1$Color1[Clim1$Year >= 1980] <-"1981-2000"
  Clim1$Color1[Clim1$Year >= 2000] <-"2001-2018"
  
  Clim1$Color1<-as.factor(Clim1$Color1)
  
  for (i in 1:nrow(Clim1)){
  # for (i in 1:10){ # turn on for troubleshooting
    Clim3<-Clim1[1:i,]
    Year<-Clim3$Year[i]
    print(paste(Year,PARK,sep=" "))
    ggplot<-ggplot(Clim3, aes(Tmean, PptIn,xmin=min(Clim1$Tmean),xmax=max(Clim1$Tmean),ymin=(min(Clim1$PptIn)),ymax=max(Clim1$PptIn))) +
      geom_point(shape=21,colour="black",aes(fill=Color1),size=4) +
      geom_point(shape=21,size=4,colour="black") +
      annotate(geom="text", x=max(Clim1$Tmean), y=max(Clim1$Ppt), label=Year, color="black",size=8,hjust=1) +
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
      geom_hline(aes(yintercept=mean(Clim1$Ppt[which(Clim1$Year>=1900 | Clim1$Year<2000)])),linetype=2,colour="black") +
      geom_vline(aes(xintercept=mean(Clim1$Tmean[which(Clim1$Year>=1900 | Clim1$Year<2000)])),linetype=2,colour="black") +
      border()
    
    Clim3$Year<-as.numeric(Clim3$Year)
    Clim3$decade<-ifelse(Clim3$Year<1900,1890,(Clim3$Year - Clim3$Year %% 20))
    temp<-ggplot(Clim3, aes(x=Tmean,y=decade,group=decade)) + 
      geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.01,aes(fill=Color1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.text=element_text(size=18), axis.title = element_text(size=18),legend.position="none",
            axis.title.x = element_text(margin = margin(t = -4, r = 0, b = -4, l = 0)),
            axis.title.y = element_text(margin = margin(t = 0, r = -2, b = 0, l = -20))) +
      xlab(expression(paste("Annual average temperature ("~degree~F,")"))) +
      ylab("") +
      scale_x_continuous(limits=c(min(Clim1$Tmean), max(Clim1$Tmean))) +
      scale_y_reverse(expand = c(0,0),breaks=c(2000, 1980, 1960, 1940, 1920, 1900, 1890)) +
      scale_fill_manual(name=" ",values = Colorramp)
    temp
    
    precip<-ggplot(Clim3, aes(x=PptIn,y=decade,group=decade)) + 
      geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03,aes(fill=Color1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            axis.text = element_text(angle=270, size = 18), 
            axis.title.y.left = element_text(angle=270, size=18,margin = margin(t = -4, r = 0, b = -4, l = 0)),legend.position="none") +
      xlab("Annual precipitation (in)") + ylab("") +
      scale_x_continuous(limits=c(min(Clim1$PptIn), max(Clim1$PptIn))) +
      scale_y_reverse(expand = c(0,0),breaks=c(2000, 1980, 1960, 1940, 1920, 1900, 1890)) +
      coord_flip() +
      scale_fill_manual(name=" ",values = Colorramp)
    
    setwd("C:/Users/achildress/Documents/Data_Visualization/2020_Test/gifs/")
    
    title<-paste(LongPARK, "Climate (1895-2018)")
    gg2<-ggarrange(ggplot,
                   ncol = 1, nrow = 1,  #align = "hv", 
                   common.legend = TRUE,legend="right") 
    
    gg2 <- annotate_figure(gg2,top=text_grob(title, 
                                      color="black", face="bold", size=20),
                    bottom = text_grob("Data source: PRISM Climate Group
                                   Prepared by: HD.Vincelette, AN.Runyon, GW.Schuurman", color = "blue",
                                       hjust = 1, x = 1, face = "italic", size = 10))
    
    ggsave(plot=gg2, file=paste(PARK,"_",Year,"-BASIC1.tiff",sep=""), width = 11, height = 11)
    
    gg<-ggarrange(temp, NULL, ggplot, precip, 
                  ncol = 2, nrow = 2,  #align = "hv", 
                  widths = c(3, 2), heights = c(2, 3),
                  common.legend = TRUE,legend="right") 
   
    gg <- annotate_figure(gg,top=text_grob(title, 
                                     color="black", face="bold", size=20),
                    bottom = text_grob("Data source: PRISM Climate Group
                                   Prepared by: HD.Vincelette, AN.Runyon, GW.Schuurman", color = "blue",
                                       hjust = 1, x = 1, face = "italic", size = 10))
    gg
    
    ggsave(plot=gg,file=paste(PARK,"_",Year,"2.tiff",sep=""), width = 11, height = 11)
    
  }
  
  for (i in 1:40){
  # for (i in 1:5){ # turn on for troubleshooting
     ggsave(plot=gg2,file=paste(PARK,"_",Year,"_",i,"-BASIC1.tiff",sep=""), width = 11, height = 11)
     ggsave(plot=gg,file=paste(PARK,"_",Year,"_",i,"2.tiff",sep=""), width = 11, height = 11)
  }
  ggsave(plot=gg2,file=paste(PARK,"-BASIC.png",sep=""), width = 11, height = 11)
  ggsave(plot=gg,file=paste(PARK,".png",sep=""), width = 11, height = 11)
  
  ## Creating animation
  
  # https://www.rforge.net/doc/packages/animation/saveGIF.
  shell("convert -delay 20 *2.tiff PARK.gif")
  file.rename("PARK.gif", paste(PARK,".gif"))
  
  shell("convert -delay 20 *1.tiff PARK-BASIC.gif")
  file.rename("PARK-BASIC.gif", paste(PARK,"-BASIC.gif"))
  
  dev.off()
  file.remove(list.files(pattern=".tiff"))
}

# ---------- Future line plot additions --------------
# Clim3
# 
# c <- ggplot(data=Clim3) + geom_point(aes(Year,)) +
#   geom_line(aes(Year,Tmean)) 
# c <- c + geom_smooth(method=lm, data=Clim3,aes(Year,Tmean),na.rm=TRUE,level=.99)
# c <- c + geom_smooth(method=lm, data=Clim3[which(Clim3$Year >=1970),],aes(Year,Tmean),na.rm=TRUE)
# c
# 
# d <- ggplot(data=Clim3) + geom_point(aes(Year,PptIn)) +
#   geom_line(aes(Year,PptIn)) 
# d <- d + geom_smooth(method=lm, data=Clim3,aes(Year,PptIn),na.rm=TRUE,level=.99)
# d <- d + geom_smooth(method=lm, data=Clim3[which(Clim3$Year >=1970),],aes(Year,PptIn),na.rm=TRUE)
# d



