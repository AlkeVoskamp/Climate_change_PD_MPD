#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#                   Figures for the Manuscript                #
#       Plotting projected future changes in SR, PD, MPD      #
#               Global maps and and Scatterplots              #
#                       November 2019                         #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

rm(list=ls())

#-#-# Load the libraries #-#-#
library(ggplot2)
library(lattice)
library(rasterVis)
library(maps)
library(grid)
library(gridExtra)
library(rgdal)
library(RColorBrewer)
library(colorspace)

#-#-# Get the data #-#-#
setwd("E:/PD analysis/Second analysis non random loss win/Result files random PD change/")
RPDchange <- read.csv("PDandMPD_random_changes_disp2_rcp26_2080_RC.csv")
head(RPDchange)
str(RPDchange)

## Missing cells
Mdata <- read.csv("E:/PD analysis/Second analysis non random loss win/Result files random PD change/PD_SR_MPD_values_missing_cells_to_complete_maps_Disp2_rcp26_80.csv")
RPDchange <- rbind(RPDchange,Mdata)

RPDchange$SRpropChange <- ((RPDchange$SRFut - RPDchange$SRCur)/RPDchange$SRCur)*100
RPDchange$PDpropChange <- ((RPDchange$FaithFut - RPDchange$Faith)/RPDchange$Faith)*100
RPDchange$MPDpropChange <- ((RPDchange$MPDFut - RPDchange$MPD)/RPDchange$MPD)*100

RPDchange$SRAbsChange <- RPDchange$SRFut - RPDchange$SRCur
RPDchange$PDAbsChange <- RPDchange$Faith - RPDchange$FaithFut
RPDchange$MPDAbsChange <- RPDchange$MPD - RPDchange$MPDFut

## Continent data
ConData <- get(load("Continents_gridded_05.Rdata"))
head(ConData)
colnames(ConData) <-c("x","y","Continent")
ConData$Continent[ConData$Continent == 1] <- "Africa"
ConData$Continent[ConData$Continent == 2] <- NA
ConData$Continent[ConData$Continent == 3] <- "Asia"
ConData$Continent[ConData$Continent == 4] <- "Europe"
ConData$Continent[ConData$Continent == 5] <- "North America"
ConData$Continent[ConData$Continent == 6] <- "Australia"
ConData$Continent[ConData$Continent == 7] <- NA
ConData$Continent[ConData$Continent == 8] <- "South America"

CombCon <- merge(RPDchange,ConData,by=c("x","y"))

## Realm data 
realm <- read.csv("E:/PD analysis/Realm_coordinates_Lat_Lon.csv")
realm <- realm[c(2,3,4)]

CombCon <- merge (CombCon,realm,by=c("x","y"))


#-#-# Worldmap (global and cropped) #-#-#
setwd("H:/Manuscript chapter 3 SDMs/Manuscript drafts/1st draft/Figures/Worldmap/ne_50m_admin_0_countries/")

worldmap <- readOGR("ne_50m_admin_0_countries.shp", layer="ne_50m_admin_0_countries")
plot(worldmap)


#-#-# Scatterplot absolut and proportional SR #-#-#
SRscatter <- ggplot(CombCon,aes(x=SRAbsChange,y=SRpropChange,col=Continent))+
  geom_point(alpha = 0.3) +
  scale_color_manual(values=c("#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#E41A1C"),na.translate = F)+ #Continent
  #scale_color_manual(values=c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99"),na.translate = F)+#Realm 
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.key = element_rect(fill = NA))+
  guides(colour = guide_legend(override.aes = list(size=8,fill=NA)))+
  geom_hline(yintercept=0, linetype="dashed", color= "grey")+
  geom_vline(xintercept=0, linetype="dashed", color= "grey")+
  labs(x="Absolute change (# species)", y="% change species", title="")+
  ggtitle("(a)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))
 plot(SRscatter)
 
#-#-# Map SR change globally (proportional) #-#-#
min(RPDchange$SRpropChange,na.rm = T)
max(RPDchange$SRpropChange,na.rm = T)

#bl1 <- colorRampPalette(rev(c("navy","royalblue","lightskyblue")))(200) #blue to red                    
#re1 <- colorRampPalette(rev(c("mistyrose","red2","darkred")))(77)      
#wi1 <- colorRampPalette(rev(c("gray82","gray90","gray82")))(6)

bl1 <- colorRampPalette(rev(c("#08306B","#08519C","#2171B5","#4292C6","#9ECAE1","#DEEBF7")))(200)  
re1 <- colorRampPalette(rev(c("#FFEDA0","#FEB24C","#FC4E2A","#E31A1C","#800026")))(70) 
wi1 <- colorRampPalette(rev(c("#FFFFCC")))(4)


SRplot<- ggplot(data=RPDchange, aes(y=y, x=x)) +
  geom_raster(aes(fill =  SRpropChange), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(re1,wi1,bl1),limits=c(-72,202))+ # Insert colour and set range #-72,100
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  #geom_path(data=worldmap_df, aes(long,lat, group=group), color="black",size=0.3)+ # Add the country boarders
  #coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax))+ #Set the plot extent
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = "none")+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("(d)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))

print(SRplot)

#-#-# Scatter plot % change SR against % change PD #-#-#
PDscatter <- ggplot(CombCon,aes(x=SRpropChange,y=PDpropChange,col=Continent))+
  geom_point(alpha = 0.3) +
  scale_color_manual(values=c("#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#E41A1C"),na.translate = F)+ #Continent
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.key = element_rect(fill = NA))+
  guides(colour = guide_legend(override.aes = list(size=8,fill=NA)))+
  geom_hline(yintercept=0, linetype="dashed", color= "grey")+
  geom_vline(xintercept=0, linetype="dashed", color= "grey")+
  labs(x="% change SR", y="% change PD", title="")+
  ggtitle("(b)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))
plot(PDscatter)

#-#-# Map PD change globally (proportional) #-#-#
min(RPDchange$PDpropChange,na.rm = T)
max(RPDchange$PDpropChange,na.rm = T)

PDplot<- ggplot(data=RPDchange, aes(y=y, x=x)) +
  geom_raster(aes(fill =  PDpropChange), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(re1,wi1,bl1),limits=c(-72,202))+ # Insert colour and set range #-72,100
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = "none")+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("(e)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))

print(PDplot)

#-#-# Scatter plot % change SR against % change MPD #-#-#
MPDscatter <- ggplot(CombCon,aes(x=SRpropChange,y=MPDpropChange,col=Continent))+
  geom_point(alpha = 0.3) +
  scale_color_manual(values=c("#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#E41A1C"),na.translate = F)+ #Continent
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.key = element_rect(fill = NA))+
  guides(colour = guide_legend(override.aes = list(size=8,fill=NA)))+
  geom_hline(yintercept=0, linetype="dashed", color= "grey")+
  geom_vline(xintercept=0, linetype="dashed", color= "grey")+
  labs(x="% change SR", y="% change MPD", title="")+
  ggtitle("(c)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))
plot(MPDscatter)

#-#-# Map MPD change globally (proportional) #-#-#
min(RPDchange$MPDpropChange,na.rm = T)
max(RPDchange$MPDpropChange,na.rm = T)

bl1 <- colorRampPalette(rev(c("#08306B","#08519C","#2171B5","#4292C6","#9ECAE1","#DEEBF7")))(46)  
re1 <- colorRampPalette(rev(c("#FFEDA0","#FEB24C","#FC4E2A","#E31A1C","#800026")))(22) 
wi1 <- colorRampPalette(rev(c("#FFFFCC")))(2)


MPDplot<- ggplot(data=RPDchange, aes(y=y, x=x)) +
  geom_raster(aes(fill =  MPDpropChange), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(re1,wi1,bl1),limits=c(-23,47))+ # Insert colour and set range #-72,100
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = "none")+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("(f)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))

print(MPDplot)

#-#-# Prepaire the legends #-#-#
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

## Legend proportional change
bl1 <- colorRampPalette(rev(c("#08306B","#08519C","#2171B5","#4292C6","#9ECAE1","#DEEBF7")))(200)  
re1 <- colorRampPalette(rev(c("#FFEDA0","#FEB24C","#FC4E2A","#E31A1C","#800026")))(70) 
wi1 <- colorRampPalette(rev(c("#FFFFCC")))(4)

leg1 <- ggplot(data=RPDchange, aes(y=y, x=x)) +
  geom_raster(aes(fill =  SRpropChange), stat = "identity", position = "identity", hjust = 0, vjust = 0, interpolate = FALSE,)+
  scale_fill_gradientn("% change",colours=c(re1,wi1,bl1),limits=c(-72,200),breaks=c(-72,200),labels=c("-72","200"))+ # Insert colour and set range
  theme(legend.position = c(0.5, 0.6))+ # Positioning the legend <- needs to run with legend position to extract legend  
  theme(legend.text=element_text(size=19),legend.title=element_text(size=23))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) # Change size of legend key

legPC<-g_legend(leg1)
plot(legPC)

## Legend proportional change MPD
bl1 <- colorRampPalette(rev(c("#08306B","#08519C","#2171B5","#4292C6","#9ECAE1","#DEEBF7")))(46)  
re1 <- colorRampPalette(rev(c("#FFEDA0","#FEB24C","#FC4E2A","#E31A1C","#800026")))(22) 
wi1 <- colorRampPalette(rev(c("#FFFFCC")))(2)
leg2 <- ggplot(data=RPDchange, aes(y=y, x=x)) +
  geom_raster(aes(fill =  SRpropChange), stat = "identity", position = "identity", hjust = 0, vjust = 0, interpolate = FALSE,)+
  scale_fill_gradientn("% change",colours=c(re1,wi1,bl1),limits=c(-23,47),breaks=c(-23,47),labels=c("-23","47"))+ # Insert colour and set range
  theme(legend.position = c(0.5, 0.6))+ # Positioning the legend <- needs to run with legend position to extract legend  
  theme(legend.text=element_text(size=19),legend.title=element_text(size=23))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) # Change size of legend key

legPCmpd<-g_legend(leg2)
plot(legPCmpd)

blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(
    plot.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )


#-#-# Arrange plot and save #-#-#  
CombPlots <- arrangeGrob(SRplot,PDplot,MPDplot,
                         widths = c(5,5,5),
                         heights = c(0.5),
                         ncol = 3,
                         nrow = 1)
plot(CombPlots)

CombScatter <- arrangeGrob(SRscatter,PDscatter,MPDscatter,
                         widths = c(5,5,5),
                         heights = c(0.5),
                         ncol = 3,
                         nrow = 1)
plot(CombScatter)

CombLeg <- arrangeGrob(legPC,blankPlot,legPC,blankPlot,legPCmpd,blankPlot,
                           widths = c(2.5,2.5,2.5,2.5,2.5,2.5),
                           heights = c(0.5),
                           ncol = 6,
                           nrow = 1)
plot(CombLeg)

Final <- arrangeGrob(CombScatter,CombPlots,CombLeg,
                     widths = c(5),
                     heights = c(1,1,0.5),
                     ncol = 1,
                     nrow = 3)
plot(Final)


setwd("E:/PD analysis/PD manuscript/PD Manuscript Figures/")
ggsave("Figure 1 Changes SR PD MPD rcp26.tiff",Final,width=24, height=14, unit="in", dpi=600, bg="transparent")
ggsave("Figure 1 Changes SR PD MPD rcp26.png",Final,width=24, height=13, unit="in", dpi=600, bg="transparent")


