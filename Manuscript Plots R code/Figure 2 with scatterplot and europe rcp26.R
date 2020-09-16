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


# #-#-# Get the results from the analysi and assemble the dataframe #-#-#
# setwd("/Users/alkevoskamp/Documents/PD manuscript/")
# RPDchange <- read.csv("PDandMPD_random_changes_disp1_rcp26_2080_RC_Final.csv")
# head(RPDchange)
# str(RPDchange)
# 
# min(RPDchange$SRChange)
# max(RPDchange$SRChange)
# 
# ## Merge in the missing cells to the final dataframe
# Mdata <- read.csv("/Users/alkevoskamp/Documents/PD manuscript/PD_SR_MPD_values_missing_cells_to_complete_maps_Disp2_rcp60_80.csv")
# RPDchange <- rbind(RPDchange,Mdata)
# 
# RPDchange$SRpropChange <- ((RPDchange$SRFut - RPDchange$SRCur)/RPDchange$SRCur)*100
# RPDchange$PDpropChange <- ((RPDchange$FaithFut - RPDchange$Faith)/RPDchange$Faith)*100
# RPDchange$MPDpropChange <- ((RPDchange$MPDFut - RPDchange$MPD)/RPDchange$MPD)*100
# 
# RPDchange$SRAbsChange <- RPDchange$SRFut - RPDchange$SRCur
# RPDchange$PDAbsChange <- RPDchange$Faith - RPDchange$FaithFut
# RPDchange$MPDAbsChange <- RPDchange$MPD - RPDchange$MPDFut
# 
# 
# #-#-# Change order of coordinates to fix map extent #-#-#
# RPDchangeC1 <- subset(RPDchange,x > -170) ## Subset to x coords higher than -170 these are fine
# RPDchangeC2 <- subset(RPDchange,x <= -170) ## Subset to x coords lower than -170 these need to move
# RPDchangeC2$x <- abs(RPDchangeC2$x) + 10 ## Make coords positive to shift to other side and add 10 degree to attach end of map (179.5 center)
# x <- rev(RPDchangeC2$x) ## Reorder the x coords because they need to be mirrored to be attached on the other side
# RPDchangeC2$x <- x ## Replace with the reordered x coords
# RPDchangeC3 <- rbind(RPDchangeC1,RPDchangeC2) ##Merge the two dataframes back together
# 
# RPDchange <- RPDchangeC3
# 
# 
# ## Add the continent data
# ConData <- get(load("Continents_gridded_05.Rdata"))
# head(ConData)
# colnames(ConData) <-c("x","y","Continent")
# ConData$Continent[ConData$Continent == 1] <- "Africa"
# ConData$Continent[ConData$Continent == 2] <- NA
# ConData$Continent[ConData$Continent == 3] <- "Asia"
# ConData$Continent[ConData$Continent == 4] <- "Europe"
# ConData$Continent[ConData$Continent == 5] <- "North America"
# ConData$Continent[ConData$Continent == 6] <- "Australia"
# ConData$Continent[ConData$Continent == 7] <- NA
# ConData$Continent[ConData$Continent == 8] <- "South America"
# 
# CombCon <- merge(RPDchange,ConData,by=c("x","y"))
# 
# ## Add the realm data
# realm <- read.csv("/Users/alkevoskamp/AG BGFM Dropbox/Voskamp/Realm_coordinates_Lat_Lon.csv")
# realm <- realm[c(2,3,4)]
# 
# CombCon <- merge (CombCon,realm,by=c("x","y"))
# 
# ## Save the final file
# setwd("/Users/alkevoskamp/Documents/PD manuscript/Data for plots/")
# write.csv(CombCon,"PDandMPD_random_changes_disp2_rcp60_2080_Final.csv")


#-#-#-#-#-#-#-#-#-#-#-#-#-#-# Plot Figure 2 #-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#-#-# Plot data #-#-#
setwd("/Users/alkevoskamp/Documents/PD manuscript/Data for plots/")
CombCon <- read.csv("PDandMPD_random_changes_disp2_rcp26_2080_Final.csv")

#-#-# Worldmap (global and cropped) #-#-#
setwd("/Users/alkevoskamp/Documents/PD manuscript/Data for plots/ne_50m_admin_0_countries/")

worldmap <- readOGR("ne_50m_admin_0_countries.shp", layer="ne_50m_admin_0_countries")
plot(worldmap)


#---#---#---#---#---#---#---# SR plots #---#---#---#---#---#---#---#
min(CombCon$SRAbsChange)
max(CombCon$SRAbsChange)

#-#-# Scatterplot absolut and proportional SR #-#-#
SRscatter <- ggplot(CombCon,aes(x=SRAbsChange,y=SRpropChange,col=Continent))+
  geom_point(alpha = 0.8,cex = 2, pch = 16) +
  scale_color_manual(values=c("#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#E41A1C"),na.translate = F)+ #Continent
  #scale_color_manual(values=c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99"),na.translate = F)+#Realm 
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16))+
  #theme(legend.position = "none")+
  guides(colour = guide_legend(override.aes = list(size=8,fill=NA)))+
  geom_hline(yintercept=0, linetype="dashed", color= "grey")+
  geom_vline(xintercept=0, linetype="dashed", color= "grey")+
  labs(x="Absolute change (# species)", y="% change SR", title="")+
  ggtitle("(a)            Species richness")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))
plot(SRscatter)

#-#-# Map SR change globally (proportional) #-#-#
min(CombCon$SRpropChange,na.rm = T)
max(CombCon$SRpropChange,na.rm = T)

bl1 <- colorRampPalette(rev(c("#08306B","#08519C","#2171B5","#4292C6","#9ECAE1","#DEEBF7")))(200) 
re1 <- colorRampPalette(rev(c("#FFEDA0","#FEB24C","#FC4E2A","#E31A1C","#800026")))(70) 
wi1 <- colorRampPalette(rev(c("#FFFFCC")))(4)

SRplot<- ggplot(data=CombCon, aes(y=y, x=x)) +
  geom_raster(aes(fill =  SRpropChange), stat = "identity", position = "identity")+
  scale_fill_gradientn("SR\n%change",colours=c(re1,wi1,bl1),limits=c(-72,202))+ # Insert colour and set range #-72,100
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = c(0.1, 0.2))+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("(d)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))

print(SRplot)

#-#-# Map SR change Europe #-#-#
SREurope <- ggplot(data=CombCon, aes(y=y, x=x)) +
  geom_raster(aes(fill =  SRpropChange), stat = "identity", position = "identity")+
  scale_fill_gradientn("SR\n%change",colours=c(re1,wi1,bl1),limits=c(-72,202))+ 
  borders("world",  xlim = c(-10, 40), ylim = c(35,70), lwd=0.3, colour ="black")+
  coord_cartesian(xlim = c(-10, 40), ylim = c(35, 70))+ #Set the plot extent
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = "none")+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("(g)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))

print(SREurope)


#---#---#---#---#---#---#---# PD plots #---#---#---#---#---#---#---#

#-#-# Scatter plot % change SR against % change PD #-#-#
PDscatter <- ggplot(CombCon,aes(x=SRpropChange,y=PDpropChange,col=Continent))+
  geom_point(alpha = 0.3) +
  #scale_color_manual(values=c("#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#E41A1C"),na.translate = F)+ #Continent
  scale_color_manual(values=c("black","black","black","black","black","black"),na.translate = F)+ #Continent no colour
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.key = element_rect(fill = NA),)+
  theme(legend.position = "none")+
  guides(colour = guide_legend(override.aes = list(size=8,fill=NA)))+
  geom_hline(yintercept=0, linetype="dashed", color= "grey")+
  geom_vline(xintercept=0, linetype="dashed", color= "grey")+
  labs(x="% change SR", y="% change PD", title="")+
  ggtitle("(b)        Faith phylogenetic diversity")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))
plot(PDscatter)

#-#-# Map PD change globally (proportional) #-#-#
min(CombCon$PDpropChange,na.rm = T)
max(CombCon$PDpropChange,na.rm = T)

PDplot<- ggplot(data=CombCon, aes(y=y, x=x)) +
  geom_raster(aes(fill =  PDpropChange), stat = "identity", position = "identity")+
  scale_fill_gradientn("PD\n%change",colours=c(re1,wi1,bl1),limits=c(-72,202))+ # Insert colour and set range #-72,100
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = c(0.1, 0.2))+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("(e)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))

print(PDplot)

#-#-# Map PD change Europe #-#-#
PDEurope <- ggplot(data=CombCon, aes(y=y, x=x)) +
  geom_raster(aes(fill =  PDpropChange), stat = "identity", position = "identity")+
  scale_fill_gradientn("PD\n%change",colours=c(re1,wi1,bl1),limits=c(-72,202))+
  borders("world",  xlim = c(-10, 40), ylim = c(35,70), lwd=0.3, colour ="black")+
  coord_cartesian(xlim = c(-10, 40), ylim = c(35, 70))+ #Set the plot extent
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = "none")+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("(h)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))

print(PDEurope)


#---#---#---#---#---#---#---# MPD plots #---#---#---#---#---#---#---#

#-#-# Scatter plot % change SR against % change MPD #-#-#
MPDscatter <- ggplot(CombCon,aes(x=SRpropChange,y=MPDpropChange,col=Continent))+
  geom_point(alpha = 0.3) +
  #scale_color_manual(values=c("#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#E41A1C"),na.translate = F)+ #Continent
  scale_color_manual(values=c("black","black","black","black","black","black"),na.translate = F)+ #Continent no colour
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.key = element_rect(fill = NA))+
  theme(legend.position = "none")+
  geom_hline(yintercept=0, linetype="dashed", color= "grey")+
  geom_vline(xintercept=0, linetype="dashed", color= "grey")+
  labs(x="% change SR", y="% change MPD", title="")+
  ggtitle("(c)           Mean pairwise distance")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))
plot(MPDscatter)

#-#-# Map MPD change globally (proportional) #-#-#
min(CombCon$MPDpropChange,na.rm = T)
max(CombCon$MPDpropChange,na.rm = T)

bl1 <- colorRampPalette(rev(c("#08306B","#08519C","#2171B5","#4292C6","#9ECAE1","#DEEBF7")))(45.41)
re1 <- colorRampPalette(rev(c("#FFEDA0","#FEB24C","#FC4E2A","#E31A1C","#800026")))(21.86) 
wi1 <- colorRampPalette(rev(c("#FFFFCC")))(2)


MPDplot<- ggplot(data=CombCon, aes(y=y, x=x)) +
  geom_raster(aes(fill =  MPDpropChange), stat = "identity", position = "identity")+
  scale_fill_gradientn("MPD\n%change",colours=c(re1,wi1,bl1),limits=c(-21.86,45.41))+ # Insert colour and set range #-72,100
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = c(0.1, 0.2))+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("(f)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))

print(MPDplot)

#-#-# Map PD change Europe #-#-#
MPDEurope <- ggplot(data=CombCon, aes(y=y, x=x)) +
  geom_raster(aes(fill =  MPDpropChange), stat = "identity", position = "identity")+
  scale_fill_gradientn("MPD\n%change",colours=c(re1,wi1,bl1),limits=c(-21.86,45.41))+
  borders("world",  xlim = c(-10, 40), ylim = c(35,70), lwd=0.3, colour ="black")+
  coord_cartesian(xlim = c(-10, 40), ylim = c(35, 70))+ #Set the plot extent
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = "none")+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("(i)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))

print(MPDEurope)


#---#---#---#---#---#---#---# Arange plots #---#---#---#---#---#---#---#

#-#-# Blank plot #-#-#  
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

CombPlots <- arrangeGrob(SRplot,PDplot,MPDplot,
                         widths = c(3,3,3),
                         heights = c(5),
                         ncol = 3,
                         nrow = 1)
plot(CombPlots)

CombScatter <- arrangeGrob(SRscatter,PDscatter,MPDscatter,
                           widths = c(3,3,3),
                           heights = c(5),
                           ncol = 3,
                           nrow = 1)
plot(CombScatter)

CombEurope <- arrangeGrob(blankPlot,SREurope,blankPlot,blankPlot,PDEurope,blankPlot,blankPlot,MPDEurope,blankPlot,
                          widths = c(0.5,2,0.5,0.5,2,0.5,0.5,2,0.5),
                          heights = c(5),
                          ncol = 9,
                          nrow = 1)
plot(CombEurope)


Final <- arrangeGrob(CombScatter,CombPlots,CombEurope,
                     widths = c(0.5),
                     heights = c(10,8,8),
                     ncol = 1,
                     nrow = 3)
plot(Final)


setwd("/Users/alkevoskamp/Documents/PD manuscript/Manuscript Figures/")
ggsave("Figure 2 Changes SR PD MPD rcp26_medDisp_europe_Final.tiff",Final,width=18, height=13, unit="in", dpi=600, bg="transparent")
#ggsave("Figure 1 Changes SR PD MPD rcp60_colour.png",Final,width=24, height=13, unit="in", dpi=600, bg="transparent")





