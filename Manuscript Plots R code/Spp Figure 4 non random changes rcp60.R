#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#              Figure 4 for the Manuscript            #
#        Random vs projected changes PD and MPD       #
#         Split by species coming in and going        #
#               Significant cells only                #
#                   September 2020                    #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

rm(list=ls())

#-#-# Load the libraries #-#-#
library(ggplot2)
library(lattice)
library(rasterVis)
library(maps)
library(grid)
library(gridExtra)
library(rgdal)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-# Plot Figure 4 #-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#-#-# Get and prepaire the plot data #-#-#
setwd("/Users/alkevoskamp/Documents/PD manuscript/Data for plots/")
CombCon <- read.csv("PDandMPD_random_changes_low_dispersal_rcp60_2080.csv")

## Add actual vs random comparison columns
CombCon$PDInvsRIn <- CombCon$FaithACIn - CombCon$FaithInRmean
CombCon$PDOutvsROut <- CombCon$FaithACOut - CombCon$FaithOutRmean
CombCon$MPDInvsRIn <- CombCon$MPDACIn - CombCon$MPDInRmean
CombCon$MPDOutvsROut <- CombCon$MPDACOut - CombCon$MPDOutRmean

head(CombCon)

CombCon$PDOutvsROut <- as.numeric(as.character(CombCon$PDOutvsROut))
CombCon$PDInvsRIn <- as.numeric(as.character(CombCon$PDInvsRIn))
hist(CombCon$PDOutvsROut)
hist(CombCon$PDInvsRIn)


#-#-# Function to extract plot legends #-#-#
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


#-#-# Random loss vs actual loss #-#-#
min(CombCon$PDOutvsROut,na.rm = T)
max(CombCon$PDOutvsROut,na.rm = T)

bl1 <- colorRampPalette(rev(c("lightskyblue","royalblue","navy")))(576) #553                     
re1 <- colorRampPalette(rev(c("darkred","red2","mistyrose")))(530)      #538
wi1 <- colorRampPalette(rev(c("gray82","gray90","gray82")))(4)

## Subset to signicicantly more loss and less loss
PDRLoss <- subset(CombCon,FaithOutPvalue >= 0.95 | FaithOutPvalue <= 0.05)
PDRLoss <- PDRLoss[!is.na(PDRLoss$PDOutvsROut),]

plot1<- ggplot(data=PDRLoss, aes(y=y, x=x)) +
  geom_raster(aes(fill =  PDOutvsROut), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(bl1,wi1,re1),limits=c(-576,530))+ # Insert colour and set range
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = "none")+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("(a)                                    Change through species loss")+ 
  theme(plot.title = element_text(size = 26,face="bold",hjust = 0))

print(plot1)

##Legend PD change out Plot 1
bl1 <- colorRampPalette(rev(c("lightskyblue","royalblue","navy")))(576)  #931.3    553                
re1 <- colorRampPalette(rev(c("darkred","red2","mistyrose")))(530)       #1026.5   538
out <- ggplot(data=PDRLoss, aes(y=y, x=x)) +
  geom_raster(aes(fill =  PDOutvsROut), stat = "identity", position = "identity", hjust = 0, vjust = 0, interpolate = FALSE,)+
  scale_fill_gradientn("Projected PD change\n - random PD change\n",colours=c(bl1,wi1,re1),limits=c(-553,538),breaks=c(-553,538),labels=c("Stronger decrease\n than random","Weaker decrease\n than random"))+ # Insert colour and set range
  theme(legend.position = c(0.5, 0.6))+ # Positioning the legend <- needs to run with legend position to extract legend  
  theme(legend.text=element_text(size=23),legend.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) # Change size of legend key

legendPlot1<-g_legend(out)
plot(legendPlot1)


#-#-# Random gain vs actual gain #-#-#
min(CombCon$PDInvsRIn,na.rm = T)
max(CombCon$PDInvsRIn,na.rm = T)

bl1 <- colorRampPalette(rev(c("navy","royalblue","lightskyblue")))(489)  #931.3                    
re1 <- colorRampPalette(rev(c("mistyrose","red2","darkred")))(591)       #1026.5
wi1 <- colorRampPalette(rev(c("gray82","gray90","gray82")))(4)

## Subset to signicicantly more gain and less gain
PDRGain <- subset(CombCon,FaithInPvalue >= 0.95 | FaithInPvalue <= 0.05)
PDRGain <- PDRGain[!is.na(PDRGain$PDInvsRIn), ]

plot2<- ggplot(data=PDRGain, aes(y=y, x=x)) +
  geom_raster(aes(fill = PDInvsRIn), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(re1,wi1,bl1),limits=c(-593,491))+ # Insert colour and set range
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = "none")+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("(b)                                    Change through species gain")+ 
  theme(plot.title = element_text(size = 26,face="bold",hjust = 0))

print(plot2)

##Legend PD change in Plot2
bl1 <- colorRampPalette(rev(c("navy","royalblue","lightskyblue")))(489)  #931.3                    
re1 <- colorRampPalette(rev(c("mistyrose","red2","darkred")))(591)       #1026.5
wi1 <- colorRampPalette(rev(c("gray82","gray90","gray82")))(4)

In <- ggplot(data=PDRGain, aes(y=y, x=x)) +
  geom_raster(aes(fill = PDInvsRIn), stat = "identity", position = "identity", hjust = 0, vjust = 0, interpolate = FALSE,)+
  scale_fill_gradientn("Projected PD change\n - random PD change\n",colours=c(re1,bl1),limits=c(-965,602),breaks=c(-965,602),labels=c("Weaker increase\n than random","Stronger increase\n than random"))+ # Insert colour and set range
  theme(legend.position = c(0.5, 0.6))+ # Positioning the legend <- needs to run with legend position to extract legend  
  theme(legend.text=element_text(size=23),legend.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) # Change size of legend key

legendPlot2<-g_legend(In)
plot(legendPlot2)


#-#-# Random MPD change #-#-#
CombCon$MPDOutvsROut <- as.numeric(as.character(CombCon$MPDOutvsROut))
CombCon$MPDInvsRIn <- as.numeric(as.character(CombCon$MPDInvsRIn))
hist(CombCon$MPDOutvsROut)
hist(CombCon$MPDInvsRIn)


#-#-# Random loss vs actual loss #-#-#
min(CombCon$MPDOutvsROut,na.rm = T)
max(CombCon$MPDOutvsROut,na.rm = T)

bl1 <- colorRampPalette(rev(c("lightskyblue","royalblue","navy")))(79) #1137                     
re1 <- colorRampPalette(rev(c("darkred","red2","mistyrose")))(35)      #468
wi1 <- colorRampPalette(rev(c("gray82","gray90","gray82")))(0)

## Subset signicicantly more loss and less loss
MPDRLoss <- subset(CombCon,MPDOutPvalue >= 0.95 | MPDOutPvalue <= 0.05)
MPDRLoss <- MPDRLoss[!is.na(MPDRLoss$MPDOutvsROut), ]

plot3<- ggplot(data=MPDRLoss, aes(y=y, x=x)) +
  geom_raster(aes(fill =  MPDOutvsROut), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(bl1,wi1,re1),limits=c(-79,35))+ # Insert colour and set range
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = "none")+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("(c)")+ 
  theme(plot.title = element_text(size = 26,face="bold",hjust = 0))

print(plot3)

##Legend MPD change out plot3
bl1 <- colorRampPalette(rev(c("lightskyblue","royalblue","navy")))(79)  #931.3                    
re1 <- colorRampPalette(rev(c("darkred","red2","mistyrose")))(35)       #1026.5
wi1 <- colorRampPalette(rev(c("gray82","gray90","gray82")))(0)
out <- ggplot(data=MPDRLoss, aes(y=y, x=x)) +
  geom_raster(aes(fill =  PDOutvsROut), stat = "identity", position = "identity", hjust = 0, vjust = 0, interpolate = FALSE,)+
  scale_fill_gradientn("Projected MPD change\n - random MPD change\n",colours=c(bl1,wi1,re1),limits=c(-79,36),breaks=c(-79,36),labels=c("Weaker decrease\n than random","Stronger decrease\n than random"))+ # Insert colour and set range
  theme(legend.position = c(0.5, 0.6))+ # Positioning the legend <- needs to run with legend position to extract legend  
  theme(legend.text=element_text(size=23),legend.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) # Change size of legend key

legendPlot3<-g_legend(out)
plot(legendPlot3)


#-#-# Random win vs actual win #-#-#
min(CombCon$MPDInvsRIn,na.rm = T)
max(CombCon$MPDInvsRIn,na.rm = T)

bl1 <- colorRampPalette(rev(c("navy","royalblue","lightskyblue")))(36)  #931.3                    
re1 <- colorRampPalette(rev(c("mistyrose","red2","darkred")))(23)       #1026.5
wi1 <- colorRampPalette(rev(c("gray82","gray90","gray82")))(0)

## Subset signicicantly more loss and less loss
MPDRGain <- subset(CombCon,MPDInPvalue >= 0.95 | MPDInPvalue <= 0.05)
MPDRGain <- MPDRGain[!is.na(MPDRGain$MPDInvsRIn), ]

plot4 <- ggplot(data=MPDRGain, aes(y=y, x=x)) +
  geom_raster(aes(fill =  MPDInvsRIn), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(re1,wi1,bl1),limits=c(-23,36))+ # Insert colour and set range
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = "none")+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("(d)")+ 
  theme(plot.title = element_text(size = 26,face="bold",hjust = 0))

print(plot4)

##Legend MPD change in Plot4
bl1 <- colorRampPalette(rev(c("navy","royalblue","lightskyblue")))(36)  #931.3                    
re1 <- colorRampPalette(rev(c("mistyrose","red2","darkred")))(23)       #1026.5
wi1 <- colorRampPalette(rev(c("gray82","gray90","gray82")))(0)
In <- ggplot(data=MPDRGain, aes(y=y, x=x)) +
  geom_raster(aes(fill = PDInvsRIn), stat = "identity", position = "identity", hjust = 0, vjust = 0, interpolate = FALSE,)+
  scale_fill_gradientn("Projected MPD change\n - random MPD change\n",colours=c(re1,wi1,bl1),limits=c(-18,33),breaks=c(-18,33),labels=c("Weaker increase\n than random","Stronger increase\n than random"))+ # Insert colour and set range
  theme(legend.position = c(0.5, 0.6))+ # Positioning the legend <- needs to run with legend position to extract legend  
  theme(legend.text=element_text(size=23),legend.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) # Change size of legend key

legendPlot4<-g_legend(In)
plot(legendPlot4)


#-#-# put plots together and save #-#-#

#-#-# Combine plot random PD loss and gain #-#-#  
CombNull <- arrangeGrob(plot1,legendPlot1,plot2,legendPlot2,plot3,legendPlot3,plot4,legendPlot4,
                        widths = c(4,1.1,4,1.1),
                        heights = c(0.3,0.3),
                        ncol = 4,
                        nrow = 2)



plot(CombNull)


setwd("/Users/alkevoskamp/Documents/PD manuscript/Manuscript Figures/")
ggsave("Spp Figure 7 Random_PD and_MPD_change_rcp60_significant_cells_low_disp.tiff",CombNull,width=35, height=13, unit="in", dpi=300, bg="transparent")
#ggsave("Spp Figure 4 Random_PD and_MPD_change_rcp60_significant_cellsLow_disp.png",CombNull,width=24, height=13, unit="in", dpi=600, bg="transparent")


