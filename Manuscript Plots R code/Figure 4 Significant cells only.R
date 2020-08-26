#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#              Figure 4 for the Manuscript            #
#        Random vs projected changes PD and MPD       #
#         Split by species coming in and going        #
#               Significant cells only                #
#                   November 2019                     #
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


#-#-# Random PD change #-#-#
setwd("E:/PD analysis/Second analysis non random loss win/Result files random PD change/")
RPDchange <- read.csv("PDandMPD_random_changes_disp2_rcp60_2080_RC_Final.csv")
head(RPDchange)
str(RPDchange)
RPDchange$PDOutvsROut <- as.numeric(as.character(RPDchange$PDOutvsROut))
RPDchange$PDInvsRIn <- as.numeric(as.character(RPDchange$PDInvsRIn))
hist(RPDchange$PDOutvsROut)
hist(RPDchange$PDInvsRIn)


#-#-# Change order of coordinates to fix map extent #-#-#
RPDchangeC1 <- subset(RPDchange,x > -170) ## Subset to x coords higher than -170 these are fine
RPDchangeC2 <- subset(RPDchange,x <= -170) ## Subset to x coords lower than -170 these need to move
RPDchangeC2$x <- abs(RPDchangeC2$x) + 10 ## Make coords positive to shift to other side and add 10 degree to attach end of map (179.5 center)  
x <- rev(RPDchangeC2$x) ## Reorder the x coords because they need to be mirrored to be attached on the other side
RPDchangeC2$x <- x ## Replace with the reordered x coords
RPDchangeC3 <- rbind(RPDchangeC1,RPDchangeC2) ##Merge the two dataframes back together

RPDchange <- RPDchangeC3


#-#-# Prepaire the Legends #-#-#
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


#-#-# Random loss vs actual loss #-#-#
min(RPDchange$PDOutvsROut,na.rm = T)
max(RPDchange$PDOutvsROut,na.rm = T)

PDMoreLoss <- subset(RPDchange,PDOutvsROut_Pvalue_class==1)
min(PDMoreLoss$PDOutvsROut,na.rm = T)
max(PDMoreLoss$PDOutvsROut,na.rm = T)

PDLessLoss <- subset(RPDchange,PDOutvsROut_Pvalue_class==4)
min(PDLessLoss$PDOutvsROut,na.rm = T)
max(PDLessLoss$PDOutvsROut,na.rm = T)

bl1 <- colorRampPalette(rev(c("navy","royalblue","lightskyblue")))(538) #1137                     
re1 <- colorRampPalette(rev(c("mistyrose","red2","darkred")))(553)      #468
wi1 <- colorRampPalette(rev(c("gray82","gray90","gray82")))(0)

## Subset signicicantly more loss and less loss
PDRLoss <- subset(RPDchange,PDOutvsROut_Pvalue_class==1 | PDOutvsROut_Pvalue_class==4)
PDRLoss <- PDRLoss[!is.na(PDRLoss$PDOutvsROut),]

plot1<- ggplot(data=PDRLoss, aes(y=y, x=x)) +
  geom_raster(aes(fill =  PDOutvsROut), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(re1,wi1,bl1),limits=c(-553,538))+ # Insert colour and set range
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = "none")+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("(a)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))

print(plot1)

##Legend PD change out Plot 1
bl1 <- colorRampPalette(rev(c("navy","royalblue","lightskyblue")))(553)  #931.3                    
re1 <- colorRampPalette(rev(c("mistyrose","red2","darkred")))(538)       #1026.5
out <- ggplot(data=RPDchange, aes(y=y, x=x)) +
  geom_raster(aes(fill =  PDOutvsROut), stat = "identity", position = "identity", hjust = 0, vjust = 0, interpolate = FALSE,)+
  scale_fill_gradientn("PD change",colours=c(re1,wi1,bl1),limits=c(-553,538),breaks=c(-553,538),labels=c("-553","538"))+ # Insert colour and set range
  theme(legend.position = c(0.5, 0.6))+ # Positioning the legend <- needs to run with legend position to extract legend  
  theme(legend.text=element_text(size=19),legend.title=element_text(size=23))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) # Change size of legend key

legendPlot1<-g_legend(out)
plot(legendPlot1)


#-#-# Random gain vs actual gain #-#-#
min(RPDchange$PDInvsRIn,na.rm = T)
max(RPDchange$PDInvsRIn,na.rm = T)

PDMoreGain <- subset(RPDchange,PDInvsRIn_Pvalue_class==1)
min(PDMoreGain$PDInvsRIn,na.rm = T)
max(PDMoreGain$PDInvsRIn,na.rm = T)

PDLessGain <- subset(RPDchange,PDInvsRIn_Pvalue_class==4)
min(PDLessGain$PDInvsRIn,na.rm = T)
max(PDLessGain$PDInvsRIn,na.rm = T)

bl1 <- colorRampPalette(rev(c("navy","royalblue","lightskyblue")))(603)  #931.3                    
re1 <- colorRampPalette(rev(c("mistyrose","red2","darkred")))(966)       #1026.5

## Subset signicicantly more gain and less gain
PDRGain <- subset(RPDchange,PDInvsRIn_Pvalue_class==1 | PDInvsRIn_Pvalue_class==4)
PDRGain <- PDRGain[!is.na(PDRGain$PDInvsRIn), ]

plot2<- ggplot(data=PDRGain, aes(y=y, x=x)) +
  geom_raster(aes(fill = PDInvsRIn), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(re1,wi1,bl1),limits=c(-966,603))+ # Insert colour and set range
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = "none")+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("(b)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))

print(plot2)

##Legend PD change in Plot2
bl1 <- colorRampPalette(rev(c("navy","royalblue","lightskyblue")))(603)  #931.3                    
re1 <- colorRampPalette(rev(c("mistyrose","red2","darkred")))(966)       #1026.5

In <- ggplot(data=RPDchange, aes(y=y, x=x)) +
  geom_raster(aes(fill = PDInvsRIn), stat = "identity", position = "identity", hjust = 0, vjust = 0, interpolate = FALSE,)+
  scale_fill_gradientn("PD change",colours=c(re1,bl1),limits=c(-966,603),breaks=c(-966,603),labels=c("-966","603"))+ # Insert colour and set range
  theme(legend.position = c(0.5, 0.6))+ # Positioning the legend <- needs to run with legend position to extract legend  
  theme(legend.text=element_text(size=19),legend.title=element_text(size=23))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) # Change size of legend key

legendPlot2<-g_legend(In)
plot(legendPlot2)


#-#-# Random MPD change #-#-#
setwd("E:/PD analysis/Second analysis non random loss win/Result files random PD change/")
RPDchange <- read.csv("PDandMPD_random_changes_disp2_rcp60_2080_RC_Final.csv")
head(RPDchange)
str(RPDchange)
RPDchange$MPDOutvsROut <- as.numeric(as.character(RPDchange$MPDOutvsROut))
RPDchange$MPDInvsRIn <- as.numeric(as.character(RPDchange$MPDInvsRIn))
hist(RPDchange$MPDOutvsROut)
hist(RPDchange$MPDInvsRIn)


#-#-# Random loss vs actual loss #-#-#
min(RPDchange$MPDOutvsROut,na.rm = T)
max(RPDchange$MPDOutvsROut,na.rm = T)

MPDMoreLoss <- subset(RPDchange,MPDOutvsROut_Pvalue_class==1)
min(MPDMoreLoss$MPDOutvsROut,na.rm = T)
max(MPDMoreLoss$MPDOutvsROut,na.rm = T)

MPDLessLoss <- subset(RPDchange,MPDOutvsROut_Pvalue_class==4)
min(MPDLessLoss$MPDOutvsROut,na.rm = T)
max(MPDLessLoss$MPDOutvsROut,na.rm = T)

bl1 <- colorRampPalette(rev(c("navy","royalblue","lightskyblue")))(36) #1137                     
re1 <- colorRampPalette(rev(c("mistyrose","red2","darkred")))(79)      #468

## Subset signicicantly more loss and less loss
MPDRLoss <- subset(RPDchange,MPDInvsRIn_Pvalue_class==1 | MPDInvsRIn_Pvalue_class==4)
MPDRLoss <- MPDRLoss[!is.na(MPDRLoss$MPDOutvsROut), ]

plot3<- ggplot(data=MPDRLoss, aes(y=y, x=x)) +
  geom_raster(aes(fill =  MPDOutvsROut), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(re1,wi1,bl1),limits=c(-79,36))+ # Insert colour and set range
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
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))

print(plot3)

##Legend MPD change out plot3
bl1 <- colorRampPalette(rev(c("navy","royalblue","lightskyblue")))(36)  #931.3                    
re1 <- colorRampPalette(rev(c("mistyrose","red2","darkred")))(79)       #1026.5
out <- ggplot(data=RPDchange, aes(y=y, x=x)) +
  geom_raster(aes(fill =  PDOutvsROut), stat = "identity", position = "identity", hjust = 0, vjust = 0, interpolate = FALSE,)+
  scale_fill_gradientn("MPD change",colours=c(re1,wi1,bl1),limits=c(-79,36),breaks=c(-79,36),labels=c("-79","36"))+ # Insert colour and set range
  theme(legend.position = c(0.5, 0.6))+ # Positioning the legend <- needs to run with legend position to extract legend  
  theme(legend.text=element_text(size=19),legend.title=element_text(size=23))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) # Change size of legend key

legendPlot3<-g_legend(out)
plot(legendPlot3)


#-#-# Random win vs actual win #-#-#
min(RPDchange$MPDInvsRIn,na.rm = T)
max(RPDchange$MPDInvsRIn,na.rm = T)

MPDMoreGain <- subset(RPDchange,MPDInvsRIn_Pvalue_class==1)
min(MPDMoreGain$MPDInvsRIn,na.rm = T)
max(MPDMoreGain$MPDInvsRIn,na.rm = T)

MPDLessGain <- subset(RPDchange,MPDInvsRIn_Pvalue_class==4)
min(MPDLessGain$MPDInvsRIn,na.rm = T)
max(MPDLessGain$MPDInvsRIn,na.rm = T)

bl1 <- colorRampPalette(rev(c("navy","royalblue","lightskyblue")))(33)  #931.3                    
re1 <- colorRampPalette(rev(c("mistyrose","red2","darkred")))(18)       #1026.5

## Subset signicicantly more loss and less loss
MPDRGain <- subset(RPDchange,MPDInvsRIn_Pvalue_class==1 | MPDInvsRIn_Pvalue_class==4)
MPDRGain <- MPDRGain[!is.na(MPDRGain$MPDInvsRIn), ]

plot4 <- ggplot(data=MPDRGain, aes(y=y, x=x)) +
  geom_raster(aes(fill =  MPDInvsRIn), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(re1,wi1,bl1),limits=c(-18,33))+ # Insert colour and set range
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
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))

print(plot4)

##Legend MPD change in Plot4
bl1 <- colorRampPalette(rev(c("navy","royalblue","lightskyblue")))(33)  #931.3                    
re1 <- colorRampPalette(rev(c("mistyrose","red2","darkred")))(18)       #1026.5

In <- ggplot(data=RPDchange, aes(y=y, x=x)) +
  geom_raster(aes(fill = PDInvsRIn), stat = "identity", position = "identity", hjust = 0, vjust = 0, interpolate = FALSE,)+
  scale_fill_gradientn("MPD change",colours=c(re1,wi1,bl1),limits=c(-18,33),breaks=c(-18,33),labels=c("-18","33"))+ # Insert colour and set range
  theme(legend.position = c(0.5, 0.6))+ # Positioning the legend <- needs to run with legend position to extract legend  
  theme(legend.text=element_text(size=19),legend.title=element_text(size=23))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) # Change size of legend key

legendPlot4<-g_legend(In)
plot(legendPlot4)


#-#-# put plots together and save #-#-#

#-#-# Combine plot random PD loss and gain #-#-#  
CombNull <- arrangeGrob(plot1,legendPlot1,plot2,legendPlot2,plot3,legendPlot3,plot4,legendPlot4,
                        widths = c(4,1,4,1),
                        heights = c(0.3,0.3),
                        ncol = 4,
                        nrow = 2)



plot(CombNull)


setwd("E:/PD analysis/PD manuscript/PD Manuscript Figures/")
ggsave("Figure 4 Random_PD and_MPD_change_rcp60_80_significant_cells.tiff",CombNull,width=24, height=10, unit="in", dpi=300, bg="transparent")

