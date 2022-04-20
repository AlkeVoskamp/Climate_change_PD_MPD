#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#             Figure S6 for the Manuscript            #
#                Revisions MPD current                #
#                     April 2022                      #
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

setwd("/Users/alkevoskamp/Dropbox (Senckenberg)/AG BGFM's shared workspace/Voskamp/PD manuscript/PD analysis/Second analysis non random loss win/Result files random PD change/")
RPDchange <- read.csv("PDandMPD_random_changes_disp2_rcp60_2080_RC_Final.csv")
RPDchange[is.na(RPDchange)] <- 0
head(RPDchange)

## Min max values
min(RPDchange$SRCur)
max(RPDchange$SRCur)

min(RPDchange$MPD)
max(RPDchange$MPD)
hist(RPDchange$MPD)

min(RPDchange$Faith)
max(RPDchange$Faith)

cols <- rev(brewer.pal(9, "Spectral"))

SR <- ggplot(data=RPDchange, aes(y=y, x=x)) +
  geom_raster(aes(fill =  SRCur), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(cols),limits=c(0,1045))+ # Insert colour and set range
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  #theme(legend.position = "none")+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("Species richness")+ 
  theme(plot.title = element_text(size = 16,hjust = 0))

print(SR)

MPD <- ggplot(data=RPDchange, aes(y=y, x=x)) +
  geom_raster(aes(fill =  MPD), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(cols),limits=c(109,178))+ # Insert colour and set range
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  #theme(legend.position = "none")+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("Mean pairwise distance")+ 
  theme(plot.title = element_text(size = 16,hjust = 0))

print(PD)

PD <- ggplot(data=RPDchange, aes(y=y, x=x)) +
  geom_raster(aes(fill =  Faith), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(cols),limits=c(292,14932))+ # Insert colour and set range
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  #theme(legend.position = "none")+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("Faith phylogenetic diversity")+ 
  theme(plot.title = element_text(size = 16,hjust = 0))

print(PD)

CombNull <- arrangeGrob(SR,PD,MPD,
                        widths = c(4,4,4),
                        heights = c(0.2),
                        ncol = 3,
                        nrow = 1)

plot(CombNull)

setwd("/Users/alkevoskamp/Dropbox (Senckenberg)/AG BGFM's shared workspace/Voskamp/PD manuscript/Manuscript Figures/")
ggsave("Spp Figure 6 Current measures revision.tiff",CombNull,width=14, height=3, unit="in", dpi=600, bg="transparent")



