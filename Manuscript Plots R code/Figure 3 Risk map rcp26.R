#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#                 Figures for the Manuscript                #
#                     PD vs MPD risk map                    #
#                       November 2019                       #         
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

rm(list=ls())

#-#-# Load the libraries #-#-#
library(ggplot2)
# library(lattice)
# library(rasterVis)
# library(maps)
library(grid)
library(gridExtra)
library(rgdal)
# library(RColorBrewer)
# library(colorspace)
library(dplyr)

#-#-# Get the data #-#-#
setwd("/Users/alkevoskamp/Documents/PD manuscript/Data for plots/")
CombCon <- read.csv("PDandMPD_random_changes_disp2_rcp26_2080_Final.csv")
head(CombCon)
str(CombCon)


CombCon$Risk[CombCon$PDpropChange >= 0 & CombCon$MPDpropChange >= 0] <- "MPDincrease/PDincrease"
CombCon$Risk[CombCon$PDpropChange >= 0 & CombCon$MPDpropChange <= 0] <- "MPDdecrease/PDincrease"
CombCon$Risk[CombCon$PDpropChange <= 0 & CombCon$MPDpropChange <= 0] <- "MPDdecrease/PDdecrease"
CombCon$Risk[CombCon$PDpropChange <= 0 & CombCon$MPDpropChange >= 0] <- "MPDincrease/PDdecrease"

CombCon$Risk_categories <- 0

CombCon$Risk_categories[CombCon$Risk == "MPDincrease/PDincrease"] <- "Increasing diversification"
CombCon$Risk_categories[CombCon$Risk == "MPDdecrease/PDincrease"] <- "Increasing competition"
CombCon$Risk_categories[CombCon$Risk == "MPDdecrease/PDdecrease"] <- "Increasing homogenisation"
CombCon$Risk_categories[CombCon$Risk == "MPDincrease/PDdecrease"] <- "Increasing overdispersion"

CombConSc <- CombCon[c("x","y","PDpropChange","MPDpropChange","Risk_categories")]

CombConSc <- na.omit(CombConSc)

#write.csv(CombCon,"PDandMPD_random_changes_disp2_rcp60_2080_RC_Final_Missing_Continent.csv")

#-#-# Worldmap (global and cropped) #-#-#
setwd("/Users/alkevoskamp/Documents/PD manuscript/Data for plots/ne_50m_admin_0_countries/")

worldmap <- readOGR("ne_50m_admin_0_countries.shp", layer="ne_50m_admin_0_countries")
plot(worldmap)


#-#-# Scatterplot %PD vs %MPD change #-#-#
SRscatter <- ggplot(CombConSc,aes(x=PDpropChange,y=MPDpropChange,col=Risk_categories))+
  scale_color_manual(values=c("grey38","grey38","grey38","grey38"),na.translate = F)+ 
  scale_x_continuous(limits = c(-200, 200))+
  scale_y_continuous(limits = c(-35, 40))+
  annotate("rect", xmin = -200, xmax = 0, ymin = -35, ymax = 0, fill= "mediumpurple1", alpha = 0.8)  + 
  annotate("rect",xmin = 0, xmax = 200, ymin = -35, ymax = 0, fill = "indianred1", alpha = 0.8)+
  annotate("rect",xmin = 0, xmax = 200, ymin = 0, ymax = 40, fill = "yellow2", alpha = 0.8)+
  annotate("rect",xmin = -200, xmax = 0, ymin = 0, ymax = 40, fill = "turquoise1", alpha = 0.8)+
  annotate("text", x = 100, y = 30, label = "Increasing\ndiversification", colour = "black",size=9)+
  annotate("text", x = 100, y = -25, label = "Increasing\nclustering", colour = "black",size=9)+
  annotate("text", x = -100, y = -25, label = "Increasing\nhomogenisation", colour = "black",size=9)+
  annotate("text", x = -100, y = 30, label = "Increasing\noverdispersion", colour = "black",size=9)+
  geom_point(alpha = 1,shape=1) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        axis.title.x = element_text(size=26),
        axis.title.y = element_text(size=26),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
  theme(legend.position = "none")+
  guides(colour = guide_legend(override.aes = list(size=8,fill=NA)))+
  geom_hline(yintercept=0, linetype="dashed", color= "black")+
  geom_vline(xintercept=0, linetype="dashed", color= "black")+
  labs(x="% change in PD\n", y="\n% change in MPD", title="")+
  ggtitle("(a)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))
plot(SRscatter)

#ggsave("Figure 2 Part 1 PD vs MPD change scatter rcp60.tiff",SRscatter,width=24, height=14, unit="in", dpi=600, bg="transparent")

#-#-# Map %PD vs %MPD change #-#-#
binaryColors <- c("indianred1","yellow2","mediumpurple1","turquoise1")
#down right, up right, down left, up left

## Rearrange the edge coordinates to fit country boarder map
# RPDchangeC1 <- subset(RPDchange,x > -170) ## Subset to x coords higher than -170 these are fine
# RPDchangeC2 <- subset(RPDchange,x <= -170) ## Subset to x coords lower than -170 these need to move
# RPDchangeC2$x <- abs(RPDchangeC2$x) + 10 ## Make coords positive to shift to other side and add 10 degree to attach end of map (179.5 center)  
# x <- rev(RPDchangeC2$x) ## Reorder the x coords because they need to be mirrored to be attached on the other side
# RPDchangeC2$x <- x ## Replace with the reordered x coords
# RPDchangeC3 <- rbind(RPDchangeC1,RPDchangeC2) ##Merge the two dataframes back together

## Set alpha
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
CombConSc$Intensity <- range01((abs(CombConSc$PDpropChange)+(abs(CombConSc$MPDpropChange))))+0.5
CombConSc$Intensity[CombConSc$Intensity > 1] <- 1

RiskMap<- ggplot(data=CombConSc, aes(y=y, x=x)) +
  geom_raster(aes(fill = Risk_categories), stat = "identity", position = "identity", alpha = 0.8)+
  scale_fill_manual(values = binaryColors)+ # Insert colour and set range
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

print(RiskMap)


#-#-# Combine plots #-#-#
Final <- arrangeGrob(SRscatter,RiskMap,
                     widths = c(5,5),
                     heights = c(1),
                     ncol = 2,
                     nrow = 1)
plot(Final)

#setwd("/Users/alkevoskamp/Documents/PD manuscript/Manuscript Figures/")
#ggsave("Figure 3 PD vs MPD change map rcp60.tiff",Final,width=18, height=13, unit="in", dpi=600, bg="transparent")


#-#-# Percentages No of assemblages in different risk categories for text #-#-#
## Which categories are there
unique(CombCon$Risk_categories)

## Overdispersion
IncrOverdispersion <- subset(CombCon,Risk_categories == "Increasing overdispersion")
head(IncrOverdispersion)
nrow(IncrOverdispersion)
round((nrow(IncrOverdispersion))/(nrow(CombCon)/100),0)

## Diversification
IncrDiversification <- subset(CombCon,Risk_categories == "Increasing diversification")
head(IncrDiversification)
nrow(IncrDiversification)
round((nrow(IncrDiversification))/(nrow(CombCon)/100),0)

## Competition
IncrCompetition <- subset(CombCon,Risk_categories == "Increasing competition")
head(IncrCompetition)
nrow(IncrCompetition)
round((nrow(IncrCompetition))/(nrow(CombCon)/100),0)

## Homogenisation
IncrHomogenisation <- subset(CombCon,Risk_categories == "Increasing homogenisation")
head(IncrHomogenisation)
nrow(IncrHomogenisation)
round((nrow(IncrHomogenisation))/(nrow(CombCon)/100),0)


#-#-# Histogram based on No of assemblages changing per continent #-#-#
head(CombCon)
BarChart <- CombCon[c("Risk_categories","Continent")]
BarChart$value <- 1
head(BarChart)
BarChart <- na.omit(BarChart)

BarChartSum <- as.data.frame(BarChart %>% group_by(Risk_categories,Continent) %>% summarize(n()))
colnames(BarChartSum) <- c("Risk_categories","Continent","value")


BarPlot <- ggplot(BarChartSum, aes(fill=Risk_categories, y=value, x=Continent)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values = binaryColors)+ # Insert colour and set range
  theme(legend.position = "none")+ # Positioning the legend 
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size=22),
        axis.title.x = element_text(size=26),
        axis.title.y = element_text(size=26),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  labs(x="", y="   # of species\n   assemblages\n", title="")+
  ggtitle("(c)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))
plot(BarPlot)


#-#-# Combine 3 plots #-#-#
FinalComb <- grid.arrange(
  arrangeGrob(SRscatter,RiskMap,
              widths = c(6,8),
              heights = c(1),
              ncol = 2,
              nrow = 1),
  arrangeGrob(BarPlot,
              widths = c(4),
              heights = c(8),
              ncol = 1,
              nrow = 1),
  nrow=2, ncol =1, heights=c(4,1)
)
plot(FinalComb)


setwd("/Users/alkevoskamp/Documents/PD manuscript/Manuscript Figures/")
ggsave("Figure 3 PD vs MPD change map rcp26.tiff",FinalComb,width=20, height=10, unit="in", dpi=600, bg="transparent")
