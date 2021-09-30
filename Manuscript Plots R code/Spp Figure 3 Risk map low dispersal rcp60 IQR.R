#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#                 Figures for the Manuscript                #
#                     PD vs MPD risk map                    #
#                       November 2019                       #         
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

rm(list=ls())

#-#-# Load the libraries #-#-#
library(ggplot2)
library(grid)
library(gridExtra)
library(rgdal)
library(dplyr)

#-#-# Get the data #-#-#
setwd("/Users/alkevoskamp/Documents/PD manuscript/Data for plots/")
CombCon <- read.csv("PDandMPD_random_changes_low_dispersal_rcp60_2080.csv")
head(CombCon)
str(CombCon)

#-#-# Add a class for no significant change #-#-#
yMedian <- median(CombCon$MPDpropChange)
xMedian <- median(CombCon$PDpropChange)
yIQ1 <- quantile(CombCon$MPDpropChange)[2] 
yIQ3 <- quantile(CombCon$MPDpropChange)[4] 
xIQ1 <- quantile(CombCon$PDpropChange)[2] 
xIQ3 <- quantile(CombCon$PDpropChange)[4] 
xmax <- max(CombCon$PDpropChange)
ymax <- max(CombCon$MPDpropChange)
xmin <- min(CombCon$PDpropChange)
ymin <- min(CombCon$MPDpropChange)

#-#-# Points center polygon #-#-#
P1 <- c(xIQ1,yMedian)
P2 <- c(xMedian,yIQ3)
P3 <- c(xIQ3,yMedian)
P4 <- c(xMedian,yIQ1)

#-#-# Other Points #-#-# 
P5 <- c(xmin,yMedian)
P6 <- c(xmin,ymax)
P7 <- c(xMedian,ymax)
P8 <- c(xmax,ymax)
P9 <- c(xmax,yMedian)
P10 <- c(xmax,ymin)
P11 <- c(xMedian,ymin)
P12 <- c(xmin,ymin)

#-#-# Prepaire point dataframe for plot #-#-#
ClassDF <- CombCon[c("X", "MPDpropChange", "PDpropChange")]
colnames(ClassDF) <- c("X", "y", "x")
str(ClassDF)
head(ClassDF)

#-#-# Prepaire IBA xy only dataframe #-#-#
DFxy <- ClassDF[c("x", "y")]
DFxy$x <- as.numeric(as.vector(DFxy$x))
DFxy <- SpatialPoints(DFxy)

#-#-# Select points for polygon E #-#-#
PointsE <- as.data.frame(do.call("rbind", list(P1,P2,P3,P4)))
colnames(PointsE) <- c("x","y")

#-#-# Make polygon E #-#-#
PolygonE = Polygon(PointsE)
PolygonE = Polygons(list(PolygonE),1)
PolygonE = SpatialPolygons(list(PolygonE))
plot(PolygonE)

#-#-# Check which points lie within polygon E #-#-#
ClassE <- over(DFxy,PolygonE)
DFpointsE <- cbind(ClassDF,ClassE)

#-#-# Select points for polygon D #-#-#
PointsD <- as.data.frame(do.call("rbind", list(P5,P6,P7,P2,P1)))
colnames(PointsD) <- c("x","y")

#-#-# Make polygon D #-#-#
PolygonD = Polygon(PointsD)
PolygonD = Polygons(list(PolygonD),1)
PolygonD = SpatialPolygons(list(PolygonD))
plot(PolygonD)

#-#-# Check which points lie within polygon D #-#-#
ClassD <- over(DFxy,PolygonD)
DFpointsED <- cbind(DFpointsE,ClassD)

#-#-# Select points for polygon C #-#-#
PointsC <- as.data.frame(do.call("rbind", list(P7,P2,P3,P9,P8)))
colnames(PointsC) <- c("x","y")

#-#-# Make polygon C #-#-#
PolygonC = Polygon(PointsC)
PolygonC = Polygons(list(PolygonC),1)
PolygonC = SpatialPolygons(list(PolygonC))
plot(PolygonC)

#-#-# Check which points lie within polygon C #-#-#
ClassC <- over(DFxy,PolygonC)
DFpointsEDC <- cbind(DFpointsED,ClassC)

#-#-# Select points for polygon B #-#-#
PointsB <- as.data.frame(do.call("rbind", list(P4,P3,P9,P10,P11)))
colnames(PointsB) <- c("x","y")

#-#-# Make polygon B #-#-#
PolygonB = Polygon(PointsB)
PolygonB = Polygons(list(PolygonB),1)
PolygonB = SpatialPolygons(list(PolygonB))
plot(PolygonB)

#-#-# Check which points lie within polygon B #-#-#
ClassB <- over(DFxy,PolygonB)
DFpointsEDCB <- cbind(DFpointsEDC,ClassB)

#-#-# Select points for polygon A #-#-#
PointsA <- as.data.frame(do.call("rbind", list(P12,P5,P1,P4,P11)))
colnames(PointsA) <- c("x","y")

#-#-# Make polygon A #-#-#
PolygonA = Polygon(PointsA)
PolygonA = Polygons(list(PolygonA),1)
PolygonA = SpatialPolygons(list(PolygonA))
plot(PolygonA)

#-#-# Check which points lie within polygon A #-#-#
ClassA <- over(DFxy,PolygonA)
DFpointsEDCBA <- cbind(DFpointsEDCB,ClassA)

DFpointsEDCBA[is.na(DFpointsEDCBA)] <- 0
DFpointsEDCBA <- DFpointsEDCBA[c("X", "ClassE", "ClassD", "ClassC", "ClassB", "ClassA")]

Plotdata <- merge(CombCon, DFpointsEDCBA, by = "X", all.x =T)

#-#-# Ad category names #-#-#
Plotdata$Risk_cat_final <- 0
Plotdata$Risk_cat_final[Plotdata$ClassE == 1] <- "No significant change"
Plotdata$Risk_cat_final[Plotdata$ClassD == 1] <- "Increasing overdispersion"
Plotdata$Risk_cat_final[Plotdata$ClassC == 1] <- "Increasing diversification"
Plotdata$Risk_cat_final[Plotdata$ClassB == 1] <- "Increasing clustering"
Plotdata$Risk_cat_final[Plotdata$ClassA == 1] <- "Increasing homogenisation"

## Save full table
#write.csv(Plotdata,"/Users/alkevoskamp/Documents/PD manuscript/Data for plots/Data for tables/PDandMPD_random_changes_low_dispersal_rcp60_2080_ass_str.csv")

CombConSc <- Plotdata[c("x","y","PDpropChange","MPDpropChange","Risk_cat_final")]

CombConSc <- na.omit(CombConSc)


#-#-# Worldmap (global and cropped) #-#-#
setwd("/Users/alkevoskamp/Documents/PD manuscript/Data for plots/Support files/ne_50m_admin_0_countries/")

worldmap <- readOGR("ne_50m_admin_0_countries.shp", layer="ne_50m_admin_0_countries")
plot(worldmap)


#-#-# Scatterplot %PD vs %MPD change #-#-#
SRscatter <- ggplot(CombConSc,aes(x=PDpropChange,y=MPDpropChange,col=Risk_cat_final))+
  #scale_color_manual(values=c("indianred1","yellow2","mediumpurple1","turquoise1","white"),na.translate = F)+
  scale_color_manual(values=c("gray40","gray40","gray40","gray40","gray80"),na.translate = F)+ 
  scale_x_continuous(limits = c(-200, 200))+
  scale_y_continuous(limits = c(-35, 40))+
  annotate("rect", xmin = -200, xmax = 0, ymin = -35, ymax = 0, fill= "#fc921f", alpha = 0.4)  + 
  annotate("rect",xmin = 0, xmax = 200, ymin = -35, ymax = 0, fill = "#ed5151", alpha = 0.4)+
  annotate("rect",xmin = 0, xmax = 200, ymin = 0, ymax = 40, fill = "#a7c636", alpha = 0.4)+
  annotate("rect",xmin = -200, xmax = 0, ymin = 0, ymax = 40, fill = "#149ece", alpha = 0.4)+
  annotate("text", x = 100, y = 30, label = "Increasing\ndiversification", colour = "black",size=7)+
  annotate("text", x = 100, y = -25, label = "Increasing\nclustering", colour = "black",size=7)+
  annotate("text", x = -100, y = -25, label = "Increasing\nhomogenisation", colour = "black",size=7)+
  annotate("text", x = -100, y = 30, label = "Increasing\noverdispersion", colour = "black",size=7)+
  geom_point(alpha = 1,size=0.01) +
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
  labs(x="% change in Faith PD\n", y="\n% change in MPD", title="")+
  ggtitle("(a)")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))
plot(SRscatter)


#-#-# Map %PD vs %MPD change #-#-#
binaryColors <- c("#ed5151","#a7c636","#fc921f","#149ece","gray80")
#down right, up right, down left, up left


## Set alpha
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
CombConSc$Intensity <- range01((abs(CombConSc$PDpropChange)+(abs(CombConSc$MPDpropChange))))+0.5
CombConSc$Intensity[CombConSc$Intensity > 1] <- 1

RiskMap<- ggplot(data=CombConSc, aes(y=y, x=x)) +
  geom_raster(aes(fill = Risk_cat_final), stat = "identity", position = "identity", alpha = 0.8)+
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


#-#-# Percentages No of assemblages in different risk categories for text #-#-#
## Which categories are there
unique(CombConSc$Risk_cat_final)

## NoChange
NoChange <- subset(CombConSc,Risk_cat_final == "No significant change")
head(NoChange)
nrow(NoChange)
round((nrow(NoChange))/(nrow(CombCon)/100),0)

## Overdispersion
IncrOverdispersion <- subset(CombConSc,Risk_cat_final == "Increasing overdispersion")
head(IncrOverdispersion)
nrow(IncrOverdispersion)
round((nrow(IncrOverdispersion))/(nrow(CombCon)/100),0)

## Diversification
IncrDiversification <- subset(CombConSc,Risk_cat_final == "Increasing diversification")
head(IncrDiversification)
nrow(IncrDiversification)
round((nrow(IncrDiversification))/(nrow(CombCon)/100),0)

## Competition
IncrCompetition <- subset(CombConSc,Risk_cat_final == "Increasing competition")
head(IncrCompetition)
nrow(IncrCompetition)
round((nrow(IncrCompetition))/(nrow(CombCon)/100),0)

## Homogenisation
IncrHomogenisation <- subset(CombConSc,Risk_cat_final == "Increasing homogenisation")
head(IncrHomogenisation)
nrow(IncrHomogenisation)
round((nrow(IncrHomogenisation))/(nrow(CombCon)/100),0)


#-#-# Histogram based on No of assemblages changing per continent #-#-#
BarChart <- merge(CombConSc,CombCon[c("x","y","Continent")],by = c("x","y"))
BarChart <- BarChart[c("Risk_cat_final","Continent")]
BarChart$value <- 1
head(BarChart)
BarChart <- na.omit(BarChart)

BarChartSum <- as.data.frame(BarChart %>% group_by(Risk_cat_final,Continent) %>% summarize(n()))
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
              widths = c(4,8),
              heights = c(1),
              ncol = 2,
              nrow = 1),
  arrangeGrob(BarPlot,
              widths = c(4),
              heights = c(8),
              ncol = 1,
              nrow = 1),
  nrow=2, ncol =1, heights=c(4,2)
)
plot(FinalComb)


setwd("/Users/alkevoskamp/Documents/PD manuscript/Manuscript Figures/")
ggsave("Spp Figure 3 PD vs MPD change map rcp60 low disp IQuart.tiff",FinalComb,width=18, height=8, unit="in", dpi=600, bg="transparent")
