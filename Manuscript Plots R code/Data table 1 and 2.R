#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#               Add columns for summary tables              #
#                           Risk map                        #
#                       September 2020                      #         
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Get the dataframe #-#-#
setwd("/Users/alkevoskamp/Documents/PD manuscript/Data for plots/")
data <- read.csv("PDandMPD_random_changes_disp1_rcp60_2080_Final.csv")
head(data)


#-#-# Merge area in km into the data frame #-#-#
setwd("/Users/alkevoskamp/Documents/PD manuscript/Data for plots/")
Area <- read.csv("Area in km per grid cell.csv")
head(Area)

CombCon <- merge(data,Area,by=c("x","y"))
head(CombCon)


#-#-# Add increase / decrease categories #-#-#
CombCon$Risk[CombCon$PDpropChange >= 0 & CombCon$MPDpropChange >= 0] <- "MPDincrease/PDincrease"
CombCon$Risk[CombCon$PDpropChange >= 0 & CombCon$MPDpropChange <= 0] <- "MPDdecrease/PDincrease"
CombCon$Risk[CombCon$PDpropChange <= 0 & CombCon$MPDpropChange <= 0] <- "MPDdecrease/PDdecrease"
CombCon$Risk[CombCon$PDpropChange <= 0 & CombCon$MPDpropChange >= 0] <- "MPDincrease/PDdecrease"

CombCon$Risk_categories <- 0

CombCon$Risk_categories[CombCon$Risk == "MPDincrease/PDincrease"] <- "Increasing diversification"
CombCon$Risk_categories[CombCon$Risk == "MPDdecrease/PDincrease"] <- "Increasing competition"
CombCon$Risk_categories[CombCon$Risk == "MPDdecrease/PDdecrease"] <- "Increasing homogenisation"
CombCon$Risk_categories[CombCon$Risk == "MPDincrease/PDdecrease"] <- "Increasing overdispersion"

head(CombCon)


#-#-# Calculate values for Risk table #-#-#
TotalArea <- sum(CombCon$Area_km)
TotalCells <- nrow(CombCon)

## Set the Risk categorie and continent
c <-  "South America" #"North America" "Europe" "Australia" "Asia" "Africa"                                             
r <-   "Increasing diversification" #"Increasing overdispersion" "Increasing competition" "Increasing homogenisation"                    

## Subset by one continent and category to get table values
#OneContinent <- subset(CombCon,Continent == "Africa"|Continent == "Asia"|Continent == "Australia"|Continent == "Europe"|Continent == "North America"|Continent == "South America")
OneContinent <- subset(CombCon,Continent == c)
TotalCellsContinent <- nrow(OneContinent)

ContinentIncDiv <- subset(OneContinent,Risk_categories == r)
TotalAreaContinent <- sum(ContinentIncDiv$Area_km)
TotalCellsContinentRC <- nrow(ContinentIncDiv)

PercAreaContinent <- TotalCellsContinentRC/(TotalCellsContinent/100)

print(TotalAreaContinent)
print(PercAreaContinent)


#-#-# Calculate values for non-random table #-#-#
##Add random change categories
CombCon$PDInvsRIn <- CombCon$FaithACIn - CombCon$FaithInRmean
CombCon$PDOutvsROut <- CombCon$FaithACOut - CombCon$FaithOutRmean
CombCon$MPDInvsRIn <- CombCon$MPDACIn - CombCon$MPDInRmean
CombCon$MPDOutvsROut <- CombCon$MPDACOut - CombCon$MPDOutRmean
head(CombCon)

## Subset to relevant columns 
RiskCat <- CombCon[c("x","y","PDInvsRIn","FaithInPvalue","PDOutvsROut","FaithOutPvalue","MPDInvsRIn","MPDInPvalue","MPDOutvsROut","MPDOutPvalue","Continent","Area_km")]
head(RiskCat)
nrow(RiskCat)

## Set the continent
c <- "South America" #"North America" # "Europe" "Australia" "Asia" "Africa" "South America"           
OneContinent <- subset(RiskCat,Continent == c)  
#OneContinent <- subset(RiskCat,Continent == "Africa"|Continent == "Asia"|Continent == "Australia"|Continent == "Europe"|Continent == "North America"|Continent == "South America")
TotalCellsContinent <- nrow(OneContinent)
TotalAreaContinent <- sum(OneContinent$Area_km)

## Subset to significant cells PD change
## MORE decrease through species loss and LESS increase through species gain
MoreLossPDOut <- subset(OneContinent, FaithOutPvalue >= 0.95)
LessGainPDIn <- subset(OneContinent, FaithInPvalue <= 0.05)
LessGainPDInMoreLossPDOut <- merge(LessGainPDIn,MoreLossPDOut,by=c("x","y"))
LessGainPDInMoreLossPDOutArea <- sum(LessGainPDInMoreLossPDOut$Area_km.x)
LessGainPDInMoreLossPDOutPerc <- nrow(LessGainPDInMoreLossPDOut)/(TotalCellsContinent/100)
print(LessGainPDInMoreLossPDOutArea)
print(LessGainPDInMoreLossPDOutPerc)

## MORE decrease through species loss and MORE increase through species gain
MoreLossPDOut <- subset(OneContinent, FaithOutPvalue >= 0.95)
MoreGainPDIn <- subset(OneContinent, FaithInPvalue <= 0.95)
MoreGainPDInMoreLossPDOut <- merge(MoreGainPDIn,MoreLossPDOut,by=c("x","y"))
MoreGainPDInMoreLossPDOutArea <- sum(MoreGainPDInMoreLossPDOut$Area_km.x)
MoreGainPDInMoreLossPDOutPerc <- nrow(MoreGainPDInMoreLossPDOut)/(TotalCellsContinent/100)
print(MoreGainPDInMoreLossPDOutArea)
print(MoreGainPDInMoreLossPDOutPerc)

## LESS decrease through species loss and MORE increase through species gain
LessLossPDOut <- subset(OneContinent, FaithOutPvalue <= 0.05)
MoreGainPDIn <- subset(OneContinent, FaithInPvalue <= 0.95)
LessLossPDOutMoreGainPDIn <- merge(LessLossPDOut,MoreGainPDIn,by=c("x","y"))
LessLossPDOutMoreGainPDInArea <- sum(LessLossPDOutMoreGainPDIn$Area_km.x)
LessLossPDOutMoreGainPDInPerc <- nrow(LessLossPDOutMoreGainPDIn)/(TotalCellsContinent/100)
print(LessLossPDOutMoreGainPDInArea)
print(LessLossPDOutMoreGainPDInPerc)

## LESS decrease through species loss and LESS increase through species gain
LessLossPDOut <- subset(OneContinent, FaithOutPvalue <= 0.05)
LessGainPDIn <- subset(OneContinent, FaithInPvalue <= 0.05)
LessLossPDOutLessGainPDIn <- merge(LessLossPDOut,LessGainPDIn,by=c("x","y"))
LessLossPDOutLessGainPDInArea <- sum(LessLossPDOutLessGainPDIn$Area_km.x)
LessLossPDOutLessGainPDInPerc <- nrow(LessLossPDOutLessGainPDIn)/(TotalCellsContinent/100)
print(LessLossPDOutLessGainPDInArea)
print(LessLossPDOutLessGainPDInPerc)


## Subset to significant cells MPD change
## MORE decrease through species loss and LESS increase through species gain
MoreLossMPDOut <- subset(OneContinent, MPDOutPvalue >= 0.8)
LessGainMPDIn <- subset(OneContinent, MPDInPvalue <= 0.2)
LessGainMPDInMoreLossMPDOut <- merge(LessGainMPDIn,MoreLossMPDOut,by=c("x","y"))
LessGainMPDInMoreLossMPDOutArea <- sum(LessGainMPDInMoreLossMPDOut$Area_km.x)
LessGainMPDInMoreLossMPDOutPerc <- nrow(LessGainMPDInMoreLossMPDOut)/(TotalCellsContinent/100)
print(LessGainMPDInMoreLossMPDOutArea)
print(LessGainMPDInMoreLossMPDOutPerc)

## MORE decrease through species loss and MORE increase through species gain
MoreLossMPDOut <- subset(OneContinent, MPDOutPvalue >= 0.8)
MoreGainMPDIn <- subset(OneContinent, MPDInPvalue <= 0.8)
MoreGainMPDInMoreLossMPDOut <- merge(MoreGainMPDIn,MoreLossMPDOut,by=c("x","y"))
MoreGainMPDInMoreLossMPDOutArea <- sum(MoreGainMPDInMoreLossMPDOut$Area_km.x)
MoreGainMPDInMoreLossMPDOutPerc <- nrow(MoreGainMPDInMoreLossMPDOut)/(TotalCellsContinent/100)
print(MoreGainMPDInMoreLossMPDOutArea)
print(MoreGainMPDInMoreLossMPDOutPerc)

## LESS decrease through species loss and MORE increase through species gain
LessLossMPDOut <- subset(OneContinent, MPDOutPvalue <= 0.2)
MoreGainMPDIn <- subset(OneContinent, MPDInPvalue <= 0.8)
LessLossMPDOutMoreGainMPDIn <- merge(LessLossMPDOut,MoreGainMPDIn,by=c("x","y"))
LessLossMPDOutMoreGainMPDInArea <- sum(LessLossMPDOutMoreGainMPDIn$Area_km.x)
LessLossMPDOutMoreGainMPDInPerc <- nrow(LessLossMPDOutMoreGainMPDIn)/(TotalCellsContinent/100)
print(LessLossMPDOutMoreGainMPDInArea)
print(LessLossMPDOutMoreGainMPDInPerc)

## LESS decrease through species loss and LESS increase through species gain
LessLossMPDOut <- subset(OneContinent, MPDOutPvalue <= 0.2)
LessGainMPDIn <- subset(OneContinent, MPDInPvalue <= 0.2)
LessLossMPDOutLessGainMPDIn <- merge(LessLossMPDOut,LessGainMPDIn,by=c("x","y"))
LessLossMPDOutLessGainMPDInArea <- sum(LessLossMPDOutLessGainMPDIn$Area_km.x)
LessLossMPDOutLessGainMPDInPerc <- nrow(LessLossMPDOutLessGainMPDIn)/(TotalCellsContinent/100)
print(LessLossMPDOutLessGainMPDInArea)
print(LessLossMPDOutLessGainMPDInPerc)

