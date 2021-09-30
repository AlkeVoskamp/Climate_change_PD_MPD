#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#               Add columns for summary tables              #
#                           Risk map                        #
#                       September 2020                      #         
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Get the dataframe #-#-#
setwd("/Users/alkevoskamp/Documents/PD manuscript/Data for plots/Data for tables/")
data <- read.csv("PDandMPD_random_changes_medium_dispersal_rcp26_2080_ass_str.csv")
head(data)


#-#-# Merge area in km into the data frame #-#-#
setwd("/Users/alkevoskamp/Documents/PD manuscript/Data for plots/Support files/")
Area <- read.csv("Area in km per grid cell.csv")
head(Area)

CombCon <- merge(data,Area,by=c("x","y"))
head(CombCon)

#-#-# Calculate values for Risk table #-#-#
TotalArea <- sum(CombCon$Area_km)
TotalCells <- nrow(CombCon)

## Set the Risk categorie and continent
continent <-  c("Europe","Australia","Asia","Africa","South America","North America")                                         
risk <-   c("Increasing homogenisation","No significant change","Increasing diversification","Increasing overdispersion","Increasing clustering")               

## Subset by continent and risk category to get table values
Continent <- lapply(continent, function(c){
  
  print(c)

  OneContinent <- subset(CombCon,Continent == c)
  TotalCellsContinent <- nrow(OneContinent)
  TotalAreaContinent <- sum(OneContinent$Area_km)
  
  Risk <- lapply(risk, function(r){

    ContinentRiskCat <- subset(OneContinent,Risk_cat_final == r)
    TotalAreaContinentRC <- sum(ContinentRiskCat$Area_km)
    TotalCellsContinentRC <- nrow(ContinentRiskCat)

    PercCellsContinent <- TotalCellsContinentRC/(TotalCellsContinent/100)
    PercAreaContinent <- TotalAreaContinentRC/(TotalAreaContinent/100)
 
    riskdata <- as.data.frame(cbind(PercCellsContinent,PercAreaContinent))
    riskdata$Risk <- r
    
    riskdata$TotalAreaContinentRC <- TotalAreaContinentRC
    riskdata$TotalCellsContinentRC <- TotalCellsContinentRC
    
    return(riskdata)
    
  })
   
  allriskdata <- as.data.frame(do.call(rbind,Risk))
  allriskdata$Continent <- c
  allriskdata$TotalAreaContinent <- TotalAreaContinent
  allriskdata$TotalCellsContinent <- TotalCellsContinent

  return(allriskdata)
  
})
  
AllData <- as.data.frame(do.call(rbind,Continent))

setwd("/Users/alkevoskamp/Documents/PD manuscript/Data for plots/Data for tables/")
write.csv(AllData,"Low_dispersal_rcp60_2080_ass_str_Summary.csv")


## Get global numbers
GlobalIncHom <- subset(CombCon,Risk_cat_final == "Increasing homogenisation")
GlobalIncHomArea <- sum(GlobalIncHom$Area_km)
GlobalNoSig <- subset(CombCon,Risk_cat_final == "No significant change") 
GlobalNoSigArea <- sum(GlobalNoSig$Area_km)
GlobalIncDiv <- subset(CombCon,Risk_cat_final == "Increasing diversification") 
GlobalIncDivArea <- sum(GlobalIncDiv$Area_km)
GlobalIncOv <- subset(CombCon,Risk_cat_final == "Increasing overdispersion") 
GlobalIncOvArea <- sum(GlobalIncOv$Area_km)
GlobalIncClu <- subset(CombCon,Risk_cat_final == "Increasing clustering") 
GlobalIncCluArea <- sum(GlobalIncClu$Area_km)

GlobalIncHomPerc <- GlobalIncHomArea/(TotalArea/100)
GlobalNoSigPerc <- GlobalNoSigArea/(TotalArea/100)
GlobalIncDivPerc <- GlobalIncDivArea/(TotalArea/100)
GlobalIncOvPerc <- GlobalIncOvArea/(TotalArea/100)
GlobalIncCluPerc <- GlobalIncCluArea/(TotalArea/100)

GlobalIncHomPerc
GlobalNoSigPerc
GlobalIncDivPerc
GlobalIncOvPerc
GlobalIncCluPerc

sum(GlobalIncHomPerc,GlobalNoSigPerc,GlobalIncDivPerc,GlobalIncOvPerc,GlobalIncCluPerc)

GlobalIncHomArea
GlobalNoSigArea
GlobalIncDivArea
GlobalIncOvArea
GlobalIncCluArea

sum(GlobalIncHomArea,GlobalNoSigArea,GlobalIncDivArea,GlobalIncOvArea,GlobalIncCluArea)
sum(CombCon$Area_km)



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

