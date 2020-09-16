dataframe <- read.csv("E:/PD analysis/Second analysis non random loss win/Result files random PD change/PDandMPD_random_changes_disp2_rcp60_2080_RC_Final_Missing_Continent.csv")
head(dataframe)
dataframexy <- dataframe[c("x","y")]

dataraster <- rasterFromXYZ(dataframexy)
area <- area(dataraster)

coord <- round(coordinates(area),4)
values <- getValues(area)
AreaData <- (as.data.frame(cbind(coord,values)))
colnames(AreaData) <- c("x","y","Area_km")
head(AreaData)

finalframe <- merge(dataframe,AreaData,by=c("x","y"))
head(finalframe)

setwd("E:/PD analysis/Second analysis non random loss win/Result files random PD change/")
write.csv(finalframe,"PDandMPD_random_changes_disp2_rcp60_2080_RC_Final_Missing_Continent_Area.csv")

