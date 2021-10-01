#-#-# Extract mean dispersal across all species #-#-#
#-#-# Add dispersal to predictions data

## Load libraries
rm(list=ls())
packages <- c("raster", "readr", "rgeos", "tidyr", "sp", "ggplot2", "snowfall")

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

# Load required packages
l <- sapply(packages, require, character.only = TRUE); rm(packages, l)

## Get the data
setwd("E:/Data for Joe trait analysis/Thresholded_model_results_Jetz_taxonomy/")
AllFiles <- list.files()
head(AllFiles)

## Extract the distances
Distances <- lapply(AllFiles, function(x){
  
  name <- paste0(strsplit(x,split="_")[[1]][1],"_",strsplit(x,split="_")[[1]][2])
  print(name)
  
  ## Current dist
  origDist <- get(load(x))
  origDist <- origDist[c(1:3)]
  origDist <- subset(origDist,presence==1)
  if(nrow(origDist)>1){
  spOrig <- rasterFromXYZ(origDist)
  
  ## Calculate dispersal distance
  poly <- raster::rasterToPolygons(spOrig,fun=function(x){x == 1},dissolve=TRUE)
  poly <- sp::disaggregate(poly)
  largepoly <- poly[which.max(sapply(poly@polygons, function(x) x@Polygons[[1]]@area)),]
  p1x <- c(xmax(largepoly),0) #giving the distance in meters (lonlat=TRUE) 
  p2x <- c(xmin(largepoly),0)
  xdist <- pointDistance(p1x,p2x,lonlat=T)
  p1y <- c(0,ymax(largepoly))
  p2y <- c(0,ymin(largepoly))
  ydist <- pointDistance(p1y,p2y,lonlat=T)
  
  ## Various distances
  dist1 <- round(sqrt((xdist^2+ydist^2))/4,2)
  
  dist2 <- round(sqrt((xdist^2+ydist^2))/2,2)
  
  dist3 <- round(sqrt((xdist^2+ydist^2)),2)
  
  dist4 <- round(sqrt((xdist^2+ydist^2))*2,2)
  
  AllData <- as.data.frame(cbind(name,dist1,dist2,dist3,dist4))
  
  colnames(AllData) <- c("Species","Distance1","Distance2","Distance3","Distance4")
  
  return(AllData)
  }
})

AllDistances <- do.call(rbind,Distances)
head(AllDistances)
nrow(AllDistances)

setwd()
write.csv(AllDistances,"E:/PD analysis/Second analysis non random loss win/Distance_buffer_in_meters_per_species.csv")
