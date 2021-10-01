#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#                   Create matrix of grid cell IDS                    #
#    Each grid cell and list of all cells within individual buffer    #
#      Null community per gridcell based on current distributions     #
#                           Alke, April 2019                          #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#-#-# Clear all memory
rm(list=ls())

#-#-# Load libraries #-#-#
library(raster)
library(rgdal)
library(snowfall)
library(lattice)

#-#-# Get data #-#-#
distancepath <- "/home/avoskamp/PDanalysis/Data_PD_analysis/"
matrixpath <- "/home/avoskamp/PDanalysis/Data_PD_analysis/"
resultsPath <- "/home/avoskamp/PDanalysis/Species_pool_null_model_dispersal1" ## <----- Change to right directory
#resultsPath <- "/home/avoskamp/PDanalysis/Species_pool_null_model_modelled_dispersal"
# distancepath <- "E:/PD analysis/Second analysis non random loss win/"
# matrixpath <- "E:/Data for Joe trait analysis/Summarized projections Jetz taxonomy/RdataFiles/"
# resultsPath <- "E:/PD analysis/Second analysis non random loss win/"

## Species matrix current distribution
SpM <- get(load(paste0(matrixpath,"Original_distributions.RData")))

## Dispersal distances ##Read in right distance file
distances <- read.csv(paste0(distancepath,"Distance_buffer_in_meters_per_species.csv")) ## Distance1 d/4 and Distance2 d/2
#distances <- read.csv(paste0(distancepath,"Final_dispersal_values_with_all_corrections_BioScen_years.csv"))[c(1,13)]
#colnames(distances) <- c("Species","Distance2")
#distances$Distance2 <- distances$Distance2*1000 ## Multiply to meters ## Only modelled dispersal because its in km

## Terrestrial cells with at least one species 
grid <- SpM[1:2]
grid$SR <- rowSums(SpM[3:ncol(SpM)],na.rm=TRUE) 
grid <- subset(grid,SR>0)
grid$value <- NA
grid <- grid[c(1,2,4)]

numbers1 <- c(1:2500)
numbers2 <- c(2501:5000)
numbers3 <- c(5001:7500)
numbers4 <- c(7501:10000)
numbers5 <- c(10001:12500)
numbers6 <- c(12501:15000)
numbers7 <- c(15001:17500)
numbers8 <- c(17501:20000)
numbers9 <- c(20001:22500)
numbers10 <- c(22501:25000)
numbers11 <- c(25001:27500)
numbers12 <- c(27501:30000)
numbers13 <- c(30001:32500)
numbers14 <- c(32501:35000)
numbers15 <- c(35001:37500)
numbers16 <- c(37501:40000)
numbers17 <- c(40001:42500)
numbers18 <- c(42501:45000)
numbers19 <- c(45001:47500)
numbers20 <- c(47501:50000)
numbers21 <- c(50001:52500)
numbers22 <- c(52501:55000)
numbers23 <- c(55001:57500)
numbers24 <- c(57501:60000)
numbers25 <- c(60001:62500)
numbers26 <- c(62501:65447)

## Set up snowfall to run parallel
sfInit(parallel=TRUE, cpus=ceiling(0.50*parallel::detectCores()))
sfLibrary(raster)
sfExport(list=c("distancepath","matrixpath","resultsPath","SpM","distances","grid","list")) 

## Create species pools for null models
SpeciesPool <- sfL lapply(numbers26,function(x){
      ##Select gridcell
      print(x)
      gridcell <- grid
      gridcell[x,3] <- 1
      
      ##Get coordinate from gridcell
      gridsub <- subset(gridcell,value==1)
      coords <- gridsub[1:2]
      coordID <- paste0(coords[1],"_",coords[2])
      print(coordID)
      
      if(!file.exists(paste0(resultsPath, "/","Cell_", coordID,"_","_Dispersal1.RData",sep=""))){
      
      ##Extract species occuring within that cell
      species <- subset(SpM,x==coords$x & y==coords$y)
      species <- species[sapply(species, function(x) !any(is.na(x)))] 
      
      if(ncol(species)>2){
      
        species <- as.vector(colnames(species[3:ncol(species)]))
      
          ##Get the distance for each species
          dist <- lapply(species,function(d){
            OneSp <- subset(distances,Species==d)
            SpDist <- OneSp[c("Distance1")] ## <----- Change to right distance
            colnames(SpDist) <- "Distances"
            return(SpDist)
          })
      
        AllDist <- do.call(rbind,dist)
        BufferDist <- median(AllDist$Distances)
      
        ##Rasterize LatLon grid and clip out cells in buffer
        gridrast <- rasterFromXYZ(gridcell,crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
        rasterOptions(todisk=TRUE) ## Add because rater version on cluster has a bug
        cellB <- raster::buffer(gridrast, width=BufferDist)
        rasterOptions(todisk=FALSE)
        coord <- round(coordinates(cellB),4)
        values <- raster::getValues(cellB)
        AllCells <- as.data.frame(cbind(coord,values))
        AllCells <- subset(AllCells,values==1)
      
        ##Clip back to terrestrial cells only
        AllCells <- merge(AllCells,grid,by=c("x","y"))
        AllSp <- merge(AllCells[1:2],SpM,by=c("x","y"))
        AllSp <- AllSp[3:ncol(AllSp)]
        AllSp <- AllSp[,colSums(AllSp,na.rm=TRUE) > 0] 
        AllSp <- colnames(AllSp[1:ncol(AllSp)])
      
        #   ##Loop through species matrix and list all species that occur within the buffer
        #   SpList <- lapply(c(1:nrow(AllCells)),function(c){
        #     #SpList <- lapply(c(1:50),function(c){
        #     print(c)
        #     coords <- AllCells[c,1:2]
        #     species <- subset(SpM,x==coords$x & y==coords$y)
        #     species <- species[sapply(species, function(x) !any(is.na(x)))] 
        #     species <- as.vector(colnames(species[3:ncol(species)]))
        #     species <- as.data.frame(species)
        #     print(nrow(species))
        #     return(species)
        #   })
        # 
        # AllSpecies <- unique(do.call(rbind,SpList))
        print(length(AllSp))
        save(AllSp, file=paste0(resultsPath, "/","Cell_", coordID,"_","_Dispersal1.RData",sep=""), compress="xz") ## <----- Change to right name
        }else{
        print("0 SR")  
        AllSp <- NA
        save(AllSp, file=paste0(resultsPath, "/","Cell_", coordID,"_","_Dispersal1.RData",sep=""), compress="xz") 
        }
      }else{
      print("done")
      }
})

sfStop()

test <- get(load(paste0(resultsPath,"/","Cell_-179.75_71.25__Dispersal2.Rdata")))
test

