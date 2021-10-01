#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#               Calculate PD and MPD per grid cell                  #
#                 Run null model for PD and MPD                     #
#        PD and MPD based on random species lost and gained         #
#                         Alke April 2019                           #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Clear the memory #-#-#
rm(list=ls())


#-#-# Load libraries #-#-#
library(ape)
library(picante)
library(snowfall)


#-#-# Set file directories #-#-# <---------------CHANGE HERE!!
treepath <- "/home/avoskamp/PD_analysis/PD analysis data/"
matrixpath <- "/home/avoskamp/PD_analysis/PD analysis data/" #"/home/avoskamp/Joe_Data/"
nullpath <- "/home/avoskamp/PD_analysis/Species_pool_null_model_dispersal1/"
outpath <- "/home/avoskamp/PD_analysis/PD_change_dispersal1_rcp60_2080/"


#-#-# Details scenario #-#-# <------------------ CHANGE HERE!!
dispersal <- "Dispersal1"
rcp <- "rcp60"
year <- "2080"


#-#-# Get the consensus tree #-#-#
## Read in tree
tr <- read.nexus(paste0(treepath,"ConsensusTree150_05credibility.tree"))
str(tr)
head(tr)
class(tr)            

## Remove negative edge.length
tr$edge.length[tr$edge.length<0]<-0


#-#-# Get the species matrix #-#-#
SpM <- get(load(paste0(matrixpath,"Baseline_distributions_lowdispersal.RData"))) #<---------------------CHANGE HERE!!
ncol(SpM)
head(SpM[1:5])
SpM[is.na(SpM)] <- 0

SpMFut <- get(load(paste0(matrixpath,"Future_distributions_2080_rcp26_lowdispersal.RData"))) #<---------CHANGE HERE!!
ncol(SpMFut)
head(SpMFut[1:5])
SpMFut[is.na(SpMFut)] <- 0

## Double check if the two matrixes are matching
Current <- colnames(SpM)
Future <- colnames(SpMFut)
length(intersect(Current,Future))

## Set number of grid cells and random iterations for lapply
numbers <- c(1:nrow(SpM)) # Run in batches to keep speed up
numbersII <- c(1:1000)


#-#-# Select missing files when restarting analysis (redistribute files accross cores) #-#-#
## Check beforehand which files are missing for more efficient disrtibution across cores and return index numbers
MissingCom <- lapply(numbers,function(n){
  print(n)

  ## Subset community file
  oneCom <- SpM[n,]
  x <- oneCom[1,1]
  y <- oneCom[1,2]

  ## Community file name
  comName <- paste0(outpath,"Cell_",x,"_",y,"_PD_Change_",dispersal,"_",rcp,"_",year,".Rdata",sep="")
  if(!file.exists(comName)){return(n)}
})

numbers <- do.call(rbind,MissingCom)


#-#-# Calculate PD/MPD and random change #-#-#
source("/home/avoskamp/PD_analysis/Rcode/RandomPDfunctionRelativeChange.R") #Link to random PD function

## Set up snowfall to run parallel
sfInit(parallel=TRUE, cpus=ceiling(0.3*parallel::detectCores())) ## Huge species matrixes - careful not to use too many cores
sfLibrary(ape);sfLibrary(picante)
sfExport(list=c("CalcPD","dispersal","rcp","year","numbers","numbersII","treepath","matrixpath","nullpath","outpath","tr","SpM","SpMFut")) ## Takes forever!!

## Calculate values using the RandomPD function
RandomPD <- sfLapply(numbers,function(n){
  print(n)
  CalcPD(x=n,
         treepath=treepath,
         matrixpath=matrixpath,
         nullpath=nullpath,
         outpath=outpath,
         tr=tr,
         SpM=SpM,
         SpMFut=SpMFut,
         numbers=numbers,
         numbersII=numbersII,
         dispersal=dispersal,
         rcp=rcp,
         year=year)
})

sfStop()  


#-#-# Check the output data #-#-#
setwd("/home/avoskamp/PD_analysis/PD_change_dispersal1_rcp60_2080_RC/")
allFiles <- list.files()

getData <- lapply(allFiles,function(x){
  data <- get(load(x))
  return(data)
})

AllData <- do.call(rbind,getData)
head(AllData)


