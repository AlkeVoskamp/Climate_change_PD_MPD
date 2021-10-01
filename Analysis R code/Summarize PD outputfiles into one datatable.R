#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#             Summarize the PD data files             #
#     Separate for each RCP and dispersal scenario    #
#                     June 2019                       #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

rm(list=ls())

setwd("/home/avoskamp/PD_analysis/PD_change_dispersal1_rcp26_2080_RC/")

All.files <- list.files()

data <- lapply(All.files,function(x){
  print(x)
  onefile <- get(load(x))
  if(ncol(onefile)== 38){
    return(onefile)}
})

AllData <- do.call(rbind,data)
nrow(AllData)
setwd("/home/avoskamp/PD_analysis/")
write.csv(AllData,"PDandMPD_random_changes_disp1_rcp26_2080_RC.csv")

#-#-# Add additional columns to dataframe #-#-#
setwd("E:/PD analysis/Second analysis non random loss win/Result files random PD change/")
DataPD <- read.csv("PDandMPD_random_changes_disp1_rcp60_2080_RC.csv")
head(DataPD)
nrow(DataPD)

## PD vs random PDchange
DataPD$PDInvsRIn <- DataPD$FaithACIn - DataPD$FaithInRmean
DataPD$PDOutvsROut <- DataPD$FaithACOut - DataPD$FaithOutRmean # Careful negative values!!

## MPD vs random PDchange
DataPD$MPDInvsRIn <- DataPD$MPDACIn - DataPD$MPDInRmean
DataPD$MPDOutvsROut <- DataPD$MPDACOut - DataPD$MPDOutRmean

## Significance PD vs random PD
## In
DataPD$PDInvsRIn_Pvalue_class <- DataPD$FaithInPvalue
DataPD$PDInvsRIn_Pvalue_class[DataPD$PDInvsRIn_Pvalue_class >= 0.95] <- "A" #rot ## Use Abc rather than numbers otherwise they might accidentally change
DataPD$PDInvsRIn_Pvalue_class[DataPD$PDInvsRIn_Pvalue_class < 0.95 & DataPD$PDInvsRIn_Pvalue_class >= 0.8] <- "B"
DataPD$PDInvsRIn_Pvalue_class[DataPD$PDInvsRIn_Pvalue_class <= 0.2 & DataPD$PDInvsRIn_Pvalue_class > 0.05] <- "C"
DataPD$PDInvsRIn_Pvalue_class[DataPD$PDInvsRIn_Pvalue_class > 0.2 & DataPD$PDInvsRIn_Pvalue_class < 0.8] <- NA
DataPD$PDInvsRIn_Pvalue_class[DataPD$PDInvsRIn_Pvalue_class <= 0.05] <- "D" #blau
DataPD$PDInvsRIn_Pvalue_class[DataPD$PDInvsRIn_Pvalue_class == "A"] <- 1
DataPD$PDInvsRIn_Pvalue_class[DataPD$PDInvsRIn_Pvalue_class == "B"] <- 2
DataPD$PDInvsRIn_Pvalue_class[DataPD$PDInvsRIn_Pvalue_class == "C"] <- 3
DataPD$PDInvsRIn_Pvalue_class[DataPD$PDInvsRIn_Pvalue_class == "D"] <- 4

## Out - be careful scale is opposite due to p-value calculation based on negative values!!
DataPD$PDOutvsROut_Pvalue_class <- DataPD$FaithOutPvalue
DataPD$PDOutvsROut_Pvalue_class[DataPD$PDOutvsROut_Pvalue_class >= 0.95] <- "D"
DataPD$PDOutvsROut_Pvalue_class[DataPD$PDOutvsROut_Pvalue_class < 0.95 & DataPD$PDOutvsROut_Pvalue_class >= 0.8] <- "C"
DataPD$PDOutvsROut_Pvalue_class[DataPD$PDOutvsROut_Pvalue_class <= 0.2 & DataPD$PDOutvsROut_Pvalue_class > 0.05] <- "B"
DataPD$PDOutvsROut_Pvalue_class[DataPD$PDOutvsROut_Pvalue_class > 0.2 & DataPD$PDOutvsROut_Pvalue_class < 0.8] <- NA
DataPD$PDOutvsROut_Pvalue_class[DataPD$PDOutvsROut_Pvalue_class <= 0.05] <- "A"
DataPD$PDOutvsROut_Pvalue_class[DataPD$PDOutvsROut_Pvalue_class == "A"] <- 1
DataPD$PDOutvsROut_Pvalue_class[DataPD$PDOutvsROut_Pvalue_class == "B"] <- 2
DataPD$PDOutvsROut_Pvalue_class[DataPD$PDOutvsROut_Pvalue_class == "C"] <- 3
DataPD$PDOutvsROut_Pvalue_class[DataPD$PDOutvsROut_Pvalue_class == "D"] <- 4

## Significance MPD vs random MPD
## In
DataPD$MPDInvsRIn_Pvalue_class <- DataPD$MPDInPvalue
DataPD$MPDInvsRIn_Pvalue_class[DataPD$MPDInvsRIn_Pvalue_class >= 0.95] <- "A"
DataPD$MPDInvsRIn_Pvalue_class[DataPD$MPDInvsRIn_Pvalue_class < 0.95 & DataPD$MPDInvsRIn_Pvalue_class >= 0.8] <- "B"
DataPD$MPDInvsRIn_Pvalue_class[DataPD$MPDInvsRIn_Pvalue_class <= 0.2 & DataPD$MPDInvsRIn_Pvalue_class > 0.05] <- "C"
DataPD$MPDInvsRIn_Pvalue_class[DataPD$MPDInvsRIn_Pvalue_class > 0.2 & DataPD$MPDInvsRIn_Pvalue_class < 0.8] <- NA
DataPD$MPDInvsRIn_Pvalue_class[DataPD$MPDInvsRIn_Pvalue_class <= 0.05] <- "D"
DataPD$MPDInvsRIn_Pvalue_class[DataPD$MPDInvsRIn_Pvalue_class == "A"] <- 1
DataPD$MPDInvsRIn_Pvalue_class[DataPD$MPDInvsRIn_Pvalue_class == "B"] <- 2
DataPD$MPDInvsRIn_Pvalue_class[DataPD$MPDInvsRIn_Pvalue_class == "C"] <- 3
DataPD$MPDInvsRIn_Pvalue_class[DataPD$MPDInvsRIn_Pvalue_class == "D"] <- 4

## Out - be careful scale is opposite due to p-value calculation based on negative values!!
DataPD$MPDOutvsROut_Pvalue_class <- DataPD$MPDOutPvalue
DataPD$MPDOutvsROut_Pvalue_class[DataPD$MPDOutvsROut_Pvalue_class >= 0.95] <- "D"
DataPD$MPDOutvsROut_Pvalue_class[DataPD$MPDOutvsROut_Pvalue_class < 0.95 & DataPD$MPDOutvsROut_Pvalue_class >= 0.8] <- "C"
DataPD$MPDOutvsROut_Pvalue_class[DataPD$MPDOutvsROut_Pvalue_class <= 0.2 & DataPD$MPDOutvsROut_Pvalue_class > 0.05] <- "B"
DataPD$MPDOutvsROut_Pvalue_class[DataPD$MPDOutvsROut_Pvalue_class > 0.2 & DataPD$MPDOutvsROut_Pvalue_class < 0.8] <- NA
DataPD$MPDOutvsROut_Pvalue_class[DataPD$MPDOutvsROut_Pvalue_class <= 0.05] <- "A"
DataPD$MPDOutvsROut_Pvalue_class[DataPD$MPDOutvsROut_Pvalue_class == "A"] <- 1
DataPD$MPDOutvsROut_Pvalue_class[DataPD$MPDOutvsROut_Pvalue_class == "B"] <- 2
DataPD$MPDOutvsROut_Pvalue_class[DataPD$MPDOutvsROut_Pvalue_class == "C"] <- 3
DataPD$MPDOutvsROut_Pvalue_class[DataPD$MPDOutvsROut_Pvalue_class == "D"] <- 4

## Save dataframe
write.csv(DataPD,"PDandMPD_random_changes_disp1_rcp60_2080_RC_Final.csv")






#-#-# Summarzie the files and add Pvalue for those files where they were missing #-#-#
setwd("/home/avoskamp/PDanalysis/PD_change_dispersal2_rcp60_2080_pvalue/")

All.files <- list.files()

test <- get(load(All.files[1]))

data <- lapply(All.files,function(x){
  print(x)
  onefile <- get(load(x))
  #if(ncol(onefile)== 42){
    return(onefile)#}
})

AllData <- do.call(rbind,data)
nrow(AllData)
setwd("/home/avoskamp/PDanalysis/")
write.csv(AllData,"PDandMPD_random_changes_disp2_rcp26_2080_Pvalues.csv")

#-#-# Read in PD data and P value #-#-#
setwd("E:/PD analysis/Second analysis non random loss win/Result files random PD change/")
PDdata <- read.csv("PDandMPD_random_changes_disp2_rcp60_2080_new.csv")
head(PDdata)
nrow(PDdata)
PDdata <- PDdata[!duplicated(PDdata),]

levelplot(Faith~x*y,data=PDdata)

Pvalue <- read.csv("PDandMPD_random_changes_disp2_rcp60_2080_Pvalues.csv")
head(Pvalue)
nrow(Pvalue)
Pvalue <- Pvalue[!duplicated(Pvalue),]

levelplot(FaithInPvalue~x*y,data=Pvalue)

final <- merge(PDdata,Pvalue,by=c("x","y"))
head(final)

levelplot(Faith~x*y,data=final)
levelplot(FaithOutPvalue.y~x*y,data=final)

write.csv(final,"PDandMPD_random_changes_disp2_rcp26_2080_Final.csv")
