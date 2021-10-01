CalcPD <- function(x,treepath,matrixpath,nullpath,outpath,tr,SpM,SpMFut,numbers,numbersII,dispersal,rcp,year){  
  
  print(x)
  
  ## Sort community 
  oneCom <- SpM[x,]
  x <- oneCom[1,1]
  y <- oneCom[1,2]
  
  if(file.exists(paste0(nullpath,"Cell_",x,"_",y,"__",dispersal,".RData"))){
    if(!file.exists(paste0(outpath,"Cell_",x,"_",y,"_PD_Change_",dispersal,"_",rcp,"_",year,sep=""))){
      if(!file.exists(paste0(outpath,"Cell_",x,"_",y,"_PD_Change_",dispersal,"_",rcp,"_",year,".Rdata",sep=""))){
      
      ComAll <- oneCom[3:length(oneCom)]
      Com <- ComAll[,colSums(ComAll) > 0]
      CurrentNames <- colnames(Com)
      
      Com <- as.data.frame(Com)
      
      if(length(Com)==0){
        
        data <- list(x=x,y=y,SRCur=0,SRFut=NA,SRChange=NA,SameSP=NA,GoingSP=NA,ComingInSP=NA,Faith=NA,MPD=NA,FaithFut=NA,MPDFut=NA,
                     FaithRM=NA,MPDRM=NA,FaithACOut=NA,MPDACOut=NA,FaithACIn=NA,MPDACIn=NA,FaithNMCur=NA,MPDNMCur=NA,FaithNMSDCur=NA,
                     MPDNMSDCur=NA,FaithNMFut=NA,MPDNMFut=NA,FaithNMSDFut=NA,MPDNMSDFut=NA,FaithOutRmean=NA,FaithOutRSD=NA,FaithOutPvalue=NA,
                     MPDOutRmean=NA,MPDOutRSD=NA,MPDOutPvalue=NA,FaithInRmean=NA,FaithInRSD=NA,FaithInPvalue=NA,MPDInRmean=NA,MPDInRSD=NA,
                     MPDInPvalue=NA)
        AllData <- as.data.frame(matrix(unlist(data),ncol=34,byrow=TRUE))
        colnames(AllData) <- c("x","y","SRCur","SRFut","SRChange","SameSP","GoingSP","ComingInSP","Faith","MPD","FaithFut","MPDFut",
                               "FaithRM","MPDRM","FaithACOut","MPDACOut","FaithACIn","MPDACIn","FaithNMCur","MPDNMCur","FaithNMSDCur","MPDNMSDCur",
                               "FaithNMFut","MPDNMFut","FaithNMSDFut","MPDNMSDFut","FaithOutRmean","FaithOutRSD","FaithOutPvalue","MPDOutRmean",
                               "MPDOutRSD","MPDOutPvalue","FaithInRmean","FaithInRSD","FaithInPvalue","MPDInRmean","MPDInRSD","MPDInPvalue")
        save(AllData, file=paste0(outpath,"Cell_",x,"_",y,"_PD_Change_",dispersal,"_",rcp,"_",year,".Rdata",sep=""), compress="xz")  
        
      }else{
        
        if(length(Com)==1){
          
          data <- list(x=x,y=y,SRCur=1,SRFut=NA,SRChange=NA,SameSP=NA,GoingSP=NA,ComingInSP=NA,Faith=NA,MPD=NA,FaithFut=NA,MPDFut=NA,
                       FaithRM=NA,MPDRM=NA,FaithACOut=NA,MPDACOut=NA,FaithACIn=NA,MPDACIn=NA,FaithNMCur=NA,MPDNMCur=NA,FaithNMSDCur=NA,
                       MPDNMSDCur=NA,FaithNMFut=NA,MPDNMFut=NA,FaithNMSDFut=NA,MPDNMSDFut=NA,FaithOutRmean=NA,FaithOutRSD=NA,FaithOutPvalue=NA,
                       MPDOutRmean=NA,MPDOutRSD=NA,MPDOutPvalue=NA,FaithInRmean=NA,FaithInRSD=NA,FaithInPvalue=NA,MPDInRmean=NA,MPDInRSD=NA,
                       MPDInPvalue=NA)
          AllData <- as.data.frame(matrix(unlist(data),ncol=34,byrow=TRUE))
          colnames(AllData) <- c("x","y","SRCur","SRFut","SRChange","SameSP","GoingSP","ComingInSP","Faith","MPD","FaithFut","MPDFut",
                                 "FaithRM","MPDRM","FaithACOut","MPDACOut","FaithACIn","MPDACIn","FaithNMCur","MPDNMCur","FaithNMSDCur","MPDNMSDCur",
                                 "FaithNMFut","MPDNMFut","FaithNMSDFut","MPDNMSDFut","FaithOutRmean","FaithOutRSD","FaithOutPvalue","MPDOutRmean",
                                 "MPDOutRSD","MPDOutPvalue","FaithInRmean","FaithInRSD","FaithInPvalue","MPDInRmean","MPDInRSD","MPDInPvalue")
          save(AllData, file=paste0(outpath,"Cell_",x,"_",y,"_PD_Change_",dispersal,"_",rcp,"_",year,".Rdata",sep=""), compress="xz")  
          
        }else{
          
          if(length(Com)>2){
            
            ## Get the future community for the same cell
            FutCom <- SpMFut[ which( SpMFut$x == x & SpMFut$y == y) , ]
            FutCom <- FutCom[3:ncol(FutCom)]
            FutCom <- FutCom[,colSums(FutCom) > 0]
            
            ## Compare current and future specieslist
            CurList <- colnames(Com) ## Current species in cell
            FutList <- colnames(FutCom) ## Future species in cell
            
            SRCur <- length(CurList) ## Current SR
            SRFut <- length(FutList) ## Future SR
            SRChange <- SRFut - SRCur  ## Change in SR
            
            SameSP <- length(intersect(CurList, FutList)) ## How many species are the same
            GoingSP <- SRCur - SameSP
            ComingInSP <- SRFut - SameSP
            
            
            #-#-# Calculate the current values #-#-#
            ## Prune the tree for the current species list 
            prunedTree <- prune.sample(samp=Com,phylo=tr)
            
            ## Faith PD current species community
            Faith <- pd(Com, prunedTree,include.root=FALSE)
            
            ## MPD current species community
            ComMatrix <- cophenetic(prunedTree) ## Pairwise distance matrix for MPD
            MPD <- mpd(Com,ComMatrix,abundance.weighted=FALSE)
            
            #-#-# Calculate the future values #-#-#
            if(length(FutCom)>2){
              ## Prune the tree for the future species list
              prunedTreeFut <- prune.sample(samp=FutCom,phylo=tr)
              
              ## Faith PD future species community
              FaithFut <- pd(FutCom, prunedTreeFut,include.root=FALSE)
              
              ## MPD future species community
              ComMatrixFut <- cophenetic(prunedTreeFut) ## Pairwise distance matrix for MPD
              MPDFut <- mpd(FutCom,ComMatrixFut,abundance.weighted=FALSE)
            }else{
              FaithFut <- NA
              MPDFut <- NA 
            }
            
            #-#-# Actual values projected PD MPD loss/increase #-#-#
            ##Create dataframes actual loss/increase
            # 1)
            ##Dataframe species that remain
            namesSame <- intersect(CurList, FutList)
            sameCom <- data.frame(matrix("", ncol = length(namesSame), nrow = 1))
            colnames(sameCom) <- namesSame
            sameCom[,1:length(sameCom)] <- 1
            
            # => 1a)
            ##Dataframe species that actually leave 
            namesACOut <- setdiff(CurList, FutList)
            ACoutCom <- data.frame(matrix("", ncol = length(namesACOut), nrow = 1))
            colnames(ACoutCom) <- namesACOut
            if(length(ACoutCom) > 1){ACoutCom[,1:length(ACoutCom)] <- 1}
            
            # => 1b)
            ##Dataframe species that actually come in 
            namesACIn <- setdiff(FutList, CurList)
            ACinCom <- data.frame(matrix("", ncol = length(namesACIn), nrow = 1))
            colnames(ACinCom) <- namesACIn
            if(length(ACinCom) > 1){ACinCom[,1:length(ACinCom)] <- 1}
            
            ##Calculate values actual loss/increase
            # => 1)
            ## PD and MPD remaining
            ## Prune the tree for the remaining species list 
            if(length(sameCom) > 2){
              prunedTreeRM <- prune.sample(samp=sameCom,phylo=tr)
              
              ## Faith PD current species community
              FaithRM <- pd(sameCom, prunedTreeRM,include.root=FALSE)
              
              ## MPD current species community
              ComMatrixRM <- cophenetic(prunedTreeRM) ## Pairwise distance matrix for MPD
              MPDRM <- mpd(sameCom,ComMatrixRM,abundance.weighted=FALSE) 
            }else{
              FaithRM <- NA
              MPDRM <- NA
            }
            
            # => 1a)
            if(length(ACoutCom) > 1){
              ## PD and MPD actually going out 
              ## Calculated as Remaining PD (species staying) - Overall current PD
              ## How much PD/MPD decrease is there in the overall community through species leaving
              
              FaithACOut <- FaithRM - Faith
              MPDACOut <- MPDRM - MPD
              
            }else{
              FaithACOut <- NA
              MPDACOut <- NA
            }
            
            # => 1b)
            if(length(ACinCom) > 1){
              ## PD and MPD gain through species coming in 
              ## Prune the tree for species for remaining and coming in species together
              ## Then calculate as (Remaining + Coming in species values) - Remaining species values
              
              RMandComeIn <- cbind(sameCom,ACinCom)
              prunedTreeACIn <- prune.sample(samp=RMandComeIn,phylo=tr)
              
              ## Faith PD remaining and coming in species 
              FaithACInSplit <- pd(RMandComeIn, prunedTreeACIn,include.root=FALSE)
              
              ## Faith PD gain through species coming in
              FaithACIn <- FaithACInSplit - FaithRM
              
              ## MPD remaining and coming in species 
              ComMatrixACIn <- cophenetic(prunedTreeACIn) ## Pairwise distance matrix for MPD
              MPDACInSplit  <- mpd(RMandComeIn,ComMatrixACIn,abundance.weighted=FALSE) 
              MPDACIn <- MPDACInSplit - MPDRM
              
            }else{
              FaithACIn <- NA
              MPDACIn <- NA
            }
            
            #-#-# Get the null species pool #-#-#
            SpPool <- get(load(paste0(nullpath,"Cell_",x,"_",y,"__",dispersal,".RData")))
            if(length(SpPool)>=SRFut){ 
              if(length(SpPool)>=SRCur){ 
              
              #-#-# NULLMODEL - Calculate PD and MPD for random current and future community #-#-#
              randomNM <- lapply(numbersII, function(r){
                print("null model")
                print(r)

                ## Random sample current
                namesNC <- sample(SpPool,SRCur)
                nullCom <- data.frame(matrix("", ncol = length(namesNC), nrow = 1))
                colnames(nullCom) <- namesNC
                nullCom[,1:length(nullCom)] <- 1
              
                ## Prune the tree for the null model list current
                prunedTreeNM <- prune.sample(samp=nullCom,phylo=tr)

                ## Faith PD random current species community
                FaithNM <- pd(nullCom, prunedTreeNM,include.root=FALSE)

                ## MPD random future species community
                ComMatrixNM <- cophenetic(prunedTreeNM) ## Pairwise distance matrix for MPD
                MPDNM <- mpd(nullCom,ComMatrixNM,abundance.weighted=FALSE)
                
                ## Random sample future
                namesNCFut <- sample(SpPool,SRFut)
                nullComFut <- data.frame(matrix("", ncol = length(namesNCFut), nrow = 1))
                colnames(nullComFut) <- namesNCFut
                nullComFut[,1:length(nullComFut)] <- 1

                ## Prune the tree for the null model list future
                prunedTreeNMFut <- prune.sample(samp=nullComFut,phylo=tr)

                ## Faith PD random future species community
                FaithNMFut <- pd(nullComFut, prunedTreeNMFut,include.root=FALSE)

                ## MPD random future species community
                ComMatrixNMFut <- cophenetic(prunedTreeNMFut) ## Pairwise distance matrix for MPD
                MPDNMFut <- mpd(nullComFut,ComMatrixNMFut,abundance.weighted=FALSE)

                Nulldata <- cbind(FaithNM[1],MPDNM)
                Nulldata <- cbind(Nulldata,FaithNMFut[1])
                Nulldata <- cbind(Nulldata,MPDNMFut)
                
                ## Summarize first part of loop to return later
                colnames(Nulldata) <- c("PDCur","MPDNMCur","PDFut","MPDNMFut")

              
              #-#-# RANDOM CHANGES - Calculate PD and MPD for random changes #-#-#
                print("random")
                
                #-#-# Random change dataframes #-#-#
                # 2)
                ##Dataframe species that go at random
                namesOut <- sample(CurrentNames,SameSP) ## Species pool here are the species currently in the cell + random draw which species remain
                outCom <- data.frame(matrix("", ncol = length(namesOut), nrow = 1))
                colnames(outCom) <- namesOut
                if(length(outCom) >= 1){outCom[,1:length(outCom)] <- 1}
                
                # 3)
                ##Dataframe species that come in at random 
                NullOut <- SpPool[!(SpPool %in% CurrentNames)] ## Reduce species pool to all species in the pool that are not currently in the cell
                namesIn <- sample(NullOut,ComingInSP)  ## Species that come in drawn from reduced null pool
                inComSplit <- data.frame(matrix("", ncol = length(namesIn), nrow = 1))
                colnames(inComSplit) <- namesIn
                if(length(inComSplit) >= 1){inComSplit[,1:length(inComSplit)] <- 1}
                inCom <- cbind(sameCom,inComSplit) ## Remaining species plus randomly added species
                
                #-#-# Random change calculations #-#-#
                # => 2)
                if(length(outCom) > 1){
                  ## PD and MPD actually going out 
                  ## Calculated as Remaining PD (species staying) - Overall current PD
                  ## How much PD/MPD decrease is there in the overall community through species leaving
                  prunedTreeOut <- prune.sample(samp=outCom,phylo=tr)
                  
                  ## Faith PD randomly chosen remaining species
                  FaithOutRSplit <- pd(outCom, prunedTreeOut,include.root=FALSE)
                  FaithOutR <- FaithOutRSplit - Faith
                  
                  ## MPD randomly chosen remaining species
                  ComMatrixOut <- cophenetic(prunedTreeOut) ## Pairwise distance matrix for MPD
                  MPDOutRSplit <- mpd(outCom,ComMatrixOut,abundance.weighted=FALSE) 
                  MPDOutR <- MPDOutRSplit - MPD
                  
                }else{
                  FaithOutR <- NA
                  MPDOutR <- NA
                }
                
                # => 3)
                if(length(inCom) > 1){
                  ## PD and MPD gain through species coming in 
                  ## Prune the tree for species for remaining and coming in species together
                  ## Then calculate as (Remaining + Coming in species values) - Remaining species values
                  prunedTreeIn <- prune.sample(samp=inCom,phylo=tr)
                  
                  ## Faith PD remaining and coming in species
                  FaithInRSplit <- pd(inCom, prunedTreeIn,include.root=FALSE)
                  
                  ## Faith PD gain through species coming in
                  FaithInR <- FaithInRSplit - FaithRM
                  
                  ## MPD current species community
                  ComMatrixIn <- cophenetic(prunedTreeIn) ## Pairwise distance matrix for MPD
                  MPDInRSplit <- mpd(inCom,ComMatrixIn,abundance.weighted=FALSE)
                  MPDInR <- MPDInRSplit - MPDRM
                  
                }else{
                  FaithInR <- NA
                  MPDInR <- NA
                }

                ## Summarize all data for the first part of the loop to return
                RandomData <- do.call(cbind,list(FaithOutR[1],MPDOutR,FaithInR[1],MPDInR))
                colnames(RandomData) <- c("FaithOutR","MPDOutR","FaithInR","MPDInR")
                AllRandomNullData <- cbind(Nulldata,RandomData)
                return(AllRandomNullData)
              })
              
              RandomData <- do.call(rbind,randomNM)
              
              
              #-#-# Summarize Nullmodel results for ses values #-#-#
              ## Current NM values
              FaithNMCur <- mean(RandomData$PDCur)
              MPDNMCur <- mean(RandomData$MPDNMCur)
              FaithNMSDCur <- sd(RandomData$PDCur)
              MPDNMSDCur <- sd(RandomData$MPDNMCur)
              
              ##Future NM values
              FaithNMFut <- mean(RandomData$PDFut)
              MPDNMFut <- mean(RandomData$MPDNMFut)
              FaithNMSDFut <- sd(RandomData$PDFut)
              MPDNMSDFut <- sd(RandomData$MPDNMFut)
              
              
              #-#-# Summarize random change data #-#-#
              ##Species leaving
              FaithOutRmean <- mean(RandomData$FaithOutR,na.rm=TRUE)
              FaithOutRSD <- sd(RandomData$FaithOutR,na.rm=TRUE)
              MPDOutRmean <- mean(RandomData$MPDOutR,na.rm=TRUE)
              MPDOutRSD <- sd(RandomData$MPDOutR,na.rm=TRUE)
              
              ##Species coming in 
              FaithInRmean <- mean(RandomData$FaithInR,na.rm=TRUE)
              FaithInRSD <- sd(RandomData$FaithInR,na.rm=TRUE)
              MPDInRmean <- mean(RandomData$MPDInR,na.rm=TRUE)
              MPDInRSD <- sd(RandomData$MPDInR,na.rm=TRUE)
              
              #-#-# Calculate significance values random change data #-#-#
              ##Species leaving
              RandomData$FaithOutSig <- RandomData$FaithOutR
              FaithOutSig <- length(RandomData$FaithOutSig[RandomData$FaithOutSig < as.numeric(FaithACOut[1])]) #Predicted loss > Random loss!! -> negative change
              FaithOutPvalue <- sum(FaithOutSig)/1000
              
              RandomData$MPDOutSig <- RandomData$MPDOutR
              MPDOutSig <- length(RandomData$MPDOutSig[RandomData$MPDOutSig < MPDACOut]) #Predicted loss > Random loss!! 
              MPDOutPvalue <- sum(MPDOutSig)/1000
              
              ##Species coming in
              RandomData$FaithInSig <- RandomData$FaithInR
              FaithInSig <- length(RandomData$FaithInSig[RandomData$FaithInSig < as.numeric(FaithACIn[1])])  #Predicted gain > Random gain!! -> positive change
              FaithInPvalue <- sum(FaithInSig)/1000
              
              RandomData$MPDInSig <- RandomData$MPDInR
              MPDInSig <- length(RandomData$MPDInSig[RandomData$MPDInSig < MPDACIn]) #Predicted gain > Random gain!! 
              MPDInPvalue <- sum(MPDInSig)/1000
              
              
              #-#-# Combine all data to return #-#-#
              AllData <- do.call(cbind,list(x,y,SRCur,SRFut,SRChange,SameSP,GoingSP,ComingInSP,Faith[1],MPD,FaithFut[1],MPDFut,FaithRM[1],MPDRM,
                                            FaithACOut[1],MPDACOut,FaithACIn[1],MPDACIn,FaithNMCur,MPDNMCur,FaithNMSDCur,MPDNMSDCur,FaithNMFut,
                                            MPDNMFut,FaithNMSDFut,MPDNMSDFut,FaithOutRmean,FaithOutRSD,FaithOutPvalue,MPDOutRmean,MPDOutRSD,MPDOutPvalue,
                                            FaithInRmean,FaithInRSD,FaithInPvalue,MPDInRmean,MPDInRSD,MPDInPvalue))
              
              colnames(AllData) <- c("x","y","SRCur","SRFut","SRChange","SameSP","GoingSP","ComingInSP","Faith","MPD","FaithFut","MPDFut","FaithRM","MPDRM",
                                     "FaithACOut","MPDACOut","FaithACIn","MPDACIn","FaithNMCur","MPDNMCur","FaithNMSDCur","MPDNMSDCur","FaithNMFut","MPDNMFut",
                                     "FaithNMSDFut","MPDNMSDFut","FaithOutRmean","FaithOutRSD","FaithOutPvalue","MPDOutRmean","MPDOutRSD","MPDOutPvalue","FaithInRmean",
                                     "FaithInRSD","FaithInPvalue","MPDInRmean","MPDInRSD","MPDInPvalue")
              
              save(AllData, file=paste0(outpath,"Cell_",x,"_",y,"_PD_Change_",dispersal,"_",rcp,"_",year,".Rdata",sep=""), compress="xz") 
              }else{print("SpPool error")}
            }else{print("SpPool error")}
          }}}
    }}}
}