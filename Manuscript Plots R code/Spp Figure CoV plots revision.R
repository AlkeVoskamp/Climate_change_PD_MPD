#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#    Extract the CoV around the species richness predictions    #
#          Based on 2 SDMs and 4 GCMs - Future SR 2080          #
#                        Alke April 2022                        #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#-#-# Clear memory #-#-#
rm(list=ls())


#-#-# Load libraries #-#-#
library(ggplot2)
library(lattice)
library(rasterVis)
library(maps)
library(grid)
library(gridExtra)
library(rgdal)
library(Rfast)


#-#-# Extract SR based on GAM #-#-#
GAM_WD <- "/Volumes/Untitled/PD analysis/Range projections global/Bird_GAM_results_climate/"
All_GAM <- list.files(GAM_WD)

#-#-# Create base DF to merge species files to #-#-# 
Coords <- read.csv(paste0("/Volumes/Untitled/PD analysis/","Realm_coordinates_Lat_Lon.csv"))[2:3]
Coords$ID <- c(1:nrow(Coords))
head(Coords)

SR_DF_GFDL <- Coords 
SR_DF_Had <- Coords
SR_DF_IPSL <- Coords
SR_DF_Miro <- Coords


#-#-# GAM 4GCMs Dispersal main manuscript (d/2) #-#-#
#-#-# Extract SR based on GAM #-#-#
GAM_Var_GFDL <- for(i in 1:length(All_GAM)){
  print(i)
  data <- read.csv(paste0(GAM_WD,All_GAM[i]))
  data <- subset(data,dispersal2 == 1)
  data <- data[c("x","y","GFDL.ESM2M_rcp60_2080")]
  data <- merge(Coords,data,all.x=T,by=c("x","y"))
  data <- data[order(data$ID),]
  data <- data[4]
  SR_DF_GFDL <- cbind(SR_DF_GFDL,data)
}

SR_DF_GFDL[is.na(SR_DF_GFDL)] <- 0
SR_DF_GFDL$SR <- rowSums(SR_DF_GFDL[c(4:ncol(SR_DF_GFDL))])


GAM_Var_Had <- for(i in 1:length(All_GAM)){
  print(i)
  data <- read.csv(paste0(GAM_WD,All_GAM[i]))
  data <- subset(data,dispersal2 == 1)
  data <- data[c("x","y","HadGEM2.ES_rcp60_2080")]
  data <- merge(Coords,data,all.x=T)
  data <- data[order(data$ID),]
  data <- data[4]
  SR_DF_Had <- cbind(SR_DF_Had,data)
}

SR_DF_Had[is.na(SR_DF_Had)] <- 0
SR_DF_Had$SR <- rowSums(SR_DF_Had[c(4:ncol(SR_DF_Had))])


GAM_Var_IPSL <- for(i in 1:length(All_GAM)){
  print(i)
  data <- read.csv(paste0(GAM_WD,All_GAM[i]))
  data <- subset(data,dispersal2 == 1)
  data <- data[c("x","y","IPSL.CM5A.LR_rcp60_2080")]
  data <- merge(Coords,data,all.x=T)
  data <- data[order(data$ID),]
  data <- data[4]
  SR_DF_IPSL <- cbind(SR_DF_IPSL,data)
}

SR_DF_IPSL[is.na(SR_DF_IPSL)] <- 0
SR_DF_IPSL$SR <- rowSums(SR_DF_IPSL[c(4:ncol(SR_DF_IPSL))])


GAM_Var_Miro <- for(i in 1:length(All_GAM)){
  print(i)
  data <- read.csv(paste0(GAM_WD,All_GAM[i]))
  data <- subset(data,dispersal2 == 1)
  data <- data[c("x","y","MIROC5_rcp60_2080")]
  data <- merge(Coords,data,all.x=T)
  data <- data[order(data$ID),]
  data <- data[4]
  SR_DF_Miro <- cbind(SR_DF_Miro,data)
}

SR_DF_Miro[is.na(SR_DF_Miro)] <- 0
SR_DF_Miro$SR <- rowSums(SR_DF_Miro[c(4:ncol(SR_DF_Miro))])


GAM_SR <- cbind(SR_DF_GFDL$SR,SR_DF_Had$SR,SR_DF_IPSL$SR,SR_DF_Miro$SR)
head(GAM_SR)

#-#-# Extract SR based on GBM #-#-#
GBM_WD <- "/Users/alkevoskamp/Dropbox (Senckenberg)/AG BGFM's shared workspace/Voskamp/PD manuscript/PD analysis/Range projections global/Bird_GBM_results_climate/"
All_GBM <- list.files(GBM_WD)

#Create base DF
Coords <- read.csv(paste0("/Volumes/Untitled/PD analysis/","Realm_coordinates_Lat_Lon.csv"))[2:3]
Coords$ID <- c(1:nrow(Coords))
head(Coords)

SR_DF_GFDL_GBM <- Coords 
SR_DF_Had_GBM <- Coords
SR_DF_IPSL_GBM <- Coords
SR_DF_Miro_GBM <- Coords

#-#-# GAM 4GCMs Dispersal main manuscript (d/2) #-#-#

GBM_Var_GFDL <- for(i in 1:length(All_GBM)){
  print(i)
  data <- read.csv(paste0(GBM_WD,All_GBM[i]))
  data <- subset(data,dispersal2 == 1)
  data <- data[c("x","y","GFDL.ESM2M_rcp60_2080")]
  data <- merge(Coords,data,all.x=T)
  data <- data[order(data$ID),]
  data <- data[4]
  SR_DF_GFDL_GBM <- cbind(SR_DF_GFDL_GBM,data)
}

SR_DF_GFDL_GBM[is.na(SR_DF_GFDL_GBM)] <- 0
SR_DF_GFDL_GBM$SR <- rowSums(SR_DF_GFDL_GBM[c(4:ncol(SR_DF_GFDL_GBM))])


GBM_Var_Had <- for(i in 1:length(All_GBM)){
  print(i)
  data <- read.csv(paste0(GBM_WD,All_GBM[i]))
  data <- subset(data,dispersal2 == 1)
  data <- data[c("x","y","HadGEM2.ES_rcp60_2080")]
  data <- merge(Coords,data,all.x=T)
  data <- data[order(data$ID),]
  data <- data[4]
  SR_DF_Had_GBM <- cbind(SR_DF_Had_GBM,data)
}

SR_DF_Had_GBM[is.na(SR_DF_Had_GBM)] <- 0
SR_DF_Had_GBM$SR <- rowSums(SR_DF_Had_GBM[c(4:ncol(SR_DF_Had_GBM))])


GBM_Var_IPSL <- for(i in 1:length(All_GBM)){
  print(i)
  data <- read.csv(paste0(GBM_WD,All_GBM[i]))
  data <- subset(data,dispersal2 == 1)
  data <- data[c("x","y","IPSL.CM5A.LR_rcp60_2080")]
  data <- merge(Coords,data,all.x=T)
  data <- data[order(data$ID),]
  data <- data[4]
  SR_DF_IPSL_GBM <- cbind(SR_DF_IPSL_GBM,data)
}

SR_DF_IPSL_GBM[is.na(SR_DF_IPSL_GBM)] <- 0
SR_DF_IPSL_GBM$SR <- rowSums(SR_DF_IPSL_GBM[c(4:ncol(SR_DF_IPSL_GBM))])


GBM_Var_Miro <- for(i in 1:length(All_GBM)){
  print(i)
  data <- read.csv(paste0(GBM_WD,All_GBM[i]))
  data <- subset(data,dispersal2 == 1)
  data <- data[c("x","y","MIROC5_rcp60_2080")]
  data <- merge(Coords,data,all.x=T)
  data <- data[order(data$ID),]
  data <- data[4]
  SR_DF_Miro_GBM <- cbind(SR_DF_Miro_GBM,data)
}

SR_DF_Miro_GBM[is.na(SR_DF_Miro_GBM)] <- 0
SR_DF_Miro_GBM$SR <- rowSums(SR_DF_Miro_GBM[c(4:ncol(SR_DF_Miro_GBM))])


GBM_SR <- cbind(SR_DF_GFDL_GBM$SR,SR_DF_Had_GBM$SR,SR_DF_IPSL_GBM$SR,SR_DF_Miro_GBM$SR)
head(GBM_SR)

SR_var_disp <- cbind(Coords,GAM_SR,GBM_SR)
SR <- rowMeans(SR_var_disp[c(4:11)])
SR_var_disp$SR <- SR
colnames(SR_var_disp) <- c("x","y","ID","SR_GFDL_GAM","SR_Had_GAM","SR_IPSL_GAM","SR_Miro_GAM","SR_GFDL_GBM","SR_Had_GBM","SR_IPSL_GBM","SR_Miro_GBM","Mean_SR")
head(SR_var_disp)


setwd("/Users/alkevoskamp/Dropbox (Senckenberg)/AG BGFM's shared workspace/Voskamp/PD manuscript/PD analysis/Uncertainty_analysis/")
write.csv(SR_var_disp,"Uncertainty_SR_Disp2.csv")

#-#-# Plot the uncertainty #-#-#
## Red in the uncertainty data frames
Disp1 <- read.csv("Uncertainty_SR_Disp1.csv")#[c(2:11)]
head(Disp1)

Disp2 <- read.csv("Uncertainty_SR_Disp2.csv")#[c(2:11)]
head(Disp2)

#-# Calculate CoV dispersal1
## Coordinates
xy <- Disp1[c(1,2)]
head(xy)
## Data
data <- as.matrix(Disp1[c(5:12)])
## Calculate CoV
CoV <- rowcvs(data, ln = FALSE, unbiased = FALSE) 
## Final DF
Disp1CoV <- cbind(Disp1,CoV)
Disp1CoV[is.na(Disp1CoV)] <- 0
max(Disp1CoV$CoV)
Disp1CoV$CoV[Disp1CoV$CoV <= 0.1] <- NA
head(Disp1CoV)

#-# Calculate CoV dispersal1
## Coordinates
xy <- Disp2[c(1,2)]
head(xy)
## Data
data <- as.matrix(Disp2[c(5:12)])
## Calculate CoV
CoV <- rowcvs(data, ln = FALSE, unbiased = FALSE) 
## Final DF
Disp2CoV <- cbind(Disp2,CoV)
Disp2CoV[is.na(Disp2CoV)] <- 0
max(Disp2CoV$CoV)
Disp2CoV$CoV[Disp2CoV$CoV <= 0.1] <- NA
head(Disp2CoV)


#-# Set colour scheme
#red <- colorRampPalette(c("mistyrose","mistyrose","red2","darkred"))(100) 
cols <- rev(brewer.pal(9, "Spectral"))

Disp1 <- ggplot(data=Disp1CoV, aes(y=y, x=x)) +
  geom_raster(aes(fill =  CoV), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(cols),limits=c(0,0.9), na.value="white")+ # Insert colour and set range
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))

print(Disp1)

Disp2 <- ggplot(data=Disp2CoV, aes(y=y, x=x)) +
  geom_raster(aes(fill =  CoV), stat = "identity", position = "identity")+
  scale_fill_gradientn("",colours=c(cols),limits=c(0,0.9), na.value="white")+ # Insert colour and set range
  borders("world",  xlim = c(-180, 180), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=180,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("")+ 
  theme(plot.title = element_text(size = 21,face="bold",hjust = 0))

print(Disp2)


setwd("/Users/alkevoskamp/Dropbox (Senckenberg)/AG BGFM's shared workspace/Voskamp/PD manuscript/Manuscript Figures/")
ggsave("Spp Figure 5 CoV dispersal1.tiff",Disp1,width=12, height=6, unit="in", dpi=600, bg="transparent")
ggsave("Spp Figure 5 CoV dispersal2.tiff",Disp1,width=12, height=6, unit="in", dpi=600, bg="transparent")

