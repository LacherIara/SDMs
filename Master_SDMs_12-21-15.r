
############################
#Purpose: Run species distribution models in Maxent
#Input: 
#Output: Modeled projections for species and clusters: "SP_mods_mu.gri", "SP_mods_yr.gri", "C_mods_mu.gri", "C_mods_yr.gri"
#Written by: Iara Lacher UCD/SCBI LacherI@si.edu
#Date: LAST EDITED: 12-21-15
#NOTES: 
############################

# ----------------------------------------------
# SET WORKING DIRECTORY
setwd("Y:/Lacher/VarInSDM") #Harvard CLUSTER

# ----------------------------------------------
# ----------------------------------------------
# PACKAGES NEEDED
# ----------------------------------------------
# ----------------------------------------------

# DATA PREPARATION AND SDMS
# General data manipulation
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(reshape)

# Raster editing and SDMs
library(raster)
library(dismo)
require(rJava)

# Raster editing and SDMs
library(raster)
library(maps)
library(maptools)
library(dismo)

# Calculate MCP area
# library(alphahull) #calculation and development of minimum convex polygon
library(rgdal)
library(ggplot2)


# ----------------------------------------------
# READ OUTPUT FILES:

# file name: SP_mods_mu / R label: SP_mu_s
SP_mu_files<-list.files(path="SpProjections/all_mods/", pattern="_mods_mu+.*gri")
SP_mu_files2<-paste("SpProjections/all_mods/", SP_mu_files, sep="")
SP_mu_s<-mapply(brick, SP_mu_files2)
SP_mu_s_names<-gsub(".gri", "", SP_mu_files)
names(SP_mu_s)<-SP_mu_s_names

# file name: SP_mods_yr / R label: SP_yr_s
SP_yr_s_files<-list.files(path="SpProjections/all_mods/", pattern="_mods_yr+.*gri")
SP_yr_s_files2<-paste("SpProjections/all_mods/", SP_yr_s_files, sep="")
SP_yr_s<-mapply(brick, SP_yr_s_files2)
SP_yr_s_names<-gsub(".gri", "", SP_yr_s_files)
names(SP_yr_s)<-SP_yr_s_names

# file name: C_mods_mu / R label: C_mu_s
C_mu_files<-list.files(path="SpProjections/ward_mods/", pattern="_mods_mu+.*gri")
C_mu_files2<-paste("SpProjections/ward_mods/", C_mu_files, sep="")
C_mu_s<-mapply(brick, C_mu_files2)
C_mu_s_names<-gsub(".gri", "", C_mu_files)
names(C_mu_s)<-C_mu_s_names

# file name: C_mods_yr / R label: C_yr_s
C_yr_s_files<-list.files(path="SpProjections/ward_mods/", pattern="_mods_yr+.*gri")
C_yr_s_files2<-paste("SpProjections/ward_mods/", C_yr_s_files, sep="")
C_yr_s<-mapply(brick, C_yr_s_files2)
C_yr_s_names<-gsub(".gri", "", C_yr_s_files)
names(C_yr_s)<-C_yr_s_names

# ----------------------------------------------
# FILES NEEDED:

{# file name: AllSpp / R label: AllSpp
AllSpp<-read.csv("AllSpp.csv")

ABVI<-subset(AllSpp, SpCode=="ABVI")  # Abronia villosa var. aurita
AMGR<-subset(AllSpp, SpCode=="AMGR")  # Amsinkia grandiflora
APOC<-subset(AllSpp, SpCode=="APOC")  #	Aphanes occidentalis
ATJO<-subset(AllSpp, SpCode=="ATJO")  # Atriplex joaquinana
BLPL<-subset(AllSpp, SpCode=="BLPL")  # Blepharizonia plumosa
CAMA<-subset(AllSpp, SpCode=="CAMA")  # California macrophylla
CALE<-subset(AllSpp, SpCode=="CALE")  # Caulanthus lemmonii
CHOC<-subset(AllSpp, SpCode=="CHOC")  # Chamaesyce ocellata ssp. ratanii
CIMA<-subset(AllSpp, SpCode=="CIMA")  # Cistanthe maritima
CLGR<-subset(AllSpp, SpCode=="CLGR")  # Clarkia gracilis ssp tracyi
CLMO<-subset(AllSpp, SpCode=="CLMO")  # Clarkia mosquinii
CRSO<-subset(AllSpp, SpCode=="CRSO")   # Crassula solieri
ERAN<-subset(AllSpp, SpCode=="ERAN")   # Eriogonum angulosum
ESCA<-subset(AllSpp, SpCode=="ESCA")   # Eschscholzia caespitosa
KACA<-subset(AllSpp, SpCode=="KACA")   # Kallstroemia californica
LELI<-subset(AllSpp, SpCode=="LELI")   # Legenere limosa
# LIVI<-subset(AllSpp, SpCode=="LIVI") # Limnanthes vinculans
LUST<-subset(AllSpp, SpCode=="LUST")   # Lupinus stiversii
MEGR<-subset(AllSpp, SpCode=="MEGR")   # Mentzelia gracilenta
MINU<-subset(AllSpp, SpCode=="MINU")   # Mimulus nudatus
MIPU<-subset(AllSpp, SpCode=="MIPU")   # Mimulus pulchellus
NASE<-subset(AllSpp, SpCode=="NASE")   # Navarretia setiloba
PAAH<-subset(AllSpp, SpCode=="PAAH")   # Paronychia ahartii
SCBO<-subset(AllSpp, SpCode=="SCBO")   # Scribneria bolanderi
 
#This loads only  columns 4 & 5
ABVI<-ABVI[,4:5] 
AMGR<-AMGR[,4:5]
APOC<-APOC[,4:5] 
ATJO<-ATJO[,4:5]  
BLPL<-BLPL[,4:5]
CALE<-CALE[,4:5] 
CAMA<-CAMA[,4:5] 
CHOC<-CHOC[,4:5] 
CIMA<-CIMA[,4:5] 
CLGR<-CLGR[,4:5]
CLMO<-CLMO[,4:5] 
CRSO<-CRSO[,4:5] 
ERAN<-ERAN[,4:5] 
ESCA<-ESCA[,4:5] 
KACA<-KACA[,4:5] 
LELI<-LELI[,4:5] 
# LIVI<-LIVI[,4:5] 
LUST<-LUST[,4:5] 
MEGR<-MEGR[,4:5] 
MINU<-MINU[,4:5]
MIPU<-MIPU[,4:5] 
NASE<-NASE[,4:5] 
PAAH<-PAAH[,4:5] 
SCBO<-SCBO[,4:5] 

# LIST DATAFRAMES WITH ONLY LON/LAT (SP)
SP<-list(ABVI,AMGR,APOC,ATJO,BLPL,CALE,CAMA,CHOC,CIMA,CLGR,CLMO,CRSO,ERAN,ESCA,KACA,LELI,LUST,MEGR,MINU,MIPU,NASE,PAAH,SCBO)

# LIST OF SPECIES NAMES (SP_names)
SP_names<-c('ABVI','AMGR','APOC','ATJO','BLPL','CALE','CAMA','CHOC','CIMA','CLGR','CLMO','CRSO','ERAN','ESCA','KACA','LELI','LUST','MEGR','MINU','MIPU','NASE','PAAH','SCBO')

names(SP)<-SP_names
}

# file name: biovar_mu / R label: biovar_mu
biovar_mu<-brick("biovar/biovar_mu", 19)

# file name: biovar_YY / R label: biovar_yr
biovar_yr_list<-list.files(path="biovar/", pattern="biovar_+.*gri")
biovar_yr_list<-biovar_yr_list[biovar_yr_list !="biovar_mu.gri"]
biovar_yr2<-paste("biovar/", biovar_yr_list, sep="")
biovar_yr<-mapply(brick, biovar_yr2)
biovar_yr_names<-gsub(".gri", "", biovar_yr_list)
names(biovar_yr)<-biovar_yr_names

# file name: SSSS_pnts / R label: pnts_SP , pnts_SP_LL
pnts_files<-list.files(path="pnts/", pattern="_pnts.txt")
pnts_files2<-paste("pnts/", pnts_files, sep="")
pnts_SP<-lapply(pnts_files2, read.table, header=TRUE, sep=",")
pnts_SP_names<-gsub(".txt", "", pnts_files)
names(pnts_SP)<-pnts_SP_names

pnts_SP_LL<-lapply(pnts_SP, subset, select=c(2:3))# need only lon lat. 

clus4SDM_files<-list.files(path="Calculations/ward_calcs/SPclus/", pattern="clus4SDM.txt")
clus4SDM_files2<-paste("Calculations/ward_calcs/SPclus/", clus4SDM_files, sep="")
clus4SDM<-lapply(clus4SDM_files2, read.table, header=TRUE, sep=",")
clus_names<-gsub(".txt", "", clus4SDM_files)
names(clus4SDM)<-clus_names

clus4SDM_LL<-lapply(clus4SDM, subset, select=c(5:6))# need only lon lat. 
clus_biochange<-lapply(clus4SDM, subset, select=c(7:25))# selects only the bioclim columns
pnts_clus<-lapply(clus4SDM, subset, select=c(4)) #pa 

# file name: SSSSSPEx_mu / R label: SPEx_mu
SPEx_mu_files<-list.files(path="extract/all_ex/", pattern="SPEx_mu.txt")
SPEx_mu_files2<-paste("extract/all_ex/", SPEx_mu_files, sep="")
SPEx_mu<-lapply(SPEx_mu_files2, read.table, header=TRUE, sep=",")
SPEx_mu_names<-gsub(".txt", "", SPEx_mu_files)
names(SPEx_mu)<-SPEx_mu_names

# file name: SSSSSPEx_yr / R label: SPEx_yr
SPEx_yrfiles<-list.files(path="extract/all_ex/", pattern="SPEx_yr+.*txt")
SPEx_yrfiles2<-paste("extract/all_ex/", SPEx_yrfiles, sep="")
SPEx_yr<-lapply(SPEx_yrfiles2, read.table, header=TRUE, sep=",")
SPEx_yr_names<-gsub(".txt", "", SPEx_yrfiles)
names(SPEx_yr)<-SPEx_yr_names

# file name: CEx_yrSSSS.(C)clus4SDM## / R label: CEx_yr
CEx_yrfiles<-list.files(path="extract/ward_ex/", pattern="CEx_yr+.*txt")
CEx_yrfiles2<-paste("extract/ward_ex/", CEx_yrfiles, sep="")
CEx_yr<-lapply(CEx_yrfiles2, read.table, header=TRUE, sep=",")
CEx_yr_names<-gsub(".txt", "", CEx_yrfiles)
names(CEx_yr)<-CEx_yr_names

# remove ID column
SPEx_mu<-lapply(SPEx_mu, function(x) { x["ID"] <- NULL; x }) # remove ID column

# UNLIST SPEx_yr
SPEx_yr <- unlist(SPEx_yr,recursive=FALSE) 

# UNLIST CEx_yr
CEx_yr <- unlist(CEx_yr,recursive=FALSE) 


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

#X X X X X X X X X X X X X X X X X X X X X X X#
#X X X X X X X X X X X X X X X X X X X X X X X#
#             SPECIES DIST MODELING
#X X X X X X X X X X X X X X X X X X X X X X X#
#X X X X X X X X X X X X X X X X X X X X X X X#

############################################################################################
################## FULL ### FULL ### FULL ##################################################
############################################################################################


##############################################
# MODEL POTENTIAL SUITABLE HABITAT 
##############################################

# ----------------------------------------------
# ----------------------------------------------
# 30 YEAR NORMALS - SDM: UN-CLUS AND CLUS
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# 30 YEAR NORMALS: SDM: UN-CLUS
# ----------------------------------------------
{

SP_xm_mu<-list()
b_t<-1
for(i in 1:length(SP)){
  print(paste(b_t, ':'))
  SP_xm_mu[[b_t]]<- maxent(data.frame(SPEx_mu[[i]]), p=pnts_SP[[i]][, c(1)])
  b_t<-b_t+1
}

# save.model(SP_xm_mu, "Sp_Maxent/SP_xm_mu")
# load.model("Sp_Maxent/SP_xm_mu") 

# ----------------------------------------------
#PREDICT ONTO 30YR NORMALS

SP_mods_mu<-list()
m_t<-1
for(i in 1:length(SP_xm_mu)){
  print(paste(m_t, ':'))
  SP_mods_mu[[m_t]]<-predict(SP_xm_mu[[i]], biovar_mu)
  m_t<-m_t+1
}


SP_mods_mu<-list()
m_t<-1
for(i in 1:length(SP_xm_mu)){
  print(paste(m_t, ':'))
  SP_mods_mu[[m_t]]<-predict(SP_xm_mu[[i]], biovar_mu)
  m_t<-m_t+1
}

names(SP_mods_mu)<-paste(SP_names,"_mods_mu", sep="")
SP_mu_s = stack(SP_mods_mu)
names(SP_mu_s)<-names(SP_mods_mu)

# WRITE RASTER FILES TO COMPUTER   
mapply(
  writeRaster,
  x=SP_mods_mu, filename=paste("SpProjections/all_mods/", names(SP_mods_mu), sep=""), format="raster", overwrite=TRUE)

# #READ FROM FILE
# SP_mu_files<-list.files(path="SpProjections/all_mods/", pattern="_mods_mu+.*gri")
# SP_mu_files2<-paste("SpProjections/all_mods/", SP_mu_files, sep="")
# SP_mu_s<-mapply(brick, SP_mu_files2)
# SP_mu_s_names<-gsub(".gri", "", SP_mu_files)
# names(SP_mu_s)<-SP_mu_s_names
  
}

# ----------------------------------------------
# 30 YEAR NORMALS: SDM: CLUS
# ----------------------------------------------
{

C_xm_mu<-list()
b_t<-1
for(i in 1:length(clus4SDM)){
  print(paste(b_t, ':'))
  C_xm_mu[[b_t]]<- maxent(data.frame(clus_biochange[[i]]), p=pnts_clus[[i]])
  b_t<-b_t+1
}
# save.model(C_xmSP_bio, "Sp_Maxent/ward_xm/C_xm_mu")
# load.model("Sp_Maxent/ward_xm/C_xm_mu") 

# ----------------------------------------------
#PREDICT ONTO 30YR NORMALS

C_mods_mu<-list()
m_t<-1
for(i in 1:length(C_xm_mu)){
  print(paste(m_t, ':'))
  C_mods_mu[[m_t]]<-predict(C_xm_mu[[i]], biovar_mu)
  m_t<-m_t+1
}

names(C_mods_mu)<-paste(clus_names,"_mods_mu", sep="")
C_mu_s = stack(C_mods_mu)
names(C_mu_s)<-names(C_mods_mu)

# WRITE RASTER FILES TO COMPUTER   
mapply(
  writeRaster,
  x=C_mods_mu, filename=paste("SpProjections/ward_mods/", names(C_mods_mu), sep=""), format="raster", overwrite=TRUE)
  
# #READ FROM FILE *47 STACKS OF 1
# C_mu_files<-list.files(path="SpProjections/ward_mods/", pattern="_mods_mu+.*gri")
# C_mu_files2<-paste("SpProjections/ward_mods/", C_mu_files, sep="")
# C_mu_s<-mapply(brick, C_mu_files2)
# C_mu_s_names<-gsub(".gri", "", C_mu_files)
# names(C_mu_s)<-C_mu_s_names

}


# ----------------------------------------------
# ----------------------------------------------
# YEARLY - SDM: UN-CLUS AND CLUS
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# YEARLY SDM: UN-CLUS 
# ----------------------------------------------
{

# Select only the necessary columns
pnts_SPpa<-lapply(pnts_SP, subset, select=c(pa)) #list of 23
pnts_SPpa2<-lapply(pnts_SPpa, rep, times=30)#create list of pnts_clus with same number of replicates as there are years.
pnts_SPpa2u <- unlist(pnts_SPpa2,recursive=FALSE) #list of 690
  

SP_xm_yr<-list() 
b_t<-1
for(i in 1:length(pnts_SPpa2u)){
  print(paste(b_t, ':'))
  SP_xm_yr[[b_t]]<- maxent(SPEx_yr[[i]], p=pnts_SPpa2u[[i]])
  b_t<-b_t+1
}

# ----------------------------------------------
#PREDICT ONTO 30YR NORMALS****don't want a stack of 690. want a stack per year. 

SP_mods_yr<-list()
m_t<-1
for(i in 1:length(SP_xm_yr)){
  print(paste(m_t, ':'))
  SP_mods_yr[[m_t]]<-predict(SP_xm_yr[[i]], biovar_mu)
  m_t<-m_t+1
}

names(SP_mods_yr)<-paste(gsub("SPEx_yr", "", names(SPEx_yr)),"_mods_yr", sep="")

# STACK BY SPECIES (OR IN THE CASE OF CLUSTERS, BY CLUSTER)##??????
sp<-substring(names(SP_mods_yr), first=1, last = 4)#is number of characters correct?
vSP_yr<- split(SP_mods_yr, sp)
SP_yr_s<-mapply(stack, vSP_yr)
names(SP_yr_s)<-names(vSP_yr)
 
# WRITE TO FILE
mapply(
  writeRaster,
  x=SP_mods_yr, filename=paste("SpProjections/all_mods/", names(SP_mods_yr), sep=""), format="raster", overwrite=TRUE)  # this gives an error, but I have no idea why bc the files write correctly. Should get 94 files total. 
  

# # 30 YEAR (Each of yr years) *should be 23 ***stacks of 30***
# SP_yr_s_files<-list.files(path="SpProjections/all_mods/", pattern="_mods_yr+.*gri")
# SP_yr_s_files2<-paste("SpProjections/all_mods/", SP_yr_s_files, sep="")
# SP_yr_s<-mapply(brick, SP_yr_s_files2)
# SP_yr_s_names<-gsub(".gri", "", SP_yr_s_files)
# names(SP_yr_s)<-SP_yr_s_names
}

# ----------------------------------------------
# YEARLY SDM: CLUS
# ----------------------------------------------
{

# SELECT ONLY THE NECESSARY COLUMNS
pnts_Cpa<-lapply(clus4SDM, subset, select=c(pa)) 
pnts_Cpa2<-lapply(pnts_Cpa, rep, times=30)#create list of pnts_Cpa with same number of replicates as there are years.
pnts_Cpa2u <- unlist(pnts_Cpa2,recursive=FALSE)


C_xm_yr<-list() #**standardize this name across code. 
b_t<-1
for(i in 1:length(pnts_Cpa2u)){
  print(paste(b_t, ':'))
  C_xm_yr[[b_t]]<- maxent(CEx_yr[[i]], p=pnts_Cpa2u[[i]])# why use new bioclim ex? 
  b_t<-b_t+1
}

# ----------------------------------------------
#PREDICT ONTO 30YR NORMALS****

C_mods_yr<-list()
m_t<-1
for(i in 1:length(C_xm_yr)){
  print(paste(m_t, ':'))
  C_mods_yr[[m_t]]<-predict(C_xm_yr[[i]], biovar_mu)
  m_t<-m_t+1
}

names(C_mods_yr)<-paste(gsub("CEx_yr", "", names(CEx_yr)),"_mods_yr", sep="")


# STACK BY SPECIES (OR IN THE CASE OF CLUSTERS, BY CLUSTER)
cl<-substring(names(C_mods_yr), first=1, last = 6) #is number of characters correct?
vC_yr<- split(C_mods_yr, cl)
C_yr_s<-mapply(stack, vC_yr)
names(C_yr_s)<-names(vC_yr)
  
# WRITE TO FILE
mapply(
  writeRaster,
  x=C_mods_yr, filename=paste("SpProjections/ward_mods/", names(C_mods_yr), sep=""), format="raster", overwrite=TRUE)  # this gives an error, but I have no idea why bc the files write correctly. Should get 94 files total. 
  

# # READ FILE (Each of yr years) *should be 47 ***stacks of 30***
# C_yr_s_files<-list.files(path="SpProjections/ward_mods/", pattern="_mods_yr+.*gri")
# C_yr_s_files2<-paste("SpProjections/ward_mods/", C_yr_s_files, sep="")
# C_yr_s<-mapply(brick, C_yr_s_files2)
# C_yr_s_names<-gsub(".gri", "", C_yr_s_files)
# names(C_yr_s)<-C_yr_s_names


}


