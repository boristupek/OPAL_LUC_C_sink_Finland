
## Carbon sink potential of land use change in Finland
## Tupek et al. 2020 boris.tupek@luke.fi

#data:
# 1) biomass and litter input (in AWEN form, total)
# 2) climate (air temperature, annual min and maximum, precipitation)
# 3) agricultural statistics
# 4) literature Karhu et al. 2011, Palosuo et al. 2014
# 5) LUC per region (Table 1)

#scripts:

### I) processing raw data for model input 
# 1) biomass and litter 
# 2) climate 

# 1) read litter data + figures (MS litter input)
# 2) read climate data  + figures (MS climate)
# 3) yasso07 model


## FIGURES LUC SIMULATIONS - SOCS  ############################################################

# 1) define functions to EXTRACT SOC or litter DATA
# 2) plot PREVIEW FIGURES
     #PREVIEW.specific.PLOTS = FALSE # TRUE
#
# simulated data required
# load(file=paste(opal.path,"data/PROCESSED/",
#                "soc.sims_climatelow.afforestation.extensification.bau_19002100_08.01.20.RData",
#                sep=""))
#saved list  c("soc.sims", "litt.sims", "sim.names", "soc.luceq100.sims", "soc.luceq.sims")

## IDENTIFY AND EXTRACT CORRECT SOC SIMULATIONS (OR LITTER INPUT) ################
#add id for identification of simulations
sim.names$sim.id <- 1:(dim(sim.names)[1]) 
#separate "region_species_fertility" column in sim names
library(tidyr)
View(sim.names)
sim.names = sim.names  %>% separate(region_species_fertility, c("region","species","fertility"),  "_" , extra = "merge")
names(sim.names)
mapply(unique, sim.names)
#define more columns
sim.names$litter.level <- sim.names$litter.type
sim.names$litter.type <- c(rep("woody",72),rep("nonwoody",(dim(sim.names)[1]-72)))
sim.names$fertility[which(sim.names$fertility == "omt")] <- "high" #high or mid
sim.names$fertility[which(sim.names$fertility == "mt")] <- "mid" 

# functions to select data from simulations ###############
#number of simulations
n.soc <- length(soc.sims) #length(sim.names)
manag.reg.spec <- sim.names[,c("management","region", "species")]
u.mang <- as.character(unique(sim.names$management))
#[1] "w_affor"    "w_cultiv"   "crop.trees"   "crop.grasstrees"  "grass.grasstrees"  "crop.extensif"    "crop.bau"   "grass.bau" 
u.reg <- c("Pirkanmaa","Kainuu","Pohjois-Pohjanmaa")
u.spec <- c("spruce","birch")
u.fert <- c("mid","high")
u.lt <- c("woody","nonwoody")
u.ci <- c("high","mean","low") #,"high0","mean0","low0") #derived from confidence interval for agricultural data
#NOTE:
# for trees, woody litter, mean =  INCREASING woody SOC from afforestation
# for trees, woody litter, low = DEPLEATING woody SOC from BAU (from ZERO woody litter input after cultivation started in 1990)
names(sim.names)
mapply(unique, sim.names)

## function to extract right column data  from soc simulations###
#various levels of selection
exdat.fun <- function(mang,reg,spec,fert,level,litt){
  subset(sim.names, management == mang & region == reg & species == spec & fertility == fert & litter.level == level & litter.type == litt)
}

exdat.bau.fun <- function(mang,reg,level,litt){
  subset(sim.names, management == mang & region == reg  & litter.level == level & litter.type == litt)
}

exdat.fun0 <- function(mang,reg,spec,fert,level){
  subset(sim.names, management == mang & region == reg & species == spec & fertility == fert & litter.level == level)
}

exdat.fun3 <- function(mang,reg,spec){
  subset(sim.names, management == mang & region == reg & species == spec)
}

exdat.fun2 <- function(mang,reg){
  subset(sim.names, management == mang & region == reg)
}


#function to avoid plotting zeros
xna.fun <- function(x){#replace 0 by na
  x[x==0]<-NA
  return(x)
}

## soc and litt C totals (sum of AWEN pools) #########
soc <- data.frame(matrix(NA, nrow = 201 , ncol = n.soc)) #201 years 1900-2100
names(soc)<-paste("c",1:n.soc,sep="")
litt <- data.frame(matrix(NA, nrow = 201 , ncol = n.soc)) #201 years 1900-2100
names(litt)<-paste("c",1:n.soc,sep="")
for(i in 1:n.soc){
  # i = 1
  sim.i <- soc.sims[[i]]$sims
  soc.i <- soc.sims[[i]]$soc
  soc[,i] <- rowSums(soc.i[,2:6])
  litt.i <- litt.sims[[i]]$litt
  n.li <- dim(litt.i)[1]
  litt[1:n.li,i] <- rowSums(litt.i[,1:4])
}

## soc equilibrium and luc ########
soc.eqluc <- data.frame(matrix(NA, nrow = 3 , ncol = n.soc)) #201 years 1900-2100
names(soc.eqluc)<-paste("c",1:n.soc,sep="")
row.names(soc.eqluc)<- c("soc.eqfor","soc.cultiv", "soc.eqluc")
for(i in 1:n.soc){
  # i = 1
  sim.i <- soc.luceq.sims[[i]]$sims
  soc.ef.i <- soc.luceq.sims[[i]]$soc.eqfor
  soc.cu.i <- soc.luceq.sims[[i]]$soc.cultiv
  soc.el.i <- soc.luceq.sims[[i]]$soc.eqluc
  
  soc.eqluc[1,i] <- sum(soc.ef.i)
  soc.eqluc[2,i] <- sum(soc.cu.i)
  soc.eqluc[3,i] <- sum(soc.el.i)
  
}

#PREVIEW FIGURES OF ALL SIMULATIONS  TOGETHER ###################################
#figure total litter (sims organized collumn-wise)
par(mfrow=c(1,1), mar=c(4,4,1,1))
plot(1,1, col = "white",xlim=c(1,201),ylim=c(0,10),xlab="year",ylab="Litter inputs")
litt.na <- litt
litt.na[litt.na == 0] <- NA
for(i in 1:n.soc){
  # i = 1
  lines(1:201,litt.na[,i]) #
}

#figure total socs (sims organized collumn-wise)
par(mfrow=c(1,1))
plot(1,1, col = "white",xlim=c(1,201),ylim=c(0,210),xlab="year",ylab="SOC sims - one rotation")
soc.na <- soc
soc.na[soc.na == 0] <- NA
for(i in 1:n.soc){
  # i = 1
  lines(1:201,soc.na[,i]) #
}

#figure total repeated rotations socs (sims organized collumn-wise)
if(repeat.rotations == TRUE ){ 
  #soc.eqluc100  is empty if repeated simulations were set to FALSE in "opal_yasso07.mod.runs_03.07.20.r"!
  
  #REPEATED SOC ROTATIONS to equilibrium after luc #repeated rotations
  soc.eqluc100 <- data.frame(matrix(NA, nrow = 5000 , ncol = n.soc)) 
  names(soc.eqluc100)<-paste("c",1:n.soc,sep="")
  for(i in 1:n.soc){
    # i = 1
    sim.i <- soc.luceq100.sims[[i]]$sims
    soc.ixk <- soc.luceq100.sims[[i]]$soc.eq100
    
    k.ix <- dim(soc.ixk)[1]
    soc.eqluc100[1:k.ix,i] <- rowSums(soc.ixk)
    
  }
  dim(soc.eqluc100)
  #remove all NA rows
  soc.eqluc100 <- subset(soc.eqluc100, rowSums(soc.eqluc100, na.rm =T)!=0)
  
par(mfrow=c(1,1))
plot(1,1, col = "white",xlim=c(1,3555),ylim=c(0,150),xlab="year",ylab="SOC sims -repeated rotations")
soc.na <- soc.eqluc100 
#soc.na[soc.na == 0] <- NA
for(i in 1:n.soc){
  # i = 1
  lines(1:3555,soc.na[,i]) #
}
}

#################################################################################
PREVIEW.specific.PLOTS = FALSE # TRUE
if(PREVIEW.specific.PLOTS == TRUE){ #fast evaluation of input data and simulated outputs
  
#PREVIEW FIGURES FOR SOC SIMULATION (Litter and SOC)##########################################
# SPECIFIC to managment, region, species, fertlity, litter uncertainty estimate #############

## LITTER INPUTS FOR SIMULATIONS ##
#vary management , new plots
for(mi in 1:8){
  # mi = 1
  #vary regions , new panels
  par(mfrow=c(3,2), mar=c(4,4,3,1))
  for(ri in 1:3){ #region
    # ri = 1
    #fig SPRUCE mid and high fertility  
    plot(c(0,201),c(0,10), col = "white", xlab="years", ylab=paste(c(rep("Woody",2),rep("Non-Woody",6))[mi], "Litter input", sep = " "),
         main = paste(u.mang[mi], u.reg[ri], u.spec[1],  sep="-")) 
    
    sim.mrsi <- exdat.fun3(u.mang[mi], u.reg[ri], u.spec[1]) #fix birch u.spec[2] to spruce u.spec[1]!!!
    u.fi <- unique(sim.mrsi$fertility)
    
    for(fi in u.fi){ #fertility
      # fi = "high"
      fi.l <- match(fi,c("mid","high"))
      sim.h.i <- exdat.fun0(u.mang[mi], u.reg[ri], u.spec[1], fi, u.ci[1] ) #u.fert[fi], u.lt[1]) #u.ci "high"
      sim.m.i <- exdat.fun0(u.mang[mi], u.reg[ri], u.spec[1], fi, u.ci[2] ) #, u.lt[1]) #u.ci "mean"
      sim.l.i <- exdat.fun0(u.mang[mi], u.reg[ri], u.spec[1], fi, u.ci[3] ) #, u.lt[1]) #u.ci "low"
      
      litt.h.i <- litt[,sim.h.i$sim.id]
      litt.m.i <- litt[,sim.m.i$sim.id]
      litt.l.i <- litt[,sim.l.i$sim.id]
      
      litt.i3 <- cbind(litt.h.i,litt.m.i,litt.l.i)
      litt.na.i3 <- litt.i3
      litt.na.i3[litt.na.i3 == 0] <- NA
      
      
      #head(litt.na.i3)
      lines(litt.na.i3[,1], col = 2, lty = fi.l)
      lines(litt.na.i3[,2], col = 1, lty = fi.l)
      lines(litt.na.i3[,3], col = 4, lty = fi.l)
      
    }
    #fig BIRCH - mid and high fertility
    plot(c(0,201),c(0,10), col = "white", xlab="years", ylab=paste(c(rep("Woody",2),rep("Non-Woody",6))[mi], "Litter input", sep = " "),
         main = paste(u.mang[mi], u.reg[ri], u.spec[2],  sep="-")) #u.fert[1],
    
    sim.mrsi <- exdat.fun3(u.mang[mi], u.reg[ri], u.spec[2])
    u.fi <- unique(sim.mrsi$fertility)
    
    for(fi in u.fi){ #fertility
      # fi = "high"
      fi.l <- match(fi,c("mid","high"))
      sim.h.i <- exdat.fun0(u.mang[mi], u.reg[ri], u.spec[2], fi, u.ci[1] ) #, u.lt[1]) #u.ci "high"
      sim.m.i <- exdat.fun0(u.mang[mi], u.reg[ri], u.spec[2], fi, u.ci[2] ) #, u.lt[1]) #u.ci "mean"
      sim.l.i <- exdat.fun0(u.mang[mi], u.reg[ri], u.spec[2], fi, u.ci[3] ) #, u.lt[1]) #u.ci "low"
      
      litt.h.i <- litt[,sim.h.i$sim.id]
      litt.m.i <- litt[,sim.m.i$sim.id]
      litt.l.i <- litt[,sim.l.i$sim.id]
      
      litt.i3 <- cbind(litt.h.i,litt.m.i,litt.l.i)
      litt.na.i3 <- litt.i3
      litt.na.i3[litt.na.i3 == 0] <- NA
      
      #head(litt.i3)
      lines(litt.na.i3[,1], col = 2, lty = fi.l)
      lines(litt.na.i3[,2], col = 4, lty = fi.l)
      lines(litt.na.i3[,3], col = 4, lty = fi.l)
      
    }
  }
}

## SOC SIMULATIONS ################
#vary management , new plots
for(mi in 1:8){
  # mi = 4
  #vary regions , new panels
  par(mfrow=c(3,2), mar=c(4,4,3,1))
  for(ri in 1:3){ #region
    # ri = 1
    #fig SPRUCE mid and high fertility  
    plot(c(0,201),c(30,130), col = "white", xlab="years", ylab="soc", #c(0,250)
         main = paste(u.mang[mi], u.reg[ri], u.spec[1],  sep="-")) 
    
    sim.mrsi <- exdat.fun3(u.mang[mi], u.reg[ri], u.spec[1]) #fix birch u.spec[2] to spruce u.spec[1]!!!
    u.fi <- unique(sim.mrsi$fertility)
    
    for(fi in u.fi){ #fertility
      # fi = "high"
      fi.l <- match(fi,c("mid","high"))
      sim.h.i <- exdat.fun0(u.mang[mi], u.reg[ri], u.spec[1], fi, u.ci[1] ) #u.fert[fi], u.lt[1]) #u.ci "high"
      sim.m.i <- exdat.fun0(u.mang[mi], u.reg[ri], u.spec[1], fi, u.ci[2] ) #, u.lt[1]) #u.ci "mean"
      sim.l.i <- exdat.fun0(u.mang[mi], u.reg[ri], u.spec[1], fi, u.ci[3] ) #, u.lt[1]) #u.ci "low"
      
      soc.h.i <- soc[,sim.h.i$sim.id]
      soc.m.i <- soc[,sim.m.i$sim.id]
      soc.l.i <- soc[,sim.l.i$sim.id]
      
      soc.i3 <- cbind(soc.h.i,soc.m.i,soc.l.i)
      soc.na.i3 <- soc.i3
      soc.na.i3[soc.na.i3 == 0] <- NA
      
      
      #head(soc.na.i3)
      lines(soc.na.i3[,1], col = 2, lty = fi.l)
      lines(soc.na.i3[,2], col = 1, lty = fi.l) # 1)#
      lines(soc.na.i3[,3], col = 4, lty = fi.l)
      
    }
    #fig BIRCH - mid and high fertility
    plot(c(0,201),c(30,130), col = "white", xlab="years", ylab="soc",
         main = paste(u.mang[mi], u.reg[ri], u.spec[2],  sep="-")) #u.fert[1],
    
    sim.mrsi <- exdat.fun3(u.mang[mi], u.reg[ri], u.spec[2])
    u.fi <- unique(sim.mrsi$fertility)
    
    for(fi in u.fi){ #fertility
      # fi = "high"
      fi.l <- match(fi,c("mid","high"))
      sim.h.i <- exdat.fun0(u.mang[mi], u.reg[ri], u.spec[2], fi, u.ci[1] ) #, u.lt[1]) #u.ci "high"
      sim.m.i <- exdat.fun0(u.mang[mi], u.reg[ri], u.spec[2], fi, u.ci[2] ) #, u.lt[1]) #u.ci "mean"
      sim.l.i <- exdat.fun0(u.mang[mi], u.reg[ri], u.spec[2], fi, u.ci[3] ) #, u.lt[1]) #u.ci "low"
      
      soc.h.i <- soc[,sim.h.i$sim.id]
      soc.m.i <- soc[,sim.m.i$sim.id]
      soc.l.i <- soc[,sim.l.i$sim.id]
      
      soc.i3 <- cbind(soc.h.i,soc.m.i,soc.l.i)
      soc.na.i3 <- soc.i3
      soc.na.i3[soc.na.i3 == 0] <- NA
      
      #head(soc.i3)
      lines(soc.na.i3[,1], col = 2, lty = fi.l)
      lines(soc.na.i3[,2], col = 1, lty = fi.l)
      lines(soc.na.i3[,3], col = 4, lty = fi.l)
      
    }
  }
}

} #end of preview figures 

