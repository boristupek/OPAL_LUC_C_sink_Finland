
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

# 2) plot PREVIEW FIGURES of individual scenarios
     #PREVIEW.specific.PLOTS = FALSE # TRUE

#################################################################################

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

