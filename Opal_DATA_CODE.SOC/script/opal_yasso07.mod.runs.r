
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


## RUN YASSO07 SOC MODELLING  SCENARIOS ############################################################################################################################

#SOC simulations #########################
soc.sims <- list() #for each managemet
litt.sims <- list() #for each managemet
soc.luceq.sims <- list()
soc.luceq100.sims <- list()

#note: here enable/disable repeated soc LUC rotations until equilibrium!!!
repeat.rotations = FALSE #TRUE 

sim.names <- NULL
#loop to change litter input for the specific management type and climate (3 scenarios and 3 regions)
#for(s in 1:3){ #climate scenarios
for(m in 1:8){ #management type
  
  #names.manage
  #[1] "w_affor"          "w_cultiv"         "crop.trees"       "crop.grasstrees"  "grass.grasstrees"
  #[6] "crop.extensif"    "crop.bau"         "grass.bau" 
  
  #litters
  #1 WOODY
  #[1] "awen.litt.list_w_affor"          "awen.litt.list_w_cultiv"    
  # rest NON-WOODY
  #[3] "awen.litt.list_crop.trees"      
  #[4] "awen.litt.list_crop.grasstrees"  "awen.litt.list_grass.grasstrees" "awen.litt.list_crop.extensif"   
  #[7] "awen.litt.list_crop.bau"         "awen.litt.list_grass.bau"  
  
  # m = 1 #wood litter
  litters[m]
  litter.m <- get(litters[m])
  #[1] "litter.list.w"   "litter.list.w.h" "litter.list.w.l"
  
  for(n in 1:length(litter.m)){ # length depends on type, woody 2 x (mean, high, low) and non-woody 3 x (mean, high, low)
    #n = 1
    litter.m[n]
    #[1] "litter.list.w"
    litter.mn <- get(litter.m[n])
    
    for(j in 1:length(litter.mn)){ # 
      #litter combinations forest 1:12 woody depending on region3-species2 and 2 fertility  
      #or 1:6 nonwoody region and species (fertility defined in the management)
      # j = 1
      litter.mnj <- litter.mn[[j]] #note! 1st row mean forest litter for spin-up
      name.mnj <- names(litter.mn)[j]
      name.reg <- strsplit(name.mnj,"_")[[1]][1]
      
 
      #limit long rotations until 2100
      if(dim(litter.mnj)[1] > 201){
        litter.mnj <- litter.mnj[1:201,] 
      }
      
      ##find clear cut litter for afforestations scenarios
      cc.mnj <- match(name.mnj, names(clearcut.stump.cw.awen))
      #litters[c(1,3,4,5)]
      #woody
      #[1] "awen.litt.list_w_affor" 
      #non-woody
      #[3] "awen.litt.list_crop.trees"       "awen.litt.list_crop.grasstrees" 
      #[5] "awen.litt.list_grass.grasstrees"
      #m=1
      if(m==1){
        clearcut.litter.stump.mnj  <- clearcut.stump.cw.awen[[cc.mnj]]
        clearcut.litter.roots.mnj  <- clearcut.roots.fw.awen[[cc.mnj]]
        clearcut.litter.fineroots.mnj  <- c(0,0,0,0)
      }else if(m==3|m==4|m==5){
        clearcut.litter.stump.mnj  <- c(0,0,0,0)
        clearcut.litter.roots.mnj  <- c(0,0,0,0)
        clearcut.litter.fineroots.mnj  <- clearcut.fineroots.nw.awen[[cc.mnj]]
      }else{
        clearcut.litter.stump.mnj  <- c(0,0,0,0)
        clearcut.litter.roots.mnj  <- c(0,0,0,0)
        clearcut.litter.fineroots.mnj  <- c(0,0,0,0)
      }
      #sum the wody litter
      clearcut.litter.mnj <- clearcut.litter.stump.mnj +
        clearcut.litter.roots.mnj +
        clearcut.litter.fineroots.mnj 
      clearcut.litter.mnj[5] <- 0 #add zero for humus
      
      if(m <3 ){ #1 and 2 litter type WOOD
        c.size <- 2
      } else{
        c.size <- 0 #NON-WOOD
      }
      
      #select climate regions for selected climate
      s=3 #RCP8.5 scenario
      clim <- get(clim.types[grep(c("low","mean","high")[s], clim.types)])
      n.cr <- match(name.reg,names(clim))
      clim.r<- clim[[n.cr]] #region
      names(clim.r)
      
      #climate and litter of equilibrium state forest in 1900
      clim.r1 <- as.numeric(clim.r[1,2:4])
      inawen.ie <- c(as.numeric(litter.mnj[1,]),0)
      
      #yasso07 equilibrium (solve analytically)
      #equilibrium for 1900
      AYS.ix <- yasso.matrix.fun(WS = c.size, clim =  clim.r1, A.print = "n")
      soileqc.ix = -1*solve(AYS.ix)%*% inawen.ie # inawen.ie =u.Li #inverse of matrix solve(B)
      
      #run yasso07 for time series
      #time series for 1901:2100
      n.out <- length(litter.mnj[,1])
      soc1921.ix <- matrix(0, nrow = 201, ncol=6)
      soc1921.ix[,1] <- 1900:2100
      soc1921.ix[1,2:6] <- as.numeric(soileqc.ix)
      
      #temp.l   prec.l temp.ampl
      clim.MT <- clim.r[-1, 2] #remove first row 
      clim.PR_mm <- clim.r[-1, 3]
      clim.TA <- clim.r[-1, 4]
      n.ix <- dim(litter.mnj)[1]
      years.ix <- 1:(n.ix-1)
      
      LI.awen.ix <- data.frame(years=years.ix, litter.mnj[-1,], H = 0)
      soc1921.mod.ix <- Yasso07Modelfi(t=years.ix, 
                                       C0 = as.numeric(soileqc.ix), #initial equilibrium carbon
                                       AWEN = 0, # zero because litter input 
                                       In=LI.awen.ix,#litter C input (same length as years)
                                       xi = 0, # 0 to use climate data, only xi = 1  will replace climate data no climate effect,
                                       MT=clim.MT,#MeanTemperature
                                       TA=clim.TA, #TemperatureAmplitude
                                       PR_mm=clim.PR_mm,#Precipitation_mm)
                                       WS=c.size) 
      
      soc1921.c.ix <- getC(soc1921.mod.ix)
      soc1921.ix[2:n.ix,2:6] <- soc1921.c.ix
      
      # soc equilibrium for the mean conditions after LUC ( repeated simulations, Janne Rämö)
      clim.luc.mean <- c(mean(clim.MT[118:200]),mean(clim.PR_mm[118:200]),mean(clim.TA[118:200]))
      LI.awen.ix.luc <- subset(LI.awen.ix, years > 117)
      years.ix.luc <- LI.awen.ix.luc$years
      inawen.ie.luc <- as.numeric(apply(LI.awen.ix.luc[,c("A","W","E","N","H")],2,mean))
      AYS.ix.luc <- yasso.matrix.fun(WS = c.size, clim =  clim.luc.mean, A.print = "n")
      soileqc.ix.luc = -1*solve(AYS.ix.luc)%*% inawen.ie.luc # inawen.ie =u.Li #inverse of matrix solve(B)
      soileqc.ix.luc = as.numeric(soileqc.ix.luc)
      
      #soc equilibrium forest (before cultivation)
      soileqc.ix = as.numeric(soileqc.ix)
      #soc before luc (end of cultivation)
      soil.ix.luc0 = as.numeric(soc1921.c.ix[117,]) 
      
      #repeat.rotations = FALSE #
      #note: enable/disable repeated soc LUC rotations until equilibrium
      if(repeat.rotations == TRUE){
        #rerun simulations 100 times
        sim.time.ix100.start <- Sys.time()
        soc1921.ix.luc <- subset(soc1921.ix, soc1921.ix[,1] > 2017 & soc1921.ix[,2] > 0)
        years.ix.luc0 <- years.ix.luc-117
        LI.awen.ix.luc1 <-LI.awen.ix.luc
        #add stumps+roots+fineroots Biomass as litter after final harvesting
        LI.awen.ix.luc1[1,c("A","W","E","N","H")] <- LI.awen.ix.luc[1,c("A","W","E","N","H")]+
          clearcut.litter.mnj
        
        LI.awen.ix.luc1$years <- years.ix.luc0
        k.ix.luc <- length(years.ix.luc0 )
        MT.mean.ixluc=rep(mean(clim.MT),k.ix.luc) #MeanTemperature
        TA.mean.ixluc=rep(mean(clim.TA),k.ix.luc)  #TemperatureAmplitude
        PR_mm.mean.ixluc=rep(mean(clim.PR_mm),k.ix.luc) #Precipitation_mm)
        
        soc1921.ix100.luc <- soc1921.ix.luc[,2:6]
        for(k in 1:100){
          #k= 1
          #initial soc in simulation loop (last of previous)
          k.ix <- dim(soc1921.ix100.luc)[1]
          soc0.ix.k <- as.numeric(soc1921.ix100.luc[k.ix,])
          #soc rotation
          soc1921.mod100.ixk <- Yasso07Modelfi(t=years.ix.luc0, 
                                               C0 = as.numeric(soc0.ix.k), #initial equilibrium carbon
                                               AWEN = 0, # zero because litter input 
                                               In=LI.awen.ix.luc1,#litter C input (same length as years)
                                               xi = 0, # 0 to use climate data, only xi = 1  will replace climate data no climate effect,
                                               MT=MT.mean.ixluc,#MeanTemperature
                                               TA=TA.mean.ixluc, #TemperatureAmplitude
                                               PR_mm=PR_mm.mean.ixluc,#Precipitation_mm)
                                               WS=c.size) 
          soc1921.c100.ixk <- getC(soc1921.mod100.ixk)
          
          
          #skip looping if difference  of sum is below 1 ton
          soc1.ix.k <- as.numeric(soc1921.c100.ixk[k.ix.luc,])
          d.soc0.ixk <- abs(sum(soc0.ix.k)-sum(soc1.ix.k))
          if (k > 30 & d.soc0.ixk < 0.1){ #break the loop if difference in SOC simulations is 0.1 tC/ha
            print(c(names.manage[m], n, j, k, d.soc0.ixk))
            break
          }
          soc1921.ix100.luc <- rbind(soc1921.ix100.luc,soc1921.c100.ixk)
        }
        sim.time.ix100.end <- Sys.time()
        sim.time.ix100.end-sim.time.ix100.start
      }else{
        soc1921.ix100.luc <- NULL
        k <- NULL
      }

      print(paste("management = ",names.manage[m]," : combination =",name.mnj, 
                  " : litter = ", c("mean","high","low","mean0","high0","low0")[n]," : lenght =",dim(litter.mnj)[1]))
      sim.names0 <- c(names.manage[m],name.mnj, 
                      c("mean","high","low","mean0","high0","low0")[n],
                      dim(litter.mnj)[1])
      #fig only for repeated simulations
      #plot(rowSums(soc1921.ix100.luc), type="l")
      #legend("topright", paste(k, sim.names0[1],n, collapse="-"), bty ="n")
      
      if(m==1 & n == 1 & j == 1){ #woody
        sim.names <- sim.names0
        soc.sims[[1]] <- list(sims=sim.names0, soc = soc1921.ix )
        litt.sims[[1]] <- list(sims=sim.names0, litt = litter.mnj ) 
        
        soc.luceq100.sims[[1]] <- list(sims=sim.names0, 
                                       soc.eq100 = soc1921.ix100.luc,
                                       k100 = k)
        
        soc.luceq.sims[[1]] <- list(sims=sim.names0, 
                                    soc.eqfor = soileqc.ix, 
                                    soc.cultiv = soil.ix.luc0,
                                    soc.eqluc = soileqc.ix.luc)
        
        
      } else { #non woody
        sim.names <- rbind(sim.names, sim.names0)
        
        l <- length(sim.names[,1])
        soc.sims[[l]] <- list(sims=sim.names0, soc = soc1921.ix )
        litt.sims[[l]] <- list(sims=sim.names0, litt = litter.mnj )
        
        soc.luceq100.sims[[l]] <- list(sims=sim.names0, 
                                       soc.eq100 = soc1921.ix100.luc,
                                       k100 = k)
        
        soc.luceq.sims[[l]] <- list(sims=sim.names0, 
                                    soc.eqfor = soileqc.ix, 
                                    soc.cultiv = soil.ix.luc0,
                                    soc.eqluc = soileqc.ix.luc)
      }
      
      
    } # end litter combinations
  } # end litter types (low, mean, high)
} # end of land management

dim(sim.names)
sim.names <- data.frame(sim.names, row.names=(1:dim(sim.names)[1]))
head(sim.names)
colnames(sim.names) <- c("management","region_species_fertility","litter.type","length.end")
sim.names

#save repeated rotations 
save(list= c("sim.names", "soc.luceq100.sims"), 
     file=paste(opal.path,"data/PROCESSED/",
                "soc.sims_repeated.rotations.after.LUC.until.equilibrium_29.01.20.RData",
                sep=""))
#load repeated rotations to avoid lenghty simulations
#load(paste(opal.path,"data/PROCESSED/",
#           "soc.sims_repeated.rotations.after.LUC.until.equilibrium_29.01.20.RData",
#           sep=""))


