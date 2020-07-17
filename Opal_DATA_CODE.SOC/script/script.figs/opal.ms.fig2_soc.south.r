## OPAL plots SOUTH SOC simulations with CI (confidence intervals) ##################

#both species, fetility (high, mid)
#management afforestation, extensisfication, business as usual
#boris Jan 2020
#load data (SOC simulations! soc)
#load("H:/R.tempwork/opal.offline.part_10.01.20.RData")

#note: figure below does not fill the all data into this table
sim.fig.names <- c(paste(rep(rep(c("Pi","Ka","Po"),each =5),2),
                         rep(c("cf", "cgf","gf","cc","gg"),6),
                         rep(c("su", "bi"),each = 15),sep="_"),
                   paste(rep(c("Pi","Ka","Po"),each =3),
                         rep(c("cg","cc","gg"),3),sep="_"))
#SOC data from figure (soil change) 
soc.fig.h <- data.frame(matrix(NA, ncol = length(sim.fig.names), nrow = length(1:201)))#118 nin stead of 1
soc.fig.m <- data.frame(matrix(NA, ncol = length(sim.fig.names), nrow = length(1:201)))
soc.fig.l <- data.frame(matrix(NA, ncol = length(sim.fig.names), nrow = length(1:201)))
names(soc.fig.h) <- sim.fig.names
names(soc.fig.m) <- sim.fig.names
names(soc.fig.l) <- sim.fig.names

# MS.FIG1 SIMPLIFIED  SOUTH ################################################


#4 panels #deforestation, afforestation spruce, afforestation birch, extensificaiton
#simplified figure only SOUTH = Pirkanmaa !!!!!
par(mfrow=c(1,4), mar=c(1,1,0.3,1), oma= c(4,5,2,1))
for(i in 1){ #regions 
  #i = 1
  reg.i <- rep(c("Pirkanmaa","Kainuu","Pohjois-Pohjanmaa"),3)[i]
  spec.i <- rep(c("spruce", "birch","spruce"),each =3)[i]
  spec.n.i <- rep(c(1, 2, 1),each =3)[i]
  
  tit.leg <- c("Deforestation / Cultivation", 
               "Extensification",
               "Afforestation SPRUCE", "Afforestation BIRCH")
  
  
  soc.i1.1 <- data.frame(matrix(NA,201,3))
  soc.i1.2 <- data.frame(matrix(NA,201,3))
  soc.i1.3 <- data.frame(matrix(NA,201,3))
  names(soc.i1.1) <- c("h","m","l")
  names(soc.i1.2) <- c("h","m","l")
  names(soc.i1.3) <- c("h","m","l")
  
  soc.bi1.1 <- data.frame(matrix(NA,201,3))
  soc.bi1.2 <- data.frame(matrix(NA,201,3))
  soc.bi1.3 <- data.frame(matrix(NA,201,3))
  names(soc.bi1.1) <- c("h","m","l")
  names(soc.bi1.2) <- c("h","m","l")
  names(soc.bi1.3) <- c("h","m","l")
  
  names(soc)
  
  #woody SPRUCE spec.i/"spruce"
  wsim.hi <- exdat.fun("affor", reg.i, "spruce", "high", "mean", "woody")$sim.id #set fixed inputs by words
  wsoc.hi <- soc[,wsim.hi]
  wsim.mi <- exdat.fun("affor", reg.i, "spruce", "mid", "mean", "woody")$sim.id 
  wsoc.mi <- soc[,wsim.mi]
  
  #woody BIRCH
  wsim.bhi <- exdat.fun("affor", reg.i, "birch", "high", "mean", "woody")$sim.id #set fixed inputs by words
  wsoc.bhi <- soc[,wsim.bhi]
  wsim.bmi <- exdat.fun("w_affor", reg.i, "birch", "mid", "mean", "woody")$sim.id 
  wsoc.bmi <- soc[,wsim.bmi]
  
  #woody for spruce equilibrium forest and ZERO litter BAU
  wsim0.shi <- exdat.fun("cultiv", reg.i, "spruce", "high", "mean", "woody")$sim.id #set fixed inputs by words
  wsoc0.shi <- soc[,wsim0.shi]
  wsim0.smi <- exdat.fun("cultiv", reg.i, "spruce", "mid", "mean", "woody")$sim.id 
  wsoc0.smi <- soc[,wsim0.smi]
  
  
  soc.i1.1 <- data.frame(matrix(NA,201,3))
  soc.i1.2 <- data.frame(matrix(NA,201,3))
  soc.i1.3 <- data.frame(matrix(NA,201,3))
  soc.i2.1 <- data.frame(matrix(NA,201,3))
  soc.i2.2 <- data.frame(matrix(NA,201,3))
  soc.i3.1 <- data.frame(matrix(NA,201,3))
  names(soc.i1.1) <- c("h","m","l")
  names(soc.i1.2) <- c("h","m","l")
  names(soc.i1.3) <- c("h","m","l")
  names(soc.i2.1) <- c("h","m","l")
  names(soc.i2.2) <- c("h","m","l")
  names(soc.i3.1) <- c("h","m","l")
  #WOODY
  
  #SPRUCE
  #woody highly fertile #ci high mean low
  wsim.hi.h <- exdat.fun("affor", reg.i, "spruce", "high", "high", "woody")$sim.id #set fixed inputs by words
  wsim.hi.m <- exdat.fun("affor", reg.i, "spruce", "high", "mean", "woody")$sim.id #set fixed inputs by words
  wsim.hi.l<- exdat.fun("affor", reg.i, "spruce", "high", "low", "woody")$sim.id #set fixed inputs by words
  wsim.hi <- c(wsim.hi.h,wsim.hi.m,wsim.hi.l)
  wsoc.hi <- soc[,wsim.hi]
  #woody mid fertile #ci high mean low
  wsim.mi.h <- exdat.fun("affor", reg.i, "spruce", "mid", "high", "woody")$sim.id #set fixed inputs by words
  wsim.mi.m <- exdat.fun("affor", reg.i, "spruce", "mid", "mean", "woody")$sim.id #set fixed inputs by words
  wsim.mi.l <- exdat.fun("affor", reg.i, "spruce", "mid", "low", "woody")$sim.id #set fixed inputs by words
  wsim.mi <- c(wsim.mi.h,wsim.mi.m,wsim.mi.l)
  wsoc.mi <- soc[,wsim.mi]
  
  #BIRCH
  #woody highly fertile #ci high mean low
  wsim.bhi.h <- exdat.fun("affor", reg.i, "birch", "high", "high", "woody")$sim.id #set fixed inputs by words
  wsim.bhi.m <- exdat.fun("affor", reg.i, "birch", "high", "mean", "woody")$sim.id #set fixed inputs by words
  wsim.bhi.l<- exdat.fun("affor", reg.i, "birch", "high", "low", "woody")$sim.id #set fixed inputs by words
  wsim.bhi <- c(wsim.bhi.h,wsim.bhi.m,wsim.bhi.l)
  wsoc.bhi <- soc[,wsim.bhi]
  #woody mid fertile #ci high mean low
  wsim.bmi.h <- exdat.fun("affor", reg.i, "birch", "mid", "high", "woody")$sim.id #set fixed inputs by words
  wsim.bmi.m <- exdat.fun("affor", reg.i, "birch", "mid", "mean", "woody")$sim.id #set fixed inputs by words
  wsim.bmi.l <- exdat.fun("affor", reg.i, "birch", "mid", "low", "woody")$sim.id #set fixed inputs by words
  wsim.bmi <- c(wsim.bmi.h,wsim.bmi.m,wsim.bmi.l)
  wsoc.bmi <- soc[,wsim.bmi]
  
  #woody for spruce equilibrium forest and ZERO litter BAU
  wsim0.shi.h <- exdat.fun("cultiv", reg.i, "spruce", "high", "high", "woody")$sim.id #low to mean #set fixed inputs by words
  wsim0.shi.m <- exdat.fun("cultiv", reg.i, "spruce", "high", "mean", "woody")$sim.id #low to mean #set fixed inputs by words
  wsim0.shi.l <- exdat.fun("cultiv", reg.i, "spruce", "high", "low", "woody")$sim.id #low to mean #set fixed inputs by words
  wsim0.shi <- c(wsim0.shi.h,wsim0.shi.m,wsim0.shi.l)
  wsoc0.shi <- soc[,wsim0.shi]
  wsim0.smi.h <- exdat.fun("cultiv", reg.i, "spruce", "mid", "high", "woody")$sim.id 
  wsim0.smi.m <- exdat.fun("cultiv", reg.i, "spruce", "mid", "mean", "woody")$sim.id
  wsim0.smi.l <- exdat.fun("cultiv", reg.i, "spruce", "mid", "low", "woody")$sim.id
  wsim0.smi <- c(wsim0.smi.h,wsim0.smi.m,wsim0.smi.l)
  wsoc0.smi <- soc[,wsim0.smi]
  
  #AFFORESTATION
  for(j in 1 : 3){ # c("high","mean","low") confidence interval for agricultural data
    # j = 1
    
    #SPRUCE
    #non-woody
    #1.1 Afforestation crop to trees (NO grass) u.mang: "crop.trees"
    sim.i1.1 <- exdat.fun("crop.trees" , reg.i, "spruce", "high", u.ci[j], "nonwoody")$sim.id 
    soc.i1.1[,j] <- xna.fun(soc[,sim.i1.1])+wsoc.hi[,j]
    #1.2 Afforestation crop to trees (ON/with grass) u.mang: "crop.grasstrees"
    sim.i1.2 <- exdat.fun("crop.grasstrees" , reg.i, "spruce", "high", u.ci[j], "nonwoody")$sim.id 
    soc.i1.2[,j] <- xna.fun(soc[,sim.i1.2])+wsoc.hi[,j]
    #1.3 Afforestation grass to trees (with grass) u.mang: "grass.grasstrees"
    sim.i1.3 <- exdat.fun("grass.grasstrees" , reg.i, "spruce", "mid", u.ci[j], "nonwoody")$sim.id 
    soc.i1.3[,j] <- xna.fun(soc[,sim.i1.3])+wsoc.mi[,j]
    
    #BIRCH
    #non-woody
    #1.1 Afforestation crop to trees (NO grass) u.mang: "crop.trees"
    sim.bi1.1 <- exdat.fun("crop.trees" , reg.i, "birch", "high", u.ci[j], "nonwoody")$sim.id 
    soc.bi1.1[,j] <- xna.fun(soc[,sim.bi1.1])+wsoc.bhi[,j]
    #1.2 Afforestation crop to trees (ON/with grass) u.mang: "crop.grasstrees"
    sim.bi1.2 <- exdat.fun("crop.grasstrees" , reg.i, "birch", "high", u.ci[j], "nonwoody")$sim.id 
    soc.bi1.2[,j] <- xna.fun(soc[,sim.bi1.2])+wsoc.bhi[,j]
    #1.3 Afforestation grass to trees (with grass) u.mang: "grass.grasstrees"
    sim.bi1.3 <- exdat.fun("grass.grasstrees" , reg.i, "birch", "mid", u.ci[j], "nonwoody")$sim.id 
    soc.bi1.3[,j] <- xna.fun(soc[,sim.bi1.3])+wsoc.bmi[,j]
    
  }
  #initial forest soc (high,mean,low)
  soc.init1h <- soc.i1.1[1,] #same as soc.i1.2[1,]
  #soc.init sensitivity
  soc.init1m <- soc.i1.3[1,]
  
  
  #BAU business as usual
  #u.mang
  for(j in 1 : 3){ # c("high","mean","low") confidence interval for agricultural data
    #2.1 crop.bau
    sim.i2.1 <- exdat.bau.fun("crop.bau" , reg.i, u.ci[j], "nonwoody")$sim.id [spec.n.i]
    sim.names[sim.i2.1,]
    soc.i2.1[,j] <- soc[,sim.i2.1]
    #2.2 grass.bau
    sim.i2.2 <- exdat.bau.fun("grass.bau" , reg.i, u.ci[j], "nonwoody")$sim.id [spec.n.i]
    soc.i2.2[,j] <- soc[,sim.i2.2]
    #add wody soc part remaining from the equilibrium forest (note this is only technical solution)
    soc.i2.1[,j] <- xna.fun(soc[,sim.i2.1])+wsoc0.shi[,j] 
    soc.i2.2[,j] <- xna.fun(soc[,sim.i2.2])+wsoc0.smi[,j] 
  }
  
  #EXTENSIFICATION
  #u.mang
  for(j in 1 : 3){ # c("high","mean","low") confidence interval for agricultural data
    #3.1 crop.extensification
    sim.i3.1 <- exdat.bau.fun("crop.extensif", reg.i, u.ci[j], "nonwoody")$sim.id [1]
    sim.names[sim.i3.1,]
    soc.i3.1[,j] <- xna.fun(soc[,sim.i3.1])+wsoc0.shi[,j]
  }
  

  ##fill the figure table data
  if(i < 7){
    #i = 7
    #print(i)
    #print(names(dsoc.fig[,(1:5)+(5*(i-1))]))
    soc.fig.h[,(1:5)+(5*(i-1))] <- cbind(soc.i1.1[,"h"],
                                       soc.i1.2[,"h"],
                                       soc.i1.3[,"h"],
                                       soc.i2.1[,"h"],
                                       soc.i2.2[,"h"]) 
    soc.fig.m[,(1:5)+(5*(i-1))] <- cbind(soc.i1.1[,"m"],
                                       soc.i1.2[,"m"],
                                       soc.i1.3[,"m"],
                                       soc.i2.1[,"m"],
                                       soc.i2.2[,"m"]) 
    soc.fig.l[,(1:5)+(5*(i-1))] <- cbind(soc.i1.1[,"l"],
                                       soc.i1.2[,"l"],
                                       soc.i1.3[,"l"],
                                       soc.i2.1[,"l"],
                                       soc.i2.2[,"l"]) 
  } else {
    #print(i)
    #print(names(dsoc.fig[,(1:3)+30+(3*(i-7))]))
    soc.fig.h[,((1:3)+30+(3*(i-7)))] <- cbind(soc.i3.1[,"h"],soc.i2.1[,"h"],soc.i2.2[,"h"])
    soc.fig.m[,((1:3)+30+(3*(i-7)))] <- cbind(soc.i3.1[,"m"],soc.i2.1[,"m"],soc.i2.2[,"m"]) 
    soc.fig.l[,((1:3)+30+(3*(i-7)))] <- cbind(soc.i3.1[,"l"],soc.i2.1[,"l"],soc.i2.2[,"l"]) 
  }
  
  
  for(p in 1:4){
    if(p==1){
      #p=1
      plot(1,1, col = "white", #main = c("Pirkanmaa","Kainuu","Pohjois-Pohjanmaa","","","")[i],
           frame.plot = FALSE,
           cex.axis = 1.2, cex.lab=1.3, 
           xaxt="n",
           yaxt="n",
           xlim = c(1900,2017), ylim = c(30,300), xlab = "time (years)", ylab = "SOC stock (tC/ha/y)" )
      
      axis(2, at= c(50,seq(100,400, by = 50)),labels= c(50, 100,150,200,250,300,"",400), cex.axis = 1.3)
      axis(1, at=seq(1900,2100, by = 50),labels=seq(1900,2100, by = 50), cex.axis = 1.3)
      
    }else{
      par(mar=c(1,0.7,0.3,0.1))
      plot(1,1, col = "white", #main = c("Pirkanmaa","Kainuu","Pohjois-Pohjanmaa","","","")[i],
           frame.plot = FALSE,
           cex.axis = 1.2, cex.lab=1.3, 
           xaxt="n",
           yaxt="n",
           xlim = c(2017,2080), ylim = c(30,300), 
           xlab = "time (years)", ylab = "SOC stock (tC/ha/y)" )
      
      axis(1, at=c(2017, 2030, 2050, 2070),labels=c(2017, 2030, 2050, 2070), cex.axis = 1.3)
      axis(2, at= c(50, seq(100,400, by = 50)),# mgp=c(0,-2,0),
           labels= F, cex.axis = 1.3) #, tcl = 0.5)
      
      
    }
    abline(v = 2017, col = 2, lty = 3)
    legend("topleft", legend=paste("(",tolower(c("A","B","C","D"))[p],")",sep=""), bty ="n", cex = 1.7)
    
    if(p == 1){
      #legend(1930,300,c("H.Forest","M.Forest",
      #                  "H.Crop","M.Grass"),
      #       fill = c(NA,NA,alpha(4, 0.3),alpha("grey", 0.7)), border = "white",
      #       pch = c(2,6,NA,NA), col = c(4,1,4,1), cex = 1.3, #ncol = 2,
      #       lty = c(NA,NA,2,1), lwd = c(NA,NA,1,1),bty = "n",
      #       title = "Deforestation / Cultivation")
      legend("bottomleft", legend=c("observations"),
             bty ="n", cex = 1.3,
             pch = 15, col = 2)
      legend("bottomright",legend=c("LUC 2017"), cex = 1.3,
             bty ="n", text.col = 2,  #box.lty = 3, box.col  = 2,
             col = 2)
      
    } else if (p == 3 ){
      legend("topright",c("H.Crop -> Forest","H.Crop -> Grass+Forest   ","M.Grass -> Forest"),
             fill = c(alpha(4, 0.3),alpha(4, 0.3),alpha("grey", 0.7)), border = "white",
             pch = c(17,2,2), col = c(4,4,1), cex = 1.2, #ncol = 2,
             lty = c(1,3,2), lwd = c(1,2,1),bty = "n", title = tit.leg[p])
    } else if (p == 4 ){
      legend("topright",c("H.Crop -> Forest","H.Crop -> Grass+Forest    ","M.Grass -> Forest"),
             fill = c(alpha(4, 0.3),alpha(4, 0.3),alpha("grey", 0.7)), border = "white",
             pch = c(16,1,1), col = c(4,4,1), cex = 1.2, #ncol = 2,
             lty = c(1,3,2), lwd = c(1,2,1),bty = "n", title = tit.leg[p])
    } 
    else {
      legend("top",c("H.Crop -> Grass",
                        "H.Crop BAU","M.Grass BAU"),
             fill = c(alpha(4, 0.3),alpha(4, 0.3),alpha("grey", 0.7)), 
             border = "white",
             pch = c(NA,NA,NA), col = c(4,4,1), cex = 1.2, 
             lty = c(1,2,1), lwd = c(1,1,1), bty = "n", title = tit.leg[p])
    }
    
    
    #sequence of line-points locaitons
    xpi <- round(seq(2017, 2080, length.out = 10),0)
    xpmi <- match(xpi,(2017:2100))
    #uncrertainty forest is +30% future increase in growth, -10% future decrease with droughts
    #seq(1,30, lenght.out = x)
    
    if(p == 1){
      #inittial socs
      points(1900,soc.init1h$m, pch = 2, col = 4)
      #arrows(1900, soc.init1h-soc.init1h*0.125, 1900, soc.init1h+soc.init1h*0.125, length=0.05, angle=90, code=3, col =4, lty = 2)
      arrows(1900, soc.init1h$l, 1900, soc.init1h$h, length=0.05, angle=90, code=3, col =4, lty = 2)
      
      points(1900,soc.init1m$m, pch = 6, col = 1)
      #arrows(1900, soc.init1m-soc.init1m*0.125, 1900, soc.init1m+soc.init1m*0.125, length=0.05, angle=90, code=3, col =1, lty =2)
      arrows(1900, soc.init1m$l, 1900, soc.init1m$h, length=0.05, angle=90, code=3, col =1, lty =2)
 
      # CULTIVATION period from afforestation crop.trees
      soc.h <- soc.i1.1[2:118,1]
      soc.m <- soc.i1.1[2:118,2]
      soc.l <- soc.i1.1[2:118,3]
      print(c(i,tail(soc.m,1),length(soc.m)))
      col.range <- 4#
      transp <- 0.3
      col.line <- 4#
      plotLineWithRange1(1901:2017,yVal=soc.m, yMin=soc.l, yMax=soc.h,
                         lineColor= col.line, linetype=1,
                         rangeColor= alpha(col.range, transp ),
                         main="Average")
      
      ## CULTIVATION period from afforestation grass.trees CI
      soc.h <- soc.i1.3[2:117,1]
      soc.m <- soc.i1.3[2:117,2]
      soc.l <- soc.i1.3[2:117,3]
      col.range <- "grey"
      transp <- 0.3
      col.line <- 1
      #mean CI
      plotLineWithRange1(1901:2016,yVal=soc.m, yMin=soc.l, yMax=soc.h,
                         lineColor= col.line, linetype=1,
                         rangeColor= alpha(col.range, transp ),
                         main="Average")

      #add data!!!
      #Palosuo regional data + CI (95 or 2sd)
      points(2009,soc.p$soc[1], pch = 15, col = 2)
      arrows(2009, soc.p$soc[1]-soc.p.er$soc.err[1], 2009, soc.p$soc[1]+soc.p.er$soc.err[1], length=0.05, angle=90, code=3, col =2, lty = 1)
      
      ## ADD WOODY AND NON-WOODY soc TO DEFORESTATION
      if(p== 1){
        
        wsoc.i1.1 <- soc.i1.1[complete.cases(soc.i1.1),]
        wsoc.i1.3 <- soc.i1.1[complete.cases(soc.i1.3),]
        
        nwsoc.i1.1 <- soc.i1.1[complete.cases(soc.i1.1),]
        nwsoc.i1.3 <- soc.i1.1[complete.cases(soc.i1.3),]
        
        j = 2 #mean
        sim.i1.1 <- exdat.fun("crop.trees" , reg.i, "spruce", "high", u.ci[j], "nonwoody")$sim.id 
        wsoc.i1.1[,j] <- xna.rm.fun(wsoc.hi[,j])
        nwsoc.i1.1[,j] <- xna.rm.fun(soc[,sim.i1.1])
        sim.i1.3 <- exdat.fun("grass.grasstrees" , reg.i, "spruce", "mid", u.ci[j], "nonwoody")$sim.id 
        wsoc.i1.3[,j] <- xna.rm.fun(wsoc.mi[,j])
        nwsoc.i1.3[,j] <- xna.rm.fun(soc[,sim.i1.3])

        
        lines(1901:2017, wsoc.i1.1[2:118,2],col=4, lty=2, lwd=1)
        lines(1901:2017, wsoc.i1.3[2:118,2],col=1, lty=2, lwd=1)
        
        lines(1901:2017, nwsoc.i1.1[2:118,2],col=4, lty=3, lwd=2)
        lines(1901:2017, nwsoc.i1.3[2:118,2],col=1, lty=3, lwd=2)
        
        legend("topright",c("H.Forest","M.Forest",
                       "H.Crop","M.Grass",
                       "",
                       "H.Crop woody ","M.Grass woody  ",
                       "H.Crop nonwoody  ","M.Grass nonwoody  "),
               ncol=1,
               fill = c(NA,NA,alpha(4, 0.3),alpha("grey", 0.7), NA, rep(NA,4)), 
               border = c(rep("white",4),NA,rep(NA,4)),
               pch = c(2,6,NA,NA, NA, rep(NA,4)), col = c(4,1,4,1,"white",4,1,4,1), cex = 1.3, #ncol = 2,
               lty = c(NA,NA,1,1,NA, 2,2,3,3), lwd = c(NA,NA,1,1,NA,1,1,2,2),bty = "n",
               title = "Deforestation / Cultivation")
        
      }
      
      
      
      } else if(p == 3){
      
        #afforestation crop.trees
        lines(2017:2100, xna.fun(soc.i1.1[118:201,2]), lty=1, col = 4)
        soc.i1.1 <- soc.i1.1[complete.cases(soc.i1.1),]
        n.end<- dim(soc.i1.1)[1]
        ny.end<- n.end+1900
        soc.h <- soc.i1.1[118:n.end,1]
        soc.m <- soc.i1.1[118:n.end,2]
        soc.l <- soc.i1.1[118:n.end,3]
        col.range <- 4#
        transp <- 0.3
        col.line <- 4#
        ##afforestation crop.grasstrees CI
        plotLineWithRange1(2017:(ny.end-1),yVal=soc.m, yMin=soc.l, yMax=soc.h,
                           lineColor= col.line, linetype=3,
                           rangeColor= alpha(col.range, transp ),
                           main="Average")
        
        #afforestation crop.grasstrees
        lines(2017:2100, xna.fun(soc.i1.2[118:201,2]), lty=3, col = 4)
        soc.i1.2 <- soc.i1.2[complete.cases(soc.i1.2),]
        n.end<- dim(soc.i1.2)[1]
        ny.end<- n.end+1900
        soc.h <- soc.i1.2[118:n.end,1]
        soc.m <- soc.i1.2[118:n.end,2]
        soc.l <- soc.i1.2[118:n.end,3]
        col.range <- 4#
        transp <- 0.3
        col.line <- 4#
        ##afforestation crop.grasstrees CI
        plotLineWithRange1(2017:(ny.end-1),yVal=soc.m, yMin=soc.l, yMax=soc.h,
                           lineColor= col.line, linetype=3,
                           rangeColor= alpha(col.range, transp ),
                           main="Average")
        
        
        #afforestation grass.trees
        lines(2017:2100, xna.fun(soc.i1.3[118:201,2]), lty=2, col = 1)
        soc.i1.3 <- soc.i1.3[complete.cases(soc.i1.3),]
        n.end<- dim(soc.i1.3)[1]
        ny.end<- n.end+1900
        soc.h <- soc.i1.3[118:n.end,1]
        soc.m <- soc.i1.3[118:n.end,2]
        soc.l <- soc.i1.3[118:n.end,3]
        col.range <- "grey"
        transp <- 0.3
        col.line <- 1
        ##afforestation grass.trees CI
        plotLineWithRange1(2017:(ny.end-1),yVal=soc.m, yMin=soc.l, yMax=soc.h,
                           lineColor= col.line, linetype=2,
                           rangeColor= alpha(col.range, transp ),
                           main="Average")  
      # SPRUCE
      #afforestation crop.trees
      lines(2017:2100, xna.fun(soc.i1.1[118:201,2]), lty=1, col = 4)
      points((2017:2100)[xpmi], (xna.fun(soc.i1.1[118:201,2]))[xpmi], pch =17, col = 4)
      #afforestation crop.grasstrees
      lines(2017:2100, xna.fun(soc.i1.2[118:201,2]), lty=3, col = 4)
      points((2017:2100)[xpmi], (xna.fun(soc.i1.2[118:201,2]))[xpmi], pch =2, col = 4)
      #afforestation grass.trees
      lines(2017:2100, xna.fun(soc.i1.3[118:201,2]), lty=2, col = 1)
      points((2017:2100)[xpmi], (xna.fun(soc.i1.3[118:201,2]))[xpmi], pch =2, col = 1)
      
    } else if(p == 4){
      #afforestation crop.trees
      lines(2017:2100, xna.fun(soc.bi1.1[118:201,2]), lty=1, col = 4)
      soc.bi1.1 <- soc.bi1.1[complete.cases(soc.bi1.1),]
      n.end<- dim(soc.bi1.1)[1]
      ny.end<- n.end+1900
      soc.h <- soc.bi1.1[118:n.end,1]
      soc.m <- soc.bi1.1[118:n.end,2]
      soc.l <- soc.bi1.1[118:n.end,3]
      col.range <- 4#
      transp <- 0.3
      col.line <- 4#
      ##afforestation crop.grasstrees CI
      plotLineWithRange1(2017:(ny.end-1),yVal=soc.m, yMin=soc.l, yMax=soc.h,
                         lineColor= col.line, linetype=3,
                         rangeColor= alpha(col.range, transp ),
                         main="Average")
      
      #afforestation crop.grasstrees
      lines(2017:2100, xna.fun(soc.bi1.2[118:201,2]), lty=3, col = 4)
      soc.bi1.2 <- soc.bi1.2[complete.cases(soc.bi1.2),]
      n.end<- dim(soc.bi1.2)[1]
      ny.end<- n.end+1900
      soc.h <- soc.bi1.2[118:n.end,1]
      soc.m <- soc.bi1.2[118:n.end,2]
      soc.l <- soc.bi1.2[118:n.end,3]
      col.range <- 4#
      transp <- 0.3
      col.line <- 4#
      ##afforestation crop.grasstrees CI
      plotLineWithRange1(2017:(ny.end-1),yVal=soc.m, yMin=soc.l, yMax=soc.h,
                         lineColor= col.line, linetype=3,
                         rangeColor= alpha(col.range, transp ),
                         main="Average")
      
      
      #afforestation grass.trees
      lines(2017:2100, xna.fun(soc.bi1.3[118:201,2]), lty=2, col = 1)
      soc.bi1.3 <- soc.bi1.3[complete.cases(soc.bi1.3),]
      n.end<- dim(soc.bi1.3)[1]
      ny.end<- n.end+1900
      soc.h <- soc.bi1.3[118:n.end,1]
      soc.m <- soc.bi1.3[118:n.end,2]
      soc.l <- soc.bi1.3[118:n.end,3]
      col.range <- "grey"
      transp <- 0.3
      col.line <- 1
      ##afforestation grass.trees CI
      plotLineWithRange1(2017:(ny.end-1),yVal=soc.m, yMin=soc.l, yMax=soc.h,
                         lineColor= col.line, linetype=2,
                         rangeColor= alpha(col.range, transp ),
                         main="Average") 
      
      #BIRCh
      #afforestation crop.trees
      lines(2017:2100, xna.fun(soc.bi1.1[118:201,2]), lty=1, col = 4)
      points((2017:2100)[xpmi], (xna.fun(soc.bi1.1[118:201,2]))[xpmi], pch =16, col = 4)
      
      #afforestation crop.grasstrees
      lines(2017:2100, xna.fun(soc.bi1.2[118:201,2]), lty=3, col = 4)
      points((2017:2100)[xpmi], (xna.fun(soc.bi1.2[118:201,2]))[xpmi], pch =1, col = 4)
      
      
      #afforestation grass.trees
      lines(2017:2100, xna.fun(soc.bi1.3[118:201,2]), lty=2, col = 1)
      points((2017:2100)[xpmi], (xna.fun(soc.bi1.3[118:201,2]))[xpmi], pch =1, col = 1)
      
      
    } else{
      
      #extensification crop.grass
      lines(2017:2100, xna.fun(soc.i3.1[118:201,2]), lty=1, col = 4)
      soc.i3.1 <- soc.i3.1[complete.cases(soc.i3.1),]
      n.end<- dim(soc.i3.1)[1]
      ny.end<- n.end+1900
      soc.h <- soc.i3.1[118:n.end,1]
      soc.m <- soc.i3.1[118:n.end,2]
      soc.l <- soc.i3.1[118:n.end,3]
      col.range <- 4
      transp <- 0.3
      col.line <- 4
      # CI
      plotLineWithRange1(2017:(ny.end-1),yVal=soc.m, yMin=soc.l, yMax=soc.h,
                         lineColor= col.line, linetype=1,
                         rangeColor= alpha(col.range, transp ),
                         main="Average")
      
      #BAU crop
      soc.i2.1 <- soc.i2.1[complete.cases(soc.i2.1),]
      n.end<- dim(soc.i2.1)[1]
      ny.end<- n.end+1900
      soc.h <- soc.i2.1[118:n.end,1]
      soc.m <- soc.i2.1[118:n.end,2]
      soc.l <- soc.i2.1[118:n.end,3]
      col.range <- 4
      transp <- 0.3
      col.line <- 4
      lines(2017:(ny.end-1), soc.m, lty=2, col = 4)
      # CI
      plotLineWithRange1(2017:(ny.end-1),yVal=soc.m, yMin=soc.l, yMax=soc.h,
                         lineColor= col.line, linetype=2,
                         rangeColor= alpha(col.range, transp ),
                         main="Average")
      #BAU grass
      soc.i2.2 <- soc.i2.2[complete.cases(soc.i2.2),]
      n.end<- dim(soc.i2.2)[1]
      ny.end<- n.end+1900
      soc.h <- soc.i2.2[118:n.end,1]
      soc.m <- soc.i2.2[118:n.end,2]
      soc.l <- soc.i2.2[118:n.end,3]
      col.range <- "grey"
      transp <- 0.3
      col.line <- 1
      lines(2017:(ny.end-1), soc.m, lty=1, col = 1)
      # CI
      plotLineWithRange1(2017:(ny.end-1),yVal=soc.m, yMin=soc.l, yMax=soc.h,
                         lineColor= col.line, linetype=1,
                         rangeColor= alpha(col.range, transp ),
                         main="Average")
      
      
    }
  }
}
mtext( expression("SOC stock" ~~ "( tC" ~ "ha"^{-1} ~ ")"), #"y"^{-1}~
       2, out = T, line = 1.5, cex = 1.2)
mtext( "Time (year)", 
       1, out = T, line = 1.7, cex = 1.2)

#dev.off()

