## OPAL plots all the SOC simulations with CI (confidence intervals) ##################
#regions, species, fetility (high, mid)
#management afforestation, extensisfication, business as usual
#boris Jan 2020


## Figure SOCs LUCs Supplement # 10.01.20 add CI ############################
library(scales) #for color transparency
#clim.mean
#3 regions, spruce, mt, omt, grass-crop.m,l,h 
#par(mfrow=c(3,3),mar=c(5,5,3,1), oma= c(0,0,0,1))
par(mfrow=c(3,3),mar=c(1,1,0.3,0.3), oma= c(4,4,2,2))
for(i in 1:9){ #panels
  #i = 2
  reg.i <- rep(c("Pirkanmaa","Kainuu","Pohjois-Pohjanmaa"),3)[i]
  spec.i <- rep(c("spruce", "birch","spruce"),each =3)[i]
  spec.n.i <- rep(c(1, 2, 1),each =3)[i]
  
  if(i == 1){
    tit.leg <- c("Afforestation (SPRUCE):")
  } else if (i == 4){
    tit.leg <- c("Afforestation (BIRCH):")  
  }
  
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
  #woody highly fertile #ci high mean low
  wsim.hi.h <- exdat.fun("affor", reg.i, spec.i, "high", "high", "woody")$sim.id #set fixed inputs by words
  wsim.hi.m <- exdat.fun("affor", reg.i, spec.i, "high", "mean", "woody")$sim.id #set fixed inputs by words
  wsim.hi.l<- exdat.fun("affor", reg.i, spec.i, "high", "low", "woody")$sim.id #set fixed inputs by words
  wsim.hi <- c(wsim.hi.h,wsim.hi.m,wsim.hi.l)
  wsoc.hi <- soc[,wsim.hi]
  #woody mid fertile #ci high mean low
  wsim.mi.h <- exdat.fun("affor", reg.i, spec.i, "mid", "high", "woody")$sim.id #set fixed inputs by words
  wsim.mi.m <- exdat.fun("affor", reg.i, spec.i, "mid", "mean", "woody")$sim.id #set fixed inputs by words
  wsim.mi.l <- exdat.fun("affor", reg.i, spec.i, "mid", "low", "woody")$sim.id #set fixed inputs by words
  wsim.mi <- c(wsim.mi.h,wsim.mi.m,wsim.mi.l)
  wsoc.mi <- soc[,wsim.mi]
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
    #non-woody
    #1.1 Afforestation crop to trees (NO grass) u.mang: "crop.trees"
    sim.i1.1 <- exdat.fun("crop.trees" , reg.i, spec.i, "high", u.ci[j], "nonwoody")$sim.id 
    soc.i1.1[,j] <- xna.fun(soc[,sim.i1.1])+wsoc.hi[,j]
    #1.2 Afforestation crop to trees (ON/with grass) u.mang: "crop.grasstrees"
    sim.i1.2 <- exdat.fun("crop.grasstrees" , reg.i, spec.i, "high", u.ci[j], "nonwoody")$sim.id 
    soc.i1.2[,j] <- xna.fun(soc[,sim.i1.2])+wsoc.hi[,j]
    #1.3 Afforestation grass to trees (with grass) u.mang: "grass.grasstrees"
    sim.i1.3 <- exdat.fun("grass.grasstrees" , reg.i, spec.i, "mid", u.ci[j], "nonwoody")$sim.id 
    soc.i1.3[,j] <- xna.fun(soc[,sim.i1.3])+wsoc.mi[,j]
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
  
  plot(1,1, col = "white", #main = c("Pirkanmaa","Kainuu","Pohjois-Pohjanmaa","","","")[i],
       cex.axis = 1.2, cex.lab=1.3, 
       xaxt="n",
       yaxt="n",
       xlim = c(1900,2100), ylim = c(70,400), xlab = "time (years)", ylab = "SOC stock (tC/ha/y)" )
  if(i == 1 | i == 4 ){
    axis(2, at= seq(100,400, by = 50),labels= c(100,"",200,"",300,"",400), cex.axis = 1.3)
    axis(1, at=seq(1900,2100, by = 50),labels=F, cex.axis = 1.3)
  } else if(i == 7){
    axis(2, at= seq(100,400, by = 50),labels= c(100,"",200,"",300,"",400), cex.axis = 1.3)
    axis(1, at=seq(1900,2100, by = 50),labels=seq(1900,2100, by = 50), cex.axis = 1.3)
  }else if(i > 7){
    axis(1, at=seq(1900,2100, by = 50),labels=seq(1900,2100, by = 50), cex.axis = 1.3)
    axis(2, at= seq(100,400, by = 50),labels= F, cex.axis = 1.3)
  } else{
    axis(2, at= seq(100,400, by = 50),labels= F, cex.axis = 1.3)
    axis(1, at=seq(1900,2100, by = 50),labels=F, cex.axis = 1.3)
  }
  if(i < 4){
    #mtext(c("Pirkanmaa","Kainuu","Pohjois-Pohjanmaa")[i], 3, line = 0.3, cex = 1.2)
    #replace region names by geographical names
    mtext(c("South","North East","North West")[i], 3, line = 0.3, cex = 1.2)
    
  }
  
  abline(v = 2017, col = 2, lty = 3)
  legend("topright", legend=paste("(",tolower(c("A","B","C","D", "E", "F","G", "H", "I"))[i],")",sep=""), bty ="n", cex = 1.7)
  
  if(i == 1 | i == 4){
    legend("top",c("H.Forest","M.Forest",
                   "H.Crop","M.Grass",
                   "H.Crop -> Forest","H.Crop -> Grass+Forest","M.Grass -> Forest"),
           fill = c(NA,NA,alpha(4, 0.3),alpha("grey", 0.7),alpha(4, 0.3),alpha(4, 0.3),alpha("grey", 0.7)), border = "white",
           pch = c(2,6,NA,NA,NA,NA,NA), col = c(4,1,4,1,4,4,1), cex = 1.2, #ncol = 2,
           lty = c(NA,NA,2,1,1,3,2), lwd = c(NA,NA,1,1,1,2,1),bty = "n", title = tit.leg)
    
    legend("bottomleft", legend=c("observations"), bty ="n", cex = 1.3,
           pch = 15, col = 2)
    
    
  } else if (i == 7){
    legend("top",c("H.Forest","M.Forest",
                   "H.Crop (BAU)","M.Grass (BAU)",
                   "H.Crop -> Grass"),
           fill = c(NA,NA,alpha(4, 0.3),alpha("grey", 0.7),alpha(4, 0.3)), border = "white",
           pch = c(2,6,NA,NA,NA), col = c(4,1,4,1,4), cex = 1.2, 
           lty = c(NA,NA,2,1,1), lwd = c(NA,NA,1,1,1), bty = "n", title = "Extensification:")
    
    legend("bottomleft", legend=c("observations"), bty ="n", cex = 1.3,
           pch = 15, col = 2)
  }
  
  
  #MAKE FUNCTION for plotting this!!!
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
                     lineColor= col.line, linetype=2,
                     rangeColor= alpha(col.range, transp ),
                     main="Average")
  
  ## CULTIVATION period from afforestation grass.trees CI
  soc.h <- soc.i1.3[2:117,1]
  soc.m <- soc.i1.3[2:117,2]
  soc.l <- soc.i1.3[2:117,3]
  col.range <- "grey"
  transp <- 0.3
  col.line <- 1
  plotLineWithRange1(1901:2016,yVal=soc.m, yMin=soc.l, yMax=soc.h,
                     lineColor= col.line, linetype=1,
                     rangeColor= alpha(col.range, transp ),
                     main="Average")
  
  if(i < 7){
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
  
  #add afforestation data!!!
  #Palosuo regional data + CI (95 or 2sd)
  if(i == 1|i == 4|i == 7){
    points(2009,soc.p$soc[1], pch = 15, col = 2)
    arrows(2009, soc.p$soc[1]-soc.p.er$soc.err[1], 2009, soc.p$soc[1]+soc.p.er$soc.err[1], length=0.05, angle=90, code=3, col =2, lty = 1)
  } else if(i == 2|i == 5|i == 8){
    points(2009,soc.p$soc[2], pch = 15, col = 2)
    arrows(2009, soc.p$soc[2]-soc.p.er$soc.err[2], 2009, soc.p$soc[2]+soc.p.er$soc.err[2], length=0.05, angle=90, code=3, col =2, lty = 1)
  } else{
    points(2009,soc.p$soc[3], pch = 15, col = 2)
    arrows(2009, soc.p$soc[3]-soc.p.er$soc.err[3], 2009, soc.p$soc[3]+soc.p.er$soc.err[3], length=0.05, angle=90, code=3, col =2, lty = 1)
  }
  
  
}
mtext( expression("SOC stock" ~~ "( Mg C" ~ "ha"^{-1} ~ ")"), #"y"^{-1}~
       2, out = T, line = 1.5, cex = 1.2)
mtext( "Time (year)", 
       1, out = T, line = 1.7, cex = 1.2)

