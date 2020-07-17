# MSFIG.3 SOC CHANGE ################################################
#par(mfrow=c(1,4),mar=c(1,2,0.3,0.3), oma= c(4,4,2,2))
layout(matrix(1:4, nrow=1, ncol=4), heights= c(1.7,1,1.7,1,1.7,1,1.7,1)) 
par(mar=c(1,2,2,1), oma=c(3,4,1,2)) 

#DEFORESTATION / cumuative
plot(1,1, col = "white", 
     cex.axis = 1.3, cex.lab=1.3, frame.plot = FALSE,
     xaxt="n",
     #yaxt="n",
     xlim = c(1900,2017), ylim = c(-130, 0), xlab = "Time (year)", ylab = "Change in SOC stock (tC/ha/y)" )
axis(1, at=seq(1900,2020, by = 20),labels= T, #seq(2020,2100, by = 20), 
     cex.axis = 1.3)
par(xpd=F)
#abline(h = 0, col = 2, lty = 1)
#labels
for(i in 1){ #regions
  #i = 1
  reg.i <- c("Pirkanmaa","Kainuu","Pohjois-Pohjanmaa")[i]
  #spec.i <- rep(c("spruce", "birch","spruce"),each =3)[i]
  #spec.n.i <- rep(c(1, 2, 1),each =3)[i]
  
  #cummulative change
  #woody
  dwsim.hi <- exdat.fun("affor", reg.i, "spruce", "high", "mean", "woody")$sim.id #set fixed inputs by words
  dwsoc.hi <- d.soc1[,dwsim.hi]
  dwsim.mi <- exdat.fun("affor", reg.i, "spruce", "mid", "mean", "woody")$sim.id 
  dwsoc.mi <- d.soc1[,dwsim.mi]
  #non-woody
  dnwsim.hi <- exdat.fun("crop.trees", reg.i, "spruce", "high", "mean", "nonwoody")$sim.id #set fixed inputs by words
  dnwsoc.hi <- d.soc1[,dnwsim.hi]
  dnwsim.mi <- exdat.fun("grass.grasstrees", reg.i, "spruce", "mid", "mean", "nonwoody")$sim.id 
  dnwsoc.mi <- d.soc1[,dnwsim.mi]
  #tot 
  dsoc.hi <- dwsoc.hi+dnwsoc.hi
  dsoc.mi <- dwsoc.mi+dnwsoc.mi
  
  #cummulative mean change per year
  dmysoc.hi <- dsoc.hi/(1:201)
  dmysoc.mi <- dsoc.mi/(1:201)
  
  #change per year
  dysoc.hi <- diff(dsoc.hi)#/(1:201)
  dysoc.mi <- diff(dsoc.mi)#/(1:201)
  
  mean(diff(dsoc.hi)[1:20])
  mean(diff(dsoc.mi)[1:20])
  min(diff(dsoc.hi)[1:20])
  min(diff(dsoc.mi)[1:20])
  
  mean(diff(dsoc.hi)[90:116])
  mean(diff(dsoc.mi)[90:116])
  
  mean(dysoc.hi[90:116])
  mean(dysoc.mi[90:116])
  
  #decomposition rate after deforestation
  dsoc.crop0 <- data.frame(y=(1900:2017)[seq(1,20, by = 1)], dsoc=(dsoc.hi[1:118])[seq(1,20, by = 1)])
  dsoc.crop0$y1 <- dsoc.crop0$y-1900
  dsoc.crop.mod0 <- lm(dsoc~y1, data = dsoc.crop0)
  summary(dsoc.crop.mod0)
  
  dsoc.grass0 <- data.frame(y=(1900:2017)[seq(1,20, by = 1)], dsoc=(dsoc.mi[1:118])[seq(1,20, by = 1)])
  dsoc.grass0$y1 <- dsoc.grass0$y-1989
  dsoc.grass.mod0 <- lm(dsoc~y1, data = dsoc.grass0)
  summary(dsoc.grass.mod0)
  
  #decomposition rate for comparison with Karhu
  dsoc.crop <- data.frame(y=(1900:2017)[seq(91,118, by = 1)], dsoc=(dsoc.hi[1:118])[seq(91,118, by = 1)])
  dsoc.crop$y1 <- dsoc.crop$y-1989
  dsoc.crop.mod <- lm(dsoc~y1, data = dsoc.crop)
  summary(dsoc.crop.mod)
  
  
  dsoc.grass <- data.frame(y=(1900:2017)[seq(91,118, by = 1)], dsoc=(dsoc.mi[1:118])[seq(91,118, by = 1)])
  dsoc.grass$y1 <- dsoc.grass$y-1989
  dsoc.grass.mod <- lm(dsoc~y1, data = dsoc.grass)
  summary(dsoc.grass.mod)
  
  #plot(dsoc.crop$y1,dsoc.crop$dsoc, type = "b", ylim = c(-150,-50))
  #points(dsoc.grass$y1,dsoc.grass$dsoc, col = 4, type = "b")
  
  #cultivation crop
  lines(1900:2017, dsoc.hi[1:118], lty=2, col = 4) #c(4,1,2)[i])
  #points((1900:2017)[seq(1,120, by = 10)], (dsoc.hi[1:118])[seq(1,120, by = 10)], col = c(4,1,2)[i], pch = c(15,16,17)[i])
  #cultivation grass
  lines(1900:2017, dsoc.mi[1:118], lty=1, col = 1) #c(4,1,2)[i])
  #points((1900:2017)[seq(1,120, by = 10)], (dsoc.mi[1:118])[seq(1,120, by = 10)], col = c(4,1,2)[i], pch = c(0,1,2)[i])
  
  
}
abline(v = 1900, col = 2, lty = 3, lwd=2)
par(xpd=T)
legend("top",#1920,27,#
       legend=c("H.Crop", "M.Grass"),
       col = c(4,1), cex = 1.3, 
       lty = c(2,1), bty = "n", title = "Cultivation   ")
legend(1880,15, legend="(a)", bty ="n", cex = 1.7)

#plot DEFORESTATION data Karhu et al. 2011
#assumption!!!! 
# sites roughly correspond to pirkanmaa
# deforestation happend in 1990 with forest stocks -100 tons from equilibrium
x <- as.numeric(soc.kd.y)+1990+c(0,0,0,0,0.7) #1900
y <- as.numeric(dc.kd.mean[2,]) -100 #assumed that forest now are not in steady state, but lost 100 tons through management
y.sd <- as.numeric(soc.kd.sd[2,])
points(x,y, pch = 18, col = 4, cex = 1.3) #alpha(1, 0.7 ))
arrows(x, y-y.sd, x, y+y.sd, length=0.05, angle=90, code=3, 
       col = 4, # alpha(1, 0.7 ),
       lty = 1)
#abline(0,as.numeric(coef(df.soc.mod)), col = "grey")
lines(((-1:30)+1990),as.numeric(coef(df.soc.mod))*(-1:30)-100, col = 4, lwd = 2) #
#zero
x0 <- 1990+c(0,0.3,0,6,0.9)
y0 <- rep(-100,5)
y0.sd <- as.numeric(soc.kd.sd[1,])
points(x0,y0, pch = 18, col =4 , cex = 1.3) #alpha(1, 0.7 ))
arrows(x0, y0-y0.sd, x0, y0+y0.sd, length=0.05, angle=90, code=3, lty = 1, col = 4) #alpha(1, 0.7 ))
#karhu 2011 deforestation model
summary(df.soc.mod)
legend("bottomleft", bty = "n",
       c("experimental data"," y = -0.67*year, \n R2 = 0.58, p = 0.048"),
       pch=c(18,NA), cex = 1.2, lty = c(NA, 1), lwd = c(NA, 2), col = 4, text.col = 4)
mtext( expression(Sigma ~ Delta ~ "SOC" ~~ "( tC" ~ "ha"^{-1} ~ ")"),
       2, out = F, line = 3, cex = 1.2)

dsoc.hi[118]
dsoc.mi[118]

range(dysoc.hi[1:118], na.rm=T)
range(dysoc.mi[1:118], na.rm=T)

as.numeric(dysoc.hi[1:118])

dmysoc.hi[118]
dmysoc.mi[118]

p= 10
if(p == 1000){
#DEFORESTATION / annual change
par(mar=c(2,2,0.1,1))
plot(1,1, col = "white", 
     cex.axis = 1.3, cex.lab=1.3, frame.plot = FALSE,
     #xaxt="n",
     #yaxt="n",
     xlim = c(1900,2017), ylim = c(-3.0, 0.5),
     xlab = "Time (year)", ylab = "Change in SOC stock (tC/ha/y)" )
#axis(4, at= seq(-200,0, by = 50),labels= F, cex.axis = 1.3, tck = 0.03)
par(xpd=F)
abline(v = 1900, col = 2, lty = 3)
abline(h = 0, col = 2, lty = 1)
mtext( expression( Delta ~ bar(SOC) ~~ "( tC" ~ "ha"^{-1} ~ "year"^{-1} ~")"),
       2, out = F, line = 3, cex = 1.2)
#cultivation crop
lines(1900:2017, dmysoc.hi[1:118], lty=2, col = 4) #c(4,1,2)[i])
#cultivation grass
lines(1900:2017, dmysoc.mi[1:118], lty=1, col = 1) #c(4,1,2)[i])
par(xpd=T)
legend(1880,0.7, legend="(e)", bty ="n", cex = 1.7)
}

# MS.FIG.2 SIMPLIFIED panels 
par(mar=c(2,2,0.3,0.3))
# EXTENSIFICATION, AFFORESTATION SPRUCE, AFFORESTATION BIRCH
for(i in c(7, 1,4)){ #panels
  #i = 4 #spruce 1, #birch 4
  reg.i <- rep(c("Pirkanmaa","Kainuu","Pohjois-Pohjanmaa"),3)[i]
  spec.i <- rep(c("spruce", "birch","spruce"),each =3)[i]
  spec.n.i <- rep(c(1, 2, 1),each =3)[i]
  
  if(i == 1){
    tit.leg <- c("Afforestation SPRUCE")
  } else if (i == 4){
    tit.leg <- c("Afforestation BIRCH")  
  }
  
  soc.i1.1 <- data.frame(matrix(NA,length(118:201),3))
  soc.i1.2 <- data.frame(matrix(NA,length(118:201),3))
  soc.i1.3 <- data.frame(matrix(NA,length(118:201),3))
  names(soc.i1.1) <- c("h","m","l")
  names(soc.i1.2) <- c("h","m","l")
  names(soc.i1.3) <- c("h","m","l")
  #woody
  wsim.hi <- exdat.fun("affor", reg.i, spec.i, "high", "mean", "woody")$sim.id #set fixed inputs by words
  wsoc.hi <- d.soc2[,wsim.hi]
  wsim.mi <- exdat.fun("affor", reg.i, spec.i, "mid", "mean", "woody")$sim.id 
  wsoc.mi <- d.soc2[,wsim.mi]
  
  #woody for spruce equilibrium forest and ZERO litter BAU
  wsim0.shi <- exdat.fun("cultiv", reg.i, "spruce", "high", "mean", "woody")$sim.id #set fixed inputs by words
  wsoc0.shi <- d.soc2[,wsim0.shi]
  wsim0.smi <- exdat.fun("cultiv", reg.i, "spruce", "mid", "mean", "woody")$sim.id 
  wsoc0.smi <- d.soc2[,wsim0.smi]
  
  
  for(j in 1 : 3){ # c("high","mean","low") confidence interval for agricultural data
    # j = 1
    #non-woody
    #1.1 Afforestation crop to trees (NO grass) u.mang: "crop.trees"
    sim.i1.1 <- exdat.fun("crop.trees" , reg.i, spec.i, "high", u.ci[j], "nonwoody")$sim.id 
    sim.names[sim.i1.1,]
    soc.i1.1[,j] <- d.soc2[,sim.i1.1]+wsoc.hi
    #1.2 Afforestation corp to trees (ON/with grass) u.mang: "crop.grasstrees"
    sim.i1.2 <- exdat.fun("crop.grasstrees" , reg.i, spec.i, "high", u.ci[j], "nonwoody")$sim.id 
    soc.i1.2[,j] <- d.soc2[,sim.i1.2]+wsoc.hi
    #1.3 Afforestation grass to trees (with grass) u.mang: "grass.grasstrees"
    sim.i1.3 <- exdat.fun("grass.grasstrees" , reg.i, spec.i, "mid", u.ci[j], "nonwoody")$sim.id 
    soc.i1.3[,j] <- d.soc2[,sim.i1.3]+wsoc.mi
    
  }
  #initial forest soc (mean), note! rerun spinups for sensitivity 25%
  soc.init1h <- soc.i1.1[1,2] #same as # soc.i1.2[1]
  #soc.init sensitivity
  soc.init1m <- soc.i1.3[1,2]
  
  
  #when SOC positive?
  head(subset( soc.i1.1, m > 0 ))
  head(subset( soc.i1.2, m > 0 ))
  head(subset( soc.i1.3, m > -0.50 ))
  
 tail(subset( soc.i1.1, !is.na(m ) ))
 tail(subset( soc.i1.2,  !is.na(m ) ))
 tail(subset( soc.i1.3,  !is.na(m ) ))
  
  
  #BAU business as usual
  u.mang
  #2.1 crop.bau
  sim.i2.1 <- exdat.bau.fun("crop.bau" , reg.i, "mean", "nonwoody")$sim.id [spec.n.i]
  sim.names[sim.i2.1,]
  soc.i2.1 <- d.soc2[,sim.i2.1]
  #2.2 grass.bau
  sim.i2.2 <- exdat.bau.fun("grass.bau" , reg.i, "mean", "nonwoody")$sim.id [spec.n.i]
  soc.i2.2 <- d.soc2[,sim.i2.2]
  #add wody soc part remaining from the equilibrium forest (note this is only technical solution)
  soc.i2.1 <- xna.fun(d.soc2[,sim.i2.1])+wsoc0.shi 
  soc.i2.2 <- xna.fun(d.soc2[,sim.i2.2])+wsoc0.smi 
  
  #EXTENSIFICATION
  u.mang
  #3.1 crop.extensification
  sim.i3.1 <- exdat.bau.fun("crop.extensif", reg.i, "mean", "nonwoody")$sim.id [1]
  sim.names[sim.i3.1,]
  soc.i3.1 <- xna.fun(d.soc2[,sim.i3.1])+wsoc0.shi 
  
  ##fill the figure table data
  if(i < 7){
    #i = 7
    #print(i)
    #print(names(dsoc.fig[,(1:5)+(5*(i-1))]))
    dsoc.fig[,(1:5)+(5*(i-1))] <- cbind(soc.i1.1[,"m"],soc.i1.2[,"m"],soc.i1.3[,"m"],soc.i2.1,soc.i2.2) 
  } else {
    #print(i)
    #print(names(dsoc.fig[,(1:3)+30+(3*(i-7))]))
    dsoc.fig[,((1:3)+30+(3*(i-7)))] <- cbind(soc.i3.1,soc.i2.1,soc.i2.2) 
  }
  
  #read Afforestation data!!!
  #Karhu site data difference + (1sd)
  #spruce
  if(i == 1){ #ks3.mean (sites in pirkanmaa)
    x=c(2017,2025,2033)
    y=dc.kbs.mean$ks3.mean
    y.sd =soc.kbs.sd$ks3.sd
    #ks4.mean 
    x1=c(2017,2025,2033)+0.7
    y1=dc.kbs.mean$ks4.mean
    y1.sd =soc.kbs.sd$ks4.sd
  } 
  if(i == 3){ #ks5.mean (sites in pohjoisphjanmaa)
    x=c(2017,2025,2033)
    y=dc.kbs.mean$ks5.mean
    y.sd =soc.kbs.sd$ks5.sd
    #ks4.mean 
    x1=c(2017,2025,2033)+0.7
    y1=dc.kbs.mean$ks6.mean
    y1.sd =soc.kbs.sd$ks6.sd
  }
  #birch
  if(i == 4){ 
    x=c(2017,2025,2033)
    y=dc.kbs.mean$kb3.mean
    y.sd =soc.kbs.sd$kb3.sd
    #ks4.mean 
    x1=c(2017,2025,2033)+0.7
    y1=dc.kbs.mean$kb4.mean
    y1.sd =soc.kbs.sd$kb4.sd
    
  } 
  if(i == 6){
    x=c(2017,2025,2033)
    y=dc.kbs.mean$kb6.mean
    y.sd =soc.kbs.sd$kb6.sd
    
  }
  
  
  #CUMMULATIVE SUM OR ANNUAL CHANGE##
  for(k in 1){#1:2
    #k = 1
    if (k == 1){ #PLOT CUMMULATIVE SUM
      ylim.i <-  c(-30,rep(c(30,30,30),each =3)[i])
      par(xpd=F)
      par(mar=c(2,2,2,0.3)) 
      plot(1,1, col = "white", #main = c("Pirkanmaa","Kainuu","Pohjois-Pohjanmaa","","","")[i],
           cex.axis = 1.2, cex.lab=1.3,  
           frame.plot = FALSE,
           xaxt="n",
           yaxt="n",
           xlim = c(2010,2080), ylim = ylim.i, xlab = "time (years)", ylab = "SOC stock (tC/ha/y)" )
      if(i == 1 ){ #| i == 4 
        axis(2, at= seq(-40,40, by = 10),labels= T, cex.axis = 1.3)
        axis(1, at=seq(2020,2100, by = 20),labels= T, #seq(2020,2100, by = 20),
             cex.axis = 1.3)
      } 
      if( i == 4 ){
        axis(2, at= seq(-40,40, by = 10),labels= T, cex.axis = 1.3)
        axis(1, at=seq(2020,2100, by = 20),labels= T, #seq(2020,2100, by = 20), 
             cex.axis = 1.3)
      }
      if(i == 7){
        axis(2, at= seq(-40,40, by = 10),labels= T, cex.axis = 1.3)
        axis(1, at=seq(2020,2100, by = 20),labels= T, #seq(2020,2100, by = 20), 
             cex.axis = 1.3)
      }
      par(xpd=F)
      abline(v = 2017, col = 2, lty = 3, lwd=2)
      abline(h = 0, col = 1, lty = 1)
      
      par(xpd=T)
      legend(2000, 37, legend=paste("(",tolower(c("c","","","D", "", "","B", "H", "I"))[i],")",sep=""), bty ="n", cex = 1.7)
      
      if(i == 1 | i == 4){
        if(i ==1){
          pchi =c(17,2,2)
        }else if (i == 4){
          pchi =c(16,1,1)
        }
        legend("topright",#"topleft", 2020,37,
               c("H.Crop -> Forest   ","H.Crop -> Grass+Forest    ","M.Grass -> Forest    "),
               ncol=1,
               pch = pchi, col = c(4,4,1), cex = 1.3, #ncol = 2,
               lty = c(1,3,2), lwd = c(1,2,1),bty = "n", title = tit.leg)
      } else if (i == 7){
        legend("top", #2020,37,#
               c("H.Crop -> Grass  ","H.Crop BAU  ","M.Grass BAU   "),
               #ncol=2,
               pch = c(NA,NA,NA), col = c(4,4,1), cex = 1.3, 
               lty = c(1,2,1), lwd = c(1,1,1), bty = "n", title = "Extensification ")
      }
      
      if(i == 1 ){#| i == 3 
        legend(2033,-20, legend=c("site 1", "site 2"), bty ="n", cex = 1.2,
               horiz = T, pch = c(0,5), col = 4, text.col = 4, 
               title = "experimental data \n H.Crop->Forest SPRUCE  ",title.col = 4)
      }
      if(i == 4){#| i == 3 
        legend(2033,-20, legend=c("site 1", "site 2"), bty ="n", cex = 1.2,
               horiz = T, pch = c(0,5), col = 4, text.col = 4, 
               title = "experimental data \n H.Crop->Forest BIRCH  ",title.col = 4)
      }
      
      #plot afforestation data Karhu et al. 2011
      if(i == 1 | i == 3 |i == 4 ){
        points(x,y, pch = 0, col =alpha(4, 0.7 ))
        arrows(x, y-y.sd, x, y+y.sd, length=0.05, angle=90, code=3, col =alpha(4, 0.7 ), lty = 1)
        points(x1,y1, pch = 5, col =alpha(4, 0.7 ))
        arrows(x1, y1-y1.sd, x1, y1+y1.sd, length=0.05, angle=90, code=3, col =alpha(4, 0.7 ), lty = 1)
      }
      if(i == 6 ){
        points(x,y, pch = 0, col =alpha(4, 0.7 ))
        arrows(x, y-y.sd, x, y+y.sd, length=0.05, angle=90, code=3, col =alpha(4, 0.7 ), lty = 1)
      }
      
      #sequence of points locaitons
      xpi <- round(seq(2017, 2080, length.out = 10),0)
      xpmi <- match(xpi,(2017:2100))
      
      
      if(i == 1){ #afforestation SPRUCE
        #afforestation crop.trees
        lines(2017:2100, xna.fun(soc.i1.1[,2]), lty=1, col = 4) #118:201
        points((2017:2100)[xpmi], (xna.fun(soc.i1.1[,2]))[xpmi],
               pch =17, col = 4)
        #afforestation crop.grasstrees
        lines(2017:2100, xna.fun(soc.i1.2[,2]), lty=3, col = 4)
        points((2017:2100)[xpmi], (xna.fun(soc.i1.2[,2]))[xpmi],
               pch =2, col = 4)
        
        #afforestation grass.trees
        lines(2017:2100, xna.fun(soc.i1.3[,2]), lty=2, col = 1)
        points((2017:2100)[xpmi], (xna.fun(soc.i1.3[,2]))[xpmi],
               pch =2, col = 1)
        
      } else if(i == 4){ #afforestation BIRCH
        #afforestation crop.trees
        lines(2017:2100, xna.fun(soc.i1.1[,2]), lty=1, col = 4) #118:201
        points((2017:2100)[xpmi], (xna.fun(soc.i1.1[,2]))[xpmi],
               pch =16, col = 4)
        #afforestation crop.grasstrees
        lines(2017:2100, xna.fun(soc.i1.2[,2]), lty=3, col = 4)
        points((2017:2100)[xpmi], (xna.fun(soc.i1.2[,2]))[xpmi],
               pch =1, col = 4)
        
        #afforestation grass.trees
        lines(2017:2100, xna.fun(soc.i1.3[,2]), lty=2, col = 1)
        points((2017:2100)[xpmi], (xna.fun(soc.i1.3[,2]))[xpmi],
               pch =1, col = 1)
        
      } else{
        #extensification crop.grass
        lines(2017:2100, xna.fun(soc.i3.1), lty=1, col = 4)
        
        #BAU crop
        #lines(2017:2100, xna.fun(soc.i2.1[118:201]), lty=2, col = 4)
        lines(2017:2100, xna.fun(soc.i2.1), lty=2, col = 4)
        #BAU grass
        #lines(2017:2100, xna.fun(soc.i2.2[118:201]), lty=1, col = 1)
        lines(2017:2100, xna.fun(soc.i2.2), lty=1, col = 1)
      }
      
    } else if(k == 2){ #PLOT ANNUAL CHANGE
      ylim.i <-  c(-1.7,rep(c(0.55,0.55,0.55),each =3)[i])
      par(xpd=F)
      par(mar=c(2,2,0.3,0.3)) 
      plot(1,1, col = "white", #main = c("Pirkanmaa","Kainuu","Pohjois-Pohjanmaa","","","")[i],
           cex.axis = 1.2, cex.lab=1.3,  
           frame.plot = FALSE,
           xaxt="n",
           yaxt="n",
           xlim = c(2010,2080), ylim = ylim.i, xlab = "time (years)", ylab = "SOC stock (tC/ha/y)" )
      if(i == 1){
        axis(2, at= seq(-1.5,1.5, by = 0.5),labels= T, cex.axis = 1.3)
        axis(1, at=seq(2020,2100, by = 20),labels=seq(2020,2100, by = 20), cex.axis = 1.3)
        
      } 
      if( i == 4 ){
        axis(2, at= seq(-1.5,1.5, by = 0.5),labels= T, cex.axis = 1.3)
        axis(1, at=seq(2020,2100, by = 20),labels=seq(2020,2100, by = 20), cex.axis = 1.3)
        
      }
      if(i == 7){
        axis(2, at= seq(-1.5,1.5, by = 0.5),labels= T, cex.axis = 1.3)
        axis(1, at=seq(2020,2100, by = 20),labels=seq(2020,2100, by = 20), cex.axis = 1.3)
      }
      par(xpd=F)
      abline(v = 2017, col = 2, lty = 3)
      abline(h = 0, col = 2, lty = 1)
      
      par(xpd=T)
      legend(2000, 0.7, legend=paste("(",tolower(c("G","","","H", "", "","F", "", ""))[i],")",sep=""), bty ="n", cex = 1.7)
      
      #sequence of points locaitons
      xpi <- round(seq(2017, 2080, length.out = 10),0)
      xpmi <- match(xpi,(2017:2100))
      
      ly <-length(2017:2100) #lenght of time period
      if(i == 1){ #afforestation SPRUCE
        #afforestation crop.trees
        lines(2017:2100, xna.fun(soc.i1.1[,2])/(1:ly), lty=1, col = 4) #118:201
        points((2017:2100)[xpmi], (xna.fun(soc.i1.1[,2])/(1:ly))[xpmi],
               pch =17, col = 4)
        #afforestation crop.grasstrees
        lines(2017:2100, xna.fun(soc.i1.2[,2])/(1:ly), lty=3, col = 4)
        points((2017:2100)[xpmi], (xna.fun(soc.i1.2[,2])/(1:ly))[xpmi],
               pch =2, col = 4)
        
        #afforestation grass.trees
        lines(2017:2100, xna.fun(soc.i1.3[,2])/(1:ly), lty=2, col = 1)
        points((2017:2100)[xpmi], (xna.fun(soc.i1.3[,2])/(1:ly))[xpmi],
               pch =2, col = 1)
        
      } else if(i == 4){ #afforestation BIRCH
        #afforestation crop.trees
        lines(2017:2100, xna.fun(soc.i1.1[,2])/(1:ly), lty=1, col = 4) #118:201
        points((2017:2100)[xpmi], (xna.fun(soc.i1.1[,2])/(1:ly))[xpmi],
               pch =16, col = 4)
        #afforestation crop.grasstrees
        lines(2017:2100, xna.fun(soc.i1.2[,2])/(1:ly), lty=3, col = 4)
        points((2017:2100)[xpmi], (xna.fun(soc.i1.2[,2])/(1:ly))[xpmi],
               pch =1, col = 4)
        
        #afforestation grass.trees
        lines(2017:2100, xna.fun(soc.i1.3[,2])/(1:ly), lty=2, col = 1)
        points((2017:2100)[xpmi], (xna.fun(soc.i1.3[,2])/(1:ly))[xpmi],
               pch =1, col = 1)
        
      } else{ #i = 7
        #extensification crop.grass
        lines(2017:2100, xna.fun(soc.i3.1)/(1:ly), lty=1, col = 4)
        
        #BAU crop
        #lines(2017:2100, xna.fun(soc.i2.1[118:201]), lty=2, col = 4)
        lines(2017:2100, xna.fun(soc.i2.1)/(1:ly), lty=2, col = 4)
        #BAU grass
        #lines(2017:2100, xna.fun(soc.i2.2[118:201]), lty=1, col = 1)
        lines(2017:2100, xna.fun(soc.i2.2)/(1:ly), lty=1, col = 1)
      }
    } #end of k condition
  } #end of k loop    
  
}
mtext( "Time (year)", 
       1, out = T, line = 1.3, cex = 1.2)

#dev.off()