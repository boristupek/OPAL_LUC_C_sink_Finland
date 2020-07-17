## Yasso07Model for SoilR examples #######################################################################

# by Boris Tupek boris.tupek@luke.fi
# Dec 2019

#code requires github SoilR library functions, updated AWEN based Yasso model and matrix functions!!

cat("\n 
 \n yasso.matrix.fun returns model matrix depending on woody size and climate 
 \n  for steady state estimation, diagnostics of age and transition time
  \n #WS = 0, clim = 1, A.print = n
  \n returns default matrix ignores WS and climate, does not print
  \n #WS = 2, clim = c(5, 500, 7), A.print = y
  \n returns WS and clim ~ matrix and print matrix in the console")

years=seq(from=1,to=10,by=1)#/365)
Litter=data.frame(year=c(1:10),Litter=rnorm(n=10,mean=10,sd=2))
TempData=data.frame(years,Temp=15+sin(2*pi*years)+
                      rnorm(n=length(years),mean=0,sd=1))
j=length(years) # how many years we simulate 
MeanTemperature <- TempData$Temp  
TemperatureAmplitude <- rep(diff(range(TempData[,2]))/2,j) # temperature amplitude 
Precipitation <- rep(800,j) # precipitation 800mm

MT=MeanTemperature
TA=TemperatureAmplitude
PR_mm=Precipitation
#note conversion from mm to meters in the model's environmental function

# EXAMPLE of SoilR version of Yasso7 model ##
Ex1=Yasso07Model(t=years,C0=rep(0,5),In=Litter)
Ct1=getC(Ex1)
Rt1=getReleaseFlux(Ex1)

#par(mfrow=c(3,2), mar=c(4,4,2,1))
#plotCPool(years,Ct1,col=1:5,xlab="years",ylab="C pool",  main ="YM07.SoilR - ignores A,W,E,N fractions of litter input",
#          ylim=c(0,max(Ct1)))
#legend("topleft",c("xA","xW","xE","xN","xH"),lty=1,col=1:5,bty="n")
#plotCPool(years,Rt1,col=1:5,xlab="years",ylab="Respiration",ylim=c(0,50))

# EXAMPLE of fixed model ##
# Modified yasso07 C. Sierra general model WITH environmental effect 
yassofix <- Yasso07Modelfi(years,
                           C0=rep(0,5), #initial carbon
                           AWEN = c(0.52,0.18,0.08,0.2,0), #to separate litter to yasso AWEN pools, this depends on plant organ and species
                           In=Litter,#litter C input (same length as years)
                           xi = 0, # only xi = 1  will replace climate data no climate effect,
                           MT=MT,#MeanTemperature
                           TA=TA, #TemperatureAmplitude
                           PR_mm=PR_mm,#Precipitation_mm)
                           WS=2) 

Ct=getC(yassofix)
Rt=getReleaseFlux(yassofix) #respiration

par(mfrow=c(2,2), mar=c(4,4,2,1))
#plot carbon pools
matplot( Ct, type="l", lty=1, col=1:5, xlab="Years", ylab="C pool", main ="YM07SoilR.fix for A,W,E,N fractions * tot.litter.C")
legend("topleft", c("xA","xW","xE","xN", "xH"), lty=1, col=1:5, bty="n", n = 1)
#plot respiration
matplot(years, Rt, type="l", ylab="Respiration", lty=1, col=1:2)

# Modify litter input in form of AWEN
LitterAWEN <- c(0.52,0.18,0.08,0.2,0) 
LA =  matrix(LitterAWEN , nrow=j, ncol=5, byrow=TRUE) 
LI.awen = as.data.frame(LA*as.vector(Litter[,2])) #LitterInput
names(LI.awen) = c("A","W","E","N", "H")
LI.y.awen = data.frame(years, LI.awen)
as.matrix(LI.y.awen[,2:6])

yassofix2 <- Yasso07Modelfi(years,
                           C0=rep(0,5), #initial carbon
                           AWEN = 0, # or AWEN e.g. c(0.52,0.18,0.08,0.2,0), #to separate litter to yasso AWEN pools, this depends on plant organ and species
                           In=LI.y.awen ,#litter C input (same length as years)
                           xi = 0, # only xi = 1  will replace climate data no climate effect,
                           MT=MT,#MeanTemperature
                           TA=TA, #TemperatureAmplitude
                           PR_mm=PR_mm,#Precipitation_mm)
                           WS=2) 

Ct2=getC(yassofix2)
Rt2=getReleaseFlux(yassofix2) #respiration

#par(mfrow=c(1,2))
#plot carbon pools
matplot( Ct2, type="l", lty=1, col=1:5, xlab="Years", ylab="C pool", main = "YM07SoilR.fix for litter.C input as A W E N columns table")
legend("topleft", c("xA","xW","xE","xN", "xH"), lty=1, col=1:5, bty="n", n = 1)
#plot respiration
matplot(years, Rt2, type="l", ylab="Respiration", lty=1, col=1:2)

Ct
Ct2
identical(Ct,Ct2)


### model diagnostics ##################

yasso.matrix.fun(WS = 2, clim = c(5, 500, 7), A.print = "y")
yasso.matrix.fun(WS = 0, clim = 1, A.print = "n")

AYS.wsfun <- function(WS){
  ksY.ws = c(ksY[1:4]*(1+delta1*WS+delta2*(WS^2))^(r),ksY[5])
  #print(ksY.ws)
  A1.ws = abs(diag(ksY.ws))
  AYS.ws = Ap %*% A1.ws
  #print(AYS.ws)
  return(AYS.ws)
}

#decomposition rates
ksY = c(kA = 0.73, kW = 5.8, kE = 0.29, kN = 0.031, 
        kH = 0.0017) 
#transfers and feedbacks
pY = c(p1 = 0.48, p2 = 0.01, p3 = 0.83, p4 = 0.99, 
       p5 = 0, p6 = 0.01, p7 = 0, p8 = 0, p9 = 0.03, p10 = 0, 
       p11 = 0.01, p12 = 0.92, pH = 0.0045)
# climate dependence parameters
beta1 = 0.096 
beta2 = -0.0014 
gamma = -1.21 
# Woody litter size dependence parameters
delta1 = -1.7 
delta2 = 0.86 
r =  -0.306
#structural matrix
Ap = diag(-1, 5, 5)
Ap[1, 2] = pY[1]
Ap[1, 3] = pY[2]
Ap[1, 4] = pY[3]
Ap[2, 1] = pY[4]
Ap[2, 3] = pY[5]
Ap[2, 4] = pY[6]
Ap[3, 1] = pY[7]
Ap[3, 2] = pY[8]
Ap[3, 4] = pY[9]
Ap[4, 1] = pY[10]
Ap[4, 2] = pY[11]
Ap[4, 3] = pY[12]
Ap[5, 1:4] = pY[13]

#calculate steady states for different litter sizes
#note: quantity litter input and  quality awens should differ for different types
u.Li <- 5 * c(0.52,0.18,0.08,0.2,0) # 4 ton c /ha litter times AWEN fractions 
par(mfrow=c(1,1))
plot(c(1,3),c(0,100), col ="white", xlab="litter size (cm)",ylab ="soil C ", main = "A W E N H steady state Carbon ~ litter size ", xaxt="n")
axis(side = 1, at = c(1.3,2,2.7), labels = c(0,2,20), tck = -0.01)
#compute steady state
for(i in 1:3){
  AYS.ws <- yasso.matrix.fun(WS = c(0,2,20)[i], clim = 1, A.print = "n")
  xss=-1*solve(AYS.ws)%*%u.Li #inverse of matrix solve(B)
  points(rep(c(1.3,2,2.7)[i],5), xss, col=1:5, pch = 16)
}
legend("topleft", c("A","W","E","N", "H"), pch =16, col=1:5, bty="n", n = 1)

#functionality ages and transit times
par(mfrow=c(2,2))
tau=seq(0,500)
for(i in 1:3){
  #i= 1
  AYS.ws <- AYS.wsfun(c(0,2,20)[i])
  SA=systemAge(A=AYS.ws, u=u.Li, a=tau)
  
  plot(tau, SA$systemAgeDensity, type="l", main = paste("System age ~ litter size ",c(0,2,20)[i]," (cm)", sep =""))
  abline(v=SA$meanSystemAge, lwd=2) #mean
  #abline(v=SA$quantilesSystemAge[2], col=2) #medium
  matplot(tau, SA$poolAgeDensity, col = 1:5, type = "l", add = T)
  abline(v=SA$meanPoolAge, col = 1:5, lwd=2) #mean
  legend("topright", c("A","W","E","N", "H"), pch =16, col=1:5, bty="n", n = 1)
}


TT=transitTime(A=AYS.ws, u=u.Li, a=tau)
names(TT)
plot(tau, TT$transitTimeDensity, type="l")
abline(v=TT$meanTransitTime)


