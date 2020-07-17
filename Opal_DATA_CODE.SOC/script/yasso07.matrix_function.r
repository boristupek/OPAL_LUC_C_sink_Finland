## Yasso07Model matrix for SoilR #######################################################################

#function useful to calculate staeady state analytically (see code examples)
# by Boris Tupek boris.tupek@luke.fi
# Dec 2019

#yasso.matrix.fun returns model matrix depending on woody size and climate 
yasso.matrix.fun <- function(WS,# 0,2,20 cm, when 0 ignored
                             clim, #MT, TA, PR_mm, if clim = 1 ignored
                             A.print #if A.print = "y", prints the structural matrix, "n" ignored
){
  # for below to work
  # read the structure functions to the environment from the model!
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
  
  AYS = Ap %*% abs(diag(ksY))
  
  if(A.print=="y"){
    print("default structural matrix")
    print(AYS)
  }
  
  # add Woody litter size dependence to structural matrix
  # WS in cm, e.g. 0 for nonwoody, 2 for finewoody, 20 - for coarsewoody litter
  # the effect of wl size as in Eq. 3.1 in model description
  
  AYS.wsfun <- function(WS){
    ksY.ws = c(ksY[1:4]*(1+delta1*WS+delta2*(WS^2))^(r),ksY[5])
    A1.ws = abs(diag(ksY.ws))
    AYS.ws = Ap %*% A1.ws
    if(A.print=="y"){
      print("structural matrix modified by woody size")
      print(AYS.ws)
    }
    return(AYS.ws)
  }
  AYS.ws <- AYS.wsfun(WS)
  
  #yasso environmental function
  if(length(clim) ==3){
    
    MT = clim[1]
    TA = clim[3]
    PR_mm = clim[2]
    
    y07.Efun <- function(MT,TA,PR_mm){
      #MT = Temp
      #TA =TempAmplit
      PR = PR_mm/1000  # conversion from mm to meters
      
      #seasonal approximation of temperature (if annual mean)
      T1=MT+4*TA/pi*(1/sqrt(2)-1)          # Eq. 2.4 in model description
      T2=MT-4*TA/(sqrt(2)*pi)              # Eq. 2.5 in model description
      T3=MT+4*TA/pi*(1-1/sqrt(2))          # Eq. 2.6 in model description
      T4=MT+4*TA/(sqrt(2)*pi)
      TS =  exp(beta1*cbind(T1,T2,T3,T4)+beta2*(cbind(T1,T2,T3,T4)^2))*(1-exp(gamma*PR))
      TS
      apply(TS,1,mean)
    }
    xiE <- y07.Efun(MT, TA, PR_mm)
    AYS.wsE <- xiE*AYS.ws
  } else{
    AYS.wsE <- 1*AYS.ws
  }
  if(A.print=="y"){
    print("returns structural matrix modified by woody size and climate")
    print(AYS.wsE)
  }
  return(AYS.wsE)
}
