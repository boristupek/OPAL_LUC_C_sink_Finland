
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


## SOC Data from literature ############################################################

# SOCs from Palosuo et al. 2015 Figure S4
# Taru Palosuo, Jaakko Heikkinen & Kristiina Regina (2015) Method for estimating soil carbon stock changes in Finnish mineral cropland and grassland soils, Carbon Management, 6:5-6, 207-220, DOI: 10.1080/17583004.2015.1131383

#data for 2009 (data from Taru)
soc.reg <- read.csv(paste(opal.path,"data/RAW/", "SOC_comparison.to.Heikkinen.csv" , sep=""),
                    header=T, sep = ";",stringsAsFactors = FALSE, na.strings=c("NA",""))

#soc for 1 m read manually from Fig. S4
soc.p <- data.frame(r=c("Pirkanmaa","Kainuu","Pohjois-Pohjanmaa"), soc = c(131.75,145,150))
soc.p.er <- data.frame(r=c("Pirkanmaa","Kainuu","Pohjois-Pohjanmaa"), soc.err = c(20.3,53.9,24.4))

#SOC from Karhu et al 2011 Afoorestation sites 
#afforestation spruce site 3, 4, 5, and 6
soc.ks3 <- read.csv(paste(opal.path,"data/RAW/", "Karhu_2011_f4_3.csv" , sep=""),
                    header=F, sep = ",",stringsAsFactors = FALSE, na.strings=c("NA",""))
soc.ks4 <- read.csv(paste(opal.path,"data/RAW/", "Karhu_2011_f4_4.csv" , sep=""),
                    header=F, sep = ",",stringsAsFactors = FALSE, na.strings=c("NA",""))
soc.ks5 <- read.csv(paste(opal.path,"data/RAW/", "Karhu_2011_f4_5.csv" , sep=""),
                    header=F, sep = ",",stringsAsFactors = FALSE, na.strings=c("NA",""))
soc.ks6 <- read.csv(paste(opal.path,"data/RAW/", "Karhu_2011_f4_6.csv" , sep=""),
                    header=F, sep = ",",stringsAsFactors = FALSE, na.strings=c("NA",""))
#afforestation birch site 3, 4, and 6
soc.kb3 <- read.csv(paste(opal.path,"data/RAW/", "Karhu_2011_f5_3.csv" , sep=""),
                    header=F, sep = ",",stringsAsFactors = FALSE, na.strings=c("NA",""))
soc.kb4 <- read.csv(paste(opal.path,"data/RAW/", "Karhu_2011_f5_4.csv" , sep=""),
                    header=F, sep = ",",stringsAsFactors = FALSE, na.strings=c("NA",""))
soc.kb6 <- read.csv(paste(opal.path,"data/RAW/", "Karhu_2011_f5_6.csv" , sep=""),
                    header=F, sep = ",",stringsAsFactors = FALSE, na.strings=c("NA",""))

#DEFORESTATION
soc.kd10 <- read.csv(paste(opal.path,"data/RAW/", "Karhu_2011_f7_10.csv" , sep=""),
                     header=F, sep = ",",stringsAsFactors = FALSE, na.strings=c("NA",""))
soc.kd11  <- read.csv(paste(opal.path,"data/RAW/", "Karhu_2011_f7_11.csv" , sep=""),
                      header=F, sep = ",",stringsAsFactors = FALSE, na.strings=c("NA",""))
soc.kd12  <- read.csv(paste(opal.path,"data/RAW/", "Karhu_2011_f7_12.csv" , sep=""),
                      header=F, sep = ",",stringsAsFactors = FALSE, na.strings=c("NA",""))
soc.kd13  <- read.csv(paste(opal.path,"data/RAW/", "Karhu_2011_f7_13.csv" , sep=""),
                      header=F, sep = ",",stringsAsFactors = FALSE, na.strings=c("NA",""))
soc.kd14 <- read.csv(paste(opal.path,"data/RAW/", "Karhu_2011_f7_14.csv" , sep=""),
                     header=F, sep = ",",stringsAsFactors = FALSE, na.strings=c("NA",""))

library(doBy)
names.k <- c("year","soc")
k.dat <- c("soc.kb3",     "soc.kb4",     "soc.kb6",
           "soc.kd10",    "soc.kd11",    "soc.kd12", "soc.kd13",    "soc.kd14",
           "soc.ks3",     "soc.ks4",     "soc.ks5",     "soc.ks6")

#ls()[grep("soc.k", ls())]
for(i in 1: length(k.dat)){
  #i = 2
  k.x <- get(k.dat[i])
  k.x[,1] <- round(k.x[,1],0)
  names(k.x) <- c("year","soc")
  k.xa <- summaryBy(soc ~ year, k.x, FUN = c(mean,sd))
  names(k.xa) <- sub("soc", sub("soc.","",k.dat[i]),names(k.xa))
  assign(k.dat[i], k.xa)
}
paste(k.dat,collapse=",")

library(dplyr)
soc.kbs <- cbind(soc.kb3,soc.kb4,soc.kb6,soc.ks3,soc.ks4,soc.ks5,soc.ks6)
soc.kbs <- round(soc.kbs[,-grep("year",names(soc.kbs))[2:7]],1)
soc.kbs.mean <- soc.kbs[,c(grep("mean",names(soc.kbs)))]
soc.kbs.sd<- soc.kbs[,c(grep("sd",names(soc.kbs)))]
dc.kbs.mean <- soc.kbs.mean - bind_rows(replicate(3, soc.kbs.mean[1,], simplify = FALSE))

soc.kd <- cbind(soc.kd10,soc.kd11,soc.kd12,soc.kd13,soc.kd14)
soc.kd.y <- round(soc.kd[2,grep("year",names(soc.kd))],1)
soc.kd.mean <- soc.kd[,c(grep("mean",names(soc.kd)))]
soc.kd.sd <- soc.kd[,c(grep("sd",names(soc.kd)))]
dc.kd.mean <- soc.kd.mean - bind_rows(replicate(2, soc.kd.mean[1,], simplify = FALSE))

df.soc.mod <- lm(as.numeric(dc.kd.mean[2,])~0+as.numeric(soc.kd.y))
summary(df.soc.mod)


#karhu et al. 2011 in 10 years
df.soc.k <- 0.0425*10-1.9036
