## Carbon sink potential of land use change in Finland
## Tupek et al. 2020 boris.tupek@luke.fi


# CONTENT of the SOC analysis include:
#data:
# 1) climate (air temperature, annual min and maximum, precipitation)
# 2) biomass and litter input (C in AWEN 5 pools distributions, total C = sum of AWEN pools) for land use types and land use change
# 3) agricultural statistics
# 4) literature Karhu et al. 2011, Palosuo et al. 2014
# 5) LUC per region (Table 1)

# r scripts:
### I) read processed data for model input 
# 1) climate data  as in MS figures (MS climate)
# 2) litter data in AWEN form as totals in MS figures (MS litter input)
#    equilibrium forest, cultivation,extensification, afforestation
# 3) yasso07 model function

### II) soc modelling
## SOC SIMULATIONS for following periods:
# a) spinup for forest(motti mean) 1900, 
# b) agriculture 116 years (grass, crop),  1990-2017
# c) rest towards 2100 forest (motti) 1 rotation (length depends on species and fertility)
#
# 4) SOC modelling +figures (MS SOC developement+obseravations Palosuo, Karhu):
# 4a) equilibrium forest
# 4b) cultivation
# 4c) extensification
# 4d) afforestation

### III) output results 
# 5) SOC changes  + figures (MS SOC change+observations Karhu):



## SOC MODELLING ############################################################################################################################

#run part below by reading processed input data
rm(list=ls())
#path for replicable clean data.code version
opal.path <- "D:/LUKE/OPAL_revision/Opal_DATA_CODE.SOC/" 
#function to plot error range
library(scales) 
source(paste(opal.path,"script/","plotLineWithRange.r", sep=""))
source(paste(opal.path,"script/","plotLineWithRange1.r", sep=""))

## LOAD YASSO07 ######## 
## 
#model functions run on SoilR general model platform
#install.packages("devtools", dependencies = T)
library(devtools)
#NOTE: SoilR package on github is more updated and contains new or updated functions! 
#devtools::install_github('MPIBGC-TEE/SoilR-exp/pkg', force = T)
library(SoilR)

#Yasso07 model as in Tuomi et al. 2011. Environmental Modeling and Software 26 (11): 1358-1362. doi:10.1016/j.envsoft.2011.05.009
source(paste(opal.path,"script/","yasso07.model_function.r", sep=""))
#Yasso07 model matrix function useful to calculate staeady state analytically (see code examples)
#yasso.matrix.fun returns model matrix depending on woody size and climate 
source(paste(opal.path,"script/","yasso07.matrix_function.r", sep=""))
#Yasso07Modelfi
#model examples (model runs, effecto of woody size and climate, analytical steady state, diagnostics)
#source(paste(opal.path,"script/","yasso07.examples_function.r", sep=""))

## READ PROCESSED INPUT DATA ###
#read SOC data from literature
source(paste(opal.path,"script/","opal_input.soc.data.r", sep=""))
# read climate input 
load(file=paste(opal.path,"data/PROCESSED/","climate.mean.low.high_reducedpast_19002100.RData",sep=""))
clim.types <- ls()[grep("climate",ls())] # 3 climate types
#[1] "climate.high.list" "climate.low.list"  "climate.mean.list"
clim.regions.names <- names(climate.mean.list) # 3 limate regions
#[1] "Pirkanmaa"         "Kainuu"           "Pohjois-Pohjanmaa"
#read litter inputs (list of tables for each scenario)
load(file=paste(opal.path,"data/PROCESSED/","litter.input_woody.19002100_08.01.20.RData",sep=""))
load(file=paste(opal.path,"data/PROCESSED/","litter.input_woodyspin.zero.19002100_08.01.20.RData",sep=""))
load(file=paste(opal.path,"data/PROCESSED/","litter.input_spin.crop.trees.19002100_08.01.20.RData",sep=""))
load(file=paste(opal.path,"data/PROCESSED/","litter.input_spin.crop.grasstrees.19002100_08.01.20.RData",sep=""))
load(file=paste(opal.path,"data/PROCESSED/","litter.input_spin.grass.grasstrees.19002100_08.01.20.RData",sep=""))
load(file=paste(opal.path,"data/PROCESSED/","litter.input_spin.crop.extensif.19002100_08.01.20.RData",sep=""))
load(file=paste(opal.path,"data/PROCESSED/","litter.input_spin.crop.bau.19002100_08.01.20.RData",sep=""))
load(file=paste(opal.path,"data/PROCESSED/","litter.input_spin.grass.bau.19002100_08.01.20.RData",sep=""))
load(file=paste(opal.path,"data/PROCESSED/","litter.input_harvesting.clearcut.RData",sep=""))

#LITTER LISTS  ##########################
#woody litter
w.list <-  ls()[grep("list.w", ls())]
w.list
#non-woody litter
nw.list <-  ls()[grep("list.nw", ls())]
nw.list

#FOREST WOODY litter
# equilibr spruce-> affor spruce or birch 
# 6 unique fo species and region (6 for OMT and 6 for MT)
awen.litt.list.w_affor <- ls()[grep("litter.list.w", ls())][1:3]
awen.litt.list.w_affor
# equilibr spruce-> zero for cultivation
# 6 unique 
awen.litt.list.w_cultiv <- ls()[grep("litter.list.w0", ls())][1:3]
awen.litt.list.w_cultiv 

# AFFORESTATION NONWOODY litter
#6 unique for species and region for OMT
awen.litt.list_crop.trees <- ls()[grep("litter.list.nw.c", ls())]
awen.litt.list_crop.trees
#6 unique for species and region for MT
awen.litt.list_grass.grasstrees <- ls()[grep("litter.list.nw.g", ls())]
awen.litt.list_grass.grasstrees
#6 unique for species and region for OMT
awen.litt.list_crop.grasstrees <- ls()[grep("litter4.list.nw.c", ls())]
awen.litt.list_crop.grasstrees 

#BAU and EXTENSIFICATION NONWOODY litter
#3 unique fo regions 
awen.litt.list_crop.bau <- ls()[grep("litter2.list.nw.c", ls())]
awen.litt.list_grass.bau <- ls()[grep("litter2.list.nw.g", ls())]
awen.litt.list_crop.extensif <- ls()[grep("litter3.list.nw.c", ls())]


#RUN YASSO07 for LUC scenario managements ######################
## 8 litter type groups 
# note: 6 LUC scenarios (woody litter "w_affor" is associated with each afforestation, "w_cultiv" with extensification and BAUs)
litters <- c("awen.litt.list.w_affor",        
             "awen.litt.list.w_cultiv",
             "awen.litt.list_crop.trees",       #OMT
             "awen.litt.list_crop.grasstrees",  #OMT
             "awen.litt.list_grass.grasstrees", #MT
             "awen.litt.list_crop.extensif",
             "awen.litt.list_crop.bau",
             "awen.litt.list_grass.bau")
#LUC scenario managements
names.manage <- c("affor","cultiv","crop.trees","crop.grasstrees","grass.grasstrees","crop.extensif","crop.bau","grass.bau")

#run Yasso07 SOC simulations for LUC scenarios and for highest temperature and precipitation increase (RCP8.5 climate projection)
source(paste(opal.path,"script/","opal_yasso07.mod.runs.r", sep=""))
#savelists of simulated data woody and nowoody socs and simulation names
#climate selection 3 high = RCP8.5
save(list= c("soc.sims", "litt.sims", "sim.names", "soc.luceq100.sims", "soc.luceq.sims"), 
     file=paste(opal.path,"data/PROCESSED/",
                "soc.sims_climatelow.afforestation.extensification.bau_19002100.RData", sep=""))

## LUC SIMULATIONS -post-process SOCs and TOCs DATA + plot FIGURES #################################################################

# preview all  simulations in one multiline litter and soc plots 
# define functions to extract data for specific scenarios 
source(paste(opal.path,"script/","opal_luc.sims.figs.r", sep=""))
#view the characteristics of simulation runs
# optionally preview 8 litter and 8 soc figures of specific scenarios
  PREVIEW.specific.PLOTS =  FALSE #TRUE  #
source(paste(opal.path,"script/","opal_luc.sims.option.figs.r", sep=""))

## soc totals sum woody and nonwoody, though woody and non-woody simulations were run separately ######
  #note:
  # past soc is same before all LUCs
  # soc specification to fertility is determined by management 
  # (croplands are associated with high fertility, and grasslands with mid fertility)!
  # otherwise soc is specific to region and species (3x2) and litter.uncertainty.type (3 high,mean,low estimate)
  
# woody
#1) Afforestation 
# specific to 3 regions, 2 species, 2 fertility levels (12)

# nonwoody
#1) Afforestation (3 regions, 2 species)
#1.1 Afforestation crop to trees (NO grass) u.mang: "crop.trees"
#1.2 Afforestation crop to trees (with grass) u.mang: "crop.grasstrees"
#1.3 Afforestation grass to trees (with grass) u.mang: "grass.grasstrees"
#2) Extensification crop to grass # specific to region (3)
#3) Business as usual # specific to region (3) 
#3.1 crop
#3.2 grass

  
## SUPPLEMEMNT MSFIG.S4 ##################
  # ALL SOCs with CI (confidence intervals) 
  # OPAL plots all the SOC simulations 
  # regions, species, fetility (high, mid)
  # management afforestation, extensisfication, business as usual
  library(dplyr)
  #function to avoid plotting zeros
  xna.fun <- function(x){#replace 0 by na
    x[x==0]<-NA
    return(x)
  }
  xna.rm.fun <- function(x){#replace NA
    x[x==0]<-NA
    x <- x[-which(is.na(x))]
    return(x)
  }
source(paste(opal.path,"script/script.figs/", "opal.ms.fig.sup4_soc.all.r", sep=""))
#save figure
dev.print(pdf, file=paste(opal.path,"figs_ms/", 
          "MSFIG.S4_SOC_CI_SUspinup.Agriculture.Afforestation.ExtensificationContinuous_1900-2100.pdf", sep=""),
          width=10, height=7, pointsize=12)
  
## MS.FIG2 SOUTH SOC + table data ######################
# SOC for deforestation, extensificaiton, afforestation spruce, afforestation birch
# SOC Woody and Non- woody reduction during cultivation 
# 
source(paste(opal.path,"script/script.figs/", "opal.ms.fig2_soc.south.r", sep=""))
#save figure
dev.print(pdf, file=paste(opal.path,"figs_ms/", 
          "MSFIG.2_SSOC_SUspinup.Defo_WNW_Ex.Affo-SU-BI_SOUTH-SIMPLE_1900-2100.pdf", sep=""),
          width=12, height=5, pointsize=12)

## SOC CHANGE ############################
# compared to initial SOC
soc1 <-  bind_rows(replicate(dim(soc)[1], soc[1,], simplify = FALSE))
d.soc1 <- xna.fun(soc) - soc1
## Land use change LUC compared to a year before
soc2 <-  bind_rows(replicate(length(118:201), soc[117,], simplify = FALSE))
d.soc2 <- xna.fun(soc[118:201,]) - soc2
sim.names.u <- sim.names[which(sim.names$litter.level == "mean" & 
                                 sim.names$management != "affor"&
                                 sim.names$management != "cultiv" ),]
sim.dat.u <- matrix(NA, ncol = dim(sim.names.u)[1], nrow = 100)

## SOC change after LUC 3 regions and all scenarios ##############
  # extract data into table and plot preliminary figure
dsoc.fig <- data.frame(matrix(NA, ncol = length(sim.fig.names), nrow = length(118:201)))
names(dsoc.fig)<-sim.fig.names
source(paste(opal.path,"script/script.figs/", "opal.tab_preview.fig_dsoc.reg.scen_all.r", sep=""))
# OUTPUT SOC changes after 2017
write.table(dsoc.fig , file = paste(opal.path,"data/PROCESSED/", "SOC.difference.afforestation.agriculture_08.01.20.csv", sep=""),
            row.names=F, na="",col.names=T, sep=",")

# MSFIG.3  SOC CHANGE + OBSERVATIONS #########

  #mean cumulative change per hectare
source(paste(opal.path,"script/script.figs/", "opal.ms.fig3_dsoc.south_cum.y.r", sep=""))
dev.print(pdf, file=paste(opal.path,"figs_ms/", 
          "MSFIG.3_SOC_CI_change_CUM_Cultiv.Extens.BAU.Afforestation_OBS_2018-2100.pdf", sep=""),
          width=12, height=5, pointsize=12)
