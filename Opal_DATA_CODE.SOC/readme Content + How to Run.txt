## Carbon sink of land use change by mineral soil cropland extensification and afforestation can offset only a small fraction of national CO2 emissions in a boreal region
## Tupek et al. 2020 boris.tupek@luke.fi

# CONTENT of the SOC analysis include:
# ful version including pre-processing climate and litter input and post-processing SOC, Biomas, Total Carbon change available upon reasonable request.
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


# TO RUN THE SOC ANALYSIS OF THE ARTICLE (including the main figures FIG.S3, FIG.2, FIG.3) 
# source "opal_main.soc.r" file located in "script/" folder in R software environment https://www.r-project.org/
# the code subsequentially executes *.r scripts in "script/" and in "script.figs/" folders

# note: replace "D:/LUKE/OPAL_revision/" path by your path to the extracted "Opal_DATA_CODE.SOC" folder in "opal_main.soc.r" 
# opal.path <- "D:/LUKE/OPAL_revision/Opal_DATA_CODE.SOC/"
