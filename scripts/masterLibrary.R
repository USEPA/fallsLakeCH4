library(tidyverse) #ggplot, dplyr, and others
library(readxl) #reading excel files
library(foreign)  # read .dbf in readSitesEqAreaData.R
library(reshape2) # melt
library(scales) # time data in ggplot
library(gridExtra) # For multipanel plots
library(minpack.lm) # for non linear diffusion model



# TRIM FUNCTION--------------------------
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# EBULLITION MASS FLUX FUNCTION------------------------

# Function for calculating mass flux rate--                  
mass.rate <- function(X1, choice1){
  # trap gas data to use if measured values aren't available
  trap_ch4.ppm <- ifelse(is.na(X1$trap_ch4.ppm), mean(X1$trap_ch4.ppm, na.rm=TRUE), X1$trap_ch4.ppm) 
  trap_co2.ppm <- ifelse(is.na(X1$trap_co2.ppm), mean(X1$trap_co2.ppm, na.rm=TRUE), X1$trap_co2.ppm)
  trap_n2o.ppm <- ifelse(is.na(X1$trap_n2o.ppm), mean(X1$trap_n2o.ppm, na.rm=TRUE), X1$trap_n2o.ppm)
  
  # barometric pressure needed: n=PV/RT
  bp <- ifelse(is.na(mean(X1$BrPrssr, na.rm=TRUE)),
               1,
               mean(X1$BrPrssr, na.rm=TRUE)/760)
  
  # temperature needed
  gas.temp <- ifelse(is.na(X1$Tmp_C_S),
                     273.15 + 20, # assume 20C if not measured
                     273.15 + X1$Tmp_C_S)
  
  # convert 1mL to moles
  mL.to.mmoles <- ((bp*0.001)/(0.082058 * gas.temp)) * 1000      #1mL = 0.001L; *100 to convt to mmol       
  
  # convert mmoles to mg
  if(choice1 == "ch4") {mg.gas <- mL.to.mmoles * 16 * (trap_ch4.ppm/1000000)}  #16mg/mmole
  if(choice1 == "co2") {mg.gas <- mL.to.mmoles * 44 * (trap_co2.ppm/1000000)}  #44mg/mmole
  if(choice1 == "n2o") {mg.gas <- mL.to.mmoles * 44 * (trap_n2o.ppm/1000000)}  #44mg/mmole
  
  # calculate rate
  mass.flux.rate <- mg.gas * X1$ebMlHrM2 #bubble rate in mg ch4-co2-n2o /day/m2
  
  # return mass flux rate in mg ch4-co2-n2o /day/m2
  mass.flux.rate
}