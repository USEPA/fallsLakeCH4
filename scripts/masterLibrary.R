library(fs)
library(tidyverse) #ggplot, dplyr, and others
library(readxl) #reading excel files
library(janitor)
library(lutz)

library(foreign)  # read .dbf in readSitesEqAreaData.R
library(reshape2) # melt
library(scales) # time data in ggplot
library(gridExtra) # For multipanel plots
library(minpack.lm) # for non linear diffusion model
library(spsurvey)
library(rgdal) # read shapefiles, should switch to sf

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

# GRTS ANALYSIS FUNCTION------------------
# Analyze continuous variable from grts survey design.
grtsMeanVariance <- function(x) {
  
  # FIRST, DEFINE FRAMESIZE.  DEPENDS ON WHETHER STRATIFIED OR NOT.
  if(length(unique(x$stratum)) == 1) {  # if unstratified
    framesize.tmp = select(x, Area_km2) %>% distinct(Area_km2)
    framesize <- c("lake" = sum(framesize.tmp$Area_km2)) # sum needed to accomodate multiple mdcaty
  }
  
  if(length(unique(x$stratum)) > 1) {  # if stratified
    owFramesize.tmp <- filter(x, stratum == "open_water") %>%
      select(Area_km2) %>% distinct(Area_km2)
    owFramesize <- sum(owFramesize.tmp$Area_km2) # sum needed to accomodate multiple mdcaty
    
    tribFramesize.tmp <- filter(x, stratum == "trib") %>%
      select(Area_km2) %>% distinct(Area_km2)
    tribFramesize <- sum(tribFramesize.tmp$Area_km2) # sum needed to accomodate multiple mdcaty
    
    framesize <- c("open_water" = owFramesize, "trib" = tribFramesize)
  }
  
  
  # DEFINE NUMBER OF ROWS IN DATAFRAME
  nRows <- nrow(x)
  
  
  
  # CREATE SITES DATAFRAME
  sites <- data.frame(siteID=x$siteID,
                      Use=x$EvalStatus == "sampled")  # use sampled sites
  
  
  
  # SUBPOP DATAFRAME
  if(length(unique(x$stratum)) == 1) {  # if unstratified
    subpop <- data.frame(siteID=x$siteID,
                         lake=rep("lake", nRows))
  }
  
  if(length(unique(x$stratum)) > 1) {  # if stratified
    subpop <- data.frame(siteID=x$siteID,
                         lake=rep("lake", nRows),
                         stratum=x$stratum)
  }
  
  
  # DESIGN DATAFRAME
  design <- data.frame(siteID=x$siteID,
                       wgt=x$adjWgt,
                       xcoord=x$xcoord,
                       ycoord=x$ycoord)
  
  
  # DATA.CONT data frame.
  data.cont <- data.frame(siteID = x$siteID,
                          ebMlHrM2 = x$ebMlHrM2, # volume of gas in trap
                          #chla = x$chla_S,
                          #tp = x$TP,
                          #tn = x$TN,
                          #tnh4 = x$TNH4,
                          #tno2 = x$TNO2,
                          trap_ch4.ppm = x$trap_ch4.ppm,
                          #tno2-3 = x$TNO-3, # this breaks code.  need to remove dash
                          #dissolved.ch4 = x$dissolved.ch4,
                          #ch4.sat.ratio = x$ch4.sat.ratio,
                          #dissolved.co2 = x$dissolved.co2,
                          #co2.sat.ratio = x$co2.sat.ratio,
                          #dissolved.n2o = x$dissolved.n2o,
                          #n2o.sat.ratio = x$n2o.sat.ratio,
                          ch4.drate.mg.m2.h = x$ch4.drate.mg.h.best,
                          co2.drate.mg.m2.h = x$co2.drate.mg.h.best,
                          ch4.erate.mg.h = x$ch4.erate.mg.h,
                          #co2.erate.mg.h = x$co2.erate.mg.h,
                          #n2o.erate.mg.h = x$n2o.erate.mg.h,
                          #co2.trate.mg.h = x$co2.trate.mg.h,
                          ch4.trate.mg.h = x$ch4.trate.mg.h)
  
  
  # CALCULATE CDF ESTIMATES
  if(length(unique(x$stratum)) == 1) {  # if unstratified
    cdf.final <- cont.analysis(sites, subpop, design, data.cont,
                               popsize=list(lake=sum(framesize)))
  }
  
  if(length(unique(x$stratum)) > 1) {  # if stratified
    cdf.final <- cont.analysis(sites, subpop, design, data.cont,
                               popsize=list(lake=sum(framesize),
                                            stratum=as.list(framesize)))
  }
  cdf.final
}