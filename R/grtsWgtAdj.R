# ADJUST WEIGHTS
# Need to define 4 inputs
# 1. sitesAdj:  TRUE/FALSE corresponding to "sampled" "notsampled"
# 2. wgtAdj: original weights
# 3. wgtCat: stratum if equal probability (stratified or unstratified), mdcaty if unequal
# 4. framesizeAdj: named vector containing area of:
#       -if unstratified, then whole lake
#       -if unstratified-unequal (Charles Mill, Hocking), then section
#       -if stratified-equal, then each strata
#       -if stratified-unequal, then each section

myWgtList <- list() # empty list to catch adjusted weights

for (i in 1:length(unique(eqAreaData$Lake_Name))) {
  lake.i <- unique(eqAreaData$Lake_Name)[i]
  data.i <- filter(eqAreaData, Lake_Name == lake.i)
  
# 1. sitesAdj:  TRUE/FALSE corresponding to "sampled" "notsampled"  
  sites.adj.i <- ifelse(data.i$EvalStatus == "sampled",
                        TRUE, FALSE)
  
# 2. wgtAdj: original weights  
  wgtAdj.i <- data.i$wgt

# 3. wgtCat: stratum if equal area (stratified or unstratified), section if unequal   
  if(length(unique(data.i$mdcaty)) == 1) { # one mdcaty means equal area
    wgtCat.i <- data.i$stratum
  } else {  # >1 mdcaty means unequal area
    wgtCat.i <- data.i$section
  }
  
# 4. framesizeAdj: named vector containing area of:
  # The contents of this object are determined by the survey desing
  # (i.e. stratified - equal, unstratified-unequal).  The 'if' statements below
  # identify which survey design was used and populates framesizeAdjust accordingly.
  # All Falls Lake survey designs are stratified - unequal area, but we are retaining
  # the full code in case it is useful for a future study.
  
  
  if(length(unique(data.i$stratum)) == 1 & length(unique(data.i$mdcaty) == 1)) { # unstratified and equal area
    framesizeAdj.i <- distinct(data.i, Area_km2) %>% select(Area_km2)
    framesizeAdj.i <- framesizeAdj.i[ , "Area_km2"]  
    attributes(framesizeAdj.i) <- NULL
    names(framesizeAdj.i) <- c("None")
  }
    
  if(length(unique(data.i$stratum)) > 1 & length(unique(data.i$mdcaty) == 1)) { # stratified, equal
    
    framesizeAdj.ow <- filter(data.i, stratum == "open_water") %>%
      distinct(Area_km2) %>% select(Area_km2)
    
    framesizeAdj.trib <- filter(data.i, stratum == "trib") %>%
      distinct(Area_km2) %>% select(Area_km2)
    
    framesizeAdj.i <- c(framesizeAdj.ow[1,1], framesizeAdj.trib[1,1])
    attributes(framesizeAdj.i) <- NULL
    names(framesizeAdj.i) <- c("open_water", "trib")
  }
  
  if(length(unique(data.i$stratum)) >= 1 & length(unique(data.i$mdcaty)) > 1) { # stratified or unstratified (CharlesM,Hocking), unequal
    nSection.i <- length(unique(data.i$section))
    section.i <- unique(data.i$section)
    
    framesizeList <- list()
    for (j in 1:nSection.i) {
      framesizeList[[j]] <- filter(data.i, section == section.i[j]) %>%
        distinct(Area_km2) %>% select(Area_km2)
    }
    
    framesizeAdj.i <- do.call("rbind", framesizeList)
    
    framesizeAdj.i <-  framesizeAdj.i[ , "Area_km2"]
    attributes(framesizeAdj.i) <- NULL
    names(framesizeAdj.i) <- section.i
  }
  
# 5. Adjust weights
  adjustedWgt.i <- adjwgt(sites.adj.i, wgtAdj.i, wgtCat.i, framesizeAdj.i)
  
  myWgtList[[i]] <- data.frame(
    Lake_Name = data.i$Lake_Name,
    siteID = data.i$siteID,
    adjWgt = adjustedWgt.i
  )
}

# 6. Verify that all lakes have >=15 sites with non zero weights
lapply(myWgtList, function(x) summarize(x,n = sum(x$adjWgt > 0)))


# 7. Incorporate adjusted weights into eqAreaData
wgtAdjDf <- do.call("rbind", myWgtList)  # Coerces list into dataframe.
wgtAdjDf[, c("Lake_Name", "siteID")] <- lapply(wgtAdjDf[, c("Lake_Name", "siteID")], 
                                               as.character) # convert to character

str(eqAreaData) # 72
str(wgtAdjDf)  # 72 observations
eqAreaData <- merge(eqAreaData, wgtAdjDf) #1426 observations






