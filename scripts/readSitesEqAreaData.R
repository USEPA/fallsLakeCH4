# THIS CODE WILL BE USED TO READ THE POPULATED SHAPEFILES FOR THE FALLS LAKES
# SAMPLING EVENTS.  SINCE
# WE REALLY DON'T NEED THE FULL SHAPEFILE FOR THE ANALYSIS, WE WILL
# JUST WORK WITH THE .dbf FILE.

# LIBRARY----------
# source("ohio2016/scriptsAndRmd/masterLibrary.R")

# READ IN .dbf FILES-------------------

# 1. Create a list of files to read in.  The completed data files all
# contain the pattern SitesEqAreaData.....dbf and are stored in 
# O:\Public\JTWalker\ENSB Methane\Shapefiles
# directory.

rootDir <- "O:/Public/JTWalker/ENSB Methane/Shapefiles/"

fileNames <- list.files(path = rootDir, 
                        pattern = "SitesEqAreaData.dbf", # file names containing this pattern
                        recursive = TRUE) # look in all subdirectories


# 2.  Read in files
mylist1 <- list()  # Create an empty list to hold data

for (i in 1:length(fileNames)){  # for each file
  # read.dbf is in foreign and spsurvey.  The foreign version allows for
  # use of as.is argument.
  data.i <- foreign::read.dbf(paste(rootDir, fileNames[i], sep = ""),
                    as.is=TRUE)
  mylist1[[i]] <- data.i
}



#3. Some dfs in list have column name "GNIS_Name".  Rename to Lake_Name where it appears. 
# ;x needed to have function report whole df.
mylist2 <- lapply(mylist1, function(x) {names(x) <- sub(pattern = "GNIS_Name", replacement = "Lake_Name", x = names(x));x})

# Add 'section' as column, if not already present.  This happens in stratified equal area designs
mylist3 <- lapply(mylist2, function(x){
  if("section" %in% names(x))  # if 'section' already exists
    x  # then report original df
  else 
    cbind(x, section = NA) # if 'section' doesn't exist, report new column of NAs
})
                                          
# 4.  Arrange columns in identical way to facilitate rbind
mylist4 <- lapply(mylist3, function(x) {
  select(x, noquote(order(colnames(x))))} # sort colnames alphabetically
  ) 

# 5.  Coerce list into dataframe via rbind
 eqAreaData <- do.call("rbind", mylist4)  # Coerces list into dataframe.

# FORMAT DATAFRAME-----------
 eqAreaData <- mutate(eqAreaData, 
                      chmDeplyDtTm = as.POSIXct(paste(trim(deplyDt), # trim removes white space
                                                      trim(chmStTm), sep=""),
                                                format = "%m/%d/%Y%H:%M",
                                                tz="UTC") + # set tz!
                        # deplyDt refers to funnels which were deployed on day 1.
                        # chambers deployed on day 2.  Therefore add 24 hours
                        # (in units of seconds) to POSIXct object.  Short term
                        # work around.  Should add a column for chamber deplyDt
                        # to attribute table.  Fix at a later date.
                        (24*60*60), 
                      trapDeplyDtTm = as.POSIXct(paste(trim(deplyDt), # trim removes white space
                                                       trim(deplyTm), sep=""),
                                                 format = "%m/%d/%Y%H:%M",
                                                 tz="UTC"),
                      trapRtrvDtTm = as.POSIXct(paste(trim(RtrvDat), # trim removes white space
                                                       trim(RtrvTim), sep=""),
                                                 format = "%m/%d/%Y%H:%M",
                                                 tz="UTC"),
                      deplyDt = as.Date(deplyDt, format = "%m/%d/%Y"))  

 # Columns that should be converted to numeric
 cols <- c("chm_vol", "wtrDpth", "smDpthS", "Tmp_C_S", "DOPrc_S", "DO__L_S",   
           "SpCn__S", "pH_S", "ORP_S", "TrNTU_S", "chla_S", "smDpthD", "Tmp_C_D", "DOPrc_D", "DO__L_D",   
           "SpCn__D", "pH_D", "ORP_D", "TrNTU_D", "chla_D", "BrPrssr", "TtTrpVl", "LatSamp", "LongSmp")
 
 eqAreaData[, cols] <- lapply(eqAreaData[, cols], as.numeric) # convert to numeric
 
 # NA in character fields (i.e. TrapExtn) shapefile are being read as character values.
 # Convert to NA.
 eqAreaData[, "TrapExtn"] <- ifelse(eqAreaData[, "TrapExtn"] == "NA", 
                                    NA, 
                                    eqAreaData[, "TrapExtn"])
 
 eqAreaData[, "ArExtnrs"] <- ifelse(eqAreaData[, "ArExtnrs"] == "NA", 
                                    NA, 
                                    eqAreaData[, "ArExtnrs"])
 
 eqAreaData[, "DG_Extn"] <- ifelse(eqAreaData[, "DG_Extn"] == "NA", 
                                    NA, 
                                    eqAreaData[, "DG_Extn"])
 
 # BAROMETRIC PRESSURE----------------
 # Assign barometric pressure to dissolved gas sampling site where BP
 # was not recorded.  The intention is to deal with this on the data entry
 # side, but if a site is overlooked, replace with mean of all other sites.
 eqAreaData <- group_by(eqAreaData, Lake_Name) %>% 
   mutate(BrPrssr = 
            # Select observation where dissolved gas was collected (i.e. anywhere a
            # deep sonde measurement was made), but BP wasn't recorded
            ifelse(is.na(BrPrssr),
                   # Set BP equal to mean values for all sites
                   mean(BrPrssr, na.rm = TRUE), 
                   BrPrssr)) # else return BP

 
 # HEADSPACE GAS AND WATER VOLUMES----------------
 # Make certain that for every dissolved gas sample, there is a value for Water
 # and gas volume.  If not, fix attribute table!
 # Every row reported by following functions should have values.
 filter(eqAreaData,
        !is.na(DG_Extn))  %>%
   select(H2O_vol, HeVol)
 
 
 
 # CHAMBER VOLUME
 # Calculate chamber volume based on relationship between water level
 # and volume.  See chamberDesign.xlsx in project folder.
 # Make sure chamber graduation was recorded for all deployments.  If not,
 # fix attribute table.
 filter(eqAreaData, # value should be present for each row.
        !is.na(chmDeplyDtTm)) %>%
   select(chm_vol)
 
 # Calculate volume (L)
 eqAreaData <- mutate(eqAreaData, chmVol.L = (42.057 + (-0.2189 * chm_vol)))
 

 