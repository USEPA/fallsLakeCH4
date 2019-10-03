## SCRIPT FOR READING CHLOROPHYLL

# LIBRARY----------
# source("ohio2016/scriptsAndRmd/masterLibrary.R")

# READ IN DATA FILES DELIVERED FROM KIMBERLY WYATT-------------------

# 1. Create a list of files to read in.  The completed data files all
# contain the pattern Beaulieu....xls

dirPath <- "C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)/ENSB Methane/chlorophyll/"
fileNames <- list.files(path = dirPath) # look in all subdirectories


# 2.  Read in files
mylist <- list()  # Create an empty list to hold data

for (i in 1:length(fileNames)){  # for each file
  data.i <- read_excel(paste0(dirPath, fileNames[i]), sheet = "sample_log")
  mylist[[i]] <- data.i
}

# 3. combine into dataframe
chlData <- do.call("rbind", mylist)  # Coerces list into dataframe.


# 3.  Format elements of dataframe
names(chlData) = gsub(pattern = c("\\(| |#|%|)|/|-|\\+"), replacement = ".", x = names(chlData))
names(chlData) = tolower(names(chlData))

chlData <- chlData %>% 
  filter(project == "AEBRR",  # per Joel Allen's 5/8/2018 e-mail
         grepl("(?i)FL", sample_id)) %>%  # grab Falls Lake samples, not case sensitive 
  select(sample_id, chl_total, collection_date) %>% # fields of interest
  # strsplit splits each sample_id into a list element based on "."
  mutate(site = strsplit(.$sample_id, "[.]") %>% # for most sample_id,  each element contains 8 items
         lapply(., function(x) x[3]) %>%  # extract 3rd element from each list element (site)
           unlist(.),  # lapply produces a list, unlist to vector
         site = ifelse(grepl("BLK", sample_id),  # if a blank
                       "FLD", # the site is FLD
                       site)) # otherwise site

# quick peak
ggplot(chlData, aes(collection_date, chl_total)) + # blanks look good
  geom_point(aes(color = ifelse(site == "FLD",
                                "blank",
                                "unknown"))) 


# Gotta do stuff below yet 
# 
# # 5. Read in Karen's spreadsheet with filter volumes
# chlVol <- read_excel("ohio2016/inputData/J. Beaulieu Chlorophyll 2016/chlorophyllVolume2016.xlsx",
#                      sheet = "Ebullition")
# 
# # Strip unusual character names from column titles.
# # The escape (\\) is needed to get R to treat them as character.
# names(chlVol) = gsub(pattern = c("\\(| |#|)|/"), replacement = ".", x = names(chlVol))
# 
# # 6.  Make some fixes to data
# chl.c <- mutate(chl.c, nmsampleID = ifelse(nmsampleID == "06302016.ATW.SUQ3.0.1.UNK",
#                                   "06302016.ATW.SU03.0.1.UNK",
#                                   ifelse(nmsampleID == "07112016.RFL.S-31.0.1.UNK",
#                                           "07112016.RFL.S31.0.1.UNK",
#                                          nmsampleID)))
# 
# # Are sample codes from Karen's data sheet in the chl data?
# # All accounted for!
# chlVol[!(chlVol$sample.ID.code %in% chl.c$nmsampleID), "sample.ID.code"]
# 
# # 7. Merge chl data with filter volume
# chlFinal <- merge(select(chl.c, nmsampleID, chla.ug, pheo), 
#                   select(chlVol, sample.ID.code, volume.filtered..mL.),
#                   by.x = "nmsampleID", 
#                   by.y = "sample.ID.code", 
#                   all.y = TRUE)  # keep all obs from Karen's data sheet
# 
# # 8. Calculate chl a of original sample
# # See APHA 10-18 for details.
# # 5cm path length, 10mL extraction volume
# chlFinal <- mutate(chlFinal,
#                    # 1000000 convert from mL to m^3
#                    # 5 is for 5cm path length
#                    chla.sample = (chla.ug * 0.01) / ((volume.filtered..mL./1000000) * 5),
#                    pheo.sample = pheo * 0.01 / ((volume.filtered..mL./1000000) * 5))
# 
# 
# # 9. Separate calibration and regular samples
# chlCal <- filter(chlFinal, grepl(pattern = "CAL", x = nmsampleID))
# chlFinal <- filter(chlFinal, !grepl(pattern = "CAL", x = nmsampleID))
# 
# #10. Create unique identifiers for merge into eqAreaData
# charIds <- strsplit(chlFinal$nmsampleID,split = "\\.") %>% # split on periods
#   lapply(function(x) { # apply function to each element of list
#     x[2:3] # extract second and third elements (lake name and site)
#   }) %>%
#   unlist()  # unlist into vector
# 
# # Coerce into df and format
# charIds <- data.frame(Lake = charIds[seq(1, length(charIds), 2)], # extract lake
#                       siteID = charIds[seq(2, length(charIds), 2)], # extract site
#                       stringsAsFactors = FALSE) %>% 
#   mutate(siteID = ifelse(siteID == 46,  # site SU-46 at Cave Run not coded right
#                          "SU-46",
#                   ifelse(siteID == 0, # site S-09 at Roaming not coded right
#                           "S-09",
#                   ifelse(siteID == "SU7", # BVR site not entered right
#                           "SU-07",
#                   ifelse(!grepl("-", siteID) & nchar(siteID) == 4, # add "-"
#                          paste(substr(siteID, 1,2), "-", substr(siteID,3,4), sep = ""),
#                   ifelse(!grepl("-", siteID) & nchar(siteID) == 3, # add "-"
#                         paste(substr(siteID, 1, 1), "-", substr(siteID,2, 3), sep = ""),
#                         siteID))))),
#          Lake = ifelse(Lake == "BVR" & siteID == "U-14", # This is labeled BVR, but should be WGF
#                        "WGF",
#                        Lake),
#          # Bring in true lake name.  key is derived from masterLibrary
#          Lake_Name = translationKeydf[match(Lake, translationKeydf$site), "Lake_Name"])
# 
# # 11. merge properly formatted siteID back into chlFinal.
# chlFinal <- cbind(select(chlFinal, chla.sample, pheo.sample), 
#                   select(charIds, Lake_Name, siteID))
# 
# # 12. merge chl data into eqAreaData
# str(eqAreaData) #1426 observations
# eqAreaData <- merge(chlFinal, eqAreaData, all = TRUE)
# str(eqAreaData) # Still 1426, merged as expected
# 
# # 13. Read in chlorophyll measured in lab using data sonde
# sondeChl <- read_excel("ohio2016/inputData/J. Beaulieu Chlorophyll 2016/sondeChlCalibrationChecks.xlsx") 
# 
# # Strip unusual character names from column titles.
# # The escape (\\) is needed to get R to treat them as character.
# names(sondeChl) = gsub(pattern = c("\\(| |#|)|/"), 
#                        replacement = ".", 
#                        x = names(sondeChl))
# sondeChl <- mutate(sondeChl, FILTER.DATE = as.Date(FILTER.DATE))
# 
# # 14. Format lab based chl measurement of cal samples for merge
# # Pull out Filter date from spec based chl measurements.
# chlCal  <- mutate(chlCal, FILTER.DATE = substr(nmsampleID, 1, 8),
#                   FILTER.DATE = as.Date(FILTER.DATE, 
#                                         format = "%m%d%Y"))
# chlCal <- merge(sondeChl, 
#                 select(chlCal, chla.sample, FILTER.DATE))
