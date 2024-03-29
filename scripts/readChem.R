# SCRIPT TO READ IN WATER CHEM



# READ AND FORMAT WATER CHEM---------------
# Data retrieved from L drive on 11/09/2016
chem17 <- read_excel(paste0("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)/ENSB Methane/waterChemistry/",
                            "2017_ESF-EFWS_NutrientData_Updated02112019_SS_CTNWorked03192019.xlsx"), 
                   sheet = "2017DATA", skip = 1)

chem18 <-  read_excel(paste0("C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)/ENSB Methane/waterChemistry/",
                             "2018_ESF-EFWS_NutrientData_Updated03012019_SS_CTNUpdate04012019.xlsx"),
                     sheet = "2018DATA", skip = 1)

chem <- bind_rows(chem17, chem18)
dim(chem17) # 44851 46
dim(chem18) # 45986 45
dim(chem) # 90837, 46   44851+45986=90837

# Replace spaces and unusual characters in column names with ".".
# Note that "(" is a special character in R.  Must precdede with \\ for
# R to read as character.
names(chem) = gsub(pattern = c("\\(| |#|)|/|-"), replacement = ".", x = names(chem))
names(chem) = tolower(names(chem))
chem <- rename(chem, 
               finalConc = peak.concentration..corrected.for.dilution.factor.,
               analyte = analyte.name..analy.,
               Lake_Name = site.id..id.,
               siteID = long.id..subid.) %>%
  mutate(rdate = as.Date(collection.date..cdate.)) %>%
  select(rdate, Lake_Name, siteID, type, analyte, finalConc,
         unit)

# Check units and analytes
distinct(chem, unit) # all ug, except mg C/L for TOC and mg N/L for TN
distinct(chem, analyte) # 


#############################################
# Pull out Falls Lake data
# Both FL and FL.53 in Lake_Name field.
# FL.53 is of no consequence

fChem <- filter(chem, Lake_Name == "FL")
             
# Change 'FL' to 'Falls Lake' 
fChem <- mutate(fChem, 
                type = ifelse(grepl("UKN", type), "UNK", type),
                Lake_Name = "Falls Lake")


# Quick and dirty QA/QC review
# Blanks, a few TN are pretty high, but low compared to unknowns
ggplot(filter(fChem, type == "BLK"), aes(rdate, finalConc)) + 
  geom_point() + 
  facet_wrap(~analyte, scales = "free")

# Replicates
repIdentifier <- filter(fChem, type == "DUP") %>% # PUll out unique ID
  select(-finalConc, -unit, -type) # omit these field

# Reduce to only dups and associated unknowns
repsAll <- merge(repIdentifier, # Pull out unknown and dup value
                 filter(fChem, type == "UNK" | type == "DUP"))

dim(filter(fChem, type == "UNK" | type == "DUP")) #1403, 7
dim(repIdentifier) # 269, 4
dim(repsAll) #679 7


# Dups agree very well.  
ggplot(filter(repsAll, analyte == "TN"),
       aes(rdate, finalConc)) + # plot
  geom_point() + 
  facet_wrap(~siteID + analyte, scales = "free")

ggplot(filter(repsAll, analyte == "TP"),
       aes(rdate, finalConc)) + # plot
  geom_point() + 
  facet_wrap(~siteID + analyte, scales = "free")

# Aggregate across dups.
fChemAgg <- filter(fChem, type != "BLK", type != "SPK", siteID != "LAB") %>% # remove spike and dups
  group_by(rdate, Lake_Name, siteID, analyte, unit) %>% # grouping variable
  summarize(finalConc = mean(finalConc)) %>% # calculate mean across groups
  ungroup() # remove grouping structure



###########################################################
# FINALIZE FOR MERGING AND ANALYSIS
# MERGING STRATEGY
# 1) merge station specific data into eqAreaData.  This puts all sonde and water
#    chem data in one place.  Push these data through grtsMeanVariance to get
#    lake-wide mean.

# Take a quick peak for obvious problems
#  siteID == 991 | 992?  weird, but this is in 10-25-2017 Falls Lake CoC
# siteID == LAB ?
ggplot(fChemAgg, aes(rdate, finalConc)) + 
  geom_point(aes(color = siteID)) + 
  facet_wrap(~analyte, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))


# Cast to wide for merge with eqAreaData
fChemAgW <- reshape2::dcast(fChemAgg, Lake_Name + siteID + rdate~ analyte, 
                        value.var = "finalConc")

# Strip ugly characters from analyte names
names(fChemAgW) = gsub(pattern = c("\\(| |#|)|/|-"), 
                             replacement = ".", 
                             x = names(fChemAgW))


# Merge with eqAreaData
str(eqAreaData) # 72 observations
eqAreaData <- merge(fChemAgW, eqAreaData, all = TRUE)
str(eqAreaData) # Still 1426, merged as expected

# Look for missing data
# See issues.R
select(eqAreaData, Lake_Name, siteID, TN, TNH4, TNO2, TNO2.3, TOC, TP, TRP) %>%
  filter(!is.na(TP))

 