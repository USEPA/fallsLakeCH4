

# READ GC DATA----------------
# RTP excel files first

# 1. Create a list of files to read in. 
rootDir <- "C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)/ENSB Methane/gcData/"

fileNames <- list.files(path = rootDir, pattern = "*.xlsx") # look in all subdirectories


# 2.  Read in files
mylist1 <- list()  # Create an empty list to hold data

for (i in 1:length(fileNames)){  # for each file
  # read_excel is from the readxl package.
  data.i <- read_excel(paste(rootDir, fileNames[i], sep = ""),
                       skip = 4)
  mylist1[[i]] <- data.i
}

# 3.  Coerce list into dataframe via rbind
gc.rtp <- do.call("rbind", mylist1)  # Coerces list into dataframe.

# 4.  Remove ugly characters from column names
names(gc.rtp) = gsub(pattern = c("\\(| |#|%|)|/|-|\\+"), replacement = ".", x = names(gc.rtp))
names(gc.rtp) = tolower(names(gc.rtp))

# Format gc data
gc.rtp <-  gc.rtp %>%   
  mutate(sample = toupper(sample), # make sure all characters in sample code are uppercase
         ch4.ppm = ifelse(grepl(pattern = "BT", x = sample), # if bubble trap
                          ch4....or.ppm. * 10000, # convert % to ppm
                          ch4....or.ppm.)) %>% # else already ppm (DG or AIR)
  filter(# (?i) not case sensitive. Shouldn't be needed (see above), but cool!
         # https://stackoverflow.com/questions/8361589/turning-off-case-sensitivity-in-r
         !(grepl("(?i)STD", gc.rtp$sample)), # remove standards
         sample != "", # exclude blank rows
         !(grepl("(?i)BLANK", gc.rtp$sample))) %>% # remove blanks
  select(sample, ch4.ppm)

# Check for duplicates.  Should be none.
filter(gc.rtp, duplicated(sample,fromLast = TRUE) | duplicated(sample,fromLast = FALSE)) %>% arrange(sample)


# Now read in Cincy .txt files
# GC DATA---------------
gc.cin1 <- read.table(paste0(rootDir, "gcMasterFile2017updated2019-08-28.txt"),
                                col.names=c("sample", "n2o.ppm", "co2.ppm", "ch4.ppm", "flag.n2o",
                                            "flag.co2", "flag.ch4", "o2.ar.percent", "n2.perc", "o2.chk",
                                            "flag.n2", "flag.o2.ar"),
                                stringsAsFactors = FALSE,
                                #colClasses=c("character", rep("num", 3), rep("int", 3), rep("num", 2),
                                #             rep("logi", 2)),
                                skip=1) 

gc.cin2 <- read.table(paste0(rootDir, "gcMasterFile2018updated2019-04-09.txt"),
                                col.names=c("sample", "ch4.ppm", "co2.ppm", "n2o.ppm", "flag.n2o",
                                            "flag.co2", "flag.ch4"),
                                stringsAsFactors = FALSE,
                                #colClasses=c("character", rep("num", 3), rep("int", 3), rep("num", 2),
                                #             rep("logi", 2)),
                                skip=1)

# Merge and format cincy data
gc.cin <- bind_rows(select(gc.cin1, sample, 
                           n2o.ppm, co2.ppm, ch4.ppm, 
                           flag.n2o, flag.co2, flag.ch4,
                           o2.ar.percent, n2.perc,
                           flag.n2, flag.o2.ar),
                    select(gc.cin2, sample, 
                           n2o.ppm, co2.ppm, ch4.ppm, 
                           flag.n2o, flag.co2, flag.ch4)) %>%
  mutate(sample = toupper(sample)) %>% # uppercase sample IDs
  filter(grepl("FL", sample)) %>% # extract Falls Lake samples
  mutate(sample = replace(sample, 
                          sample == "082118FL07_2", # "DG" not entered
                          "082118FL07_DG2"),
         sample = ifelse(nchar(sample) == 12,
                         paste0(substr(sample, 1, 4), # mmdd
                                "2018.", #yy.
                                substr(sample, 5, 6), # FL
                                ".",
                                substr(sample, 7, 8), # site
                                ".",
                                substr(sample, 10, 11), # sample type
                                ".",
                                substr(sample, 12, 12)), # replicate
                         ifelse(nchar(sample) == 14,
                                paste0(substr(sample, 1, 4), # mmddyy
                                       "2018.", 
                                       substr(sample, 7, 8), # FL
                                       ".",
                                       substr(sample, 9, 10), # site
                                       ".",
                                       substr(sample, 12, 13), # sample type
                                       ".",
                                       substr(sample, 14, 14)), #replicate
                                NA))) # if none of these conditions are met

# Check for duplicates.
filter(gc.cin, duplicated(sample,fromLast = TRUE) | duplicated(sample,fromLast = FALSE)) %>% 
  arrange(sample)



# Merge CIN and RTP gc data
gc.all <- bind_rows(gc.rtp, gc.cin)
dim(gc.rtp) # 52, 2
dim(gc.cin) # 269, 11
dim(gc.all) # 321, 11, 269+52 = 321, yeah


# PREPARE EXETAINER CODES----------------------
# Extract from eqAreaData
xtrCodes <- filter(eqAreaData, EvalStatus == "sampled") %>%
  select(Lake_Name, siteID, ArExtnrs, DG_Extn, TrapExtn)

# Remove white space
xtrCodes[, c("ArExtnrs", "DG_Extn", "TrapExtn")] <- apply(X = xtrCodes[, c("ArExtnrs", "DG_Extn", "TrapExtn")],
                                                         MARGIN = 2, 
                                                         function(x) gsub(x, pattern = " ", replacement = ""))

# Split codes into separate fields
xtrCodes <- separate(xtrCodes, ArExtnrs, into = c("ar.xtr.1", "ar.xtr.2", "ar.xtr.3"), sep = ",") %>%
  separate(DG_Extn, into = c("dg.xtr.1", "dg.xtr.2", "dg.xtr.3"), sep = ",") %>%
  separate(TrapExtn, into = c("tp.xtr.1", "tp.xtr.2", "tp.xtr.3"), sep = ",")

# Melt  
xtrCodes.m <- melt(xtrCodes, id.vars = c("Lake_Name", "siteID")) %>% # melt, converts exetainer code to factor
  mutate(variable = as.character(variable)) %>% # Must go from factor -->character
  filter(!is.na(value))  # remove NAs

# Simplify variable names
xtrCodes.m[grepl(pattern = ".1|.2|.3", x = xtrCodes.m$variable), "variable"] <- 
  gsub(pattern = ".1|.2|.3", replacement = "", x = xtrCodes.m[grepl(pattern = ".1|.2|.3", x = xtrCodes.m$variable), "variable"])


# Check for duplicates.  Should be none.
filter(xtrCodes.m, duplicated(value,fromLast = TRUE) | duplicated(value,fromLast = FALSE)) %>% arrange(value)


# MERGE EXETAINER CODES WITH GC DATA-----

xtrCodes.gas <- merge(xtrCodes.m, gas.all, by.x = "value", by.y = "sample", all = TRUE)

str(xtrCodes.m)  # 52 observations
str(gas.all) # 52 observations, expected because data contains samples from other Acton 2017 monitoring
str(xtrCodes.gas) # 53 observations.  Acton Lake samples not yet run, plus see above.

# Example architecture for addressing specific issues that may arise
# omitCodes <- c(16170, # run on GC, but field notes indicate is bad and not entered in field sheets
#                16189, # 16189 air sample run w/trap samples.  Discard.
#                16199, 16200, # contaminated, per field sheets
#                16206, # bad, per field data sheets
#                16023, # chromatogram overwritten due to sequence problem
#                16475, # chromatogram overwritten due to sequence problem
#                16298, 16299, # contaminated, omit, per field sheets.
#                161235, 161281, 161257, 161262, 161266, 161258, # Cowan Lake cntl traps
#                161237, 16151, 161265, 16165, 161279, 16158, # Cowan Lake cntl traps
#                161267, 161268, 161269, # Harsha Lake MIT redeployment
#                16242, 16143, 161244, # Caesar Cr. MIT redeployment
#                16576,  # Empty short tube run on GC
#                16603, # trap sample, but no record in field sheets.
#                16825, # no record in field sheets
#                16614:16617, # no record in field sheets
#                16070) # Karen noted loose cap. Came from trap, but looks like air.
#                
# 
# 
# xtrCodes.gas <- filter(xtrCodes.gas, !(value %in% omitCodes))


# Take a look at values
ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(Lake_Name, ch4.ppm/10000)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))




# QA/QC GC REPS--------------
# Aggregate by Lake_Name and siteID, for now
# Inspect dups (i.e., sd, CV).  Follow up on dups that don't agree well.
xtrCodes.gas.g <- filter(xtrCodes.gas,
                              !is.na(ch4.ppm), # has GC data
                              !is.na(Lake_Name)) %>% # has lake name
                              group_by(Lake_Name, siteID, variable) # group for aggregation

xtrCodes.gas.agg <- summarise(xtrCodes.gas.g, 
                     ch4.sd=sd(ch4.ppm, na.rm=TRUE),
                     m.ch4.ppm=mean(ch4.ppm, na.rm=TRUE),
                     ch4.cv=(ch4.sd/m.ch4.ppm) * 100) %>%
  rename(ch4.ppm = m.ch4.ppm) 

xtrCodes.gas.agg <- ungroup(xtrCodes.gas.agg)  # This removes grouping, which complicates things down the line.

ggplot(xtrCodes.gas.agg, aes(siteID, ch4.ppm)) + # Everything appears to have agg correctly
  geom_point() +
  facet_grid(~variable, scales="free_y")

# MERGE RAW GC DATA WITH eqAreaData---------------
# Merge all gas samples.  Will calculate dissolved concentrations downstream.
# 1) Need to melt, which requires a data.frame, not a dplyr tbl_df.
# 2) melt creates a 'variable' column, already have 'variable' column
# in xtrCodes.gas.agg. Must rename first.
xtrCodes.gas.agg <- rename(xtrCodes.gas.agg, type = variable) # rename 'variable'

xtrCodes.gas.agg.m <- melt(as.data.frame(xtrCodes.gas.agg), # convert tbl_df to df
id.vars = c("Lake_Name", "siteID", "type")) # specify id variable

xtrCodes.gas.agg.m <- mutate(xtrCodes.gas.agg.m, type =  # adopt more intuitive names
                             ifelse(type == "tp.xtr", "trap",
                                    ifelse(type == "ar.xtr", "air", 
                                           ifelse(type == "dg.xtr", "dissolved",
                                                  type))))
  
xtrCodes.gas.agg.c <- dcast(xtrCodes.gas.agg.m,  # cast
                            Lake_Name + siteID ~ type + variable) 

# Merge
eqAreaData <- merge(xtrCodes.gas.agg.c, eqAreaData, all = TRUE)


