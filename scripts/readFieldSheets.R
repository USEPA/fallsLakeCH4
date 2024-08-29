# READ FIELD SHEETS
paths <- paste0("C:\\Users\\JBEAULIE\\Environmental Protection Agency (EPA)\\",
                "SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents\\",
                "data\\RTP\\CH4_1033_Falls_Lake")

# 1. Create a list of file paths where the data are stored.  
# Function for reading 'data' tab of surgeData files.

get_data_sheet <- function(paths){
  #d <-  
  fs::dir_ls(path = paths, # see above
             regexp = 'surgeData', # file names containing this pattern
             recurse = TRUE) %>% # look in all subdirectories
    .[!grepl(c(".pdf|.docx"), .)] %>% # remove pdf and .docx review files
    #.[53] %>%
    # map will read each file in fs_path list generated above
    # imap passes the element name (here, the filename) to the function
    purrr::imap(~read_excel(.x, skip = 1, sheet = "data", 
                            na = c("NA", "", "N/A", "n/a")) %>%
                  # Assign the filename to the visit column for now
                  mutate(visit = .y,
                         visit = case_when(str_detect(visit, "September2017") ~ 1,
                                           str_detect(visit, "October2017") ~ 2,
                                           str_detect(visit, "November2017") ~ 3,
                                           str_detect(visit, "13January") ~ 4,
                                           str_detect(visit, "24January") ~ 5,
                                           str_detect(visit, "February2018") ~ 6,
                                           str_detect(visit, "March2018") ~ 7,
                                           str_detect(visit, "April2018") ~ 8,
                                           str_detect(visit, "May2018") ~ 9,
                                           str_detect(visit, "June2018") ~ 10,
                                           str_detect(visit, "July2018") ~ 11,
                                           str_detect(visit, "August2018") ~ 12,
                                           str_detect(visit, "September2018") ~ 13,
                                           str_detect(visit, "October2018") ~ 14,
                                           str_detect(visit, "November2018") ~ 15,
                                           TRUE ~ 999999999))) %>% # check for this value in final)) %>% # assign file name
    # remove empty dataframes.  Pegasus put empty Excel files in each lake
    # folder at beginning of season.  These files will be populated eventually,
    # but are causing issues with code below
    purrr::discard(~ nrow(.x) == 0) %>% 
    # format data
    map(., function(x) { 
      janitor::clean_names(x) %>%
        rename_with(~gsub("chl", "chla_sonde", .), #specify sonde
                    contains("chl")) %>%
        rename_with(~gsub("phyc", "phycocyanin_sonde", .), #specify sonde
                    contains("phyc")) %>%
        # format lake_id and site_id.  See Wiki
        mutate(lake_id = as.character(lake_id) %>%
                 tolower(.) %>% # i.e. Lacustrine -> lacustrine
                 str_remove(., "ch4_") %>% # remove any ch4_ from lake_id
                 str_remove(., "^0+"), #remove leading zeroes i.e. 078->78
               site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)),
               long = case_when(long > 0 ~ long * -1, # longitude should be negative
                                TRUE ~ long),
               # Empty columns cause data-class conflicts; make classes identical
               across(c(lat, long), ~ as.numeric(.)),
               across(contains("comment"), ~ as.character(.)),
               across(contains("flag"), ~ as.character(.)),
               across(contains("extn"), ~ as.character(.)),
               across(contains("depth"), ~round(.x, 1))) %>% # round to nearest tenth of meter
        # Format date and time objects
        mutate(across(contains("date"), ~ as.Date(.x, format = "%m.%d.%Y")), # convert date to as.Date
               across(contains("time"), ~ format(.x, format = "%H:%M:%S")), # convert time to character
               tz = lutz::tz_lookup_coords(lat, long, warn = FALSE)) %>% # this gets tz based on location
        # need to replace NA with a value, else force_tz throws error
        # `fill` will replace NA with non-NA value in column. Code assumes only one tz per lake
        # check that assumption for Missouri River impoundments
        fill(tz, .direction = "updown") %>% 
        # referencing tz via the tz argument in as.POSIXct throws an error. I don't really
        # understand why. Here we create date_time object without time zone, then enforce
        # local time zone with `force_tz`, finally display in UTC via `with_tz`
        mutate(trap_deply_date_time = as.POSIXct(x = paste0(trap_deply_date, trap_deply_time),
                                                 format = "%Y-%m-%d%H:%M:%S"),
               trap_rtrvl_date_time = as.POSIXct(x = paste0(trap_rtrvl_date, trap_rtrvl_time),
                                                 format = "%Y-%m-%d%H:%M:%S"),
               chamb_deply_date_time = as.POSIXct(x = paste0(chamb_deply_date, chamb_deply_time),
                                                  format = "%Y-%m-%d%H:%M:%S"),
               across(contains("date_time"), ~  force_tz(.x, tzone = tz) %>%
                        with_tz(., tzone = "UTC"))) %>%
        select(-tz) %>% # remove unneeded tz variable
        # chemistry data uses "flags" rather than "flag".  be consistent
        rename_with(~sub("flag", "flags", .),
                    .cols = contains("flag"))
    }) %>%
    map_dfr(., bind_rows) # rbinds into one df
}

# 3. Read 'data' tab of surgeData files.
fld_sheet <- get_data_sheet(paths = paths) 
unique(fld_sheet$lake_id)
unique(fld_sheet$site_id)
unique(fld_sheet$visit)
janitor::get_dupes(fld_sheet %>% select(lake_id, site_id, visit)) # no dups
dim(fld_sheet) #359, 82  [8/28/2024]

# 4. Function to read 'dissolved.gas' tab of surgeData file.
get_dg_sheet <- function(paths){
  #d <-  
  fs::dir_ls(path = paths, # see above
             regexp = 'surgeData', # file names containing this pattern
             recurse = TRUE) %>% # look in all subdirectories
    .[!grepl(c(".pdf|.docx"), .)] %>% # remove pdf and .docx review files
    # map will read each file in fs_path list generated above
    purrr::imap(~ read_excel(., skip = 1, sheet = "dissolved.gas", 
                             na = c("NA", "", "N/A", "n/a")) %>%
    # Assign the filename to the visit column, then map visit number
      # based on date in filename.
    mutate(visit = .y,
           visit = case_when(str_detect(visit, "September2017") ~ 1,
                             str_detect(visit, "October2017") ~ 2,
                             str_detect(visit, "November2017") ~ 3,
                             str_detect(visit, "13January") ~ 4,
                             str_detect(visit, "24January") ~ 5,
                             str_detect(visit, "February2018") ~ 6,
                             str_detect(visit, "March2018") ~ 7,
                             str_detect(visit, "April2018") ~ 8,
                             str_detect(visit, "May2018") ~ 9,
                             str_detect(visit, "June2018") ~ 10,
                             str_detect(visit, "July2018") ~ 11,
                             str_detect(visit, "August2018") ~ 12,
                             str_detect(visit, "September2018") ~ 13,
                             str_detect(visit, "October2018") ~ 14,
                             str_detect(visit, "November2018") ~ 15,
                             TRUE ~ 999999999))) %>% # check for this value in final
    # remove empty dataframes.  Pegasus put empty Excel files in each lake
    # folder at begining of season.  These files will be populated eventually,
    # but are causing issues with code below
    purrr::discard(~ nrow(.x) == 0) %>% 
    # format data
    map(., function(x){
      janitor::clean_names(x) %>%
        # Assign value to visit based on the Excel filename
        mutate(# format lake_id and site_id.  See Wiki
               lake_id = as.numeric(lake_id),
               site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)))
    }) %>%
    map_dfr(., identity) # rbinds into one df
}

# 5. Write data object for SuRGE
saveRDS(object = fld_sheet, 
        file = paste0("C:\\Users\\JBEAULIE\\Environmental Protection Agency (EPA)\\",
                      "SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents\\",
                      "data\\RTP\\CH4_1033_Falls_Lake\\falls_lake_fld_sheet.rds"))

# 6.  Read dissolved gas sheet
dg_sheet <- get_dg_sheet(paths = paths)

dg_sheet %>% print(n=Inf)

dg_sheet %>% filter(is.na(atm_pressure) | is.na(air_temperature) | 
                      is.na(water_vol) | is.na(air_vol)) %>%
  distinct(lake_id) %>% print(n=Inf)

######NOT UPDATED FROM SuRGE!!!!!!!!!!!!!!
# create object containing all exetainer codes for readGc.R
all_exet <- bind_rows(
  dg_sheet %>% # DG exetainers
    select(lake_id, site_id, visit, dg_extn) %>%
    rename(sample = dg_extn) %>%
    mutate(type = "dg"),
  fld_sheet %>% # air + trap exetainers
    select(lake_id, site_id, visit, trap_deply_date, matches("trap_extn|air_extn")) %>%
    select(!contains("notes")) %>%
    pivot_longer(!c(lake_id, site_id, visit, trap_deply_date), 
                 values_to = "sample") %>%
    mutate(sample = toupper(sample),
           type = case_when(grepl("trap", name) ~ "trap",
                            grepl("air", name) ~ "air",
                            TRUE ~ "Fly you fools")) %>%
    select(-name) %>%
    filter(!is.na(sample))
) %>%
  mutate(trap_deply_date = case_when(type == "trap" ~ trap_deply_date,
                                     TRUE ~ dttr2::NA_Date_))
dim(all_exet) #2713
  
  
  
#  # BAROMETRIC PRESSURE----------------
#  # Assign barometric pressure to dissolved gas sampling site where BP
#  # was not recorded.  All lakes have at least one measurement, so assign
#  # recorded value to missing sites.
#  eqAreaData <- group_by(eqAreaData, Lake_Name) %>% 
#    mutate(BrPrssr = 
#             # Select observation where dissolved gas was collected (i.e. anywhere a
#             # deep sonde measurement was made), but BP wasn't recorded
#             ifelse(!is.na(smDpthD) & is.na(BrPrssr),
#                    # Set BP equal to any other BP measured at the lake
#                    subset(BrPrssr, !is.na(BrPrssr)),
#                    BrPrssr)) # else return BP
# 
#  
#  # HEADSPACE GAS AND WATER VOLUMES----------------
#  # Water and gas volumes were not always recorded.  When they were,
#  # they weren't associated with a single sample. Assign mean values by lake.  If
#  # no data reported for lake, assume he=20ml and water =120ml. Data is recorded
#  # as character values.
#  
#  # Function for executing above
# volEst <- function(x, choice1) {
#   if (choice1 == "He") {
#     # Calculate mean He volume.  deal w/character values
#     vol <- strsplit(x, split = ",") %>% unlist() %>% as.numeric() %>% mean(na.rm = TRUE)
#     vol <- ifelse(is.nan(vol), 20, vol) # if not reported, assume 20mL
#   }
#   if (choice1 == "water") {
#     # Calculate mean water volume.  deal w/character values
#     vol <- strsplit(x, split = ",") %>% unlist() %>% as.numeric() %>% mean(na.rm = TRUE)
#     vol <- ifelse(is.nan(vol) | vol >= 140, # if not reported, or erroneous (cant be 140)
#                   120, vol) # assume 120mL
#   }
#   vol # return volume estimate
# }
#  
#  eqAreaData <- mutate(eqAreaData,
#                       HeVol = 
#                         # Select observation where dissolved gas was collected
#                         ifelse(!is.na(DG_Extn),
#                                # Set He volume equal to mean for lake
#                                volEst(HeVol, "He"),
#                                HeVol), # else return He
#                       H2O_vol = 
#                         # Select observation where dissolved gas was collected
#                         ifelse(!is.na(DG_Extn),
#                                # Set Water volume equal to mean for lake
#                                volEst(H2O_vol, "water"),
#                                H2O_vol)) %>% # else return H2O_vol
#    ungroup() %>% # remove grouping
#    as.data.frame() %>% # remove tbl_df class
#    mutate(HeVol = as.numeric(HeVol),
#           H2O_vol = as.numeric(H2O_vol))
#  
#  # CHAMBER VOLUME
#  # Calculate chamber volume based on relationship between water level
#  # and volume.  See chamberDesign.xlsx in East Fork folder.
#  eqAreaData <- mutate(eqAreaData, chmVol.L = (42.057 + (-0.2189 * chm_vol)))
#  
#  # Deal with instances where chamber volume was not recorded in field.
#  # 1.  A site or two missed, whereas volume recorded at most other sites.
#  # Caeaser Cr.
#  toAdjChmVol <- with(eqAreaData, Lake_Name == "Caesar Creek Lake" & EvalStatus == "sampled" & is.na(chmVol.L))
#  adjChmVol <- with(eqAreaData, Lake_Name == "Caesar Creek Lake" & EvalStatus == "sampled" & !is.na(chmVol.L))
#  estChemVol <- mean(eqAreaData[adjChmVol, "chmVol.L"])
#  eqAreaData[toAdjChmVol, "chmVol.L"] =  estChemVol
#  
#  # Apple Valley
#  toAdjChmVol <- with(eqAreaData, Lake_Name == "Apple Valley Lake" & EvalStatus == "sampled" & is.na(chmVol.L))
#  adjChmVol <- with(eqAreaData, Lake_Name == "Apple Valley Lake" & EvalStatus == "sampled" & !is.na(chmVol.L))
#  estChemVol <- mean(eqAreaData[adjChmVol, "chmVol.L"])
#  eqAreaData[toAdjChmVol, "chmVol.L"] =  estChemVol
#  
#  # Lake Waynoka
#  toAdjChmVol <- with(eqAreaData, Lake_Name == "Lake Waynoka" & EvalStatus == "sampled" & is.na(chmVol.L))
#  adjChmVol <- with(eqAreaData, Lake_Name == "Lake Waynoka" & EvalStatus == "sampled" & !is.na(chmVol.L))
#  estChemVol <- mean(eqAreaData[adjChmVol, "chmVol.L"])
#  eqAreaData[toAdjChmVol, "chmVol.L"] =  estChemVol 
#  
#  # 2.  Pleasent Hill (PH) sampled day before Charles Mill (CM).  No volume measurements
#  # made at CM; replace with mean from PH.
#  # First, create logical for conditions that need to be replaced
#  adjChmVol <- eqAreaData$Lake_Name == "Charles Mill Lake" & eqAreaData$EvalStatus == "sampled"
#  eqAreaData[adjChmVol, "chmVol.L"] = 
#    mean(eqAreaData[eqAreaData$Lake_Name == "Pleasant Hill Lake", "chmVol.L"], na.rm = TRUE)

 