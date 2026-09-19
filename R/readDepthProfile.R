# Function for reading depth profile for SuRGE sites

# Data temporarily stored at SuRGE. Move to this repo when data entry is complete?
paths <- "C:/Users/JBEAULIE/Environmental Protection Agency (EPA)/SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents/data/RTP/CH4_1033_Falls_Lake"

get_depth_profile <- function(paths){
  #d <-  
    fs::dir_ls(path = paths, # see above
               regexp = 'DepthProfile', # file names containing this pattern
               ignore.case = TRUE, # DepthProfile or depthProfile, Depthprofile, etc
               recurse = TRUE) %>% # look in all subdirectories
    .[!grepl(c(".pdf|.docx|.txt"), .)] %>% # remove pdf and .docx review files
    #.[53] %>%
    # map will read each file in fs_path list generated above
    # imap passes the element name (here, the filename) to the function
    # purrr::imap(~excel_sheets(.x)) 
    
    purrr::imap(~read_excel(.x, sheet = "data", 
                            na = c("NA", "", "N/A", "n/a")) %>%
                  # Assign the filename to the visit column for now
                  mutate(visit = .y)) %>% # assign file name
    # remove empty dataframes.  Pegasus put empty Excel files in each lake
    # folder at beginning of season.  These files will be populated eventually,
    # but are causing issues with code below
    purrr::discard(~ nrow(.x) == 0) %>% 
    
    # format data
    map(., function(x) { 
      janitor::clean_names(x) %>%
        
        # janitor converts pH to p_h. fix here
        rename_with(~gsub("p_h", "ph", .), #specify sonde
                    contains("p_h")) %>%
        
        # clean chlorophyll names
        rename(chla_sonde = chl_a_ug_l) %>% # specify sonde
        rename(chla_sonde_flag = chla_flag) %>%
        rename(chla_sonde_comment = chla_comment) %>%
        
        # clean phycocyanin names. `any_of` won't throw error if column missing
        rename(any_of(c(phycocyanin_sonde = "phyc_ug_l",
                        phycocyanin_sonde_flag = "phyc_flag",
                        phycocyanin_sonde_comment = "phyc_comment"))) %>%
        
        # fix sp_cond
        rename(sp_cond_flag = cond_flag) %>%
        rename(sp_cond_comment = cond_comment) %>%
        
        # remove units from analyte names. Create units columns below
        rename(temp = temp_c) %>% #_c pattern below picks up _comment
        rename_with(~stringi::stri_replace_all_regex(.,
                                                     pattern = c("_m", "_mg_l", "_us_cm", "_ntu"),
                                                     replacement = ""), # remove units from name
                    matches(c("_m|_mg_l|_us_cm|_ntu"))) %>% # columns to apply function to
        # Assign value to visit based on the Excel file name
        mutate(visit = str_sub(visit, start = -11, end = -1) %>% # extract last 11 characters (isit13.xlsx)
                 str_extract(., "(\\d)+") %>% # extract numeric part (13)
                 as.numeric, # convert visit to numeric
               
               # format lake_id and site_id.  See Wiki
               lake_id = as.character(lake_id) %>%
                 tolower(.) %>% # i.e. Lacustrine -> lacustrine
                 str_remove(., "ch4_") %>% # remove any ch4_ from lake_id
                 str_remove(., "^0+"), #remove leading zeroes i.e. 078->78
               site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)),
               # address data-class conflicts; make classes identical
               across(contains("comment"), ~ as.character(.)),
               across(contains("flag"), ~ as.character(.)),
               across(contains("depth"), ~round(.x, 1)), # round to nearest tenth of meter
               # create units column for each analyte
               across(c("sample_depth", "temp", "do", "sp_cond", "chla_sonde"), 
                      ~ case_when(
                        str_detect(paste(cur_column()), "depth") ~ "m",
                        str_detect(paste(cur_column()), "temp") ~ "c",
                        str_detect(paste(cur_column()), "do") ~ "mg_l",
                        str_detect(paste(cur_column()), "sp_cond") ~ "us_cm",
                        str_detect(paste(cur_column()), "chl") ~ "ug_l",
                        TRUE ~ "FLY UOU FOOLS"), # 
                      .names = "{col}_units"),
               phycocyanin_units = "ug_l" # create column even if wasnt' measured.
        ) 
    }) %>%
    map_dfr(., bind_rows) # rbinds into one df
}



# Get data
depth_profile <- get_depth_profile(paths) 

write_csv(depth_profile, file = "output/falls_lake_depth_profiles.csv")
