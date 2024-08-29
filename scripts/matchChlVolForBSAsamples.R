# Falls Lake samples sent to CIN from November 2018 and later were extracted
# within holding time, but not analyzed.  Same for samples from R10 2018 sampling for 
# SFP/WPL/BKL.  Dana drove these samples to BSA for
# analysis in September 2020.  BSA requested that we provide the volumes filtered
# for each sample.  

#  Read in list of samples sent to BSA for analysis (provided by Elovitz)
bsa <- read_excel(path = "../chlorophyll/chlSamplesSentToBSA.xlsx") %>% # relative path, up one directory
  filter(source == "AEBRR") %>% # pull out Falls Lakes samples
  select(sample.id, date)  # grab columns of interest

# inspect sample.id formatting from bsa
bsa$sample.id[1] # "20181119.FL.02.0.1.2.SW.UKN"

# inspect sample.id formatting from chl volumes
chlVol$sample.id[1] # "2017-09-07_1_0.1_1_unknown"  the first 1 is site, 0.1 is depth, 1 is replicate



# convert bsa to chlVol format
# strsplit splits each sample_id into a list element based on "."
# This is preferable to substr because it accomodates differences in number of digits used to report
# site # (i.e. 01 vs 1).

bsa <- bsa %>% 
  toEPA() %>% # uniform formatting used for chlVol
mutate(lake = strsplit(.$sample.id, "[.]") %>% # for most sample_id,  each element contains 8 items
         lapply(., function(x) x[2]) %>%  # extract 2nd element from each list element (BKL, WPL, SFP)
         unlist(.), # lapply produces a list, unlist to vector
  coc.collection.date = strsplit(.$sample.id, "[.]") %>% # for most sample_id,  each element contains 8 items
         lapply(., function(x) x[1]) %>%  # extract 1st element from each list element (date)
         unlist(.) %>%  # lapply produces a list, unlist to vector
         as.Date(.,format = "%Y%m%d"),
       site = strsplit(.$sample.id, "[.]") %>% # for most sample_id,  each element contains 8 items
         lapply(., function(x) x[3]) %>%  # extract 3rd element from each list element (site)
         unlist(.) %>% # lapply produces a list, unlist to vector
         as.numeric(), # assures uniform formatting (i.e. 01 becomes 1)
       site = ifelse(grepl("BLK", sample.id),  # if a blank
                     NA, # field blanks not associated with a site
                     site), # otherwise site
       sample.depth = strsplit(.$sample.id, "[.]") %>% # for most sample_id,  each element contains 8 items
         lapply(., function(x) paste0(x[4], ".", x[5])) %>%  # extract 4th and 5th element from each list element (depth)
         unlist(.) %>% as.numeric(),  # lapply produces a list, unlist to vector
       sample.depth = ifelse(grepl("BLK", sample.id),  # if a blank
                             NA, # field blanks not associated with a depth
                             sample.depth), # otherwise depth
       replicate = strsplit(.$sample.id, "[.]") %>% # for most sample_id,  each element contains 8 items
         lapply(., function(x) x[6]) %>%  # extract 6th element from each list element (replicate)
         unlist(.),
       replicate = ifelse(grepl("BLK", sample.id),  # if a blank
                          strsplit(.$sample.id, "[.]") %>% # for most sample_id,  each element contains 8 items
                            lapply(., function(x) x[5]) %>%  # extract 5th element for blanks
                            unlist(.), # field blanks not associated with a replicate
                          replicate), # otherwise replicate
       type = ifelse(grepl("BLK", sample.id),
                     "blank",
                     ifelse(grepl(c("unk|ukn"), sample.id, ignore.case = TRUE),
                            "unknown",
                            NA)),
       sample.id = paste(coc.collection.date, site, 
                         sample.depth, replicate, type, sep = "_")) %>% # reconstruct sample.id with uniform formatting
  filter(lake == "FL") %>% # exclude Army Corps lakes BKL, WPL, SFP
  select(sample.id)


# match bsa with chlVol
bsaVol <- inner_join(bsa, chlVol)
nrow(bsa) # 64 Falls Lake samples sent to BSA
nrow(bsaVol) # 64 matches, good got them all!

# reformat to be consistent with sample codes sent to BSA
# this gets most of the way there, but still not a perfect match
# I manually matched with sample ids in chlSamplesSentToBSA.xlsx
# I then copied the volume filtered over to 'Samples for Transport 1_JB.xlsx'
# which was sent back to Mike.
bsaVol %>%
  mutate(sample.id = gsub("-", "", sample.id),
         sample.id = gsub("_", ".", sample.id),
         sample.id = gsub("unknown", "UKN", sample.id),
         sample.id = gsub("blank", "BLK", sample.id)) %>% 
           print(n=Inf) %>%
  write.table(file = "../chlorophyll/chlVolForBSA.txt", row.names = FALSE)



