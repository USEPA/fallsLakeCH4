
# READ GC DATA----------------

get_gc_data <- function(
  visit_date_map, 
  rtp_gc_data_file, 
  epa_gc_data_file_1, 
  epa_gc_data_file_2
){
# RTP excel files first

# 1.  read RTP data
gc_rtp <- readxl::read_xlsx(
  path = rtp_gc_data_file,
  na = c("NA", "-", ""),
  sheet = "Reduced data",
  skip = 2
) %>%
  dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.x))) %>%
  janitor::clean_names() %>%
  rename(
    sample = description,
    ch4_ppm = reported_concentration_percent_bt_or_ppm_dg_aa
  ) %>%
  mutate(
    sample_date = as.Date(substr(sample, 1, 8), format = "%m%d%Y"),
    visit = visit_date_map$visit[match(sample_date, visit_date_map$sample_date)],
    sample = stringr::str_replace_all(
      stringr::str_replace(
        toupper(paste0(
          "visit.",
          visit,
          stringr::str_remove(sample, "^\\d{8}")
        )),
        "\\.0([123])$",
        ".\\1"
      ),
      c("A$" = "1", "B$" = "2", "C$" = "3")
    ),
    ch4_ppm = case_when(
      grepl(pattern = "BT", x = sample) ~ ch4_ppm * 10000, # convert % to ppm, # if bubble trap
      TRUE ~ ch4_ppm
    ) # else already ppm (DG or AIR)
  ) %>%
  filter(
    !(sample == "01122018.FL.05.BT.1" & ch4_ppm == 696000),
    !(sample == "01122018.FL.59.BT.4")
  ) %>%
  select(sample, ch4_ppm)

# Check for duplicates.  Should be none.
janitor::get_dupes(gc_rtp, sample)


# Now read in Cincy .txt files
# GC DATA---------------
gc_cin1 <- readr::read_delim(
  epa_gc_data_file_1,
  delim = "\t",
  col_names = c(
    "sample", "n2o.ppm", "co2.ppm", "ch4.ppm", "flag.n2o",
    "flag.co2", "flag.ch4", "o2.ar.percent", "n2.perc", "o2.chk",
    "flag.n2", "flag.o2.ar"
  ),
  skip = 1,
  show_col_types = FALSE
)

gc_cin2 <- readr::read_delim(
  epa_gc_data_file_2,
  delim = "\t",
  col_names = c(
    "sample", "ch4.ppm", "co2.ppm", "n2o.ppm", "flag.n2o",
    "flag.co2", "flag.ch4"
  ),
  skip = 1,
  show_col_types = FALSE
)

# Merge and format cincy data
gc_cin <- bind_rows(
  gc_cin1 %>%
    select(
      sample,
      n2o.ppm, co2.ppm, ch4.ppm, o2.ar.percent, n2.perc, 
      flag.n2o, flag.co2, flag.ch4, flag.n2, flag.o2.ar
    ),
  gc_cin2 %>%
    select(sample, 
      n2o.ppm, co2.ppm, ch4.ppm, 
      flag.n2o, flag.co2, flag.ch4
    )
) %>%
  mutate(sample = toupper(sample)) %>% # uppercase sample IDs
  filter(grepl("FL", sample)) %>% # extract Falls Lake samples
  mutate(
    # Correct the one sample whose date includes the year in the source ID.
    sample = replace(
      sample,
      sample == "082118FL07_2",
      "0821FL07_DG2"
    ),
    # Data inspection found five gc_cin2 samples labeled 0812 that should be 0821.
    sample = stringr::str_replace(
      sample,
      "^0812(?=FL)",
      "0821"
    )
  ) %>%
  # Data inspection found no matching visit for the 0525 record; exclude it.
  filter(sample != "0525FL02_AA4") %>%
  mutate(
    # Derive visit number from the corrected sample date.
    sample_mmdd = substr(sample, 1, 4),
    visit = visit_date_map$visit[
      match(
        sample_mmdd,
        format(visit_date_map$sample_date, "%m%d")
      )
    ],
    # add visit number to sample ID, remove date from sample ID
    sample = stringr::str_replace_all(
      stringr::str_replace(
        toupper(paste0(
          "VISIT.",
          visit,
          ".",
          # Remove MMDD and the optional two-digit 2018 year code.
          stringr::str_remove(sample, "^\\d{4}(?:18)?")
        )),
        "\\.0([123])$",
        ".\\1"
      ),
      # sometimes reps are coded with letters other times with numbers.  Standardize to numbers.
      c("A$" = "1", "B$" = "2", "C$" = "3")
    )
  ) %>%
  mutate(
    sample = str_replace_all(sample, "FL", "FL."), # add period after FL
    sample = str_replace_all(sample, "_", "."), # replace underscores with periods
    sample = str_replace(sample, "(.)$", ".\\1") # add period before last character
  ) %>%
  select(-sample_mmdd)

# Check for duplicates.
gc_cin %>% janitor::get_dupes(sample) # none



# Merge CIN and RTP gc data
gc_all <- bind_rows(gc_rtp, gc_cin)
dim(gc_rtp) # 444, 2
dim(gc_cin) # 268, 12
dim(gc_all) # 712, 13, 444+268 = 712, yeah

#####################################
  ###PICK UP HERE. INSPECT VALUES
# Take a look at values
ggplot(filter(gc_all, variable == "tp.xtr"), aes(Lake_Name, ch4.ppm/10000)) + 
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


}