# READ FIELD SHEETS
get_field_sheets <- function() {
  paths <- "data/field_sheets/surge_format"

  # FIELD DATA-----
  ## Function for reading 'data' tab of surgeData files----

  get_data_sheet <- function(paths) {
    #d <-
    fs::dir_ls(
      path = paths, # see above
      regexp = 'surgeData1033', # file names containing this pattern
      recurse = TRUE
    ) %>% # look in all subdirectories
      .[!grepl(c(".pdf|.docx"), .)] %>% # remove pdf and .docx review files
      #.[12:13] %>%
      # map will read each file in fs_path list generated above
      # imap passes the element name (here, the filename) to the function
      purrr::imap(
        ~ readxl::read_xlsx(
          .x,
          skip = 1,
          sheet = "data",
          na = c("NA", "", "N/A", "n/a")
        ) %>%
          # Assign the filename to the visit column for now
          mutate(
            visit = .y,
            visit = case_when(
              str_detect(visit, "September2017") ~ 1,
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
              TRUE ~ 999999999
            )
          ) # check for this value in final close mutate
      ) %>% # close imap
      # format data
      map(., function(x) {
        janitor::clean_names(x) %>%
          rename_with(
            ~ gsub("chl", "chla_sonde", .), #specify sonde
            contains("chl")
          ) %>%
          rename_with(
            ~ gsub("phyc", "phycocyanin_sonde", .), #specify sonde
            contains("phyc")
          ) %>%
          # format lake_id and site_id.  See Wiki
          mutate(
            site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)),
            long = case_when(
              long > 0 ~ long * -1, # longitude should be negative
              TRUE ~ long
            ),
            # Remove a leading mmddyyyy date and prefix the visit number.
            across(
              c(
                trap_extn1,
                trap_extn2,
                trap_extn3,
                air_extn_1,
                air_extn_2,
                air_extn_3
              ),
              ~ stringr::str_replace_all(
                stringr::str_replace(
                  case_when(
                    is.na(.) ~ NA_character_,
                    stringr::str_detect(
                      as.character(.),
                      "^\\d{8}"
                    ) ~ toupper(paste0(
                      "visit.",
                      visit,
                      stringr::str_remove(as.character(.), "^\\d{8}")
                    )),
                    TRUE ~ toupper(paste0(
                      "visit.",
                      visit,
                      ".",
                      as.character(.)
                    ))
                  ),
                  "\\.0([123])$",
                  ".\\1"
                ),
                c("A$" = "1", "B$" = "2", "C$" = "3")
              )
            ),
            # Empty columns cause data-class conflicts; make classes identical
            across(c(lat, long), ~ as.numeric(.)),
            across(contains("comment"), ~ as.character(.)),
            across(contains("flag"), ~ as.character(.)),
            across(contains("extn"), ~ as.character(.)),
            across(contains("depth"), ~ round(.x, 1))
          ) %>% # round to nearest tenth of meter
          # Format date and time objects
          # Time recorded from LGR, which is eastern. Confirmed by comparing LGR times
          # recorded on field sheets with GPS data recorded in UTC.
          mutate(
            across(contains("date"), ~ as.Date(.x, format = "%m.%d.%Y")), # convert date to as.Date
            across(contains("time"), ~ format(.x, format = "%H:%M:%S")), # convert time to character
            tz = lutz::tz_lookup_coords(lat, long, warn = FALSE)
          ) %>% # this gets tz based on location
          # need to replace NA with a value, else force_tz throws error
          # `fill` will replace NA with non-NA value in column. Code assumes only one tz per lake
          # check that assumption for Missouri River impoundments
          fill(tz, .direction = "updown") %>%
          # referencing tz via the tz argument in as.POSIXct throws an error. I don't really
          # understand why. Here we create date_time object without time zone, then enforce
          # local time zone with `force_tz`, finally display in UTC via `with_tz`
          mutate(
            trap_deply_date_time = as.POSIXct(
              x = paste0(trap_deply_date, trap_deply_time),
              format = "%Y-%m-%d%H:%M:%S"
            ),
            trap_rtrvl_date_time = as.POSIXct(
              x = paste0(trap_rtrvl_date, trap_rtrvl_time),
              format = "%Y-%m-%d%H:%M:%S"
            ),
            chamb_deply_date_time = as.POSIXct(
              x = paste0(chamb_deply_date, chamb_deply_time),
              format = "%Y-%m-%d%H:%M:%S"
            ),
            across(
              contains("date_time"),
              ~ force_tz(.x, tzone = tz) %>%
                with_tz(., tzone = "UTC")
            )
          ) %>%
          select(-tz) %>% # remove unneeded tz variable
          # chemistry data uses "flags" rather than "flag".  be consistent
          rename_with(~ sub("flag", "flags", .), .cols = contains("flag"))
      }) %>%
      map_dfr(., bind_rows) %>%
      select(-lake_id) # remove lake identifier from field-sheet output
  }

  ## Read 'data' tab of surgeData files-----
  fld_sheet <- get_data_sheet(paths = paths)
  unique(fld_sheet$site_id)
  unique(fld_sheet$visit)
  janitor::get_dupes(fld_sheet %>% select(site_id, visit)) # no dups
  dim(fld_sheet) #449, 81

  # DISSOLVED GAS DATA----
  ## Function to read 'dissolved.gas' tab of surgeData file----
  get_dg_sheet <- function(paths) {
    #d <-
    fs::dir_ls(
      path = paths, # see above
      regexp = 'surgeData', # file names containing this pattern
      recurse = TRUE
    ) %>% # look in all subdirectories
      .[!grepl(c(".pdf|.docx"), .)] %>% # remove pdf and .docx review files
      # map will read each file in fs_path list generated above
      purrr::imap(
        ~ readxl::read_xlsx(
          .,
          skip = 1,
          sheet = "dissolved.gas",
          na = c("NA", "", "N/A", "n/a")
        ) %>%
          # Assign the filename to the visit column, then map visit number
          # based on date in filename.
          mutate(
            visit = .y,
            visit = case_when(
              str_detect(visit, "September2017") ~ 1,
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
              TRUE ~ 999999999
            )
          )
      ) %>% # check for this value in final
      # remove empty dataframes.  Pegasus put empty Excel files in each lake
      # folder at begining of season.  These files will be populated eventually,
      # but are causing issues with code below
      purrr::discard(~ nrow(.x) == 0) %>%
      # format data
      map(., function(x) {
        janitor::clean_names(x) %>%
          # Format site and dissolved-gas extension identifiers.
          mutate(
            site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)),
            dg_extn = stringr::str_replace_all(
              stringr::str_replace(
                dplyr::case_when(
                  is.na(dg_extn) ~ NA_character_,
                  stringr::str_detect(
                    as.character(dg_extn),
                    "^\\d{8}"
                  ) ~ toupper(paste0(
                    "visit.",
                    visit,
                    stringr::str_remove(as.character(dg_extn), "^\\d{8}")
                  )),
                  TRUE ~ toupper(paste0(
                    "visit.",
                    visit,
                    ".",
                    as.character(dg_extn)
                  ))
                ),
                "\\.0([123])$",
                ".\\1"
              ),
              c("A$" = "1", "B$" = "2", "C$" = "3")
            )
          )
      }) %>%
      map_dfr(., identity) %>%
      select(-lake_id) # remove lake identifier from dissolved-gas output
  }

  ## Read dissolved gas sheet-----
  dg_sheet <- get_dg_sheet(paths = paths)

  ## Inspect notes field-----
  # strip VISIT.3.FL.49.DG.1. Notes indicate two samples in one vial
  # note pertaining to VISIT.6.FL.49.DG.2 and VISIT.11.FL.49.DG.2 is inconsequential
  # remove notes column after making the changes above
  dg_sheet %>%
    filter(!is.na(dg_notes))

  dg_sheet <- dg_sheet %>%
    filter(dg_extn != "VISIT.3.FL.49.DG.1") %>%
    select(-dg_notes)

  # look at a few other fields
  # lots of missing BP and air temp. will impute based on data in field sheets
  dg_sheet %>%
    filter(is.na(water_vol) | is.na(air_vol) | is.na(atm_pressure)) %>%
    print(n = Inf)

  ## Aggregate-----
  # The dissolved gas sample GC data are already aggregated into a mean value
  # in gc_data. dg_sheet contains separate records for each rep, but the
  # numbers are identical between reps.
  dg_sheet %>%
    mutate(
      # extract final character of dg_extn to identify replicate number
      rep = as.integer(stringr::str_extract(dg_extn, "\\d$"))
    ) %>%
    group_by(site_id, sample_depth_m, visit) %>%
    filter(n() > 1) %>%
    summarise(
      n_records = n(),
      reps = paste(sort(rep), collapse = ", "),
      identical = n_distinct(pick(-dg_extn, -rep)) == 1,
      .groups = "drop"
    ) %>%
    print(n = Inf)

  # Since duplicates are identical, collapse each set of reps into a single record.
  # Also, all gas samples were collected at a depth of 0.1m, so we can drop the sample_depth_m column.
  dg_sheet <- dg_sheet %>%
    group_by(site_id, visit) %>%
    slice(1) %>% # grabs first record of each group, which is identical to the other records in the group
    ungroup() %>%
    select(
      # drop the sample_depth_m column since all values are 0.1m
      -sample_depth_m,
      -dg_extn
    ) # remove the dg_extn column since it is not needed for analysis

# JOIN FIELD AND DISSOLVED GAS SHEETS----
field_sheets_joined <- dplyr::full_join(
    fld_sheet,
    dg_sheet,
    by = c("site_id", "visit"), # unique identifiers for each record
    suffix = c("_fld", "_dg")
  )

  # any site_id by visit records in dg_sheet that are not in fld_sheet?
  # no, all present. good.
  dg_sheet %>%
    dplyr::distinct(site_id, visit) %>%
    dplyr::anti_join(
      fld_sheet %>% dplyr::distinct(site_id, visit),
      by = c("site_id", "visit")
    )


   
  # ######NOT UPDATED FROM SuRGE!!!!!!!!!!!!!!
  # # create object containing all exetainer codes for readGc.R
  # all_exet <- bind_rows(
  #   dg_sheet %>% # DG exetainers
  #     select(lake_id, site_id, visit, dg_extn) %>%
  #     rename(sample = dg_extn) %>%
  #     mutate(type = "dg"),
  #   fld_sheet %>% # air + trap exetainers
  #     select(lake_id, site_id, visit, trap_deply_date, matches("trap_extn|air_extn")) %>%
  #     select(!contains("notes")) %>%
  #     pivot_longer(!c(lake_id, site_id, visit, trap_deply_date),
  #                  values_to = "sample") %>%
  #     mutate(sample = toupper(sample),
  #            type = case_when(grepl("trap", name) ~ "trap",
  #                             grepl("air", name) ~ "air",
  #                             TRUE ~ "Fly you fools")) %>%
  #     select(-name) %>%
  #     filter(!is.na(sample))
  # ) %>%
  #   mutate(trap_deply_date = case_when(type == "trap" ~ trap_deply_date,
  #                                      TRUE ~ dttr2::NA_Date_))
  # dim(all_exet) #2713

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

  # # 5. Write data object for SuRGE
  # saveRDS(object = fld_sheet,
  #         file = paste0("C:\\Users\\JBEAULIE\\Environmental Protection Agency (EPA)\\",
  #                       "SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents\\",
  #                       "data\\RTP\\CH4_1033_Falls_Lake\\falls_lake_fld_sheet.rds"))


field_sheets <- field_sheets_joined
  
return(field_sheets)
}