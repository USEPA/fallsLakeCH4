
# READ GC DATA----------------

get_gc_data <- function(
  visit_date_map, 
  rtp_gc_data_file, 
  epa_gc_data_file
){
## RTP excel files first----

# 1.  read RTP data
gc_rtp <- readxl::read_xlsx(
  path = rtp_gc_data_file,
  na = c("NA", "-", ""),
  sheet = "Reduced data",
  skip = 2
) %>%
   janitor::clean_names() %>%
  rename(
    sample = description,
    ch4_ppm = reported_concentration_percent_bt_or_ppm_dg_aa
  ) %>%
  dplyr::filter(
    dplyr::if_any(dplyr::everything(), ~ !is.na(.x)),
  !is.na(ch4_ppm)
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
      grepl(pattern = "BT", x = sample) ~ ch4_ppm * 10000, # convert % to ppm, # if trap trap
      TRUE ~ ch4_ppm
    ) # else already ppm (DG or AIR)
  ) %>%
  filter(
    !(sample == "VISIT.4.FL.05.BT.1" & ch4_ppm == 696000), # 01122018
    !(sample == "VISIT.4.FL.59.BT.4"), # 01122018
    # Exclude the RTP sample with an invalid '?' replicate identifier.
    sample != "VISIT.1.FL.12.DG.?"
  ) %>%
  select(sample, ch4_ppm, visit)

# Check for duplicates.  Should be none.
janitor::get_dupes(gc_rtp, sample)


## CIN .txt files-----
gc_cin <- readr::read_delim(
  epa_gc_data_file,
  delim = "\t",
  col_names = c(
    "sample", "ch4.ppm", "co2.ppm", "n2o.ppm", "flag.n2o",
    "flag.co2", "flag.ch4"
  ),
  skip = 1,
  show_col_types = FALSE
) %>%
  janitor::clean_names() %>%
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



## Merge CIN and RTP gc data----
gc_all <- bind_rows(gc_rtp, gc_cin) %>%
  mutate(
    # Extract characters between the third and fourth "." and assign to site_id.
    site_id = as.numeric(stringr::str_match(sample, "^(?:[^.]*\\.){3}([^.]+)\\.")[, 2]),
    # create sample_type column
    sample_type = case_when(
      grepl("BT", sample) ~ "trap",
      grepl("AA", sample) ~ "air",
      grepl("DG", sample) ~ "dissolved",
      TRUE ~ "other"
    ),
    # sample replicate number. Final character of sample
    rep = (stringr::str_extract(sample, "\\d$"))
  )
dim(gc_rtp) # 429, 3
dim(gc_cin) # 268, 12
dim(gc_all) # 697, 12, 429+268 = 697, yeah


# INSPECT VALUES------
  # modify arguments to filter for different sample types, sites, and visits
gc_all %>%
  mutate(
    # convert to factor for plotting
    across(c(site_id, rep, visit), as.factor)
  ) %>%
  select(-contains("flag")) %>%
  pivot_longer(cols = !c(sample, site_id, visit, rep, sample_type)) %>%
  filter(!is.na(value), name == "ch4_ppm", sample_type == "dissolved") %>% # change sample type...
  ggplot(aes(visit, value)) + # value/10000 for trap
  geom_point() +
  facet_wrap(~site_id, scales = "free")

 # flaged values?
 # these are all air samples with very reasonable results. will retain them.
 gc_all |>
   dplyr::filter(
     dplyr::if_any(
       dplyr::contains("flag"),
       ~ !is.na(.x)
     )
   ) %>% 
   print(n=Inf)
  
  
# MAKE CORRECTIONS AND AGGREGATE-----  
gc_all_corrected_agg <- gc_all %>%
  filter(
  # make corrections based on above inspections
  # air samples
    !(sample_type == "air" & ch4_ppm > 4),
    # trap samples
    !(sample_type == "trap" & site_id == 14 & visit == 4), # CH4 = 0.21
    !(sample_type == "trap" & site_id == 24 & visit == 4), # CH4 = 0.07
    !(sample_type == "trap" & site_id == 24 & visit == 9), # CH4 = 0.15
    !(sample_type == "trap" & site_id == 47 & visit == 14), # CH4 is tiny
    !(sample_type == "trap" & site_id == 52 & visit == 3), # CH4 is tiny
    !(sample_type == "trap" & site_id == 54 & visit == 4), # CH4 is tiny
    !(sample_type == "trap" & site_id == 59 & visit == 4), # CH4 is tiny
    # dissolved samples
    !(sample_type == "dissolved" & site_id == 5 & visit == 10 & rep == 3), # much higher than rep
    !(sample_type == "dissolved" & site_id == 7 & visit == 1 & rep == 2), # much higher than rep
    !(sample_type == "dissolved" & site_id == 42 & visit == 1 & rep == 2) # much higher than rep
  ) %>%
    select(-contains("flag")) %>%
    group_by(site_id, visit, sample_type) %>% # this will aggregate replicates within each site and visit
    summarise(
      across(
        c(ch4_ppm, co2_ppm, n2o_ppm),
        ~ mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
      rename(
        ch4_gc = ch4_ppm,
        co2_gc = co2_ppm,
        n2o_gc = n2o_ppm
      ) %>%
        mutate(
          ch4_gc_units = "ppm",
          co2_gc_units = "ppm",
          n2o_gc_units = "ppm"
        ) %>%
        pivot_wider(
          names_from = sample_type,
          values_from = -c(site_id, visit, sample_type),
          names_glue = "{sample_type}_{.value}"
        )


  # Count air samples by site.
  gc_all_corrected_agg %>%
    filter(!is.na(air_ch4_gc)) %>%
    count(site_id, name = "n_air_samples")

  # Count trap samples by site.
  gc_all_corrected_agg %>%
    filter(!is.na(trap_ch4_gc)) %>%
    count(site_id, name = "n_trap_samples") %>%
    print(n = Inf)

  # Count dissolved samples by site.
  gc_all_corrected_agg %>%
    filter(!is.na(dissolved_ch4_gc)) %>%
    count(site_id, name = "n_dissolved_samples") %>%
    print(n = Inf)

gc_data <- gc_all_corrected_agg

# RETURN DATA----
  #   
  return(gc_data)
}