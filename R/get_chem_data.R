# SCRIPT TO READ IN WATER CHEM
get_chem_data <- function(
  chem_data_2017_file,
  chem_data_2018_file,
  toc_data_file
) {

  # READ NUTRIENTS----
  # Data retrieved from L drive on 11/09/2016
  nutrients <- bind_rows(
    readxl::read_xlsx(chem_data_2017_file, sheet = "2017DATA", skip = 1),
    readxl::read_xlsx(chem_data_2018_file, sheet = "2018DATA", skip = 1)
  ) %>%
    janitor::clean_names() %>%
    rename(
      final_conc = peak_concentration_corrected_for_dilution_factor,
      analyte = analyte_name_analy,
      lake_name = site_id_id,
      site_id = long_id_subid,
      sample_type = type,
    ) %>%
    # Pull out Falls Lake data
    # Both FL and FL.53 in lake_name field.
    # FL.53 is of no consequence
    filter(
      lake_name == "FL",
      sample_type != "SPK" # remove spikes
    ) %>%
    mutate(sample_date = as.Date(collection_date_cdate)) %>%
    select(sample_date, site_id, sample_type, analyte, final_conc, unit) %>%
    mutate(
      sample_type = case_when(
        grepl("UKN", sample_type) ~ "UNK",
        TRUE ~ sample_type
      ),
      site_id = case_when(
        sample_type == "BLK" ~ NA_integer_,
        TRUE ~ as.numeric(site_id) # will give warning, OK
      )
    )

  # READ TOC-TN DATA----
  toc <- foreign::read.dbf(toc_data_file, as.is = TRUE) %>%
    tibble %>%
    janitor::clean_names() %>%
    slice(-1) %>% # remove garbage first row
    select(
      # remove unnecessary fields
      -labid,
      -studyid,
      -flag,
      -comment, # a few TOC matrix spike issues, but not worth worrying about
      # rename
      sample_date = colldate
    ) %>%
    filter(
      grepl("_FL_", sampid, ignore.case = TRUE),
      sampid != "20171020_FL_50_UNK" # weird dup, don't need
    ) %>%
    tidyr::separate(
      sampid,
      into = c(
        "sample_date_coc",
        "site_part_1",
        "site_part_2",
        "replicate",
        "sample_type"
      ),
      sep = "\\_",
      remove = FALSE,
      extra = "merge",
      fill = "right"
    ) %>%
    mutate(
      sample_type = case_when(
        grepl("BLK", sampid) ~ "BLK",
        sampid %in%
          c(
            # 8/20/2018
            "20180820_FL_41_2_UKN",
            "20180820_FL_2_2_UKN",
            # 9/25/2018
            "20180925_FL_41_2_UKN",
            "20180925_FL_2_2_UKN",
            # 11/19/2018
            "20181119_FL_41_2_UKN",
            "20181119_FL_02_2_UKN"
          ) ~ "DUP",
        replicate %in% c("UNK", "UKN") ~ "UNK",
        sample_type == "UKN" ~ "UNK",
        TRUE ~ sample_type
      ),
      site_id = case_when(
        sampid == "20180404_FL_15-1_UKN" ~ 15,
        TRUE ~ as.numeric(site_part_2) # will give warning, OK
      ),
      tn = case_when(
        tn == 1e+16 ~ NA_real_, # place holder for not analyzed
        TRUE ~ tn
      ),
      toc_units = "mg/L",
      tn_units = "mgN/L"
    ) %>%
    select(
      sample_date,
      site_id,
      sample_type,
      tn,
      tn_units,
      toc = toc_comb,
      toc_units
    )

  # Check toc for unexpected duplicates
  toc %>%
    filter(sample_type == "UNK") %>%
    get_dupes(sample_date, site_id, sample_type) # none found

  # COMPARE TN----
  # We have TN from Lachat and Shimadzu TOC/TN analyzer. Compare the two.
  tn <- full_join(
    nutrients %>%
      filter(
        analyte == "TN",
        # restrict comparison to unknowns only
        sample_type == "UNK"
      ) %>%
      select(sample_date, site_id, sample_type, lachat_tn = final_conc) %>%
      mutate(lachat_tn = lachat_tn / 1000), # convert to ugN/L
    toc %>%
      filter(sample_type == "UNK") %>%
      select(sample_date, site_id, sample_type, shimadzu_tn = tn)
  ) 

  # not an awesome comparison
    ggplot(tn, aes(lachat_tn, shimadzu_tn)) +
    geom_point() 

# are we missing at lachat tn that we have at shimadzu?  
  # no. lets just use lachat throughout to be consistent
  tn %>%
    filter(is.na(lachat_tn) & !is.na(shimadzu_tn))
  
# MERGE NUTRIENTS AND TOC----
  chem <- bind_rows(
    nutrients,
    toc %>%
      select(-tn, -tn_units) %>%
      rename(final_conc = toc, unit = toc_units) %>%
      mutate(analyte = "toc")
  ) %>%
    mutate(
      analyte = tolower(analyte),
      analyte = dplyr::recode(analyte, `tno2-3` = "tno2_3")
    )
  
# REVIEW BLANKS----
  # Blanks, a few TN are pretty high, but low compared to unknowns
  ggplot(filter(chem, sample_type == "BLK"), aes(sample_date, final_conc)) +
    geom_point() +
    facet_wrap(~analyte, scales = "free")

# REVIEW DUPLICATES----
  rep_identifier <- filter(chem, sample_type == "DUP") %>% # PUll out unique ID
    select(-final_conc, -unit, -sample_type) # omit these field

  # Reduce to only dups and associated unknowns
  reps_all <- merge(
    rep_identifier, # Pull out unknown and dup value
    filter(chem, sample_type == "UNK" | sample_type == "DUP")
  )

  dim(filter(chem, sample_type == "UNK" | sample_type == "DUP")) #1585, 6
  dim(rep_identifier) # 282, 3
  dim(reps_all) # 705, 6

  # Dups agree very well.
  ggplot(filter(reps_all, analyte == "tn"), aes(sample_date, final_conc)) + # plot
    geom_point() +
    facet_wrap(~ site_id + analyte)

  ggplot(filter(reps_all, analyte == "tp"), aes(sample_date, final_conc)) + # plot
    geom_point() +
    facet_wrap(~ site_id + analyte)

    ggplot(filter(reps_all, analyte == "tnh4"), aes(sample_date, final_conc)) + # plot
    geom_point() +
    facet_wrap(~ site_id + analyte)

    ggplot(filter(reps_all, analyte == "trp"), aes(sample_date, final_conc)) + # plot
    geom_point() +
    facet_wrap(~ site_id + analyte)

    ggplot(filter(reps_all, analyte == "tno2"), aes(sample_date, final_conc)) + # plot
    geom_point() +
    facet_wrap(~ site_id + analyte)

    ggplot(filter(reps_all, analyte == "tno2_3"), aes(sample_date, final_conc)) + # plot
    geom_point() +
    facet_wrap(~ site_id + analyte)

  ggplot(filter(reps_all, analyte == "toc"), aes(sample_date, final_conc)) + # plot
    geom_point() +
    facet_wrap(~ site_id + analyte)
  
  # Aggregate across dups.
  chem_agg <- chem %>%
    filter(
      sample_type != "BLK" # remove blanks
    ) %>% 
    group_by(sample_date, site_id, analyte, unit) %>% # grouping variable
    summarize(final_conc = mean(final_conc)) %>% # calculate mean across groups
    ungroup() # remove grouping structure

  # Take a quick peak for obvious problems
  # looks pretty good.
  ggplot(chem_agg, aes(sample_date, final_conc)) +
    geom_point() +
    facet_wrap(~analyte, scales = "free_y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

  # PIVOT TO WIDE----
  # Create one value and one units column for each analyte.
  chem_data <- chem_agg %>%
    pivot_wider(
      id_cols = c(sample_date, site_id),
      names_from = analyte,
      values_from = c(final_conc, unit),
      names_glue = "{analyte}_{.value}"
    ) %>%
    rename_with(
      ~ sub("_final_conc$", "", .x),
      ends_with("_final_conc")
    ) %>%
    rename_with(
      ~ sub("_unit$", "_units", .x),
      ends_with("_unit")
    )
  
  return(chem_data)
}