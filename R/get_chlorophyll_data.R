## SCRIPT FOR READING CHLOROPHYLL

get_chlorophyll_data <- function(
  epa_chlorophyll_data_file,
  bsa_chlorophyll_data_file,
  visit_date_map
) {
  # Per Chris Nietch, column Chl_a_JH is the corrected chlorophyll-a value (in ug/L) in the raw water sample.
  # see email in data/chlorophyll/epa
  # missing data from May, June and July 2018. Emailing with Nietch to see if I can find the data in Joel's
  # L drive folders that were transferred to him.
  epa_chlorophyll_data <- readxl::read_xlsx(
    epa_chlorophyll_data_file,
    sheet = "sample_log"
  ) %>%
    clean_names() %>%
    mutate(
      # COC confirmed site 40 was written in error; the true site_id is 44.
      sample_id = replace(
        sample_id,
        sample_id == "20171020.FL.40.0.1.1.SW.UKN",
        "20171020.FL.44.0.1.1.SW.UKN"
      ),
      sample_date = as.Date(collection_date, format = "%m/%d/%Y"),
      chla = chl_a_jh,
      chla_unit = "ug/L"
    ) %>%
    # pull out Falls Lake samples (sample_id contains ".fl.")
    filter(grepl("\\.fl\\.", sample_id, ignore.case = TRUE)) %>%
    # split sample_id into its components (date, site, depth, replicate, matrix, sample type)
    tidyr::separate(
      sample_id,
      into = c(
        "sample_date_coc",
        "site_part_1",
        "site_part_2",
        "depth_part_1",
        "depth_part_2",
        "replicate",
        "matrix",
        "sample_type"
      ),
      sep = "\\.",
      remove = FALSE,
      extra = "merge",
      fill = "right"
    ) %>%
    mutate(
      sample_date_coc = as.Date(sample_date_coc, format = "%Y%m%d"),
      site_id = as.numeric(site_part_2), # will give warning, OK
      sample_depth = as.numeric(paste(depth_part_1, depth_part_2, sep = "."))
    ) %>%
    select(
      sample_id,
      sample_date,
      sample_date_coc,
      site_id,
      sample_depth,
      sample_type,
      replicate,
      chla,
      chla_unit
    ) %>%
    mutate(
      sample_type = case_when(
        replicate == "DI" ~ "BLK",
        replicate == "2" ~ "DUP",
        sample_id == "20171127.FL.1.FBLK" ~ "BLK",
        sample_type == "UKN" ~ "UNK",
        TRUE ~ sample_type
      ),
      replicate = case_when(
        replicate == "DI" ~ 1,
        TRUE ~ as.integer(replicate)
      ),
      site_id = case_when(
        sample_id == "20171127.FL.1.FBLK" ~ NA_integer_,
        TRUE ~ site_id
      )
    ) %>%
    select(-sample_id)

  # if sample_date and sample_date_coc are equal, then print a message and drop sample_date_coc column
  if (
    all(
      epa_chlorophyll_data$sample_date == epa_chlorophyll_data$sample_date_coc,
      na.rm = TRUE
    )
  ) {
    message(
      "sample_date and sample_date_coc are equal, dropping sample_date_coc column"
    )
    epa_chlorophyll_data <- epa_chlorophyll_data %>%
      select(-sample_date_coc)
  }

  bsa_chlorophyll_data <- readxl::read_xlsx(
    bsa_chlorophyll_data_file,
    sheet = "OUTPUT - VOLUMETRIC"
  ) %>%
    clean_names() %>%
    mutate(sample_date = as.Date(sample_date, format = "%m/%d/%Y")) %>%
    # pull out Falls Lake samples (location contains ".fl.")
    filter(grepl("\\.fl\\.", location, ignore.case = TRUE)) %>%
    # split sample_id into its components (date, site, depth, replicate, matrix, sample type)
    tidyr::separate(
      location,
      into = c(
        "sample_date_coc",
        "site_part_1",
        "site_part_2",
        "depth_part_1",
        "depth_part_2",
        "replicate",
        "matrix",
        "sample_type"
      ),
      sep = "\\.",
      remove = FALSE,
      extra = "merge",
      fill = "right"
    ) %>%
    mutate(
      # used this to check against sample_date, all good
      sample_date_coc = as.Date(sample_date_coc, format = "%Y%m%d"),
      site_id = suppressWarnings(as.numeric(site_part_2)), # will give warning, OK
      sample_depth = suppressWarnings(as.numeric(paste(
        depth_part_1,
        depth_part_2,
        sep = "."
      ))) # will give warning, OK
    ) %>%
    select(-site_part_1, -site_part_2, -depth_part_1, -depth_part_2) %>%
    mutate(
      sample_type = case_when(
        replicate == "DI" ~ "BLK",
        replicate == "2" ~ "DUP",
        sample_type == "UKN" ~ "UNK",
        TRUE ~ sample_type
      ),
      replicate = suppressWarnings(case_when(
        replicate == "DI" ~ 1,
        TRUE ~ as.integer(replicate)
      )),
      chla_unit = "ug/l"
    ) %>%
    select(
      site_id,
      sample_depth,
      sample_type,
      replicate,
      sample_date,
      sample_date_coc,
      chla = corrected_chla_mg_m3,
      chla_unit
    )

  # if sample_date and sample_date_coc are equal, then print a message and drop sample_date_coc column
  if (
    all(
      bsa_chlorophyll_data$sample_date == bsa_chlorophyll_data$sample_date_coc,
      na.rm = TRUE
    )
  ) {
    message(
      "sample_date and sample_date_coc are equal, dropping sample_date_coc column"
    )
    bsa_chlorophyll_data <- bsa_chlorophyll_data %>%
      select(-sample_date_coc)
  }

  # combine the two datasets-----
  chlorophyll_data <- bind_rows(
    bsa_chlorophyll_data,
    epa_chlorophyll_data
  ) %>%
    mutate(
      # Assign each sample to its visit by matching sample_date to the visit-date map.
      visit = visit_date_map$visit[
        match(sample_date, visit_date_map$sample_date)
      ]
    ) 

  # inspect blanks are replicates------
  # blank values are super tiny all good. Filter out.
  chlorophyll_data %>%
    group_by(sample_type) %>%
    summarize(
      mean = mean(chla, na.rm = TRUE),
    )

  # now inspect replicates
  # yup, looks good. Aggregate
  chlorophyll_data %>%
    filter(sample_type != "BLK") %>%
    ggplot(aes(
      x = as.factor(visit),
      y = chla,
      color = as.factor(sample_type)
    )) +
    geom_point() +
    facet_wrap(~site_id, scales = "free")

  # remove blanks and agrregate replicates
  chlorophyll_data <- chlorophyll_data %>%
    filter(sample_type != "BLK") %>%
    group_by(
      site_id,
      visit
    ) %>%
    summarize(
      chla = mean(chla, na.rm = TRUE),
      chla_unit = first(chla_unit),
      sample_date = first(sample_date)
    ) %>%
    ungroup() %>%
    # drop sample date because we have visit number
    select(-sample_date)

  chlorophyll_data %>% janitor::get_dupes(site_id, visit) # 0, good


  return(chlorophyll_data)
}