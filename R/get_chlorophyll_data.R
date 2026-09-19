## SCRIPT FOR READING CHLOROPHYLL

get_chlorophyll_data <- function(epa_chlorophyll_data_file, bsa_chlorophyll_data_file) {
  

  # uncertain how to interpret the chl data in the spreadsheet.
  # emailed Nietch for assistance [9/18/26]
  # requested access to ShareDrive containing data and R script [9/18/26]
  # just read unique identifies for now.
  epa_chlorophyll_data <- readxl::read_xlsx(
    epa_chlorophyll_data_file,
    sheet = "sample_log") %>%
    clean_names() %>%
    mutate(sample_date = as.Date(collection_date, format = "%m/%d/%Y")) %>%
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
    select(-site_part_1, -site_part_2, -depth_part_1, -depth_part_2) %>%
    mutate(
      sample_type = case_when(
        replicate == "DI" ~ "BLK",
        replicate == "2" ~ "DUP",
        sample_id == "20171127.FL.1.FBLK" ~ "BLK",
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
    )
 
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
      sample_date,
      chla = corrected_chla_mg_m3
    )
    
# combine the two datasets
# Only BSA data ready now [9/18/26]
# Add EPA when processed
  chlorophyll_data <- bsa_chlorophyll_data
  
return(chlorophyll_data)

}