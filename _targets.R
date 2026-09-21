# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Enforce package preferences
conflicts_prefer(
  dplyr::filter(), 
  dplyr::select(),
  readxl::read_xlsx(),
  flextable::align(),
  flextable::compose()
)

# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "janitor",
    "readxl",
    "fs",
    "minpack.lm" # for non linear diffusion model
  )
)

# Run the R scripts in the R/ folder with your custom functions:
source("R/get_chlorophyll_data.R")
source("R/get_chem_data.R")
source("R/get_field_sheets.R")
source("R/get_visit_date_map.R")
source("R/get_gc_data.R")


# TARGETS PIPELINE----
list(
  # Get data----
  ## hardcoded visit date map----
  tar_target(
    name = visit_date_map,
    command = get_visit_date_map()
  ),

  ## chlorophyll----
  tarchetypes::tar_file(
    name = epa_chlorophyll_data_file,
    command = "data/chlorophyll/epa/chlData_20180507.xlsx"
  ),
  tarchetypes::tar_file(
    name = bsa_chlorophyll_data_file,
    command = "data/chlorophyll/bsa/20201109 USEPA Chlorophyll Report 779.xlsx"
  ),
tar_target(
  name = chlorophyll_data,
  command = get_chlorophyll_data(
    epa_chlorophyll_data_file = epa_chlorophyll_data_file,
    bsa_chlorophyll_data_file = bsa_chlorophyll_data_file
  )
  ),

  ## chemistry----
  tarchetypes::tar_file(
    name = chem_data_2017_file,
    command = "data/water_chemistry/2017_ESF-EFWS_NutrientData_Updated02112019_SS_CTNWorked03192019.xlsx"
  ),
  tarchetypes::tar_file(
    name = chem_data_2018_file,
    command = "data/water_chemistry/2018_ESF-EFWS_NutrientData_Updated03012019_SS_CTNUpdate04012019.xlsx"
  ),
  tar_file(
    name = toc_data_file,
    command = "data/water_chemistry/Beaulieu.dbf"
  ),
  tar_target(
    name = chem_data,
    command = get_chem_data(
      chem_data_2017_file = chem_data_2017_file,
      chem_data_2018_file = chem_data_2018_file,
      toc_data_file = toc_data_file
    )
  ),

  ## gc data----
  tar_file(
    name = rtp_gc_data_file,
    command = "data/gc_data/RTP_GC Data summary_191213.xlsx"
  ),
  tar_file(
    name = epa_gc_data_file_1,
    command = "data/gc_data/gcMasterFile2017updated2019-08-28.txt"
  ),
  tar_file(
    name = epa_gc_data_file_2,
    command = "data/gc_data/gcMasterFile2018updated2019-04-09.txt"
  ),
  # tar_target(
  #   name = gc_data,
  #   command = get_gc_data(
  #     visit_date_map = visit_date_map,
  #     rtp_gc_data_file = rtp_gc_data_file,
  #     epa_gc_data_file_1 = epa_gc_data_file_1,
  #     epa_gc_data_file_2 = epa_gc_data_file_2
  #   )
  # ),





  # a list containing "fld_sheet" and "dg_sheet"
  tar_target(
    name = field_sheets,
    command = get_field_sheets() # no argument needed since the function reads in the files from the data folder
)
)
