# Load libraries and functions
source("scripts/masterLibrary.R")

# Read data
source("scripts/readSitesEqAreaData.R")
source("scripts/readGc.R")
source("scripts/readLgr.R")  # Reads in raw LGR data

# Calculate derived quantities
source("scripts/plotCleanLgr.R") # Merges chamber time with eqAreaData, 10min
source("scripts/calculateEmissions.R") # Merges with eqAreaData, 3min
