# Load libraries and functions
source("scripts/masterLibrary.R")

# Read data
source("scripts/readSitesEqAreaData.R")
source("scripts/readGc.R")
source("scripts/readLgr.R")  # Reads in raw LGR data
source("scripts/readChem.R") # Merges with eqAreaData

# Calculate derived quantities
source("scripts/plotCleanLgr.R") # Merges chamber time with eqAreaData, 10min
source("scripts/calculateEmissions.R") # Merges with eqAreaData, 3min

# grts calculations
source("scripts/grtsWgtAdj.R") # Merges with eqAreaData, 2s
source("scripts/grtsMeanVariance.R") # 20s