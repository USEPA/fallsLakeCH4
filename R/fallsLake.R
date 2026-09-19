# FALLs LAKE DESIGN
# 10 APRIL 2017, Sarah Waldo

# CURRENT DESIGN IS FOR STRATIFIED UNEQUAL-PROBABILITY
# Falls lake is similar to Cave Run: area of 48 km2 vs. 31 km2 at Cave Run,
# perimeter of 280 km at Falls Lake vs. 245 km at Cave Run.
# This first "Straw Design" is in prep for the 4/10/17 call with 
# John Walker's group. I'll use a stratified, unequal probability design,
# 28 sample sites broken into 12 tributary (12 km2, or 25% of surface area)
# and 16 open water (36.2 km2, or 75% of surface area). 


## WHEN MODIFIYING FOR ANOTHER LAKE MAKE THE FOLLOWING CHANGES: 
##  MODIFY THE GRTS DESIGN LIST FOR THE NUMBER OF MAIN AND OVERSAMPLE SITES WANTED 
##  FOR EACH STRATA AND EACH SECTION, FOR Falls Lake:
##    OPEN WATER MAINSITES = 16
##            SECTION A (NORTH_WEST, 13 km2) = 4 
##            SECTION B (MIDDLE, 10 km2) = 4
##            SECTION C (UPPER_DAM 6.2 km2) = 4
##            SECTION D (LOWER_DAM 6.9 km2) = 4
##    OPEN WATER OVER SAMPLE = 24

##    TRIBUTARY MAIN SITES = 14
##    TRIBUTARY OVER SAMPLE = 18
##  CHANGE THE ZOOM FACTOR ON LINE 179
##  FIND AND REPLACE ALL INSTANCES OF THE LAKE NAME


# LIBRARY----------
source("scripts/masterLibrary.R")

# CREATE OBJECT TO HOLD FIRST PORTION OF ONE-DRIVE WORKING DIRECTORY PATH-----
# TO BE USED FOR ALL SHAPEFILES
rootDir <- "C:/Users/JBEAULIE/OneDrive - Environmental Protection Agency (EPA)/ENSB Methane/"

# READ POLYGON SHAPEFILE-----
# ONLY USED TO CONFIRM THE POLYGON LOOKS RIGHT.  OBJECT NOT DIRECTLY USED IN GRTS CALL
fallsLakeEqArea <- readOGR(dsn = paste(rootDir, "grtsInputOutput", sep=""), # Could use read.shape from spsurvey package
                           layer = "fallsLakeEqArea")  # shapefile name
plot(fallsLakeEqArea) # visualize polygon

# EXTRACT ATTRIBUTE TABLE
attfallsLake <- read.dbf(filename = paste(rootDir, "grtsInputOutput/fallsLakeEqArea", sep=""))

# SET UP FOR GRTS FUNCTION---------
# Call the set.seed function so that the survey designs can be replicate
set.seed(4447864)
########
########
# 10/17/2019
# The code runs, but I can't reproduce the design Sarah provided to John Walker.
# We have the shapefiles Sarah produced, so this is OK, but not ideal.
# I will comment out code below to ensure the correct design isn't overwritten.
# See issues.R for full details
########
########

# 
# # Create the design list
# ### We decided to set the number of main sites in the tributary area to 5, since it is a relatively small area (0.4 sq km)
# ### the unequal probability splits the open water part of the lake into two sections of almost equal area
# fallsLakeDsgn <- list("open_water" = list(panel=c(mainSites=16),
#                                           seltype="Unequal",
#                                           caty.n=c("lower_dam" = 4,
#                                                    "upper_dam" = 4,
#                                                    "middle" = 4,
#                                                    "north_west" = 4),
#                                           over=24),
#                       "trib"=list(panel=c(mainSites=14), #updated to 14 on 5/4/17 so that the ratio of the sample density in the tributary 
#                                                          #and the open water matches that of Cave run. See excel spreadsheet "fallsLakeVsCaveRun"
#                                   seltype="Unequal",
#                                   caty.n=c("trib" = 14),
#                                   over=18))
# 
# fallsLakeSitesEqArea <- grts(design=fallsLakeDsgn,
#                              DesignID="SU", # SU for stratified, unequal
#                              type.frame="area",
#                              src.frame="shapefile",
#                              in.shape=paste(rootDir, "grtsInputOutput/fallsLakeEqArea", sep=""),
#                              att.frame=attfallsLake,
#                              stratum="strata",
#                              mdcaty="section",
#                              shapefile=TRUE,
#                              out.shape=paste(rootDir, "grtsInputOutput/fallsLakeSitesEqArea", sep=""),
#                              prjfilename=paste(rootDir, "grtsInputOutput/fallsLakeEqArea", sep=""))
# 
# # Review the survey design
# select(fallsLakeSitesEqArea@data, mdcaty, stratum, panel) %>%
#   arrange(stratum, mdcaty,panel)
# 
# # Print the survey design summary
# summary(fallsLakeSitesEqArea)
# 
# plot(fallsLakeEqArea)
# points(fallsLakeSitesEqArea$xcoord, fallsLakeSitesEqArea$ycoord)
# 
# # new feature added 14 June 2016
# # Set up for mutating the "lakeSitesEqArea" file to include all of the fill-able fields
# # necessary in order to keep the Equal Area projection
# fallsLakeSitesEqArea <- readOGR(dsn = paste(rootDir, "fallsLake", sep=""), # Could use read.shape from spsurvey package
#                                 layer = "fallsLakeSitesEqArea")  # shapefile name
# 
# # add fill-able fields, preparation for analyzing GRTS results
# fallsLakeSitesEqArea@data <- mutate(fallsLakeSitesEqArea@data, 
#                                     deplyDt = "",    # adding all of these colums to the 
#                                     deplyTm = "",    # shape file to be filled in the field
#                                     chmStTm = "",  # tried to enter them in the order they will be filled
#                                     chm_vol = "",
#                                     bbblngO = "",                                
#                                     wtrDpth = "", 
#                                     smDpthS = "",
#                                     Tmp_C_S = "",
#                                     DOPrc_S = "",
#                                     DO__L_S = "",
#                                     SpCn__S = "",
#                                     pH_S = "",
#                                     ORP_S = "",
#                                     TrNTU_S = "",
#                                     chla_S = "",
#                                     smDpthD = "",
#                                     Tmp_C_D = "",
#                                     DOPrc_D = "",
#                                     DO__L_D = "",
#                                     SpCn__D = "",
#                                     pH_D = "",
#                                     ORP_D = "",
#                                     TrNTU_D = "",
#                                     chla_D = "",
#                                     ArExtnrs = "",
#                                     DG_Extn = "",
#                                     H2O_vol = "",
#                                     HeVol = "",
#                                     BrPrssr = "",
#                                     RtrvDat = "",
#                                     RtrvTim = "",
#                                     TtTrpVl = "",
#                                     TrapExtn = "",
#                                     Notes = "",
#                                     LatSamp = "",
#                                     LongSmp = ""
# )
# 
# 
# 
# # re-write this mutated file, will keep the equal area projection
# writeOGR(obj = fallsLakeSitesEqArea,  # write projected shapefile to disk for use on field computer
#          dsn = paste(rootDir, "fallsLake", sep=""), 
#          layer = "fallsLakeSitesEqArea",
#          driver = "ESRI Shapefile",
#          overwrite_layer = TRUE)
# 
# 
# # MANIPULATE, PROJECT, SUBSET, AND WRITE SHAPEFILES FOR PLOTTING-------
# # Project spatial polygon into WGS84 for plotting in ggmap/ggplot 
# fallsLakeEqArea84 <- spTransform(x = fallsLakeEqArea, 
#                                  CRS("+proj=longlat +datum=WGS84")) # specifies projection
# writeOGR(obj = fallsLakeEqArea84,  # write projected shapefile to disk for use on field computer
#          dsn = paste(rootDir, "fallsLake", sep=""), 
#          layer = "fallsLakeEqArea84",
#          driver = "ESRI Shapefile",
#          overwrite_layer = TRUE)
# fallsLakeEqArea84@data$id = rownames(fallsLakeEqArea84@data)
# fallsLakeEqArea84.f <- fortify(fallsLakeEqArea84, region="id")  # fortify polygon for ggmap/ggplot
# fallsLakeEqArea84.f <- merge(fallsLakeEqArea84.f, fallsLakeEqArea84@data, 
#                              by="id")  # bring attributes back in 
# 
# # Read and project spatial points dataframe for plottin
# fallsLakeSitesEqAreaPlot <- readOGR(dsn = paste(rootDir, "fallsLake", sep=""), 
#                                     layer = "fallsLakeSitesEqArea")  # shapefile created with grts function
# fallsLakeSites84 <- spTransform(x = fallsLakeSitesEqAreaPlot, #reproject
#                                 CRS("+proj=longlat +datum=WGS84")) # projection needed for google maps
# fallsLakeSites84@data <- mutate(fallsLakeSites84@data, 
#                                 long=coordinates(fallsLakeSites84)[,1], # add long to @data slot
#                                 lat=coordinates(fallsLakeSites84)[,2]) # add lat to @data slot
# 
# 
# # write out table of overdraw sites for reference in field
# write.table(filter(fallsLakeSites84@data, panel == "OverSamp")   %>%
#               select(siteID, stratum, mdcaty, long, lat) %>%
#               arrange(stratum, mdcaty),
#             file = paste(rootDir, "fallsLake/fallsLakeOverSampList.txt", sep=""),
#             row.names = FALSE, sep="\t")
# 
# # write projected shapefile to disk for use on field computer.
# # iPad and geoplatform require separate shapefiles for main and oversamp sites.
# # arcPad requires one shapefile with all sites.
# # write one shapefile with all sites for use with arcPad
# writeOGR(obj = fallsLakeSites84, 
#          dsn = paste(rootDir, "fallsLake", sep=""), 
#          layer = "fallsLakeSites84",
#          driver = "ESRI Shapefile",
#          overwrite_layer = TRUE)
# 
# # separate shapefile for mainSites and OverSamp (overdraw sites)
# fallsLakeSites84ListByPanel <- split(fallsLakeSites84, # split preserves class, outputs a list
#                                      f= fallsLakeSites84@data$panel)
# 
# # to look at the list (optional):
# fallsLakeSites84ListByPanel[[1]]@data        #should be mainSites
# fallsLakeSites84ListByPanel[[2]]@data        #should be OverSamp sites
# 
# # Write 'mainSites' to disk
# writeOGR(obj = fallsLakeSites84ListByPanel[[1]], # pulls out 'mainSites' shapefile from list.  Should confirm.
#          dsn = paste(rootDir, "fallsLake", sep=""), 
#          layer = "fallsLakeSites84mainSites",
#          driver = "ESRI Shapefile",
#          overwrite_layer = TRUE)
# # Write 'OverSamp' to disk
# writeOGR(obj = fallsLakeSites84ListByPanel[[2]], # pulls out 'OverSamp' shapefile from list.  Should confirm.
#          dsn = paste(rootDir, "fallsLake", sep=""), 
#          layer = "fallsLakeSites84OverSamp",
#          driver = "ESRI Shapefile",
#          overwrite_layer = TRUE)
# 
# # VISUALIZE SURVEY DESIGN--------
# # Get ggmap
# bbox <- make_bbox(data=fallsLakeSites84@data, #defines map extent based on sample site lat/lon
#                   long, lat, f=0.3) # f is zoom.  Large #, less zoom. tweak for each lake.  
# fallsLakeSat <- get_map(location = bbox,
#                         color = "color",
#                         source = "google",
#                         maptype = "satellite")
# 
# # Plot ggmap with sites
# # First map contains only main sample sites
# ggmap(fallsLakeSat) +
#   ylab("Latitude") +
#   xlab ("Longitude") +
#   geom_polygon(data=fallsLakeEqArea84.f, aes(long, lat, group=group, fill=section)) +
#   #scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
#   geom_point(data=filter(fallsLakeSites84@data, panel == "mainSites"), #only main sites
#              aes(x=long, y=lat),
#              size = 2, color = "#FFFFFF") + # specify color to be consistent across maps
#   geom_text(data=filter(fallsLakeSites84@data, panel == "mainSites"), #only main sites
#             aes(label=siteID, x=long, y=lat),
#             hjust=1.2, vjust=0, size=2, color = "#FFFFFF") +     ### "#00BFC4" is blue
#   coord_equal() +                                                ### "#F8766D" is pink
#   ggtitle("Main Measurement locations for Falls Lake")
# 
# ggsave(filename=paste(rootDir, "fallsLake/fallsLakeMainSites.tiff", sep=""),
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# # Second map contains only oversample sites
# ggmap(fallsLakeSat) +
#   ylab("Latitude") +
#   xlab ("Longitude") +
#   geom_polygon(data=fallsLakeEqArea84.f, aes(long, lat, group=group, fill=section)) +
#   #scale_fill_manual(values = c("#000066", "#333399", "#006666")) + # colors from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
#   geom_point(data=filter(fallsLakeSites84@data, panel == "OverSamp"), #only main sites
#              aes(x=long, y=lat),
#              size = 2, color = "#00BFC4") + # specify color to be consistent across maps
#   geom_text(data=filter(fallsLakeSites84@data, panel == "OverSamp"), #only main sites
#             aes(label=siteID, x=long, y=lat),
#             hjust=1.2, vjust=0, size=2, color = "#00BFC4") +
#   coord_equal() +
#   ggtitle("Oversample locations for Falls Lake")
# 
# ggsave(filename=paste(rootDir, "fallsLake/fallsLakeOversampleSites.tiff", sep=""),
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# # Third map contains all sites
# ggmap(fallsLakeSat) +
#   ylab("Latitude") +
#   xlab ("Longitude") +
#   geom_polygon(data=fallsLakeEqArea84.f, aes(long, lat, group=group)) +
# geom_point(data=fallsLakeSites84@data, 
#              aes(x=long, y=lat, color = panel),
#              size = 2) +
#   geom_text(data=fallsLakeSites84@data,
#             aes(label=siteID, x=long, y=lat, color=panel),
#             hjust=1.2, vjust=0, # horizontal and vertical adjustment
#             size=2, show.legend = FALSE) + # text size and call to suppres legend
#   scale_color_discrete(name = "Sites",
#                        labels = c("Main", "Oversample")) +
#   coord_equal() +
#   ggtitle("All Measurement locations for Falls Lake")
# 
# ggsave(filename=paste(rootDir, "fallsLake/fallsLakeAllSites.tiff", sep=""),
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")