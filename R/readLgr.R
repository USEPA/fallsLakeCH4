# SCRIPT TO READ, FORMAT, AND INSPECT LGR GHG DATA 

# LIBRARIES---------------
# library(ggplot2) # load from masterLibrary
# library(scales)  # load from masterLibrary
# source("ohio2016/scriptsAndRmd/masterLibrary.R")


# READ DATA -----------------
# List of .txt files containing data from Falls Lake monitoring
txtFiles <- list.files("O:/Public/JTWalker/ENSB Methane/greenhouseGasAnalyzer/", 
                       pattern="*.txt$", recursive = TRUE) # $ matches end of string, excludes '...txt.zip' file

# Directories contain _s, _l, and _b files that don't contain data of interest.
# Strip these files out.
txtFiles <- txtFiles[!grepl(pattern = "_s|_l|_b", x = txtFiles)] # exclude files with _l or _s or _b

ggaList <- list()  # Empty list to hold results

for (i in 1:length(txtFiles)) {  # loop to read and format each file
    gga.i <- read.table(paste("O:/Public/JTWalker/ENSB Methane/greenhouseGasAnalyzer/", 
                              txtFiles[i], sep=""),
                        sep=",",  # comma separate
                        skip=1,  # Skip first line of file.  Header info
                        colClasses = c("character", rep("numeric", 21), rep("NULL", 6)),
                        as.is=TRUE, # Prevent conversion to factor
                        header=TRUE, # Import column names
                        fill=TRUE) 
  # FORMAT DATA
# gga.i <- gga.i[1:(which(gga.i$Time == "-----BEGIN PGP MESSAGE-----") - 1), ]  # Remove PGP message
  gga.i$Time <- gsub("^\\s+|\\s+$", "", gga.i$Time)  #  Strip white spaces
  gga.i$Date <- substr(gga.i$Time, start=1, stop=10)  # Extract date
  gga.i$Second <- round(  # extract second, round to integer
    as.numeric(
      substr(gga.i$Time, start=nchar(gga.i$Time) - 5, stop=nchar(gga.i$Time))
    ), 
    digits=0)
  gga.i$Second <- ifelse(gga.i$Second == 60, 59, gga.i$Second)  # POSIXct can't handle 60 seconds
  gga.i$hms <- paste(substr(gga.i$Time, start=12, stop=17), gga.i$Second, sep="")  # time vector
  gga.i$RDateTime <- as.POSIXct(paste(gga.i$Date, gga.i$hms,sep=""),
                                format="%m/%d/%Y%H:%M:%S",
                                tz = "UTC")  # POSIXct
  gga.i$RDate <- as.Date(gga.i$Date, format = "%m/%d/%Y")  # format as R Date oject
  names(gga.i)[grep("ppm", names(gga.i))] = gsub("^X.", "", names(gga.i)[grep("X", names(gga.i))]) # replace "X." with ""
  gga.i <- select(gga.i, RDate, RDateTime, CH4._ppm, CO2._ppm, GasT_C)  # select columns of interest
  
  ggaList[[i]] <- gga.i  # dump in list
}  # End of loop, < 1 minute

# Merge files
gga <- do.call("rbind", ggaList)  # Coerces list into dataframe.


# BASIC PLOTS-----------------
# ggplot(gga, aes(RDateTime, CH4._ppm)) + geom_point() +
#   scale_x_datetime(labels=date_format ("%m/%d %H:%M")) +
#   ylim(0,200)
# 
# ggplot(gga, aes(RDateTime, CO2._ppm)) + geom_point() +
#   scale_x_datetime(labels=date_format ("%m/%d %H:%M"))


