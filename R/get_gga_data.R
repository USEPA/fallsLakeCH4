get_gga_data <- function() {


# READ DATA -----------------
# List of .txt files containing data from Falls Lake monitoring
# "O:/Public/JTWalker/ENSB Methane/greenhouseGasAnalyzer/" old data source
txt_files <- list.files("data/greenhouse_gas_analyzer", 
                       pattern="*.txt$", recursive = TRUE) # $ matches end of string, excludes '...txt.zip' file

# Directories contain _s, _l, and _b files that don't contain data of interest.
# Strip these files out.
txt_files <- txt_files[!grepl(pattern = "_s|_l|_b|archive", x = txt_files)] # exclude auxiliary files and archive folder

gga_list <- list()  # Empty list to hold results

for (i in 1:length(txt_files)) {  # loop to read and format each file
    gga.i <- read.table(paste("data/greenhouse_gas_analyzer/", 
                              txt_files[i], sep=""),
                        sep=",",  # comma separate
                        skip=1,  # Skip first line of file.  Header info
                        colClasses = c("character", rep("numeric", 21), rep("NULL", 6)),
                        as.is=TRUE, # Prevent conversion to factor
                        header=TRUE, # Import column names
                        fill=TRUE) 
  # FORMAT DATA
# gga.i <- gga.i[1:(which(gga.i$Time == "-----BEGIN PGP MESSAGE-----") - 1), ]  # Remove PGP message
gga.i$Time <- gsub("^\\s+|\\s+$", "", gga.i$Time)  #  Strip white spaces
gga.i$date <- substr(gga.i$Time, start=1, stop=10)  # Extract date
gga.i$second <- round(  # extract second, round to integer
  as.numeric(
    substr(gga.i$Time, start=nchar(gga.i$Time) - 5, stop=nchar(gga.i$Time))
  ), 
  digits=0)
  gga.i$second <- ifelse(gga.i$second == 60, 59, gga.i$second)  # POSIXct can't handle 60 seconds
  gga.i$hms <- paste(substr(gga.i$Time, start=12, stop=17), gga.i$second, sep="")  # time vector
  gga.i$date_time <- as.POSIXct(paste(gga.i$date, gga.i$hms,sep=""),
  format="%m/%d/%Y%H:%M:%S",
  tz = "America/New_York")  # POSIXct; GGA clock time is Eastern
  gga.i$date <- as.Date(gga.i$date, format = "%m/%d/%Y")  # format as R Date oject
  names(gga.i)[grep("ppm", names(gga.i))] = gsub("^X.", "", names(gga.i)[grep("X", names(gga.i))]) # replace "X." with ""
  gga.i <- gga.i %>%
    # select columns of interest
    dplyr::select(
      date, date_time, CH4._ppm, CO2._ppm, GasT_C
    ) %>%  
  rename(
    ch4_gga = CH4._ppm,
    co2_gga = CO2._ppm,
    gga_temp = GasT_C
  ) %>%
    mutate(
      ch4_gga_units = "ppm",
      co2_gga_units = "ppm",
    )
  
  gga_list[[i]] <- gga.i  # dump in list
}  # End of loop, < 1 minute

# Merge files
gga_data <- do.call("rbind", gga_list) %>% 
  tibble::as_tibble() %>% # Coerces list into dataframe.
  arrange(date_time) %>%
  filter(!is.na(date_time))


# BASIC PLOTS-----------------
# ggplot(gga, aes(date_time, ch4_gga)) + geom_point() +
#   scale_x_datetime(labels = scales::date_format ("%m/%d %H:%M")) +
#   ylim(0,200)

# ggplot(gga, aes(date_time, co2_gga)) + geom_point() +
#   scale_x_datetime(labels=scales::date_format ("%m/%d %H:%M"))

return(gga_data)
  
}