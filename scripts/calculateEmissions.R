


# EMISSION RATE CALCULATIONS--------------------
# STEP 1:  CALCULATE EMISSION RATE VIA LINEAR AND NONLINEAR REGRESSION
#          FOR SITES WHERE PERIODS OF LINEAR ACCUMULATION ARE INDICATED 
# STEP 2: USE AIC TO DETERMINE WHETHER LINEAR OR NON-LINEAR FIT IS BEST.
#         CONFIRM CHOICE BY INSPECTING RAW DATA
# STEP 3: MERGE WITH eqAreaData



# STEP 1: LINEAR AND NONLINEAR REGRESSION
n <- length(unique(paste(gga$Lake_Name, gga$siteID)))
temp <- rep(NA, n)

# Dataframe to hold results
OUT <- data.frame(site = temp, Lake_Name = temp,
                  ch4.diff.max=temp, #Sarah added on 6/14/17; making histogram of max ch4 levels measured by LGR
                  ch4.lm.slope = temp, ch4.lm.drate.mg.h = temp, ch4.lm.aic = temp, ch4.lm.r2 = temp, ch4.lm.pval = temp,
                  ch4.ex.aic = temp, ch4.ex.r2 = temp, ch4.ex.slope = temp, ch4.ex.drate.mg.h = temp, ch4.ex.k=temp, 
                  co2.lm.slope = temp, co2.lm.drate.mg.h = temp, co2.lm.aic = temp, co2.lm.r2 = temp, co2.lm.pval = temp,
                  co2.ex.aic = temp, co2.ex.r2 = temp, co2.ex.slope = temp, co2.ex.k = temp, co2.ex.drate.mg.h = temp)


# Remove data not recorded during deployment
gga.model <- filter(gga, !is.na(Lake_Name))
pdf("output/figures/curveFits.pdf")
start.time <- Sys.time()
for (i in 1:length(unique(paste(gga.model$Lake_Name, gga.model$siteID)))) {  # For each unique site
  site.lake.i <- unique(paste(gga.model$siteID, gga.model$Lake_Name))[i]
  site.i <- gsub(" .*$", "", site.lake.i)  # extract site.  regex allows for siteIDs of different lengths (i.e. S-01, SU-01)
  lake.i <- substr(site.lake.i, start = nchar(site.i) + 2, stop = nchar(site.lake.i)) # extract lake name
  OUT[i,"site"] <- site.i
  OUT[i,"Lake_Name"] <- lake.i  
  # Need chamber volume from eqAreaData.
  chmVol.L.i <- filter(eqAreaData, siteID == site.i, Lake_Name == lake.i) %>% 
    select(chmVol.L)   
  


  data.i.ch4 <- filter(gga.model,  # extract data
                       RDateTime >= ch4DeplyDtTm, # based on diff start time
                       RDateTime <= ch4RetDtTm, # based on diff end time
                       siteID == site.i,  
                       Lake_Name == lake.i)  %>% 
    # Calculate elapsed time (seconds).  lm behaves strangely when used with POSIXct data.
    mutate(elapTime = RDateTime - RDateTime[1], # Calculate elapsed time (seconds). 
           chmVol.L = chmVol.L.i[1,1]) %>%  # subscripting needed to remove name
    select(Lake_Name, siteID, CH4._ppm, elapTime, GasT_C, chmVol.L)  # Pull out data of interest
  
  OUT[i, "ch4.diff.max"]<-max(data.i.ch4$CH4._ppm, na.rm=TRUE) #maximum CH4 mixing ratio measured during the chamber deployment time
  
  data.i.co2 <- filter(gga.model,  # extract data
                       RDateTime >= co2DeplyDtTm, # based on diff start time
                       RDateTime <= co2RetDtTm, # based on diff end time
                       siteID == site.i,  
                       Lake_Name == lake.i)  %>% 
    # Calculate elapsed time (seconds).  lm behaves strangely when used with POSIXct data.
    mutate(elapTime = RDateTime - RDateTime[1], # Calculate elapsed time (seconds). 
           chmVol.L = chmVol.L.i[1,1]) %>%  # subscripting needed to remove name
    select(Lake_Name, siteID, CO2._ppm, elapTime, GasT_C, chmVol.L)  # Pull out data of interest
  
  # Are there data available to run the model?
  co2.indicator <- length(data.i.co2$CO2._ppm) == 0
  ch4.indicator <- length(data.i.ch4$CH4._ppm) == 0
  
  # Data needed for emission rate calcs.  Same #'s for CO2 and CH4.  Arbitrarily pulled from CO2.  
  temp.i <- if (co2.indicator) mean(data.i.ch4$GasT_C, na.rm = TRUE) else (mean(data.i.co2$GasT_C, na.rm = TRUE))  # GGA measured temp
  volume.i <- if (co2.indicator) unique(data.i.ch4[!is.na(data.i.ch4$chmVol.L), "chmVol.L"]) else
    unique(data.i.co2[!is.na(data.i.co2$chmVol.L), "chmVol.L"])# Dome volume
  
  # lm
  lm.ch4.i <- try(lm(data.i.ch4$CH4._ppm ~ data.i.ch4$elapTime), silent = TRUE)  # suppress warning if fails 
  lm.co2.i <- try(lm(data.i.co2$CO2._ppm ~ data.i.co2$elapTime), silent = TRUE)  # linear regression
  
  # lm slopes
  slope.ch4.i <- if(ch4.indicator) NA else (as.numeric(coef(lm.ch4.i)[2]))  # lm slope: ppm s-1   
  slope.co2.i <- if(co2.indicator) NA else (as.numeric(coef(lm.co2.i)[2]))   # lm slope: ppm s-1
  OUT[i, c("ch4.lm.slope", "co2.lm.slope")] <- c(slope.ch4.i, slope.co2.i)
  
  # lm p-values
  fstat.ch4 <- if(ch4.indicator) rep(NA,3) else summary(lm.ch4.i)$fstatistic
  fstat.co2 <- if(co2.indicator) rep(NA,3) else summary(lm.co2.i)$fstatistic
  OUT[i, c("ch4.lm.pval")]  <- pf(fstat.ch4[1], fstat.ch4[2], fstat.ch4[3], lower.tail = FALSE)
  OUT[i, c("co2.lm.pval")]  <- pf(fstat.co2[1], fstat.co2[2], fstat.co2[3], lower.tail = FALSE)
  
  # lm r2 values
  OUT[i, c("ch4.lm.r2")]  <- if(ch4.indicator) NA else summary(lm.ch4.i)["r.squared"]
  OUT[i, c("co2.lm.r2")]  <- if(co2.indicator) NA else summary(lm.co2.i)["r.squared"]
  
  # lm AIC values
  OUT[i, c("ch4.lm.aic")] <- if(ch4.indicator) NA else AIC(lm.ch4.i)
  OUT[i, c("co2.lm.aic")] <- if(co2.indicator) NA else AIC(lm.co2.i)
  
  # Exponential Model
  cmax.ch4 <- data.i.ch4$CH4._ppm[max(which(!is.na(data.i.ch4$CH4._ppm)))]  # cmax = final CH4
  c.initial.ch4 <- data.i.ch4$CH4._ppm[min(which(!is.na(data.i.ch4$CH4._ppm)))]  # initial CH4 
  exp.ch4.i <-try(nlsLM(CH4._ppm~cmax-(cmax-b)*exp(-k*as.numeric(elapTime)),
                        data = data.i.ch4, 
                        start=list(cmax=cmax.ch4, b=cmax.ch4-c.initial.ch4, k=.03)),
                  silent = TRUE) 
  
  cmax.co2 <- data.i.co2$CO2._ppm[max(which(!is.na(data.i.co2$CO2._ppm)))]  # cmax = final CO2
  c.initial.co2 <- data.i.co2$CO2._ppm[min(which(!is.na(data.i.co2$CO2._ppm)))]  # initial CO2   
  exp.co2.i <-try(nlsLM(CO2._ppm~cmax-(cmax-b)*exp(-k*as.numeric(elapTime)),
                        data = data.i.co2, start=list(cmax=505, b= 400, k=0.004)),
                  silent=TRUE) 
  # Ex r2
  rss.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else sum(residuals(exp.ch4.i)^2)
  tss.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else 
    sum((data.i.ch4$CH4._ppm - mean(data.i.ch4$CH4._ppm, na.rm=TRUE))^2, na.rm=TRUE)
  OUT[i, "ch4.ex.r2"] = 1 - rss.ch4.i/tss.ch4.i
  
  rss.co2.i <- if(class(exp.co2.i) == "try-error") NA else sum(residuals(exp.co2.i)^2)
  tss.co2.i <- if(class(exp.co2.i) == "try-error") NA else 
    sum((data.i.co2$CO2._ppm - mean(data.i.co2$CO2._ppm, na.rm=TRUE))^2, na.rm=TRUE)
  OUT[i, "co2.ex.r2"] = 1 - rss.co2.i/tss.co2.i
  
  # Ex AIC
  OUT[i, "ch4.ex.aic"] = if(class(exp.ch4.i) == "try-error") NA else AIC(exp.ch4.i)
  OUT[i, "co2.ex.aic"] = if(class(exp.co2.i) == "try-error") NA else AIC(exp.co2.i)
  
  # Ex slope
  coef.exp.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else coef(exp.ch4.i)  
  OUT[i, "ch4.ex.slope"] = if(class(exp.ch4.i) == "try-error") NA else 
    coef.exp.ch4.i["k"]*(coef.exp.ch4.i["cmax"]-coef.exp.ch4.i["b"])  # ppm s-1
  
  coef.exp.co2.i <- if(class(exp.co2.i) == "try-error") NA else coef(exp.co2.i)  
  OUT[i, "co2.ex.slope"] = if(class(exp.co2.i) == "try-error") NA else 
    coef.exp.co2.i["k"]*(coef.exp.co2.i["cmax"]-coef.exp.co2.i["b"])  # ppm s-1
  
  #Ex k  
  OUT[i, "ch4.ex.k"] = if(class(exp.ch4.i) == "try-error") NA else 
    coef.exp.ch4.i["k"]
  OUT[i, "co2.ex.k"] = if(class(exp.co2.i) == "try-error") NA else 
    coef.exp.co2.i["k"]
  
  # Emission rate.  Assumes atmospheric pressure of 1 atm.
  # Converting from parts per million to umole cross out.  No conversion factor necessary. Dome area = 0.2 m2
  ch4.lm.drate.i.umol.s <- ((volume.i * 1 * slope.ch4.i) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CH4 s-1
  OUT[i, "ch4.lm.drate.mg.h"] = if (length(ch4.lm.drate.i.umol.s) == 0)  # throws error if no data
    NA else
      ch4.lm.drate.i.umol.s * (16/1000) * (60*60)  # mg CH4 m-2 h-1
  
  co2.lm.drate.i.umol.s <- ((volume.i * 1 * slope.co2.i) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CO2 s-1
  OUT[i, "co2.lm.drate.mg.h"] =  if (length(co2.lm.drate.i.umol.s) == 0) # throws error if no data
    NA else
      co2.lm.drate.i.umol.s * (44/1000) * (60*60)  #mg CO2 m-2 h-1
  
  ch4.ex.drate.i.umol.s <- ((volume.i * 1 * OUT[i, "ch4.ex.slope"]) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CH4 s-1
  OUT[i, "ch4.ex.drate.mg.h"] = if (length(ch4.lm.drate.i.umol.s) == 0) # throws error if no data
    NA else
      ch4.ex.drate.i.umol.s * (16/1000) * (60*60)  # mg CH4 m-2 h-1
  
  co2.ex.drate.i.umol.s <- ((volume.i * 1 * OUT[i, "co2.ex.slope"]) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CO2 s-1
  OUT[i, "co2.ex.drate.mg.h"] =  if (length(co2.lm.drate.i.umol.s) == 0) # throws error if no data
    NA else
      co2.ex.drate.i.umol.s * (44/1000) * (60*60)  #mg CO2 m-2 h-1
  
  # Plots
  # CH4 first
  ch4.ex.pred <- try(  
    data.frame(ch4.pred = predict(exp.ch4.i, newdata = data.i.ch4), # pred values from exponential model
               elapTime = data.i.ch4$elapTime),
    silent = TRUE)
  
  ch4.title <- paste(OUT[i, "site"], # plot title
                     OUT[i, "Lake_Name"],
                     "ex.r2=",
                     round(OUT[i, "ch4.ex.r2"], 2),
                     "ex.AIC=",
                     round(OUT[i, "ch4.ex.aic"],2),
                     "ex.rate=",
                     round(OUT[i, "ch4.ex.drate.mg.h"], 2),                    
                     "\n lm.r2=",
                     round(OUT[i, "ch4.lm.r2"],2),
                     "lm.AIC=",
                     round(OUT[i, "ch4.lm.aic"],2),
                     "lm.rate=",
                     round(OUT[i, "ch4.lm.drate.mg.h"], 2),
                     sep=" ")
  p.ch4 <- ggplot(data.i.ch4, aes(as.numeric(elapTime), CH4._ppm)) + 
    geom_point() +
    xlab("Seconds") +
    ggtitle(ch4.title) +
    stat_smooth(method = "lm", se=FALSE)
  if (class(exp.ch4.i) == "try-error") p.ch4 else  # if exp model worked, add exp line
    p.ch4 <- p.ch4 + geom_line(data=ch4.ex.pred, aes(as.numeric(elapTime), ch4.pred), color = "red")
  print(p.ch4)
  
  
  # CO2 models
  co2.ex.pred <- try(
    data.frame(co2.pred = predict(exp.co2.i, newdata = data.i.co2),  # pred data from exp model
               elapTime = data.i.co2$elapTime),
    silent=TRUE)
  
  co2.title <- paste(OUT[i, "site"], # plot title
                     OUT[i, "Lake_Name"],
                     "ex.r2=",
                     round(OUT[i, "co2.ex.r2"], 2),
                     "ex.AIC=",
                     round(OUT[i, "co2.ex.aic"],2),
                     "ex.rate=",
                     round(OUT[i, "co2.ex.drate.mg.h"], 2),                    
                     "\n lm.r2=",
                     round(OUT[i, "co2.lm.r2"],2),
                     "lm.AIC=",
                     round(OUT[i, "co2.lm.aic"],2),
                     "lm.rate=",
                     round(OUT[i, "co2.lm.drate.mg.h"], 2),
                     sep=" ")
  p.co2 <- ggplot(data.i.co2, aes(as.numeric(elapTime), CO2._ppm)) + 
    geom_point() +
    xlab("Seconds") +
    ggtitle(co2.title) +
    stat_smooth(method = "lm", se=FALSE)
  if (class(exp.co2.i) == "try-error") p.co2 else  # if exp model worked, add exp line
    p.co2 <- p.co2 + geom_line(data=co2.ex.pred, aes(as.numeric(elapTime), co2.pred), color = "red")
  print(p.co2)
}  
dev.off()
start.time;Sys.time() 


# STEP 2: USE AIC TO DETERMINE WHETHER LINEAR OR NON-LINEAR FIT IS BEST.
#         CONFIRM CHOICE BY INSPECTING RAW DATA
# Choose best rate.  Just use AIC
# Include is.na(ex.aic) to accomodate failed exp models.
OUT <- mutate(OUT, 
              co2.best.model = ifelse(co2.lm.aic < co2.ex.aic | is.na(co2.ex.aic), 
                                           "linear", "exponential"),
              co2.drate.mg.h.best = ifelse(co2.best.model == "linear",
                                           co2.lm.drate.mg.h, co2.ex.drate.mg.h),
              ch4.best.model = ifelse(ch4.lm.aic < ch4.ex.aic | is.na(ch4.ex.aic), 
                                      "linear", "exponential"),
              ch4.drate.mg.h.best = ifelse(ch4.best.model == "linear",
                                           ch4.lm.drate.mg.h, ch4.ex.drate.mg.h)) 
# Inspect r2.
plot(with(OUT,ifelse(co2.best.model == "linear", co2.lm.r2, co2.ex.r2)))  # CO2: some low ones to investigate
plot(with(OUT,ifelse(ch4.best.model == "linear", ch4.lm.r2, ch4.ex.r2)))  # CH4:  some low ones to investigate

# If r2 of best model < 0.9, then set to NA
OUT <- mutate(OUT, 
              co2.drate.mg.h.best = ifelse((co2.lm.aic < co2.ex.aic | is.na(co2.ex.aic)) & co2.lm.r2 < 0.9, # if ex is best, but r2<0.9
                                                NA, # then NA
                                           ifelse((co2.ex.aic < co2.lm.aic) & co2.ex.r2 < 0.9, # if lm is best, but r2<0.9
                                                  NA, # the NA
                                                  co2.drate.mg.h.best)), # otherwise assume value defined above
                                                  
              ch4.drate.mg.h.best = ifelse((ch4.lm.aic < ch4.ex.aic | is.na(ch4.ex.aic)) & ch4.lm.r2 < 0.9, # if ex is best, but r2<0.9
                                           NA, # then NA
                                           ifelse((ch4.ex.aic < ch4.lm.aic) & ch4.ex.r2 < 0.9, # if lm is best, but r2<0.9
                                                  NA, # the NA
                                                  ch4.drate.mg.h.best))) # otherwise assume value defined above

# Inspect r2 after scrubbing r2<0.9
plot(with(OUT[!is.na(OUT$co2.drate.mg.h.best),], 
          ifelse(co2.best.model == "linear", co2.lm.r2, co2.ex.r2)))  # CO2: all > 0.9

plot(with(OUT[!is.na(OUT$ch4.drate.mg.h.best),], 
          ifelse(ch4.best.model == "linear", ch4.lm.r2, ch4.ex.r2)))  # CH4: all > 0.9

# STEP 3: MERGE DIFFUSION RATES WITH eqAreaData
# First, strip NA from OUT
OUT <- filter(OUT, !is.na(Lake_Name)) # Just one NA slipped in??
eqAreaData <- merge(eqAreaData, OUT, by.x = c("Lake_Name", "siteID"), 
      by.y = c("Lake_Name", "site"), all=TRUE)

str(eqAreaData) # 1531 observations

# Any sites not have a diffusive rate?
filter(eqAreaData, EvalStatus == "sampled", is.na(ch4.drate.mg.h.best)) %>%
  select(Lake_Name, siteID, 
         ch4.lm.drate.mg.h, ch4.ex.drate.mg.h, ch4.drate.mg.h.best,
         co2.lm.drate.mg.h, co2.ex.drate.mg.h, co2.drate.mg.h.best)



# CALCULATE EBULLITION RATE------------------

# First calculate volumetric ebullion rate.  Straightforward operation
# that can be vectorized across the entire df.
eqAreaData <- mutate(eqAreaData, 
                     ebMlHrM2 = TtTrpVl / 
                       (as.numeric(trapRtrvDtTm - trapDeplyDtTm) * 
                          ((3.14*.28^2)))) # diameter = 22.25in=0.56m, r=.28m))

# Mass flux rate must be calculated by Lake.  Tried to apply by group using
# by_group, ddply, and lapply.  I couldn't figure it out, resorted to for loop

myEbList <- list()
for (i in 1:length(unique(eqAreaData$Lake_Name))) {
  lake.i <- unique(eqAreaData$Lake_Name)[i]
  data.i <- filter(eqAreaData, Lake_Name == lake.i )
  # mass.rate function will throw warning if all three gases (co2, ch4, n2o)
  # are not supplied.  This can be ignored.
  out.ch4 <- mass.rate(data.i, choice1 = "ch4") 

  myEbList[[i]] <- data.frame(ebCh4mgM2h = out.ch4,
                              Lake_Name = data.i$Lake_Name,
                              siteID = data.i$siteID)
}

ebResults <- do.call("rbind", myEbList) %>%  # This coerces the list into a dataframe. Cool.
  rename(ch4.erate.mg.h = ebCh4mgM2h)


str(eqAreaData) # 72 observations
str(ebResults)  # 72 observations
eqAreaData <- merge(eqAreaData,ebResults, all = TRUE) 
str(eqAreaData) # 70 observations


# CALCULATE TOTAL EMISSION RATES------------------
# If CH4 ebul is measured, but CH4 diff couldn't be calculated,
# CH4 tot = CH4 ebb.  In all other cases TOT = diff + ebul,
# resulting tot = NA if is.na(ebul) or is.na(diff).
eqAreaData <- mutate(eqAreaData,
                     ch4.trate.mg.h = ifelse(is.na(ch4.drate.mg.h.best) &
                                               !is.na(ch4.erate.mg.h),
                                             ch4.erate.mg.h,
                                             ch4.drate.mg.h.best +ch4.erate.mg.h))



