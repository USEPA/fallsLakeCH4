calculate_ebullition <- function(dat_raw) {

# First calculate volumetric ebullition rate (mL/hr/m2)
  dat_ebullition <- dat_raw %>%
    select(site_id, visit, trap_volume, trap_rtrvl_date_time, trap_deply_date_time, 
      trap_ch4_gc, trap_co2_gc, trap_n2o_gc, 
      atm_pressure, 
      temp_s) %>% # assume trap gas temperature equals shallow water temperature
    mutate(
      volumetric_ebullition = trap_volume /
        (as.numeric(
          trap_rtrvl_date_time - trap_deply_date_time,
          units = "hours"
        ) * # time difference in hours
          ((3.14 * .28^2))), # diameter = 22.25in=0.56m, r=.28m))
      volumetric_ebullition_units = "ml_m2_hr"
    )
      
  


# Trap data were inspected for outliers and aggregated across replicated
# in get_gc_data.R. No need for further cleaning here.
# CO2 and N2O were only measured in visits 11-15 at CIN.
  
# Function for calculating mass flux rate--                  
mass_rate <- function(X1, choice1){
  # trap gas data to use if measured values aren't available
  trap_ch4_gc <- X1 %>%
    mutate(trap_ch4_gc = case_when(is.na(trap_ch4_gc) & any(!is.na(trap_ch4_gc)) ~ mean(trap_ch4_gc, na.rm=TRUE), # if trap_ch4_gc is missing for the site, but any others sites in lake have ch4, use lake mean
                               #is.na(ch4_ppm) & all(is.na(ch4_ppm)) ~ mean(gc_lakeid %>% filter(type == "trap") %>% pull(ch4_ppm), na.rm = TRUE), # if no trap samples collected from the lake, use project mean
                               TRUE ~ trap_ch4_gc)) %>% # else use measured value
    pull(trap_ch4_gc)
  
  trap_co2_gc <- X1 %>%
    mutate(trap_co2_gc = case_when(is.na(trap_co2_gc) & any(!is.na(trap_co2_gc)) ~ mean(trap_co2_gc, na.rm=TRUE), # if trap_co2_gc is missing for the site, but any others sites in lake have ch4, use lake mean
                               #is.na(co2_ppm) & all(is.na(co2_ppm)) ~ mean(gc_lakeid %>% filter(type == "trap") %>% pull(co2_ppm), na.rm = TRUE), # if no trap samples collected from the lake, use project mean
                               TRUE ~ trap_co2_gc)) %>% # else use measured value
    pull(trap_co2_gc)
  
  trap_n2o_gc <- X1 %>%
    mutate(trap_n2o_gc = case_when(is.na(trap_n2o_gc) & any(!is.na(trap_n2o_gc)) ~ mean(trap_n2o_gc, na.rm=TRUE), # if trap_n2o_gc is missing for the site, but any others sites in lake have ch4, use lake mean
                               #is.na(trap_n2o_gc) & all(is.na(trap_n2o_gc)) ~ mean(gc_lakeid %>% filter(type == "trap") %>% pull(trap_n2o_gc), na.rm = TRUE), # if no trap samples collected from the lake, use project mean
                               TRUE ~ trap_n2o_gc)) %>% # else use measured value
    pull(trap_n2o_gc)
  
  # barometric pressure needed: n=PV/RT
  # see get_bp_data.R for how missing barometric pressure was imputed
  bp <- X1$atm_pressure #mmHg
  
  # temperature needed
  air_temperature <- X1$temp_s # assume trap gas temperature equals shallow water temperature

  # convert 1mL to moles
  mL_to_mmoles <- ((bp*0.001)/(0.082058 * air_temperature)) * 1000      #1mL = 0.001L; *1000 to convt to mmol
  
  # convert mmoles to mg
  if(choice1 == "ch4") {mg_gas <- mL_to_mmoles * 16 * (trap_ch4_gc/1000000)}  #16mg/mmole
  if(choice1 == "co2") {mg_gas <- mL_to_mmoles * 44 * (trap_co2_gc/1000000)}  #44mg/mmole
  if(choice1 == "n2o") {mg_gas <- mL_to_mmoles * 44 * (trap_n2o_gc/1000000)}  #44mg/mmole
  
  # calculate rate
  mass_flux_rate <- mg_gas * X1$volumetric_ebullition #bubble rate in mg ch4-co2-n2o /hour/m2
  
  # return mass flux rate in mg ch4-co2-n2o /hour/m2
  mass_flux_rate
}

  # calculate mass ebullition rate for each gas
  dat_ebullition <- dat_ebullition %>%
    mutate(
      ch4_ebullition = mass_rate(., "ch4"),
      ch4_ebullition_units = "mg_m2_hr",
      co2_ebullition = mass_rate(., "co2"),
      co2_ebullition_units = "mg_m2_hr",
      n2o_ebullition = mass_rate(., "n2o"),
      n2o_ebullition_units = "mg_m2_hr"
    )

  return(dat_ebullition)

}