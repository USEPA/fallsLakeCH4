

impute_data <- function(dat_raw, nasa_pressure_file){

# Impute atmospheric pressure in two stages:
# 1. Use the mean measured pressure within each visit.
# 2. For visits with no measured pressure, use cached NASA POWER values.
# The cached data use the daily surface-pressure (PS) product, converted from
# kPa to mm Hg. The normal workflow does not require an internet connection.

dat_impute <- dat_raw %>%
  # First standardize visits that contain at least one measured pressure.
  group_by(visit) %>%
  mutate(
    atm_pressure = if_else(
      any(!is.na(atm_pressure)),
      mean(atm_pressure, na.rm = TRUE),
      NA_real_
    )
  ) %>%
  ungroup()

missing_pressure_visits <- dat_impute %>%
  # Identify visits that still have no measured pressure after stage 1.
  group_by(visit) %>%
  summarise(
    no_measured_pressure = all(is.na(atm_pressure)),
    .groups = "drop"
  ) %>%
  filter(no_measured_pressure) %>%
  pull(visit)

if (length(missing_pressure_visits) > 0) {
  # NASA POWER query used to retrieve the pressure data saved below. Retained
  # for documentation; do not run during the normal pipeline.
  # nasa_requests <- dat_impute %>%
  #   filter(visit %in% missing_pressure_visits, !is.na(trap_deply_date)) %>%
  #   distinct(lat, long, trap_deply_date) %>%
  #   mutate(
  #     start = format(trap_deply_date, "%Y%m%d"),
  #     end = start
  #   )
  #
  # nasa_pressure <- purrr::map_dfr(
  #   seq_len(nrow(nasa_requests)),
  #   function(i) {
  #     request <- nasa_requests[i, ]
  #     url <- paste0(
  #       "https://power.larc.nasa.gov/api/temporal/daily/point?",
  #       "parameters=PS&community=AG&longitude=", request$long,
  #       "&latitude=", request$lat,
  #       "&start=", request$start,
  #       "&end=", request$end,
  #       "&format=CSV"
  #     )
  #
  #     response <- tryCatch(
  #       readLines(url, warn = FALSE),
  #       error = function(e) character()
  #     )
  #
  #     if (length(response) == 0) {
  #       return(tibble::tibble(
  #         lat = request$lat,
  #         long = request$long,
  #         trap_deply_date = request$trap_deply_date,
  #         nasa_atm_pressure = NA_real_
  #       ))
  #     }
  #
  #     header <- grep("^YEAR,", response)[1]
  #
  #     if (is.na(header)) {
  #       return(tibble::tibble(
  #         lat = request$lat,
  #         long = request$long,
  #         trap_deply_date = request$trap_deply_date,
  #         nasa_atm_pressure = NA_real_
  #       ))
  #     }
  #
  #     read.csv(
  #       textConnection(paste(response[header:length(response)], collapse = "\n")),
  #       stringsAsFactors = FALSE
  #     ) %>%
  #       transmute(
  #         lat = request$lat,
  #         long = request$long,
  #         trap_deply_date = request$trap_deply_date,
  #         nasa_atm_pressure = PS * 7.50062
  #       )
  #   }
  # )

  nasa_pressure <- readr::read_csv(
    nasa_pressure_file,
    show_col_types = FALSE
  )

  # Match cached pressure values to observations by coordinates and deployment date.
  # Then average the matched values within each visit.
  visit_pressure <- dat_impute %>%
    filter(visit %in% missing_pressure_visits) %>%
    select(visit, lat, long, trap_deply_date) %>%
    left_join(
      nasa_pressure,
      by = c("lat", "long", "trap_deply_date")
    ) %>%
    group_by(visit) %>%
    summarise(
      nasa_atm_pressure = mean(nasa_atm_pressure, na.rm = TRUE),
      .groups = "drop"
    )

  dat_impute <- dat_impute %>%
    # Fill only remaining missing values; measured values are preserved.
    left_join(visit_pressure, by = "visit") %>%
    mutate(
      atm_pressure_units = if_else(
        is.na(atm_pressure) & !is.na(nasa_atm_pressure),
        "mmHg",
        atm_pressure_units
      ),
      atm_pressure = coalesce(atm_pressure, nasa_atm_pressure)
    ) %>%
    select(-nasa_atm_pressure)
}





return(dat_impute)

}