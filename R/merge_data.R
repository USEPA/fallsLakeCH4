merge_data <- function(field_sheets, gc_data, chlorophyll_data, chem_data) {
  # inspect data----
  # are all site_id x visit combinations in gc_data present in field_sheets?
  # yes
   gc_data |>
    dplyr::distinct(site_id, visit) |>
    dplyr::anti_join(
      field_sheets |>
        dplyr::distinct(site_id, visit),
      by = c("site_id", "visit")
    )

  # are all site_id x visit combinations in chlorophyll_data present in field_sheets.
  # yes
  chlorophyll_data |>
    dplyr::distinct(site_id, visit) |>
    dplyr::anti_join(
      field_sheets |>
        dplyr::distinct(site_id, visit),
      by = c("site_id", "visit")
    )
  
  # are all site_id x visit combinations in chem_data present in field_sheets.
  # yes
  chem_data |>
    dplyr::distinct(site_id, visit) |>
    dplyr::anti_join(
      field_sheets |>
        dplyr::distinct(site_id, visit),
      by = c("site_id", "visit")
    )
  
  # merge data----
  # 1. field sheets and gc_data
  dat_raw <- field_sheets |>
    dplyr::full_join(
      gc_data,
      by = c("site_id", "visit"),
      suffix = c("_field", "_gc")
    )
  dim(gc_data) # 247
  dim(field_sheets) # 449
  dim(dat_raw) # 449

  # 2. add chlorophyll data
  dat_raw <- dat_raw |>
    dplyr::full_join(
      chlorophyll_data,
      by = c("site_id", "visit"),
      suffix = c("", "_chlorophyll")
    )
    dim(chlorophyll_data) # 144
    dim(dat_raw) # 449
  
  # 3. add chemistry data
  dat_raw <- dat_raw |>
    dplyr::full_join(
      chem_data,
      by = c("site_id", "visit"),
      suffix = c("", "_chem")
    )
    dim(chem_data) # 180
    dim(dat_raw) # 449
  
  
  return(dat_raw)
}
