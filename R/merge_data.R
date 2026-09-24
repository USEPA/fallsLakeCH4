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
  
  
  
# check dat_raw for missing values
  # trap samples
  # any time a value was recorded for trap_extn1 there should be a value for trap_ch4_gc
  # Visit 4, sites 24, 14, 54, 59. CH4 was super low, seemingly in error. These values
  # were filtered out in read_gc
  # Visit 10, sites 47, 55. Field sheets show samples were collected, but they are not in
  # GC data file.
  # Visit 9, site 24, CH4 was super low, seemingly in error. These values
  # were filtered out in read_gc
  # Visit 14, site 47, filtered out because CH4 is tiny
  # Visit 14, site 12, sample code in field sheets, but no evidence that sample was analyzed.
  dat_raw |>
    dplyr::filter(!is.na(trap_extn1) & is.na(trap_ch4_gc)) |>
    dplyr::select(site_id, visit, trap_extn1, trap_ch4_gc)
  
  
  # dissolved samples
  # visit 11, sites 45, 47, 49, samples not analyzed
  # "visit 1-site 1", "visit 10-site 4", "visit 6, site 47", samples not analyzed
  dat_raw |>
    dplyr::filter(!is.na(dg_extn_temp) & is.na(dissolved_ch4_gc)) |>
    dplyr::select(site_id, visit, dg_extn_temp, dissolved_ch4_gc)

  return(dat_raw)
}
