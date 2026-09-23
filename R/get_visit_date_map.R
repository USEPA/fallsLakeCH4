# Create a hardcoded map from sampling dates to visit numbers.
get_visit_date_map <- function() {
  visit_date_map <-
    tibble::tribble(
      ~sample_date , ~visit ,
      "2017-09-06" ,      1 ,
      "2017-09-07" ,      1 ,
      # Added after inspecting unmatched chlorophyll dates; nearest mapped dates indicate visit 1.
      "2017-09-08" ,      1 ,
      "2017-10-18" ,      2 ,
      "2017-10-19" ,      2 ,
      "2017-10-20" ,      2 ,
      # Added after inspecting unmatched chlorophyll dates; nearest mapped dates indicate visit 2.
      "2017-10-21" ,      2 ,
      # Added after inspecting unmatched chlorophyll dates; nearest mapped dates indicate visit 3.
      "2017-11-27" ,      3 ,
      "2017-11-28" ,      3 ,
      "2017-11-29" ,      3 ,
      # Added after inspecting unmatched chlorophyll dates; nearest mapped dates indicate visit 4.
      "2018-01-11" ,      4 ,
      "2018-01-12" ,      4 ,
      "2018-01-13" ,      4 ,
      # Added after inspecting unmatched chlorophyll dates; nearest mapped dates indicate visit 5.
      "2018-01-23" ,      5 ,
      "2018-01-24" ,      5 ,
      "2018-01-25" ,      5 ,
      # Added after inspecting unmatched chlorophyll dates; nearest mapped dates indicate visit 6.
      "2018-02-12" ,      6 ,
      "2018-02-13" ,      6 ,
      "2018-02-14" ,      6 ,
      # Added after inspecting unmatched chlorophyll dates; nearest mapped dates indicate visit 7.
      "2018-03-12" ,      7 ,
      "2018-03-13" ,      7 ,
      "2018-03-14" ,      7 ,
      # Added after inspecting unmatched chlorophyll dates; nearest mapped dates indicate visit 8.
      "2018-04-03" ,      8 ,
      "2018-04-04" ,      8 ,
      "2018-04-05" ,      8 ,
      "2018-05-22" ,      9 ,
      "2018-05-23" ,      9 ,
      "2018-06-12" ,     10 ,
      "2018-06-13" ,     10 ,
      "2018-07-10" ,     11 ,
      "2018-07-11" ,     11 ,
      "2018-08-20" ,     12 ,
      "2018-08-21" ,     12 ,
      "2018-09-25" ,     13 ,
      "2018-09-26" ,     13 ,
      "2018-10-22" ,     14 ,
      "2018-10-23" ,     14 ,
      "2018-11-19" ,     15 ,
      "2018-11-20" ,     15
    ) |>
    dplyr::mutate(sample_date = as.Date(sample_date))

  return(visit_date_map)
}
