clean_gga_data <- function(gga_data, dat_impute) {

# THIS SCRIPT WILL BE USED TO PLOT AND CLEAN GGA DATA IN PREPARATION
# FOR CALCULATION OF DIFFUSIVE EMISSION RATE

#1. LOOP TO ASSIGN SITEID AND DEPLY TIME TO GGA OBSERVATIONS.
# 16 rows in dat_raw have NA for chamb_deply_date_time. These are all from visit 1
# when the gas analyzer wasnt working.We want to remove these rows, or the missing
# values complicate the loop.

# Create a 210-second matching window after each chamber deployment.
# The one-minute lead-in accommodates small differences between field and GGA times.
chamber_windows <- dat_impute %>%
  filter(!is.na(chamb_deply_date_time)) %>%
  mutate(
    chamber_row = row_number(),
    window_start = chamb_deply_date_time - 60,
    window_end = chamb_deply_date_time + 210,
    visit_field = visit
  ) %>%
  select(
    chamber_row,
    matched_site_id = site_id,
    matched_visit = visit_field,
    matched_deply_date_time = chamb_deply_date_time,
    window_start,
    window_end
  )

# Match each GGA observation to its chamber window and transfer the field metadata.
gga_data <- gga_data %>%
  mutate(gga_row = row_number()) %>%
  left_join(
    chamber_windows,
    join_by(date_time > window_start, date_time < window_end),
    relationship = "many-to-many"
  ) %>%
  mutate(
    match_distance = abs(as.numeric(date_time - matched_deply_date_time))
  ) %>%
  group_by(gga_row) %>%
  # If windows overlap, retain the chamber deployment closest to the GGA time.
  arrange(is.na(match_distance), match_distance, chamber_row, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  mutate(
    # Assign site, visit, deployment, and retrieval times from the matched field record.
    site_id = matched_site_id,
    visit = matched_visit,
    co2_deply_date_time = if_else(
      !is.na(chamber_row),
      matched_deply_date_time,
      as.POSIXct(NA, tz = "America/New_York")
    ),
    co2_ret_date_time = if_else(
      !is.na(chamber_row),
      matched_deply_date_time + 210,
      as.POSIXct(NA, tz = "America/New_York")
    ),
    ch4_deply_date_time = if_else(
      !is.na(chamber_row),
      matched_deply_date_time,
      as.POSIXct(NA, tz = "America/New_York")
    ),
    ch4_ret_date_time = if_else(
      !is.na(chamber_row),
      matched_deply_date_time + 210,
      as.POSIXct(NA, tz = "America/New_York")
    )
  ) %>%
  ungroup() %>%
  arrange(gga_row) %>%
  select(
    -gga_row,
    -chamber_row,
    -matched_site_id,
    -matched_visit,
    -matched_deply_date_time,
    -window_start,
    -window_end,
    -match_distance
  )


#3. RECORD ADJUSTMENTS TO TIME SERIES PLOTS
  # Jake will do visits 
adj_data <- tribble(
  ~site_id, ~visit, ~co2_deply_date_time, ~co2_ret_date_time, ~ch4_deply_date_time, ~ch4_ret_date_time,
  # visit 9
  53, 9, "2018-05-23 08:20:51", "2018-05-23 08:24:10", "2018-05-23 08:20:51", "2018-05-23 08:24:10",
  50, 9, "2018-05-23 08:43:58", "2018-05-23 08:48:53", "2018-05-23 08:43:58", "2018-05-23 08:48:53",
  44, 9, "2018-05-23 08:57:08", "2018-05-23 09:02:39", "2018-05-23 08:57:08", "2018-05-23 08:57:35",
  52, 9, "2018-05-23 09:11:45", "2018-05-23 09:16:32", "2018-05-23 09:10:00", "2018-05-23 09:15:00",
  56, 9, "2018-05-23 09:29:07", "2018-05-23 09:33:02", "2018-05-23 09:28:52", "2018-05-23 09:33:01",
  47, 9, "2018-05-23 09:37:34", "2018-05-23 09:42:47", "2018-05-23 09:37:34", "2018-05-23 09:42:47",
  43, 9, "2018-05-23 09:47:35", "2018-05-23 09:52:43", "2018-05-23 09:47:35", "2018-05-23 09:52:43",
  54, 9, "2018-05-23 10:10:24", "2018-05-23 10:15:31", "2018-05-23 10:09:59", "2018-05-23 10:15:30",
  41, 9, "2018-05-23 10:29:44", "2018-05-23 10:33:18", "2018-05-23 10:29:48", "2018-05-23 10:33:09",
  49, 9, "2018-05-23 10:40:30", "2018-05-23 10:45:07", "2018-05-23 10:40:30", "2018-05-23 10:44:25",
  57, 9, "2018-05-23 10:54:19", "2018-05-23 10:59:14", "2018-05-23 10:54:19", "2018-05-23 10:59:12",
  45, 9, "2018-05-23 11:04:55", "2018-05-23 11:09:39", "2018-05-23 11:04:55", "2018-05-23 11:09:38",
  10, 9, "2018-05-23 11:13:45", "2018-05-23 11:16:49", "2018-05-23 11:13:41", "2018-05-23 11:16:46",
  4, 9, "2018-05-23 11:25:49", "2018-05-23 11:30:46", "2018-05-23 11:25:47", "2018-05-23 11:30:46",
  8, 9, "2018-05-23 11:36:29", "2018-05-23 11:41:48", "2018-05-23 11:36:51", "2018-05-23 11:41:48",
  3, 9, "2018-05-23 11:47:52", "2018-05-23 11:53:05", "2018-05-23 11:47:36", "2018-05-23 11:49:02",
  14, 9, "2018-05-23 12:05:05", "2018-05-23 12:09:59", "2018-05-23 12:05:53", "2018-05-23 12:09:59",
  15, 9, "2018-05-23 12:23:18", "2018-05-23 12:28:38", "2018-05-23 12:23:39", "2018-05-23 12:28:35",
  11, 9, "2018-05-23 12:47:03", "2018-05-23 12:53:57", "2018-05-23 12:47:03", "2018-05-23 12:53:58",
  7, 9, "2018-05-23 13:22:51", "2018-05-23 13:27:47", "2018-05-23 13:22:51", "2018-05-23 13:27:45",
  1, 9, "2018-05-23 13:40:59", "2018-05-23 13:45:02", "2018-05-23 13:41:05", "2018-05-23 13:45:00",
  21, 9, "2018-05-23 13:52:59", "2018-05-23 13:58:30", "2018-05-23 13:52:59", "2018-05-23 13:58:30",
  13, 9, "2018-05-23 14:06:30", "2018-05-23 14:13:03", "2018-05-23 14:06:59", "2018-05-23 14:13:03",
  12, 9, "2018-05-23 14:26:18", "2018-05-23 14:31:40", "2018-05-23 14:26:28", "2018-05-23 14:31:40",
  5, 9, "2018-05-23 14:43:04", "2018-05-23 14:48:26", "2018-05-23 14:43:16", "2018-05-23 14:48:26",
  2, 9, "2018-05-23 15:00:06", "2018-05-23 15:04:29", "2018-05-23 14:59:52", "2018-05-23 15:04:27",
  16, 9, "2018-05-23 15:16:37", "2018-05-23 15:20:56", "2018-05-23 15:16:37", "2018-05-23 15:20:56",
  24, 9, "2018-05-23 15:28:20", "2018-05-23 15:33:18", "2018-05-23 15:33:12", "2018-05-23 15:33:17",

  # visit 10
  53, 10, "2018-06-13 08:27:17", "2018-06-13 08:32:32", "2018-06-13 08:27:17", "2018-06-13 08:32:32",
  42, 10, "2018-06-13 08:42:56", "2018-06-13 08:46:20", "2018-06-13 08:42:56", "2018-06-13 08:46:20",
  50, 10, "2018-06-13 08:52:42", "2018-06-13 08:57:10", "2018-06-13 08:53:07", "2018-06-13 08:57:10",
  44, 10, "2018-06-13 09:03:29", "2018-06-13 09:08:47", "2018-06-13 09:03:18", "2018-06-13 09:04:38",
  52, 10, "2018-06-13 09:19:23", "2018-06-13 09:24:29", "2018-06-13 09:19:12", "2018-06-13 09:20:55",
  56, 10, "2018-06-13 09:29:51", "2018-06-13 09:35:20", "2018-06-13 09:29:54", "2018-06-13 09:31:45",
  47, 10, "2018-06-13 09:41:04", "2018-06-13 09:46:24", "2018-06-13 09:41:01", "2018-06-13 09:42:45",
  43, 10, "2018-06-13 09:57:29", "2018-06-13 10:02:59", "2018-06-13 09:57:38", "2018-06-13 09:59:07",
  54, 10, "2018-06-13 10:07:51", "2018-06-13 10:12:12", "2018-06-13 10:08:08", "2018-06-13 10:09:33",
  55, 10, "2018-06-13 10:18:36", "2018-06-13 10:23:23", "2018-06-13 10:18:58", "2018-06-13 10:23:22",
  41, 10, "2018-06-13 10:31:12", "2018-06-13 10:34:40", "2018-06-13 10:31:11", "2018-06-13 10:34:40",
  49, 10, "2018-06-13 10:44:13", "2018-06-13 10:47:55", "2018-06-13 10:45:25", "2018-06-13 10:47:30",
  57, 10, "2018-06-13 10:53:34", "2018-06-13 10:57:42", "2018-06-13 10:52:24", "2018-06-13 10:57:42",
  45, 10, "2018-06-13 11:05:23", "2018-06-13 11:08:46", "2018-06-13 11:05:23", "2018-06-13 11:08:47",
  10, 10, "2018-06-13 11:17:17", "2018-06-13 11:20:47", "2018-06-13 11:17:43", "2018-06-13 11:20:46",
  4, 10, "2018-06-13 11:29:40", "2018-06-13 11:34:15", "2018-06-13 11:30:07", "2018-06-13 11:35:49",
  8, 10, "2018-06-13 11:44:44", "2018-06-13 11:48:24", "2018-06-13 11:44:33", "2018-06-13 11:47:29",
  3, 10, "2018-06-13 11:59:01", "2018-06-13 12:04:36", "2018-06-13 11:59:13", "2018-06-13 12:04:36",
  14, 10, "2018-06-13 12:25:13", "2018-06-13 12:30:13", "2018-06-13 12:25:12", "2018-06-13 12:26:39",
  15, 10, "2018-06-13 12:46:20", "2018-06-13 12:51:44", "2018-06-13 12:46:15", "2018-06-13 12:51:44",
  11, 10, "2018-06-13 13:02:11", "2018-06-13 13:06:06", "2018-06-13 13:02:04", "2018-06-13 13:06:04",
  7, 10, "2018-06-13 13:42:34", "2018-06-13 13:45:25", "2018-06-13 13:40:04", "2018-06-13 13:41:14",
  1, 10, "2018-06-13 14:24:48", "2018-06-13 14:28:38", "2018-06-13 14:24:52", "2018-06-13 14:28:41",
  21, 10, "2018-06-13 14:36:14", "2018-06-13 14:41:38", "2018-06-13 14:36:13", "2018-06-13 14:41:38",
  13, 10, "2018-06-13 14:50:44", "2018-06-13 14:54:49", "2018-06-13 14:50:35", "2018-06-13 14:54:49",
  12, 10, "2018-06-13 15:10:59", "2018-06-13 15:14:31", "2018-06-13 15:10:37", "2018-06-13 15:14:29",
  5, 10, "2018-06-13 15:23:25", "2018-06-13 15:28:46", "2018-06-13 15:23:19", "2018-06-13 15:28:49",
  2, 10, "2018-06-13 15:43:09", "2018-06-13 15:48:04", "2018-06-13 15:43:10", "2018-06-13 15:48:05",
  16, 10, "2018-06-13 16:13:35", "2018-06-13 16:17:24", "2018-06-13 16:13:17", "2018-06-13 16:14:56",
  24, 10, "2018-06-13 16:27:34", "2018-06-13 16:31:49", "2018-06-13 16:27:21", "2018-06-13 16:31:49"

) %>%
# Convert date/time data from character to POSIXct
  mutate(
    co2_deply_date_time = as.POSIXct(co2_deply_date_time, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"),
    co2_ret_date_time = as.POSIXct(co2_ret_date_time, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"),
    ch4_deply_date_time = as.POSIXct(ch4_deply_date_time, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"),
    ch4_ret_date_time = as.POSIXct(ch4_ret_date_time, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
  )

#4. UPDATE DEPLOYMENT AND RETRIEVAL TIMES BASED ON FIXES ABOVE (SEE POINT 3)
# `adj_data` contains manual corrections to chamber deployment and retrieval
# times. Each row represents one site-by-visit combination.
#
# For each correction, calculate the earliest and latest corrected timestamp.
# These bounds define the period of the GGA record that should receive the
# corrected metadata. `rowwise()` is used because the min/max calculation is
# performed across four timestamp columns within each row.
adjustment_windows <- adj_data %>%
  rowwise() %>%
  mutate(
    adjustment_start = min(
      c(
        co2_deply_date_time,
        co2_ret_date_time,
        ch4_deply_date_time,
        ch4_ret_date_time
      ),
      na.rm = TRUE
    ),
    adjustment_end = max(
      c(
        co2_deply_date_time,
        co2_ret_date_time,
        ch4_deply_date_time,
        ch4_ret_date_time
      ),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  select(
    site_id,
    visit,
    adjustment_start,
    adjustment_end,
    co2_deply_date_time,
    co2_ret_date_time,
    ch4_deply_date_time,
    ch4_ret_date_time
  )

# Join the correction table to GGA using the identifiers already assigned above.
# A left join keeps every GGA observation, including observations with no manual
# correction. The `_adjusted` suffix distinguishes corrected timestamps from
# the original timestamp columns in `gga_data`.
# Manual corrections apply to every GGA observation for the matching site/visit.
gga_data_clean <- gga_data %>%
  left_join(
    adjustment_windows,
    by = c("site_id", "visit"),
    suffix = c("", "_adjusted")
  ) %>%
  mutate(
    # Replace timestamps for every row matching the corrected site and visit.
    co2_deply_date_time = if_else(
      !is.na(co2_deply_date_time_adjusted),
      co2_deply_date_time_adjusted,
      co2_deply_date_time
    ),
    co2_ret_date_time = if_else(
      !is.na(co2_ret_date_time_adjusted),
      co2_ret_date_time_adjusted,
      co2_ret_date_time
    ),
    ch4_deply_date_time = if_else(
      !is.na(ch4_deply_date_time_adjusted),
      ch4_deply_date_time_adjusted,
      ch4_deply_date_time
    ),
    ch4_ret_date_time = if_else(
      !is.na(ch4_ret_date_time_adjusted),
      ch4_ret_date_time_adjusted,
      ch4_ret_date_time
    )
  ) %>%
  select(
    # Remove helper columns used only to identify and apply corrections.
    -adjustment_start,
    -adjustment_end,
    -ends_with("_adjusted")
  )




#5.  PLOT CO2 AND CH4 PROFILES FOR INSPECTION

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
pdf("output/figures/gga_profile.pdf", paper = "a4r") # landscape orientation

plot_groups <- gga_data_clean %>%
  filter(!is.na(site_id), !is.na(visit)) %>%
  distinct(site_id, visit)

for (i in seq_len(nrow(plot_groups))) {
  data.i <- gga_data_clean %>%
    filter(
      site_id == plot_groups$site_id[i],
      visit == plot_groups$visit[i]
    )

  plot_data <- data.i %>%
    select(
      date_time,
      ch4_gga,
      co2_gga,
      ch4_deply_date_time,
      ch4_ret_date_time,
      co2_deply_date_time,
      co2_ret_date_time
    ) %>%
    pivot_longer(
      cols = c(ch4_gga, co2_gga),
      names_to = "gas",
      values_to = "concentration"
    ) %>%
    mutate(gas = recode(gas, ch4_gga = "CH4", co2_gga = "CO2"))

  vlines <- tibble(
    gas = c("CH4", "CO2"),
    deployment = c(
      data.i$ch4_deply_date_time[which(!is.na(data.i$ch4_deply_date_time))[1]],
      data.i$co2_deply_date_time[which(!is.na(data.i$co2_deply_date_time))[1]]
    ),
    retrieval = c(
      data.i$ch4_ret_date_time[which(!is.na(data.i$ch4_ret_date_time))[1]],
      data.i$co2_ret_date_time[which(!is.na(data.i$co2_ret_date_time))[1]]
    )
  )

  print(
    ggplot(plot_data, aes(x = date_time, y = concentration)) +
      geom_point() +
      geom_vline(
        data = vlines,
        aes(xintercept = as.numeric(deployment)),
        inherit.aes = FALSE,
        linetype = "dashed",
        na.rm = TRUE
      ) +
      geom_vline(
        data = vlines,
        aes(xintercept = as.numeric(retrieval)),
        inherit.aes = FALSE,
        linetype = "dashed",
        na.rm = TRUE
      ) +
      facet_wrap(vars(gas), ncol = 2, scales = "free_y") +
      scale_x_datetime(date_labels = "%H:%M") +
      labs(
        title = paste("Site", plot_groups$site_id[i], "Visit", plot_groups$visit[i]),
        x = NULL,
        y = "Concentration (ppm)"
      ) +
      theme(
        axis.text.x = element_text(size = 7),
        plot.title = element_text(size = 11)
      )
  )
}

dev.off()

# 6. INTERACTIVE CO2 AND CH4 PROFILE PLOTS

make_vline_shapes <- function(vlines, gas_name, x_reference) {
  line_data <- vlines %>%
    filter(gas == gas_name) %>%
    tidyr::pivot_longer(
      cols = c(deployment, retrieval),
      names_to = "event",
      values_to = "timestamp"
    ) %>%
    filter(!is.na(timestamp))

  lapply(seq_len(nrow(line_data)), function(j) {
    list(
      type = "line",
      x0 = format(line_data$timestamp[j], "%Y-%m-%dT%H:%M:%S", tz = "America/New_York"),
      x1 = format(line_data$timestamp[j], "%Y-%m-%dT%H:%M:%S", tz = "America/New_York"),
      y0 = 0,
      y1 = 1,
      xref = x_reference,
      yref = "paper",
      line = list(
        color = if_else(line_data$event[j] == "deployment", "black", "red"),
        dash = "dash",
        width = 1
      )
    )
  })
}

interactive_profiles <- lapply(seq_len(nrow(plot_groups)), function(i) {
    data.i <- gga_data_clean %>%
      filter(
        site_id == plot_groups$site_id[i],
        visit == plot_groups$visit[i]
      )

    vlines <- tibble(
      gas = c("CH4", "CO2"),
      deployment = c(
        data.i$ch4_deply_date_time[which(!is.na(data.i$ch4_deply_date_time))[1]],
        data.i$co2_deply_date_time[which(!is.na(data.i$co2_deply_date_time))[1]]
      ),
      retrieval = c(
        data.i$ch4_ret_date_time[which(!is.na(data.i$ch4_ret_date_time))[1]],
        data.i$co2_ret_date_time[which(!is.na(data.i$co2_ret_date_time))[1]]
      )
    )

    # Use the raw GGA data because gga_data_clean only receives site metadata
    # through the chamber-matching window, which ends at retrieval.
    profile_data <- gga_data %>%
      filter(
        date_time >= min(vlines$deployment, na.rm = TRUE) - 60,
        date_time <= max(vlines$retrieval, na.rm = TRUE) + 120
      )

    # Keep each gas profile through two minutes after its retrieval time.
    ch4_data <- profile_data %>%
      filter(
        !is.na(ch4_gga),
        date_time <= vlines$retrieval[vlines$gas == "CH4"] + 120
      )

    co2_data <- profile_data %>%
      filter(
        !is.na(co2_gga),
        date_time <= vlines$retrieval[vlines$gas == "CO2"] + 120
      )

    ch4_plot <- plotly::plot_ly(
      data = ch4_data,
      x = ~date_time,
      y = ~ch4_gga,
      type = "scatter",
      mode = "markers",
      name = "CH4",
      hovertemplate = paste(
        "Time: %{x}<br>",
        "CH4: %{y:.2f} ppm<extra></extra>"
      )
    ) %>%
      plotly::layout(
        title = "CH4",
        xaxis = list(title = NULL),
        yaxis = list(title = "Concentration (ppm)"),
        shapes = make_vline_shapes(vlines, "CH4", "x")
      )

    co2_plot <- plotly::plot_ly(
      data = co2_data,
      x = ~date_time,
      y = ~co2_gga,
      type = "scatter",
      mode = "markers",
      name = "CO2",
      hovertemplate = paste(
        "Time: %{x}<br>",
        "CO2: %{y:.2f} ppm<extra></extra>"
      )
    ) %>%
      plotly::layout(
        title = "CO2",
        xaxis = list(title = "Date and time"),
        yaxis = list(title = "Concentration (ppm)"),
        shapes = make_vline_shapes(vlines, "CO2", "x2")
      )

    plotly::subplot(
      ch4_plot,
      co2_plot,
      nrows = 1,
      shareX = TRUE,
      titleX = TRUE,
      titleY = TRUE
    ) %>%
      plotly::layout(
        title = paste(
          "Site", plot_groups$site_id[i],
          "Visit", plot_groups$visit[i]
        )
      )
})

names(interactive_profiles) <- paste(
  "site", plot_groups$site_id,
  "visit", plot_groups$visit,
  sep = "_"
)

print(interactive_profiles[["site_50_visit_9"]])

  
  return(gga_data_clean)
}








