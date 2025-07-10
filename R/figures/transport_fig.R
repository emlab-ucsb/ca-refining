## february 26, 2024
## pm2.5 figure

# Import save functions for structure-compliant file saving
source("R/save_functions.R")

## pulse fig info x refinery
## --------------------------------------------------

# test <- ggplot() +
#   geom_sf(data = raw_ct_2020_all %>% filter(STATEFP == "06" & COUNTYFP %in% c("095", "013")), aes(geometry = geometry), fill="#A84268", color= "black") +
#   # theme_void() +
#   theme_minimal() +
#   labs( y = NULL,
#        x = NULL) +
#   geom_sf(data = raw_counties %>% filter(STATEFP == "06" & COUNTYFP %in% c("095", "013")), mapping = aes(geometry = geometry), lwd = 0.15, alpha = 0)

create_srm_xwalk <- function(
  main_path,
  save_path,
  srm_weighted_pm25,
  ct_xwalk,
  raw_counties,
  raw_ct_2020_all
) {
  srm_pm25_df <- copy(srm_weighted_pm25)
  srm_pm25_df[, GEOID := paste0("0", GEOID)]

  srm_pm25_df[,
    weighted_total_pm25 := weighted_totalpm25_nh3 +
      weighted_totalpm25_nox +
      weighted_totalpm25_pm25 +
      weighted_totalpm25_sox +
      weighted_totalpm25_voc
  ]

  ## Adjust mismatch of census tract ids between inmap and benmap (census ID changed in 2012
  ## http://www.diversitydatakids.org/sites/default/files/2020-02/ddk_coi2.0_technical_documentation_20200212.pdf)
  srm_pm25_df[, GEOID := ifelse(GEOID == "06037137000", "06037930401", GEOID)]

  ## select relevant columns from xwalk
  ct_xwalk_df <- copy(ct_xwalk)

  ## select relevant columns from xwalk
  setDT(ct_xwalk_df)
  ct_xwalk_df <- ct_xwalk_df[, .(GEOID_2020, GEOID_2019, rel_intersect)]

  ## prepare pollution output
  setnames(srm_pm25_df, "GEOID", "GEOID_2019")

  ## merge
  srm_pm25_df <- merge(
    srm_pm25_df,
    ct_xwalk,
    by = c("GEOID_2019"),
    all = TRUE,
    allow.cartesian = TRUE
  )

  ## calculate pm2.5 for 2020 census tract, weight by rel_intersection
  srm_pm25_df <- srm_pm25_df[,
    .(
      weighted_total_pm25 = weighted.mean(
        weighted_total_pm25,
        rel_intersect,
        na.rm = T
      )
    ),
    by = .(site_id, GEOID_2020)
  ]

  ## filter out NA GEOID_2020, NA pm2.5
  srm_pm25_df <- srm_pm25_df[!is.na(GEOID_2020)]

  ## rename columns
  setnames(
    srm_pm25_df,
    c("GEOID_2020", "weighted_total_pm25"),
    c("census_tract", "total_pm25")
  )

  ## county names
  county_names <- raw_counties %>%
    select(COUNTYFP, NAME) %>%
    st_drop_geometry() %>%
    unique()

  ## geoid to census tract
  county_df <- raw_ct_2020_all %>%
    filter(STATEFP == "06") %>%
    select(census_tract = GEOID, COUNTYFP, ALAND) %>%
    st_drop_geometry() %>%
    left_join(county_names) %>%
    select(census_tract, COUNTYFP, NAME)

  ## add counties to srm
  srm_pm25_df <- merge(
    srm_pm25_df,
    county_df,
    by = c("census_tract"),
    all = TRUE,
    allow.cartesian = TRUE
  )

  ## File saving is now handled by targets pipeline
  ## Previously saved with:
  ## simple_fwrite_repo(
  ##   srm_pm25_df,
  ##   folder_path = NULL,
  ##   filename = "srm_pm25_refinery_level.csv",
  ##   save_path = save_path,
  ##   file_type = "table",
  ##   figure_number = NULL,
  ##   extra_subfolder = "pulse-figs"
  ## )

  return(srm_pm25_df)
}

create_srm_ct <- function(main_path, save_path, refinery_pm25_srm) {
  pm25_srm <- copy(refinery_pm25_srm)
  setDT(pm25_srm)

  pm25_srm <- pm25_srm[,
    .(total_pm25 = sum(total_pm25)),
    by = .(census_tract, COUNTYFP, NAME)
  ]

  ## File saving is now handled by targets pipeline
  ## Previously saved with:
  ## simple_fwrite_repo(
  ##   pm25_srm,
  ##   folder_path = NULL,
  ##   filename = "srm_pm25_ct.csv",
  ##   save_path = save_path,
  ##   file_type = "table",
  ##   figure_number = NULL,
  ##   extra_subfolder = "pulse-figs"
  ## )

  return(pm25_srm)
}

create_pulse_fig <- function(
  main_path,
  save_path,
  refinery_pm25_srm,
  ct_pm25_srm,
  raw_counties,
  raw_ct_2020_all,
  refin_locs,
  ca_crs
) {
  ## Refineries plus
  refin_new_locations <- copy(refin_locs)

  ## join census tract sp data with ct pm2.5
  ct_pm25_srm_sp <- ct_pm25_srm %>%
    left_join(
      raw_ct_2020_all %>%
        filter(STATEFP == "06") %>%
        select(census_tract = GEOID, geometry)
    ) %>%
    st_as_sf() %>%
    st_transform(ca_crs)

  ## join census tract sp data with refinery level pm2.5 srm
  refinery_pm25_srm_sp <- refinery_pm25_srm %>%
    left_join(
      raw_ct_2020_all %>%
        filter(STATEFP == "06") %>%
        select(census_tract = GEOID, geometry)
    ) %>%
    filter(!is.na(site_id)) %>%
    st_as_sf() %>%
    st_transform(ca_crs)

  ## site ids
  site_ids <- unique(refinery_pm25_srm$site_id)

  ## for each site id, plot and put relevant county boundaries

  for (i in 1:length(site_ids)) {
    id_tmp <- site_ids[i]

    srm_tmp <- refinery_pm25_srm_sp %>%
      filter(site_id == id_tmp)

    srm_tmp_c <- refinery_pm25_srm_sp %>%
      filter(site_id == id_tmp) %>%
      filter(total_pm25 >= 0.0001)

    county_plot_tmp <- srm_tmp_c %>%
      select(COUNTYFP) %>%
      st_drop_geometry() %>%
      unique()

    county_tmp <- raw_counties %>%
      filter(COUNTYFP %in% unique(srm_tmp_c$COUNTYFP))

    refin_tmp <- refin_new_locations %>%
      filter(site_id == id_tmp)

    refin_tmp_name <- as.character(refin_tmp$refinery_name[1])

    ## figure
    pm25_fig_tmp <- ggplot() +
      geom_sf(
        data = srm_tmp %>% filter(COUNTYFP %in% unique(srm_tmp_c$COUNTYFP)),
        aes(fill = total_pm25, geometry = geometry),
        color = NA
      ) +
      # theme_void() +
      theme_minimal() +
      scale_fill_gradient(
        high = "#A84268",
        low = "#FAFAFA",
        space = "Lab",
        na.value = "grey50",
        limits = c(min(srm_tmp$total_pm25), max(srm_tmp$total_pm25)),
        breaks = c(0, 0.004)
      ) +
      labs(
        title = paste0("PM2.5 concentration from ", refin_tmp_name),
        y = NULL,
        x = NULL,
        # title = bold(expression(bold(paste("PM"[2.5], " concentration from ", refin_tmp_name))),
        fill = expression(paste("PM"[2.5], " (", mu, "/", m^3, ")"))
      ) +
      geom_sf(
        data = county_tmp,
        mapping = aes(geometry = geometry),
        lwd = 0.15,
        alpha = 0
      ) +
      geom_sf_text(data = county_tmp, aes(label = NAME)) +
      geom_sf(
        data = refin_tmp,
        mapping = aes(geometry = geometry),
        alpha = 0.9,
        pch = 16
      ) +
      theme(
        # legend.justification defines the edge of the legend that the legend.position coordinates refer to
        legend.justification = c(0, 1),
        # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
        legend.position = c(0.05, 0.15),
        legend.key.width = unit(0.7, "line"),
        legend.key.height = unit(0.5, "line"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.margin = margin(0, 2, 0, 8),
        plot.title = element_text(face = "bold", size = 8, hjust = -0.05)
      ) +
      guides(
        fill = guide_colourbar(
          title.position = "top",
          title.hjust = 0,
          direction = "horizontal",
          ticks.colour = "black",
          frame.colour = "black",
          order = 1
        )
      )

    # ggsave(plot = pm25_fig_tmp,
    #        filename = paste0(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/pulse-figs/pulse_",
    #                          id_tmp, ".jpeg"),
    #        device = "jpeg",
    #        # width = 6.5,
    #        # height = 8,
    #        dpi = 300)

    # NOTE from Meas: the plot save directory is hard coded in this function instead of _targets.R, not always easy to track
    # I've added `create.dir = TRUE` otherwise the function will fail if the directory doesn't exist
    # I also added the usage of `file.path` to make it more robust to whether a path has "/" at the end or not
    # NOTE from tracey: this throws and erorr for me (Error in f(...): unused argument (create.dir = TRUE))
    # added code to create the directory if it doesn't exist

    # Save using simple_ggsave_repo to ensure consistent structure and tracking
    # Using "extra" as file_type since these are not main figures
    simple_ggsave_repo(
      plot = pm25_fig_tmp,
      folder_path = file.path(
        save_path,
        "results",
        "figures",
        "extra",
        "pulse-figs"
      ),
      filename = paste0("pulse_", id_tmp),
      width = 7,
      height = 5,
      dpi = 300
    )
  }

  ## make fig for all locations

  ## crop
  disp_win2_wgs84 <- st_sfc(
    st_point(c(-122.5, 33)),
    st_point(c(-117, 39)),
    crs = 4326
  )

  disp_win2_trans <- st_transform(disp_win2_wgs84, crs = ca_crs)

  disp_win2_coord <- st_coordinates(disp_win2_trans)

  disp_win_df <- as.data.frame(disp_win2_coord)

  ## limits for zoom
  xlim <- c(disp_win_df$X[1], disp_win_df$X[2]) # Set limits for zoom panel
  ylim <- c(disp_win_df$Y[1], disp_win_df$Y[2])

  ## plot
  pm25_fig_all <- ggplot() +
    geom_sf(
      data = ct_pm25_srm_sp,
      aes(fill = total_pm25, geometry = geometry),
      color = NA
    ) +
    theme_void() +
    # theme_minimal() +
    scale_fill_gradient(
      high = "#A84268",
      low = "#FAFAFA",
      space = "Lab",
      na.value = "grey50",
      limits = c(
        min(ct_pm25_srm_sp$total_pm25),
        max(ct_pm25_srm_sp$total_pm25)
      ),
      breaks = c(0.001, 0.02)
    ) +
    labs(
      title = "PM2.5 concentration from refineries",
      y = NULL,
      x = NULL,
      # title = bold(expression(bold(paste("PM"[2.5], " concentration from ", refin_tmp_name))),
      fill = expression(paste("PM"[2.5], " (", mu, "/", m^3, ")"))
    ) +
    geom_sf(
      data = raw_counties,
      mapping = aes(geometry = geometry),
      lwd = 0.15,
      alpha = 0
    ) +
    # geom_sf_text(data = raw_counties, aes(label = NAME), size = 3) +
    geom_sf(
      data = refin_new_locations,
      mapping = aes(geometry = geometry),
      alpha = 0.8,
      pch = 16,
      size = 0.5
    ) +
    coord_sf(
      xlim = disp_win2_coord[, "X"],
      ylim = disp_win2_coord[, "Y"],
      datum = ca_crs,
      expand = FALSE
    ) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0.05, 0.15),
      legend.key.width = unit(0.7, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      plot.margin = margin(0, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 12, hjust = -0.05)
    ) +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0,
        direction = "horizontal",
        ticks.colour = "black",
        frame.colour = "black",
        order = 1
      )
    )

  # ggsave(plot = pm25_fig_all,
  #        filename = paste0(main_path, "outputs/academic-out/refining/figures/2024-08-beta-adj/pulse-figs/pulse_all_crop.jpeg"),
  #        device = "jpeg",
  #        # width = 6.5,
  #        # height = 8,
  #        dpi = 300)
  #

  # NOTE from Meas: same as above, I switched to using `file.path` instead of paste0
  # NOTE from Tracey: same error! removed `create.dir = TRUE`
  # Save using simple_ggsave_repo to ensure consistent structure and tracking
  # Using direct folder path instead of file_type/figure_number to avoid the error
  simple_ggsave_repo(
    plot = pm25_fig_all,
    folder_path = file.path(
      save_path,
      "results",
      "figures",
      "extra",
      "pulse-figs"
    ),
    filename = "pulse_all_crop",
    width = 7,
    height = 5,
    dpi = 300
  )

  return(pm25_fig_all)
}

# ## crop
# ## -----------------------------------
# disp_win_la_wgs84 <- st_sfc(st_point(c(-118.5, 33.6)), st_point(c(-117.8, 34.2)),
#                             crs = 4326)
#
# disp_win_la_trans <- st_transform(disp_win_la_wgs84, crs = ca_crs)
#
# disp_win_la_coord <- st_coordinates(disp_win_la_trans)
#
# zoom_coord_df <- as.data.frame(disp_win_la_coord)
#
# county_crop <- st_crop(CA_counties_noisl, xmin = zoom_coord_df$X[1], xmax = zoom_coord_df$X[2], ymin = zoom_coord_df$Y[1], ymax = zoom_coord_df$Y[2])
# ct_cropped <- st_crop(ct_map_county, xmin = zoom_coord_df$X[1], xmax = zoom_coord_df$X[2], ymin = zoom_coord_df$Y[1], ymax = zoom_coord_df$Y[2])
#
# ## only include census tracts that are in the crop
# ct_intersect <- st_intersection(ct_map_county, county_crop)

# ## crop
# ## -----------------------------------
# disp_win_la_wgs84 <- st_sfc(st_point(c(-118.5, 33.6)), st_point(c(-117.8, 34.2)),
#                             crs = 4326)
#
# disp_win_la_trans <- st_transform(disp_win_la_wgs84, crs = ca_crs)
#
# disp_win_la_coord <- st_coordinates(disp_win_la_trans)
#
# zoom_coord_df <- as.data.frame(disp_win_la_coord)
#
# county_crop <- st_crop(CA_counties_noisl, xmin = zoom_coord_df$X[1], xmax = zoom_coord_df$X[2], ymin = zoom_coord_df$Y[1], ymax = zoom_coord_df$Y[2])
# ct_cropped <- st_crop(ct_map_county, xmin = zoom_coord_df$X[1], xmax = zoom_coord_df$X[2], ymin = zoom_coord_df$Y[1], ymax = zoom_coord_df$Y[2])
#
# ## only include census tracts that are in the crop
# ct_intersect <- st_intersection(ct_map_county, county_crop)
