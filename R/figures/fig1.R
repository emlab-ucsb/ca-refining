# ## figure 1
# ## original version: september 9, 2024
## updated version: march 30, 2025 - address reviewer comments


#' Helper function to create a proper coord_sf without graticule errors
#' @param coords Coordinates from st_coordinates()
#' @param expand Whether to expand the plot area
#' @return A coord_sf object with proper xlim and ylim
# safe_coord_sf <- function(coords, expand = FALSE) {
#   coord_sf(
#     xlim = c(coords[1, "X"], coords[2, "X"]),
#     ylim = c(coords[1, "Y"], coords[2, "Y"]),
#     expand = expand,
#     datum = NULL # Disable graticules to avoid MULTILINESTRING error
#   )
# }

safe_coord_sf <- function(coords, expand = FALSE) {
  coord_sf(
    xlim = c(coords[1, "X"], coords[2, "X"]),
    ylim = c(coords[1, "Y"], coords[2, "Y"]),
    expand = expand,
    datum = sf::st_crs(4326),  # specify the CRS for axis labels
    label_graticule = "SW"     # optional: format of axis ticks (S = south, W = west)
  )
}


create_figure_1 <- function(
  main_path,
  save_path,
  ca_crs,
  dt_refcap,
  refin_locs,
  dt_renref,
  renewables_info,
  dt_altair,
  refining_site_output,
  refining_sites_cons_ghg_2019_2045,
  raw_counties,
  raw_ct_2020_all,
  raw_ces,
  dt_inmap_re,
  raw_ct_2019,
  health_weighted,
  refining_mortality,
  labor_2019,
  ca_regions,
  raw_pop_income_2021,
  cpi2020,
  cpi2019,
  annual_direct_labor
) {
  ## califonia
  states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

  california <- states %>%
    filter(ID == "california") %>%
    st_transform(ca_crs)

  ## select site id and barrels per day for 3422 and 342
  man_capacity <- dt_refcap %>%
    filter(site_id %in% c(3422, 342)) %>%
    select(site_id, barrels_per_day)

  ## add coordinates to refineries
  # refin_new_locations = refin locs

  # ## site out
  # site_out <- fread(file.path(main_path, refin_out_path, site_out_file))

  ## figure 1a: refinery capacity locations
  ## ---------------------------------------------------------------------------

  ## alt air 2021 capacity
  aa_cap <- dt_altair %>%
    filter(year == 2021) %>%
    mutate(site_id = "t-800") %>%
    select(site_id, barrels_per_day, installation_year = year)

  ## renewables capacity
  renewable_cap <- renewables_info %>%
    left_join(dt_renref) %>%
    filter(site_id %in% c("342-2", "99999")) %>%
    select(
      site_id,
      barrels_per_day = installation_capacity_bpd,
      installation_year
    )

  ## future renewables capacity (all)
  fut_cap <- rbind(aa_cap, renewable_cap)

  ## capacity
  refin_capacity <- dt_refcap %>%
    mutate(site_id = as.character(site_id)) %>%
    select(site_id, barrels_per_day) %>%
    filter(!site_id %in% man_capacity$site_id) %>%
    full_join(man_capacity %>% mutate(site_id = as.character(site_id))) %>%
    full_join(fut_cap) %>%
    mutate(
      installation_year = ifelse(
        is.na(installation_year),
        "pre 2020",
        as.character(installation_year)
      )
    )

  ## join with locations
  refin_capacity <- refin_locs %>%
    left_join(refin_capacity) %>%
    mutate(
      installation = ifelse(
        installation_year == "pre 2020",
        "Existing capacity",
        "Future capacity"
      )
    )
  ## bbls after reductions

  ## counties boundaries
  county_boundaries <- raw_counties %>%
    st_transform(ca_crs) %>%
    dplyr::select(
      adj_county_name = NAME,
      OBJECTID
    )

  ## remove islands
  CA_counties_noisl <- county_boundaries %>%
    filter(!OBJECTID %in% c(3, 49))

  ## census tracts
  census_tracts <- raw_ct_2020_all %>%
    filter(STATEFP == "06") %>%
    st_transform(ca_crs) %>%
    rename(census_tract = GEOID) %>%
    select(census_tract, ALAND, COUNTYFP)

  ## DAC and CES

  ## dac
  dac_ces <- raw_ces %>%
    select(`Census Tract`, `SB 535 Disadvantaged Community`) %>%
    rename(
      census_tract = `Census Tract`,
      dac = `SB 535 Disadvantaged Community`
    ) %>%
    mutate(census_tract = paste0("0", census_tract, sep = "")) %>%
    mutate(ct_type = ifelse(dac == "Yes", "DAC", "Not DAC"))

  ## dac spatial
  dac_sp <- left_join(census_tracts, dac_ces)

  ## dac only
  dac_areas <- dac_sp %>%
    filter(dac == "Yes")

  ## panels a, b, c: current health outcomes, one panel per cluster
  ## PM2.5 concentration of all refinery emissions
  ## --------------------------------------------------------------------------

  ## filter for 2019 and BAU
  census_tract_pm25_2019 <- health_weighted[
    year == 2019 &
      scen_id == "BAU historic production"
  ]
  # population
  pop_2020 <- refining_mortality |>
    filter(year == 2020) |>
    select(census_tract, pop) |>
    unique() |>
    as.data.table()

  ## merge
  census_tract_pm25_2019 <- merge(
    census_tract_pm25_2019,
    pop_2020[, .(census_tract, pop)],
    by = c("census_tract"),
    all.x = T
  )

  ## weight by total population
  census_tract_pm25_2019[, pop_x_pm25 := total_pm25 * pop]
  ct_census_tract_pm25_2019 <- census_tract_pm25_2019[, .(
    scen_id,
    census_tract,
    disadvantaged,
    year,
    pop,
    total_pm25,
    pop_x_pm25
  )]


  ## join with spatial data
  ct_census_tract_pm25_2019_sp <- census_tracts %>%
    left_join(ct_census_tract_pm25_2019)

  ## --------------------------------------------------------------------------
  ## create coordinates for cluster zoom-ins
  ## --------------------------------------------------------------------------

  ## merge counties to census tracts
  ## -----------------------------------

  ## counties boundaries
  county_ct_boundaries <- raw_counties %>%
    st_transform(ca_crs) %>%
    dplyr::select(
      COUNTYFP,
      NAME,
      OBJECTID
    ) %>%
    filter(!OBJECTID %in% c(3, 49)) %>%
    st_drop_geometry() %>%
    unique() %>%
    rename(county_name = NAME) %>%
    select(-OBJECTID)

  ## create counties based on census tracts
  county_boundaries_from_census_tracts <- census_tracts |>
    group_by(COUNTYFP) |>
    summarise(geometry = st_union(geometry)) |>
    ungroup() |>
    left_join(county_ct_boundaries)

  ## north cluster - bay area
  ## -------------------------------------------------------------------

  disp_win_bay_cluster_wgs84 <- st_sfc(
    st_point(c(-122.5, 37.7)),
    st_point(c(-121.1, 38.6)),
    crs = 4326
  )

  disp_win_bay_cluster_transf <- st_transform(
    disp_win_bay_cluster_wgs84,
    crs = ca_crs
  )

  disp_win_bay_cluster_coord <- st_coordinates(disp_win_bay_cluster_transf)

  bay_cluster_zoom_coord_df <- as.data.frame(disp_win_bay_cluster_coord)

  ## crop county and census tracts
  bay_cluster_county_crop <- st_crop(
    county_boundaries_from_census_tracts,
    xmin = bay_cluster_zoom_coord_df$X[1],
    xmax = bay_cluster_zoom_coord_df$X[2],
    ymin = bay_cluster_zoom_coord_df$Y[1],
    ymax = bay_cluster_zoom_coord_df$Y[2]
  )
  bay_cluster_ct_cropped <- st_crop(
    ct_census_tract_pm25_2019_sp,
    xmin = bay_cluster_zoom_coord_df$X[1],
    xmax = bay_cluster_zoom_coord_df$X[2],
    ymin = bay_cluster_zoom_coord_df$Y[1],
    ymax = bay_cluster_zoom_coord_df$Y[2]
  )

  bay_cluster_ct_cropped <- bay_cluster_ct_cropped |>
    mutate(cluster_title = "North cluster: Bay area")
  

  ## north cluster - kern
  ## -------------------------------------------------------------------

  disp_win_kern_cluster_wgs84 <- st_sfc(
    st_point(c(-121.3, 34.7)),
    st_point(c(-117.8, 35.8)),
    crs = 4326
  )

  disp_win_kern_cluster_transf <- st_transform(
    disp_win_kern_cluster_wgs84,
    crs = ca_crs
  )

  disp_win_kern_cluster_coord <- st_coordinates(disp_win_kern_cluster_transf)

  kern_cluster_zoom_coord_df <- as.data.frame(disp_win_kern_cluster_coord)

  ## crop county and census tracts
  kern_cluster_county_crop <- st_crop(
    county_boundaries_from_census_tracts,
    xmin = kern_cluster_zoom_coord_df$X[1],
    xmax = kern_cluster_zoom_coord_df$X[2],
    ymin = kern_cluster_zoom_coord_df$Y[1],
    ymax = kern_cluster_zoom_coord_df$Y[2]
  )
  kern_cluster_ct_cropped <- st_crop(
    ct_census_tract_pm25_2019_sp,
    xmin = kern_cluster_zoom_coord_df$X[1],
    xmax = kern_cluster_zoom_coord_df$X[2],
    ymin = kern_cluster_zoom_coord_df$Y[1],
    ymax = kern_cluster_zoom_coord_df$Y[2]
  )

  kern_cluster_ct_cropped <- kern_cluster_ct_cropped |>
    mutate(cluster_title = "North cluster: Central")

  ## southern cluster - LA
  ## -------------------------------------------------------
  disp_win_la_cluster_wgs84 <- st_sfc(
    st_point(c(-119.0, 33.6)),
    st_point(c(-117.5, 34.5)),
    crs = 4326
  )

  disp_win_la_cluster_transf <- st_transform(
    disp_win_la_cluster_wgs84,
    crs = ca_crs
  )

  disp_win_la_cluster_coord <- st_coordinates(disp_win_la_cluster_transf)

  la_cluster_zoom_coord_df <- as.data.frame(disp_win_la_cluster_coord)

  ## crop county and census tracts
  la_cluster_county_crop <- st_crop(
    county_boundaries_from_census_tracts,
    xmin = la_cluster_zoom_coord_df$X[1],
    xmax = la_cluster_zoom_coord_df$X[2],
    ymin = la_cluster_zoom_coord_df$Y[1],
    ymax = la_cluster_zoom_coord_df$Y[2]
  )
  la_cluster_ct_cropped <- st_crop(
    ct_census_tract_pm25_2019_sp,
    xmin = la_cluster_zoom_coord_df$X[1],
    xmax = la_cluster_zoom_coord_df$X[2],
    ymin = la_cluster_zoom_coord_df$Y[1],
    ymax = la_cluster_zoom_coord_df$Y[2]
  )

  la_cluster_ct_cropped <- la_cluster_ct_cropped |>
    mutate(cluster_title = "South cluster")

  
  ## save health inputs
  ## ----------------------------------------------------------------------
  fig1_health_inputs <- rbind(bay_cluster_ct_cropped |> st_drop_geometry(),
                              la_cluster_ct_cropped |> st_drop_geometry())
  
  if (!dir.exists(file.path(save_path, "figure-1"))) {
    dir.create(file.path(save_path, "figure-1"), recursive = TRUE)
  }
  
  
  ## save figure inputs
  simple_fwrite_repo(
    data = fig1_health_inputs, 
    folder_path = NULL,
    filename = "fig1_health_inputs.csv",
    save_path = save_path,
    file_type = "figure",
    figure_number = "figure-1"
  )
  
  
  ## refinery color
  refinery_color <- "#095F66"
  # refinery_color <- "#fca311"
  # refinery_color <- "black"

  ## figure
  ct_health_panel_a <- ggplot() +
    geom_sf(
      data = bay_cluster_ct_cropped,
      mapping = aes(geometry = geometry, fill = pop_x_pm25),
      lwd = 0.0,
      color = "white",
      alpha = 1,
      show.legend = TRUE
    ) +
    scale_fill_gradient(
      high = "#79032E",
      low = "white",
      space = "Lab",
      na.value = "grey50",
      limits = c(
        min(ct_census_tract_pm25_2019_sp$pop_x_pm25),
        max(ct_census_tract_pm25_2019_sp$pop_x_pm25)
      ),
      breaks = c(0, 5000, 10000),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    facet_wrap(~cluster_title) +
    # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
    geom_sf(
      data = bay_cluster_county_crop,
      mapping = aes(geometry = geometry),
      lwd = 0.15,
      alpha = 0
    ) +
    geom_sf_text(
      data = CA_counties_noisl %>%
        filter(
          adj_county_name %in%
            c("Solano", "Contra Costa", "San Joaquin", "Sacramento")
        ),
      mapping = aes(
        geometry = geometry,
        label = adj_county_name
      ),
      size = 2,
      fontface = "bold",
      color = "#343a40"
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity"),
      mapping = aes(
        geometry = geometry,
        size = barrels_per_day / 1000
      ),
      alpha = 0.9,
      pch = 1,
      color = refinery_color,
      stroke = 1
    ) +
    scale_size_continuous(
      range = c(1, 5),
      breaks = c(15, 150, 300)
    ) +
    scale_color_manual(values = c(refinery_color)) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = expression(paste(
        "Population-weighted PM"[2.5],
        " (",
        mu,
        "g/",
        m^3,
        ")"
      )),
      color = NULL,
      shape = NULL,
      x = NULL,
      y = "Latitude"
    ) +
    coord_sf(
      xlim = disp_win_bay_cluster_coord[, "X"],
      ylim = disp_win_bay_cluster_coord[, "Y"],
      expand = FALSE
    ) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0.01, 0.2),
      legend.key.width = unit(0.9, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightgrey", color = NA),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 7),
      legend.background = element_rect(fill = NA)
    ) +
    guides(
      # fill = guide_colourbar(
      #   title.position = "top",
      #   title.hjust = 0,
      #   direction = "horizontal",
      #   ticks.colour = "black", frame.colour = "black",
      #   order = 1
      # ),
      fill = "none",
      size = "none",
      shape = "none",
      color = "none"
    ) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 1, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf # Extending the rectangle over the entire plot area
    )


  ## figure
  ct_health_panel_a_total <- ggplot() +
    geom_sf(
      data = bay_cluster_ct_cropped,
      mapping = aes(geometry = geometry, fill = total_pm25),
      lwd = 0.0,
      color = "white",
      alpha = 1,
      show.legend = TRUE
    ) +
    scale_fill_gradient(
      high = "#79032E",
      low = "white",
      space = "Lab",
      na.value = "grey50",
      limits = c(
        min(ct_census_tract_pm25_2019_sp$total_pm25),
        max(ct_census_tract_pm25_2019_sp$total_pm25)
      ),
      breaks = c(0, 1, 2),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    facet_wrap(~cluster_title) +
    # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
    geom_sf(
      data = bay_cluster_county_crop,
      mapping = aes(geometry = geometry),
      lwd = 0.15,
      alpha = 0
    ) +
    geom_sf_text(
      data = CA_counties_noisl %>%
        filter(
          adj_county_name %in%
            c("Solano", "Contra Costa", "San Joaquin", "Sacramento")
        ),
      mapping = aes(
        geometry = geometry,
        label = adj_county_name
      ),
      size = 2,
      fontface = "bold",
      color = "#343a40"
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity"),
      mapping = aes(
        geometry = geometry,
        size = barrels_per_day / 1000
      ),
      alpha = 0.9,
      pch = 1,
      color = refinery_color,
      stroke = 1
    ) +
    scale_size_continuous(
      range = c(1, 5),
      breaks = c(15, 150, 300)
    ) +
    scale_color_manual(values = c(refinery_color)) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = expression(paste("PM"[2.5], " (", mu, "g/", m^3, ")")),
      color = NULL,
      shape = NULL,
      x = "Longitude",
      y = "Latitude"
    ) +
    coord_sf(
      xlim = disp_win_bay_cluster_coord[, "X"],
      ylim = disp_win_bay_cluster_coord[, "Y"],
      expand = FALSE
    ) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      panel.background = element_rect(fill = "lightgrey", color = NA),
      legend.position = c(0.01, 0.2),
      legend.key.width = unit(0.7, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # panel.background = element_blank(),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 7),
      legend.background = element_rect(fill = NA)
    ) +
    guides(
      # fill = guide_colourbar(
      #   title.position = "top",
      #   title.hjust = 0,
      #   direction = "horizontal",
      #   ticks.colour = "black", frame.colour = "black",
      #   order = 1
      # ),
      fill = "none",
      size = "none",
      shape = "none",
      color = "none"
    ) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 1, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf # Extending the rectangle over the entire plot area
    )


  ## figure
  ct_health_panel_b <- ggplot() +
    geom_sf(
      data = kern_cluster_ct_cropped,
      mapping = aes(geometry = geometry, fill = pop_x_pm25),
      lwd = 0.0,
      color = "white",
      alpha = 1,
      show.legend = TRUE
    ) +
    scale_fill_gradient(
      high = "#79032E",
      low = "white",
      space = "Lab",
      na.value = "grey50",
      limits = c(
        min(ct_census_tract_pm25_2019_sp$pop_x_pm25),
        max(ct_census_tract_pm25_2019_sp$pop_x_pm25)
      ),
      breaks = c(0, 5000, 10000),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    facet_wrap(~cluster_title) +
    # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
    geom_sf(
      data = kern_cluster_county_crop,
      mapping = aes(geometry = geometry),
      lwd = 0.15,
      alpha = 0
    ) +
    geom_sf_text(
      data = CA_counties_noisl %>%
        filter(adj_county_name %in% c("Kern", "San Luis Obispo")),
      mapping = aes(
        geometry = geometry,
        label = adj_county_name
      ),
      size = 2,
      fontface = "bold",
      color = "#343a40"
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity"),
      mapping = aes(
        geometry = geometry,
        size = barrels_per_day / 1000
      ),
      alpha = 0.9,
      pch = 1,
      color = refinery_color,
      stroke = 1
    ) +
    scale_size_continuous(
      range = c(1, 5),
      breaks = c(15, 150, 300)
    ) +
    scale_color_manual(values = c(refinery_color)) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = expression(paste(
        "Population-weighted PM"[2.5],
        " (",
        mu,
        "g/",
        m^3,
        ")"
      )),
      color = NULL,
      shape = NULL,
      x = NULL,
      y = "Latitude"
    ) +
    coord_sf(
      xlim = disp_win_kern_cluster_coord[, "X"],
      ylim = disp_win_kern_cluster_coord[, "Y"],
      expand = FALSE
    ) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0.01, 0.2),
      legend.key.width = unit(0.9, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightgrey", color = NA),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 7),
      legend.background = element_rect(fill = NA)
    ) +
    guides(
      # fill = guide_colourbar(
      #   title.position = "top",
      #   title.hjust = 0,
      #   direction = "horizontal",
      #   ticks.colour = "black", frame.colour = "black",
      #   order = 1
      # ),
      fill = "none",
      size = "none",
      shape = "none",
      color = "none"
    ) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 1, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf # Extending the rectangle over the entire plot area
    )


  ## figure
  ct_health_panel_b_total <- ggplot() +
    geom_sf(
      data = kern_cluster_ct_cropped,
      mapping = aes(geometry = geometry, fill = total_pm25),
      lwd = 0.0,
      color = "white",
      alpha = 1,
      show.legend = TRUE
    ) +
    scale_fill_gradient(
      high = "#79032E",
      low = "white",
      space = "Lab",
      na.value = "grey50",
      limits = c(
        min(ct_census_tract_pm25_2019_sp$total_pm25),
        max(ct_census_tract_pm25_2019_sp$total_pm25)
      ),
      breaks = c(0, 1, 2),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    facet_wrap(~cluster_title) +
    # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
    geom_sf(
      data = kern_cluster_county_crop,
      mapping = aes(geometry = geometry),
      lwd = 0.15,
      alpha = 0
    ) +
    geom_sf_text(
      data = CA_counties_noisl %>%
        filter(adj_county_name %in% c("Kern", "San Luis Obispo")),
      mapping = aes(
        geometry = geometry,
        label = adj_county_name
      ),
      size = 2,
      fontface = "bold",
      color = "#343a40"
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity"),
      mapping = aes(
        geometry = geometry,
        size = barrels_per_day / 1000
      ),
      alpha = 0.9,
      pch = 1,
      color = refinery_color,
      stroke = 1
    ) +
    scale_size_continuous(
      range = c(1, 5),
      breaks = c(15, 150, 300)
    ) +
    scale_color_manual(values = c(refinery_color)) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = expression(paste("PM"[2.5], " (", mu, "g/", m^3, ")")),
      color = NULL,
      shape = NULL,
      x = "Longitude",
      y = "Latitude"
    ) +
    coord_sf(
      xlim = disp_win_kern_cluster_coord[, "X"],
      ylim = disp_win_kern_cluster_coord[, "Y"],
      expand = FALSE
    ) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0.01, 0.2),
      legend.key.width = unit(0.7, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightgrey", color = NA),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 7),
      legend.background = element_rect(fill = NA)
    ) +
    guides(
      # fill = guide_colourbar(
      #   title.position = "top",
      #   title.hjust = 0,
      #   direction = "horizontal",
      #   ticks.colour = "black", frame.colour = "black",
      #   order = 1
      # ),
      fill = "none",
      size = "none",
      shape = "none",
      color = "none"
    ) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 1, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf # Extending the rectangle over the entire plot area
    )


  ## figure
  ct_health_panel_c <- ggplot() +
    geom_sf(
      data = la_cluster_ct_cropped,
      mapping = aes(geometry = geometry, fill = pop_x_pm25),
      lwd = 0.0,
      color = "white",
      alpha = 1,
      show.legend = TRUE
    ) +
    scale_fill_gradient(
      high = "#79032E",
      low = "white",
      space = "Lab",
      na.value = "grey50",
      limits = c(
        min(ct_census_tract_pm25_2019_sp$pop_x_pm25),
        max(ct_census_tract_pm25_2019_sp$pop_x_pm25)
      ),
      breaks = c(0, 5000, 10000),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    facet_wrap(~cluster_title) +
    # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
    geom_sf(data = st_transform(la_cluster_county_crop, crs = 4326),
            mapping = aes(geometry = geometry),
            lwd = 0.15,
            alpha = 0) +
    geom_sf_text(
      data = st_transform(CA_counties_noisl, crs = 4326) %>%
        filter(adj_county_name %in% c("Los Angeles", "Orange")),
      mapping = aes(
        geometry = geometry,
        label = adj_county_name
      ),
      size = 2,
      fontface = "bold",
      color = "#343a40"
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(
          installation == "Existing capacity" &
            region == "South"
        ),
      mapping = aes(
        geometry = geometry,
        size = barrels_per_day / 1000
      ),
      alpha = 0.9,
      pch = 1,
      color = refinery_color,
      stroke = 1
    ) +
    scale_size_continuous(
      range = c(1, 5),
      breaks = c(15, 150, 300)
    ) +
    scale_color_manual(values = c(refinery_color)) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = expression(paste(
        "Population-weighted PM"[2.5],
        " (",
        mu,
        "g/",
        m^3,
        ")"
      )),
      color = NULL,
      shape = NULL,
      x = NULL,
      y = "Latitude",
    ) +
    safe_coord_sf(disp_win_la_cluster_coord) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0.01, 0.2),
      legend.key.width = unit(0.9, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightgrey", color = NA),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 7),
      legend.background = element_rect(fill = NA)
    ) +
    guides(
      # fill = guide_colourbar(
      #   title.position = "top",
      #   title.hjust = 0,
      #   direction = "horizontal",
      #   ticks.colour = "black", frame.colour = "black",
      #   order = 1
      # ),
      fill = "none",
      size = "none",
      shape = "none",
      color = "none"
    ) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 1, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf # Extending the rectangle over the entire plot area
    )


  ## figure
  ct_health_panel_c_total <- ggplot() +
    geom_sf(
      data = la_cluster_ct_cropped,
      mapping = aes(geometry = geometry, fill = total_pm25),
      lwd = 0.0,
      color = "white",
      alpha = 1,
      show.legend = TRUE
    ) +
    scale_fill_gradient(
      high = "#79032E",
      low = "white",
      space = "Lab",
      na.value = "grey50",
      limits = c(
        min(ct_census_tract_pm25_2019_sp$total_pm25),
        max(ct_census_tract_pm25_2019_sp$total_pm25)
      ),
      breaks = c(0, 1, 2),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    facet_wrap(~cluster_title) +
    # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
    geom_sf(
      data = la_cluster_county_crop,
      mapping = aes(geometry = geometry),
      lwd = 0.15,
      alpha = 0
    ) +
    geom_sf_text(
      data = CA_counties_noisl %>%
        filter(adj_county_name %in% c("Los Angeles", "Orange")),
      mapping = aes(
        geometry = geometry,
        label = adj_county_name
      ),
      size = 2,
      fontface = "bold",
      color = "#343a40"
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity"),
      mapping = aes(
        geometry = geometry,
        size = barrels_per_day / 1000
      ),
      alpha = 0.9,
      pch = 1,
      color = refinery_color,
      stroke = 1
    ) +
    scale_size_continuous(
      range = c(1, 5),
      breaks = c(15, 150, 300)
    ) +
    scale_color_manual(values = c(refinery_color)) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = expression(paste("PM"[2.5], " (", mu, "g/", m^3, ")")),
      color = NULL,
      shape = NULL,
      x = "Longitude",
      y = "Latitude"
    ) +
    safe_coord_sf(disp_win_la_cluster_coord) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0.01, 0.2),
      legend.key.width = unit(0.7, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightgrey", color = NA),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 7),
      legend.background = element_rect(fill = NA)
    ) +
    guides(
      # fill = guide_colourbar(
      #   title.position = "top",
      #   title.hjust = 0,
      #   direction = "horizontal",
      #   ticks.colour = "black", frame.colour = "black",
      #   order = 1
      # ),
      fill = "none",
      size = "none",
      shape = "none",
      color = "none"
    ) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 1, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf # Extending the rectangle over the entire plot area
    )


  ## pm2.5 legend
  ## ---------------------------------------------------------------------

  ## figure
  health_legend_fig <- ggplot() +
    geom_sf(
      data = la_cluster_ct_cropped,
      mapping = aes(geometry = geometry, fill = pop_x_pm25),
      lwd = 0.0,
      color = "white",
      alpha = 1,
      show.legend = TRUE
    ) +
    scale_fill_gradient(
      high = "#79032E",
      low = "white",
      space = "Lab",
      na.value = "grey50",
      limits = c(
        min(ct_census_tract_pm25_2019_sp$pop_x_pm25),
        max(ct_census_tract_pm25_2019_sp$pop_x_pm25)
      ),
      breaks = c(0, 5000, 10000),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = expression(paste(
        "Population-weighted PM"[2.5],
        " (",
        mu,
        "g/",
        m^3,
        ")"
      )),
      color = NULL,
      shape = NULL,
      x = "Longitude",
      y = "Latitude"
    ) +
    safe_coord_sf(disp_win_la_cluster_coord) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0.01, 0.2),
      legend.key.width = unit(1, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(8, 2, 0, 8),
      legend.background = element_rect(fill = NA)
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

  # Convert your plot to a grob object
  g <- ggplotGrob(health_legend_fig)

  # Find all legend ("guide-box") grobs
  legend_grobs <- g$grobs[which(
    sapply(g$grobs, function(x) x$name) == "guide-box"
  )]

  # See how many there are
  length(legend_grobs)

  # Draw one to inspect
  grid.newpage()
  grid.draw(legend_grobs[[1]])


  ## refinery legend
  ## ---------------------------------------------------------------------

  refinery_legend_fig <- ggplot() +
    geom_sf(
      data = la_cluster_ct_cropped,
      mapping = aes(geometry = geometry, fill = total_pm25),
      lwd = 0.0,
      color = "white",
      alpha = 1,
      show.legend = TRUE
    ) +
    scale_fill_gradient(
      high = "#79032E",
      low = "white",
      space = "Lab",
      na.value = "grey50",
      limits = c(
        min(ct_census_tract_pm25_2019_sp$total_pm25),
        max(ct_census_tract_pm25_2019_sp$total_pm25)
      ),
      breaks = c(0, 1, 2),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity"),
      mapping = aes(
        geometry = geometry,
        size = barrels_per_day / 1000
      ),
      alpha = 0.9,
      pch = 1,
      color = refinery_color,
      stroke = 1
    ) +
    scale_size_continuous(
      range = c(1, 5),
      breaks = c(15, 150, 300)
    ) +
    scale_color_manual(values = c(refinery_color)) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = expression(paste("PM"[2.5], " (", mu, "g/", m^3, ")")),
      size = "Refinery capacity\n(thous. bbls per day)",
      color = NULL,
      shape = NULL,
      x = "Longitude",
      y = "Latitude"
    ) +
    safe_coord_sf(disp_win_la_cluster_coord) +
    theme(
      #legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0, 0.2),
      legend.key.width = unit(0.7, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.key = element_blank(),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightgrey", color = NA),
      legend.background = element_rect(fill = NA)
    ) +
    guides(
      fill = "none",
      size = guide_legend(
        title.position = "left",
        title.hjust = 0,
        direction = "horizontal",
        override.aes = list(fill = NA)
      ),
      shape = "none",
      color = "none"
    )

  # Convert your plot to a grob object
  gr <- ggplotGrob(refinery_legend_fig)

  # Find all legend ("guide-box") grobs
  legend_grobs_r <- gr$grobs[which(
    sapply(gr$grobs, function(x) x$name) == "guide-box"
  )]

  # See how many there are
  length(legend_grobs_r)

  # Draw one to inspect
  grid.newpage()
  grid.draw(legend_grobs_r[[1]])


  ## plot health together
  ## --------------------------------------------------------------------------

  # ## plot together
  # ct_health_panel_a_legend <- ggdraw(ct_health_panel_a, clip = "on") +
  #   draw_plot(refinery_legend, x = 0.25, y = 0.05, width = 0.025, height = 0.025)

  xaxis_lab <- ggdraw() + draw_label("Longitude", size = 10, angle = 0)

  ## plot all four together

  fig1_health <- plot_grid(
    ct_health_panel_a_total,
    ct_health_panel_c_total,
    align = "vh",
    labels = c("A", "B"),
    nrow = 2,
    ncol = 1,
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    rel_widths = c(1, 1),
    rel_heights = c(1, 1)
  )

  fig1_health

  # fig1_health_legend <- plot_grid(
  #   # refinery_legend,
  #   # health_legend,
  #   align = "vh",
  #   # labels = c("A", "B"),
  #   nrow = 1,
  #   ncol = 2,
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   hjust = -1,
  #   rel_widths = c(1, 1),
  #   rel_heights = c(1, 1)
  # )

  # fig1_health_all <- plot_grid(
  #   fig1_health,
  #   # fig1_health_legend,
  #   ncol = 1,
  #   hjust = -1,
  #   rel_widths = c(1, 1),
  #   rel_heights = c(1, 0.1))


  ## ---------------------------------------------------------------------------
  ## DAC insets
  ## ---------------------------------------------------------------------------

  ## st_union of no island counties
  # ca_union <- st_union(CA_counties_noisl)
  ca_union_cart <- st_union(county_boundaries_from_census_tracts)

  ## northern CA
  fig1_north_inset <- ggplot() +
    geom_sf(
      data = ca_union_cart,
      mapping = aes(),
      fill = "transparent",
      linewidth = 0.4,
      show.legend = FALSE
    ) +
    geom_sf(
      data = dac_areas,
      mapping = aes(geometry = geometry),
      fill = "#C0C0C0",
      lwd = 0,
      color = "#C0C0C0",
      show.legend = TRUE
    ) +
    # geom_sf(data = disp_win2_wgs84, shape = 0, size = 35, color = "red", stroke = 2) +# Draw box around zoomed region
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity"),
      mapping = aes(
        geometry = geometry
      ),
      alpha = 0.5,
      pch = 21,
      color = refinery_color,
      fill = refinery_color,
      size = 1,
      stroke = 0.5
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity"),
      mapping = aes(
        geometry = geometry
      ),
      alpha = 1,
      pch = 21,
      color = refinery_color,
      fill = NA,
      size = 1,
      stroke = 0.5
    ) +
    scale_color_manual(values = c(refinery_color)) +
    annotate(
      geom = "rect",
      xmin = bay_cluster_zoom_coord_df[1, 1],
      xmax = bay_cluster_zoom_coord_df[2, 1],
      ymin = bay_cluster_zoom_coord_df[1, 2],
      ymax = bay_cluster_zoom_coord_df[2, 2],
      color = "black",
      linewidth = 0.5,
      fill = NA
    ) +
    theme_void() +
    # coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
    #          datum = ca_crs, expand = FALSE) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0.15, 0.15),
      legend.title = element_text(size = 7),
      plot.title = element_text(hjust = 0, face = "bold"),
      plot.title.position = "plot"
    ) +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0,
        direction = "horizontal"
      )
    )


  ## map inset, CA with box around zoom area
  fig1_south_inset <- ggplot() +
    geom_sf(
      data = ca_union_cart,
      mapping = aes(),
      fill = "transparent",
      linewidth = 0.4,
      show.legend = FALSE
    ) +
    geom_sf(
      data = dac_areas,
      mapping = aes(geometry = geometry),
      fill = "#C0C0C0",
      lwd = 0,
      color = "#C0C0C0",
      show.legend = TRUE
    ) +
    # geom_sf(data = disp_win2_wgs84, shape = 0, size = 35, color = "red", stroke = 2) +# Draw box around zoomed region
    annotate(
      geom = "rect",
      xmin = la_cluster_zoom_coord_df[1, 1],
      xmax = la_cluster_zoom_coord_df[2, 1],
      ymin = la_cluster_zoom_coord_df[1, 2],
      ymax = la_cluster_zoom_coord_df[2, 2],
      color = "black",
      linewidth = 0.5,
      fill = NA
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity"),
      mapping = aes(
        geometry = geometry
      ),
      alpha = 0.5,
      pch = 21,
      color = refinery_color,
      fill = refinery_color,
      size = 1,
      stroke = 0.5
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity"),
      mapping = aes(
        geometry = geometry
      ),
      alpha = 1,
      pch = 21,
      color = refinery_color,
      fill = NA,
      size = 1,
      stroke = 0.5
    ) +
    scale_color_manual(values = c(refinery_color)) +
    theme_void() +
    # coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
    #          datum = ca_crs, expand = FALSE) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0.15, 0.15),
      legend.title = element_text(size = 7),
      plot.title = element_text(hjust = 0, face = "bold"),
      plot.title.position = "plot"
    ) +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0,
        direction = "horizontal"
      )
    )


  # ## make map
  # fig1_map <- ggplot() +
  #   geom_sf(data = ca_union, mapping = aes(), fill = "transparent", lwd = 0.4, show.legend = FALSE) +
  #   geom_sf(data = dac_areas, mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0, color = "#C0C0C0", show.legend = TRUE) +
  #   geom_sf(
  #     data = refin_capacity |>
  #       filter(installation == "Existing capacity"),
  #     mapping = aes(
  #       geometry = geometry,
  #       size = barrels_per_day / 1000
  #     ),
  #     alpha = 0.9,
  #     pch = 1,
  #     color = refinery_color,
  #     lwd = 0.1
  #   ) +
  #   geom_sf(data = CA_counties_noisl, mapping = aes(geometry = geometry), lwd = 0.05, fill = NA) +
  #   # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
  #   # geom_sf_text(data = county_19, mapping = aes(geometry = geometry, label = adj_county_name), size = 2, fontface = "bold", color = "black") +
  #   # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
  #   labs(
  #     title = NULL,
  #     color = NULL,
  #     size = "Refinery capacity\n(thous. bbls per day)",
  #     x = "Longitude",
  #     y = "Latitude"
  #   ) +
  #   # scale_color_manual(values = c('#191970', '#e2711d')) +
  #   scale_size_continuous(range = c(1, 5)) +
  #   coord_sf(xlim = disp_win2_coord[, "X"], ylim = disp_win2_coord[, "Y"], expand = FALSE) +
  #   # theme_void() +
  #   theme(
  #     legend.position = "none",
  #     plot.margin = margin(6, 2, 0, 8),
  #     legend.title = element_text(size = 12),
  #     plot.title = element_text(hjust = -0.1, face = "bold", size = 7),
  #     panel.grid.major = element_blank(),
  #     panel.grid.minor = element_blank(),
  #     panel.background = element_blank(),
  #     axis.title = element_text(size = 5),
  #     axis.text = element_text(size = 5)
  #   ) +
  #   annotation_custom(
  #     grob = rectGrob(gp = gpar(lwd = 2, col = "black", fill = NA)), # lwd for line width, col for color
  #     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf # Extending the rectangle over the entire plot area
  #   )
  #
  ## DAC legend
  ## ------------------------

  ## figure
  fig1_dac_legend <- ggplot() +
    # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
    geom_sf(
      data = california,
      mapping = aes(),
      fill = "transparent",
      lwd = 0.4,
      show.legend = FALSE
    ) +
    geom_sf(
      data = dac_areas,
      mapping = aes(geometry = geometry, fill = ct_type),
      lwd = 0,
      show.legend = TRUE
    ) +
    labs(
      title = "Oil production",
      fill = NULL,
      x = NULL,
      y = NULL
    ) +
    scale_fill_manual(values = c("DAC" = "#C0C0C0")) +
    theme(legend.text = element_text(size = 7)) +
    guides(fill = guide_legend(override.aes = list(size = 3)))

  ## extract legend
  dac_legend <- get_legend(
    fig1_dac_legend
  )


  ## labor, 2020, total_comp_usd9_l
  ## ------------------------------------------------------------------------

  ## select columns, filter for main text scenario
  labor_mpa_df <- copy(annual_direct_labor)

  labor_mpa_df <- labor_mpa_df |>
    filter(
      demand_scenario == "BAU",
      refining_scenario == "historic production",
      oil_price_scenario == "reference case",
      product_scenario == "2020 prices",
      year == 2020
    ) |>
    select(demand_scenario:year, total_comp_usd19_h, total_comp_usd19_l) |>
    pivot_longer(
      total_comp_usd19_h:total_comp_usd19_l,
      names_to = "re_emp_scen",
      values_to = "value"
    )

  ## merge
  census_tract_labor_2020 <- merge(
    labor_mpa_df,
    pop_2020[, .(census_tract, pop)],
    by = c("census_tract"),
    all.x = T
  )

  setDT(census_tract_labor_2020)

  ## weight by total population
  census_tract_labor_2020[, pop_x_comp19 := value * pop]
  census_tract_labor_2020[, pop_x_comp19 := fifelse(is.na(pop_x_comp19), 0, pop_x_comp19)]
  census_tract_labor_2020[, value := fifelse(is.na(value), 0, value)]

  ## merge counties to census tracts
  ## -----------------------------------------------------------------

  # ## join with spatial data 2019
  # census_tract_labor_2019_sp <- raw_ct_2019 |>
  #   rename(census_tract = GEOID) |>
  #   left_join(census_tract_labor_2020)

  # Create the expanded data
  census_tracts_l_expanded <- expand_grid(
    census_tracts,
    re_emp_scen = c("total_comp_usd19_h", "total_comp_usd19_l")
  )
  
  ## merge with spatial data
  census_tract_labor_2020_sp <- census_tracts_l_expanded |>
    left_join(census_tracts |> select(census_tract, geometry)) |>
    left_join(census_tract_labor_2020) |>
    mutate(value = ifelse(is.na(value), 0, value),
           pop_x_comp19 = ifelse(is.na(value), 0, pop_x_comp19)) |>
    st_as_sf()
    

  census_tract_labor_2020_sp_w_remp <- census_tract_labor_2020_sp |>
    filter(re_emp_scen == "total_comp_usd19_l")

  census_tract_labor_2020_sp_wo_remp <- census_tract_labor_2020_sp |>
    filter(re_emp_scen != "total_comp_usd19_l")

  ## crop
  ## -----------------------------------------------------------------

  ## bay area
  bay_cluster_ct_cropped_labor <- st_crop(
    census_tract_labor_2020_sp,
    xmin = bay_cluster_zoom_coord_df$X[1],
    xmax = bay_cluster_zoom_coord_df$X[2],
    ymin = bay_cluster_zoom_coord_df$Y[1],
    ymax = bay_cluster_zoom_coord_df$Y[2]
  )

  bay_cluster_ct_cropped_labor <- bay_cluster_ct_cropped_labor |>
    mutate(cluster_title = "North cluster: Bay area")

  ## central
  kern_cluster_ct_cropped_labor <- st_crop(
    census_tract_labor_2020_sp,
    xmin = kern_cluster_zoom_coord_df$X[1],
    xmax = kern_cluster_zoom_coord_df$X[2],
    ymin = kern_cluster_zoom_coord_df$Y[1],
    ymax = kern_cluster_zoom_coord_df$Y[2]
  )

  kern_cluster_ct_cropped_labor <- kern_cluster_ct_cropped_labor |>
    mutate(cluster_title = "North cluster: Central")

  ## la area
  la_cluster_ct_cropped_labor <- st_crop(
    census_tract_labor_2020_sp,
    xmin = la_cluster_zoom_coord_df$X[1],
    xmax = la_cluster_zoom_coord_df$X[2],
    ymin = la_cluster_zoom_coord_df$Y[1],
    ymax = la_cluster_zoom_coord_df$Y[2]
  )

  la_cluster_ct_cropped_labor <- la_cluster_ct_cropped_labor |>
    mutate(cluster_title = "South cluster")
  
  ## save labor inputs
  ## -------------------------------------------------
  
  fig1_labor_inputs <- rbind(bay_cluster_ct_cropped_labor |> 
                               filter(re_emp_scen == "total_comp_usd19_l") |>
                               st_drop_geometry(),
                             la_cluster_ct_cropped_labor |> 
                               filter(re_emp_scen == "total_comp_usd19_l") |>
                               st_drop_geometry()
                             )
  
  ## save figure inputs
  simple_fwrite_repo(
    data = fig1_labor_inputs, 
    folder_path = NULL,
    filename = "fig1_labor_inputs.csv",
    save_path = save_path,
    file_type = "figure",
    figure_number = "figure-1"
  )

  ## plot labor
  ## --------------------------------------------------------------

  ## figure
  ct_labor_bay_area_wt_w_reemp <- ggplot() +
    geom_sf(
      data = bay_cluster_ct_cropped_labor |>
        filter(re_emp_scen == "total_comp_usd19_l"),
      mapping = aes(geometry = geometry, fill = pop_x_comp19 / 1e9),
      lwd = 0.0,
      color = "white",
      alpha = 1,
      show.legend = TRUE
    ) +
    scale_fill_gradient(
      high = "#003566",
      low = "white",
      space = "Lab",
      na.value = "red",
      limits = c(
        min(census_tract_labor_2020_sp_w_remp$pop_x_comp19 / 1e9),
        max(census_tract_labor_2020_sp_w_remp$pop_x_comp19 / 1e9)
      ),
      breaks = c(0, 1, 2),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    facet_wrap(~cluster_title) +
    # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
    geom_sf(
      data = bay_cluster_county_crop,
      mapping = aes(geometry = geometry),
      lwd = 0.15,
      alpha = 0
    ) +
    geom_sf_text(
      data = CA_counties_noisl %>%
        filter(
          adj_county_name %in%
            c("Solano", "Contra Costa", "San Joaquin", "Sacramento")
        ),
      mapping = aes(
        geometry = geometry,
        label = adj_county_name
      ),
      size = 2,
      fontface = "bold",
      color = "#343a40"
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity", cluster == "Bay Area"),
      mapping = aes(
        geometry = geometry,
        size = barrels_per_day / 1000
      ),
      alpha = 0.9,
      pch = 1,
      color = refinery_color,
      stroke = 1
    ) +
    scale_size_continuous(
      range = c(1, 5),
      breaks = c(15, 150, 300)
    ) +
    scale_color_manual(values = c(refinery_color)) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = "Compensation (USD billion)",
      color = NULL,
      shape = NULL,
      x = NULL,
      y = "Latitude"
    ) +
    coord_sf(
      xlim = disp_win_bay_cluster_coord[, "X"],
      ylim = disp_win_bay_cluster_coord[, "Y"],
      expand = FALSE
    ) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position.inside = c(0.01, 0.2),
      legend.key.width = unit(0.9, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightgrey", color = NA),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 7),
      legend.background = element_rect(fill = NA)
    ) +
    guides(
      # fill = guide_colourbar(
      #   title.position = "top",
      #   title.hjust = 0,
      #   direction = "horizontal",
      #   ticks.colour = "black", frame.colour = "black",
      #   order = 1
      # ),
      fill = "none",
      size = "none",
      shape = "none",
      color = "none"
    ) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 1, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf # Extending the rectangle over the entire plot area
    )


  ## figure
  ct_labor_bay_area_w_reemp <- ggplot() +
    geom_sf(
      data = bay_cluster_ct_cropped_labor |>
        filter(re_emp_scen == "total_comp_usd19_l"),
      mapping = aes(geometry = geometry, fill = value / 1e6),
      lwd = 0.0,
      color = "white",
      alpha = 1,
      show.legend = TRUE
    ) +
    scale_fill_gradient(
      high = "#003566",
      low = "white",
      space = "Lab",
      na.value = "red",
      limits = c(
        min(census_tract_labor_2020_sp_w_remp$value / 1e6),
        max(census_tract_labor_2020_sp_w_remp$value / 1e6)
      ),
      breaks = c(0, 1, 2, 3, 4),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    facet_wrap(~cluster_title) +
    # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
    geom_sf(
      data = bay_cluster_county_crop,
      mapping = aes(geometry = geometry),
      lwd = 0.15,
      alpha = 0
    ) +
    geom_sf_text(
      data = CA_counties_noisl %>%
        filter(
          adj_county_name %in%
            c("Solano", "Contra Costa", "San Joaquin", "Sacramento")
        ),
      mapping = aes(
        geometry = geometry,
        label = adj_county_name
      ),
      size = 2,
      fontface = "bold",
      color = "#343a40"
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity", cluster == "Bay Area"),
      mapping = aes(
        geometry = geometry,
        size = barrels_per_day / 1000
      ),
      alpha = 0.9,
      pch = 1,
      color = refinery_color,
      stroke = 1
    ) +
    scale_size_continuous(
      range = c(1, 5),
      breaks = c(15, 150, 300)
    ) +
    scale_color_manual(values = c(refinery_color)) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = "Compensation (USD million)",
      color = NULL,
      shape = NULL,
      x = "Longitude",
      y = "Latitude"
    ) +
    coord_sf(
      xlim = disp_win_bay_cluster_coord[, "X"],
      ylim = disp_win_bay_cluster_coord[, "Y"],
      expand = FALSE
    ) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position.inside = c(0.01, 0.2),
      legend.key.width = unit(0.9, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightgrey", color = NA),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 7),
      legend.background = element_rect(fill = NA)
    ) +
    guides(
      # fill = guide_colourbar(
      #   title.position = "top",
      #   title.hjust = 0,
      #   direction = "horizontal",
      #   ticks.colour = "black", frame.colour = "black",
      #   order = 1
      # ),
      fill = "none",
      size = "none",
      shape = "none",
      color = "none"
    ) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 1, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf # Extending the rectangle over the entire plot area
    )


  ## figure
  ct_labor_kern_wt <- ggplot() +
    geom_sf(
      data = kern_cluster_ct_cropped_labor |>
        filter(re_emp_scen == "total_comp_usd19_l"),
      mapping = aes(geometry = geometry, fill = pop_x_comp19 / 1e9),
      lwd = 0.0,
      color = "white",
      alpha = 1,
      show.legend = TRUE
    ) +
    scale_fill_gradient(
      high = "#003566",
      low = "white",
      space = "Lab",
      na.value = "red",
      limits = c(
        min(census_tract_labor_2020_sp_w_remp$pop_x_comp19 / 1e9),
        max(census_tract_labor_2020_sp_w_remp$pop_x_comp19 / 1e9)
      ),
      breaks = c(0, 1, 2),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    facet_wrap(~cluster_title) +
    # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
    geom_sf(
      data = kern_cluster_county_crop,
      mapping = aes(geometry = geometry),
      lwd = 0.15,
      alpha = 0
    ) +
    geom_sf_text(
      data = CA_counties_noisl %>%
        filter(adj_county_name %in% c("Kern", "San Luis Obispo")),
      mapping = aes(
        geometry = geometry,
        label = adj_county_name
      ),
      size = 2,
      fontface = "bold",
      color = "#343a40"
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity", cluster == "Bay Area"),
      mapping = aes(
        geometry = geometry,
        size = barrels_per_day / 1000
      ),
      alpha = 0.9,
      pch = 1,
      color = refinery_color,
      stroke = 1
    ) +
    scale_size_continuous(
      range = c(1, 5),
      breaks = c(15, 150, 300)
    ) +
    scale_color_manual(values = c(refinery_color)) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = "Compensation (USD billion)",
      color = NULL,
      shape = NULL,
      x = NULL,
      y = "Latitude"
    ) +
    coord_sf(
      xlim = disp_win_kern_cluster_coord[, "X"],
      ylim = disp_win_kern_cluster_coord[, "Y"],
      expand = FALSE
    ) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position.inside = c(0.01, 0.2),
      legend.key.width = unit(0.9, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightgrey", color = NA),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 7),
      legend.background = element_rect(fill = NA)
    ) +
    guides(
      # fill = guide_colourbar(
      #   title.position = "top",
      #   title.hjust = 0,
      #   direction = "horizontal",
      #   ticks.colour = "black", frame.colour = "black",
      #   order = 1
      # ),
      fill = "none",
      size = "none",
      shape = "none",
      color = "none"
    ) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 1, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf # Extending the rectangle over the entire plot area
    )


  ## figure
  ct_labor_kern_total <- ggplot() +
    geom_sf(
      data = kern_cluster_ct_cropped_labor |>
        filter(re_emp_scen == "total_comp_usd19_l"),
      mapping = aes(geometry = geometry, fill = value / 1e6),
      lwd = 0.0,
      color = "white",
      alpha = 1,
      show.legend = TRUE
    ) +
    scale_fill_gradient(
      high = "#003566",
      low = "white",
      space = "Lab",
      na.value = "red",
      limits = c(
        min(census_tract_labor_2020_sp_w_remp$value / 1e6),
        max(census_tract_labor_2020_sp_w_remp$value / 1e6)
      ),
      breaks = c(0, 1, 2, 3, 4),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    facet_wrap(~cluster_title) +
    # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
    geom_sf(
      data = kern_cluster_county_crop,
      mapping = aes(geometry = geometry),
      lwd = 0.15,
      alpha = 0
    ) +
    geom_sf_text(
      data = CA_counties_noisl %>%
        filter(adj_county_name %in% c("Kern", "San Luis Obispo")),
      mapping = aes(
        geometry = geometry,
        label = adj_county_name
      ),
      size = 2,
      fontface = "bold",
      color = "#343a40"
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity"),
      mapping = aes(
        geometry = geometry,
        size = barrels_per_day / 1000
      ),
      alpha = 0.9,
      pch = 1,
      color = refinery_color,
      stroke = 1
    ) +
    scale_size_continuous(
      range = c(1, 5),
      breaks = c(15, 150, 300)
    ) +
    scale_color_manual(values = c(refinery_color)) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = "Compensation (USD million)",
      color = NULL,
      shape = NULL,
      x = NULL,
      y = "Latitude"
    ) +
    coord_sf(
      xlim = disp_win_kern_cluster_coord[, "X"],
      ylim = disp_win_kern_cluster_coord[, "Y"],
      expand = FALSE
    ) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position.inside = c(0.01, 0.2),
      legend.key.width = unit(0.9, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightgrey", color = NA),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 7),
      legend.background = element_rect(fill = NA)
    ) +
    guides(
      # fill = guide_colourbar(
      #   title.position = "top",
      #   title.hjust = 0,
      #   direction = "horizontal",
      #   ticks.colour = "black", frame.colour = "black",
      #   order = 1
      # ),
      fill = "none",
      size = "none",
      shape = "none",
      color = "none"
    ) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 1, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf # Extending the rectangle over the entire plot area
    )


  ## los angeles
  ct_labor_la_wt <- ggplot() +
    geom_sf(
      data = la_cluster_ct_cropped_labor,
      mapping = aes(geometry = geometry, fill = pop_x_comp19 / 1e9),
      lwd = 0.0,
      color = "white",
      alpha = 1,
      show.legend = TRUE
    ) +
    scale_fill_gradient(
      high = "#003566",
      low = "white",
      space = "Lab",
      na.value = "red",
      limits = c(
        min(census_tract_labor_2020_sp_w_remp$pop_x_comp19 / 1e9),
        max(census_tract_labor_2020_sp_w_remp$pop_x_comp19 / 1e9)
      ),
      breaks = c(0, 1, 2),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    facet_wrap(~cluster_title) +
    # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
    geom_sf(
      data = la_cluster_county_crop,
      mapping = aes(geometry = geometry),
      lwd = 0.15,
      alpha = 0
    ) +
    geom_sf_text(
      data = CA_counties_noisl %>%
        filter(adj_county_name %in% c("Los Angeles", "Orange")),
      mapping = aes(
        geometry = geometry,
        label = adj_county_name
      ),
      size = 2,
      fontface = "bold",
      color = "#343a40"
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(
          installation == "Existing capacity" &
            region == "South"
        ),
      mapping = aes(
        geometry = geometry,
        size = barrels_per_day / 1000
      ),
      alpha = 0.9,
      pch = 1,
      color = refinery_color,
      stroke = 1
    ) +
    scale_size_continuous(
      range = c(1, 5),
      breaks = c(15, 150, 300)
    ) +
    scale_color_manual(values = c(refinery_color)) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = "Compensation (USD billion)",
      color = NULL,
      shape = NULL,
      x = NULL,
      y = NULL
    ) +
    safe_coord_sf(disp_win_la_cluster_coord) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position.inside = c(0.01, 0.2),
      legend.key.width = unit(0.9, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightgrey", color = NA),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 7),
      legend.background = element_rect(fill = NA)
    ) +
    guides(
      # fill = guide_colourbar(
      #   title.position = "top",
      #   title.hjust = 0,
      #   direction = "horizontal",
      #   ticks.colour = "black", frame.colour = "black",
      #   order = 1
      # ),
      fill = "none",
      size = "none",
      shape = "none",
      color = "none"
    ) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 1, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf # Extending the rectangle over the entire plot area
    )


  ## figure
  ct_labor_la_total <- ggplot() +
    geom_sf(
      data = la_cluster_ct_cropped_labor |>
        filter(re_emp_scen == "total_comp_usd19_l"),
      mapping = aes(geometry = geometry, fill = value / 1e6),
      lwd = 0.0,
      color = "white",
      alpha = 1,
      show.legend = TRUE
    ) +
    scale_fill_gradient(
      high = "#003566",
      low = "white",
      space = "Lab",
      na.value = "red",
      limits = c(
        min(census_tract_labor_2020_sp_w_remp$value / 1e6),
        max(census_tract_labor_2020_sp_w_remp$value / 1e6)
      ),
      breaks = c(0, 1, 2, 3, 4),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    facet_wrap(~cluster_title) +
    # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
    geom_sf(
      data = la_cluster_county_crop,
      mapping = aes(geometry = geometry),
      lwd = 0.15,
      alpha = 0
    ) +
    geom_sf_text(
      data = CA_counties_noisl %>%
        filter(adj_county_name %in% c("Los Angeles", "Orange")),
      mapping = aes(
        geometry = geometry,
        label = adj_county_name
      ),
      size = 2,
      fontface = "bold",
      color = "#343a40"
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity"),
      mapping = aes(
        geometry = geometry,
        size = barrels_per_day / 1000
      ),
      alpha = 0.9,
      pch = 1,
      color = refinery_color,
      stroke = 1
    ) +
    scale_size_continuous(
      range = c(1, 5),
      breaks = c(15, 150, 300)
    ) +
    scale_color_manual(values = c(refinery_color)) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = "Compensation (USD million)",
      color = NULL,
      shape = NULL,
      x = "Longitude",
      y = "Latitude"
    ) +
    safe_coord_sf(disp_win_la_cluster_coord) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position.inside = c(0.01, 0.2),
      legend.key.width = unit(0.9, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightgrey", color = NA),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 7),
      legend.background = element_rect(fill = NA)
    ) +
    guides(
      # fill = guide_colourbar(
      #   title.position = "top",
      #   title.hjust = 0,
      #   direction = "horizontal",
      #   ticks.colour = "black", frame.colour = "black",
      #   order = 1
      # ),
      fill = "none",
      size = "none",
      shape = "none",
      color = "none"
    ) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 1, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf # Extending the rectangle over the entire plot area
    )


  ## labor legend
  ## ---------------------------------------------------------------------

  ## figure
  labor_legend_fig <- ggplot() +
    geom_sf(
      data = la_cluster_ct_cropped_labor |>
        filter(re_emp_scen == "total_comp_usd19_l"),
      mapping = aes(geometry = geometry, fill = value / 1e6),
      lwd = 0.0,
      color = "white",
      alpha = 1,
      show.legend = TRUE
    ) +
    scale_fill_gradient(
      high = "#003566",
      low = "white",
      space = "Lab",
      na.value = "red",
      limits = c(
        min(census_tract_labor_2020_sp_w_remp$value / 1e6),
        max(census_tract_labor_2020_sp_w_remp$value / 1e6)
      ),
      breaks = c(0, 1, 2, 3, 4),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    facet_wrap(~cluster_title) +
    # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
    geom_sf(
      data = bay_cluster_county_crop,
      mapping = aes(geometry = geometry),
      lwd = 0.15,
      alpha = 0
    ) +
    geom_sf_text(
      data = CA_counties_noisl %>%
        filter(
          adj_county_name %in%
            c("Solano", "Contra Costa", "San Joaquin", "Sacramento")
        ),
      mapping = aes(
        geometry = geometry,
        label = adj_county_name
      ),
      size = 2,
      fontface = "bold",
      color = "#343a40"
    ) +
    geom_sf(
      data = refin_capacity |>
        filter(installation == "Existing capacity", cluster == "Bay Area"),
      mapping = aes(
        geometry = geometry,
        size = barrels_per_day / 1000
      ),
      alpha = 0.9,
      pch = 1,
      color = refinery_color,
      stroke = 1
    ) +
    scale_size_continuous(
      range = c(1, 5),
      breaks = c(15, 150, 300),
      guide = "none"
    ) +
    scale_color_manual(values = c(refinery_color), guide = "none") +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = "Compensation (USD million)",
      color = NULL,
      shape = NULL,
      x = NULL,
      y = "Latitude"
    ) +
    safe_coord_sf(disp_win_la_cluster_coord) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0.01, 0.2),
      legend.key.width = unit(1, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(8, 2, 0, 8),
      legend.background = element_rect(fill = NA)
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

  # Convert your plot to a grob object
  gl <- ggplotGrob(labor_legend_fig)

  # Find all legend ("guide-box") grobs
  legend_grobs_l <- gl$grobs[which(
    sapply(gl$grobs, function(x) x$name) == "guide-box"
  )]

  # See how many there are
  length(legend_grobs_l)

  # Draw one to inspect
  grid.newpage()
  grid.draw(legend_grobs_l[[1]])


  ## plot labor together
  ## --------------------------------------------------------------------------

  ## plot all four together

  fig1_labor <- plot_grid(
    ct_labor_bay_area_w_reemp,
    ct_labor_la_total,
    align = "vh",
    labels = c("C", "D"),
    nrow = 2,
    ncol = 1,
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    rel_widths = c(1, 1),
    rel_heights = c(1, 1)
  )

  fig1_labor

  # fig1_labor_legend <- plot_grid(
  #   # refinery_legend,
  #   # labor_legend,
  #   align = "vh",
  #   # labels = c("A", "B"),
  #   nrow = 1,
  #   ncol = 2,
  #   # # labels = 'AUTO',
  #   # label_size = 10,
  #   hjust = -1,
  #   rel_widths = c(1, 1),
  #   rel_heights = c(1, 1)
  # )

  # fig1_labor_all <- plot_grid(
  #   fig1_labor,
  #   # fig1_labor_legend,
  #   ncol = 1,
  #   hjust = -1,
  #   rel_widths = c(1, 1),
  #   rel_heights = c(1, 0.1))


  ## all four
  fig1_all <- plot_grid(
    fig1_health,
    fig1_labor,
    ncol = 2,
    hjust = -1,
    rel_widths = c(1, 1),
    rel_heights = c(1, 1)
  )

  # Return all plot objects and plot parameters for saving in _targets.R
  return(list(
    plots = list(
      fig1a_pop_weighted = ct_health_panel_a,
      fig1a_not_weighted = ct_health_panel_a_total,
      fig1b_pop_weighted = ct_health_panel_b,
      fig1b_not_weighted = ct_health_panel_b_total,
      fig1c_pop_weighted = ct_health_panel_c,
      fig1c_not_weighted = ct_health_panel_c_total,
      fig1_health = fig1_health,
      fig1_north_inset = fig1_north_inset,
      fig1_south_inset = fig1_south_inset,
      fig1_labor_pop_wt_bay_area = ct_labor_bay_area_wt_w_reemp,
      fig1_labor_total_bay_area = ct_labor_bay_area_w_reemp,
      fig1_labor_pop_wt_kern = ct_labor_kern_wt,
      fig1_labor_total_kern = ct_labor_kern_total,
      fig1_labor_pop_wt_la = ct_labor_la_wt,
      fig1_labor_total_la = ct_labor_la_total,
      fig1_labor = fig1_labor,
      fig1_total_comp_all = fig1_all
    ),
    legends = list(
      fig1_dac_legend = legend_grobs[[1]],
      fig1_health_legend = legend_grobs[[1]],
      fig1_labor_legend = legend_grobs_l[[1]],
      fig1_refining_legend = legend_grobs_r[[1]]
    ),
    dimensions = list(
      fig1a_pop_weighted = list(width = 60, height = 80, units = "mm"),
      fig1a_not_weighted = list(width = 60, height = 80, units = "mm"),
      fig1b_pop_weighted = list(width = 60, height = 80, units = "mm"),
      fig1b_not_weighted = list(width = 60, height = 80, units = "mm"),
      fig1c_pop_weighted = list(width = 60, height = 80, units = "mm"),
      fig1c_not_weighted = list(width = 60, height = 80, units = "mm"),
      fig1_health = list(width = 240, height = 160, units = "mm"),
      fig1_north_inset = list(width = 45, height = 35, units = "mm"),
      fig1_south_inset = list(width = 45, height = 35, units = "mm"),
      fig1_labor_pop_wt_bay_area = list(width = 80, height = 80, units = "mm"),
      fig1_labor_total_bay_area = list(width = 80, height = 80, units = "mm"),
      fig1_labor_pop_wt_kern = list(width = 80, height = 80, units = "mm"),
      fig1_labor_total_kern = list(width = 80, height = 80, units = "mm"),
      fig1_labor_pop_wt_la = list(width = 80, height = 80, units = "mm"),
      fig1_labor_total_la = list(width = 80, height = 80, units = "mm"),
      fig1_labor = list(width = 240, height = 160, units = "mm"),
      fig1_total_comp_all = list(width = 240, height = 160, units = "mm")
    )
  ))
}
