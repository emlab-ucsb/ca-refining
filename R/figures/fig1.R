# ## figure 1
# ## september 9, 2024

create_figure_1 <- function(main_path,
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
                            cpi2019) {
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
    select(site_id, barrels_per_day = installation_capacity_bpd, installation_year)

  ## future renewables capacity (all)
  fut_cap <- rbind(aa_cap, renewable_cap)

  ## capacity
  refin_capacity <- dt_refcap %>%
    mutate(site_id = as.character(site_id)) %>%
    select(site_id, barrels_per_day) %>%
    filter(!site_id %in% man_capacity$site_id) %>%
    full_join(man_capacity %>% mutate(site_id = as.character(site_id))) %>%
    full_join(fut_cap) %>%
    mutate(installation_year = ifelse(is.na(installation_year), "pre 2020", as.character(installation_year)))


  ## join with locations
  refin_capacity <- refin_locs %>%
    left_join(refin_capacity) %>%
    mutate(installation = ifelse(installation_year == "pre 2020", "Existing capacity", "Future capacity"))
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

  ## crop area
  disp_win2_wgs84 <- st_sfc(st_point(c(-122.5, 33)), st_point(c(-117, 39)),
    crs = 4326
  )

  disp_win2_trans <- st_transform(disp_win2_wgs84, crs = ca_crs)

  disp_win2_coord <- st_coordinates(disp_win2_trans)

  disp_win_df <- as.data.frame(disp_win2_coord)

  ## limits for zoom
  xlim <- c(disp_win_df$X[1], disp_win_df$X[2]) # Set limits for zoom panel
  ylim <- c(disp_win_df$Y[1], disp_win_df$Y[2])

  ## st_union of no island counties
  ca_union <- st_union(CA_counties_noisl)

  ## refinery color
  refinery_color <- "#095F66"


  ## map inset, CA with box around zoom area
  fig1_inset <- ggplot() +
    geom_sf(data = ca_union, mapping = aes(), fill = "transparent", linewidth = 0.4, show.legend = FALSE) +
    geom_sf(data = dac_areas, mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0, color = "#C0C0C0", show.legend = TRUE) +
    # geom_sf(data = disp_win2_wgs84, shape = 0, size = 35, color = "red", stroke = 2) +# Draw box around zoomed region
    annotate(
      geom = "rect",
      xmin = xlim[1],
      xmax = xlim[2],
      ymin = ylim[1],
      ymax = ylim[2],
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
    guides(fill = guide_colourbar(
      title.position = "top",
      title.hjust = 0,
      direction = "horizontal"
    ))
  ## make map
  fig1_map <- ggplot() +
    geom_sf(data = ca_union, mapping = aes(), fill = "transparent", lwd = 0.4, show.legend = FALSE) +
    geom_sf(data = dac_areas, mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0, color = "#C0C0C0", show.legend = TRUE) +
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
      lwd = 0.1
    ) +
    geom_sf(data = CA_counties_noisl, mapping = aes(geometry = geometry), lwd = 0.05, fill = NA) +
    # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
    # geom_sf_text(data = county_19, mapping = aes(geometry = geometry, label = adj_county_name), size = 2, fontface = "bold", color = "black") +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      title = NULL,
      color = NULL,
      size = "Refinery capacity\n(thous. bbls per day)",
      x = "Longitude",
      y = "Latitude"
    ) +
    # scale_color_manual(values = c('#191970', '#e2711d')) +
    scale_size_continuous(range = c(1, 5)) +
    coord_sf(xlim = disp_win2_coord[, "X"], ylim = disp_win2_coord[, "Y"], expand = FALSE) +
    # theme_void() +
    theme(
      legend.position = "none",
      plot.margin = margin(6, 2, 0, 8),
      legend.title = element_text(size = 12),
      plot.title = element_text(hjust = -0.1, face = "bold", size = 7),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = 5),
      axis.text = element_text(size = 5)
    ) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 2, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf # Extending the rectangle over the entire plot area
    )

  ## DAC legend
  ## ------------------------

  ## figure
  fig1_dac_legend <- ggplot() +
    # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
    geom_sf(data = california, mapping = aes(), fill = "transparent", lwd = 0.4, show.legend = FALSE) +
    geom_sf(data = dac_areas, mapping = aes(geometry = geometry, fill = ct_type), lwd = 0, show.legend = TRUE) +
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

  fig1_refing_legend <- ggplot() +
    geom_sf(data = ca_union, mapping = aes(), fill = "transparent", lwd = 0.4, show.legend = FALSE) +
    # geom_sf(data = dac_areas , mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0, color = "#C0C0C0", show.legend = TRUE) +
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
      lwd = 0.1
    ) +
    geom_sf(data = CA_counties_noisl, mapping = aes(geometry = geometry), lwd = 0.05, fill = NA) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      title = NULL,
      color = NULL,
      size = "Refinery capacity\n(thous. bbls per day)",
      x = NULL,
      y = NULL
    ) +
    scale_size_continuous(
      range = c(1, 5),
      breaks = c(15, 150, 300)
    ) +
    # scale_color_manual(values = c('#191970', '#e2711d')) +
    coord_sf(
      xlim = disp_win2_coord[, "X"], ylim = disp_win2_coord[, "Y"],
      datum = ca_crs, expand = FALSE
    ) +
    theme_void() +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0, 0.2),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7),
      plot.title = element_text(hjust = -0.1, face = "bold", size = 7)
    ) +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0,
        direction = "horizontal",
        ticks.colour = "black", frame.colour = "black"
      ),
      size = guide_legend(direction = "horizontal"),
      color = guide_legend(override.aes = list(size = 3))
    )

  refin_legend <- get_legend(
    fig1_refing_legend
  )




  ## plot together
  map_fig_a <- ggdraw(fig1_map, clip = "on") +
    draw_plot(fig1_inset, x = 0.77, y = 0.67, width = 0.2, height = 0.36) +
    draw_plot(dac_legend, x = 0.22, y = 0.20, width = 0.03, height = 0.005) +
    draw_plot(refin_legend, x = 0.16, y = 0.16, width = 0.025, height = 0.025)
  # +
  #   draw_plot_label(
  #     c("A. Oil fields and disadvantaged communities (DAC)", "", "", ""),
  #     # c(0, 0.45),
  #     # c(1, 0.95),
  #     size = 12
  #   )

  ## create a folder for fig 1
  fig_1_folder <- file.path(
    main_path,
    "outputs",
    "academic-out",
    "refining",
    "figures",
    "2024-08-update",
    "fig1"
  )

  # check if the folder exists
  if (!dir.exists(fig_1_folder)) {
    # Create the folder if it does not exist
    dir.create(fig_1_folder)
  }

  ggsave(map_fig_a,
    filename = file.path(
      fig_1_folder,
      "figure1a.png"
    ),
    width = 88,
    height = 120,
    units = "mm",
    dpi = 300,
    device = "png"
  )

  ggsave(map_fig_a,
    filename = file.path(
      fig_1_folder,
      "figure1a.pdf"
    ),
    width = 88,
    height = 120,
    units = "mm",
    dpi = 300,
    device = "pdf"
  )

  embed_fonts(paste0(fig_1_folder, "/figure1a.pdf"),
    outfile = paste0(fig_1_folder, "/figure1a.pdf")
  )


  ## figure 1c: PM2.5 concentration from Torrance Refinery
  ## ---------------------------------------------------------------------------

  ## refining sites
  sites_vector <- c(226)

  refining_srm <- dt_inmap_re[site %in% sites_vector, ]
  setnames(refining_srm, "totalpm25_aw", "weighted_totalpm25")

  refining_srm_reshape <- refining_srm[, .(total_pm25 = sum(weighted_totalpm25)),
    by = c("site", "GEOID")
  ]

  ## add to map df
  ct_map <- left_join(raw_ct_2019, refining_srm_reshape, by = c("GEOID"))

  ## DACS

  # dac_population <- read.csv(paste0(main_path, "data/health/raw/ces3results_part.csv"), stringsAsFactors = FALSE) %>%
  #   subset(sb535_dac=="Yes")%>%
  #   dplyr::rename(GEOID=census_tract)

  # CA_ct$GEOID = as.double(CA_ct$GEOID)

  # dac_map <- left_join(CA_ct, dac_population, by=c("GEOID"))
  # dac_map <- dac_map %>% dplyr::filter(sb535_dac=="Yes" & COUNTYFP=="037")
  # dac_map <- dac_map %>% dplyr::filter(sb535_dac=="Yes")

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

  # ## merge
  # ct_map_county <- ct_map %>%
  #   left_join(county_ct_boundaries)

  ## create counties based on census tracts
  county_boundaries_from_census_tracts <- ct_map |>
    group_by(COUNTYFP) |>
    summarise(geometry = st_union(geometry)) |>
    ungroup() |>
    left_join(county_ct_boundaries)


  ## crop
  ## -----------------------------------

  disp_win_la_wgs84 <- st_sfc(st_point(c(-118.5, 33.6)), st_point(c(-117.8, 34.2)),
    crs = 4326
  )

  disp_win_la_trans <- st_transform(disp_win_la_wgs84, crs = ca_crs)

  disp_win_la_coord <- st_coordinates(disp_win_la_trans)

  zoom_coord_df <- as.data.frame(disp_win_la_coord)

  county_crop <- st_crop(county_boundaries_from_census_tracts, xmin = zoom_coord_df$X[1], xmax = zoom_coord_df$X[2], ymin = zoom_coord_df$Y[1], ymax = zoom_coord_df$Y[2])
  ct_cropped <- st_crop(ct_map, xmin = zoom_coord_df$X[1], xmax = zoom_coord_df$X[2], ymin = zoom_coord_df$Y[1], ymax = zoom_coord_df$Y[2])

  ## only include census tracts that are in the crop
  ct_intersect <- st_intersection(ct_map |> select(-COUNTYFP), county_crop)


  ## figure
  total_pm25 <- ggplot() +
    geom_sf(data = ct_map |> filter(GEOID %in% ct_intersect$GEOID), aes(geometry = geometry, fill = total_pm25), color = NA) +
    scale_fill_gradient(
      high = "#79032E", low = "#FFFFFF", space = "Lab", na.value = "grey50",
      limits = c(min(ct_cropped$total_pm25), max(ct_cropped$total_pm25)),
      breaks = c(0.001, 0.004)
    ) +
    geom_sf(data = county_crop, mapping = aes(geometry = geometry), lwd = 0.15, alpha = 0) +
    geom_sf(data = refin_capacity %>% filter(site_id == "226"), mapping = aes(geometry = geometry), alpha = 0.9, pch = 21, color = refinery_color, lwd = 0.1, size = 2) +
    labs(
      title = NULL,
      # title = expression(bold(paste("C. PM"[2.5], "concentration from Torrance Refinery"))),
      fill = expression("PM"[2.5] ~ "concentration (" * mu * "g/" * m^3 * ")"),
      x = "Longitude",
      y = "Latitude"
    ) +
    annotate("text", x = 151000, y = -450000, label = "Torrance\nrefinery", color = "black", size = 2) +
    annotate("text", x = 150000, y = -430000, label = "Los Angeles", color = "#545E68", size = 2) +
    annotate("text", x = 195000, y = -480000, label = "Orange", color = "#545E68", size = 2) +
    xlim(c(142000, 200000)) +
    ylim(c(-489424, -424700)) +
    # coord_sf(xlim = disp_win_la_coord[,'X'], ylim = disp_win_la_coord[,'Y'], expand = FALSE) +
    # annotate(
    #   geom = "text", x = 75000, y = -375000,
    #   label = "Los Angeles", hjust = 0, vjust = 1, size = 1.25, fontface = "bold"
    # ) +
    # geom_sf_text(data = refin_capacity %>% filter(site_id == '226') %>% mutate(name = "Torrance\nRefinery"),
    #              mapping = aes(geometry = geometry, label = name), size = 1, fontface = "bold", color = "black", vjust = 1, hjust = 1) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0.035, 0.23),
      legend.key.width = unit(0.7, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 4),
      legend.text = element_text(size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = 5),
      axis.text = element_text(size = 4),
      legend.background = element_rect(fill = NA) # Make legend background transparent
      # plot.margin = margin(0, 2, 0, 8),
      # plot.title = element_text(face = 'bold', size = 5, hjust = -0.05)
    ) +
    guides(fill = guide_colourbar(
      title.position = "top",
      title.hjust = 0,
      direction = "horizontal",
      ticks.colour = "black", frame.colour = "black",
      order = 1
    )) + # Also make the box around the legend transparent)) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 1, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf # Extending the rectangle over the entire plot area
    )


  ggsave(total_pm25,
    filename = file.path(fig_1_folder, "fig1c.png"),
    width = 88,
    height = 120,
    dpi = 300,
    units = "mm",
    device = "png"
  )

  ggsave(total_pm25,
    filename = file.path(fig_1_folder, "fig1c.pdf"),
    width = 88,
    height = 120,
    dpi = 300,
    units = "mm",
    device = "pdf",
    family = "Arial"
  )

  embed_fonts(file.path(fig_1_folder, "fig1c.pdf"),
    outfile = file.path(fig_1_folder, "fig1c.pdf")
  )

  #

  ## figure 1b: PM2.5 concentration of all refinery emissions
  ## -------------------------------------------------------------------------------

  ## filter for 2019 and BAU
  census_tract_pm25_2019 <- health_weighted[year == 2019 &
    scen_id == "BAU historic production"]
  # population
  pop_2020 <- refining_mortality |>
    filter(year == 2020) |>
    select(census_tract, pop) |>
    unique() |>
    as.data.table()

  ## merge
  census_tract_pm25_2019 <- merge(census_tract_pm25_2019, pop_2020[, .(census_tract, pop)],
    by = c("census_tract"),
    all.x = T
  )

  ## weight by total population
  census_tract_pm25_2019[, pop_x_pm25 := total_pm25 * pop]
  ct_census_tract_pm25_2019 <- census_tract_pm25_2019[, .(scen_id, census_tract, disadvantaged, year, pop, total_pm25, pop_x_pm25)]

  ## join with spatial data
  ct_census_tract_pm25_2019_sp <- census_tracts %>%
    left_join(ct_census_tract_pm25_2019) %>%
    filter()


  ## figure
  ct_health_map <- ggplot() +
    geom_sf(data = ct_census_tract_pm25_2019_sp, mapping = aes(geometry = geometry, fill = pop_x_pm25), lwd = 0.0, color = "white", alpha = 1, show.legend = TRUE) +
    scale_fill_gradient(
      high = "#79032E", low = "white", space = "Lab", na.value = "grey50",
      limits = c(min(ct_census_tract_pm25_2019_sp$pop_x_pm25), max(ct_census_tract_pm25_2019_sp$pop_x_pm25)),
      breaks = c(0, 5000, 10000),
      labels = function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
    geom_sf(data = CA_counties_noisl, mapping = aes(geometry = geometry), lwd = 0.15, alpha = 0) +
    geom_sf_text(
      data = CA_counties_noisl %>%
        filter(adj_county_name %in% c("Los Angeles", "Orange", "Solano", "Contra Costa", "Kern", "San Joaquin")),
      mapping = aes(
        geometry = geometry,
        label = adj_county_name
      ), size = 1.5, fontface = "bold", color = "#343a40"
    ) +
    geom_sf(
      data = refin_capacity %>%
        filter(installation == "Existing capacity") %>%
        mutate(object = "Refinery location"),
      mapping = aes(geometry = geometry, color = object), shape = 21, alpha = 0.9, size = 2, stroke = 0.5
    ) +
    scale_color_manual(values = c(refinery_color)) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = expression(paste("Population-weighted PM"[2.5], " (", mu, "g/", m^3, ")")),
      color = NULL,
      shape = NULL,
      x = "Longitude",
      y = "Latitude"
    ) +
    coord_sf(xlim = disp_win2_coord[, "X"], ylim = disp_win2_coord[, "Y"], expand = FALSE) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0.01, 0.15),
      legend.key.width = unit(0.7, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 6),
      legend.text = element_text(size = 6),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = 5),
      axis.text = element_text(size = 4),
      legend.background = element_rect(fill = NA)
    ) +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0,
        direction = "horizontal",
        ticks.colour = "black", frame.colour = "black",
        order = 1
      ),
      size = "none",
      shape = "none",
      color = "none"
    ) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 1, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf # Extending the rectangle over the entire plot area
    )

  ## refinery legend
  ct_health_map_legend <- ggplot() +
    geom_sf(data = ct_census_tract_pm25_2019_sp, mapping = aes(geometry = geometry), lwd = 0.0, color = "white", alpha = 1, show.legend = FALSE) +
    geom_sf(
      data = refin_capacity %>%
        filter(installation == "Existing capacity") %>%
        mutate(object = "Refinery location"),
      mapping = aes(geometry = geometry, color = object), shape = 21, alpha = 0.9, size = 2, stroke = 0.5
    ) +
    scale_color_manual(values = c(refinery_color)) +
    theme_void() +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      # fill = expression(paste("Population-weighted PM"[2.5], " (",mu,"/",m^3,")")),
      color = NULL,
      x = "Longitude",
      y = "Latitude"
    ) +
    coord_sf(
      xlim = disp_win2_coord[, "X"], ylim = disp_win2_coord[, "Y"],
      datum = ca_crs, expand = FALSE
    ) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0, 0.2),
      legend.key.width = unit(0.7, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 6),
      legend.text = element_text(size = 6),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = 5),
      axis.text = element_text(size = 4)
    ) +
    guides(
      shape = guide_legend(order = 1),
      fill = guide_legend(show = FALSE),
      size = guide_legend(show = FALSE),
      color = guide_legend(
        direction = "horizontal",
        override.aes = list(
          pch = 21,
          size = 2,
          alpha = 1,
          stroke = 0.5
        )
      )
    )

  refin_legend2 <- get_legend(
    ct_health_map_legend
  )

  ## plot together
  map_fig_b <- ggdraw(ct_health_map, clip = "on") +
    draw_plot(refin_legend2, x = 0.2, y = 0.22, width = 0.025, height = 0.025)

  ## save
  ggsave(map_fig_b,
    filename = file.path(fig_1_folder, "fig1b.png"),
    width = 88,
    height = 120,
    dpi = 300,
    units = "mm"
  )

  ggsave(map_fig_b,
    filename = file.path(fig_1_folder, "fig1b.pdf"),
    width = 88,
    height = 120,
    units = "mm",
    dpi = 300,
    device = "pdf"
  )

  embed_fonts(file.path(fig_1_folder, "fig1b.pdf"),
    outfile = file.path(fig_1_folder, "fig1b.pdf")
  )



  ## figure 1d: wages from refining
  ## -----------------------------------------------------------------------------------

  ## county compensation 2019 from IMPLAN by destination region
  county_compensation_df <- labor_2019 |>
    group_by(DestinationRegion) |>
    summarise(total_employee_compensation = sum(EmployeeCompensation)) |>
    ungroup() |>
    mutate(
      DestinationRegion = gsub(" County, CA \\(\\d{4}\\)$", "", DestinationRegion),
      DestinationRegion = gsub(" \\(\\d{4}\\)$", "", DestinationRegion)
    ) |>
    rename(region = DestinationRegion) |>
    ## join with counties
    left_join(ca_regions)

  ## adjust county-level compensation based on population ----------------------

  ## compute county populations
  pop_2020 <- refining_mortality %>%
    filter(year == 2020) %>%
    select(census_tract, year, pop) %>%
    unique() %>%
    as.data.table()

  ## census tract x county
  c_ct_df <- raw_pop_income_2021[state == "California"]
  c_ct_df[, census_tract := as.character(substr(geoid, 10, nchar(geoid)))]
  c_ct_df <- c_ct_df[, .(county, census_tract)]
  c_ct_df[, county := str_remove(county, " County")]

  ## merge with counties
  pop_2020 <- merge(pop_2020, c_ct_df,
    by = c("census_tract"),
    all.x = T
  )

  ## summarize by county
  pop_2020 <- pop_2020[, .(county_pop = sum(pop)), by = .(county)]

  ## compute county / region ratio
  county_region_ratio <- merge(pop_2020, ca_regions,
    by = "county"
  )

  ## sum region pop
  county_region_ratio[, region_pop := sum(county_pop), by = .(region)]

  ## calc ratio
  county_region_ratio[, county_ratio := county_pop / region_pop]

  county_region_ratio <- county_region_ratio[, .(county, region, county_pop, region_pop, county_ratio)]

  ## merge
  county_compensation_df_county <- merge(county_compensation_df, county_region_ratio,
    by = c("region", "county"),
    allow.cartesian = T
  )

  ## compute county value
  setDT(county_compensation_df_county)

  county_compensation_df_county[, total_employee_compensation_county := total_employee_compensation * county_ratio]


  ## adjust USD from 2020 to 2019 dollars
  county_compensation_df_county[, total_employee_compensation_county_2019 := total_employee_compensation_county * cpi2019 / cpi2020]

  ## prepare for map
  setnames(county_compensation_df_county, "county", "county_name")

  labor_map_df <- merge(CA_counties_noisl |> rename(county_name = adj_county_name), county_compensation_df_county,
    by = "county_name",
    all.x = T
  )

  ## color pals
  blues_pal <- c("#FAFAFA", "#778DA9", "#415A77", "#1B263B", "#0D1B2A")



  ## make map
  labor_map <- ggplot() +
    geom_sf(
      data = labor_map_df,
      mapping = aes(
        geometry = geometry,
        fill = total_employee_compensation_county_2019 / 1e9
      ), lwd = 0.1, color = "lightgrey", alpha = 1, show.legend = TRUE
    ) +
    scale_fill_gradientn(
      colors = blues_pal,
      breaks = c(0, 1, 2, 3, 4)
    ) +
    geom_sf_text(
      data = labor_map_df %>%
        filter(county_name %in% c(
          "Los Angeles", "Orange", "Solano", "San Luis Obispo",
          "Kern", "Contra Costa"
        )),
      mapping = aes(
        geometry = geometry,
        label = county_name
      ), size = 2, color = "#343a40"
    ) +
    geom_sf(
      data = refin_capacity %>%
        filter(installation == "Existing capacity") %>%
        mutate(object = "Refinery location"),
      mapping = aes(geometry = geometry, color = object), shape = 21, alpha = 0.9, size = 0.6, stroke = 0
    ) +
    scale_color_manual(values = c(refinery_color)) +
    geom_sf(
      data = refin_capacity %>%
        filter(installation == "Existing capacity") %>%
        mutate(object = "Refinery location"),
      mapping = aes(geometry = geometry, color = object), shape = 21, alpha = 0.9, size = 2, stroke = 0.5
    ) +
    # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
    labs(
      # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
      fill = "Employee compensation (2019 USD billion)",
      color = NULL,
      x = "Longitude",
      y = "Latitude"
    ) +
    coord_sf(xlim = disp_win2_coord[, "X"], ylim = disp_win2_coord[, "Y"], expand = FALSE) +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = c(0.01, 0.15),
      legend.key.width = unit(0.7, "line"),
      legend.key.height = unit(0.5, "line"),
      legend.title = element_text(size = 6),
      legend.text = element_text(size = 6),
      plot.margin = margin(8, 2, 0, 8),
      plot.title = element_text(face = "bold", size = 4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = 5),
      axis.text = element_text(size = 4),
      legend.background = element_rect(fill = NA)
    ) +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0,
        direction = "horizontal",
        ticks.colour = "black", frame.colour = "black",
        order = 1
      ),
      size = "none",
      shape = "none",
      color = "none"
    ) +
    annotation_custom(
      grob = rectGrob(gp = gpar(lwd = 1, col = "black", fill = NA)), # lwd for line width, col for color
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf # Extending the rectangle over the entire plot area
    )


  # labor_legend <- ggplot() +
  #   geom_sf(data = labor_map_df,
  #           mapping = aes(geometry = geometry,
  #                         fill = total_employee_compensation_county_2019 / 1e9), lwd = 0.1, color = "lightgrey", alpha = 1, show.legend = TRUE) +
  #   scale_fill_gradientn(colors = blues_pal,
  #                        breaks = c(0, 1, 2, 3, 4)) +
  #   geom_sf_text(data = labor_map_df %>%
  #                  filter(county_name %in% c('Los Angeles', 'Orange', 'Solano', 'San Luis Obispo',
  #                                            'Kern', 'Contra Costa')),
  #                mapping = aes(geometry = geometry,
  #                              label = county_name), size = 2, color = "#343a40") +
  #   geom_sf(data = refin_capacity %>%
  #             filter(installation == "Existing capacity") %>%
  #             mutate(object = "Refinery location"),
  #           mapping = aes(geometry = geometry, color = object), shape = 19, alpha = 0.9, size = 0.6, stroke = 0) +
  #   scale_color_manual(values = c(refinery_color)) +
  #   geom_sf(data = refin_capacity %>%
  #             filter(installation == "Existing capacity") %>%
  #             mutate(object = "Refinery location"),
  #           mapping = aes(geometry = geometry), color = "grey", fill = "transparent", shape = 21, size = 0.61, stroke = 0.1) +
  #   # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
  #   labs(
  #     # title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
  #     fill = "Employee compensation (2019 USD billion)",
  #     color = NULL,
  #     x = "Longitude",
  #     y = "Latitude") +
  #   coord_sf(xlim = disp_win2_coord[,'X'], ylim = disp_win2_coord[,'Y'], expand = FALSE) +
  #   theme(
  #     # legend.justification defines the edge of the legend that the legend.position coordinates refer to
  #     legend.justification = c(0, 1),
  #     # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
  #     legend.position = c(0, 0.2),
  #     legend.key.width = unit(0.7, "line"),
  #     legend.key.height = unit(0.5, "line"),
  #     legend.title = element_text(size = 4),
  #     legend.text = element_text(size = 4),
  #     plot.margin = margin(8, 2, 0, 8),
  #     plot.title = element_text(face = 'bold', size = 4),
  #     panel.grid.major = element_blank(),
  #     panel.grid.minor = element_blank(),
  #     panel.background = element_blank(),
  #     axis.title = element_text(size = 5),
  #     axis.text = element_text(size = 4)) +
  #   guides(shape = guide_legend(order = 1),
  #          fill = guide_legend(show = FALSE),
  #          size = guide_legend(show = FALSE),
  #          color = guide_legend(direction = "horizontal",
  #                               override.aes = list(pch = 19,
  #                                                   size = 1,
  #                                                   alpha = 1)))
  #



  ## plot together
  map_fig_d <- ggdraw(labor_map, clip = "on") +
    draw_plot(refin_legend2, x = 0.2, y = 0.23, width = 0.025, height = 0.025)


  # ## histogram
  # property_value_hist <- ggplot(labor_map_df, aes(total_employee_compensation_county_2019 / 1e9)) +
  #   geom_histogram(fill="#69b3a2", alpha=0.9, binwidth = 0.01) +
  #   labs(x = "Employee compensation (USD billion)",
  #        y = "count (counties)") +
  #   theme_minimal()

  ggsave(map_fig_d,
    filename = file.path(
      fig_1_folder,
      "figure1d.png"
    ),
    width = 88,
    height = 120,
    units = "mm",
    dpi = 300,
    device = "png"
  )

  ggsave(map_fig_d,
    filename = file.path(
      fig_1_folder,
      "figure1d.pdf"
    ),
    width = 88,
    height = 120,
    units = "mm",
    dpi = 300,
    device = "pdf"
  )

  embed_fonts(
    file.path(
      fig_1_folder,
      "figure1d.pdf"
    ),
    outfile = file.path(
      fig_1_folder,
      "figure1d.pdf"
    )
  )

  # ## save histogram
  # ggsave(property_value_hist,
  #        filename = file.path(fig_1_folder,
  #                             "figure1d_hist.png"),
  #        width = 80,
  #        height = 65,
  #        units = "mm",
  #        dpi = 300,
  #        device = 'png')


  ## plot all four together

  fig1_abcd <- plot_grid(
    map_fig_a,
    total_pm25,
    map_fig_b,
    map_fig_d,
    align = "vh",
    labels = c("A", "C", "B", "D"),
    nrow = 2,
    ncol = 2,
    # # labels = 'AUTO',
    # label_size = 10,
    hjust = -1,
    rel_widths = c(1, 1, 1, 1),
    rel_heigts = c(1, 1, 1, 1)
  )

  fig1_abcd



  ggsave(fig1_abcd,
    filename = file.path(
      fig_1_folder,
      "figure1.png"
    ),
    width = 160,
    height = 160,
    units = "mm",
    dpi = 300,
    device = "png"
  )

  ggsave(fig1_abcd,
    filename = file.path(
      fig_1_folder,
      "figure1.pdf"
    ),
    width = 160,
    height = 160,
    units = "mm",
    dpi = 300,
    device = "pdf"
  )

  embed_fonts(
    file.path(
      fig_1_folder,
      "figure1.pdf"
    ),
    outfile = file.path(
      fig_1_folder,
      "figure1.pdf"
    )
  )
}
