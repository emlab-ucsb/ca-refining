# =============================================================================
# Location map: Phillips 66 Wilmington and Valero Benicia within California
# -----------------------------------------------------------------------------
# PURPOSE
#   A simple reference map placing the two focus refineries in the state, with
#   the other fourteen modelled refineries shown for context. The point of the
#   context points is that the health results in this folder are driven largely
#   by WHERE a refinery sits - Wilmington in the dense Los Angeles basin, Benicia
#   on the inland edge of the Bay Area - so seeing the two in relation to the
#   rest of the sector is the useful thing.
#
# INPUTS (all from the targets store)
#   refin_locs    refinery point locations, already in EPSG:3310
#   raw_counties  county polygons (EPSG:3857, reprojected here)
#   ca_crs        3310, NAD83 / California Albers - the project's CRS target
#
# STYLE
#   sf throughout, matching the rest of the repo (R/figures/fig1.R). No terra
#   needed: everything here is vector data already joined upstream.
# =============================================================================

suppressPackageStartupMessages({
  library(targets); library(sf); library(dplyr); library(data.table)
  library(ggplot2); library(ggrepel)
})

OUT_DIR <- "extras/refinery-level-analysis"
STORE   <- "_targets"
FOCUS   <- c("343", "164")
FOCUS_LAB <- c("343" = "Phillips 66\nWilmington", "164" = "Valero\nBenicia")
FOCUS_COL <- c("Phillips 66\nWilmington" = "#c1666b", "Valero\nBenicia" = "#4281a4")

ca_crs <- tar_read_raw("ca_crs", store = STORE)

# --- counties ----------------------------------------------------------------
# raw_counties carries 60 rows and is in Web Mercator; filter to California and
# reproject to the project CRS, as R/figures/fig1.R:135-136 does.
# NOTE: raw_counties holds 60 features for California's 58 counties - San
# Francisco and Santa Barbara each appear twice, their island parts stored as
# separate features rather than as multipolygons. Harmless for drawing (both
# parts render), but it will duplicate rows for anyone joining county attributes
# by NAME or GEOID; the pipeline guards against this with unique() in
# create_srm_xwalk (R/figures/transport_fig.R:82-85).
counties <- tar_read_raw("raw_counties", store = STORE) %>%
  filter(STATEFP == "06") %>%
  st_transform(ca_crs)

ca_outline <- st_union(counties)

# --- refineries --------------------------------------------------------------
refin <- tar_read_raw("refin_locs", store = STORE) %>%
  mutate(site_id = as.character(site_id))

focus <- refin %>%
  filter(site_id %in% FOCUS) %>%
  mutate(lab = FOCUS_LAB[site_id])

others <- refin %>% filter(!site_id %in% FOCUS)

# Counties hosting the two focus refineries, shaded for orientation.
host <- counties %>%
  filter(NAME %in% c("Los Angeles", "Solano"))

# --- map ---------------------------------------------------------------------
p <- ggplot() +
  geom_sf(data = counties, fill = "grey96", colour = "white", linewidth = 0.25) +
  geom_sf(data = host, fill = "grey88", colour = "white", linewidth = 0.25) +
  geom_sf(data = ca_outline, fill = NA, colour = "grey55", linewidth = 0.4) +
  # the other fourteen modelled refineries, for context
  geom_sf(data = others, shape = 21, fill = "white", colour = "grey45",
          size = 2, stroke = 0.5) +
  # the two of interest
  geom_sf(data = focus, aes(fill = lab), shape = 21, colour = "black",
          size = 4.2, stroke = 0.7) +
  geom_label_repel(
    data = focus,
    aes(label = lab, geometry = geometry, colour = lab),
    stat = "sf_coordinates",
    size = 3.4, lineheight = 0.95, fontface = "bold",
    label.size = 0.35, label.padding = unit(0.3, "lines"),
    fill = "white", box.padding = 0.9, point.padding = 0.5,
    min.segment.length = 0, segment.colour = "grey40", segment.size = 0.4,
    nudge_x = c(-360000, -360000), nudge_y = c(-120000, 180000),
    show.legend = FALSE) +
  scale_fill_manual(values = FOCUS_COL, name = NULL) +
  scale_colour_manual(values = FOCUS_COL, guide = "none") +
  labs(
    title = "Phillips 66 Wilmington and Valero Benicia",
    subtitle = paste("Open circles are the other 14 California refineries in the model.",
                     "Shaded counties are Los Angeles and Solano."),
    caption = paste("Refinery locations from the project's refin_locs target;",
                    "county boundaries from raw_counties. NAD83 / California Albers (EPSG:3310).")) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.02),
    plot.subtitle = element_text(colour = "grey30", size = 9, hjust = 0.02,
                                 margin = margin(b = 6)),
    plot.caption = element_text(colour = "grey45", size = 7.5, hjust = 0.02),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10))

ggsave(file.path(OUT_DIR, "fig_refinery_map.png"), p,
       width = 6.5, height = 8, dpi = 300, bg = "white")

message("\n--- validation ---")
cat("refineries plotted:", nrow(focus), "focus +", nrow(others), "context =", nrow(refin), "\n")
cat("focus sites found :", paste(sort(focus$site_id), collapse = ", "), "\n")
cat("county features   :", nrow(counties), "for",
    length(unique(counties$GEOID)), "unique counties\n")
stopifnot(nrow(focus) == 2, length(unique(counties$GEOID)) == 58)
message("\nWrote: ", file.path(OUT_DIR, "fig_refinery_map.png"))
