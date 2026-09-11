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

# --- capacity ----------------------------------------------------------------
# Point area encodes processing capacity in barrels per day. No single target
# covers all sixteen sites, so it is assembled from three:
#   dt_refcap                        crude capacity, the 13 crude refineries
#   individual_refineries_operating  renewable capacity, for the renewable-only
#                                    sites 342-2 and 99999
#   altair_ref                       AltAir Paramount (t-800), absent from both
# Where a site has both crude and renewable capacity (332 Golden Eagle, 342
# Rodeo) the crude figure is used, since that is what the site is sized by in
# the production model. The three renewable-only facilities are small - 2,283 to
# 15,000 bpd against 15,000 to 363,000 for crude - so the visual range is
# dominated by the crude refineries either way.
cap_crude <- as.data.table(tar_read_raw("dt_refcap", store = STORE))[
  , .(site_id = as.character(site_id), capacity_bpd = as.numeric(barrels_per_day))]

cap_renew <- unique(as.data.table(
  tar_read_raw("individual_refineries_operating", store = STORE))[
    refinery_type == "renewable", .(site_id = as.character(refinery_id),
                                    capacity_bpd = as.numeric(capacity_bpd))])

cap_altair <- unique(as.data.table(tar_read_raw("altair_ref", store = STORE))[
  , .(site_id = as.character(site_id),
      capacity_bpd = as.numeric(installation_capacity_bpd))])[!is.na(capacity_bpd)]
cap_altair <- cap_altair[, .(capacity_bpd = max(capacity_bpd)), by = site_id]

capacity <- rbind(cap_crude, cap_renew[!site_id %in% cap_crude$site_id],
                  cap_altair[!site_id %in% c(cap_crude$site_id, cap_renew$site_id)])
capacity <- capacity[, .(capacity_bpd = max(capacity_bpd)), by = site_id]

# --- refineries --------------------------------------------------------------
refin <- tar_read_raw("refin_locs", store = STORE) %>%
  mutate(site_id = as.character(site_id)) %>%
  left_join(capacity, by = "site_id")

stopifnot(!any(is.na(refin$capacity_bpd)))

focus <- refin %>%
  filter(site_id %in% FOCUS) %>%
  mutate(lab = FOCUS_LAB[site_id])

others <- refin %>% filter(!site_id %in% FOCUS)

# Counties hosting the two focus refineries, shaded and labelled on the map.
host <- counties %>%
  filter(NAME %in% c("Los Angeles", "Solano"))

# Label anchors: county centroids, nudged clear of the refinery points. Solano is
# small and sits beside the Bay Area cluster, so its label is pushed north-east;
# Los Angeles county is large enough that its centroid sits inland of the coast.
# st_point_on_surface rather than st_centroid: guaranteed to fall inside the
# polygon, and it does not warn about attributes being assumed constant.
host_lab <- host %>%
  group_by(NAME) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_point_on_surface() %>%
  mutate(
    nx = if_else(NAME == "Solano",  62000, 95000),
    ny = if_else(NAME == "Solano",  52000, 60000))

# --- map ---------------------------------------------------------------------
p <- ggplot() +
  geom_sf(data = counties, fill = "grey96", colour = "white", linewidth = 0.3) +
  geom_sf(data = host, fill = "grey86", colour = "white", linewidth = 0.3) +
  geom_sf(data = ca_outline, fill = NA, colour = "grey50", linewidth = 0.5) +
  # county names placed directly on the map rather than described in a subtitle
  geom_sf_text(data = host_lab, aes(label = NAME, geometry = geometry),
               stat = "sf_coordinates", nudge_x = host_lab$nx, nudge_y = host_lab$ny,
               size = 5, colour = "grey30", fontface = "italic") +
  # the other fourteen modelled refineries, sized by capacity
  geom_sf(data = others, aes(size = capacity_bpd), shape = 21,
          fill = "white", colour = "grey40", stroke = 0.7) +
  # the two of interest
  geom_sf(data = focus, aes(size = capacity_bpd, fill = lab), shape = 21,
          colour = "black", stroke = 1.1) +
  geom_label_repel(
    data = focus,
    aes(label = lab, geometry = geometry, colour = lab),
    stat = "sf_coordinates",
    size = 5.2, lineheight = 0.95, fontface = "bold",
    label.size = 0.5, label.padding = unit(0.4, "lines"),
    fill = "white", box.padding = 1.1, point.padding = 0.8,
    min.segment.length = 0, segment.colour = "grey35", segment.size = 0.6,
    nudge_x = c(-420000, -420000), nudge_y = c(-140000, 210000),
    show.legend = FALSE) +
  scale_size_area(
    max_size = 16, name = "Capacity\n(barrels per day)",
    breaks = c(25000, 100000, 200000, 363000),
    labels = scales::comma) +
  scale_fill_manual(values = FOCUS_COL, guide = "none") +
  scale_colour_manual(values = FOCUS_COL, guide = "none") +
  labs(title = "Phillips 66 Wilmington and Valero Benicia") +
  guides(size = guide_legend(override.aes = list(fill = "white", colour = "grey40"))) +
  theme_void(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.02,
                              margin = margin(b = 10)),
    legend.position = c(0.86, 0.74),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.height = unit(1.5, "lines"),
    plot.margin = margin(12, 12, 12, 12))

ggsave(file.path(OUT_DIR, "fig_refinery_map.png"), p,
       width = 8.5, height = 10, dpi = 300, bg = "white")

message("\n--- validation ---")
cat("refineries plotted:", nrow(focus), "focus +", nrow(others), "context =", nrow(refin), "\n")
cat("focus sites found :", paste(sort(focus$site_id), collapse = ", "), "\n")
cat("county features   :", nrow(counties), "for",
    length(unique(counties$GEOID)), "unique counties\n")
stopifnot(nrow(focus) == 2, length(unique(counties$GEOID)) == 58)
message("\nWrote: ", file.path(OUT_DIR, "fig_refinery_map.png"))
