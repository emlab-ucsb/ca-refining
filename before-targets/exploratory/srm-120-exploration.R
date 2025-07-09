## tracey mangin
## july 7, 2025
## srm exploration for refinery 120

## attach libraries
library(data.table)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(tidyverse)

## user
user <- "vincent"

## paths
## -------------------------------------------

# list paths
list_paths <- c(
    "tracey-laptop" = "/Users/traceymangin/Library/CloudStorage/GoogleDrive-tmangin@ucsb.edu/Shared\ drives/emlab/projects/current-projects/calepa-cn/",
    "tracey-desktop" = "/Users/tracey/Library/CloudStorage/GoogleDrive-tmangin@ucsb.edu/Shared\ drives/emlab/projects/current-projects/calepa-cn/",
    "vincent" = "H://Shared drives/emlab/projects/current-projects/calepa-cn",
    "meas" = "data"
  )

# set main path
main_path <- list_paths[user]

file_inmap_re <- file.path(main_path, 
                           "data-staged-for-deletion/health/source_receptor_matrix/inmap_processed_srm/refining")

file_refin_locs <- file.path(main_path, 
                             "/data-staged-for-deletion/stocks-flows/processed/refinery_lat_long_revised.csv")

file_refin_locs_orig <- file.path(main_path,
                                  "data-staged-for-deletion/GIS/raw/Petroleum_Refineries_US_EIA/Petroleum_Refineries_US_2019_v2.shp"
)

ca_crs <- 3310

## functions
## --------------------------------------------

read_inmap_data <- function(inmap_path, bsite) {
  
  if (bsite != 120) {
  
  nh3 <- fread(
    paste0(inmap_path, "/nh3/srm_nh3_site", bsite, ".csv"),
    header = TRUE,
    colClasses = c(GEOID = "character")
  )
  } else {nh3 <- data.table()}
  
  nox <- fread(
    paste0(inmap_path, "/nox/srm_nox_site", bsite, ".csv"),
    header = TRUE,
    colClasses = c(GEOID = "character")
  )
  pm25 <- fread(
    paste0(inmap_path, "/pm25/srm_pm25_site", bsite, ".csv"),
    header = TRUE,
    colClasses = c(GEOID = "character")
  )
  sox <- fread(
    paste0(inmap_path, "/sox/srm_sox_site", bsite, ".csv"),
    header = TRUE,
    colClasses = c(GEOID = "character")
  )
  voc <- fread(
    paste0(inmap_path, "/voc/srm_voc_site", bsite, ".csv"),
    header = TRUE,
    colClasses = c(GEOID = "character")
  )
  
  nh3[, pollutant := "nh3"]
  nox[, pollutant := "nox"]
  pm25[, pollutant := "pm25"]
  sox[, pollutant := "sox"]
  voc[, pollutant := "voc"]
  
  all_pollutants <- rbind(nox, pm25, sox, voc, nh3, fill = TRUE)
  all_pollutants[, site := bsite]
  
}

process_weighted_pm25 <- function(dt_inmap_re) {
  dt <- copy(dt_inmap_re)
  setnames(dt, "totalpm25_aw", "weighted_totalpm25")

  dt_wide <- dcast(
    dt,
    GEOID + site ~ pollutant,
    value.var = "weighted_totalpm25"
  )
  setnames(
    dt_wide,
    c("nh3", "nox", "pm25", "sox", "voc"),
    paste0("weighted_totalpm25_", c("nh3", "nox", "pm25", "sox", "voc"))
  )
  
  dt_wide <- dt_wide[!is.na(GEOID)]
  
  setnames(dt_wide, "site", "site_id")

  dt_wide <- unique(dt_wide)

  dt_wide
}

read_refin_locs <- function(file_refin_locs, file_refin_locs_orig, ca_crs) {
  refin_crs <- st_crs(st_read(file_refin_locs_orig))
  
  ## Refineries plus
  refin_new_locations <- fread(file_refin_locs) %>%
    mutate(coords = gsub("^c\\(|\\)$", "", geometry)) %>%
    separate(coords, c("lon", "lat"), sep = ",") %>%
    select(-geometry) %>%
    st_as_sf(
      coords = c("lon", "lat"),
      crs = refin_crs
    ) %>%
    st_transform(ca_crs)
}


## read data
## --------------------------------------------

buff_sites <- c(
  97,
  119,
  120,
  164,
  202,
  209,
  226,
  271,
  279,
  332,
  342,
  343,
  800,
  3422,
  34222,
  99999
)

# refinery locations
refin_locs <- read_refin_locs(file_refin_locs,
  file_refin_locs_orig,
  ca_crs
)

# read inputs
dt_inmap_re <-  rbindlist(lapply(
  buff_sites,
  read_inmap_data,
  inmap_path = file_inmap_re
))

# process srm
srm_weighted_pm25 <- process_weighted_pm25(dt_inmap_re)

## find closest refineries
## --------------------------------------------------

# Select the reference point (e.g., id == "A")
ref_point <- refin_locs[refin_locs$site_id == 120, ]

# Compute distances from the reference point to all others
distances <- st_distance(ref_point, refin_locs)

# Optional: add to the original data
refin_locs$distance_to_120 <- as.numeric(distances)  # units in meters by default

# rank
refin_locs$dist_rank <- rank(refin_locs$distance_to_120, ties.method = "min")

# disance in km
refin_locs$dist_km <- refin_locs$distance_to_120 / 1000

## modify so that 120 doesn't count
refin_locs <- refin_locs |>
  mutate(dist_rank = ifelse(site_id == 120, 0, dist_rank - 1),
         site_name = paste0(site_id, " dist = ", round(dist_km, 1), " km"))

refin_locs <- refin_locs %>%
  mutate(site_name = fct_reorder(site_name, dist_rank))

# usa <- ne_states(country = "united states of america", returnclass = "sf")
# california <- usa[usa$name == "California", ]
# 
# california <- st_transform(california, ca_crs)
# 
# refinery_fig <- ggplot() +
#   geom_sf(data = california, fill = NA, color = "black") +
#   geom_sf(data = refin_locs, aes(color = site_id), alpha = 0.7, size = 1) +    # point locations
#   # geom_sf_text(data = refin_locs, aes(label = site_id), nudge_y = 0.05) +  # labels
#   theme_minimal()
# 
# plotly::ggplotly(refinery_fig)

## compute ratio, NH3 to PM2.5
## --------------------------------------------

srm_ratio_df_nh3 <- srm_weighted_pm25 |>
  select(GEOID, site_id, weighted_totalpm25_nh3)

srm_ratio_df <- srm_weighted_pm25 |>
  pivot_longer(weighted_totalpm25_nh3:weighted_totalpm25_voc, 
               names_to = "pollutant",
               values_to = "concentration") |>
  filter(pollutant != "weighted_totalpm25_nh3") |>
  left_join(srm_ratio_df_nh3) |>
  mutate(site_id = as.character(site_id),
         ratio_nh3_pol = weighted_totalpm25_nh3 / concentration) |>
  left_join(refin_locs |>
              select(site_id, cluster, distance_to_120, dist_rank, site_name, geometry)) |>
  mutate(pollutant_name = str_remove(pattern = "weighted_totalpm25_", pollutant))

allboxplot <- ggplot(srm_ratio_df |> filter(site_id != 120,
                              cluster == "Bay Area"), aes(x = pollutant_name, y = ratio_nh3_pol)) +
  geom_boxplot() +
  facet_wrap(~ site_name, scales = "fixed") +  # or facet_grid(pollutant ~ site_id)
  scale_fill_brewer(palette = "Set2") +  # or manual colors
  labs(y = "concentration ratio: NH3 to pollutant",
       x = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold")
  )

ggsave(filename = "srm_ratio_pollutant_pm25.png",
       plot = allboxplot,
       path = here::here("before-targets", "exploratory", "exploratory-figs"),
       width = 8,
       height = 6,
       dpi = 300)


boxplot_summary_df <- srm_ratio_df |> 
  filter(!site_id %in% c(120, 3422),
         cluster == "Bay Area",
         pollutant_name == "pm25") |>
  group_by(site_id, site_name, pollutant) |>
  summarise(
    Q1 = quantile(ratio_nh3_pol, 0.25, na.rm = TRUE),
    Median = quantile(ratio_nh3_pol, 0.5, na.rm = TRUE),
    Q3 = quantile(ratio_nh3_pol, 0.75, na.rm = TRUE),
    IQR = IQR(ratio_nh3_pol, na.rm = TRUE),
    Lower_Whisker = max(min(ratio_nh3_pol), Q1 - 1.5 * IQR),
    Upper_Whisker = min(max(ratio_nh3_pol), Q3 + 1.5 * IQR)
  )

ratio_fig <- ggplot(srm_ratio_df |> filter(!site_id %in% c(120, 3422),
                                           cluster == "Bay Area",
                                           pollutant_name == "pm25"), 
                    aes(x = site_name, y = ratio_nh3_pol)) +
  geom_boxplot() +
  geom_text(data = boxplot_summary_df, aes(x = site_name, y = Median, label = round(Median, 2)),
            vjust = -0.5, size = 3, color = "black") +
  labs(y = "concentration ratio: NH3 to PM2.5",
       x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(face = "bold")
  )

ggsave(filename = "srm_ratio_nh3_pm25.png",
       plot = ratio_fig,
       path = here::here("before-targets", "exploratory", "exploratory-figs"),
       width = 8,
       height = 6,
       dpi = 300)

## repeat, 120 PM2.5 to other site PM2.5
## --------------------------------------------

srm_ratio_df_120 <- srm_weighted_pm25 |>
  filter(site_id == 120) |>
  select(GEOID, pm25_120 = weighted_totalpm25_pm25) 
  
srm_ratio_pm25_df <- srm_weighted_pm25 |>
  filter(site_id != 120) |>
  select(GEOID, site_id, pm25 = weighted_totalpm25_pm25) |>
  left_join(srm_ratio_df_120) |>
  mutate(site_id = as.character(site_id),
         ratio_pm25 = pm25_120 / pm25) |>
  left_join(refin_locs |>
            select(site_id, cluster, distance_to_120, dist_rank, site_name, geometry)) 

pm25_boxplot_summary_df <- srm_ratio_pm25_df |> 
  filter(!site_id %in% c(120, 3422),
         cluster == "Bay Area") |>
  group_by(site_id, site_name) |>
  summarise(
    Q1 = quantile(ratio_pm25, 0.25, na.rm = TRUE),
    Median = quantile(ratio_pm25, 0.5, na.rm = TRUE),
    Q3 = quantile(ratio_pm25, 0.75, na.rm = TRUE),
    IQR = IQR(ratio_pm25, na.rm = TRUE),
    Lower_Whisker = max(min(ratio_pm25), Q1 - 1.5 * IQR),
    Upper_Whisker = min(max(ratio_pm25), Q3 + 1.5 * IQR)
  )

pm25_ratio_fig <- ggplot(srm_ratio_pm25_df |> filter(!site_id %in% c(120, 3422),
                                           cluster == "Bay Area"), 
                         aes(x = site_name, y = ratio_pm25)) +
  geom_boxplot() +
  geom_text(data = pm25_boxplot_summary_df, aes(x = site_name, y = Median, label = round(Median, 2)),
            vjust = -0.5, size = 3, color = "black") +
  labs(y = "Concentration ratio: site 120 PM2.5 to PM2.5",
       x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(face = "bold")
  )

ggsave(filename = "srm_ratio_120pm25_pm25.png",
       plot = pm25_ratio_fig,
       path = here::here("before-targets", "exploratory", "exploratory-figs"),
       width = 8,
       height = 6,
       dpi = 300)

pm25_ratio_fig_no <- ggplot(srm_ratio_pm25_df |> filter(!site_id %in% c(120, 3422),
                                                     cluster == "Bay Area"), 
                         aes(x = site_name, y = ratio_pm25)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = pm25_boxplot_summary_df, aes(x = site_name, y = Median, label = round(Median, 2)),
            vjust = -0.2, size = 3, color = "black") +
  labs(y = "Concentration ratio: site 120 PM2.5 to PM2.5",
       x = NULL) +
  ylim(0.75, 1.3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(face = "bold")
  )

ggsave(filename = "srm_ratio_120pm25_pm25_no_outliers.png",
       plot = pm25_ratio_fig_no,
       path = here::here("before-targets", "exploratory", "exploratory-figs"),
       width = 8,
       height = 6,
       dpi = 300)

#### importance of NH3

#across sites
total <- srm_weighted_pm25 %>%
  #filter(!(site_id %in% "120"))%>%
  group_by(site_id)%>%
  mutate(across(starts_with("weighted"), sum))%>%
  select(-GEOID)%>%
  ungroup()%>%
  unique()%>%
  gather("poll", "total_pm25", -site_id)%>%
  mutate(poll = toupper(gsub("^.*_", "", poll)))

ef <- fread(paste0(main_path,"/data-staged-for-deletion/health/processed/refinery_emission_factor.csv"), 
      stringsAsFactors =  F)%>%
  mutate(ton_bbl = 0.001*kg_bbl)%>%
  select(-kg_bbl)

poll_importance <- ef %>% 
  mutate(pollutant_code = ifelse(pollutant_code == "SO2", "SOX", pollutant_code),
         pollutant_code = ifelse(pollutant_code == "PM25-PRI", "PM25", pollutant_code))%>%
  left_join(total, by = c("id1"="site_id", "pollutant_code"= "poll"))%>%
  mutate(total_pm25_bbl = ton_bbl*total_pm25)

poll_importance %>%
  ggplot(aes(x= as.factor(id1), y = total_pm25_bbl))+
  geom_point()+
  facet_wrap(~pollutant_code)+
  labs(x = "Site ID", y = "Total secondary PM2.5 dispersed per barrel")+
  theme_classic(16)
