# Create Supplementary Figure 1. two economic regions in NY
# author: Qingran Li
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)
getwd()

library(tidyverse)
library(data.table)
library(tigris)
# library(usmap)
library(bit64) # for long integer in GEOID
library(sf)
library(ggpubr)
library(maptiles) # fetch open-license basemap tiles (OSM, CARTO)
library(tidyterra) # function geom_spatraster_rgb()

rm(list=ls())
gc()

# import merged EJST data ============================================
DAC_merged = readRDS("DAC_US_merged.rds") %>% 
  mutate(tractID = as.integer64(GEOID19))

# import DOE data
dt.doe = readRDS("EJ_index_DAC_DOE_v2.rds")
dict.doe = fread("DOE_data_cols.csv")

# Group census tracts based on Energy and Socioeconomic burdens =========
# socioeconomic burden (SB) indicator = population with income below 200% FPL
# energy burden (EB) indicator = grid outage duration (percentile)
dt1.doe = dt.doe %>% 
  select(tractID, State_abbrev, 
         lowincome_fpl_pct, grid_outage_duration) %>% 
  mutate(Bgroup = case_when(
    (grid_outage_duration >= 80 & lowincome_fpl_pct >= 80) ~ "High EB and High SB",
    (grid_outage_duration >= 80 & lowincome_fpl_pct <= 20) ~ "High EB and Low SB",
    (grid_outage_duration <= 20 & lowincome_fpl_pct >= 80) ~ "Low EB and High SB",
    .default = "Others"
  ))

# merge data with DAC_merged (by GEOID19 as tractID)
dt = DAC_merged %>% left_join(dt1.doe, by = "tractID")
rm(DAC_merged, dt1.doe)

# map NY census with EJST data (focus on NY state) ==================
NY_map = tracts(state = 36, year = 2020) %>% filter(ALAND > 0) %>% 
  mutate(tractID = as.integer64(GEOID)) %>% select(tractID) 

# merge groups into NY_map 
dt_map = NY_map %>% left_join(dt, by = "tractID") %>% 
  mutate(countyID = substr(GEOID19,1,5)) %>% 
  filter(!is.na(DAC_doe))

dt_map$Bgroup = factor(dt_map$Bgroup, levels = c("High EB and High SB",
                                                 "High EB and Low SB",
                                                 "Low EB and High SB",
                                                 "Others"))

group_colors <- c(
  "High EB and High SB" = "#fdc086", #Double disadv.
  "High EB and Low SB" = "#beaed4", #Energy disadv.
  "Low EB and High SB" = "#7fc97f", #Socioeconomic disadv.
  "Others" = NA
)

DAC_colors = c("1" = "red", "0" = NA)

# Get map legend ====================================================
fullMap = ggplot(dt_map) +
  geom_sf(aes(fill = Bgroup, color = factor(DAC_doe))) +
  scale_fill_manual(values = group_colors, na.value = "white",
                    name = "Burden Profile") +
  scale_color_manual(values = DAC_colors, na.value = NA,
                     name = "DAC in DOE") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
Maplegend = get_legend(fullMap) %>% as_ggplot()

# download county boundaries ========================================
NY_counties = counties(state = 36, cb = TRUE, year = 2020) %>% 
  rename(countyID = GEOID) %>% st_transform(st_crs(dt_map))

# highlight counties in 2 Economic Development Regions of New York State
NY_counties <- NY_counties %>% 
  mutate(Region = case_when(
    NAME %in% c("Dutchess", "Orange", "Putnam",
                "Rockland", "Sullivan", "Ulster", "Westchester") ~ "Mid-Hudson",
    NAME %in% c("Jefferson", "St. Lawrence", "Lewis",
                "Franklin", "Clinton", "Essex") ~ "North Country",
    .default = "Others"
  )) 

p1.data = dt_map %>% left_join(NY_counties %>% data.table() %>% 
                                 select(countyID, NAME, Region),
                               by = "countyID") %>% 
  filter(Region == "Mid-Hudson")

p2.data = dt_map %>% left_join(NY_counties %>% data.table() %>% 
                                 select(countyID, NAME, Region),
                               by = "countyID") %>% 
  filter(Region == "North Country")


# Set Basemap from OpenStreetMap ====================================
# Reproject data to WGS84
p1.data.wgs <- p1.data %>% st_transform(4326)
p2.data.wgs <- p2.data %>% st_transform(4326)
NY_counties.wgs <- NY_counties %>% st_transform(4326)

# Pull basemap tiles cropped to each region
basemap_p1 <- get_tiles(
  x = p1.data.wgs,
  provider = "CartoDB.Positron",
  zoom = 9,
  crop = TRUE
)

basemap_p2 <- get_tiles(
  x = p2.data.wgs,
  provider = "CartoDB.Positron",
  zoom = 9,
  crop = TRUE
)



# Map again with basemap as the bottom layer ===============================
p1 = ggplot() +
  geom_spatraster_rgb(data = basemap_p1) +
  geom_sf(data = p1.data.wgs, aes(fill = Bgroup, color = factor(DAC_doe)),
          alpha = 0.8, inherit.aes = FALSE) +
  geom_sf(data = NY_counties.wgs %>% filter(Region == "Mid-Hudson"),
          fill = NA, color = "gray30", lwd = 0.7,
          inherit.aes = FALSE) +
  scale_fill_manual(values = group_colors, na.value = "white") +
  scale_color_manual(values = DAC_colors, na.value = NA) +
  theme_void() +
  theme(
    legend.position = "none"
  )

p2 = ggplot() +
  geom_spatraster_rgb(data = basemap_p2)  +
  geom_sf(data = p2.data.wgs, aes(fill = Bgroup, color = factor(DAC_doe)),
          alpha = 0.8, inherit.aes = FALSE) +
  geom_sf(data = NY_counties.wgs %>% filter(Region == "North Country"),
          fill = NA, color = "gray30", lwd = 0.7,
          inherit.aes = FALSE) +
  scale_fill_manual(values = group_colors, na.value = "white") +
  scale_color_manual(values = DAC_colors, na.value = NA) +
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  labs(caption = "Basemap \u00a9 OpenStreetMap contributors, \u00a9 CARTO")

ggarrange(p1, p2, Maplegend, nrow =1,
          widths = c(1,1.1,0.5), hjust = 0,
          labels = c("a", "b", "")) 

ggsave("Figure_save/SI_Fig 1.jpg", 
       dpi = 300, width = 11, height = 5)
