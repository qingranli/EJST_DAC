# Generate Figure 1 (US map) and Figure 2 (NY map)
# Qingran Li (last edit Oct 2024)
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)
getwd()

library(tidyverse)
library(data.table)
library(tigris)
library(bit64) # for long interger in GEOID
library(sf)
# library(tidycensus)
library(ggpubr)
library(usmap)

rm(list=ls())
gc()

# import merged DAC data ============================================
DAC_US_merged = readRDS("DAC_US_merged.rds")
DAC_NY_merged = readRDS("DAC_NY_merged.rds")

# removing water census tract (ALAND = 0)
tr2020 = readRDS("GEOID20_84414_US.rds")
tr2020 <- tr2020 %>% 
  filter(ALAND > 0, GEOID20 != "12057980100") # 320 tracts removed

DAC_NY_merged <- DAC_NY_merged %>% filter(GEOID20 %in% tr2020$GEOID20)
DAC_US_merged <- DAC_US_merged %>% filter(GEOID20 %in% tr2020$GEOID20)

#####################################################################
# plot 1: DAC disparities between federal EJSTs (state Map)
#####################################################################
dt.plot = DAC_US_merged %>% 
  group_by(STATEFP) %>% 
  summarise(CEJST = 100*mean(DAC_cj),
            DOE = 100*mean(DAC_doe),
            DOT = 100*mean(DAC_dot)) %>% 
  ungroup() %>% rename(fips = STATEFP)

dt.plot <- dt.plot %>% pivot_longer(cols = c(CEJST,DOE,DOT),
                                    names_to = "tool",
                                    values_to = "value")

state_map = us_map() # get US map
state_map <- state_map %>% left_join(dt.plot, by = "fips")

# Plot US state map *************************************************
p1 = plot_usmap(data = state_map, values = "value", linewidth = 0.2) +
  scale_fill_gradientn(
    colours = c("#01665e","#35978f","#80cdc1","#c7eae5",
                "#f6e8c3","#dfc27d", "#bf812d","#8c510a"),  
    limits = c(0, 80),
    name = "DAC within state (percent)"
  ) +
  facet_wrap(~tool) + theme_void() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 10, face = "bold")) +
  guides(
    fill = guide_colorbar(
      barwidth = 15,  # Custom width of the color bar
      barheight = 0.5  # Custom height of the color bar
    )
  )
p1

# save Figure 1 =====================================================
ggsave("Fig1_US_map.jpg", dpi = 300, width = 8, height = 3)


#####################################################################
# plot 2A: DAC disparities across 4 tools (NY state Map)
#####################################################################
dt = DAC_NY_merged %>%  # count DAC designations
  mutate(sum_DAC = DAC_ny + DAC_cj + DAC_doe + DAC_dot)

NY_map = tracts(state = 36, year = 2020)
NY_map <- NY_map %>% filter(ALAND > 0) %>% 
  select(GEOID) %>% rename(GEOID20 = GEOID) %>% 
  left_join(dt, by = "GEOID20") %>% 
  filter(!is.na(sum_DAC))

NY_map$sum_DAC = factor(NY_map$sum_DAC)

p3 = ggplot(NY_map) + 
  geom_sf(aes(fill = sum_DAC), color = NA) +
  scale_fill_manual(values = c("0"="#e8e8e8", "1"="#0571b0","2"="#92c5de",
                               "3"="#f4a582", "4"="#ca0020")) +
  labs(fill = "Count of DAC \nDesignations") +
  theme_void() + theme(legend.position = c(0.2, 0.9))
p3

#####################################################################
# plot 2B: DAC disparities across 4 tools (rural vs. urban)
#####################################################################
NYS_raw = fread("data_raw_NYSERDA.csv")
NYS_raw <- NYS_raw %>% select(GEOID, REDC, NYC_Region, Urban_Rural,
                              Tribal_Designation, Population_Count)
dt <- merge(dt %>% mutate(GEOID19_num = as.integer64(GEOID19)), 
            NYS_raw, by.x = "GEOID19_num", by.y = "GEOID",
            all.x = TRUE)

dt.plot1 = dt %>% 
  group_by(sum_DAC) %>% mutate(total_tracts = n()) %>% 
  group_by(Urban_Rural, sum_DAC, total_tracts) %>% 
  summarise(count_tracts = n()) 

p4 = ggplot(dt.plot1, aes(x = Urban_Rural, y = 0.001*count_tracts)) +
  geom_col(aes(fill = factor(sum_DAC))) +
  scale_fill_manual(values = c("0"="#e8e8e8", "1"="#0571b0","2"="#92c5de",
                               "3"="#f4a582", "4"="#ca0020")) +
  labs(x = "", y = "census tracts (thousand)", fill = "Count of DAC Designations") +
  # coord_flip() +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position = "none")
p4

#####################################################################
# plot 2C: DAC disparities across 4 tools (rural vs. urban)
#####################################################################
dt.plot2 = dt %>% 
  group_by(sum_DAC) %>% mutate(total_pop = sum(Population_Count)) %>% 
  group_by(Urban_Rural, sum_DAC, total_pop) %>% 
  summarise(count_pop = sum(Population_Count)) 

p5 = ggplot(dt.plot2, aes(x = Urban_Rural, y = 1e-6*count_pop)) +
  geom_col(aes(fill = factor(sum_DAC))) +
  scale_fill_manual(values = c("0"="#e8e8e8", "1"="#0571b0","2"="#92c5de",
                               "3"="#f4a582", "4"="#ca0020")) +
  labs(x = "", y = "population (million)", fill = "Count of DAC Designations") +
  # coord_flip() +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position = "none")
p5


# save Figure 2 =====================================================
ggarrange(p3, ggarrange(p4,p5, nrow = 2, 
                        labels = c("b.", "c."), 
                        label.x = -0.1  # Adjust x position of labels
                        ), 
          labels = c("a."), nrow = 1, widths = c(0.65, 0.35))
ggsave("Fig2_NY_map.jpg", dpi = 300, width = 8, height = 5)



