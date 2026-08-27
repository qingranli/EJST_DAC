# Create Supplementary Figure 3: 
# Disadvantaged community (DAC) designations in three scoring-based EJSTs ...
# ... and the cumulative scores.  
# author: Qingran Li
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)
getwd()

library(tidyverse)
library(data.table)
library(bit64)
library(ggpubr)

rm(list=ls())
gc()

#####################################################################
# import DAC score saved from 3 EJSTs 
dt1 = readRDS("DAC_score_DOE_long.rds")
dt2 = readRDS("DAC_score_DOT_long.rds")
dt3 = readRDS("DAC_score_NYS_long.rds")

DAC_US_merged = readRDS("DAC_US_merged.rds")
DAC_US_merged <- DAC_US_merged %>% 
  mutate(tractID_dot = as.integer64(GEOID20),
         tractID_doe = as.integer64(GEOID19),
         tractID_nys = as.integer64(GEOID19))

# keep tracts in DAC_merged
dt1 <- dt1 %>% filter(tractID %in% unique(DAC_US_merged$tractID_doe))
dt2 <- dt2 %>% filter(tractID %in% unique(DAC_US_merged$tractID_dot))
dt3 <- dt3 %>% filter(tractID %in% unique(DAC_US_merged$tractID_nys))

# extract DAC dummy, cumulative Score (raw), and score (ptl) =======
dt_DOE = dt1 %>% 
  select(tractID, DAC, Score_Value, Cumulative_Score_Ptl) %>% 
  mutate(tool = "DOE")

dt_DOT = dt2 %>% select(tractID, DAC, Score_Value, Cumulative_Score_Ptl) %>% 
  mutate(tool = "DOT")

dt_NY = dt3 %>% 
  select(tractID, DAC, Score_Value, Cumulative_Score_Ptl) %>% 
  mutate(tool = "NYS")


# add urban (dummy) to dt's =========================================
# get urban (dummy variable) from DOT, add to DAC_US_merged
DAC_US_merged <- merge(DAC_US_merged, 
                       dt2 %>% select(tractID, Urban_dummy),
                       by.x = "tractID_dot", by.y = "tractID",
                       all.x = TRUE)

dt1_DOE <- merge(dt_DOE, DAC_US_merged %>% select(tractID_doe, Urban_dummy),
                by.x = "tractID", by.y = "tractID_doe", 
                all.y = TRUE)
dt1_DOT <- merge(dt_DOT, DAC_US_merged %>% select(tractID_dot, Urban_dummy),
                by.x = "tractID", by.y = "tractID_dot", all.y = TRUE)
dt1_NY <- merge(dt_NY, DAC_US_merged %>% filter(STATEFP == "36") %>% 
                  select(tractID_nys, Urban_dummy),
                by.x = "tractID", by.y = "tractID_nys", all.y = TRUE)

#####################################################################
# plot scoring-based EJST results (score vs percentile)
dt.plot = rbind(dt1_NY, dt1_DOE, dt1_DOT)

# min-max normalize score
dt.plot <- dt.plot %>% filter(!is.na(Score_Value)) %>% 
  group_by(tool) %>% 
  mutate(a = min(Score_Value, na.rm = TRUE),
         b = max(Score_Value, na.rm = TRUE)) %>% 
  mutate(Score_norm = (Score_Value-a)/(b-a),
         DAC_text = ifelse(DAC==1, "DAC Tracts", "Other Tracts"))

ggplot(data = dt.plot, 
       aes(x= Score_norm, y = Cumulative_Score_Ptl)) +
  geom_point(aes(color = factor(Urban_dummy),
                 shape = factor(Urban_dummy)), alpha = 0.7, size = 2) + 
  facet_grid(tool~DAC_text) +
  scale_color_manual(values = c("0"="#998ec3", "1"="#f1a340")) +
  scale_shape_manual(values = c("0"=0, "1"=20)) +
  labs(x = "Cumulative Score, min-max normalized", 
       y = "Cumulative Score, full-sample percentile rank", 
       color = "Urban tract (in 2020 census map)",
       shape = "Urban tract (in 2020 census map)") +
  theme_bw() + theme(panel.grid.major.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     legend.position = "top")

# save Figure S2 =====================================================
ggsave("Figure_save/SI_Fig 3.jpg", 
       dpi = 300, width = 6, height = 4.5)
