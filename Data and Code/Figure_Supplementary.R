# Produce supplementary figures
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

#####################################################################
# plot SI: DAC disparities between federal EJSTs (correlation)
#####################################################################
pop20 = readRDS("US_tr2020_demography.rds")

dt = DAC_US_merged %>% 
  dplyr::select(GEOID20, DAC_cj, DAC_doe, DAC_dot) %>% 
  left_join(pop20, by = "GEOID20")

popVar = c("Population Density", "White", "Black", "American Indian",
           "Asian", "Native Hawaii")
dt.plot1 = data.table(variable = popVar, tool = "CEJST",
                      value = c(cor(dt$DAC_cj, dt$pop_density, use = "complete.obs"),
                                cor(dt$DAC_cj, dt$white_share, use = "complete.obs"),
                                cor(dt$DAC_cj, dt$black_share, use = "complete.obs"),
                                cor(dt$DAC_cj, dt$native_share, use = "complete.obs"),
                                cor(dt$DAC_cj, dt$asian_share, use = "complete.obs"),
                                cor(dt$DAC_cj, dt$hawaii_share, use = "complete.obs")))

dt.plot2 = data.table(variable = popVar, tool = "DOE",
                      value = c(cor(dt$DAC_doe, dt$pop_density, use = "complete.obs"),
                                cor(dt$DAC_doe, dt$white_share, use = "complete.obs"),
                                cor(dt$DAC_doe, dt$black_share, use = "complete.obs"),
                                cor(dt$DAC_doe, dt$native_share, use = "complete.obs"),
                                cor(dt$DAC_doe, dt$asian_share, use = "complete.obs"),
                                cor(dt$DAC_doe, dt$hawaii_share, use = "complete.obs")))

dt.plot3 = data.table(variable = popVar, tool = "DOT",
                      value = c(cor(dt$DAC_dot, dt$pop_density, use = "complete.obs"),
                                cor(dt$DAC_dot, dt$white_share, use = "complete.obs"),
                                cor(dt$DAC_dot, dt$black_share, use = "complete.obs"),
                                cor(dt$DAC_dot, dt$native_share, use = "complete.obs"),
                                cor(dt$DAC_dot, dt$asian_share, use = "complete.obs"),
                                cor(dt$DAC_dot, dt$hawaii_share, use = "complete.obs")))

dt.plot <- rbind(dt.plot1, dt.plot2, dt.plot3)
dt.plot$variable = factor(dt.plot$variable, levels = rev(popVar))

p_S1 = ggplot(dt.plot, aes(x = variable, y = value)) + 
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_point(aes(shape = tool, color = tool), size = 2) + 
  coord_flip() +
  labs(x = "", y = "correlation coefficient (US sample)",
       color = "", shape = "") +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(-0.54, 0.54), breaks = seq(-1,1,0.2)) +
  theme_bw()
p_S1
rm(dt.plot1, dt.plot2, dt.plot3)

#####################################################################
# plot SI: DAC disparities within NY state (correlation)
#####################################################################
dt = DAC_NY_merged %>%
  dplyr::select(GEOID20, DAC_ny, DAC_cj, DAC_doe, DAC_dot) %>% 
  left_join(pop20, by = "GEOID20")

popVar = c("Population Density", "White", "Black", "American Indian",
           "Asian", "Native Hawaii")
dt.plot1 = data.table(variable = popVar, tool = "CEJST",
                      value = c(cor(dt$DAC_cj, dt$pop_density, use = "complete.obs"),
                                cor(dt$DAC_cj, dt$white_share, use = "complete.obs"),
                                cor(dt$DAC_cj, dt$black_share, use = "complete.obs"),
                                cor(dt$DAC_cj, dt$native_share, use = "complete.obs"),
                                cor(dt$DAC_cj, dt$asian_share, use = "complete.obs"),
                                cor(dt$DAC_cj, dt$hawaii_share, use = "complete.obs")))

dt.plot2 = data.table(variable = popVar, tool = "DOE",
                      value = c(cor(dt$DAC_doe, dt$pop_density, use = "complete.obs"),
                                cor(dt$DAC_doe, dt$white_share, use = "complete.obs"),
                                cor(dt$DAC_doe, dt$black_share, use = "complete.obs"),
                                cor(dt$DAC_doe, dt$native_share, use = "complete.obs"),
                                cor(dt$DAC_doe, dt$asian_share, use = "complete.obs"),
                                cor(dt$DAC_doe, dt$hawaii_share, use = "complete.obs")))

dt.plot3 = data.table(variable = popVar, tool = "DOT",
                      value = c(cor(dt$DAC_dot, dt$pop_density, use = "complete.obs"),
                                cor(dt$DAC_dot, dt$white_share, use = "complete.obs"),
                                cor(dt$DAC_dot, dt$black_share, use = "complete.obs"),
                                cor(dt$DAC_dot, dt$native_share, use = "complete.obs"),
                                cor(dt$DAC_dot, dt$asian_share, use = "complete.obs"),
                                cor(dt$DAC_dot, dt$hawaii_share, use = "complete.obs")))

dt.plot4 = data.table(variable = popVar, tool = "NY State",
                      value = c(cor(dt$DAC_ny, dt$pop_density, use = "complete.obs"),
                                cor(dt$DAC_ny, dt$white_share, use = "complete.obs"),
                                cor(dt$DAC_ny, dt$black_share, use = "complete.obs"),
                                cor(dt$DAC_ny, dt$native_share, use = "complete.obs"),
                                cor(dt$DAC_ny, dt$asian_share, use = "complete.obs"),
                                cor(dt$DAC_ny, dt$hawaii_share, use = "complete.obs")))

dt.plot <- rbind(dt.plot1, dt.plot2, dt.plot3, dt.plot4)
dt.plot$variable = factor(dt.plot$variable, levels = rev(popVar))

p_S2 = ggplot(dt.plot, aes(x = variable, y = value)) + 
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_point(aes(shape = tool, color = tool), size = 2) + 
  coord_flip() +
  labs(x = "", y = "correlation coefficient (state sample)",
       color = "", shape = "") +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(-0.54, 0.54), breaks = seq(-1,1,0.2)) +
  theme_bw() 
p_S2

rm(dt.plot1, dt.plot2, dt.plot3, dt.plot4)
#####################################################################
ggarrange(p_S2, p_S1, ncol = 1, labels = c("a.", "b."),
          label.x = 0,  # Adjust x position of labels
          label.y = 1.02, # Adjust y position to move them up
          common.legend = TRUE,  legend = "right")
# ggsave("Fig_S1_demographic_corr.jpg", dpi = 300, width = 6, height = 4.5)



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
  dplyr::select(tractID, DAC, Score_Value, Cumulative_Score_Ptl) %>% 
  mutate(tool = "DOE")

dt_DOT = dt2 %>% 
  dplyr::select(tractID, DAC, Score_Value, Cumulative_Score_Ptl) %>% 
  mutate(tool = "DOT")

dt_NY = dt3 %>% 
  dplyr::select(tractID, DAC, Score_Value, Cumulative_Score_Ptl) %>% 
  mutate(tool = "NYS")


# add urban (dummy) to dt's =========================================
# get urban (dummy variable) from DOT, add to DAC_US_merged
DAC_US_merged <- merge(DAC_US_merged, 
                       dt2 %>% dplyr::select(tractID, Urban_dummy),
                       by.x = "tractID_dot", by.y = "tractID",
                       all.x = TRUE)

dt1_DOE <- merge(dt_DOE, 
                 DAC_US_merged %>% dplyr::select(tractID_doe, Urban_dummy),
                 by.x = "tractID", by.y = "tractID_doe", 
                 all.y = TRUE)
dt1_DOT <- merge(dt_DOT, DAC_US_merged %>% dplyr::select(tractID_dot, Urban_dummy),
                 by.x = "tractID", by.y = "tractID_dot", all.y = TRUE)
dt1_NY <- merge(dt_NY, DAC_US_merged %>% filter(STATEFP == "36") %>% 
                  dplyr::select(tractID_nys, Urban_dummy),
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

pS2 = ggplot(data = dt.plot, 
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
pS2
# save Figure S2 =====================================================
# ggsave("Fig_S2_threshold.jpg", dpi = 300, width = 6, height = 4.5)
