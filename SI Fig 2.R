# Create Supplementary Figure 2: 
# Linear correlation between disadvantaged community (DAC) designations and ...
# ... population demographics. 
# author: Qingran Li
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)
getwd()

library(tidyverse)
library(data.table)
library(tigris)
library(bit64)
library(sf)
library(patchwork)
library(ggpubr)
library(usmap)

rm(list=ls())
gc()


# import merged DAC data ============================================
DAC_US_merged = readRDS("DAC_US_merged.rds")
DAC_NY_merged = readRDS("DAC_NY_merged.rds")
nrow(DAC_US_merged) # 84414 tracts
nrow(DAC_NY_merged) # 5411 tracts in NY

# removing water census tract (ALAND = 0)
DAC_NY_merged <- DAC_NY_merged %>% filter(ALAND > 0, GEOID20 != "12057980100")
DAC_US_merged <- DAC_US_merged %>% filter(ALAND > 0, GEOID20 != "12057980100")
84414 - nrow(DAC_US_merged) # 320 tracts removed
5411 - nrow(DAC_NY_merged)  # 18 tracts removed in NY

# import demographic variables for 2020 census tracts ===============
pop20 = readRDS("US_tr2020_demography.rds")


#####################################################################
# plot SI: DAC disparities between federal EJSTs (correlation)
#####################################################################
dt = DAC_US_merged %>% 
    select(GEOID20, DAC_cj, DAC_doe, DAC_dot) %>% 
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

fig_B = ggplot(dt.plot, aes(x = variable, y = value)) + 
    geom_hline(yintercept = 0, lty = "dashed") +
    geom_point(aes(shape = tool, color = tool), size = 2) + 
    coord_flip() +
    labs(x = "", y = "correlation coefficient (US sample)",
         color = "", shape = "") +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(limits = c(-0.54, 0.54), breaks = seq(-1,1,0.2)) +
    theme_bw()
fig_B
rm(dt.plot1, dt.plot2, dt.plot3)

#####################################################################
# plot SI: DAC disparities within NY state (correlation)
#####################################################################
dt = DAC_NY_merged %>%
    select(GEOID20, DAC_ny, DAC_cj, DAC_doe, DAC_dot) %>% 
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

fig_A = ggplot(dt.plot, aes(x = variable, y = value)) + 
    geom_hline(yintercept = 0, lty = "dashed") +
    geom_point(aes(shape = tool, color = tool), size = 2) + 
    coord_flip() +
    labs(x = "", y = "correlation coefficient (state sample)",
         color = "", shape = "") +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(limits = c(-0.54, 0.54), breaks = seq(-1,1,0.2)) +
    theme_bw() 
fig_A

rm(dt.plot1, dt.plot2, dt.plot3, dt.plot4)

# save SI figure ====================================================
ggarrange(fig_A, fig_B, ncol = 1, labels = c("a", "b"),
          label.x = 0,  # Adjust x position of labels
          label.y = 1.02, # Adjust y position to move them up
          common.legend = TRUE,  legend = "right")

ggsave("Figure_save/SI_Fig 2.jpg",
       dpi = 300, width = 6, height = 4.5)

