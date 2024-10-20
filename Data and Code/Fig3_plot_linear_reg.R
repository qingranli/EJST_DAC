# plot post-lasso linear regression results (score as dep var.)
# Qingran Li (last edit Oct 2024)
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)
getwd()

library(tidyverse)
library(data.table)
library(tigris)
library(bit64) # for long interger in GEOID
library(openxlsx)
library(dplyr)
# library(MASS)
# library(glmnet)
# library(caret)
# library(progress)
library(ggpubr)

rm(list=ls())
gc()

method = "minErr" # 1SE vs. minErr

#####################################################################
# Get Post-lasso logistic regression results 
#####################################################################
# load post-lasso regression results 
regfitList = readRDS(paste0("Score_postLasso_",method,".RData"))

# import indicator name and category labels -------------------------
toolName = "DOT"   # select tool (DOE, DOT, NYS)

if (toolName == "DOE") {
  dict = fread("DOE_data_cols.csv")
  regfit = regfitList[[1]] 
} else if (toolName == "DOT") {
  dict = fread("DOT_data_cols.csv")
  regfit = regfitList[[2]]
} else if (toolName == "NYS") {
  dict = fread("NYS_data_cols.csv")
  regfit = regfitList[[3]]
}
summary(regfit)

# save regression results to data table (est, 95% CI) ---------------
regtable = as.data.table(summary(regfit)$coefficients, 
                         keep.rownames = TRUE)
# get 95% confidence interval
ci95 = confint(regfit)
ci95 = as.data.table(ci95, keep.rownames = TRUE)
# add CI to regression table
regtable = regtable %>% left_join(ci95, by = "rn")
# rename columns
colnames(regtable) = c("variable", "estimate","se","tvalue",
                       "pvalue","ci_low","ci_high")

if (toolName == "DOE") {
  regtable_doe = merge(regtable, dict, 
                       by.x = "variable", by.y = "col_name",all.x = TRUE)
  regtable_doe$tool = "DOE"
} else if (toolName == "DOT") {
  regtable_dot = merge(regtable, dict, 
                       by.x = "variable", by.y = "col_name",all.x = TRUE)
  regtable_dot$tool = "DOT"
} else if (toolName == "NYS") {
  regtable_nys = merge(regtable, dict, 
                       by.x = "variable", by.y = "col_name",all.x = TRUE)
  regtable_nys$tool = "NY State"
}

rm(regfit, regtable)

regtable = rbind(regtable_doe, regtable_dot, regtable_nys)
# fwrite(regtable, paste0("RegTable_score_",method,"_save.csv"))

#####################################################################
# Plot top 10 importance indicators in cumulative score (normalized)
#####################################################################
# define color palette ----------------------------------------------
level_doe = c("Vulnerable Population",
             "Fossil Dependence",
             "Environment and Climate", 
             "Energy Burden")
colList_doe = c("Vulnerable Population"="#ff7f00",
               "Fossil Dependence"="#fdbf6f",
               "Environment and Climate"="#33a02c",
               "Energy Burden" = "#1f78b4")

level_dot = c("Social Vulnerability",
             "Climate and Disaster", 
             "Health Vulnerability",
             "Transportation Insecurity", 
             "Environmental Burden")
colList_dot = c("Social Vulnerability"="#ff7f00",
               "Climate and Disaster"="#33a02c",
               "Health Vulnerability" = "#b2df8a",
               "Transportation Insecurity"="#a6cee3", 
               "Environmental Burden"="#a6761d")

level_nys = c("Income", "Race and Ethnicity",
              "Climate Change Risks",  "Health",
              "Land Use and Facility Siting",
              "Potential Pollution",
              "Housing Mobility & Communications")
colList_nys = c("Income"="#ff7f00",
                "Race and Ethnicity"="#e34a33",
                "Climate Change Risks"="#33a02c",
                "Health"="#b2df8a",
                "Land Use and Facility Siting" = "#a6761d",
                "Potential Pollution" = "#666666",
                "Housing Mobility & Communications"="#f781bf")

# rank estimate by its absolute value =================================
dt2 = regtable_doe %>% filter(variable != "(Intercept)") %>% 
  mutate(abs_est = abs(estimate)) %>% 
  arrange(desc(abs_est))
dt2$EJ_category = factor(dt2$EJ_category, levels = level_doe)

dt3 = regtable_dot  %>% filter(variable != "(Intercept)") %>% 
  mutate(abs_est = abs(estimate)) %>% 
  arrange(desc(abs_est))
dt3$EJ_category = factor(dt3$EJ_category, levels = level_dot)

dt4 = regtable_nys  %>% filter(variable != "(Intercept)") %>% 
  mutate(abs_est = abs(estimate)) %>% 
  arrange(desc(abs_est))
dt4$EJ_category = factor(dt4$EJ_category, levels = level_nys)

# generate plot =====================================================
p2 = ggplot(dt2[1:10,]) +
  geom_hline(yintercept = 0.5*max(dt2$abs_est), linetype = 2, color = "grey") +
  geom_errorbar(aes(x=reorder(name_short,abs(estimate)), y = estimate,
                    ymin=ci_low, ymax=ci_high),
                width = 0.3) +
  geom_point(aes(x=reorder(name_short,estimate), y=estimate, color=EJ_category),
             shape = 17, size = 2) +
  labs(x = "", y="", color="DOE category") +
  scale_color_manual(values = colList_doe) +
  coord_flip() + theme_light() +
  theme(panel.grid = element_blank(), 
        text = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9, face = "bold"))

p2

p3 = ggplot(dt3[1:10,]) + 
  geom_hline(yintercept = 0.5*max(dt3$abs_est), linetype = 2, color = "grey") +
  geom_errorbar(aes(x=reorder(name_short,abs(estimate)), y = estimate,
                    ymin=ci_low, ymax=ci_high), 
                width = 0.3) +
  geom_point(aes(x=reorder(name_short,estimate), y=estimate, color=EJ_category), 
             shape = 20, size = 2) +
  labs(x = "", y="", color="DOT category") +
  scale_color_manual(values = colList_dot) +
  coord_flip() + theme_light() + 
  theme(panel.grid = element_blank(), 
        text = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9, face = "bold")) 

p3

p4 = ggplot(dt4[1:10,]) + 
  geom_hline(yintercept = 0.5*max(dt4$abs_est), linetype = 2, color = "grey") +
  geom_errorbar(aes(x=reorder(name_short,abs(estimate)), y = estimate,
                    ymin=ci_low, ymax=ci_high), 
                width = 0.3) +
  geom_point(aes(x=reorder(name_short,estimate), y=estimate, color=EJ_category), 
             shape = 19, size = 2) +
  labs(x = "", y="", color="NY State (sub)category") +
  scale_color_manual(values = colList_nys) +
  coord_flip() + theme_light() + 
  theme(panel.grid = element_blank(), 
        text = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9, face = "bold")) 

p4

# save figure ============================================================
ggarrange(p2, p3, p4, nrow = 3, align = "v", labels = c("a.", "b.", "c."))
# ggsave(paste0("Fig3_ScorePtl_reg_",method,".jpg"), 
#        dpi = 300,width = 8.5,height = 6.75)
