# Create main Figure 4: post-lasso logistic regression results
# Create Supp. Figure 5 (select lambda using min-error method)
# author: Qingran Li
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)
getwd()

library(tidyverse)
library(data.table)
library(tigris)
library(bit64)
library(dplyr)
library(ggpubr)

rm(list=ls())
gc()

method = "1SE" # 1SE (for Fig. 4) vs. minE (for Supp Fig. 5)

#####################################################################
# Get Post-lasso logistic regression results 
#####################################################################
# load post-lasso regression results 
regfitList = readRDS(paste0("DAC_postLasso_",method,".RData"))

# import indicator name and category labels -------------------------
toolName = "CEJST"   # select tool (CEJST, DOE, DOT, NYS)
if (is.null(toolName)){
  message(" ============ Tool Name is not defined !!! ============")
  message(" ============ Set Tool Name before proceed ============")
}


if (toolName %in% c("CEJST","DOE", "DOT", "NYS")) {
if (toolName == "CEJST") {
  dict = fread("CEJST_data_cols_binary.csv")
  regfit = regfitList[[1]] 
} else if (toolName == "DOE") {
  dict = fread("DOE_data_cols.csv")
  regfit = regfitList[[2]] 
} else if (toolName == "DOT") {
  dict = fread("DOT_data_cols.csv")
  regfit = regfitList[[3]]
} else if (toolName == "NYS") {
  dict = fread("NYS_data_cols.csv")
  regfit = regfitList[[4]]
}

  summary(regfit)
  message(sprintf("[%s] Num of Obs. = %d", toolName, nrow(regfit$data)))

# save regression results to data table (est, 95% CI) ---------------
regtable = as.data.table(summary(regfit)$coefficients, 
                         keep.rownames = TRUE)
# get 95% confidence interval
ci95 = confint(regfit)
ci95 = as.data.table(ci95, keep.rownames = TRUE)
# add CI to regression table
regtable = regtable %>% left_join(ci95, by = "rn")

# add odds ratio to regression table
regtable = regtable %>% mutate(oddRatio = exp(coef(regfit)))

# rename columns
colnames(regtable) = c("variable", "estimate","se","zvalue",
                       "pvalue","ci_low","ci_high", "oddRatio")

if (toolName == "CEJST") {
  regtable_cej = merge(regtable, dict, 
                       by.x = "variable", by.y = "col_name",all.x = TRUE)
  regtable_cej$tool = "CEJST"
} else if (toolName == "DOE") {
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
message(sprintf("[%s] regtable saved.", toolName))
rm(regfit, regtable)
}

# combine all regression tables (CEJST, DOE, DOT, NYS) =====================
regtable = rbind(regtable_cej, regtable_doe, regtable_dot, regtable_nys)
fwrite(regtable, paste0("Figure_save/RegTable_",method,"_save.csv"))

#####################################################################
# Plot top 10 indicators of DAC designation
#####################################################################
# define color palette ----------------------------------------------
level_cej = c("Socioeconomic Burden", "Workforce Development",
             "Climate Change", "Health",
             "Energy", "Transportation",
             "Legacy Pollution", "Waste and Wastewater", "Housing")
colList_cej = c("Socioeconomic Burden"="#ff7f00",
               "Workforce Development"="#fdbf6f",
               "Climate Change"="#33a02c",
               "Health"="#b2df8a",
               "Energy" = "#1f78b4",
               "Transportation"="#a6cee3",
               "Legacy Pollution" = "#a6761d",
               "Waste and Wastewater" = "#666666",
               "Housing"="#f781bf")

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
dt1 = regtable_cej %>% filter(variable != "(Intercept)") %>% 
  mutate(abs_est = abs(estimate)) %>% 
  arrange(desc(abs_est)) 
dt1$EJ_category = factor(dt1$EJ_category, levels = level_cej)

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
p1 = ggplot(dt1[1:10]) +
  geom_hline(yintercept = 0.5*max(dt1$abs_est), linetype = 2, color = "grey") +
  geom_errorbar(aes(x=reorder(name_short,abs(estimate)), y = estimate,
                    ymin=ci_low, ymax=ci_high),
                width = 0.3) +
  geom_point(aes(x=reorder(name_short,estimate), y=estimate, color=EJ_category),
             shape = 15, size = 2) +
  labs(x = "", y="", color="CEJST (sub)category") +
  scale_color_manual(values = colList_cej) +
  coord_flip() + theme_light() +
  theme(panel.grid = element_blank(), 
        text = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9, face = "bold"))

p1

p2 = ggplot(dt2[1:10]) +
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

p3 = ggplot(dt3[1:10]) + 
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

p4 = ggplot(dt4[1:10]) + 
  geom_hline(yintercept = 0.5*max(dt4$abs_est), linetype = 2, color = "grey") +
  geom_errorbar(aes(x=reorder(name_short,abs(estimate)), y = estimate,
                    ymin=ci_low, ymax=ci_high), 
                width = 0.3) +
  geom_point(aes(x=reorder(name_short,estimate), y=estimate, color=EJ_category), 
             shape = 19, size = 2) +
  labs(x = "", y="", color="NYS (sub)category") +
  scale_color_manual(values = colList_nys) +
  coord_flip() + theme_light() + 
  theme(panel.grid = element_blank(), 
        text = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9, face = "bold")) 

p4

# save figure ============================================================
ggarrange(p1, p2, p3, p4, nrow = 4, align = "v",
          labels = c("a", "b", "c", "d"))
if (method == "1SE") {
  ggsave(paste0("Figure_save/main_Figure 4.pdf"), 
         dpi = 300, width = 8.5, height = 9)
}

if (method == "minE") {
  ggsave(paste0("Figure_save/SI_Fig 5.jpg"), 
         dpi = 300, width = 8.5, height = 9)
}
