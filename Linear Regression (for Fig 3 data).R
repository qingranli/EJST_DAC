# Get post-lasso linear regression result (Score Ptl ~ indicators)
# save 1 data file for main Fig 3: Score_postLasso_1SE.RData
# save 1 data file for Supp. Fig 4: Score_postLasso_minE.RData
# author: Qingran Li
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)
getwd()

library(tidyverse)
library(data.table)
library(bit64) # for long integer in GEOID
library(dplyr)
library(MASS)
library(glmnet)
library(caret)
library(progress)

rm(list=ls())
gc()

# import DAC score saved from 3 EJSTs ===============================
dt1 = readRDS("DAC_score_DOE_long.rds")
dt2 = readRDS("DAC_score_DOT_long.rds")
dt3 = readRDS("DAC_score_NYS_long.rds")

# extract cumulative Score (raw and percentile) 
dt_DOE = dt1 %>% mutate(tool = "DOE") %>% 
  dplyr::select(tool, tractID, Score_Value, Cumulative_Score_Ptl)

dt_DOT = dt2 %>% mutate(tool = "DOT") %>% 
  dplyr::select(tool, tractID, Score_Value, Cumulative_Score_Ptl)

dt_NYS = dt3 %>% mutate(tool = "NYS") %>% 
  dplyr::select(tool, tractID, Score_Value, Cumulative_Score_Ptl)

dt_score = rbind(dt_DOE, dt_DOT, dt_NYS)
rm(dt_DOE, dt_DOT, dt_NYS)

#####################################################################
# 0. Prepare data for regression 
#####################################################################
toolName = NULL   # select scoring-based tool (DOE, DOT, NYS)
if (is.null(toolName)){
  message(" ============ Tool Name is not defined !!! ============")
  message(" ============ Set Tool Name before proceed ============")
}

# import data (DAC and indicators) 
# import indicator name and category labels from "dictionary" files
if (toolName == "DOE") {
  dt0 = readRDS("EJ_index_DAC_DOE.rds") # *****DOE
  dict = fread("DOE_data_cols.csv")
  dt <- dt0 %>% left_join(dt_score %>% filter(tool=="DOE"),
                          by = "tractID")
} else if (toolName == "DOT") {
  dt0 = readRDS("EJ_index_DAC_DOT.rds") # *****DOT
  dict = fread("DOT_data_cols.csv")
  dt <- dt0 %>% left_join(dt_score %>% filter(tool=="DOT"),
                          by = "tractID")
} else if (toolName == "NYS") {
  dt0 = readRDS("EJ_index_DAC_NYS.rds") #*****NY State
  dict = fread("NYS_data_cols.csv")
  dt <- dt0 %>% left_join(dt_score %>% filter(tool=="NYS"),
                          by = "tractID")
}

# summary(dt)
# colnames(dt)

#####################################################################
# 1. post-LASSO: linear regression (lambda use 1SE rule)
#####################################################################
if (toolName %in% c("DOE", "DOT", "NYS")) {
  message(sprintf("*** Start Analysis for [%s] - pick lambda use 1SE rule ***", 
                  toolName))
  
dt1 = na.omit(dt) # remove data containing NA values
y = dt1$Cumulative_Score_Ptl
# change column index accordingly
if (toolName == "DOE") {
  x = data.matrix(dt1[,5:40]) # ***DOE (36 indicators)
} else if (toolName == "DOT") {
  x = data.matrix(dt1[,5:44]) # ***DOT (40 indicators)
} else if (toolName == "NYS") {
  x = data.matrix(dt1[,4:48]) # ***NY State (45 indicators)
}

# step 1: select lambda ---------------------------------------------
# use the one-standard-error (1SE) rule: The std err of the CV estimate
# ... is calculated for each fold. Use the most parsimonious model where the 
# ... CV error is within 1 SE of the minimum CV error.
list_lambda = c(1:10)
pb <- progress_bar$new(total = length(list_lambda))

time.start = Sys.time()
set.seed(7)
for(i in 1:length(list_lambda)){
  cv.model <- cv.glmnet(x,y,family = "gaussian", alpha = 1)
  list_lambda[i] = cv.model$lambda.1se
  pb$tick()
}
print(Sys.time()-time.start) # ~10sec
best_lambda = mean(list_lambda) # get the average value from 10 iterations
message(sprintf("[%s] best lambda: %.8f",toolName, best_lambda))

#DOE best lambda: 0.09815532
#DOT best lambda: 0.1059757
#NYS best lambda: 0.1569863

# step 2: lasso regression ------------------------------------------
regfit_lasso <- glmnet(x, y, family = "gaussian", 
                      alpha = 1, 
                      lambda = best_lambda)
est.lasso = coef(regfit_lasso) # get coefficient

# get list of variables post lasso regression
est.dt = data.table(col_name = rownames(est.lasso)[-1],
                    estLasso = est.lasso[-1]) %>% 
  filter(estLasso != 0)

# step 3: post-lasso regression -------------------------------------
selected_cols = c("Cumulative_Score_Ptl",est.dt$col_name)
dt_post <- dt[, ..selected_cols]
regfit <- glm(Cumulative_Score_Ptl ~., data = dt_post, family = "gaussian")
message(sprintf("[%s] Post-LASSO regression complete.", toolName))
message(sprintf("[%s] Num of Obs. = %d", toolName, nrow(dt_post)))


# save regression object by tool ********************************
if (toolName == "DOE") {
  regfit_doe = regfit
} else if (toolName == "DOT") {
  regfit_dot = regfit
} else if (toolName == "NYS") {
  regfit_nys = regfit
}
message(sprintf("[%s] Post-LASSO regression object saved.", toolName))

}
# save result to list object ======================================
regfitList = list()
regfitList[[1]] = regfit_doe 
regfitList[[2]] = regfit_dot
regfitList[[3]] = regfit_nys
saveRDS(regfitList, file="Score_postLasso_1SE.RData")


#####################################################################
# 2. post-LASSO: linear regression (lambda use min-Error rule)
#####################################################################
if (toolName %in% c("DOE", "DOT", "NYS")) {
  message(sprintf("*** Start Analysis for [%s] - pick lambda use min-Error rule ***", 
                  toolName))
dt1 = na.omit(dt) # remove data containing NA values
y = dt1$Cumulative_Score_Ptl
# change column index accordingly
if (toolName == "DOE") {
  x = data.matrix(dt1[,5:40]) # ***DOE (36 indicators)
} else if (toolName == "DOT") {
  x = data.matrix(dt1[,5:44]) # ***DOT (40 indicators)
} else if (toolName == "NYS") {
  x = data.matrix(dt1[,4:48]) # ***NY State (45 indicators)
}

# step 1: select lambda ---------------------------------------------
# Select lambda that minimizes cross-validation error.
list_lambda = c(1:10)
pb <- progress_bar$new(total = length(list_lambda))

time.start = Sys.time()
set.seed(7)
for(i in 1:length(list_lambda)){
  cv.model <- cv.glmnet(x,y,family = "gaussian", alpha = 1)
  list_lambda[i] = cv.model$lambda.min
  pb$tick()
}
print(Sys.time()-time.start) 

best_lambda = mean(list_lambda) # get the average value from 10 iterations
message(sprintf("[%s] best lambda: %.8f",toolName, best_lambda))

#DOE best lambda: 0.03320902
#DOT best lambda: 0.02685454
#NYS best lambda: 0.02413095

# step 2: lasso regression ------------------------------------------
regfit_lasso <- glmnet(x, y, family = "gaussian", 
                       alpha = 1, 
                       lambda = best_lambda)
est.lasso = coef(regfit_lasso) # get coefficient

# get list of variables post lasso regression
est.dt = data.table(col_name = rownames(est.lasso)[-1],
                    estLasso = est.lasso[-1]) %>% 
  filter(estLasso != 0)

# step 3: post-lasso regression -------------------------------------
selected_cols = c("Cumulative_Score_Ptl",est.dt$col_name)
dt_post <- dt[, ..selected_cols]
regfit <- glm(Cumulative_Score_Ptl ~., data = dt_post, family = "gaussian")
message(sprintf("[%s] Post-LASSO regression complete.", toolName))
message(sprintf("[%s] Num of Obs. = %d", toolName, nrow(dt_post)))

# save regression object by tool ********************************
if (toolName == "DOE") {
  regfit_doe = regfit
} else if (toolName == "DOT") {
  regfit_dot = regfit
} else if (toolName == "NYS") {
  regfit_nys = regfit
}
message(sprintf("[%s] Post-LASSO regression object saved.", toolName))
}
# save result to list object ======================================
regfitList = list()
regfitList[[1]] = regfit_doe 
regfitList[[2]] = regfit_dot
regfitList[[3]] = regfit_nys
saveRDS(regfitList, file="Score_postLasso_minE.RData")
